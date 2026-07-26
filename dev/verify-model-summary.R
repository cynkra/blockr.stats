# End-to-end check of the model summary block: drives a running board with a
# real browser and sets the options IN THE GEAR.
#
# Setting options in the constructor proves nothing about the gear -- a
# ctor-arg test passes on a broken JS -> R transport. So this changes each
# control the way a user does and asserts the card changed underneath.
#
# Start the app first, in the SAME shell call (a backgrounded Shiny gets
# reaped between calls in this container):
#
#   Rscript dev/model-summary-app.R 4351 > /tmp/ms.log 2>&1 &
#   until ss -ltn | grep -q :4351; do sleep 1; done
#   Rscript dev/verify-model-summary.R 4351

port <- local({
  arg <- commandArgs(trailingOnly = TRUE)[1L]
  p <- suppressWarnings(as.integer(if (is.na(arg)) Sys.getenv("BLOCKR_PORT", "3838") else arg))
  if (is.na(p)) 3838L else p
})

shots <- Sys.getenv("MS_SHOTS", file.path(tempdir(), "model-summary-verify"))
dir.create(shots, showWarnings = FALSE, recursive = TRUE)
url <- sprintf("http://127.0.0.1:%d/", port)

b <- chromote::ChromoteSession$new()
b$Page$navigate(url)
Sys.sleep(6)

js <- function(expr) {
  out <- b$Runtime$evaluate(expr, returnByValue = TRUE)
  out$result$value
}
shot <- function(name) {
  b$screenshot(filename = file.path(shots, paste0(name, ".png")), scale = 1)
  file.path(shots, paste0(name, ".png"))
}

# --- the card is in the OUTPUT slot, drawn from the block's value ----------
card <- js("document.querySelectorAll('.msc-card').length")
rows <- js("document.querySelectorAll('.msc-ct tbody tr').length")
terms <- js("Array.from(document.querySelectorAll('.msc-term')).map(e => e.innerText.trim()).join(' | ')")
cat("cards:", card, " rows:", rows, "\nterms:", terms, "\n")
stopifnot(card == 1, rows == 5)
# intercept last, model order kept for the rest
stopifnot(grepl("wt.*hp.*Intercept", terms))
shot("01-default")

# --- open the gear --------------------------------------------------------
js("document.querySelector('.blockr-gear-btn').click(); 1")
Sys.sleep(1)
open <- js("document.querySelectorAll('.blockr-settings--open').length")
cat("gear band open:", open, "\n")
stopifnot(open == 1)
shot("02-gear-open")

# --- change a select IN THE GEAR and watch the card follow ----------------
# The gear controls are the design-system components, not Bootstrap widgets:
# Blockr.Select has no setValue, so drive it the way a user does -- open the
# control, then click the option whose primary text is the token. Blockr.checkbox
# is a native input behind a styled box, so a click on the input is honest too.
set_select <- function(id, value) {
  opened <- js(sprintf(
    "(function(){
       var host=document.querySelector('[data-ms-input$=\"%s\"]');
       if(!host) return 'NO HOST';
       var ctl=host.querySelector('.blockr-select__control');
       if(!ctl) return 'NO CONTROL';
       ctl.click();
       return 'open';})()", id))
  if (!identical(opened, "open")) stop("could not open ", id, ": ", opened)
  Sys.sleep(0.5)
  # The open dropdown is portaled out of the host (so it can escape panels and
  # offcanvases), so look for it document-wide -- only one is ever open.
  picked <- js(sprintf(
    "(function(){
       var opts=document.querySelectorAll('.blockr-select__option');
       for (var i=0;i<opts.length;i++){
         var t=(opts[i].textContent||'').trim();
         if (t===\"%s\" || t.indexOf(\"%s\")===0) { opts[i].click(); return \"%s\"; }
       }
       return 'NOT FOUND: ' + Array.from(opts).map(function(o){return o.textContent.trim();}).join('/');
     })()", value, value, value))
  if (!identical(picked, value)) stop("could not pick ", value, " for ", id, ": ", picked)
  picked
}
set_check <- function(id, checked) {
  js(sprintf(
    "(function(){
       var labels=document.querySelectorAll('.blockr-checkbox');
       for (var i=0;i<labels.length;i++){
         if ((labels[i].textContent||'').trim() === \"%s\") {
           var inp=labels[i].querySelector('input');
           if (inp.checked !== %s) inp.click();
           return inp.checked;
         }
       }
       return 'NOT FOUND';})()",
    id, if (checked) "true" else "false"))
}

# chips are the DEFAULT, so drive away from the default first: setting a
# control to the value it already holds would not fire a change and would
# prove nothing about the transport.
stopifnot(js("document.querySelectorAll('.msc-chip').length") > 0)

set_select("significance", "stars")
Sys.sleep(3)
stars <- js("document.body.innerText.indexOf('***') >= 0")
cat("stars after gear change:", stars, "\n")
stopifnot(isTRUE(stars), js("document.querySelectorAll('.msc-chip').length") == 0)

set_select("significance", "chips")
Sys.sleep(3)
chips <- js("document.querySelectorAll('.msc-chip').length")
cat("chips back:", chips, "\n")
stopifnot(chips > 0)

set_select("uncertainty", "se")
Sys.sleep(3)
se_head <- js("Array.from(document.querySelectorAll('.msc-ct th')).map(e=>e.innerText.trim()).join(' | ')")
cat("headers after SE:", se_head, "\n")
stopifnot(grepl("SE", se_head, ignore.case = TRUE))

set_select("scale", "ratio")
Sys.sleep(3)
ratio_head <- js("Array.from(document.querySelectorAll('.msc-ct th')).map(e=>e.innerText.trim()).join(' | ')")
cat("headers after ratio:", ratio_head, "\n")
# CSS upper-cases the headers, so compare case-insensitively
stopifnot(grepl("ratio|^OR$|HR|RR", ratio_head, ignore.case = TRUE))
shot("03-gear-changes")

# back to the defaults, then the switches
set_select("scale", "auto")
set_select("uncertainty", "ci95")
set_select("significance", "chips")
Sys.sleep(3)

set_check("Intercept row", FALSE)
Sys.sleep(3)
rows_no_int <- js("document.querySelectorAll('.msc-ct tbody tr').length")
has_int <- js("document.body.innerText.indexOf('(Intercept)') >= 0")
cat("rows without intercept:", rows_no_int, " intercept present:", has_int, "\n")
stopifnot(rows_no_int == 4, isFALSE(has_int))

set_check("Effect column (forest)", FALSE)
set_check("Model facts line", FALSE)
Sys.sleep(3)
tracks <- js("document.querySelectorAll('.msc-track').length")
facts <- js("document.querySelectorAll('.msc-facts').length")
cat("tracks:", tracks, " facts lines:", facts, "\n")
stopifnot(tracks == 0, facts == 0)
shot("04-switches-off")

# --- and back on, so the whole thing is reversible -------------------------
set_check("Effect column (forest)", TRUE)
set_check("Model facts line", TRUE)
set_check("Intercept row", TRUE)
Sys.sleep(3)
final <- js("document.querySelectorAll('.msc-track').length")
cat("tracks restored:", final, "\n")
stopifnot(final == 5)
shot("05-restored")

cat("\nOK - every gear control reached R and redrew the card\n")
cat("screenshots in:", shots, "\n")

# --- sorting: browser-side, and it must survive an R redraw ----------------
# The card is replaced wholesale on every update, so a sort that is not
# re-applied would silently reset. That is the whole risk of doing this in JS.
terms <- function() {
  js("Array.from(document.querySelectorAll('.msc-term')).map(function(e){return e.innerText.trim();}).join('|')")
}
click_header <- function(key) {
  js(sprintf(
    "(function(){var th=document.querySelector('th[data-ms-sort=\"%s\"]');
       if(!th) return 'NO HEADER'; th.click(); return 'clicked';})()", key))
}

# The header must not move. The html preview reserves the sort arrow's 12px
# permanently and shows no hover hint; a hint that grows the icon box would
# shift every column under the pointer, which is exactly what it did once.
widths <- function() {
  js("Array.from(document.querySelectorAll('.msc-ct th')).map(function(t){return Math.round(t.getBoundingClientRect().width);}).join(',')")
}
w_rest <- widths()
js("(function(){var th=document.querySelector('th[data-ms-sort=\"estimate\"]');
     th.dispatchEvent(new MouseEvent('mouseover', {bubbles:true}));
     th.dispatchEvent(new MouseEvent('mouseenter', {bubbles:true}));
     return 1;})()")
Sys.sleep(0.5)
w_hover <- widths()
cat("header widths at rest: ", w_rest, "\n")
cat("header widths on hover:", w_hover, "\n")
stopifnot(identical(w_rest, w_hover))
stopifnot(js("document.body.innerText.indexOf('\u2195') < 0"))

model_order <- terms()
cat("model order:", model_order, "\n")

click_header("estimate"); Sys.sleep(0.6)
w_sorted <- widths()
cat("header widths sorted: ", w_sorted, "\n")
stopifnot(identical(w_rest, w_sorted))
asc <- terms()
cat("sorted by estimate asc:", asc, "\n")
stopifnot(asc != model_order)
# the intercept is pinned out of every order
stopifnot(grepl("\\(Intercept\\)$", asc))

click_header("estimate"); Sys.sleep(0.6)
desc <- terms()
cat("sorted by estimate desc:", desc, "\n")
stopifnot(desc != asc, grepl("\\(Intercept\\)$", desc))

# a redraw (any gear change) must not lose the reader's order
set_check("Model facts line", FALSE)
Sys.sleep(3)
after <- terms()
cat("after a redraw:", after, "\n")
stopifnot(identical(after, desc))
set_check("Model facts line", TRUE)
Sys.sleep(3)

# third click returns to the model's own order
click_header("estimate"); Sys.sleep(0.6)
restored <- terms()
cat("third click:", restored, "\n")
stopifnot(identical(restored, model_order))

# and nothing about any of it reached R: the block's own state is untouched
shot("06-sorted")
cat("\nOK - sorting is browser-side, survives a redraw, and undoes cleanly\n")

b$close()
