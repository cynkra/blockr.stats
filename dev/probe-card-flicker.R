# Why does the card blink on an update?
#
# Instruments the summary block's output slot with a MutationObserver, triggers
# a change, and reports the exact sequence: does the DOM go EMPTY (a real
# teardown, i.e. core setting output$result <- NULL while the block is not
# ready) or does it merely fade (Shiny's .recalculating class)? The fix is
# different for each, so measure before prescribing.
#
# Two cases, because they can differ:
#   own    - one of the card's own gear options changes (one-block app)
#   model  - the MODEL upstream changes (three-block board): the reported case
#
#   Rscript dev/model-summary-app.R 4370 > /tmp/a.log 2>&1 &
#   until ss -ltn | grep -q :4370; do sleep 1; done
#   Rscript dev/probe-card-flicker.R 4370 own
#
#   Rscript dev/model-summary-board-app.R 4371 > /tmp/b.log 2>&1 &
#   until ss -ltn | grep -q :4371; do sleep 1; done
#   Rscript dev/probe-card-flicker.R 4371 model

args <- commandArgs(trailingOnly = TRUE)
port <- suppressWarnings(as.integer(if (length(args) >= 1L) args[1L] else "3838"))
case <- if (length(args) >= 2L) args[2L] else "own"

b <- chromote::ChromoteSession$new()
b$Page$navigate(sprintf("http://127.0.0.1:%d/", port))
Sys.sleep(if (case == "model") 9 else 6)

js <- function(expr) b$Runtime$evaluate(expr, returnByValue = TRUE)$result$value

# Watch the output node that holds the card. Every childList mutation, plus
# whether the node was empty at that instant and which classes it carried.
watching <- js("
(function(){
  var card = document.querySelector('.msc-card');
  var host = card ? card.closest('.shiny-html-output') : document.querySelector('.shiny-html-output');
  if (!host) return 'NO OUTPUT NODE';
  window.__probe = { t0: performance.now(), log: [] };
  var rec = function (what) {
    window.__probe.log.push({
      t: Math.round(performance.now() - window.__probe.t0),
      what: what,
      empty: host.innerHTML.trim().length === 0,
      chars: host.innerHTML.length,
      recalc: host.className.indexOf('recalculating') >= 0,
      opacity: getComputedStyle(host).opacity,
      cards: document.querySelectorAll('.msc-card').length
    });
  };
  new MutationObserver(function (muts) {
    muts.forEach(function (m) {
      rec(m.type === 'childList'
        ? ('childList +' + m.addedNodes.length + ' -' + m.removedNodes.length)
        : ('attr:' + m.attributeName));
    });
  }).observe(host, { childList: true, subtree: false, attributes: true,
                     attributeFilter: ['class', 'style'] });
  // sample opacity while the swap happens: the fade is invisible to mutations
  window.__probe.timer = setInterval(function () {
    var o = getComputedStyle(host).opacity;
    var last = window.__probe.log[window.__probe.log.length - 1];
    if (!last || last.opacity !== o) rec('opacity=' + o);
  }, 25);
  rec('baseline');
  return host.id || '(no id)';
})()")
cat("watching:", watching, "\n")

trigger <- if (case == "model") {
  # A genuine upstream model update: refit as a Poisson GLM. The summary block
  # then gets a different model object, on a different scale.
  "(function(){
     var btns = document.querySelectorAll('.formula-model-type .btn, .formula-model-type button');
     for (var i = 0; i < btns.length; i++) {
       if ((btns[i].textContent || '').indexOf('Poisson') >= 0) { btns[i].click(); return 'refit'; }
     }
     return 'NOT FOUND: ' + Array.from(btns).map(function(x){return x.textContent.trim();}).join('/');
   })()"
} else {
  "(function(){
     var labels = document.querySelectorAll('.blockr-checkbox');
     for (var i = 0; i < labels.length; i++) {
       if ((labels[i].textContent || '').trim() === 'Intercept row') {
         labels[i].querySelector('input').click(); return 'toggled';
       }
     }
     return 'NOT FOUND';
   })()"
}
cat("trigger:", js(trigger), "\n")
Sys.sleep(5)

js("clearInterval(window.__probe.timer); 1")
cat(gsub("},", "},\n", js("JSON.stringify(window.__probe.log)")), "\n\n")

cat("case:", case, "\n")
cat("mutations with an EMPTY output slot:", js("window.__probe.log.filter(function(e){return e.empty;}).length"), "\n")
cat("lowest opacity seen:",
    js("Math.min.apply(null, window.__probe.log.map(function(e){return parseFloat(e.opacity);}))"), "\n")

b$close()
