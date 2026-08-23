# blockr.stats -- aedes-ivm: real published data, and the price of the easy model.
#
# One dataset (tiger mosquito egg counts from six towns across the Swiss-Italian
# border, 2019), one question -- does the mosquito control programme run on the
# Swiss side actually work? -- and TWO models of the same data, side by side on
# purpose.
#
# THIS BOARD'S DATA IS REAL AND PUBLIC, and that is the point of it. The source
# is an open-access paper, the numbers are the authors' own, and refitting the
# published model in the board returns the published result to two decimals:
# 3.81 times more eggs where there is no
# programme, 95% CI 2.72 to 5.35, against the paper's 3.8 (2.7-5.4). Anyone in
# the room can download the same file and check.
#
#   Ravasi D, Parrondo Monton D, Tanadini M, Flacio E (2021). Effectiveness of
#   integrated Aedes albopictus management in southern Switzerland.
#   Parasites & Vectors 14, 405. doi:10.1186/s13071-021-04903-2 (CC BY 4.0)
#
# THE POINT OF THIS BOARD IS THE ESCALATION, and here it has a number on it.
#
#   Arm 1, the model block. A Poisson glm of egg count on area, authored by
#     picking a response and dragging in one term: no code. It answers the
#     question -- 3.59 times more eggs without the programme -- and it is
#     WRONG IN A WAY THAT MATTERS. Its 95% interval is 3.51 to 3.67. The
#     honest interval is 2.72 to 5.35, seventeen times wider. The Poisson
#     assumes the variance equals the mean; here the residual deviance is
#     about 180 times its degrees of freedom, and ten readings of one trap are
#     counted as ten independent facts. The point estimate survives that. The
#     precision does not.
#
#   Arm 2, the code block. The model the statisticians actually published:
#     a negative binomial mixed model with a quadratic season, an exposure
#     term for how long each trap sat in the field, and random intercepts for
#     trap and municipality. `glmmTMB::glmmTMB` is not a blockr block and does
#     not need to be. The code block is a CodeMirror editor holding a plain R
#     script; the broom adapter tidies its result downstream exactly as it
#     tidies the glm.
#
# So the story is: start no-code, and when the statistics outgrow the block
# picker, drop into R WITHOUT LEAVING THE BOARD. It is a better story than
# "we also have a mixed-model block", because the thing the audience should
# take home is not that blockr has many blocks -- it is that the no-code path
# has an edge, blockr shows you where it is, and stepping over it costs you
# nothing you had already built.
#
# THE AREA FACTOR'S LEVEL ORDER IS LOAD-BEARING. `Intervention` is the first
# level of `AREA`, so the area coefficient in both arms is a contrast AGAINST
# the towns running the programme, which is how the published result reads.
# Sorting those levels alphabetically would flip the sign of the headline. The
# dataset ships as an .rda, not a CSV, for exactly this reason -- see
# `?aedes_ovitraps` and data-raw/aedes-ivm.R.
#
# THREE views, the same shape as stats-101, so the two demo the same product:
#
#   Model     the dashboard, and the comparison. LEFT is the no-code arm: the
#             glm's formula widget over its coefficient card. RIGHT is the
#             published arm: the effect table on top, the fit's diagnostics
#             below. Edit the formula on the left and only the left moves --
#             the two arms are independent fits of one dataset, which is the
#             whole exhibit.
#   Report    the DOCUMENT BUILDER (blockr.outline's report extension, not the
#             outline), the deck builder behind it on a tab, and the two ggplot
#             figures beside it: the season by area, and the trap network
#             across the border. What it downloads is a quarto document whose
#             chunks are canonical R, which is the deliverable the whole board
#             exists to produce.
#   Workflow  the minidag, DAG on a tab behind it, assistant beside both.
#
# Run the shipped copy (installed packages):
#   source(system.file("examples/aedes-ivm.R", package = "blockr.stats"))
# Run it against local source checkouts instead:
#   source("blockr.stats/dev/aedes-ivm.R")     # sets dev_local <- TRUE

# ---- Package loading (dual: installed vs local source) ---------------------
if (!exists("dev_local")) dev_local <- FALSE

blockr_pkgs <- c(
  "blockr.ui",
  "blockr.core",
  "blockr.dock",
  "blockr.io",        # the read block that fetches the published CSV
  "blockr.dplyr",     # the sqrt mutate feeding the season chart
  "blockr.viz",       # the dashboard charts + summary/table blocks
  "blockr.stats",     # dataset + model + model-summary + broom blocks
  "blockr.session",   # project save / load / versions
  "blockr.assistant", # board-level chat
  "blockr.outline",   # the report builder, the deck builder, the minidag
  # NOT optional here, unlike in stats-101: the published arm below IS a
  # blockr.extra code block. Without this the board does not construct.
  "blockr.extra"
)

for (pkg in blockr_pkgs) {
  if (dev_local) pkgload::load_all(pkg, quiet = TRUE)
  else library(pkg, character.only = TRUE)
}

# Both are Suggests, and only the published arm needs them: glmmTMB to fit,
# broom.mixed for the tidy method on the fit. Fail here with a sentence rather
# than inside a code editor with a red squiggle, or -- worse -- with "no tidy
# method" three blocks downstream.
# Named one per line, not looped: the gallery generator reads this file
# line-by-line for the demo's dependency list, so a loop variable would go on
# the website as a package called `pkg`.
if (!requireNamespace("glmmTMB", quietly = TRUE)) {      # the published arm's fit
  stop("aedes-ivm needs the glmmTMB package: install.packages('glmmTMB')",
       call. = FALSE)
}
if (!requireNamespace("broom.mixed", quietly = TRUE)) {  # its tidy() method
  stop("aedes-ivm needs the broom.mixed package: install.packages('broom.mixed')",
       call. = FALSE)
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {      # the season figure
  stop("aedes-ivm needs the ggplot2 package: install.packages('ggplot2')",
       call. = FALSE)
}
# NOT attached, and nothing on this board calls `tidy()` at all. gtsummary
# reaches the glmmTMB tidier through broom.helpers, which loads broom.mixed
# itself the first time a mixed model arrives at `tbl_regression()`. Checked
# in a session where broom.mixed was unloaded: the table builds, and the
# namespace is loaded afterwards. That is what lets the downloaded qmd render
# in a session that has never heard of this app -- installed is enough.

options(
  blockr.dock_is_locked = FALSE,
  blockr.tabular_display = blockr.ui::html_table_display,
  blockr.background_construction_delay = 0,
  blockr.visible_extensions = "outline",
  # No render gating: a block that leaves the screen keeps its output instead
  # of having it torn down and re-assigned on the way back, so tabbing between
  # the model, its table and the figure no longer greys the panel. Shiny fades
  # a recalculating output after 500ms, and the figure re-render is slower than
  # that, which is why it was the visible one.
  #
  # Costs lazy evaluation: every block on a view evaluates at startup rather
  # than on first sight, so the GLMM fits while the app boots.
  blockr.gate_visibility = FALSE
)

if (dev_local) options(blockr.outline.execute = "in-process")

if (is.null(getOption("blockr.chat_function"))) {
  if (nzchar(Sys.getenv("OPENAI_API_KEY"))) {
    llm <- Sys.getenv("BLOCKR_LLM_MODEL", "gpt-5.1")
    options(blockr.chat_function = list(
      openai = function(system_prompt = NULL, params = NULL) {
        ellmer::chat_openai(model = llm)
      }
    ))
  } else {
    message(
      "No OPENAI_API_KEY: the board still runs, but the Assistant panel ",
      "cannot talk to a model."
    )
  }
}

# The published model, as a SCRIPT, because that is what a code block stores.
#
# This block used to be a Function block. The swap is the reason the exported
# document reads the way it does. A Function block wraps its body in a
# function and calls it, so the report got
# `local({.fn <- function(data, random_trap = TRUE) {...}; .fn(ovi)})` --
# correct, runnable, and not something anyone would write. A code block
# substitutes the control values into the body as literals and emits the
# statement bare, so the same model lands in the qmd as one `glmmTMB::glmmTMB()`
# call a reader can lift.
#
# `random_trap <- TRUE` IS THE CHECKBOX. A top-level assignment whose right
# side is a plain value is a control, so a logical becomes a tickbox with no UI
# code written. Unticking it is the single most instructive click on this
# board: it drops the trap random intercept, refits, and the confidence
# interval visibly narrows, because the model has gone back to believing ten
# readings of one trap are ten independent facts. That is the Poisson arm's
# mistake, reproduced on demand inside the honest model.
#
# THE `if` HAS TO BE THE LAST TERM IN THE FORMULA. `+` is left-associative, so
# `+ if (x) a + b` parses as `+ if (x) (a + b)` and unticking the box would
# drop the municipality intercept too. Trailing, it governs exactly its own
# operand, which the emitter then prunes -- the exported formula has no `if` in
# it at either setting.
#
# Comments inside this script do NOT reach the document: the emitter rebuilds
# the call from the parse tree, and R drops comments on the way. That is why
# every explanation below lives in the report's text items instead, which is
# also how Matteo's own report reads -- a paragraph, then the code it
# describes.
glmm_script <- 'glmmTMB::glmmTMB(
  No..eggs.AEDES ~ AREA +
    poly(Day.ovitrap.collected, degree = 2) +
    scale(ALTITUDE) +
    No..Days.ovitrap.in.field +
    (1 | MUNICIPALITY) +
    (1 | TRAP.ID.fac),
  family = glmmTMB::nbinom1,
  data = data
)
'


# The published model's RESULT, drawn the way the paper draws it: one grey line
# per trap, faceted by arm, the fitted curve and its 95% band on top (Ravasi et
# al. Fig. 3, refitted). This is the figure the report exists to carry.
#
# ITS INPUT IS THE MODEL, NOT THE DATA, because a code block takes exactly one
# input and the fit is the thing that cannot be recomputed cheaply. The observed
# collections come back out of the fit's model frame, inverting the poly() basis
# the same way `glmm_season_script` used to.
#
# WHY predict() ON A GRID IS SAFE HERE. `poly()` and `scale()` would normally
# re-centre themselves on whatever data they are handed, which would bend the
# curve silently rather than error. R's predvars mechanism pins the original
# basis and glmmTMB honours it: predicting a 40-row subset reproduces the
# original fit to 4e-15. Verified 2026-08-22.
#
# THE ARM LABELS ARE NOT A FACTOR HERE. The board reads the published CSV, so
# AREA arrives as character and levels() on it is NULL -- which silently
# collapsed the prediction grid to zero rows the first time this ran. Derived
# with sort(unique()) instead, which is exactly the ordering R itself gives a
# character predictor, so the grid matches the model's own contrasts whether
# the data came from the CSV or the packaged .rda. Verified in the app
# 2026-08-22.
#
# NO CONTROLS ON PURPOSE. Every constant here is inline, because a top-level
# assignment whose right side is a plain value becomes a control, and this
# block had five of them -- two tickboxes, two numbers and a colour pair --
# spread across the panel above a figure that is the only thing worth looking
# at. This is an exhibit, not a knob board. With nothing declared there is
# also no gear, which is the convention: no options means no gear.
#
# The axis is the paper's, linear and shared across the two panels. Wrap the
# plot in ggplot2::scale_y_sqrt() if the intervention panel needs to breathe.

season_gg_script <- 'mf <- stats::model.frame(data)
pol <- mf[["poly(Day.ovitrap.collected, degree = 2)"]]
cfs <- attr(pol, "coefs")
day <- pol[, 1] * sqrt(cfs$norm2[3]) + cfs$alpha[1]

area_levels <- if (is.factor(mf$AREA)) {
  levels(mf$AREA)
} else {
  sort(unique(as.character(mf$AREA)))
}

grid <- expand.grid(
  AREA = area_levels,
  Day.ovitrap.collected = seq(min(day), max(day), length.out = 120),
  stringsAsFactors = FALSE
)
grid$ALTITUDE <- attr(mf[["scale(ALTITUDE)"]], "scaled:center")
grid$No..Days.ovitrap.in.field <- 14
grid$MUNICIPALITY <- mf$MUNICIPALITY[1]
grid$TRAP.ID.fac <- mf$TRAP.ID.fac[1]

pred <- stats::predict(data, grid, type = "link", se.fit = TRUE, re.form = NA)
grid$fit <- exp(pred$fit)
grid$lower <- exp(pred$fit - 1.96 * pred$se.fit)
grid$upper <- exp(pred$fit + 1.96 * pred$se.fit)

obs <- data.frame(
  Day.ovitrap.collected = day,
  AREA = mf$AREA,
  TRAP.ID.fac = mf$TRAP.ID.fac,
  No..eggs.AEDES = mf$No..eggs.AEDES
)
obs <- obs[order(obs$TRAP.ID.fac, obs$Day.ovitrap.collected), ]

ggplot2::ggplot(grid, ggplot2::aes(Day.ovitrap.collected)) +
  ggplot2::geom_line(
    data = obs,
    ggplot2::aes(y = No..eggs.AEDES, group = TRAP.ID.fac),
    colour = "#9a9aa2", linewidth = 0.38, alpha = 0.75
  ) +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = lower, ymax = upper, fill = AREA), alpha = 0.25
  ) +
  ggplot2::geom_line(ggplot2::aes(y = fit, colour = AREA), linewidth = 1.5) +
  ggplot2::facet_wrap(~AREA) +
  ggplot2::scale_colour_manual(values = c("#0072B2", "#E69F00")) +
  ggplot2::scale_fill_manual(values = c("#0072B2", "#E69F00")) +
  ggplot2::labs(x = "Day of year", y = "Eggs per collection") +
  ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(legend.position = "none")
'

# THE COEFFICIENT TABLE, FOR BOTH ARMS, IN ONE CALL.
#
# `tbl_regression()` is to a fit what `tbl_summary()` is to a data frame: the
# one function you reach for, rather than a pipeline you assemble. It reads the
# model, exponentiates, attaches the confidence interval and the p-value, keeps
# the reference level as its own row, and renders as gt -- on screen and in the
# document, from the same call.
#
# WHAT THIS REPLACED, and it is the strongest argument for it. The published
# arm used to go `broom.mixed::tidy()` -> filter -> mutate(exp) -> select ->
# table block: FIVE blocks to turn a fit into four columns, three of them there
# only because the table block renders a wide frame badly and rounds the panel
# without rounding the report. All five are gone. The escalation story is
# untouched -- the glmmTMB call is still hand-written R in a code block, which
# is the point being made -- but what happens AFTER the fit is no longer a
# pipeline the audience has to watch being built.
#
# One call reads each arm, which is the exhibit: two fits of one dataset, read
# the same way, disagreeing by a factor of seventeen on the interval. 3.59 (3.51, 3.67) on the left and 3.81 (2.72, 5.35) on the right.
#
# THE ONE WRAPPER, AND WHY IT IS A WRAPPER AND NOT AN ARGUMENT. broom.mixed's
# tidier names the model COMPONENT every row came from, and a glmmTMB has more
# than one of them (the counts, and the dispersion). gtsummary reads that
# column as a grouping variable, so it prints a caution that a multi-component
# model may not behave like an ordinary `tbl_regression`, and it heads the
# table with a group row reading `cond` -- a group with one member. The
# caution is about `tbl_merge()` and friends on a grouped table, which this
# board never does, so it is noise here.
#
# `suppressMessages()` mutes it and keeps the call one line, which is the line
# worth showing. The price, and it is accepted rather than overlooked, is the
# `cond` row, which is cosmetic and stays. Passing a tidier that drops the
# component column removes both, at the cost of five lines of plumbing in the
# script the audience reads; that trade went the other way on 2026-08-23.
regression_script <- 'suppressMessages(
  gtsummary::tbl_regression(data, exponentiate = TRUE)
)
'

# THE SAME CALL, BUT IT HAS TO SURVIVE THE MODEL-TYPE BUTTONS.
#
# The basic model's four buttons are there to be pressed, and pressing one
# changes what `exponentiate` means. `tbl_regression(exponentiate = TRUE)`
# errors outright on an `lm` -- "`exponentiate = TRUE` is not valid for this
# type of model" -- so a fixed TRUE turns a click on "Linear (lm)" into a red
# block in the middle of the demo.
#
# The rule is the LINK, not the class: exponentiating a coefficient is only a
# multiplier when the link is log or logit. Poisson and logistic qualify, an lm
# does not, and neither does the Gamma -- its default link is inverse, where
# exp(beta) means nothing. gtsummary is happy to exponentiate the Gamma anyway;
# this is deliberately stricter than gtsummary on that one, because a number
# nobody can interpret is worse than a number on the log scale.
#
# The GLMM keeps the plain `exponentiate = TRUE` above: it is always nbinom1
# with a log link, it is a report chunk, and the document is better for the
# one-liner.
basic_regression_script <- 'link <- stats::family(data)$link

gtsummary::tbl_regression(data, exponentiate = link %in% c("log", "logit"))
'

# THE DESCRIPTIVE SUMMARY, AS A STATISTICIAN WOULD WRITE IT.
#
# Nobody enumerates min/median/mean/max to describe a variable. They call the
# one function their package provides and let it decide: `summary()` in base R,
# `PROC MEANS`, `summarize` in Stata. With a grouping variable and a table
# meant for a paper, the R answer is `gtsummary::tbl_summary()`.
#
# ONE BLOCK, NOT FOUR. This replaced a `new_summary_table_block()` feeding a
# `new_table_block()` for the app, plus two more blocks emitting canonical code
# for the document. The summary table block is a reimplementation of gtsummary
# -- its whole vocabulary (`mean_sd`, `median_q1_q3`, `min_max`, `add_overall`)
# is gtsummary's, and the two agree to the digit -- so once the document has to
# carry canonical R anyway, the reimplementation earns nothing here and the
# call itself does both jobs.
#
# WHERE THAT STOPS BEING TRUE: gtsummary is not fast. One variable over 327
# rows is 0.18s and all thirteen over six groups is 0.82s, which is nothing; a
# pharma table of hundreds of variables is another matter, and that is exactly
# what `blockr.viz::new_summary_table_block()` is for. This board is small and
# its document is the deliverable, so the call wins HERE. Not a general
# recommendation.
#
# `group` IS A CONTROL. A top-level assignment of a plain value becomes one on
# the card, and a factor becomes a dropdown whose levels are the choices, so
# this line is the selector -- no blockr vocabulary in the script. The current
# value is substituted back as a LITERAL, which is also why the export reads
# `by = "AREA"` rather than naming a variable: passing a character variable to
# a tidyselect argument warns, passing the string does not.
#
# The by-town table this board used to carry is gone, and the paragraph above
# it never needed one: the paper reports its ranges BY AREA -- 0 to 513 against
# 0 to 2117 -- which is exactly this table's Min, Max row.
desc_script <- 'group <- factor("AREA", c("AREA", "MUNICIPALITY"))

gtsummary::tbl_summary(
  data,
  by = group,
  include = No..eggs.AEDES,
  type = No..eggs.AEDES ~ "continuous2",
  statistic = No..eggs.AEDES ~ c(
    "{N_nonmiss}", "{mean} ({sd})", "{median} ({p25}, {p75})", "{min}, {max}"
  )
)
'

# The GLM arm's season picture, and the reason it exists: with
# `eggs ~ AREA` the model has ONE fitted value per area, so the plot is two
# flat lines. Add `+ poly(Day.ovitrap.collected, 2)` in the formula widget and
# the same plot becomes two curves. That is the demo's cheapest proof that the
# board is live rather than a screenshot.
#
# `data[["data"]]` and not `stats::model.frame(data)`: a glm keeps the frame it
# was GIVEN in `$data`, and the model frame holds only the columns the formula
# names -- which at the first step does not include the day. Bracket-string
# indexing, not `$data`, so the emitter cannot mistake the component name for
# the input it substitutes.
glm_diag_script <- 'data.frame(
  data[["data"]][c("Day.ovitrap.collected", "AREA", "No..eggs.AEDES")],
  fitted = stats::fitted(data)
)
'

# ---------------------------------------------------------------- board ----

board <- new_dock_board(
  blocks = c(
    # -- The data ----------------------------------------------------------
    # 327 ovitrap readings, 36 traps, six towns, one season, FETCHED FROM THE
    # PUBLISHER over https rather than loaded from this package.
    #
    # `new_dataset_block("aedes_ovitraps", "blockr.stats")` was here first and
    # is the obvious choice, but it emits `ovi <- blockr.stats::aedes_ovitraps`
    # -- which makes the exported document depend on a package nobody outside
    # this workspace has, and asks the reader to take our word for the numbers.
    # This emits a `readr::read_csv()` of a public URL: ordinary R that runs in
    # any session with readr installed, and that shows on its face that the
    # data is the publisher's own file. For a document whose entire argument is
    # "this is reproducible", that difference IS the argument.
    #
    # `blockr.io::new_read_block()`, the block actually built for this, with
    # `source = "path"` pointed at the publisher's own file. One block, no
    # wrapper: "you can read data straight from a URL" is a claim the demo
    # should be able to make by picking a block, not by shipping twelve lines
    # of `new_data_block()` that only this board has.
    #
    # THE EXPORTED CHUNK NAMES THE URL, which took a fix in blockr.io to be
    # true. The read block downloads a URL to a temp file before reading it,
    # and used to build its expression around that path, so the qmd came out
    # carrying `/tmp/RtmpXXXX/fileYYYY.csv` -- a path that exists on nobody
    # else's machine, in a document whose whole argument is that you can run
    # it on yours. A format now declares `url_ok` when its reader takes a URL
    # (readr's do, readxl's and arrow's do not) and the emitted literal is
    # the URL while the read still goes through the download. Filed as
    # _inbox/2026-08-19-io-read-block-exports-temp-path-for-urls.md
    # (BristolMyersSquibb/blockr.io#41), fixed 2026-08-22.
    #
    # The URL is the article's Additional file 2, off Springer's static-content
    # CDN. Note the CDN, not the article page: link.springer.com sits behind a
    # bot wall that answers "Client Challenge" to anything scripted, while
    # static-content.springer.com serves the file directly. Verified
    # 2026-08-19, byte-identical to the copy vendored in data-raw/aedes-ivm/.
    #
    # WHAT THE CSV LOSES, and why it does not matter here. The .rda ships AREA,
    # MUNICIPALITY and TRAP.ID.fac as factors with a deliberate level order; the
    # CSV has them as plain character, so R takes the levels alphabetically.
    # The one that is load-bearing survives by luck -- "Intervention" sorts
    # before "Non-intervention", so it stays the reference level and the
    # headline coefficient keeps its sign. The other two are random-effect
    # groupings, where order means nothing. Refitting from this URL returns
    # 3.81 (95% CI 2.72-5.35), the same as from the packaged data.
    #
    # The packaged `aedes_ovitraps` stays: it is documented, typed, and works
    # offline, which the URL does not.
    ovi = blockr.io::new_read_block(
      path = paste0(
        "https://static-content.springer.com/esm/",
        "art%3A10.1186%2Fs13071-021-04903-2/MediaObjects/",
        "13071_2021_4903_MOESM2_ESM.csv"
      ),
      source = "path",
      block_name = "Read Data"
    ),

    # -- Table 1 -----------------------------------------------------------
    # See `desc_script`: one gtsummary call, a dropdown for the grouping, and
    # the same table in the panel and in the document.
    desc = new_code_block(
      script = desc_script, block_name = "Descriptive Stats"
    ),

    # -- The season, as the demo shows it ----------------------------------
    # Counts over time on a SQUARE-ROOT scale with a loess per area. Neither
    # plot block has a `scale_y_sqrt()`, so the transform is a mutate and the
    # axis is labelled in sqrt units. That is not a workaround with a cost:
    # ggplot2 applies a scale transform BEFORE the stat, so `scale_y_sqrt() +
    # geom_smooth()` fits the smoother on the square-root scale too. This is
    # the same fit, drawn against an axis that says so.
    ovi_sqrt = blockr.dplyr::new_mutate_block(
      mutations = list(
        list(name = "sqrt_eggs", expr = "sqrt(No..eggs.AEDES)")
      ),
      block_name = "Mutate"
    ),
    # A chart block and not a ggplot block, because this is the one plot in
    # the board that needs a SMOOTHER: `blockr.viz` fits one per colour group
    # (and per facet panel, since 0.2.67), `blockr.ggplot` has no such option.
    # Not a report item -- the document keeps the ggplot pair, whose chunks
    # read as ggplot2.
    # `visible = "inputs"` and the reason is worth knowing, because it reads
    # backwards: a chart block draws its chart in the INPUT half of the card
    # (it is an interactive widget the user configures) and returns the plotted
    # data frame as its OUTPUT. Left at the default both show, so the panel is
    # a chart with a table of the same numbers underneath it. On an exhibit the
    # table is noise -- the picture is the point, and the numbers are one panel
    # away in Read Data.
    season_ct = `attr<-`(
      blockr.viz::new_chart_block(
        chart_type = "scatter",
        x = "Day.ovitrap.collected", y = "sqrt_eggs",
        color = "AREA", smoother = "loess",
        title = "Eggs collected",
        subtitle = "All traps, per day, sort()",
        block_name = "Visualization"
      ),
      "visible", "inputs"
    ),

    # -- Arm 1: the no-code model ------------------------------------------
    # A Poisson glm, authored entirely in the formula widget. `visible =
    # "inputs"` drops the R print; the card below is the picture.
    mdl = `attr<-`(
      new_model_block(
        model_type = "poisson",
        # NO SEASON TERM AT STARTUP, deliberately. `+ poly(Day.ovitrap.collected,
        # 2)` is the edit Matteo makes live: the board opens on `~ AREA`, whose
        # fitted values are two flat lines, and adding the term turns them into
        # two curves in the panel beside it. Seeding the finished formula here
        # would spend the demo's best twenty seconds before anyone is watching.
        formula = "No..eggs.AEDES ~ AREA",
        block_name = "Basic Model"
      ),
      "visible", "inputs"
    ),
    # ONE TABLE STYLE ON THE WHOLE BOARD. A `new_model_summary_block()` used to
    # sit here: a nicer card than this, with the effect drawn as a dot and its
    # interval, and the fit statistics in the header. It is gone on purpose.
    #
    # Judged as a table it was redundant -- everything it did has a gtsummary
    # equivalent, down to `add_glance_source_note()` for the header facts and
    # `ggstats::ggcoef_model()` for the forest plot -- and it emitted
    # `blockr.stats::model_summary()`, so it could never appear in a document
    # whose whole claim is canonical R. What it cost was consistency: the
    # descriptive table, this one and the published model's were three
    # different-looking tables for one audience to read in eight minutes.
    #
    # Now they are one table three times.
    mcoef = new_code_block(
      script = basic_regression_script, block_name = "Model Summary"
    ),

    # The formula widget's consequence, made visible. See `glm_diag_script`.
    mdiag = new_code_block(
      script = glm_diag_script, block_name = "Compute Fitted"
    ),
    mfit = `attr<-`(
      blockr.viz::new_chart_block(
        chart_type = "scatter",
        x = "Day.ovitrap.collected", y = "fitted", color = "AREA",
        block_name = "Predicted Values"
      ),
      "visible", "inputs"
    ),

    # -- Arm 2: the escalation ---------------------------------------------
    # Custom R in the board, and now the whole arm in one block: it fits the
    # published model and tabulates it in the same script, so the escalation
    # reads as ONE step ("your method is not a block? write it") instead of a
    # fit followed by a chain that turns the fit into something showable.
    #
    # It returns the FIT, and two blocks read it: `gtbl` turns it into the
    # effect table and `gseason_gg` into the season figure. Three blocks, one
    # per job, so the exported document gets a model chunk, a table chunk and a
    # figure chunk instead of one script doing all three.
    glmm = new_code_block(
      script = glmm_script, block_name = "Published Model (GLMM)"
    ),

    # The headline number. `tbl_regression()` resolves its own tidier, so the
    # downloaded qmd does not depend on broom.mixed happening to be attached.
    gtbl = new_code_block(
      script = regression_script, block_name = "Published Model Summary"
    ),

    # The document's figure. See `season_gg_script`.
    # Left at the DEFAULT visibility on purpose, so the controls show with the
    # plot. `visible = "outputs"` gives the figure the whole panel and looks
    # better, but the four controls live in the input half and go with it --
    # gear included -- and hiding what a block can do is not how this project
    # asks for the trade to be made. Maximise the panel when presenting.
    gseason_gg = new_code_block(
      script = season_gg_script, block_name = "Published Plot"
    )
  ),
  links = links(
    from = c("ovi", "ovi", "ovi", "ovi",
             "ovi_sqrt",
             "mdl", "mdl",
             "mdiag",
             "glmm", "glmm"),
    to   = c("desc", "ovi_sqrt", "mdl", "glmm",
             "season_ct",
             "mdiag", "mcoef",
             "mfit",
             "gtbl", "gseason_gg")
  ),
  stacks = stacks(
    data = new_dock_stack(
      c("ovi", "desc", "ovi_sqrt", "season_ct"),
      name = "Data", color = "#2563eb"
    ),
    # ONE model stack, not two. The board used to split "the easy model" from
    # "the published model" and hang a third group of diagnostics off it. That
    # was the right shape when the comparison was the whole exhibit; now that
    # each arm is a fit and a table, the split cost more to explain than it
    # bought.
    model = new_dock_stack(
      c("mdl", "mdiag", "mfit", "glmm", "mcoef", "gtbl", "gseason_gg"),
      name = "Model", color = "#7c3aed"
    )
  ),
  extensions = list(
    # THE OUTLINE IS THE PIPELINE VIEW, and the only one. The DAG drew the same
    # graph with the same edges and was harder to read at this size: a board of
    # seventeen blocks spreads wide enough that the interesting part is offscreen
    # unless you pan. The outline says the same thing as a list, grouped by
    # stack, and it is already on the Data view where the demo starts. Two
    # answers to one question is a worse demo than one answer.
    blockr.outline::new_outline_extension(),
    blockr.assistant::new_assistant_extension(),
    # THE REPORT EXTENSION, not the outline. Same board, a different reading of
    # it: an ordered list of items, each either a block or a paragraph of
    # markdown, with two switches per block for quarto's `echo` / `output`
    # pair. The outline narrated a board; this builds a document, and the
    # document's source is a deliverable in its own right.
    #
    # WHY THE PROSE MOVED. Under the outline every block carried a
    # `description` and the document was assembled from them, so a paragraph
    # could only ever sit against exactly one block. Here text is its own kind
    # of item, so a paragraph can introduce two figures at once, or close a
    # section, or open the document. That is how Matteo's own modelling report
    # reads: a paragraph, then the code it describes, then what the code
    # printed.
    #
    # CODE IS ON WHEREVER THE CODE IS THE POINT. The default when a block is
    # added is output-only, which is right for an exhibit (a table nobody wants
    # to see built) and wrong for the two model fits and the two figures, which
    # are the things a reader would want to copy. `code = TRUE, output = FALSE`
    # is the third state, used for the steps that make something the next chunk
    # shows.
    blockr.outline::new_report_extension(
      # The PAPER'S OWN TITLE. This document is the paper's first analysis
      # refitted, not a new study with a clever name.
      title = "Effectiveness of integrated Aedes albopictus management in southern Switzerland",
      settings = list(
        toc = TRUE,
        number_sections = TRUE,
        # Headings are editorial and written in the text items below; block
        # names make good panel labels and bad section titles.
        block_titles = "none",
        warnings = FALSE
      ),
      # THE NARRATIVE IS THE AUTHORS', NOT OURS. Every paragraph below that is
      # not explicitly marked as a note is quoted from Ravasi et al. (2021),
      # which is open access under CC BY 4.0 and so may be reused with
      # attribution and an indication of changes. That is the point of the
      # exercise: the document a reader takes away should be the published
      # study, with our code underneath it producing the published numbers --
      # not a paraphrase of the study in our voice, which is what this was
      # until 2026-08-19.
      #
      # Two honesty constraints follow from that, and both are load-bearing:
      #   * The paper reports THREE analyses (eggs per ovitrap, adult females
      #     per GAT, and a 2012/2013 comparison by hurdle model). This board
      #     refits ONLY the first. The lead note says so, or the quoted
      #     abstract would have the document claiming work it does not show.
      #   * The Poisson arm is OURS and appears in no part of the paper. It
      #     lives in an appendix, labelled as not part of the published
      #     analysis, so nobody can mistake it for something the authors did.
      items = list(
        # ONE PARAGRAPH, THEN THE CITATION. Everything load-bearing survives:
        # what was refitted, who it is with, what is quoted and what is ours,
        # the licence the quoting rests on, and that no number here is
        # preloaded. What went is the sentence naming the two analyses this
        # document does not reproduce, which "the first of the three" already
        # says.
        list(text = paste0(
          "> **About this document.** The first of the three analyses in ",
          "Ravasi et al. (2021), refitted from the data published alongside ",
          "the paper. It is joint work with **Matteo Tanadini** (Zurich Data ",
          "Scientists), a co-author of that paper, prepared for the Swiss ",
          "Statistics Meeting 2026. The narrative is quoted from the paper ",
          "and reused under ",
          "[CC BY 4.0](https://creativecommons.org/licenses/by/4.0/); the ",
          "code, the appendix and the notes marked as notes are ours. Nothing ",
          "here is preloaded: the first chunk downloads Additional file 2 ",
          "from the publisher, and every number below is computed from it ",
          "when the document is rendered, with readr, dplyr, ggplot2, glmmTMB ",
          "and broom.mixed, and no package of ours.\n\n",
          "> Ravasi D, Parrondo Monton D, Tanadini M, Flacio E (2021). ",
          "*Effectiveness of integrated Aedes albopictus management in ",
          "southern Switzerland*. **Parasites & Vectors** 14, 405. ",
          "<https://doi.org/10.1186/s13071-021-04903-2>"
        )),

        list(text = paste0(
          "## Background\n\n",
          "The exotic invasive tiger mosquito, *Aedes albopictus*, appeared in ",
          "southern Switzerland in 2003. The spread of the mosquito has been ",
          "surveyed constantly since then, and an integrated vector management ",
          "(IVM) has been implemented to control its numbers. The control ",
          "measures focus on the aquatic phase of the mosquito with removal of ",
          "breeding sites and applications of larvicides in public areas ",
          "whereas private areas are reached through extensive public ",
          "information campaigns. Here, we evaluated the efficacy of the IVM.\n\n",
          "Since all the municipalities with *Ae. albopictus* in southern ",
          "Switzerland are currently implementing the IVM, Italian ",
          "municipalities just across the Swiss-Italian border, where *Ae. ",
          "albopictus* is present but no coordinated intervention programme is ",
          "in place, served as control."
        )),

        list(text = paste0(
          "## Study sites and design\n\n",
          "The field surveys were carried out in six small to medium-sized ",
          "towns (3000 to 16,000 inhabitants) around the border area between ",
          "Ticino in Switzerland and the Lombardy region in Italy. The ",
          "municipalities are located in the historical-geographical region of ",
          "Insubria. The six municipalities surveyed are located within a ",
          "radius of 7 km, have similar dimension and urban structure, with a ",
          "small-town centre surrounded by residential areas, and similar ",
          "climatic and altitudinal characteristics (255-414 m a.s.l.).\n\n",
          "Three municipalities (i.e. Balerna, Coldrerio and Mendrisio) in the ",
          "Mendrisiotto district in Ticino follow systematically the cantonal ",
          "IVM since 2009 with monthly or weekly treatments of catch basins ",
          "with diflubenzuron- or Bti-based products, respectively. We ",
          "categorized these three municipalities as \"intervention\" areas. ",
          "The three municipalities in the provinces of Como (i.e. Maslianico ",
          "and Uggiate-Trevano) and Varese (i.e. Malnate), in Lombardy, to our ",
          "knowledge, did not follow an IVM and only applied adulticides ",
          "irregularly. They were, therefore, categorized as ",
          "\"non-intervention\" areas.\n\n",
          "The territory of each municipality was divided into a grid of ",
          "250 x 250 m cells. Six cells, called sampling sites, were selected ",
          "at random in urban context in each municipality. An ovitrap and a ",
          "Gravid *Aedes* Trap were placed in each sampling site at a distance ",
          "of 20-100 m from each other."
        )),
        list(block = "ovi", code = TRUE, output = FALSE),

        list(text = paste0(
          "In 2019, egg counts per ovitrap per inspection rounds of about 14 ",
          "days ranged from 0 to 513 in the municipalities that were part of ",
          "the intervention area (i.e. Balerna, Coldrerio and Mendrisio) and ",
          "from 0 to 2117 in the municipalities not following a defined ",
          "management plan (i.e. Malnate, Maslianico and Uggiate-Trevano). ",
          "Mean *Ae. albopictus* egg counts were consistently higher in the ",
          "non-intervention municipalities."
        )),
        list(block = "desc", code = FALSE, output = TRUE),

        list(text = paste0(
          "The first eggs in the season were found already in the first period ",
          "of the survey in late May to early June. In the non-intervention ",
          "municipalities, there was a steady increase in the number of eggs ",
          "with a peak in August, followed by a decrease in September and ",
          "October, indicating the end of the reproductive season. In the ",
          "intervention municipalities, the increase in the number of eggs was ",
          "much more contained compared to the non-intervention ",
          "municipalities, without an evident peak."
        )),

        list(text = paste0(
          "## Statistical analysis\n\n",
          "The graphical analysis was performed with the ggplot2 package. All ",
          "three analyses were performed with the `glmmTMB` function from the ",
          "glmmTMB package. Inference was performed with likelihood ratio ",
          "tests (for *P*-values) and profiling likelihood methods (to ",
          "estimate confidence intervals). The level of significance was set ",
          "at alpha = 0.05. Different distributional families and non-nested ",
          "models were compared with information criteria. Model assumptions ",
          "were assessed via usual residuals analyses. Quadratic effects were ",
          "modelled via orthogonal polynomials.\n\n",
          "### First model\n\n",
          "The response variable \"number of eggs\" (`No..eggs.AEDES`) was ",
          "modelled with a generalised mixed-effects model. In particular, to ",
          "account for the nature of the data, a negative binomial ",
          "distribution was assumed. This allowed accounting for the fact that ",
          "we are dealing with count data and that overdispersion is present. ",
          "The predictor of main interest \"AREA\" defined whether the trap was ",
          "to be found in a sampling site under IVM (i.e. in intervention ",
          "area) or not (i.e. in non-intervention area) and was included as a ",
          "fixed effect. The other predictors were \"Municipality\", ",
          "\"TRAP.ID.fac\" (i.e. trap identity), \"Day of the year\", ",
          "\"No..Days.ovitrap.in.field\" (i.e. number of days that the trap was ",
          "deployed in the field) and \"Altitude\" (i.e. altitude of the trap ",
          "in meters a.s.l.). Municipality and TRAP.ID.fac were taken as ",
          "random effects. No..Days.ovitrap.in.field was included to account ",
          "for the \"exposure\" effect, as not all traps stayed exactly 14 days ",
          "in the field (range 10 to 19 days). Traps that are left longer in ",
          "the field are expected to contain more eggs. The seasonal effect of ",
          "time (i.e. date when ovitrap collected, \"Day of the year\") was ",
          "modelled with a quadratic effect."
        )),
        list(block = "glmm", code = TRUE, output = FALSE),
        list(block = "gtbl", code = FALSE, output = TRUE),
        # THE ONE FIGURE IN THE DOCUMENT, SIZED FOR THE DOCUMENT. The board's
        # default 8 x 4.5 is a reasonable figure and a bad two-panel figure:
        # split across facets each panel gets under four inches, so the season
        # is squashed and the curve reads as a bump. `column: page` (what
        # `full_width` emits) takes the width quarto otherwise reserves for the
        # margin and the table of contents, and a 12 x 4.5 device gives the two
        # panels room to be the shape the data is.
        list(block = "gseason_gg", code = FALSE, output = TRUE,
             fig_width = 12, fig_height = 4.5, full_width = TRUE),

        list(text = paste0(
          "*Note (ours).* The coefficients above are exponentiated, so each ",
          "one is the factor by which the egg count changes: **3.81** times ",
          "more eggs where there is no programme. Read the two seasonal rows ",
          "with care. They are orthogonal polynomial contrasts, and a ",
          "multiplier on a contrast is hard to interpret; they are in the ",
          "table because they are in the model. The figure is the same fit ",
          "drawn on the scale of the data: one grey line per trap, the ",
          "fitted curve and its 95% band by area."
        )),

        list(text = paste0(
          "## Results\n\n",
          "The model fitted well and the fitted values agreed with the raw ",
          "data. The effect of AREA (i.e. intervention vs. non-intervention) ",
          "was clearly present and biologically relevant (*P* < 0.0001). In ",
          "non-intervention sites there were about four times (3.8) more eggs ",
          "than in intervention sites (95% confidence interval, CI: ",
          "2.7-5.4). The estimated variability of the random effects indicated ",
          "that there was very little variation among municipalities.\n\n",
          "There seemed to be some variation among ovitraps. To quantify these ",
          "differences among traps we looked at the two most extreme estimated ",
          "conditional modes: the \"worst\" ovitrap had -40% eggs with respect ",
          "to an \"average\" ovitrap; the \"best\" ovitrap had +140% eggs with ",
          "respect to an \"average\" ovitrap. Altitude did not seem to play a ",
          "relevant role (*P* = 0.607). The number of days of trap deployment ",
          "in the field had a significant effect (the model predicted about ",
          "10% more eggs for each additional day the ovitrap was deployed in ",
          "the field; *P* = 0.003)."
        )),

        list(text = paste0(
          "## Conclusions\n\n",
          "The results of the survey strongly support the efficacy of the IVM ",
          "programme implemented in southern Switzerland compared to ",
          "municipalities without defined control measures. With the constant ",
          "implementation of an IVM, it appears possible to contain the ",
          "numbers of *Ae. albopictus* at a manageable level, reducing the ",
          "nuisance for the human population and the risk of arbovirus ",
          "epidemics."
        )),

        list(text = paste0(
          "## Appendix: the obvious model\n\n",
          "*This section is not part of the published analysis.* We fitted it ",
          "ourselves, to compare with the model above.\n\n",
          "Egg counts are counts, so the obvious model is a Poisson ",
          "regression of eggs on area: one response, one predictor."
        )),
        list(block = "mdl", code = TRUE, output = FALSE),
        list(block = "mcoef", code = FALSE, output = TRUE),

        # SHORT ON PURPOSE. This paragraph used to explain why the Poisson
        # interval is too narrow: overdispersion, ten readings of one trap
        # counted as ten facts, nothing held constant for the season. All of it
        # is true (the residual deviance really is about 180x its df), and it
        # is a paragraph of statistics in a document whose argument is
        # somewhere else. The appendix shows the quick model and moves on.
        list(text = paste0(
          "It puts the effect at about 3.6 times more eggs, close to the ",
          "published 3.8. The interval is much narrower: 3.51 to 3.67, ",
          "against 2.7 to 5.4. Use the published model."
        ))
      )
    )
  ),
  grids = list(
    Data = dock_grid(
      group("ovi", "desc", sizes = c(1, 2)),
      "season_ct",
      ext("outline"),
      orientation = "horizontal", sizes = c(1, 1, 1)
    ),
    # The comparison, on one view. LEFT: the formula widget over the two ways
    # of reading its fit -- the card and the gtsummary table, tabbed, because
    # they say the same thing to different audiences. MIDDLE: the predicted
    # curve, with the published model and the fitted frame behind it. RIGHT:
    # the outline, so the pipeline stays in sight while the model moves.
    Model = dock_grid(
      group("mdl", "mcoef", sizes = c(2, 3)),
      "mfit",
      # `panels(active =)` rather than reordering: the tabs read fit, then
      # table, then picture, which is the order the work happens in, while the
      # PICTURE is what the view opens on. The print-out of a glmmTMB is a wall
      # of variance components -- honest, and not what anyone should meet
      # first.
      panels("glmm", "gtbl", "gseason_gg", active = "gseason_gg"),
      orientation = "horizontal", sizes = c(35, 27, 38)
    ),
    # gseason_gg is deliberately NOT given a column here, even though the
    # document uses it. Listing one block in two grids (it already has a panel
    # on Model) renders this whole view BLANK -- report, figure and outline
    # all three. Verified against the pre-change board 2026-08-22.
    #
    # The reason it probably does not need one: the mount rule that forced
    # document figures onto this view applies to CHART and ggplot BLOCKS, whose
    # code is only known once the widget has drawn, whereas a code block's
    # chunk is just its script. NOT CONFIRMED END TO END -- the builder lists
    # the figure and no "waiting for R code" placeholder appears anywhere, but
    # the exported qmd was not read back. Download it once and check the
    # ggplot2 chunk is there before relying on this.
    Report = dock_grid(
      ext("report"), ext("outline"),
      orientation = "horizontal", sizes = c(2, 1)
    ),
    Workflow = dock_grid(
      ext("outline"), ext("assistant"), sizes = c(2, 1)
    )
  ),
  active = "Model"
)

serve(board, plugins = custom_plugins(manage_project()))
