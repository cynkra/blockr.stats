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
  "blockr.dag",
  "blockr.dock",
  "blockr.io",        # the read block that fetches the published CSV
  "blockr.dplyr",     # the three blocks that reshape the tidy coefficient frame
  "blockr.ggplot",    # the two document figures
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
for (pkg in c("glmmTMB", "broom.mixed")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("aedes-ivm needs the ", pkg, " package: install.packages('", pkg, "')",
         call. = FALSE)
  }
}
# NOT attached. Nothing on this board calls a bare `tidy()`: the coefficient
# step names `broom.mixed::tidy()` in full, which loads the namespace and so
# registers the glmmTMB method by itself. That is deliberate -- it is what lets
# the downloaded qmd render in a session that has never heard of this app.

options(
  blockr.dock_is_locked = FALSE,
  blockr.tabular_display = blockr.ui::html_table_display,
  blockr.background_construction_delay = 0,
  blockr.visible_extensions = c("dag", "minidag")
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
glmm_script <- 'random_trap <- TRUE

glmmTMB::glmmTMB(
  No..eggs.AEDES ~ AREA +
    poly(Day.ovitrap.collected, degree = 2) +
    scale(ALTITUDE) +
    No..Days.ovitrap.in.field +
    (1 | MUNICIPALITY) +
    if (random_trap) (1 | TRAP.ID.fac),
  family = glmmTMB::nbinom1,
  data = data
)
'

# The per-observation frame the two diagnostic charts feed on.
#
# WHY THIS IS NOT A `new_broom_block(output = "augment")`, which is the obvious
# choice and what stats-101 uses. The published formula wraps two
# predictors in `poly()` and `scale()`, and those come back out of the model
# frame as MATRIX columns. broom hands them straight through, and a data frame
# carrying a 327x2 matrix in one cell is not something a table or a chart block
# can draw. Four named columns of plain vectors is.
#
# Note what this block's INPUT is: the fitted model, not a data frame. Blocks
# pass R objects, not tables, so a code block downstream of a model gets the
# model. `data` is the input's name inside the script and is replaced by the
# upstream block's name on export, so the chunk in the document reads
# `stats::predict(glmm, type = "response")` -- which is what you would have
# written by hand.
# Fully qualified on purpose: see the `gcoef` block for why `broom.mixed::` and
# not `broom::` is what makes the downloaded document render on its own.
tidy_script <- 'broom.mixed::tidy(data, conf.int = TRUE)
'

# The appendix's exhibit. `new_model_summary_block()` draws a far better card
# in the app, but it emits `blockr.stats::model_summary(mdl)` -- the last call
# in the document that would send a reader looking for one of our packages.
# `exponentiate = TRUE` does on the glm exactly what the mutate does on the
# GLMM, so the two arms are now read the same way as well as fitted the same
# way. The card stays on the Model view; the document gets this.
poisson_script <- 'broom::tidy(data, exponentiate = TRUE, conf.int = TRUE)
'

diag_script <- 'data.frame(
  stats::model.frame(data)[c("No..eggs.AEDES", "AREA")],
  fitted = stats::fitted(data),
  resid = stats::residuals(data, type = "pearson")
)
'
# A DATA BLOCK THAT READS A URL AND SAYS SO IN THE EXPORT.
#
# blockr has no block for "load this data with exactly this call". The nearest
# things all fail differently:
#   * `new_dataset_block()` emits `blockr.stats::aedes_ovitraps`, which ties the
#     exported document to a package no reader has.
#   * `blockr.io::new_read_block()` downloads a URL to a temp file and builds
#     its expression around THAT path, so the document carries
#     `/tmp/RtmpXXXX/fileYYYY.csv` -- see
#     _inbox/2026-08-19-io-read-block-exports-temp-path-for-urls.md.
#   * `blockr.core::new_fixed_block()` emits its expression verbatim, which is
#     right, but it is a TRANSFORM block: it has a `data` input, nothing is
#     linked into it at the root of a board, and so it never evaluates. Used
#     here on 2026-08-19 it left every panel on "Waiting for a data input"
#     while still producing a perfectly good qmd -- because the report renders
#     the emitted code in a fresh session and never asks the board whether it
#     ran. A dead board and a working document look identical from the download.
#
# So: twelve lines of `new_data_block()`, which is the supported way to make one.
# `expr` is the call, verbatim, and `state` carries the url so the board
# restores through `do.call(ctor, payload)`.
new_url_csv_block <- function(url = character(), ...) {
  blockr.core::new_data_block(
    function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        list(
          expr = shiny::reactive(
            bquote(readr::read_csv(.(url), show_col_types = FALSE))
          ),
          state = list(url = url)
        )
      })
    },
    function(id) {
      shiny::tagList(
        shiny::tags$p(
          class = "text-muted small",
          "Reads ", shiny::tags$code(url)
        )
      )
    },
    class = "url_csv_block",
    ...
  )
}

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
    # WHY A `new_fixed_block()` AND NOT `blockr.io::new_read_block()`, which is
    # the block actually built for this. The read block downloads a URL to a
    # temp file and then builds its expression around THAT path, so the report
    # came out carrying
    # `readr::read_csv(file = "/tmp/RtmpwhbpHa/filef70d45de23aa.csv")` -- a
    # path that exists on nobody's machine, in a document whose whole point is
    # that you can run it on yours. Verified 2026-08-19; filed as
    # _inbox/2026-08-19-io-read-block-exports-temp-path-for-urls.md. A fixed
    # block emits its expression verbatim, which is all this needs.
    #
    # It is a TRANSFORM block with nothing linked into it, which sounds wrong
    # and works: the expression ignores its input, so the board treats it as a
    # source. Worth knowing, because it is the only way currently available to
    # put an arbitrary hand-written data-loading call at the root of a board.
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
    # offline, which the URL does not. The board simply prefers the public
    # source, because the board's job is to produce that document.
    ovi = new_url_csv_block(
      url = paste0(
        "https://static-content.springer.com/esm/",
        "art%3A10.1186%2Fs13071-021-04903-2/MediaObjects/",
        "13071_2021_4903_MOESM2_ESM.csv"
      ),
      block_name = "Ovitrap readings 2019 (Additional file 2)"
    ),

    # -- Table 1 -----------------------------------------------------------
    # The paper's Table 1, as a plain dplyr summarise: min, median, mean and
    # max eggs per trap in each of the six towns. Run it and the six means come
    # out at the published 56.8 / 80.1 / 59.3 against 261.2 / 218.6 / 223.8,
    # which is a check anyone in the room can do.
    #
    # THIS USED TO BE `new_summary_table_block()` FEEDING A `new_table_block()`,
    # and the swap is about the exported document. That pair emits
    # `with(list(data = ovi), blockr.viz::summary_table(data, vars = ...,
    # sections = character(0), ...))` and then
    # `blockr.viz::static_exhibit(dplyr::filter(blockr.viz::as_annotated_df(desc),
    # TRUE))` -- correct, and three blockr calls plus a no-op filter where a
    # reader expects one line of dplyr. The styled exhibit buys nothing here:
    # this table is six rows of numbers, `df-print: kable` renders it, and the
    # chunk now reads as something you would have typed.
    desc = blockr.dplyr::new_summarize_block(
      summaries = list(
        list(type = "simple", name = "min_eggs",
             func = "min", col = "No..eggs.AEDES"),
        list(type = "simple", name = "median_eggs",
             func = "median", col = "No..eggs.AEDES"),
        list(type = "simple", name = "mean_eggs",
             func = "mean", col = "No..eggs.AEDES"),
        list(type = "simple", name = "max_eggs",
             func = "max", col = "No..eggs.AEDES")
      ),
      by = list("MUNICIPALITY"),
      block_name = "Table 1: eggs per trap by town"
    ),

    # -- Arm 1: the no-code model ------------------------------------------
    # A Poisson glm, authored entirely in the formula widget. `visible =
    # "inputs"` drops the R print; the card below is the picture.
    mdl = `attr<-`(
      new_model_block(
        model_type = "poisson",
        formula = "No..eggs.AEDES ~ AREA",
        block_name = "Poisson glm"
      ),
      "visible", "inputs"
    ),
    summ = `attr<-`(
      new_model_summary_block(block_name = "Area effect (glm)"),
      "visible", "outputs"
    ),
    mcoef = new_code_block(
      script = poisson_script, block_name = "Area effect (glm), tidied"
    ),

    # -- Arm 2: the escalation ---------------------------------------------
    # Custom R in the board. Returns a glmmTMB object; the code block
    # renders any unrecognised object as its print(), which for a glmmTMB is
    # the formula, the family and the variance components -- a reasonable
    # panel, and the table below is the real output.
    glmm = new_code_block(
      script = glmm_script, block_name = "Negative binomial GLMM (custom R)"
    ),

    # THE ONE-LINE STEP THAT MAKES THE DOCUMENT SELF-CONTAINED.
    #
    # This was a `new_broom_block()`, which is the natural blockr answer and
    # which works perfectly well inside the app. It emits
    # `broom::tidy(glmm, conf.int = TRUE)`, and broom has no `tidy()` method
    # for a glmmTMB fit -- broom.mixed supplies one, as an S3 registration that
    # only exists once broom.mixed is LOADED. The app has it loaded, so the
    # panel was fine and the app-side render was fine. The downloaded qmd was
    # not: rendered in a fresh R session it died at this chunk with
    # "No `tidy()` method for objects of class <glmmTMB>" (verified
    # 2026-08-19). A report that only renders inside the app that wrote it is
    # not a report.
    #
    # A code block naming `broom.mixed::tidy()` outright fixes it, because `::`
    # loads the namespace and that registration is what dispatch needs. No
    # `library()` call anywhere, and the chunk still reads as one line of R.
    # Filed as _inbox/2026-08-19-outline-report-qmd-has-no-setup-chunk.md:
    # the emitter has no way to declare a document's packages, and
    # fully-qualified calls cover everything except S3 registration.
    gcoef = new_code_block(
      script = tidy_script, block_name = "Tidy coefficients"
    ),

    # THE TIDY OUTPUT IS AN ORDINARY DATA FRAME, so ordinary blocks work on it.
    # These two are the argument in miniature: no special "model results"
    # machinery, just a filter and a mutate.
    #
    # tidy() returns the variance components alongside the fixed effects. They
    # are worth a look (the municipality SD comes out at essentially zero,
    # which is the paper's "very little variation among municipalities"), but
    # they are not the exhibit, and broom.mixed's confidence bounds on those
    # rows are not to be trusted.
    # `type = "expr"` conditions, not the value picker. The picker builds
    # `effect %in% "fixed"`, which is right and reads like generated code; an
    # expr condition is emitted verbatim, so the chunk says what a person would
    # have written. It also lets one block do both jobs -- keep the fixed
    # effects, and drop the two seasonal polynomial contrasts, for which a
    # multiplier is meaningless. Dropping them here is what lets the next block
    # be three clean lines instead of three `ifelse(grepl(...))` calls.
    gfix = blockr.dplyr::new_filter_block(
      conditions = list(
        list(type = "expr", expr = 'effect == "fixed"'),
        list(type = "expr", expr = '!grepl("^poly", term)')
      ),
      block_name = "Interpretable fixed effects"
    ),

    # A log-link coefficient is a multiplier once exponentiated, and the
    # multiplier is the finding: how many times more eggs where nobody is
    # controlling the mosquito. This block is what turns a model output into
    # a sentence a public health officer can act on.
    # A log-link coefficient is a multiplier once exponentiated, and the
    # multiplier is the finding: how many times more eggs where nobody is
    # controlling the mosquito. Rounded here rather than in the table block,
    # because `new_table_block(digits = )` rounds the panel and NOT the
    # rendered report (see _inbox/2026-08-19-viz-table-digits-ignored-on-export.md),
    # and because the report and the panel should not be able to disagree.
    gratio = blockr.dplyr::new_mutate_block(
      mutations = list(
        list(name = "times_more", expr = "round(exp(estimate), 2)"),
        list(name = "ci_low",     expr = "round(exp(conf.low), 2)"),
        list(name = "ci_high",    expr = "round(exp(conf.high), 2)")
      ),
      block_name = "As a multiplier"
    ),

    # Down to the four columns worth projecting. Not tidiness for its own sake:
    # a tidy frame is eleven columns wide, the table block renders them in
    # order, and `times_more` -- the entire point of the exhibit -- lands off
    # the right-hand edge where nobody in row six will ever see it. Verified on
    # the live board before this block existed.
    gsel = blockr.dplyr::new_select_block(
      columns = list("term", "times_more", "ci_low", "ci_high"),
      block_name = "The four columns that matter"
    ),
    gtbl = new_table_block(digits = 2L, block_name = "Area effect (GLMM)"),

    # -- The fit -----------------------------------------------------------
    gdiag = new_code_block(
      script = diag_script, block_name = "Fitted values & residuals"
    ),

    # echarts, not ggplot: these redraw whenever the GLMM refits (untick the
    # trap random effect and watch), and a chart block pushes data to a browser
    # where a ggplot block round-trips a PNG through R. `report = FALSE` -- the
    # document gets the ggplot pair instead.
    gfit = `attr<-`(
      blockr.viz::new_chart_block(
        chart_type = "scatter", x = "No..eggs.AEDES", y = "fitted",
        color = "AREA", identity_line = TRUE,
        block_name = "Predicted vs actual"
      ),
      "visible", "inputs"
    ),
    gres = `attr<-`(
      blockr.viz::new_chart_block(
        chart_type = "scatter", x = "fitted", y = "resid",
        color = "AREA", hlines = 0,
        block_name = "Residuals vs fitted"
      ),
      "visible", "inputs"
    ),

    # -- The document pair: blockr.ggplot blocks ----------------------------
    # Beside the outline in Report, and ggplot because the rendered chunk
    # should read as ggplot2 -- a recipe a reader can lift. Both keep their
    # settings band: a ggplot block hidden to `visible = "outputs"` on a
    # non-startup panel builds no mapping and draws nothing.
    season_gg = blockr.ggplot::new_ggplot_block(
      type = "point",
      x = "Day.ovitrap.collected", y = "no.eggs.normalised.14.days",
      color = "AREA",
      block_name = "The season, by area"
    ),
    # The residual check the DOCUMENT gets. `gres` next to it is the same
    # picture in echarts for the dashboard, and it is deliberately NOT in the
    # report: a chart block reports its code to the builder only while its panel
    # is MOUNTED, and `gres` lives on the un-fronted tab of a Model-view slot.
    # Put it in the document and its chunk comes out as
    # "# gres: waiting for R code to be generated" -- and the HTML download then
    # waits on it forever. Verified 2026-08-19, which is how this block exists.
    resid_gg = blockr.ggplot::new_ggplot_block(
      type = "point", x = "fitted", y = "resid", color = "AREA",
      block_name = "Residuals vs fitted"
    ),
    map_gg = blockr.ggplot::new_ggplot_block(
      type = "point", x = "WGS84.LNG", y = "WGS84.LAT",
      color = "AREA",
      block_name = "The trap network"
    )
  ),
  links = links(
    from = c("ovi", "ovi", "ovi", "ovi", "ovi",
             "mdl", "mdl",
             "glmm", "glmm",
             "gcoef", "gfix", "gratio", "gsel",
             "gdiag", "gdiag", "gdiag"),
    to   = c("desc", "mdl", "glmm", "season_gg", "map_gg",
             "summ", "mcoef",
             "gcoef", "gdiag",
             "gfix", "gratio", "gsel", "gtbl",
             "gfit", "gres", "resid_gg")
  ),
  stacks = stacks(
    data = new_dock_stack(
      c("ovi", "desc", "season_gg", "map_gg"),
      name = "The data", color = "#2563eb"
    ),
    simple = new_dock_stack(
      c("mdl", "summ", "mcoef"), name = "The easy model", color = "#7c3aed"
    ),
    published = new_dock_stack(
      c("glmm", "gcoef", "gfix", "gratio", "gsel", "gtbl"),
      name = "The published model", color = "#059669"
    ),
    fit = new_dock_stack(
      c("gdiag", "gfit", "gres", "resid_gg"),
      name = "The fit", color = "#d97706"
    )
  ),
  extensions = list(
    blockr.dag::new_dag_extension(),
    blockr.outline::new_minidag_extension(),
    blockr.outline::new_slides_extension(
      title = "Does mosquito control work? Evidence from across the border",
      # Presentation order, not evaluation order: open on the answer.
      slides = c("gtbl", "season_gg", "map_gg")
    ),
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
        list(text = paste0(
          "> **About this document.** This is the first of the three analyses ",
          "in Ravasi et al. (2021), refitted from the data the authors ",
          "published alongside the paper. It is joint work with **Matteo ",
          "Tanadini** (Zurich Data Scientists), a co-author of that paper, ",
          "prepared with him for the Swiss Statistics Meeting 2026.\n\n",
          "> The narrative below is quoted from the paper itself; the code, ",
          "the appendix, and notes marked as notes are ours. The paper is open ",
          "access under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/) ",
          "and is reused here on that basis:\n\n",
          "> Ravasi D, Parrondo Monton D, Tanadini M, Flacio E (2021). ",
          "*Effectiveness of integrated Aedes albopictus management in ",
          "southern Switzerland*. **Parasites & Vectors** 14, 405. ",
          "<https://doi.org/10.1186/s13071-021-04903-2>\n\n",
          "> The paper's other two analyses, of adult females caught in Gravid ",
          "*Aedes* Traps and of the 2012/2013 comparison, are not reproduced ",
          "here.\n\n",
          "> **Nothing here is preloaded.** The first chunk downloads ",
          "Additional file 2 from the publisher, and every number below is ",
          "computed from it when the document is rendered. Running this file ",
          "needs readr, dplyr, ggplot2, glmmTMB and broom.mixed, and no ",
          "package of ours."
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
        list(block = "map_gg", code = TRUE, output = TRUE),

        list(text = paste0(
          "In 2019, egg counts per ovitrap per inspection rounds of about 14 ",
          "days ranged from 0 to 513 in the municipalities that were part of ",
          "the intervention area (i.e. Balerna, Coldrerio and Mendrisio) and ",
          "from 0 to 2117 in the municipalities not following a defined ",
          "management plan (i.e. Malnate, Maslianico and Uggiate-Trevano). ",
          "Mean *Ae. albopictus* egg counts were consistently higher in the ",
          "non-intervention municipalities."
        )),
        list(block = "desc", code = TRUE, output = TRUE),

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
        list(block = "season_gg", code = TRUE, output = TRUE),

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
        list(block = "glmm", code = TRUE, output = TRUE),

        list(text = paste0(
          "*Note (ours).* The coefficients are on the log scale, so ",
          "exponentiating them turns each into the factor by which the egg ",
          "count changes. The table below keeps the terms that read that way. ",
          "The two seasonal terms are orthogonal polynomial contrasts, for ",
          "which a multiplier means nothing, so they are left out of it; they ",
          "are in the model summary above, on the scale they belong to."
        )),
        list(block = "gcoef", code = TRUE, output = FALSE),
        list(block = "gfix", code = TRUE, output = FALSE),
        list(block = "gratio", code = TRUE, output = FALSE),
        list(block = "gsel", code = TRUE, output = TRUE),

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
          "### Model assumptions\n\n",
          "*Note (ours).* \"Model assumptions were assessed via usual residuals ",
          "analyses.\" The paper does not print those diagnostics, so this is ",
          "our reconstruction of one: Pearson residuals against fitted values. ",
          "For a negative binomial the spread of the raw residuals grows with ",
          "the mean by construction, so raw residuals would show a funnel ",
          "whatever the model did."
        )),
        list(block = "gdiag", code = TRUE, output = FALSE),
        list(block = "resid_gg", code = TRUE, output = TRUE),

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
          "*This section is not part of the published analysis.* It is ours, ",
          "and it exists to show what the shortcut costs.\n\n",
          "Egg counts are counts, so the first reach is a Poisson regression ",
          "of count on area. It takes one response and one term to write."
        )),
        list(block = "mdl", code = TRUE, output = FALSE),
        list(block = "mcoef", code = TRUE, output = TRUE),

        list(text = paste0(
          "It puts the effect at about 3.6 times more eggs, with a 95% ",
          "interval from 3.51 to 3.67. The estimate is close to the published ",
          "one; the interval is not. A Poisson assumes the variance equals the ",
          "mean, and here the residual deviance is about 180 times its degrees ",
          "of freedom. It also counts ten readings of one trap as ten ",
          "independent observations, and holds nothing constant for a season ",
          "that peaks in August. The published model's interval, 2.7 to 5.4, ",
          "is roughly seventeen times wider, and it is the one the design ",
          "supports."
        ))
      )
    )
  ),
  grids = list(
    # The comparison IS the view. Left column: the no-code arm, formula widget
    # over coefficient card. Right column: the published arm's effect table on
    # top, its diagnostics tabbed below. Tabbing is free there -- both charts
    # are `report = FALSE`, so nothing depends on the un-fronted one being
    # mounted.
    Model = dock_grid(
      group("mdl", "summ", sizes = c(3, 4)),
      group("gtbl", c("gfit", "gres"), sizes = c(3, 4)),
      orientation = "horizontal", sizes = c(1, 1)
    ),
    # A real SPLIT, not tabs: an unmounted ggplot block reports no code to the
    # outline, and the Download then waits on it forever. The minidag TABS with
    # the outline because extensions are exempt from that rule -- it binds
    # blocks. `panels()`, not `c()`: `ext()` returns a panel_ref object and
    # `c()` on two of them dies at construction with "Unknown layout node
    # type: NULL".
    # FOUR columns, and the fourth is not decoration. Every ggplot the document
    # uses has to be MOUNTED while the report builder is reading the board, or
    # its chunk comes out empty and the download hangs on it. So all three
    # document figures share this view with the builder, in real splits rather
    # than tabs.
    Report = dock_grid(
      panels(ext("report"), ext("slides")),
      "season_gg", "map_gg", "resid_gg",
      orientation = "horizontal", sizes = c(4, 2, 2, 2)
    ),
    Workflow = dock_grid(
      panels(ext("minidag"), ext("dag")), ext("assistant"), sizes = c(2, 1)
    )
  ),
  active = "Model"
)

serve(board, plugins = custom_plugins(manage_project()))
