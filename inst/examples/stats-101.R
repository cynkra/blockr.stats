# blockr.stats -- stats-101: reproducible modelling, end to end.
#
# One dataset (palmerpenguins), one question -- what drives a penguin's
# body mass? -- and one page you actually work on. Every step is a block,
# so the board IS the analysis script; nothing happens off to the side in
# a console.
#
# THREE views, because three is what there is to show:
#
#   Model     the dashboard. Left: the model block, cut down to its formula
#             widget (no R print -- the card next to it is the picture),
#             with the coefficient card under it. Right: one full-height
#             blockr.viz chart, predicted-vs-actual, with residuals-vs-
#             fitted on a tab behind it. Drop `bill_length_mm`, add `sex`,
#             switch to an interaction, and the coefficients and the chart
#             move at once. That live re-fit is the demo.
#   Report    the document and its figures: outline on the left, the two
#             ggplot figures (the relationship, the Q-Q check) side by side
#             beside it. Behind the outline, on a tab, the DECK BUILDER --
#             the same board read as four slides instead of as a document.
#             Also where the next panel goes -- open anything from the block
#             browser here and it lands in this row.
#   Workflow  the outline (the board as a list of rows with a rail for the
#             links), the DAG canvas on a tab behind it, assistant chat
#             beside both.
#
# The data prep (penguins -> complete cases, and the summary frame behind
# Table 1) gets NO view. Nobody wants to sit and look at it; it still runs,
# it shows up as nodes in Workflow, and it reaches the report. A block in no
# grid is hidden, not removed.
#
# Four things wrap around that workflow, and they are the point of this
# demo as much as the statistics are:
#
#   blockr.outline    narrates the board as a document. Every block carries
#                     a plain-language description; the Report panel renders
#                     the lot to html / pptx / pdf, code and figures
#                     included. This is the reproducible artefact.
#
#                     The same panel is also the workflow editor. It
#                     lists the blocks as rows with a commit-graph rail for
#                     the links, and does everything the DAG canvas does
#                     without asking anyone to read a graph layout, so it is
#                     mounted on Workflow as well as on Report.
#
#                     Beside it, the DECK BUILDER
#                     (`new_slides_extension()`): pick the blocks whose
#                     output is a slide, drag them into order, download pptx
#                     or HTML. Slide order is independent of evaluation
#                     order, so a deck can open on its conclusion.
#   blockr.assistant  a board-level chat that can read the board and add,
#                     link and lay out blocks. "Add an interaction between
#                     flipper length and species" is a sentence, not a
#                     refactor.
#   blockr.session    save / load / version the whole workflow (the
#                     manage_project plugin, top of the page). Reopen it
#                     tomorrow and the analysis is where you left it.
#   blockr.dag        the Workflow panel: the dependency graph of the
#                     analysis, which is the thing a methods section is
#                     usually trying to describe in prose.
#
# CHARTS AND PLOTS ARE DIFFERENT TOOLS HERE, on purpose:
#
#   blockr.viz chart blocks, on the dashboard. Echarts: R pushes data, the
#     browser redraws. On a page whose whole point is "edit the formula and
#     watch", that is the difference between snappy and sluggish -- a
#     ggplot block is a fresh R render round-tripped as a PNG on every
#     keystroke, which is the wrong instrument for a dashboard. Reference
#     geometry comes free too: `identity_line = TRUE` and `hlines = 0` are
#     arguments, not layers to hand-write.
#
#   blockr.ggplot blocks, in the document. What lands in the rendered
#     report is `ggplot2::ggplot(...) + geom_point()` -- a recipe a reader
#     can lift, not a call into a blockr package. (blockr.viz CAN export:
#     `report_call()` + `static_chart()` re-render a chart server-side, so
#     the old "viz is only a data passthrough" objection is dead. It just
#     puts `blockr.viz::static_chart(...)` in the chunk instead.)
#
# So the two charts are `report = FALSE` and the two ggplots carry the
# figures. Interactive where you work, legible where you publish.
#
# WHERE a ggplot lives is load-bearing, not taste. It reports its code to
# the outline only while its panel is MOUNTED, so a ggplot on a view of its
# own -- or on the un-fronted tab of a `c(...)` slot -- comes out of the
# report as "waiting for R code to be generated" and the Download waits on
# it forever (verified 2026-07-26; blockr.outline's own
# dev/example-penguins.R has the same hole). Sharing a real split with the
# outline, as here, means they are mounted exactly when it is looking.
# Related trap: a ggplot block set to `visible = "outputs"` on a
# non-startup panel draws nothing at all -- its mapping is built by the
# settings band, which never runs if it is never drawn.
#
# Deferred panels have a second, unrelated-looking hole of the same family: a
# dplyr filter block parked on a non-active view renders its conditions EMPTY
# (while filtering correctly). Not triggered here -- `clean` has no panel at
# all -- but see _scratch/handover/dplyr-filter-lazy-restore/ before putting
# one on a second tab.
#
# Run the shipped copy (installed packages):
#   source(system.file("examples/stats-101.R", package = "blockr.stats"))
# Run it against local source checkouts instead:
#   source("blockr.stats/dev/stats-101.R")     # sets dev_local <- TRUE
#
# This same file is what the blockr.cloud gallery serves: the deploy
# generator copies it verbatim to apps/stats-101/app.R (see
# blockr.deploy/shinyproxy-hetzner/gallery-manifest.yml, slug `stats-101`).
# Nothing below may assume a source checkout, a port, or a writable cwd.

# ---- Package loading (dual: installed vs local source) ---------------------
# `dev_local = FALSE` (the default, and what ships) attaches the INSTALLED
# packages with library(). The dev/stats-101.R wrapper sets it TRUE to load
# every blockr package from its LOCAL checkout with pkgload::load_all().
# One board, two loaders, no drift.
if (!exists("dev_local")) dev_local <- FALSE

blockr_pkgs <- c(
  "blockr.ui",
  "blockr.core",
  "blockr.dag",
  "blockr.dock",
  "blockr.dplyr",     # the complete-cases filter
  "blockr.ggplot",    # the two document figures
  "blockr.viz",       # the two dashboard charts + summary/table blocks
  "blockr.stats",     # model + model-summary + broom blocks (this example)
  "blockr.session",   # project save / load / versions
  "blockr.assistant", # board-level chat
  "blockr.outline",   # the narrated report
  # Not used by any block placed below -- attached so its blocks, above all the
  # Function block (write arbitrary R in a CodeMirror editor, with autocomplete
  # and inline AI diff), are in the "+ New" picker for whoever is driving the
  # demo. .onLoad calls register_extra_blocks(), so attaching is all it takes.
  "blockr.extra"
)

# Loaded FIRST, before any option value or board code touches a blockr
# namespace.
for (pkg in blockr_pkgs) {
  if (dev_local) pkgload::load_all(pkg, quiet = TRUE)
  else library(pkg, character.only = TRUE)
}

library(palmerpenguins)

options(
  blockr.dock_is_locked = FALSE,
  blockr.tabular_display = blockr.ui::html_table_display,
  # The outline projects the board into a document; with a construction
  # delay the report panel sits on "Evaluating..." until every block has
  # been visited. 0 = build eagerly.
  blockr.background_construction_delay = 0,
  # A panel opened from the block browser lands in the LAST ACTIVE GROUP,
  # and only the panels named here are exempt. The default is the DAG
  # alone; the outline is a workflow editor for the same reason, so a new
  # block must not stack on top of it either. Mount names, not classes.
  blockr.visible_extensions = c("dag", "outline")
)

# Report rendering. quarto executes the document in a FRESH R session, which
# is the right default -- it proves the report depends on nothing but
# installed packages, which is exactly true of the deployed image. It is the
# wrong thing under pkgload::load_all(), where the blockr namespaces exist in
# this process and nowhere else, so `blockr.stats::model_summary()` in the
# coefficients chunk dies with "not an exported object". Knit in-process
# there, and only there.
if (dev_local) options(blockr.outline.execute = "in-process")

# The assistant's chat client resolves through the board's `llm_model`
# option, which reads `blockr.chat_function`. Set it BEFORE the extension is
# constructed (below), or the option latches onto ellmer's bare default.
#
# Only if nothing has set it already. On the public gallery the spec mounts
# an LLM gateway as R_PROFILE_USER, which installs a rate- and spend-capped
# chat function before this file runs; clobbering it here would route the
# demo around the cap.
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

# ---------------------------------------------------------------- board ----

board <- new_dock_board(
  blocks = c(
    # -- The data ----------------------------------------------------------
    # 344 penguins, 3 species, 4 body measurements.
    peng = new_dataset_block(
      dataset = "penguins", package = "palmerpenguins",
      block_name = "Palmer penguins"
    ),

    # A handful of birds are missing a measurement or a sex. Restrict to
    # complete records so the table, the model and the diagnostics all
    # describe the SAME 333 birds -- otherwise each step silently drops a
    # different set of rows and the numbers stop reconciling.
    #
    # Done with the filter block's own NA handling (exclude the <NA> entry
    # in the value picker), which generates the plain !is.na(...) filter.
    clean = blockr.dplyr::new_filter_block(
      conditions = list(
        list(type = "values", column = "body_mass_g",
             values = list("<NA>"), mode = "exclude"),
        list(type = "values", column = "flipper_length_mm",
             values = list("<NA>"), mode = "exclude"),
        list(type = "values", column = "bill_length_mm",
             values = list("<NA>"), mode = "exclude"),
        list(type = "values", column = "sex",
             values = list("<NA>"), mode = "exclude")
      ),
      block_name = "Complete cases"
    ),

    # -- Table 1 -----------------------------------------------------------
    # Mean (SD) of each measurement, one column per species. The summary
    # block emits the annotated "Table 1" frame; the table block renders it
    # (sections, indents, per-species spanners) and exports as a flextable.
    # Neither gets a panel. Table 1 is a report deliverable, not a thing to
    # sit and stare at, and it is right there in the document on the left of
    # the Report view. Unlike a ggplot block it needs no panel to be part of
    # the report -- it is R-side, so it reports its code whether or not
    # anyone is looking. The right-hand column is reserved for the two that
    # do need mounting.
    desc = new_summary_table_block(
      vars = c("body_mass_g", "flipper_length_mm", "bill_length_mm"),
      by = "species", stats = "mean_sd", add_overall = TRUE,
      block_name = "Describe: body measurements"
    ),
    desc_tbl = new_table_block(block_name = "Table 1 (by species)"),

    # -- The model ---------------------------------------------------------
    # Fits and returns the model object. `visible = "inputs"` drops its
    # output pane: the plain R print of an lm is exactly what the card next
    # to it renders properly, so the panel is just the formula widget --
    # response picker, term chips, interaction builder. That is the control
    # surface of the whole page. Edit it and everything else re-estimates.
    mdl = `attr<-`(
      new_model_block(
        model_type = "lm",
        formula = "body_mass_g ~ flipper_length_mm + bill_length_mm + species",
        block_name = "Linear model"
      ),
      "visible", "inputs"
    ),

    # The visual summary: model facts line (kind, n, R^2) plus a coefficient
    # table with an inline forest -- estimate, CI whisker, significance
    # chips. Feeds on the model object directly (it calls tidy + glance
    # itself, no adapter block in between). Its own value is still the tidy
    # coefficient frame, so a table or chart could hang off it. All display
    # options live in its gear, so the card is all the panel needs to show.
    summ = `attr<-`(
      new_model_summary_block(block_name = "Coefficients"),
      "visible", "outputs"
    ),

    # -- The per-observation frame the plots feed on -----------------------
    # broom::augment appends the per-observation columns to the model frame:
    # .fitted, .resid, .std.resid, and (qq = TRUE) the normal-quantile pair
    # .qq_theoretical / .qq_sample -- which is what turns a Q-Q plot into a
    # plain scatter, no special plot type needed. Pure plumbing, no panel.
    aug = new_broom_block(
      output = "augment", qq = TRUE, block_name = "Fitted values & residuals"
    ),

    # -- The dashboard pair: blockr.viz CHART blocks ------------------------
    # These two live on the Model page, where they redraw on every formula
    # edit, so they are echarts and not ggplot: a chart block pushes data to
    # the client and the browser redraws, where a ggplot block is a fresh R
    # render round-tripped as a PNG. On a page whose whole point is "change
    # the formula and watch", that difference is the feel of the thing.
    # They also get reference geometry for free -- the identity diagonal and
    # the zero line below are chart arguments, not layers to hand-write.
    #
    # They are NOT in the report (`report = FALSE` in the annotations). The
    # document gets the ggplot pair instead, whose exported code reads as
    # ggplot2. Interactive here, legible there.
    pred = `attr<-`(
      blockr.viz::new_chart_block(
        chart_type = "scatter", x = "body_mass_g", y = ".fitted",
        color = "species", identity_line = TRUE,
        block_name = "Predicted vs actual"
      ),
      "visible", "inputs"
    ),
    resid = `attr<-`(
      blockr.viz::new_chart_block(
        chart_type = "scatter", x = ".fitted", y = ".resid",
        color = "species", hlines = 0,
        block_name = "Residuals vs fitted"
      ),
      "visible", "inputs"
    ),

    # -- The document pair: blockr.ggplot blocks ----------------------------
    # These sit beside the outline in Report, and they are ggplot precisely
    # because that is what lands in the rendered document: the chunk reads
    # `ggplot2::ggplot(...) + geom_point()`, a recipe a reader can lift, not
    # a call into a blockr package. They are read, not driven, so a server
    # render costs nothing here.
    #
    # Both keep their settings band. Not for looks: a ggplot block hidden to
    # `visible = "outputs"` on a panel outside the startup view draws
    # nothing at all -- its mapping is built by the band, and the band never
    # runs if it is never drawn.
    relation = blockr.ggplot::new_ggplot_block(
      type = "point", x = "flipper_length_mm", y = "body_mass_g",
      color = "species", block_name = "Mass vs flipper length"
    ),
    qqp = blockr.ggplot::new_ggplot_block(
      type = "point", x = ".qq_theoretical", y = ".qq_sample",
      color = "species", block_name = "Normal Q-Q"
    )
  ),
  links = links(
    from = c("peng",
             "clean", "clean", "clean",
             "desc",
             "mdl", "mdl",
             "aug", "aug", "aug"),
    to   = c("clean",
             "desc", "mdl", "relation",
             "desc_tbl",
             "summ", "aug",
             "pred", "resid", "qqp")
  ),
  # Stacks group the DAG nodes and give the rendered report its sections.
  stacks = stacks(
    data = new_dock_stack(
      c("peng", "clean", "desc", "desc_tbl", "relation"),
      name = "The data", color = "#2563eb"
    ),
    model = new_dock_stack(
      c("mdl", "summ"), name = "The model", color = "#7c3aed"
    ),
    fit = new_dock_stack(
      c("aug", "pred", "resid", "qqp"), name = "The fit", color = "#d97706"
    )
  ),
  extensions = list(
    blockr.dag::new_dag_extension(),
    # The slide builder: pick the blocks whose output becomes a slide, drag
    # them into order, download pptx or HTML. Deliberately narrower than the
    # outline -- Christoph's read is that the full narrated report is
    # overpowered for the everyday case, and "which four exhibits do I show
    # on Tuesday" is the everyday case.
    #
    # Seeded with the four exhibits that carry the story, in PRESENTATION
    # order, which is not evaluation order: the deck opens on the answer
    # (the coefficients) and shows the sample afterwards. It can do that
    # because `export_deck_qmd()` emits every block's code up front as
    # hidden chunks and a slide carries only its exhibit expression -- where
    # the outline's document order IS its evaluation order.
    #
    # The two dashboard charts are left out on purpose. A blockr.viz chart
    # exports through the shared exhibit path and comes out as a TABLE, not
    # a picture; the two ggplots are the figures here, same division of
    # labour as the report.
    blockr.outline::new_slides_extension(
      title = "What drives a penguin's body mass?",
      slides = c("summ", "desc_tbl", "relation", "qqp")
    ),
    blockr.assistant::new_assistant_extension(),
    blockr.outline::new_outline_extension(
      title = "What drives a penguin's body mass?",
      # Document order. Without it the outline falls back to the DAG's
      # topological order, where `relation` (fed straight off `clean`) sorts
      # ahead of the model and splits its stack's section in two. Reading
      # order is an editorial decision, so state it.
      block_order = c(
        "peng", "clean", "desc", "desc_tbl", "relation",
        "mdl", "summ",
        "aug", "pred", "resid", "qqp"
      ),
      # One description per block. This is the whole trick: the analysis
      # and its narration live in the same object, so the document cannot
      # drift from the code that produced it. `report = FALSE` keeps a
      # plumbing step out of the document without removing it from the
      # board.
      #
      # SPELL OUT `report = TRUE` ON EVERY BLOCK THAT BELONGS IN THE
      # DOCUMENT. The outline's default is OFF (blockr.outline's
      # `ann_report()`, R/export.R): a board is not a report until someone
      # says what is in it, so a fresh board opens empty rather than on 80
      # rows to be excluded one by one. A board that only spells out its
      # EXCLUSIONS -- which this one did until 2026-08-11 -- therefore
      # renders an empty document and a "11 outside the document" count,
      # with no error anywhere. The flags below are exhaustive on purpose:
      # every block says which side it is on.
      annotations = list(
        peng = list(
          description = paste(
            "The **Palmer penguins**: 344 birds of three species, measured",
            "on bill, flipper and body mass. A friendlier stand-in for iris,",
            "and a natural regression target -- body mass is the thing to",
            "predict."
          ),
          report = FALSE
        ),
        clean = list(
          description = paste(
            "Restrict to the 333 birds with a complete set of measurements,",
            "so every step below describes the same sample."
          ),
          report = FALSE
        ),
        desc = list(report = FALSE),
        desc_tbl = list(
          description = paste(
            "The sample, described: mean (SD) of each measurement, overall",
            "and by species. Gentoos are the heavy ones, and the gap is",
            "large enough that species has to enter the model."
          ),
          report = TRUE
        ),
        mdl = list(
          description = paste(
            "Fit body mass as a linear function of **flipper length**,",
            "**bill length** and **species**. Flipper length carries most",
            "of the signal; the species terms shift the intercept."
          ),
          report = TRUE
        ),
        summ = list(
          description = paste(
            "The estimated coefficients with 95% confidence intervals. Each",
            "extra millimetre of flipper adds a few dozen grams; the species",
            "terms are the baseline differences that size alone does not",
            "explain. An interval clear of zero is the significance chip",
            "beside it."
          ),
          report = TRUE
        ),
        aug = list(report = FALSE),
        # The two dashboard charts stay OUT of the document: the report
        # carries the ggplot pair below, whose exported code reads as
        # ggplot2 rather than as a blockr.viz::static_chart() call.
        resid = list(
          report = FALSE,
          description = paste(
            "Residuals against fitted values -- the first plot to look at.",
            "A shapeless band around zero means the linear form is fine; a",
            "funnel would mean the spread grows with the prediction, a curve",
            "would mean a missing nonlinear term."
          )
        ),
        pred = list(
          report = FALSE,
          description = paste(
            "Predicted body mass against the real thing. The closer the",
            "points hug the diagonal, the better the model predicts; the",
            "scatter around it is what flipper, bill and species leave",
            "unexplained."
          )
        ),
        relation = list(
          description = paste(
            "The relationship, drawn: body mass against flipper length,",
            "coloured by species. The upward slope is the flipper effect;",
            "the three offset clouds are the species differences the model",
            "estimates."
          ),
          report = TRUE
        ),
        qqp = list(
          description = paste(
            "The standardised residuals against normal quantiles. Points on",
            "the diagonal mean the normal assumption behind the intervals",
            "above holds; the tails are where it usually gives."
          ),
          report = TRUE
        )
      ),
      stack_annotations = list(
        data = list(
          description = "Where the numbers come from, and what they look like."
        ),
        model = list(description = "The estimate."),
        diag = list(
          description = "Whether the estimate is allowed to mean what it says."
        ),
        results = list(description = "The same story as pictures.")
      )
    )
  ),
  # Each view is a GRID. Orientation ALTERNATES with depth, so only the root
  # names one: a horizontal root splits into columns, each group() inside it
  # is a stack of rows, each group() inside THAT is columns again. Sizes are
  # relative. A leaf naming an extension resolves to that extension's panel.
  # Views derive from the grids, and a block in no grid is simply hidden --
  # it still runs, and still reports its code to the outline.
  grids = list(
    # The dashboard. Left column: formula widget over coefficient card.
    # Right column: ONE slot, two tabs -- predicted vs actual with the
    # identity diagonal fronted, residuals vs fitted with the zero line
    # behind it. `c(...)` tabs where two leaves would split, so one chart
    # gets the whole column instead of two getting half each, and the page
    # fits a screen without scrolling. Edit the formula and the fronted one
    # redraws in the browser.
    #
    # Tabbing is free HERE because both are `report = FALSE`: an un-fronted
    # tab is not mounted, which would cost a ggplot its report chunk (see
    # the Report grid below) but costs a dashboard chart nothing.
    Model = dock_grid(
      group("mdl", "summ", sizes = c(3, 4)),
      c("pred", "resid"),
      orientation = "horizontal", sizes = c(2, 3)
    ),
    # The document and its figures. Outline on the left, the two ggplots
    # side by side beside it, each next to the prose describing it. Also
    # where the next panel goes: open anything from the block browser while
    # this view is up and it lands in this row.
    #
    # A real SPLIT, not `c(...)` tabs. Only the fronted tab of a tabbed slot
    # is mounted, and an unmounted ggplot block reports no code to the
    # outline -- its chunk would come out as "waiting for R code to be
    # generated" and the Download would wait on it forever. Split, all three
    # are mounted whenever this view is, which is exactly when the outline
    # is looking.
    # Three columns, not outline-plus-a-stack: stacked, each figure gets half
    # the height and its settings band (which a deferred ggplot has to keep,
    # see above) leaves nothing for the plot. Side by side each gets the full
    # column height, and the band costs width it can afford.
    #
    # The deck builder TABS with the outline rather than taking a fourth
    # column: two readings of the same board, one at a time, and a fourth
    # column would leave the figures too narrow to be worth showing. Tabbing
    # an extension is free -- the mounting rule above binds BLOCKS, which
    # report their code only while drawn. `relation` and `qqp` keep their own
    # columns, so both stay mounted whichever tab is up, and the outline's
    # Download demands what it needs anyway.
    # `panels()`, NOT `c()`: a bare character vector tabs fine (see the two
    # charts above), but `ext()` returns a `panel_ref` OBJECT and `c()` on
    # two of them concatenates their fields into an unclassed list, which
    # dies at board construction with "Unknown layout node type: NULL".
    # `panels()` is the constructor that takes refs.
    Report = dock_grid(
      panels(ext("outline"), ext("slides")), "relation", "qqp",
      orientation = "horizontal", sizes = c(4, 3, 3)
    ),
    # Outline fronted, DAG on the tab behind it, assistant beside both.
    Workflow = dock_grid(
      panels(ext("outline"), ext("dag")), ext("assistant"), sizes = c(2, 1)
    )
  ),
  active = "Model"
)

# blockr.session's manage_project plugin: save / load / version the whole
# workflow from the app, so the board is a document you can come back to.
serve(board, plugins = custom_plugins(manage_project()))
