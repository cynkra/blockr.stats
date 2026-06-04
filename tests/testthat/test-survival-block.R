eval_bquoted <- function(expr, df) {
  resolved <- do.call(bquote, list(expr, list(data = as.name("data"))))
  eval(resolved, list(data = df))
}

make_surv_df <- function(n = 80) {
  set.seed(1)
  data.frame(
    time = round(stats::rexp(n, 0.08), 1) + 1,
    event = stats::rbinom(n, 1, 0.6),
    grp = factor(sample(c("A", "B"), n, TRUE))
  )
}

test_that("survival_state maps CDEX-style args to a Surv widget state", {
  st <- survival_state("AVAL", "DTHEV", "TRT")
  expect_equal(st$response, list(fn = "Surv", time = "AVAL", event = "DTHEV", eventLevel = NULL))
  expect_length(st$terms, 1L)
  expect_equal(st$terms[[1]]$var, "TRT")
  # no group -> empty RHS
  st0 <- survival_state("AVAL", "DTHEV", character())
  expect_length(st0$terms, 0L)
})

test_that("KM / Cox / CIF all fit from CDEX-style constructor args", {
  d <- make_surv_df()
  for (ty in c("km", "cox", "cif")) {
    blk <- new_survival_block(
      type = ty, time_var = "time", event_var = "event", group_var = "grp"
    )
    testServer(blk$expr_server, args = list(data = reactive(d)), {
      session$flushReact()
      e <- session$returned$expr()
      expect_false(is.null(e), info = ty)
      fit <- eval_bquoted(e, d)
      expect_false(is.null(fit), info = ty)
    })
  }
})

test_that("KM/Cox emit a survival::Surv(...) ~ group formula", {
  d <- make_surv_df()
  blk <- new_survival_block(type = "cox", time_var = "time",
                            event_var = "event", group_var = "grp")
  testServer(blk$expr_server, args = list(data = reactive(d)), {
    session$flushReact()
    fit <- eval_bquoted(session$returned$expr(), d)
    expect_s3_class(fit, "coxph")
    expect_match(deparse1(stats::formula(fit)), "Surv", fixed = TRUE)
  })
})

test_that("state round-trips to the (type, time_var, event_var, group_var) contract", {
  d <- make_surv_df()
  blk <- new_survival_block(type = "km", time_var = "time",
                            event_var = "event", group_var = "grp")
  testServer(blk$expr_server, args = list(data = reactive(d)), {
    session$flushReact()
    expect_equal(session$returned$state$type(), "km")
    expect_equal(session$returned$state$time_var(), "time")
    expect_equal(session$returned$state$event_var(), "event")
    expect_equal(session$returned$state$group_var(), "grp")
  })
})

test_that("empty (no time/event) yields NULL expr", {
  d <- make_surv_df()
  blk <- new_survival_block(type = "km")
  testServer(blk$expr_server, args = list(data = reactive(d)), {
    session$flushReact()
    expect_null(session$returned$expr())
  })
})
