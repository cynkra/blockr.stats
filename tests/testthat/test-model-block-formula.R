eval_bquoted <- function(expr, df) {
  resolved <- do.call(bquote, list(expr, list(data = as.name("data"))))
  eval(resolved, list(data = df))
}

test_that("model block builds an lm fit from injected formula state", {
  ct <- list(mpg = "numeric", hp = "numeric", cyl = "numeric")
  st <- parse_formula("mpg ~ hp + cyl", ct)
  blk <- new_model_block(model_type = "lm", formula = st)

  testServer(blk$expr_server, args = list(data = reactive(mtcars)), {
    session$flushReact()
    fit <- eval_bquoted(session$returned$expr(), mtcars)
    expect_s3_class(fit, "lm")
    expect_setequal(attr(terms(fit), "term.labels"), c("hp", "cyl"))
  })
})

test_that("model block builds a logistic glm", {
  ct <- list(vs = "numeric", hp = "numeric")
  st <- parse_formula("vs ~ hp", ct)
  blk <- new_model_block(model_type = "logistic", formula = st)

  testServer(blk$expr_server, args = list(data = reactive(mtcars)), {
    session$flushReact()
    fit <- eval_bquoted(session$returned$expr(), mtcars)
    expect_s3_class(fit, "glm")
    expect_equal(fit$family$family, "binomial")
  })
})

test_that("empty formula state yields a NULL expression", {
  blk <- new_model_block()
  testServer(blk$expr_server, args = list(data = reactive(mtcars)), {
    session$flushReact()
    expect_null(session$returned$expr())
  })
})

test_that("interaction + factor formula fits and round-trips term labels", {
  ct <- list(mpg = "numeric", hp = "numeric", cyl = "factor")
  st <- parse_formula("mpg ~ hp * cyl", ct)
  blk <- new_model_block(model_type = "lm", formula = st)

  testServer(blk$expr_server, args = list(data = reactive(mtcars)), {
    session$flushReact()
    fit <- eval_bquoted(session$returned$expr(), mtcars)
    expect_s3_class(fit, "lm")
    expect_setequal(attr(terms(fit), "term.labels"), c("hp", "cyl", "hp:cyl"))
  })
})
