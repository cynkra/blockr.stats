# Test data factories for blockr.stats block tests.
#
# Pattern B testing per blockr.docs/patterns/r-driven-blocks.md:
#   - Drive block_server with a reactive data source
#   - Assert on session$returned$result() (the materialized result)
#   - For column-picker blocks, swap the data and verify state cleanup
#
# Each test inlines its own shiny::testServer call. The data source is
# wrapped in a reactiveVal so that mid-test data swaps work:
#
#   data_rv <- shiny::reactiveVal(.tdf_a())
#   shiny::testServer(
#     blockr.core:::get_s3_method("block_server", block),
#     {
#       session$flushReact()
#       expect_false(is.null(session$returned$result()))
#       data_rv(.tdf_b())          # swap to a dataset with no shared cols
#       session$flushReact()
#       expect_true(all(session$returned$state$<x>() %in% colnames(.tdf_b())))
#     },
#     args = list(x = block, data = list(data = function() data_rv()))
#   )

# Two numeric + two factor (one 2-level, one 3-level), 40 rows.
.tdf_a <- function() {
  set.seed(101)
  data.frame(
    yA  = rnorm(40),
    xA1 = rnorm(40),
    xA2 = rnorm(40),
    fA  = factor(rep(c("g1", "g2"), each = 20)),
    fA3 = factor(rep(c("a", "b", "c"), length.out = 40))
  )
}

# Same shape, completely different column names.
.tdf_b <- function() {
  set.seed(202)
  data.frame(
    yB  = rnorm(40),
    xB1 = rnorm(40),
    xB2 = rnorm(40),
    fB  = factor(rep(c("h1", "h2"), each = 20)),
    fB3 = factor(rep(c("p", "q", "r"), length.out = 40))
  )
}
