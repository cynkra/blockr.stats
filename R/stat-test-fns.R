#' Pluggable test functions
#'
#' Each function takes (data, values, groups, params) and returns a tibble.
#' These are called by stratified_eval after the data has been split by
#' group-by columns.
#'
#' @name test_functions
#' @noRd
NULL

#' Normality test
#' @noRd
test_normality <- function(data, values, groups, params) {
  method <- params$method %||% "shapiro.wilk"

  results <- lapply(values, function(vcol) {
    keep <- !is.na(data[[vcol]])
    x <- data[[vcol]][keep]

    if (length(x) < 3) {
      warning("Normality test skipped for '", vcol, "': fewer than 3 observations.")
      return(tibble::tibble_row(
        method = NA_character_, values = vcol,
        n = length(x), statistic = NA_real_, p.value = NA_real_
      ))
    }

    test_call <- switch(method,
      shapiro.wilk     = stats::shapiro.test(x),
      anderson.darling = nortest::ad.test(x),
      shapiro.francia  = nortest::sf.test(x),
      lilliefors       = nortest::lillie.test(x),
      cramer.vonmises  = nortest::cvm.test(x),
      jarque.bera      = moments::jarque.test(x)
    )

    tibble::tibble_row(
      method    = trimws(test_call$method),
      values    = vcol,
      n         = length(x),
      statistic = unname(test_call$statistic),
      p.value   = unname(test_call$p.value)
    )
  })

  dplyr::bind_rows(results)
}

#' Two-sample t-test
#' @noRd
test_t_twosample <- function(data, values, groups, params) {
  alternative <- params$alternative %||% "two.sided"
  var_equal <- identical(params$variant %||% "welch", "pooled")
  conf_level <- params$conf_level %||% 0.95
  mu <- params$null %||% 0

  results <- list()
  for (vcol in values) {
    for (gcol in groups) {
      keep <- !is.na(data[[vcol]]) & !is.na(data[[gcol]])
      x <- data[[vcol]][keep]
      g <- data[[gcol]][keep]
      lvls <- unique(stats::na.omit(g))

      if (length(lvls) < 2) {
        warning("T-test skipped for '", vcol, "' by '", gcol, "': fewer than 2 group levels.")
        results <- c(results, list(tibble::tibble_row(
          method = NA_character_, values = vcol, groups = gcol,
          group.x = as.character(lvls[1]), group.y = NA_character_,
          n.x = sum(g == lvls[1], na.rm = TRUE), n.y = 0L
        )))
        next
      }

      pairs <- utils::combn(lvls, 2, simplify = FALSE)
      for (p in pairs) {
        x1 <- x[g == p[1]]
        x2 <- x[g == p[2]]

        if (length(x1) < 2 || length(x2) < 2) {
          warning("T-test skipped: fewer than 2 observations per group.")
          results <- c(results, list(tibble::tibble_row(
            method = NA_character_, values = vcol, groups = gcol,
            group.x = as.character(p[1]), group.y = as.character(p[2]),
            n.x = length(x1), n.y = length(x2)
          )))
          next
        }

        fit <- stats::t.test(x1, x2, alternative = alternative, mu = mu,
                             var.equal = var_equal, conf.level = conf_level)
        results <- c(results, list(tibble::tibble_row(
          method      = trimws(fit$method),
          values      = vcol,
          groups      = gcol,
          group.x     = as.character(p[1]),
          group.y     = as.character(p[2]),
          n.x         = length(x1),
          n.y         = length(x2),
          mean.x      = unname(fit$estimate[1]),
          mean.y      = unname(fit$estimate[2]),
          mean.diff   = unname(fit$estimate[1] - fit$estimate[2]),
          null.diff   = unname(fit$null.value),
          conf.low    = unname(fit$conf.int[1]),
          conf.high   = unname(fit$conf.int[2]),
          conf.level  = unname(attr(fit$conf.int, "conf.level")),
          alternative = gsub("\\.", " ", fit$alternative),
          stderr      = unname(fit$stderr),
          statistic   = unname(fit$statistic),
          df          = unname(fit$parameter),
          p.value     = unname(fit$p.value)
        )))
      }
    }
  }

  dplyr::bind_rows(results)
}

#' Two-sample Wilcoxon rank-sum test
#' @noRd
test_wilcoxon <- function(data, values, groups, params) {
  alternative <- params$alternative %||% "two.sided"
  conf_level <- params$conf_level %||% 0.95
  mu <- params$null %||% 0

  results <- list()
  for (vcol in values) {
    for (gcol in groups) {
      keep <- !is.na(data[[vcol]]) & !is.na(data[[gcol]])
      x <- data[[vcol]][keep]
      g <- data[[gcol]][keep]
      lvls <- unique(stats::na.omit(g))

      if (length(lvls) < 2) {
        warning("Wilcoxon test skipped for '", vcol, "' by '", gcol,
                "': fewer than 2 group levels.")
        results <- c(results, list(tibble::tibble_row(
          method = NA_character_, values = vcol, groups = gcol,
          group.x = as.character(lvls[1]), group.y = NA_character_,
          n.x = sum(g %in% lvls[1], na.rm = TRUE), n.y = 0L
        )))
        next
      }

      pairs <- utils::combn(lvls, 2, simplify = FALSE)
      for (p in pairs) {
        x1 <- x[g == p[1]]
        x2 <- x[g == p[2]]

        if (length(x1) < 2 || length(x2) < 2) {
          warning("Wilcoxon test skipped: fewer than 2 observations per group.")
          results <- c(results, list(tibble::tibble_row(
            method = NA_character_, values = vcol, groups = gcol,
            group.x = as.character(p[1]), group.y = as.character(p[2]),
            n.x = length(x1), n.y = length(x2)
          )))
          next
        }

        fit <- stats::wilcox.test(x1, x2, alternative = alternative, mu = mu,
                                  conf.int = TRUE, conf.level = conf_level)
        results <- c(results, list(tibble::tibble_row(
          method        = trimws(fit$method),
          values        = vcol,
          groups        = gcol,
          group.x       = as.character(p[1]),
          group.y       = as.character(p[2]),
          n.x           = length(x1),
          n.y           = length(x2),
          location.diff = unname(fit$estimate),
          null.shift    = unname(fit$null.value),
          conf.low      = unname(fit$conf.int[1]),
          conf.high     = unname(fit$conf.int[2]),
          conf.level    = unname(attr(fit$conf.int, "conf.level")),
          alternative   = gsub("\\.", " ", fit$alternative),
          statistic     = unname(fit$statistic),
          p.value       = unname(fit$p.value)
        )))
      }
    }
  }

  dplyr::bind_rows(results)
}

#' One-sample t-test
#' @noRd
test_t_onesample <- function(data, values, groups, params) {
  alternative <- params$alternative %||% "two.sided"
  conf_level <- params$conf_level %||% 0.95
  mu <- params$null %||% 0

  results <- lapply(values, function(vcol) {
    keep <- !is.na(data[[vcol]])
    x <- data[[vcol]][keep]

    if (length(x) < 2) {
      warning("One-sample t-test skipped for '", vcol, "': fewer than 2 observations.")
      return(tibble::tibble_row(
        method = NA_character_, values = vcol,
        n = length(x), mean = NA_real_, null.mean = mu,
        conf.low = NA_real_, conf.high = NA_real_, conf.level = conf_level,
        alternative = NA_character_, stderr = NA_real_,
        statistic = NA_real_, df = NA_real_, p.value = NA_real_
      ))
    }

    fit <- stats::t.test(x, mu = mu, alternative = alternative,
                         conf.level = conf_level)
    tibble::tibble_row(
      method      = trimws(fit$method),
      values      = vcol,
      n           = length(x),
      mean        = unname(fit$estimate),
      null.mean   = unname(fit$null.value),
      conf.low    = unname(fit$conf.int[1]),
      conf.high   = unname(fit$conf.int[2]),
      conf.level  = unname(attr(fit$conf.int, "conf.level")),
      alternative = gsub("\\.", " ", fit$alternative),
      stderr      = unname(fit$stderr),
      statistic   = unname(fit$statistic),
      df          = unname(fit$parameter),
      p.value     = unname(fit$p.value)
    )
  })

  dplyr::bind_rows(results)
}

#' One-sample Wilcoxon signed-rank test
#' @noRd
test_wilcoxon_onesample <- function(data, values, groups, params) {
  alternative <- params$alternative %||% "two.sided"
  conf_level <- params$conf_level %||% 0.95
  mu <- params$null %||% 0

  results <- lapply(values, function(vcol) {
    keep <- !is.na(data[[vcol]])
    x <- data[[vcol]][keep]

    if (length(x) < 2) {
      warning("One-sample Wilcoxon test skipped for '", vcol,
              "': fewer than 2 observations.")
      return(tibble::tibble_row(
        method = NA_character_, values = vcol,
        n = length(x), location = NA_real_, null.location = mu,
        conf.low = NA_real_, conf.high = NA_real_, conf.level = conf_level,
        alternative = NA_character_,
        statistic = NA_real_, p.value = NA_real_
      ))
    }

    fit <- stats::wilcox.test(x, mu = mu, alternative = alternative,
                              conf.int = TRUE, conf.level = conf_level)
    tibble::tibble_row(
      method        = trimws(fit$method),
      values        = vcol,
      n             = length(x),
      location      = unname(fit$estimate),
      null.location = unname(fit$null.value),
      conf.low      = unname(fit$conf.int[1]),
      conf.high     = unname(fit$conf.int[2]),
      conf.level    = unname(attr(fit$conf.int, "conf.level")),
      alternative   = gsub("\\.", " ", fit$alternative),
      statistic     = unname(fit$statistic),
      p.value       = unname(fit$p.value)
    )
  })

  dplyr::bind_rows(results)
}

#' One-way ANOVA (Welch or classical)
#' @noRd
test_anova_oneway <- function(data, values, groups, params) {
  var_equal <- identical(params$variant %||% "welch", "pooled")

  results <- list()
  for (vcol in values) {
    for (gcol in groups) {
      keep <- !is.na(data[[vcol]]) & !is.na(data[[gcol]])
      x <- data[[vcol]][keep]
      g <- data[[gcol]][keep]

      if (length(unique(g)) < 2) {
        warning("One-way ANOVA skipped for '", vcol, "' by '", gcol,
                "': fewer than 2 groups.")
        results <- c(results, list(tibble::tibble_row(
          method = NA_character_, values = vcol, groups = gcol,
          n = length(x), n.groups = length(unique(g)),
          statistic = NA_real_, num.df = NA_real_,
          denom.df = NA_real_, p.value = NA_real_
        )))
        next
      }

      res <- stats::oneway.test(x ~ factor(g), var.equal = var_equal)
      results <- c(results, list(tibble::tibble_row(
        method    = trimws(res$method),
        values    = vcol,
        groups    = gcol,
        n         = length(x),
        n.groups  = length(unique(g)),
        statistic = unname(res$statistic),
        num.df    = unname(res$parameter[1]),
        denom.df  = unname(res$parameter[2]),
        p.value   = unname(res$p.value)
      )))
    }
  }

  dplyr::bind_rows(results)
}

#' Kruskal-Wallis rank sum test
#' @noRd
test_kruskal_wallis <- function(data, values, groups, params) {
  results <- list()
  for (vcol in values) {
    for (gcol in groups) {
      keep <- !is.na(data[[vcol]]) & !is.na(data[[gcol]])
      x <- data[[vcol]][keep]
      g <- data[[gcol]][keep]

      if (length(unique(g)) < 2) {
        warning("Kruskal-Wallis test skipped for '", vcol, "' by '", gcol,
                "': fewer than 2 groups.")
        results <- c(results, list(tibble::tibble_row(
          method = NA_character_, values = vcol, groups = gcol,
          n = length(x), n.groups = length(unique(g)),
          statistic = NA_real_, df = NA_real_, p.value = NA_real_
        )))
        next
      }

      res <- stats::kruskal.test(x, g)
      results <- c(results, list(tibble::tibble_row(
        method    = trimws(res$method),
        values    = vcol,
        groups    = gcol,
        n         = length(x),
        n.groups  = length(unique(g)),
        statistic = unname(res$statistic),
        df        = unname(res$parameter),
        p.value   = unname(res$p.value)
      )))
    }
  }

  dplyr::bind_rows(results)
}

#' Homogeneity of variances test
#' @noRd
test_homogeneity <- function(data, values, groups, params) {
  method <- params$method %||% "bartlett"

  results <- list()
  for (vcol in values) {
    for (gcol in groups) {
      keep <- !is.na(data[[vcol]]) & !is.na(data[[gcol]])
      x <- data[[vcol]][keep]
      g <- data[[gcol]][keep]

      # remove groups with fewer than 2 observations
      grp_counts <- table(g)
      bad_groups <- names(grp_counts[grp_counts < 2])
      if (length(bad_groups) > 0) {
        warning("Homogeneity test: groups with fewer than 2 observations removed.")
        keep2 <- !(g %in% bad_groups)
        x <- x[keep2]
        g <- g[keep2]
      }

      if (length(unique(g)) < 2) {
        warning("Homogeneity test skipped for '", vcol, "' by '", gcol,
                "': fewer than 2 groups.")
        results <- c(results, list(tibble::tibble_row(
          method = NA_character_, values = vcol, groups = gcol,
          n = length(x), n.groups = length(unique(g)),
          statistic = NA_real_, df = NA_real_,
          df.residual = NA_real_, p.value = NA_real_
        )))
        next
      }

      if (method %in% c("bartlett", "fligner")) {
        res <- if (method == "bartlett") {
          stats::bartlett.test(x, g)
        } else {
          stats::fligner.test(x, g)
        }
        results <- c(results, list(tibble::tibble_row(
          method      = trimws(res$method),
          values      = vcol,
          groups      = gcol,
          n           = length(x),
          n.groups    = length(unique(g)),
          statistic   = unname(res$statistic),
          df          = unname(res$parameter),
          df.residual = NA_real_,
          p.value     = unname(res$p.value)
        )))
      } else {
        if (!requireNamespace("car", quietly = TRUE)) {
          stop("The 'car' package is required for Levene and Brown-Forsythe tests. ",
               "Install it with: install.packages('car')")
        }
        center <- if (method == "levene") base::mean else stats::median
        method_name <- if (method == "levene") {
          "Levene's test of homogeneity of variances"
        } else {
          "Brown-Forsythe test of homogeneity of variances"
        }
        res <- car::leveneTest(x, factor(g), center = center)
        results <- c(results, list(tibble::tibble_row(
          method      = method_name,
          values      = vcol,
          groups      = gcol,
          n           = length(x),
          n.groups    = length(unique(g)),
          statistic   = unname(res[1L, "F value"]),
          df          = unname(res[1L, "Df"]),
          df.residual = unname(res[2L, "Df"]),
          p.value     = unname(res[1L, "Pr(>F)"])
        )))
      }
    }
  }

  dplyr::bind_rows(results)
}

#' Correlation test
#' @noRd
test_correlation <- function(data, values, groups, params) {
  method <- params$method %||% "pearson"
  alternative <- params$alternative %||% "two.sided"
  conf_level <- params$conf_level %||% 0.95

  if (length(values) < 2) {
    warning("Correlation test requires at least 2 value columns.")
    return(tibble::tibble())
  }

  pairs <- utils::combn(values, 2, simplify = FALSE)
  results <- lapply(pairs, function(p) {
    vcol1 <- p[1]
    vcol2 <- p[2]
    keep <- !is.na(data[[vcol1]]) & !is.na(data[[vcol2]])
    x <- data[[vcol1]][keep]
    y <- data[[vcol2]][keep]

    if (length(x) < 3) {
      warning("Correlation test skipped for '", vcol1, "' vs '", vcol2,
              "': fewer than 3 paired observations.")
      return(tibble::tibble_row(
        method = NA_character_, values.x = vcol1, values.y = vcol2,
        n = length(x), cor = NA_real_,
        conf.low = NA_real_, conf.high = NA_real_, conf.level = NA_real_,
        alternative = NA_character_, statistic = NA_real_,
        df = NA_real_, p.value = NA_real_
      ))
    }

    res <- stats::cor.test(x, y, method = method,
              alternative = alternative, conf.level = conf_level)
    tibble::tibble_row(
      method      = trimws(res$method),
      values.x    = vcol1,
      values.y    = vcol2,
      n           = length(x),
      cor         = unname(res$estimate),
      conf.low    = if (!is.null(res$conf.int)) unname(res$conf.int[1]) else NA_real_,
      conf.high   = if (!is.null(res$conf.int)) unname(res$conf.int[2]) else NA_real_,
      conf.level  = if (!is.null(res$conf.int)) unname(attr(res$conf.int, "conf.level")) else NA_real_,
      alternative = gsub("\\.", " ", res$alternative),
      statistic   = unname(res$statistic),
      df          = if (!is.null(res$parameter)) unname(res$parameter) else NA_real_,
      p.value     = unname(res$p.value)
    )
  })

  dplyr::bind_rows(results)
}
