## Aedes albopictus IVM data (Ravasi et al. 2021), packaged for the
## aedes-ivm example board.
##
## Source: Ravasi D, Parrondo Monton D, Tanadini M, Flacio E. "Effectiveness of
## integrated Aedes albopictus management in southern Switzerland."
## Parasites & Vectors 14, 405 (2021). https://doi.org/10.1186/s13071-021-04903-2
## Open access, CC BY 4.0. The two frames below are the article's Additional
## files 2 (ovitrap egg counts) and 3 (GAT adult female counts); Additional
## file 5 is the same ovitrap frame saved as an .rds with its factor levels and
## date types intact, which is what we read.
##
## NOTHING HERE IS SIMULATED. Every row is a real trap reading from the 2019
## season, published by the authors alongside the paper. That is the whole
## reason this board exists: the demo can say "download it yourself" and mean
## it, and the numbers it prints can be checked against the paper.
##
## The raw files are vendored in data-raw/aedes-ivm/ rather than fetched at
## build time. link.springer.com sits behind a bot wall ("Client Challenge"),
## so a download_file() here would work on a laptop and fail in CI.
##
## Run `Rscript data-raw/aedes-ivm.R` to rebuild and re-verify.

library(dplyr)

raw_dir <- "data-raw/aedes-ivm"

## ------------------------------------------------------------- ovitraps ----

ovi_raw <- readRDS(file.path(raw_dir, "13071_2021_4903_MOESM5_ESM.rds"))

## ONE PUBLISHED CELL IS WRONG, and the file carries its own correction.
## Additional file 2 has two normalised-count columns that agree on 326 of 327
## rows. Row 215 (trap MAL-6a, 0 eggs over 14 days in the field) reads 14 in
## `No..Eggs.AEDES.in.14.days` and 0 in `no.eggs.normalised.14.days` -- the
## deployment length has slipped into the count column. `no.eggs.normalised.
## 14.days` equals eggs / days * 14 on every row including that one, so it is
## the recomputed column and the one we keep. Neither enters the model (which
## takes the raw count and the exposure days separately), so this affects the
## figures only.
stopifnot(
  sum(abs(ovi_raw$No..Eggs.AEDES.in.14.days -
            ovi_raw$no.eggs.normalised.14.days) > 1e-6) == 1L,
  isTRUE(all.equal(
    ovi_raw$no.eggs.normalised.14.days,
    ovi_raw$No..eggs.AEDES / ovi_raw$No..Days.ovitrap.in.field * 14
  ))
)

aedes_ovitraps <- ovi_raw |>
  select(-No..Eggs.AEDES.in.14.days) |>
  mutate(
    ## POSIXct midnights are dates wearing a timezone. Date reads better in a
    ## block preview and changes no fit -- the season enters the model as
    ## `Day.ovitrap.collected`, a day-of-year number, not as a timestamp.
    across(starts_with("Date.when."), as.Date)
  ) |>
  as.data.frame()

## ------------------------------------------------------------------ GAT ----

## Additional file 3 ships as CSV only, so the types are rebuilt here to match
## the ovitrap frame. FACTOR LEVEL ORDER IS LOAD-BEARING: `Intervention` must
## be the first level of AREA, because every published coefficient is a
## contrast AGAINST the area under integrated vector management. Sorting these
## alphabetically would silently flip the sign of the headline result.
area_levels <- levels(aedes_ovitraps$AREA)
muni_levels <- levels(aedes_ovitraps$MUNICIPALITY)

aedes_gat <- read.csv(file.path(raw_dir, "13071_2021_4903_MOESM3_ESM.csv")) |>
  mutate(
    AREA = factor(AREA, levels = area_levels),
    ## The six municipalities are listed intervention-first, not
    ## alphabetically, so a table or a legend reads as the design.
    MUNICIPALITY = factor(MUNICIPALITY, levels = muni_levels),
    TRAP.ID.fac = factor(TRAP.ID.fac, levels = levels(aedes_ovitraps$TRAP.ID.fac)),
    across(starts_with("Date.when."), as.Date)
  )

stopifnot(
  !anyNA(aedes_ovitraps), !anyNA(aedes_gat),
  nrow(aedes_ovitraps) == 327L, nrow(aedes_gat) == 301L
)

usethis::use_data(aedes_ovitraps, aedes_gat, overwrite = TRUE)

## --------------------------------------------------------- verification ----

## Refit the two published models and check we land on the published numbers.
## This is the whole warrant for the board: the code block arm claims to be
## "the model in the paper", and this is where that claim is checked.
if (requireNamespace("glmmTMB", quietly = TRUE)) {

  m_eggs <- glmmTMB::glmmTMB(
    No..eggs.AEDES ~ AREA +
      poly(Day.ovitrap.collected, degree = 2) +
      scale(ALTITUDE) +
      No..Days.ovitrap.in.field +
      (1 | TRAP.ID.fac) + (1 | MUNICIPALITY),
    family = glmmTMB::nbinom1, data = aedes_ovitraps
  )

  b <- glmmTMB::fixef(m_eggs)$cond[["AREANon-intervention"]]
  ci <- confint(m_eggs, parm = "AREANon-intervention", method = "wald")

  cat("\n-- First model (eggs per ovitrap) --\n")
  cat(sprintf(
    "non-intervention / intervention: %.2f (95%% CI %.2f-%.2f)\n",
    exp(b), exp(ci[1, 1]), exp(ci[1, 2])
  ))
  cat("paper (Results, p. 6):        3.8  (95% CI 2.7-5.4)\n")

  ## Altitude is a null result in the paper and should stay one here; the
  ## exposure effect is about +10% eggs per extra day in the field.
  cf <- summary(m_eggs)$coefficients$cond
  cat(sprintf(
    "altitude p = %.3f (paper 0.607);  per extra day +%.0f%% (paper +10%%, p = 0.003)\n",
    cf["scale(ALTITUDE)", "Pr(>|z|)"],
    100 * (exp(cf["No..Days.ovitrap.in.field", "Estimate"]) - 1)
  ))

  ## Second model: adult females per GAT. The paper adds a day-by-area
  ## interaction here, because the ratio between the two areas is NOT fixed
  ## over time -- it runs from about 2 at the season's edges to nearly 4 in
  ## August (paper, Fig. 4).
  ##
  ## NOTE THE FAMILY CHANGE. This one is nbinom2 where the egg model is
  ## nbinom1; the paper says only that "different distributional families were
  ## compared with information criteria" and puts the choice in Additional file
  ## 12, which is not distributed with the data. nbinom2 is what reproduces its
  ## reported p-values for altitude (0.095) and deployment days (0.566) to
  ## three decimals, and it also wins on AIC (1720.0 against 1722.5), so it is
  ## almost certainly the published fit. Under nbinom1 those two p-values come
  ## out at 0.061 and 0.111 -- same conclusions, different numbers.
  m_gat <- glmmTMB::glmmTMB(
    No..Ad..Albo.in.GAT ~ AREA * poly(Day.GAT.collected, degree = 2) +
      scale(ALTITUDE) +
      No..Days.GAT.in.field +
      (1 | TRAP.ID.fac) + (1 | MUNICIPALITY),
    family = glmmTMB::nbinom2, data = aedes_gat
  )
  cfg <- summary(m_gat)$coefficients$cond

  cat("\n-- Second model (adult females per GAT) --\n")
  cat(sprintf(
    "AREA p = %.2e (paper < 0.0001);  altitude p = %.3f (paper 0.095);  days p = %.3f (paper 0.566)\n",
    cfg["AREANon-intervention", "Pr(>|z|)"],
    cfg["scale(ALTITUDE)", "Pr(>|z|)"],
    cfg["No..Days.GAT.in.field", "Pr(>|z|)"]
  ))

  ## The ratio over the season, the shape of Fig. 4.
  nd <- expand.grid(
    AREA = factor(area_levels, levels = area_levels),
    Day.GAT.collected = seq(170, 285, by = 5),
    ALTITUDE = mean(aedes_gat$ALTITUDE),
    No..Days.GAT.in.field = 14,
    TRAP.ID.fac = NA, MUNICIPALITY = NA
  )
  nd$fit <- predict(m_gat, newdata = nd, type = "response", allow.new.levels = TRUE)
  ratio <- tapply(nd$fit, nd$Day.GAT.collected, function(x) x[2] / x[1])
  cat(sprintf(
    "non-intervention/intervention ratio over the season: %.1f at the edges, %.1f at peak (paper: ~2 to ~4)\n",
    min(ratio), max(ratio)
  ))

} else {
  message("glmmTMB not installed: datasets written, published fit NOT verified.")
}

## --------------------------------------------------------------- Table 1 ----

## Mean counts per trap by municipality, the paper's Table 1. A cheap check
## that no rows were dropped or regrouped on the way in.
cat("\n-- Table 1, mean count per trap (paper: 56.8 80.1 59.3 | 261.2 218.6 223.8) --\n")
print(round(tapply(
  aedes_ovitraps$No..eggs.AEDES, aedes_ovitraps$MUNICIPALITY, mean
), 1))
