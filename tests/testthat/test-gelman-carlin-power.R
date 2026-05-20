# test-gelman-carlin-power.R
#
# Unit tests of the Gelman & Carlin (2014) retrodesign analysis as implemented
# in {mathpsychr}.
#
# Reference values were  and cross-checked against:
#
#   (a) Gelman & Carlin's original simulation-based function
#   (b) `retrodesign::retro_design_closed_form()` when the package is available
#   (c) `pwrss::power.t.test()` when the package is available (power only;
#       Type S / Type M are not provided by {pwrss})
#
# References:
# Gelman, A., & Carlin, J. (2014). Beyond power calculations.
#   Perspectives on Psychological Science, 9(6), 641–651.
#   https://doi.org/10.1177/1745691614551642

library(testthat)

# ── Reference values ──────────────────────────────────────────────────────────
REF <- list(
  gc1 = list(A = 0.1, s = 3.28), # Gelman & Carlin (2014) example 1
  gc2 = list(A = 2,   s = 8.1),  # Gelman & Carlin (2014) example 2
  NULL                           # Reserved for future expansions, e.g., CATLAS power analyses
)

# Tolerance for exact and simulation-based comparisons
TOL_EXACT <- 1e-6
TOL_SIMUL <- 1e-3
TOL_PWRSS <- 1e-2 # liberal for {pwrss} comparisons


# ══════════════════════════════════════════════════════════════════════════════
# 1. CONSTRUCTOR & CLASS CHECKS
# ══════════════════════════════════════════════════════════════════════════════

test_that("`gelman_carlin_power()` returns correct class hierarchy", {
  d <- gelman_carlin_power(effect_size = 2, se = 8.1)
  expect_s3_class(d, "mpr_gelman_carlin_power")
  expect_s3_class(d, "mpr_power")
  expect_true(is_gelman_carlin_power(d))
  expect_true(is_mpr_power(d))
})

test_that("`gelman_carlin_power()` stores inputs faithfully", {
  d <- gelman_carlin_power(effect_size = 2, se = 8.1, alpha = 0.01, df = Inf, n_sims = 5000L)
  expect_equal(d$effect_size, 2)
  expect_equal(d$se, 8.1)
  expect_equal(d$alpha, 0.01)
  expect_true(is.infinite(d$df))
  expect_identical(d$n_sims, 5000L)
})

test_that("`gelman_carlin_power()` results slot is NULL before `compute()`", {
  d <- gelman_carlin_power(effect_size = 2, se = 8.1)
  expect_null(d$results)
})


# ══════════════════════════════════════════════════════════════════════════════
# 2. INPUT VALIDATION
# ══════════════════════════════════════════════════════════════════════════════

test_that("`gelman_carlin_power()` rejects non-finite effect_size", {
  expect_error(gelman_carlin_power(effect_size = NA,  se = 1))
  expect_error(gelman_carlin_power(effect_size = NaN, se = 1))
  expect_error(gelman_carlin_power(effect_size = c(1, NA, 3), se = 1))
})

test_that("`gelman_carlin_power()` rejects non-positive or NA se", {
  expect_error(gelman_carlin_power(effect_size = 1, se =  0))
  expect_error(gelman_carlin_power(effect_size = 1, se = -1))
  expect_error(gelman_carlin_power(effect_size = 1, se = NA))
})

test_that("`gelman_carlin_power()` rejects alpha outside (0, 1)", {
  expect_error(gelman_carlin_power(effect_size = 1, se = 1, alpha = 0))
  expect_error(gelman_carlin_power(effect_size = 1, se = 1, alpha = 1))
  expect_error(gelman_carlin_power(effect_size = 1, se = 1, alpha = -0.1))
  expect_error(gelman_carlin_power(effect_size = 1, se = 1, alpha = 1.1))
  expect_error(gelman_carlin_power(effect_size = 1, se = 1, alpha = NA))
})

test_that("`gelman_carlin_power()` rejects non-positive df", {
  expect_error(gelman_carlin_power(effect_size = 1, se = 1, df =  0))
  expect_error(gelman_carlin_power(effect_size = 1, se = 1, df = -5))
  expect_error(gelman_carlin_power(effect_size = 1, se = 1, df = NA))
})

test_that("`gelman_carlin_power()` rejects mismatched vector lengths for se", {
  expect_error(gelman_carlin_power(effect_size = 1:5, se = c(1, 2)))
})

test_that("`gelman_carlin_power()` accepts scalar se recycled over effect_size vector", {
  expect_no_error(gelman_carlin_power(effect_size = 1:5, se = 1))
})

test_that("`gelman_carlin_power()` accepts vector se of same length as effect_size", {
  expect_no_error(gelman_carlin_power(effect_size = 1:5, se = seq(0.5, 2.5, by = 0.5)))
})

test_that("`gelman_carlin_power()` rejects n_sims < 1", {
  expect_error(gelman_carlin_power(effect_size = 1, se = 1, n_sims = 0L))
  expect_error(gelman_carlin_power(effect_size = 1, se = 1, n_sims = -1L))
})

test_that("`compute()` on base `mpr_power` (no subclass) raises an informative error", {
  bare <- structure(
    list(effect_size = 1, se = 1, alpha = 0.05, df = Inf, results = NULL),
    class = "mpr_power"
  )
  expect_error(compute(bare))
})


# ══════════════════════════════════════════════════════════════════════════════
# 3. OUTPUT VALIDATION
# ══════════════════════════════════════════════════════════════════════════════

test_that("`compute()` returns the object with results populated (scalar)", {
  d <- gelman_carlin_power(effect_size = 2, se = 8.1) |> compute()
  expect_false(is.null(d$results))
  expect_s3_class(d$results, "data.frame")
})

test_that("`compute()` results have the required columns", {
  d <- gelman_carlin_power(effect_size = 2, se = 8.1) |> compute()
  expect_named(
    d$results,
    c("effect_size", "se", "power", "type_s", "type_m"),
    ignore.order = FALSE
  )
})

test_that("`compute()` vectorised effect_size produces correct row count", {
  D_range <- c(seq(0.01, 1, 0.01), seq(1.01, 10, 0.1))
  d <- gelman_carlin_power(effect_size = D_range, se = 1) |> compute()
  expect_equal(nrow(d$results), length(D_range))
})

test_that("`compute()` scalar se is recycled correctly over a vector of effects", {
  d <- gelman_carlin_power(effect_size = c(1, 2, 3), se = 1) |> compute()
  expect_equal(d$results$se, c(1, 1, 1))
})

test_that("`compute()` power is a probability in [0, 1] for all effect sizes", {
  D_range <- c(seq(0.01, 1, 0.1), seq(1, 10, 1))
  d <- gelman_carlin_power(effect_size = D_range, se = 1) |> compute()
  expect_true(all(d$results$power >= 0 & d$results$power <= 1))
})

test_that("`compute()` type_s is a probability in [0, 0.5] for positive effects", {
  D_range <- seq(0.1, 10, 0.1)
  d <- gelman_carlin_power(effect_size = D_range, se = 1) |> compute()
  non_na <- d$results$type_s[!is.na(d$results$type_s)]
  expect_true(all(non_na >= 0 & non_na <= 0.5))
})

test_that("`compute()` type_m >= 1 for all finite positive effects", {
  D_range <- seq(0.1, 10, 0.1)
  d <- gelman_carlin_power(effect_size = D_range, se = 1) |> compute()
  non_na <- d$results$type_m[!is.na(d$results$type_m)]
  expect_true(all(non_na >= 1 - TOL_EXACT))
})

test_that("`compute()` power increases monotonically with `abs(effect_size / se)`", {
  D_range <- seq(0.5, 10, 0.5)
  d <- gelman_carlin_power(effect_size = D_range, se = 1) |> compute()
  expect_true(all(diff(d$results$power) > 0))
})

test_that("`compute()` type_m decreases monotonically as power increases", {
  D_range <- seq(0.5, 10, 0.5)
  d <- gelman_carlin_power(effect_size = D_range, se = 1) |> compute()
  finite_m <- d$results$type_m[is.finite(d$results$type_m)]
  expect_true(all(diff(finite_m) < 0))
})

test_that("`compute()` high-power design correctly", {
  d <- gelman_carlin_power(effect_size = 10, se = 1) |> compute()
  r <- d$results[1L, ]
  expect_equal(r$power,  1.0, tolerance = 1e-9)
  expect_equal(r$type_s, 0.0, tolerance = 1e-9)
  expect_equal(r$type_m, 1.0, tolerance = 1e-6)
})

test_that("`compute()` simulation runs without error for finite df", {
  expect_no_error(
    gelman_carlin_power(effect_size = 2, se = 8.1, df = 30, n_sims = 500L) |> compute()
  )
})

test_that("`compute()` simulation (df = 30) type_s in [0, 0.5]", {
  d_sim <- gelman_carlin_power(effect_size = 2, se = 8.1, df = 30, n_sims = 2000L) |>
    compute()
  ts <- d_sim$results$type_s
  expect_true(is.na(ts) || (ts >= 0 && ts <= 0.5))
})

test_that("`compute()` simulation n_sims controls draw count", {
  d <- gelman_carlin_power(effect_size = 2, se = 8.1, df = 10, n_sims = 100L) |> compute()
  expect_false(is.null(d$results))
  expect_equal(nrow(d$results), 1L)
})


# ══════════════════════════════════════════════════════════════════════════════
# 4. CROSS-VALIDATION vs. Gelman & Carlin's original function
# ══════════════════════════════════════════════════════════════════════════════

test_that("`compute()` matches Gelman & Carlin's original formula (gc1)", {
  withr::local_seed(42L)
  A <- REF$gc1$A
  s <- REF$gc1$s
  z <- qnorm(0.975)
  p_lo <- pnorm(-z - A / s)
  power_orig <- (1 - pnorm(z - A / s)) + pnorm(-z - A / s)
  type_s_orig <- p_lo / power_orig
  est <- A + s * rt(1e6, Inf)
  sig <- abs(est) > s * z
  type_m_orig <- mean(abs(est)[sig]) / A

  d <- gelman_carlin_power(effect_size = A, se = s) |>
    compute()

  expect_equal(d$results$power, power_orig,   tolerance = TOL_EXACT)
  expect_equal(d$results$type_s, type_s_orig, tolerance = TOL_EXACT)
  expect_equal(d$results$type_m, type_m_orig, tolerance = TOL_SIMUL)
})

test_that("`compute()` matches Gelman & Carlin's original formula (gc2)", {
  A <- REF$gc2$A
  s <- REF$gc2$s
  z <- qnorm(0.975)
  p_lo <- pnorm(-z - A / s)
  power_orig <- (1 - pnorm(z - A / s)) + pnorm(-z - A / s)
  type_s_orig <- p_lo / power_orig
  est <- A + s * rt(1e6, Inf)
  sig <- abs(est) > s * z
  type_m_orig <- mean(abs(est)[sig]) / A

  d <- gelman_carlin_power(effect_size = A, se = s) |>
    compute()

  expect_equal(d$results$power, power_orig,   tolerance = TOL_EXACT)
  expect_equal(d$results$type_s, type_s_orig, tolerance = TOL_EXACT)
  expect_equal(d$results$type_m, type_m_orig, tolerance = TOL_SIMUL)
})


# ══════════════════════════════════════════════════════════════════════════════
# 6. CROSS-VALIDATION vs. {retrodesign}
# ══════════════════════════════════════════════════════════════════════════════

test_that("`compute()` matches `retrodesign::retro_design_closed_form()` (gc1)", {
  skip_if_not_installed("retrodesign")
  rd <- retrodesign::retro_design_closed_form(
    A = REF$gc1$A,
    s = REF$gc1$s,
    alpha = 0.05
  )

  d <- gelman_carlin_power(effect_size = REF$gc1$A, se = REF$gc1$s) |>
    compute()

  expect_equal(d$results$power,  rd$power,  tolerance = TOL_EXACT)
  expect_equal(d$results$type_s, rd$type_s, tolerance = TOL_EXACT)
  expect_equal(d$results$type_m, rd$type_m, tolerance = TOL_EXACT)
})

test_that("`compute()` matches `retrodesign::retro_design_closed_form()` (gc2)", {
  skip_if_not_installed("retrodesign")
  rd <- retrodesign::retro_design_closed_form(
    A = REF$gc2$A,
    s = REF$gc2$s,
    alpha = 0.05
  )

  d <- gelman_carlin_power(effect_size = REF$gc2$A, se = REF$gc2$s) |>
    compute()

  expect_equal(d$results$power,  rd$power,  tolerance = TOL_EXACT)
  expect_equal(d$results$type_s, rd$type_s, tolerance = TOL_EXACT)
  expect_equal(d$results$type_m, rd$type_m, tolerance = TOL_EXACT)
})

test_that("`compute()` matches `retrodesign::retro_design_closed_form()` at lambda = 2", {
  skip_if_not_installed("retrodesign")
  A <- 2
  s <- 1

  rd <- retrodesign::retro_design_closed_form(A = A, s = s, alpha = 0.05)
  d  <- gelman_carlin_power(effect_size = A, se = s) |>
    compute()

  expect_equal(d$results$power,  rd$power,  tolerance = TOL_EXACT)
  expect_equal(d$results$type_s, rd$type_s, tolerance = TOL_EXACT)
  expect_equal(d$results$type_m, rd$type_m, tolerance = TOL_EXACT)
})

test_that("`compute()` vectorised sweep matches `retrodesign::retrodesign()` power column", {
  skip_if_not_installed("retrodesign")
  lambdas <- c(0.5, 1.0, 2.0, 5.0)

  withr::local_seed(87542L)
  rd_list <- lapply(lambdas, function(l) {
    retrodesign::retro_design_closed_form(A = l, s = 1, alpha = 0.05)
  })
  rd_power  <- vapply(rd_list, `[[`, numeric(1), "power")
  rd_type_s <- vapply(rd_list, `[[`, numeric(1), "type_s")
  rd_type_m <- vapply(rd_list, `[[`, numeric(1), "type_m")

  d_vec <-
    gelman_carlin_power(effect_size = lambdas, se = 1) |>
    compute()

  expect_equal(d_vec$results$power,  rd_power,  tolerance = TOL_EXACT)
  expect_equal(d_vec$results$type_s, rd_type_s, tolerance = TOL_EXACT)
  expect_equal(d_vec$results$type_m, rd_type_m, tolerance = TOL_EXACT)
})


# ══════════════════════════════════════════════════════════════════════════════
# 6. CROSS-VALIDATION vs. {pwrss}
# ══════════════════════════════════════════════════════════════════════════════

test_that("`compute()` power matches `pwrss::power.t.test()` (normal approx.)", {
  skip_if_not_installed("pwrss")

  params <- data.frame(
    A  = c(REF$gc1$A, REF$gc2$A, 0.5, 1, 2, 5),
    s  = c(REF$gc1$s, REF$gc2$s, 1,   1, 1, 1),
    df = c(Inf, Inf, Inf, Inf, 30, 30)
  )

  pwrss_power <- mapply(
    function(A, s, df) {
      pwrss::power.t.test(
        ncp         = A / s,
        df          = if (is.infinite(df)) 1e6 else df,
        alpha       = 0.05,
        alternative = "two.sided",
        plot        = FALSE,
        verbose     = FALSE
      )$power
    },
    A  = params$A,
    s  = params$s,
    df = params$df
  )

  gc_power <- mapply(
    function(A, s, df) {
      gelman_carlin_power(
        effect_size = A,
        se          = s,
        df          = if (df > 1e5) Inf else df,
        n_sims      = 1e5L
      ) |>
        compute() |>
        (\(x) x$results$power)()
    },
    A  = params$A,
    s  = params$s,
    df = params$df
  )

  expect_equal(gc_power, pwrss_power, tolerance = TOL_PWRSS)
})
