#' Base constructor for `mpr_power`
#'
#' Internal. Only performs lightweight type checks (no domain logic).
#' Every subclass constructor must call this and append its own class
#' label via the `subclass` argument.
#'
#' @param effect_size Numeric. Assumed true effect size(s).
#' @param se Numeric. Standard error(s).
#' @param alpha Numeric(1). Significance level.
#' @param df Numeric(1). Degrees of freedom (`Inf` = normal).
#' @param subclass Character; Subclass name added to the class vector.
#' @param ... Additional named slots forwarded as list elements.
#'
#' @returns An S3 list with class `c(subclass, "mpr_power")`.
#'
#' @noRd
mpr_power <- function(
    effect_size,
    se,
    alpha = 0.05,
    df = Inf,
    subclass = character(),
    ...
) {
  stopifnot(is.numeric(effect_size), length(effect_size) >= 1L)
  stopifnot(is.numeric(se),          length(se)          >= 1L)
  stopifnot(is.numeric(alpha),       length(alpha)        == 1L)
  stopifnot(is.numeric(df),          length(df)           == 1L)
  stopifnot(is.character(subclass))

  structure(
    list(
      effect_size = effect_size,
      se = se,
      alpha = alpha,
      df = df,
      results = NULL,
      ...
    ),
    class = c(subclass, "mpr_power")
  )
}


#' Constructor for the Gelman & Carlin design calculator
#'
#' Implements the retrodesign analysis of Gelman and Carlin (2014).
#'
#' @param effect_size Numeric. Assumed true effect size(s).
#' @param se Numeric. Standard error(s).
#' @param alpha Numeric(1). Significance level.
#' @param df Numeric(1). Degrees of freedom (`Inf` = normal).
#' @param n_sims Integer(1); Number of simulations for M Type error estimation.
#'
#' @seealso [mpr_power()]
#'
#' @returns An S3 list with class `c("mpr_gelman_carlin_power", "mpr_power")`.
#'
#' @references
#'   Gelman, A., & Carlin, J. (2014). Beyond power calculations: Assessing
#'   Type S (sign) and Type M (magnitude) errors. *Perspectives on
#'   Psychological Science*, 9(6), 641–651.
#'   \doi{10.1177/1745691614551642}
#'
#' @noRd
mpr_gelman_carlin_power <- function(
    effect_size,
    se,
    alpha = 0.05,
    df = Inf,
    n_sims = 10000L
) {
  mpr_power(
    effect_size = effect_size,
    se          = se,
    alpha       = alpha,
    df          = df,
    subclass    = "mpr_gelman_carlin_power",
    n_sims      = as.integer(n_sims)
  )
}
