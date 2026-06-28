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


#' Compute Gelman & Carlin (2014) design analysis
#'
#' Encapsulates the assumptions for the design analysis of Gelman &
#' Carlin (2014).
#' Call [compute()] to run the analysis and [plot()] to visualise results.
#'
#' @param effect_size Numeric. Assumed true effect size(s).
#'   A vector triggers a sweep analysis (one result row per value).
#' @param se Numeric. Standard error. Recycled if scalar.
#' @param alpha Numeric(1). Two-sided significance level.
#'   Default `0.05`.
#' @param df Numeric(1). Degrees of freedom. `Inf` (default)
#'   selects the fast exact computation (from Lu et al. 2019). Any
#'   finite value triggers Monte-Carlo simulation.
#' @param n_sims Integer(1); Monte-Carlo draws used when
#'   `df < Inf`. Defaults to 10,000.
#'
#' @returns An S3 object with class `c("mpr_gelman_carlin_power", "mpr_power")`.
#'
#' @references
#'   Gelman, A., & Carlin, J. (2014). Beyond power calculations: Assessing
#'   Type S (sign) and Type M (magnitude) errors. *Perspectives on
#'   Psychological Science*, 9(6), 641--651.
#'   \doi{10.1177/1745691614551642}
#'
#'   Lu, J., Qiu, Y., & Deng, A. (2019). A note on Type S/M errors in
#'   hypothesis testing. *British Journal of Mathematical and Statistical
#'   Psychology*, 72(1), 1-17. \doi{10.1111/bmsp.12132}
#'
#' @examples
#' # Scalar: single design point
#' d <- gelman_carlin_power(effect_size = 2, se = 8.1)
#' d <- compute(d)
#' print(d)
#' summary(d)
#'
#' # Scalar with finite df (simulation)
#' d_t <- gelman_carlin_power(effect_size = 2, se = 8.1, df = 30)
#' d_t <- compute(d_t)
#' print(d_t)
#'
#' # Sweep: replicate Gelman & Carlin's Figure 2
#' D_range <- c(seq(0.01, 1, 0.01), seq(1.01, 10, 0.1))
#' d_fig   <- gelman_carlin_power(effect_size = D_range, se = 1)
#' d_fig   <- compute(d_fig)
#' plot(d_fig)
#'
#' @export
gelman_carlin_power <- function(
    effect_size,
    se,
    alpha = 0.05,
    df = Inf,
    n_sims = 10000L
) {
  effect_size <- tryCatch(as.numeric(effect_size), warning = function(w) {
    cli::cli_abort(c(
      "Cannot coerce `effect_size` to numeric: {conditionMessage(w)}"
    ))
  })

  se <- tryCatch(as.numeric(se), warning = function(w) {
    cli::cli_abort(c(
      "Cannot coerce `se` to numeric: {conditionMessage(w)}"
    ))
  })

  alpha  <- as.numeric(alpha)
  df     <- as.numeric(df)
  n_sims <- as.integer(n_sims)

  obj <- mpr_gelman_carlin_power(effect_size, se, alpha, df, n_sims)
  validate_gelman_carlin_power(obj)
}
