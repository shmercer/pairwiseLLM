# ------------------------------------------------------------------------------
# Internal wrappers
# ------------------------------------------------------------------------------
# These small helpers serve two purposes:
#  1) They keep `fit_bt_model()` readable by centralising namespace calls.
#  2) They make hard-to-test branches testable *without* heavy-handed stubbing
#     (e.g., mocking `base::requireNamespace()` or `sirt::btm()` directly).


.require_ns <- function(pkg, quietly = TRUE) {
  base::requireNamespace(pkg, quietly = quietly)
}

.sirt_btm <- function(...) {
  sirt::btm(...)
}

#' Build Bradley-Terry comparison data from pairwise results
#'
#' This function converts pairwise comparison results into the
#' three-column format commonly used for Bradley-Terry models:
#' the first two columns contain object labels and the third
#' column contains the comparison result (1 for a win of the
#' first object, 0 for a win of the second).
#'
#' It accepts either:
#' \itemize{
#'   \item legacy columns \code{ID1}, \code{ID2}, \code{better_id}, or
#'   \item canonical columns \code{A_id}, \code{B_id}, \code{better_id}.
#' }
#' Rows where \code{better_id} does not match either side of the pair
#' (including \code{NA}) are excluded.
#'
#' @param results A data frame or tibble with either
#'   \code{ID1}/\code{ID2}/\code{better_id} or
#'   \code{A_id}/\code{B_id}/\code{better_id}.
#'
#' @return A tibble with three columns:
#'   \itemize{
#'     \item \code{object1}: ID from \code{ID1}
#'     \item \code{object2}: ID from \code{ID2}
#'     \item \code{result}: numeric value, 1 if \code{better_id == ID1},
#'       0 if \code{better_id == ID2}
#'   }
#'   Rows with invalid or missing \code{better_id} are dropped.
#'
#' @examples
#' results <- tibble::tibble(
#'   ID1       = c("S1", "S1", "S2"),
#'   ID2       = c("S2", "S3", "S3"),
#'   better_id = c("S1", "S3", "S2")
#' )
#'
#' bt_data <- build_bt_data(results)
#' bt_data
#'
#' # Using the example writing pairs
#' data("example_writing_pairs")
#' bt_ex <- build_bt_data(example_writing_pairs)
#' head(bt_ex)
#'
#' @seealso [fit_bt_model()], [summarize_bt_fit()]
#' @family frequentist models
#' @export
build_bt_data <- function(results) {
  results <- tibble::as_tibble(results)

  has_legacy <- all(c("ID1", "ID2", "better_id") %in% names(results))
  has_canonical <- all(c("A_id", "B_id", "better_id") %in% names(results))
  if (!has_legacy && !has_canonical) {
    rlang::abort(
      "`results` must contain either `ID1`, `ID2`, `better_id` or `A_id`, `B_id`, `better_id`."
    )
  }

  if (has_legacy) {
    id1 <- as.character(results$ID1)
    id2 <- as.character(results$ID2)
  } else {
    id1 <- as.character(results$A_id)
    id2 <- as.character(results$B_id)
  }
  better_id <- as.character(results$better_id)

  out <- dplyr::mutate(
    tibble::tibble(id1 = id1, id2 = id2, better_id = better_id),
    result = dplyr::case_when(
      .data$better_id == .data$id1 ~ 1L,
      .data$better_id == .data$id2 ~ 0L,
      TRUE ~ NA_integer_
    )
  )

  out <- dplyr::filter(out, !is.na(.data$result))

  out <- dplyr::transmute(
    out,
    object1 = .data$id1,
    object2 = .data$id2,
    result  = as.numeric(.data$result) # sirt::btm is happiest with numeric 0/1
  )

  tibble::as_tibble(out)
}

#' Fit a Bradley-Terry model with optional frequentist engines
#'
#' This function fits a Bradley–Terry paired-comparison model to data
#' prepared by \code{\link{build_bt_data}}. It supports five modeling
#' engines:
#' \itemize{
#'   \item \pkg{sirt}: \code{\link[sirt]{btm}} — the default engine, which
#'         produces ability estimates, standard errors, and MLE reliability.
#'   \item \pkg{BradleyTerry2}: \code{\link[BradleyTerry2]{BTm}} — used as a
#'         fallback if \pkg{sirt} is unavailable or fails; computes ability
#'         estimates and standard errors, but not reliability.
#'   \item \pkg{brglm2}: explicit Firth mean bias reduction for random or
#'         nonadaptive schedules, with centered covariance and SSR.
#'   \item `alpha`: explicit alpha adjustment motivated by adaptive schedules,
#'         using base R with centered covariance and SSR.
#'   \item `lapse`: experimental unpenalized model matching with estimated
#'         first-position bias and lapse probability, using base R.
#' }
#'
#' When \code{engine = "auto"} (the default), the function attempts
#' \pkg{sirt} first and automatically falls back to \pkg{BradleyTerry2}
#' on availability or execution failure. Explicit engine requests never fall
#' back. Invalid inputs, disconnected graphs, and invalid reliability results
#' raise errors even in automatic mode. The output format is standardized, so
#' downstream code can rely on consistent fields.
#'
#' @details
#' The input \code{bt_data} must contain exactly three columns:
#' \enumerate{
#'   \item object1: character ID for the first item in the pair
#'   \item object2: character ID for the second item
#'   \item result: numeric indicator (1 = object1 wins, 0 = object2 wins)
#' }
#'
#' Ability estimates (\code{theta}) represent latent "writing quality"
#' parameters on a log-odds scale. Higher values mean stronger relative
#' performance on the assessed trait. Zero is not a pass mark, and these
#' estimates are not rubric grades or automatically comparable across
#' independently fitted sets. Standard errors are included for all
#' modeling engines when their uncertainty checks pass. Raw MLE reliability is available from \pkg{sirt};
#' Firth and alpha fits return independently calculated SSR.
#'
#' For sirt, `$ssr` independently calculates
#' `1 - mean(se^2) / stats::var(theta)` from all returned items, using sample
#' variance. Agreement with `$fit$mle.rel` is required within
#' `1e-12 * max(1, abs(engine_reliability), abs(ssr))`. Negative SSR is retained.
#' Nonfinite theta/SEs, negative SEs, zero score variance, and nonfinite
#' calculations raise errors without dropping items. This includes sirt fits
#' with missing SEs from fixed theta or extreme scores when epsilon is zero.
#' See [scale_separation_reliability()] for the component definitions.
#'
#' SSR depends on estimated score variance and the SE convention: it is not
#' an estimator-free measure of recovery. BradleyTerry2 uses engine contrasts
#' (normally a reference item); this wrapper does not calculate SSR from its
#' reference-based SEs. Its legacy `$reliability` remains `NA`.
#'
#' The sirt default epsilon is resolved from the installed engine (0.3 in
#' sirt 4.2.133), and the returned epsilon is checked and recorded. Other
#' estimator defaults are unchanged, including sirt's tie and positional
#' parameters. `effective_settings` records resolved arguments; for sirt,
#' `fix.delta_requested` is separated from `returned_parameters` because
#' sirt 4.2.133 accepts but does not apply `fix.delta`. Other engine versions
#' are marked unverified for that argument. No fix for the upstream estimator
#' is applied here.
#'
#' Connectivity is checked before any engine is called, including ties
#' removed by `ignore.ties = TRUE`, and BradleyTerry2 subsets/zero weights.
#' Disconnected data cannot identify global BT scores or SSR. Missing outcomes,
#' invalid IDs, and self-comparisons raise errors rather than being dropped.
#' Direct sirt inputs may include ties coded 0.5; the BradleyTerry2 wrapper
#' requires binary outcomes.
#'
#' sirt provides iterations but no explicit convergence flag. Early termination
#' is recorded as `stopping_criterion_met`; reaching `maxiter` is recorded as
#' `iteration_limit_reached` with `converged = NA`, not as proven convergence
#' or nonconvergence. BradleyTerry2's reported convergence is preserved.
#' Reliability validity describes the arithmetic, not proof of convergence.
#'
#' Install an optional engine before fitting, for example with
#' `install.packages("sirt")`. Pairwise data preparation does not need that
#' engine. See the [offline walkthrough](https://shmercer.github.io/pairwiseLLM/articles/getting-started.html)
#' for fitting and interpreting bundled synthetic comparisons.
#'
#' Firth fits use binomial-logit `brglm2::brglmFit` with `type = "AS_mean"`,
#' equivalent to adding half the log determinant of expected information to
#' the log likelihood. This is a genuine Firth estimator, not sirt epsilon
#' adjustment. It is an explicit option for random/nonadaptive schedules;
#' it is not recommended here as the primary adaptive-schedule correction.
#' No schedule type is inferred from outcomes. See Hamilton and Tawn,
#' \doi{10.1111/jedm.70022}, and the `brglm2` mean-bias-reduction documentation.
#'
#' The Firth design has no intercept, tie, positional, or lapse parameter.
#' Binary comparisons are aggregated in deterministic item/pair order.
#' Internal contrasts use the last radix-sorted item as reference, then both
#' estimates and covariance are transformed to sum-to-zero coordinates.
#' `$vcov` is the model-based inverse expected information at the bias-reduced
#' estimate, transformed to item coordinates; it is not a penalized-Hessian
#' or bootstrap covariance. Its rank is the number of items minus one because
#' of centering. SEs are square roots of its diagonal. Separation and undefeated
#' or winless items are supported when the comparison graph is connected.
#'
#' Firth fits must converge with finite estimates and valid covariance. Failures
#' error without fallback. A valid fit with zero score variance is retained:
#' `$reliability` is `NA` and `$ssr$status` is `"zero_score_variance"`.
#' Other invalid theta/SE or reliability arithmetic raises an error. Use
#' [predict.pairwiseLLM_bt_firth()] for first-item win probabilities.
#'
#' @param bt_data A data frame or tibble with exactly three columns:
#'   two character ID columns and one numeric \code{result} column
#'   equal to 0 or 1. Usually produced by \code{\link{build_bt_data}}.
#' @param engine Character string specifying the modeling engine. One of:
#'   \code{"auto"} (default), \code{"sirt"}, \code{"BradleyTerry2"},
#'   \code{"brglm2"}, `"alpha"`, or `"lapse"`. Automatic selection never chooses
#'   Firth, alpha, or lapse.
#' @param verbose Logical. If \code{TRUE} (default), show engine output (iterations,
#'   warnings). If \code{FALSE}, suppress noisy output to keep
#'   examples and reports clean.
#' @param ... Additional arguments passed through to \code{sirt::btm()}
#'   or \code{BradleyTerry2::BTm()}. For `brglm2`, only a named `control`
#'   list is accepted, with numerical settings `epsilon` (default `1e-10`),
#'   `maxit` (200), `slowit` (1), `max_step_factor` (12), and `trace` (FALSE).
#'   `verbose = FALSE` disables tracing; numerical warnings are retained.
#'   The mean-bias-reduction method cannot be changed through controls.
#'   For `alpha`, only a named `control` list is accepted: `epsilon` (default
#'   `1e-12`), `maxit` (200), `gradient_tol` (`1e-7`), `step_tol` (`1e-7`),
#'   `min_rcond` (`1e-12`), and `trace` (FALSE). See the alpha section below.
#'   For `lapse`, only a named `control` list is accepted: `maxit` (2000),
#'   `reltol` (`1e-12`), `gradient_tol` (`1e-7`), `step_tol` (`1e-7`),
#'   `min_rcond` (`1e-12`), and `trace` (FALSE). See the lapse section below.
#' @param sirt_eps Optional finite, nonnegative epsilon adjustment for sirt,
#'   supplied by exact name. `NULL` preserves the engine default or legacy
#'   `eps` in `...`. Supplying both forms raises an error. This argument is
#'   valid only with `engine = "sirt"` or `"auto"`; on automatic fallback
#'   it remains recorded as requested but is not applied to BradleyTerry2.
#'
#' @param alpha Explicit finite nonnegative numeric scalar, supplied by exact
#'   name, required only for `engine = "alpha"`. There is no default penalty
#'   and no tuning from outcomes. Values 0.30 and 0.50 are supported alongside
#'   other nonnegative values. Zero requests ordinary unpenalized estimation
#'   and requires a strongly connected directed win graph. `NULL` is only
#'   accepted for other engines.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{engine}{The engine actually used ("sirt", "BradleyTerry2", "brglm2", "alpha", or "lapse").}
#'   \item{fit}{The fitted model object.}
#'   \item{theta}{
#'     A tibble with columns:
#'     \itemize{
#'       \item \code{ID}: object identifier
#'       \item \code{theta}: estimated ability parameter
#'       \item \code{se}: standard error of \code{theta}; `NA` for valid lapse-boundary fits
#'     }
#'   }
#'   \item{reliability}{
#'       Raw MLE reliability for sirt or calculated SSR for Firth/alpha. \code{NA} for
#'       \pkg{BradleyTerry2} and lapse models or a zero-variance Firth/alpha fit.
#'   }
#'   \item{ssr}{For sirt, the [scale_separation_reliability()] decomposition
#'     plus `engine_reliability`, `agrees`, `absolute_difference`, and
#'     `tolerance`. For BradleyTerry2, `ssr` and `engine_reliability` are `NA`,
#'     `valid` is `FALSE`, `agrees` is `NA`, and `status` is
#'     `"unavailable_se_convention"`. For Firth/alpha, the helper decomposition, or
#'     `valid = FALSE` and `status = "zero_score_variance"` when undefined.}
#'   \item{provenance}{A list recording `engine`, `requested_engine`, loaded
#'     `engine_version` and `package_version`, `supplied_arguments`,
#'     `requested_sirt_eps`, `effective_settings`, `adjustment`,
#'     `identification`, `convergence` (status, converged, iterations),
#'     `theta_finite`, `se_finite`, `reliability_valid`, `reliability_status`,
#'     and `fallback_reason` (`NULL` unless automatic fallback occurred).
#'     Identification records sirt centering or BradleyTerry2's contrasts,
#'     reference category and player levels. Save the full object to retain
#'     these settings; the legacy summary tibble is unchanged. Firth provenance
#'     also records the coordinate transformation and covariance convention.
#'     Alpha adds `engine_package = "stats"`, the explicit penalty, solver,
#'     parameter ordering, convergence code/message and uncertainty scope.}
#'   \item{vcov}{Firth/alpha/lapse: centered item covariance matrix, with row/column
#'     labels in the same order as `theta$ID`; `NULL` for valid lapse-boundary fits.}
#'   \item{comparisons}{Firth/alpha/lapse: original item pairs for default prediction.}
#'   \item{beta, epsilon, model_variant}{Lapse only: first-position bias, guessing
#'     probability, and exact variant `"btl_e_b"`.}
#'   \item{parameter_vcov}{Lapse only: joint centered theta/beta/epsilon covariance,
#'     ordered by `theta:ID` labels followed by `beta` and `epsilon`; `NULL` at the zero boundary.}
#'   \item{log_likelihood, objective}{Lapse only: unpenalized log likelihood and
#'     its negative. No penalty or parameter-transformation Jacobian is added.}
#'   \item{alpha}{Alpha engine only: the requested penalty strength.}
#'   \item{diagnostics}{Alpha: objective components, item scores,
#'     reduced-coordinate gradient, penalized Hessian and unpenalized information,
#'     matrix checks, Newton correction, optimizer status, and numerical warnings.
#'     Lapse: ordered-pair probabilities, natural-coordinate score and observed
#'     Hessian, information/matrix checks, Newton correction, and all optimizer
#'     attempts including the epsilon-zero boundary and epsilon-one objective.}
#' }
#'
#' @section Experimental frequentist lapse model matching:
#' `engine = "lapse"` matches the Bayesian `btl_e_b` likelihood:
#' \deqn{p(A\ wins)=(1-\epsilon)\operatorname{logit}^{-1}(\theta_A-\theta_B+\beta)+\epsilon/2.}
#' Positive beta favors the first presented item; epsilon in `[0, 1]` is the
#' probability of guessing, distinct from sirt's epsilon adjustment. The fit
#' estimates all three parameter groups jointly, with sum-to-zero theta, no
#' penalty and no Bayesian priors. It is a model-matching prototype, not a
#' replacement for Bayesian estimation or a production recommendation.
#'
#' Binary rows are aggregated by ordered pair. Joint optimization uses the
#' existing centered reference contrasts and natural epsilon constrained to
#' `[0, 1]`, with L-BFGS-B. Starts have zero item contrasts and beta, and epsilon
#' 0.001, 0.05, 0.2, 0.5 and 0.9. The objective and gradient are divided by the
#' comparison count for optimization; reported likelihoods, Hessians and
#' stationarity checks use the unscaled sum. L-BFGS-B uses
#' `factr = reltol / .Machine$double.eps`, and its projected-score tolerance is
#' `gradient_tol` divided by the comparison count. Up to ten damped Newton steps
#' polish successful attempts in natural coordinates, within the bounds.
#' The highest-likelihood candidate is checked independently; a materially worse
#' solution is never selected to obtain valid uncertainty. Native convergence,
#' natural-coordinate scores and centered Newton corrections must satisfy the
#' numerical controls. All attempts, including failed ones, remain in diagnostics.
#'
#' The epsilon-zero face is optimized separately with BFGS, and the epsilon-one
#' constant-probability likelihood is evaluated exactly. The zero-face solution
#' must pass its own theta/beta convergence, stationarity and curvature checks
#' even when an interior fit is selected. It can be selected when its objective
#' is lower or within `100 * .Machine$double.eps * max(1, abs(objective))` of the
#' joint candidate and its one-sided negative-log-likelihood epsilon score is
#' at least `-gradient_tol`. A small positive fitted epsilon alone does not
#' establish a boundary optimum. There are no bounds on item strengths or beta,
#' penalties, or clipping of probabilities, epsilon, or Hessian eigenvalues.
#' Out-of-domain optimizer evaluations are retained as failed attempts.
#'
#' Connectedness alone does not establish identification of lapse and bias.
#' The ordered design must have sufficient rank, and full natural-parameter
#' expected information must be positive definite with reciprocal condition
#' number at least `min_rcond`. Interior fits also require a positive definite,
#' well-conditioned joint observed negative-log-likelihood Hessian. Its inverse
#' is transformed to centered theta, beta and natural epsilon; theta SEs include
#' estimation of both nuisance parameters. The full covariance is positive
#' semidefinite and singular only because theta sums to zero. SEs are local,
#' model-based approximations conditional on the realized graph; these checks
#' do not establish repeated-sample calibration.
#'
#' An identified epsilon-zero optimum is a valid point-estimate fit. It returns
#' theta, beta, exactly zero epsilon, likelihoods and predictions, with
#' `provenance$convergence$status = "converged_boundary"` and `converged = TRUE`.
#' Required observed curvature is positive definite along the theta/beta face;
#' when the epsilon score is within `gradient_tol` of zero, the full joint
#' curvature must also pass. Ordinary joint lapse-model uncertainty is not
#' reported: `theta$se` is `NA`, `vcov` and `parameter_vcov` are `NULL`, and
#' `provenance$uncertainty` has `method = "none"`, `valid = FALSE`, and
#' `status = "nonregular_boundary"`. Interior fits have uncertainty status
#' `"valid"`. Epsilon one leaves theta and beta unidentified and is rejected.
#'
#' Genuine failed checks raise `pairwiseLLM_bt_lapse_error`, also a BT validation
#' error, retaining available `theta` (without SEs), `beta`, `epsilon`,
#' `diagnostics`, `provenance` and `failure_reason`. No regularization or simple-BT
#' fallback is introduced after failure. Conventional SSR is unavailable for
#' this prototype (`reliability = NA`, `ssr$valid = FALSE`), including valid
#' boundary and interior fits. [bootstrap_bt_model()] rejects these fit objects.
#' Boundary intervals and bootstrap uncertainty are outside this prototype.
#'
#' The original frozen issue-305 audit recovered 324 of 450 cases and rejected
#' 100 boundary candidates and 26 numerical failures. The boundary follow-up
#' repeats the identical cases and recovery tolerances: 352 interior-valid fits
#' and 98 boundary-valid fits recovered, with zero numerical/identification
#' failures and zero recovery failures among valid fits. Read its results with
#' `readLines(system.file("validation", "bt-lapse-305-boundary.txt", package = "pairwiseLLM"))`.
#' The original `bt-lapse-305.txt` and accompanying evidence are preserved.
#' This synthetic qualification does not establish production validity,
#' boundary interval coverage or repeated-sample uncertainty calibration.
#' See [predict.pairwiseLLM_bt_lapse()] for ordered plug-in probabilities.
#'
#' @section Alpha-adjusted estimation:
#' Hamilton and Tawn (\doi{10.1111/jedm.70022}, equation 3) define an adjustment
#' to the score equation for item r:
#' \deqn{a_r = \alpha\left(1 - \frac{2}{N-1}\sum_{j\ne r}p_{rj}\right).}
#' With \eqn{p_{ij}=\operatorname{logit}^{-1}(\theta_i-\theta_j)}, observed win
#' counts \eqn{w_{ij}}, and \eqn{c=\alpha/(N-1)}, the implemented objective is
#' \deqn{\ell_\alpha(\theta) = \sum_{i<j}\{w_{ij}\log p_{ij} +
#' w_{ji}\log(1-p_{ij})\} + c\sum_{i<j}\log\{p_{ij}(1-p_{ij})\}.}
#' The penalty covers every unordered pair, including unobserved pairs, and
#' adds c pseudo-wins in each direction. It differs from sirt's conventional
#' epsilon adjustment, which uses observed win proportions. Neither method
#' changes which pairs are selected. Alpha adjustment is motivated by adaptive
#' scheduling; it is not universally preferred or a guarantee of unbiased SSR.
#' Firth remains the intended modern comparator for random schedules.
#' [bootstrap_bt_model()] separately provides schedule-aware bootstrap bias
#' correction; it does not turn these conditional SEs into corrected-score SEs.
#'
#' The alpha engine shares Firth's binary input and sum-to-zero convention.
#' Items are radix-sorted; coefficient i is the contrast of item i to the last
#' item, for i = 1,...,N-1. If B is the centered reference map, theta = B beta.
#' For pair design row x and total observed comparisons m, the negative
#' objective Hessian is \eqn{H_\alpha=\sum_{i<j}(m_{ij}+2c)p_{ij}(1-p_{ij})xx^T}.
#' The original-data information is \eqn{I=\sum_{i<j}m_{ij}p_{ij}(1-p_{ij})xx^T}.
#' The returned covariance is \eqn{B I^{-1} B^T}, evaluated at the alpha estimate,
#' with SEs from its diagonal. These are model-based SEs conditional on the
#' realized comparison graph, not schedule-aware uncertainty. The penalized
#' Hessian, inverse penalized curvature, and sandwich covariance are not used
#' for reported SEs or SSR. Centering makes the item covariance rank N-1.
#'
#' A single `stats::glm.fit` IWLS fit uses weighted binary rows for the augmented
#' counts, zero starts,
#' and a quasibinomial-logit working family with dispersion fixed at one.
#' This supplies the exact binomial-logit estimating equations without warnings
#' about fractional pseudo-counts; no dispersion estimate or GLM covariance is
#' used. The objective and derivatives are evaluated independently. Native
#' convergence and full rank are necessary but not sufficient: the maximum
#' absolute adjusted item score must be at most `gradient_tol`, and the maximum
#' absolute item-coordinate Newton correction at most `step_tol`. Both reduced
#' matrices must be positive definite with reciprocal condition number at least
#' `min_rcond`. All numeric controls must be positive and finite; `maxit` must
#' be an integer, `min_rcond` less than one, and `trace` logical.
#'
#' Fits never change alpha or solver after failure. Numerical validation errors
#' have class `pairwiseLLM_bt_alpha_error` (also a BT validation error). Their
#' `theta`, `provenance`, `diagnostics`, and `failure_reason` fields preserve
#' available results for auditing, including converged theta if uncertainty
#' fails. No theta-only public fit is returned. A valid equal-strength fit is
#' retained with `NA` SSR and `zero_score_variance` status, as for Firth.
#' When every item's total wins equal its total losses, zero is the exact
#' stationary solution. `$diagnostics$exact_zero_solution` records its use;
#' `$diagnostics$coefficients` are the effective contrasts, while `$fit` retains
#' the raw IWLS output. This is an exact count-based identity, not rounding small
#' estimates to zero. Native convergence and uncertainty checks still apply.
#' Extremely small/large penalties can exceed numerical resolution and error.
#' The dense all-pair design has no large-scale sparse-optimization guarantee.
#' Use [predict.pairwiseLLM_bt_alpha()] for plug-in pair probabilities.
#'
#' @examples
#' # Example using built-in comparison data
#' data("example_writing_pairs")
#' bt <- build_bt_data(example_writing_pairs)
#'
#' if (requireNamespace("sirt", quietly = TRUE)) {
#'   fit1 <- fit_bt_model(bt, engine = "sirt", sirt_eps = 0.3, verbose = FALSE)
#'   fit1$ssr
#'   fit1$provenance$adjustment
#' }
#' if (requireNamespace("BradleyTerry2", quietly = TRUE)) {
#'   fit2 <- fit_bt_model(bt, engine = "BradleyTerry2", verbose = FALSE)
#' }
#'
#' @import tibble
#' @import dplyr
#' @importFrom stats aggregate
#' @seealso [build_bt_data()], [summarize_bt_fit()]
#' @family frequentist models
#' @export
fit_bt_model <- function(bt_data,
                         engine = c("auto", "sirt", "BradleyTerry2", "brglm2", "alpha", "lapse"),
                         verbose = TRUE,
                         ...,
                         sirt_eps = NULL,
                         alpha = NULL) {
  bt_data <- as.data.frame(bt_data)
  if (ncol(bt_data) != 3L) {
    stop("`bt_data` must have exactly three columns.", call. = FALSE)
  }

  # Preserve the formerly unambiguous abbreviation for the default engine.
  if (identical(engine, "a")) engine <- "auto"
  engine <- match.arg(engine)
  .bt_validate_data(bt_data)
  dots <- list(...)
  if (!is.null(sirt_eps)) {
    .bt_validate_eps(sirt_eps)
    if (!engine %in% c("sirt", "auto")) .bt_abort("`sirt_eps` requires engine = 'sirt' or 'auto'.")
  }
  if (!is.null(alpha) && engine != "alpha") .bt_abort("`alpha` requires engine = 'alpha'.")
  if (engine == "alpha") return(.bt_fit_alpha(bt_data, alpha, verbose, dots))
  if (engine == "lapse") return(.bt_fit_lapse(bt_data, verbose, dots))
  if (engine == "brglm2") return(.bt_fit_firth(bt_data, verbose, dots))

  # --------------------------
  # sirt helper
  # --------------------------
  fit_sirt <- function(dat, verbose, ...) {
    if (!.require_ns("sirt", quietly = TRUE)) {
      stop(
        "Package 'sirt' must be installed to use engine = \"sirt\".\n",
        "Install it with: install.packages(\"sirt\")",
        call. = FALSE
      )
    }

    settings <- .bt_sirt_settings(dat, list(...), sirt_eps)
    # sirt::btm often prints iteration progress. Capture when verbose = FALSE.
    run_btm <- function() do.call(.sirt_btm, c(list(dat), settings))

    fit <- if (isTRUE(verbose)) {
      run_btm()
    } else {
      suppressWarnings({
        tmp <- utils::capture.output(
          fit0 <- run_btm(),
          type = "output"
        )
        invisible(tmp)
        fit0
      })
    }

    effects <- fit$effects
    if (is.null(effects)) {
      .bt_abort("sirt::btm output missing `effects`.")
    }

    if (!all(c("individual", "theta", "se.theta") %in% names(effects))) {
      .bt_abort(paste0(
        "sirt::btm$effects does not contain expected columns ",
        "`individual`, `theta`, `se.theta`."
      ))
    }

    theta <- tibble::tibble(
      ID    = effects$individual,
      theta = effects$theta,
      se    = effects$se.theta
    )

    ssr <- .bt_ssr_sirt(fit, theta)
    provenance <- .bt_provenance("sirt", engine, fit, settings, dots, sirt_eps, ssr)
    list(
      engine      = "sirt",
      fit         = fit,
      theta       = theta,
      reliability = fit$mle.rel,
      ssr = ssr, provenance = provenance
    )
  }

  # --------------------------
  # BradleyTerry2 helper
  # --------------------------
  fit_bt2 <- function(dat, verbose, ...) {
    if (!.require_ns("BradleyTerry2", quietly = TRUE)) {
      stop(
        "Package 'BradleyTerry2' must be installed to use engine = \"BradleyTerry2\".\n",
        "Install it with: install.packages(\"BradleyTerry2\")",
        call. = FALSE
      )
    }

    if (any(dat[[3L]] == 0.5)) .bt_abort("BradleyTerry2 requires binary outcomes; ties are not supported here.")
    dat <- as.data.frame(dat)
    names(dat)[1:3] <- c("object1", "object2", "result")

    # Aggregate wins for object1 vs object2
    wins1 <- stats::aggregate(I(result == 1) ~ object1 + object2, data = dat, sum)
    wins0 <- stats::aggregate(I(result == 0) ~ object1 + object2, data = dat, sum)

    agg <- merge(wins1, wins0, by = c("object1", "object2"), all = TRUE)
    agg[is.na(agg)] <- 0
    names(agg)[3:4] <- c("win1", "win2")

    # Force both player factors to share identical levels
    players <- sort(unique(c(agg$object1, agg$object2)))
    agg$object1 <- factor(agg$object1, levels = players)
    agg$object2 <- factor(agg$object2, levels = players)

    settings <- .bt_resolve_settings(
      BradleyTerry2::BTm,
      c(list(outcome = cbind(agg$win1, agg$win2), player1 = agg$object1,
             player2 = agg$object2, data = agg), list(...)),
      c("outcome", "player1", "player2", "data")
    )
    used <- seq_len(nrow(agg))
    if (!is.null(settings$subset)) used <- used[settings$subset]
    if (!is.null(settings$weights)) used <- used[settings$weights[used] > 0]
    if (anyNA(used)) .bt_abort("BradleyTerry2 subset/weights select missing comparisons.")
    .bt_check_connected(agg[used, , drop = FALSE], players)

    # Fit; optionally suppress warnings when verbose = FALSE (keeps examples clean)
    fit <- if (isTRUE(verbose)) {
      BradleyTerry2::BTm(
        outcome = cbind(agg$win1, agg$win2),
        player1 = agg$object1,
        player2 = agg$object2,
        data    = agg,
        ...
      )
    } else {
      suppressWarnings(
        BradleyTerry2::BTm(
          outcome = cbind(agg$win1, agg$win2),
          player1 = agg$object1,
          player2 = agg$object2,
          data    = agg,
          ...
        )
      )
    }

    abil <- BradleyTerry2::BTabilities(fit)

    theta <- tibble::tibble(
      ID    = rownames(abil),
      theta = abil[, 1],
      se    = abil[, 2]
    )

    .bt_validate_estimates(theta$theta, theta$se)
    ssr <- list(ssr = NA_real_, valid = FALSE, status = "unavailable_se_convention",
                engine_reliability = NA_real_, agrees = NA)
    provenance <- .bt_provenance("BradleyTerry2", engine, fit, settings, dots, sirt_eps, ssr)
    list(
      engine      = "BradleyTerry2",
      fit         = fit,
      theta       = theta,
      reliability = NA_real_,
      ssr = ssr, provenance = provenance
    )
  }

  # --------------------------
  # Dispatch
  # --------------------------
  if (engine == "sirt") {
    return(fit_sirt(bt_data, verbose = verbose, ...))
  }

  if (engine == "BradleyTerry2") {
    return(fit_bt2(bt_data, verbose = verbose, ...))
  }

  engine_error <- function(e) {
    # Scientific validation errors must not silently change the estimator.
    if (inherits(e, "pairwiseLLM_bt_validation_error")) stop(e)
    e
  }
  res_sirt <- tryCatch(
    fit_sirt(bt_data, verbose = verbose, ...),
    error = engine_error
  )
  if (!inherits(res_sirt, "error")) {
    return(res_sirt)
  }

  res_bt2 <- tryCatch(
    fit_bt2(bt_data, verbose = verbose, ...),
    error = engine_error
  )
  if (!inherits(res_bt2, "error")) {
    res_bt2$provenance$fallback_reason <- conditionMessage(res_sirt)
    return(res_bt2)
  }

  stop(
    "Both sirt and BradleyTerry2 failed:\n",
    "sirt error: ", conditionMessage(res_sirt), "\n",
    "BradleyTerry2 error: ", conditionMessage(res_bt2),
    call. = FALSE
  )
}
