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

#' Fit a Bradley–Terry model with sirt and fallback to BradleyTerry2
#'
#' This function fits a Bradley–Terry paired-comparison model to data
#' prepared by \code{\link{build_bt_data}}. It supports two modeling
#' engines:
#' \itemize{
#'   \item \pkg{sirt}: \code{\link[sirt]{btm}} — the preferred engine, which
#'         produces ability estimates, standard errors, and MLE reliability.
#'   \item \pkg{BradleyTerry2}: \code{\link[BradleyTerry2]{BTm}} — used as a
#'         fallback if \pkg{sirt} is unavailable or fails; computes ability
#'         estimates and standard errors, but not reliability.
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
#' independently fitted sets. Standard errors are included for both
#' modeling engines. MLE reliability is only available from \pkg{sirt}.
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
#' Connectivity is checked before either engine is called, including ties
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
#' @param bt_data A data frame or tibble with exactly three columns:
#'   two character ID columns and one numeric \code{result} column
#'   equal to 0 or 1. Usually produced by \code{\link{build_bt_data}}.
#' @param engine Character string specifying the modeling engine. One of:
#'   \code{"auto"} (default), \code{"sirt"}, or \code{"BradleyTerry2"}.
#' @param verbose Logical. If \code{TRUE} (default), show engine output (iterations,
#'   warnings). If \code{FALSE}, suppress noisy output to keep
#'   examples and reports clean.
#' @param ... Additional arguments passed through to \code{sirt::btm()}
#'   or \code{BradleyTerry2::BTm()}.
#' @param sirt_eps Optional finite, nonnegative epsilon adjustment for sirt,
#'   supplied by exact name. `NULL` preserves the engine default or legacy
#'   `eps` in `...`. Supplying both forms raises an error. This argument is
#'   invalid with explicit `engine = "BradleyTerry2"`; on automatic fallback
#'   it remains recorded as requested but is not applied to BradleyTerry2.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{engine}{The engine actually used ("sirt" or "BradleyTerry2").}
#'   \item{fit}{The fitted model object.}
#'   \item{theta}{
#'     A tibble with columns:
#'     \itemize{
#'       \item \code{ID}: object identifier
#'       \item \code{theta}: estimated ability parameter
#'       \item \code{se}: standard error of \code{theta}
#'     }
#'   }
#'   \item{reliability}{
#'       MLE reliability (sirt engine only). \code{NA} for
#'       \pkg{BradleyTerry2} models.
#'   }
#'   \item{ssr}{For sirt, the [scale_separation_reliability()] decomposition
#'     plus `engine_reliability`, `agrees`, `absolute_difference`, and
#'     `tolerance`. For BradleyTerry2, `ssr` and `engine_reliability` are `NA`,
#'     `valid` is `FALSE`, `agrees` is `NA`, and `status` is
#'     `"unavailable_se_convention"`.}
#'   \item{provenance}{A list recording `engine`, `requested_engine`, loaded
#'     `engine_version` and `package_version`, `supplied_arguments`,
#'     `requested_sirt_eps`, `effective_settings`, `adjustment`,
#'     `identification`, `convergence` (status, converged, iterations),
#'     `theta_finite`, `se_finite`, `reliability_valid`, `reliability_status`,
#'     and `fallback_reason` (`NULL` unless automatic fallback occurred).
#'     Identification records sirt centering or BradleyTerry2's contrasts,
#'     reference category and player levels. Save the full object to retain
#'     these settings; the legacy summary tibble is unchanged.}
#' }
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
                         engine = c("auto", "sirt", "BradleyTerry2"),
                         verbose = TRUE,
                         ...,
                         sirt_eps = NULL) {
  bt_data <- as.data.frame(bt_data)
  if (ncol(bt_data) != 3L) {
    stop("`bt_data` must have exactly three columns.", call. = FALSE)
  }

  engine <- match.arg(engine)
  .bt_validate_data(bt_data)
  dots <- list(...)
  if (!is.null(sirt_eps)) {
    .bt_validate_eps(sirt_eps)
    if (engine == "BradleyTerry2") .bt_abort("`sirt_eps` requires engine = 'sirt' or 'auto'.")
  }

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
