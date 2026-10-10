#' Schedule-aware parametric bootstrap bias correction for Bradley-Terry scores
#'
#' Simulate binary comparisons from fitted BT scores, refit the same estimator,
#' and estimate itemwise bias. Adaptive runs repeat the actual package scheduling
#' algorithm, including changes driven by simulated earlier outcomes.
#'
#' @param object A fit from [fit_bt_model()] or a named finite numeric vector of
#'   initial/generating BT scores. Item IDs must be unique. The initial estimate
#'   also supplies the generating probabilities; a separate generating model is
#'   not fitted. Positional, lapse, and tie models are outside this interface.
#' @param mode Required: `"fixed"` for an outcome-independent frozen schedule or
#'   `"adaptive"` to rerun within-set selection. Do not freeze an observed
#'   outcome-dependent adaptive schedule and describe it as schedule-aware.
#' @param n_rep Required integer number of bootstrap replicates, at least two.
#'   Choose this for the precision and cost of your application.
#' @param seed Required integer master seed, from zero to `.Machine$integer.max`.
#' @param schedule Fixed mode: a data frame with `object1`/`object2` or
#'   `A_id`/`B_id`. Row order, presentations and repeated pairs are retained.
#'   Other columns, including observed outcomes, are ignored. If omitted,
#'   recover `object$comparisons` when available.
#' @param initial_state Adaptive mode: a pristine [adaptive_rank_start()] state,
#'   before any comparison attempts or refits. Its connected N - 1 initial pairs,
#'   initial presentations, predictive initialization, pairing strategy and
#'   constraints remain fixed. Predictive scores must be outcome-independent.
#'   Sparse reservoir membership and presentations are retained, but original
#'   reservoir outcomes are not used. Sessions are never written to disk.
#' @param budget Total number of committed comparisons, including initialization.
#'   Required in adaptive mode; in fixed mode defaults to the number of schedule
#'   rows and must equal that count.
#' @param estimator `"alpha"`, `"brglm2"`, or a function with arguments `bt_data`
#'   and `item_ids`, plus any `estimator_args`. A function returns a named finite
#'   theta vector or a fit with a `theta` data frame (`ID`, `theta`). It must raise
#'   an error for unsuccessful estimation. Reported failed convergence/uncertainty
#'   diagnostics invalidate a returned fit. For alpha/Firth fitted objects,
#'   recover the original estimator/settings and reject conflicting overrides.
#'   Other inputs require an explicit estimator. No automatic engine fallback.
#' @param estimator_args Named estimator arguments. Alpha requires an explicit
#'   `alpha`; numerical controls follow [fit_bt_model()].
#' @param btl_config Adaptive scheduling-refit configuration, passed to
#'   [adaptive_rank_run_live()]. Defaults to the saved configuration or canonical
#'   defaults. This is separate from the final `estimator`. Refits are never
#'   silently disabled: the default scheduling fitter requires optional CmdStan.
#'   An explicitly larger-than-budget `refit_pairs_target` describes a design
#'   without scheduled refits during this budget.
#' @param schedule_fit_fn Optional adaptive scheduling fitter with the existing
#'   `function(state, config)` posterior-fit contract. Omitted means the canonical
#'   Bayesian fitter. Bootstrap supplies deterministic `config$cmdstan$seed` at
#'   each refit. Custom functions must honor it for external RNGs and must be
#'   deterministic, provider-free, and independent of mutable external state.
#' @param min_success Minimum successful replicates, from two through `n_rep`.
#'   Default requires all replicates. Failures are never included in averages.
#' @param workers Positive worker count, default one (serial). More than one
#'   uses optional `future` and `future.apply` with a temporary multisession plan.
#'   The previous plan and caller RNG state are restored.
#' @param keep Retention: `"summary"` (default) keeps summaries and compact
#'   diagnostics; `"theta"` also keeps aligned replicate estimates; `"full"`
#'   additionally keeps comparisons, adaptive states and detailed failure
#'   diagnostics (which can include dense matrices). Full artifacts can be large.
#'
#' @details
#' Each outcome has probability `plogis(theta[A] - theta[B])`. Initial scores and
#' refits are aligned by ID and centered to sum zero. With successful refits only,
#' `bias = mean(theta_boot) - theta_initial` and
#' `theta_corrected = theta_initial - bias`. Corrections can change rankings and
#' need not improve every item or every application.
#'
#' Adaptive p50, Pollitt-inspired, hybrid and random strategies use the canonical
#' live runner with a synthetic judge. Initialization stays frozen, while later
#' selector randomness changes by replicate. Scheduled refits, TrueSkill updates,
#' hybrid identifiability/tapering and constraints remain active. The existing
#' `max_pairs_after_stop` continuation control is set to `budget` so statistical
#' stopping cannot truncate the requested fixed budget. Terminal exhaustion is a
#' failed replicate; it never triggers replacement with another strategy.
#'
#' RNG algorithm is Mersenne-Twister, with Inversion normals and Rejection
#' sampling. For replicate b and stage s (selector, outcomes, scheduling refits,
#' final estimator, numbered 1--4), the integer seed is
#' `max(1, floor((seed * 1000003 + b * 10007 + s * 101 + 304) %% 2147483647))`.
#' Scheduling-refit seeds use the canonical stage derivation again, with the
#' scheduling seed, committed comparison count, stage 1, and offset zero.
#' Serial and parallel results use the same seeds and aggregation order. Exact
#' reproducibility requires compatible R/package/engine versions and deterministic
#' callbacks. There is no checkpoint/resume API; restarting repeats the run.
#'
#' `bootstrap_sd` describes replicate-estimate dispersion, whereas `mcse_bias`
#' is `bootstrap_sd / sqrt(n_success)`: simulation error in the estimated bias.
#' Neither is automatically a standard error for the bias-corrected estimator.
#' This function does not construct confidence intervals or corrected SSR.
#'
#' An unmet `min_success` raises `pairwiseLLM_bt_bootstrap_error` with a `result`
#' field containing diagnostics and missing bias/corrected scores. Tolerated
#' failures raise a warning and remain in the diagnostic table. Numerical warnings
#' from successful fits are recorded per replicate and signaled once at completion.
#'
#' @return A `pairwiseLLM_bt_bootstrap` list containing `theta` (ID, initial theta,
#'   bootstrap mean, bias, corrected theta, bootstrap SD and Monte Carlo error),
#'   `n_success`, `n_failed`, `status`, `replicates` (seeds, failures and schedule
#'   diagnostics), `failures` (available error diagnostics), optional `draws` and
#'   `artifacts`, and `provenance`. Provenance includes normalized generating
#'   parameters, schedule/initial state, estimator/settings, master seed, RNG
#'   convention, scheduling configuration, callbacks and software versions.
#' @examples
#' dat <- data.frame(object1 = rep("a", 8), object2 = rep("b", 8),
#'                   result = c(rep(1, 6), rep(0, 2)))
#' fit <- fit_bt_model(dat, engine = "alpha", alpha = 1, verbose = FALSE)
#' boot <- bootstrap_bt_model(fit, mode = "fixed", n_rep = 20, seed = 304)
#' boot$theta
#' @seealso [fit_bt_model()], [adaptive_rank_start()], [adaptive_rank_run_live()]
#'   and the [integrated CJ workflow](https://shmercer.github.io/pairwiseLLM/articles/adaptive-cj-workflow.html)
#' @family frequentist models
#' @export
bootstrap_bt_model <- function(object, mode, n_rep, seed, schedule = NULL,
                               initial_state = NULL, budget = NULL, estimator = NULL,
                               estimator_args = list(), btl_config = NULL, schedule_fit_fn = NULL,
                               min_success = n_rep, workers = 1L, keep = c("summary", "theta", "full")) {
  mode <- match.arg(mode, c("fixed", "adaptive"))
  keep <- match.arg(keep)
  n_rep <- .bt_bootstrap_integer(n_rep, "n_rep", 2L)
  seed <- .bt_bootstrap_integer(seed, "seed", 0L)
  min_success <- .bt_bootstrap_integer(min_success, "min_success", 2L)
  workers <- .bt_bootstrap_integer(workers, "workers")
  if (min_success > n_rep) .bt_bootstrap_abort("`min_success` cannot exceed `n_rep`.")
  for (pkg in c("withr", if (workers > 1L) c("future", "future.apply"))) {
    if (!.require_ns(pkg)) .bt_bootstrap_abort(paste0("Bootstrap execution requires optional package '", pkg, "'."))
  }
  theta <- .bt_bootstrap_theta(object)
  refit <- .bt_bootstrap_estimator(object, estimator, estimator_args)
  if (mode == "fixed") {
    if (!is.null(initial_state) || !is.null(btl_config) || !is.null(schedule_fit_fn)) {
      .bt_bootstrap_abort("Adaptive state/refit arguments are not used in fixed mode; omit them.")
    }
    if (is.null(schedule) && is.list(object)) schedule <- object$comparisons
    schedule <- .bt_bootstrap_pairs(schedule, names(theta))
    budget <- .bt_bootstrap_integer(budget %||% nrow(schedule), "budget")
    if (budget != nrow(schedule)) .bt_bootstrap_abort("Fixed `budget` must equal the number of schedule rows.")
  } else {
    if (!is.null(schedule)) .bt_bootstrap_abort("Adaptive mode requires `initial_state`, not a frozen schedule.")
    initial_state <- .bt_bootstrap_with_seed(seed, .bt_bootstrap_initial_state(initial_state, names(theta)))
    budget <- .bt_bootstrap_integer(budget, "budget", length(theta) - 1L)
    if (!is.null(schedule_fit_fn) && !is.function(schedule_fit_fn)) {
      .bt_bootstrap_abort("`schedule_fit_fn` must be a function or NULL.")
    }
    btl_config <- .adaptive_btl_resolve_config(initial_state, btl_config %||% initial_state$config$btl_config)
    btl_config$refit_pairs_target <- .bt_bootstrap_integer(
      btl_config$refit_pairs_target %||% .adaptive_refit_pairs_target(initial_state, btl_config), "refit_pairs_target")
  }
  spec <- list(theta = theta, mode = mode, schedule = schedule, initial_state = initial_state,
    budget = budget, refit = refit, btl_config = btl_config, schedule_fit_fn = schedule_fit_fn,
    seed = seed, keep = keep)
  .bt_bootstrap_with_seed(seed, .bt_bootstrap_collect(spec, n_rep, min_success, workers))
}

.bt_bootstrap_collect <- function(spec, n_rep, min_success, workers) {
  if (workers > 1L) {
    old_plan <- future::plan("list")
    on.exit(future::plan(old_plan), add = TRUE)
    future::plan(future::multisession, workers = workers)
  }
  n <- length(spec$theta)
  count <- 0L
  mean <- m2 <- numeric(n)
  diagnostics <- failures <- vector("list", n_rep)
  draws <- if (spec$keep != "summary") matrix(NA_real_, n_rep, n, dimnames = list(NULL, names(spec$theta))) else NULL
  artifacts <- if (spec$keep == "full") vector("list", n_rep) else NULL
  # Bound transient worker results as well as retained state. Reduce in index
  # order, using the same update in serial and parallel execution.
  blocks <- split(seq_len(n_rep), ceiling(seq_len(n_rep) / max(1, 2 * as.double(workers))))
  for (indices in blocks) {
    results <- if (workers == 1L) lapply(indices, .bt_bootstrap_one, spec = spec) else
      future.apply::future_lapply(indices, .bt_bootstrap_one, spec = spec, future.seed = TRUE)
    for (j in seq_along(indices)) {
      i <- indices[[j]]
      out <- results[[j]]
      diagnostics[[i]] <- out$diagnostics
      failures[i] <- list(out$failure)
      if (!is.null(artifacts)) artifacts[i] <- list(out$artifacts)
      if (!is.null(out$theta)) {
        count <- count + 1L
        delta <- unname(out$theta) - mean
        mean <- mean + delta / count
        m2 <- m2 + delta * (unname(out$theta) - mean)
        if (!is.null(draws)) draws[i, ] <- out$theta
      }
    }
  }
  enough <- count >= min_success
  sd <- if (count >= 2L) sqrt(pmax(0, m2 / (count - 1L))) else rep(NA_real_, n)
  bias <- if (enough) mean - unname(spec$theta) else rep(NA_real_, n)
  result <- structure(list(
    theta = tibble::tibble(ID = names(spec$theta), theta_initial = unname(spec$theta),
      bootstrap_mean = if (count) mean else rep(NA_real_, n), bias = bias,
      theta_corrected = unname(spec$theta) - bias, bootstrap_sd = sd, mcse_bias = sd / sqrt(count)),
    n_success = count, n_failed = n_rep - count,
    status = if (!enough) "insufficient_success" else if (count == n_rep) "complete" else "partial",
    replicates = dplyr::bind_rows(diagnostics), failures = failures, draws = draws, artifacts = artifacts,
    provenance = c(spec, list(format_version = 1L, n_rep = n_rep, min_success = min_success,
      rng = c("Mersenne-Twister", "Inversion", "Rejection"), seed_derivation = "canonical_stage_v1_offset_304",
      identification = "sum_to_zero", generating_model = "binary_bt",
      pairing_strategy = if (spec$mode == "fixed") "fixed" else .adaptive_pairing_strategy(spec$initial_state),
      continuation = if (spec$mode == "adaptive") list(max_pairs_after_stop = spec$budget) else NULL,
      versions = c(R = as.character(getRversion()), pairwiseLLM = as.character(getNamespaceVersion("pairwiseLLM")),
        stats = as.character(getNamespaceVersion("stats")),
        if (identical(spec$refit$estimator, "brglm2")) c(brglm2 = as.character(getNamespaceVersion("brglm2"))))))),
    class = "pairwiseLLM_bt_bootstrap")
  if (!enough) {
    .bt_bootstrap_abort("Too few successful bootstrap replicates; inspect `condition$result`.", result = result)
  }
  if (any(!is.finite(as.matrix(result$theta[-1L])))) {
    result$status <- "invalid_summary"
    result$theta$bias <- result$theta$theta_corrected <- NA_real_
    .bt_bootstrap_abort("Bootstrap summary arithmetic is outside the finite numerical range.", result = result)
  }
  if (count < n_rep) rlang::warn("Failed bootstrap replicates were excluded; inspect `replicates` and `failures`.",
    class = "pairwiseLLM_bt_bootstrap_warning")
  if (any(lengths(result$replicates$warnings) > 0L)) {
    rlang::warn("Bootstrap numerical warnings were recorded in `replicates$warnings`.",
      class = "pairwiseLLM_bt_bootstrap_warning")
  }
  result
}
