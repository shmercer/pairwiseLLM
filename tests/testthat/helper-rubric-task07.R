# Completed-result boundary fixtures, not simulations of adaptive stopping.
rubric_completed_adaptive <- function(cj, trait = "organization") {
  fit <- utils::tail(cj$fits, 1L)[[1L]]
  ids <- names(fit$theta_mean)
  now_fn <- function() as.POSIXct("2026-09-12", tz = "UTC")
  environment(now_fn) <- baseenv()
  state <- pairwiseLLM::adaptive_rank_start(
    tibble::tibble(item_id = ids, global_item_id = ids), seed = 9207L,
    now_fn = now_fn)
  state$trait <- trait
  state$btl_fit <- pairwiseLLM:::.adaptive_btl_adapt_fit(fit)
  state$config$btl_config$model_variant <- fit$model_variant
  state$item_log <- list(pairwiseLLM:::.adaptive_build_item_log_refit(state, 1L))
  n_pairs <- length(ids) - 1L
  state$round_log <- pairwiseLLM:::append_round_log(state$round_log,
    list(refit_id = 1L, step_id_at_refit = n_pairs, total_pairs_done = n_pairs,
      model_variant = fit$model_variant, stop_decision = TRUE,
      stop_reason = "btl_converged", reliability_EAP = 0.95))
  state$refit_meta$last_refit_round_id <- 1L
  state$refit_meta$last_refit_step <- n_pairs
  state$refit_meta$last_refit_M_done <- n_pairs
  state$meta$stop_decision <- TRUE
  state$meta$stop_reason <- "btl_converged"
  state$step_log <- tibble::tibble(pair_id = seq_len(n_pairs))
  state$history_pairs <- tibble::tibble(A_id = ids[-length(ids)], B_id = ids[-1L])
  state
}

rubric_skip_method <- function(method) {
  if (method == "ordinal_linear") testthat::skip_if_not_installed("ordinal")
  if (method == "ordinal_monotone") {
    testthat::skip_if_not_installed("mgcv", minimum_version = "1.9.4")
    testthat::skip_if_not_installed("withr")
  }
}

rubric_workflow_fit <- function(cj, rubric, method, K = 3L, trait = "organization") {
  pairwiseLLM::fit_rubric_calibration(cj,
    if (method == "percentile") NULL else rubric,
    method = method, levels = seq_len(K), trait = trait)
}

# Deterministic stratified logistic quantiles and an independent probability oracle.
# Every location has training and held-out items on the same completed CJ scale.
rubric_recovery_data <- function(K = 3L, shape = "linear", imbalanced = FALSE,
                                 spacing = 1.5, shift = 0, repeats = 80L) {
  x <- rep(seq(-3, 3, length.out = 25L), each = repeats)
  eta <- switch(shape, linear = 1.2 * x, compression = 3 * tanh(x),
    s_shape = 0.35 * x + 0.5 * x^3, weak = 0.03 * x, reversed = -1.2 * x)
  thresholds <- seq(-spacing, spacing, length.out = K - 1L) + if (imbalanced) 3 else 0
  cumulative <- stats::plogis(outer(eta, thresholds, function(e, tau) tau - e))
  truth <- cbind(cumulative, 1) - cbind(0, cumulative)
  # Each half separately spans the logistic quantiles; labels are never sampled to ensure categories.
  u <- rep((seq_len(repeats) - 0.5) / repeats, 25L)
  category <- as.integer(1L + rowSums(cumulative < u))
  theta <- x + shift
  cj <- rubric_linear_fixed(theta)
  list(cj = cj, rubric = data.frame(item_id = names(cj$fit$theta_mean), rubric_score = category),
    theta = theta, x = x, truth = truth, thresholds = thresholds,
    train = rep(seq_len(repeats) %% 2L == 1L, 25L))
}

rubric_oracle_rps <- function(probabilities, category) {
  K <- ncol(probabilities)
  cumulative <- t(apply(probabilities, 1L, cumsum))[, seq_len(K - 1L), drop = FALSE]
  mean((cumulative - outer(category, seq_len(K - 1L), `<=`))^2)
}
