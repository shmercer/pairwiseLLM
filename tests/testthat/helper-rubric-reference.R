# Synthetic sampler output for interface tests, not MCMC accuracy evidence.
# Tests mock only the sampler boundary; fitting records real evidence identity.
rubric_reference_sampler <- function(bt_data, config, seed = NULL) {
  ids <- bt_data$item_id
  index <- match(ids, sort(ids))
  theta <- seq(-2, 2, length.out = length(ids))[index]
  draws <- outer(seq_len(120L), index, function(i, j) 0.3 * sin(i * j))
  draws <- sweep(draws, 2L, colMeans(draws), "-")
  draws <- sweep(draws, 2L, theta, "+")
  colnames(draws) <- ids
  variant <- config$model_variant
  list(draws = list(theta = draws,
    epsilon = if (pairwiseLLM:::model_has_e(variant)) rep(.04, 120L) else NULL,
    beta = if (pairwiseLLM:::model_has_b(variant)) rep(.1, 120L) else NULL),
    model_variant = variant,
    diagnostics = list(divergences = 0L, max_rhat = 1, min_ess_bulk = 1000),
    mcmc_config_used = list(chains = 2L))
}

rubric_reference_evidence <- function(n = 27L) {
  ids <- sprintf("r%02d", seq_len(n))
  a <- rep(ids[-n], each = 6L)
  b <- rep(ids[-1L], each = 6L)
  pairwiseLLM::build_btl_results_data(data.frame(ID1 = a, ID2 = b,
    better_id = ifelse(rep(c(TRUE, FALSE), length.out = length(a)), a, b)))
}

rubric_reference_completed <- function(evidence = rubric_reference_evidence(), variant = "btl", ...) {
  ids <- sort(unique(c(evidence$A_id, evidence$B_id)))
  pairwiseLLM::fit_bayes_btl_mcmc(evidence, ids, model_variant = variant,
    cmdstan = list(chains = 2L, parallel_chains = 1L), ...)
}

rubric_reference_prepare <- function(cj, evidence = rubric_reference_evidence(), ...) {
  pairwiseLLM::prepare_linked_rubric_reference(cj, evidence, "H", trait = "organization", ...)
}

rubric_reference_labels <- function(reference) {
  theta <- reference$items$theta
  u <- rep(c(.15, .5, .85), length.out = length(theta))
  category <- 1L + rowSums(plogis(outer(theta, c(-1, 1), function(x, cut) cut - x)) < u)
  data.frame(item_id = reference$items$item_id, rubric_score = category)
}

rubric_reference_link_args <- function(reference, estimator) {
  spoke <- list(set_id = "S", items = data.frame(item_id = c("s1", "s2", "s3"),
    global_item_id = c("gs1", "gs2", "gs3")))
  ids <- reference$hub$items$item_id
  cross <- data.frame(observation_id = paste0("cross", 1:12), A_set = reference$set_id,
    A_item = rep(ids[c(1L, length(ids))], 6L), B_set = "S",
    B_item = rep(spoke$items$item_id, each = 4L), y_A = rep(c(0L, 1L), 6L))
  kind <- switch(estimator, fixed_shape_offset = "points", gaussian_posterior_bridge = "draws",
    joint_offset = "observations")
  hub <- switch(kind, points = reference$points, draws = reference$posterior_draws,
    observations = reference$evidence)
  target <- switch(kind, points = c(s1 = -1, s2 = 0, s3 = 1),
    draws = sweep(outer(seq_len(120L), 1:3, function(i, j) .2 * sin(i * j)), 2L, c(-1, 0, 1), "+"),
    observations = data.frame(observation_id = paste0("spoke", 1:12), A_set = "S",
      A_item = rep(c("s1", "s2"), 6L), B_set = "S", B_item = rep(c("s2", "s3"), 6L),
      y_A = rep(c(0L, 0L, 1L), 4L)))
  if (kind == "draws") colnames(target) <- spoke$items$item_id
  list(estimator = estimator, hub = reference$hub, spoke = spoke,
    phase_a = list(hub = c(stats::setNames(list(hub), kind), list(source = reference$source)),
      spoke = stats::setNames(list(target), kind)), cross = cross, judge = reference$judge)
}
