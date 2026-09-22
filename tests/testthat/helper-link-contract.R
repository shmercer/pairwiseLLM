# Synthetic explicit-evidence fixtures; no adaptive state or provider dependencies.
link_contract_args <- function(estimator = "fixed_shape_offset", edges = 0L) {
  hub <- list(set_id = "H", items = data.frame(item_id = c("b", "a")))
  spoke <- list(set_id = "S", items = data.frame(item_id = c("b", "a")))
  evidence <- function(set, id) data.frame(observation_id = id, A_set = set,
    A_item = "a", B_set = set, B_item = "b", y_A = 1L)
  phase_a <- switch(estimator,
    fixed_shape_offset = list(hub = list(points = c(b = 3, a = 1)), spoke = list(points = c(b = 1, a = 0))),
    gaussian_posterior_bridge = list(
      hub = list(draws = matrix(c(-1, 1, -2, 2, -3, 3, -4, 4), 4, byrow = TRUE, dimnames = list(NULL, c("a", "b")))),
      spoke = list(draws = matrix(c(-.5, .5, -1, 1, -1.5, 1.5, -2, 2), 4, byrow = TRUE, dimnames = list(NULL, c("a", "b"))))),
    joint_offset = list(hub = list(observations = evidence("H", "within-h")),
      spoke = list(observations = evidence("S", "within-s"))))
  cross <- data.frame(observation_id = if (edges) paste0("cross-", seq_len(edges)) else character(),
    A_set = rep("H", edges), A_item = rep("a", edges), B_set = rep("S", edges),
    B_item = rep("b", edges), y_A = rep(1L, edges))
  list(estimator = estimator, hub = hub, spoke = spoke, phase_a = phase_a, cross = cross,
    judge = list(beta = .2, epsilon = .1, model_variant = "btl_e_b", link = "logit", source = "synthetic pooled Phase A"))
}

link_contract_input <- function(estimator = "fixed_shape_offset", edges = 0L) {
  do.call(pairwiseLLM::prepare_link_input, link_contract_args(estimator, edges))
}

link_contract_result <- function(input = link_contract_input(), valid = TRUE, covariance = TRUE) {
  prior <- input$control$delta_prior
  interval <- stats::qnorm(c(.025, .975), prior$mean, prior$sd)
  means <- switch(input$estimator,
    fixed_shape_offset = c(input$phase_a$hub$value, input$phase_a$spoke$value + prior$mean),
    gaussian_posterior_bridge = c(colMeans(input$phase_a$hub$value), colMeans(input$phase_a$spoke$value) + prior$mean),
    joint_offset = c(-1, 1, -.5, .5 + 2 * prior$mean))
  coords <- colnames(input$item_transform)
  cov <- diag(length(coords))
  cov[1L, 1L] <- prior$sd^2
  if (input$estimator == "gaussian_posterior_bridge") {
    cov[2L, 2L] <- stats::var(pairwiseLLM:::.link_to_reduced(input$phase_a$hub$value, input$basis$hub)[, 1L])
    cov[3L, 3L] <- stats::var(pairwiseLLM:::.link_to_reduced(input$phase_a$spoke$value, input$basis$spoke)[, 1L])
  }
  dimnames(cov) <- list(coords, coords)
  sd <- sqrt(diag(input$item_transform %*% cov %*% t(input$item_transform)))
  if (!valid) means[] <- NA_real_
  pairwiseLLM:::.link_new_result(input, unname(means),
    delta = list(mean = if (valid) prior$mean else NA_real_, sd = if (valid) prior$sd else NA_real_,
      lower = if (valid) interval[1L] else NA_real_, upper = if (valid) interval[2L] else NA_real_,
      identification = if (!valid) "failed" else if (nrow(input$cross)) "cross_set" else "prior_only"),
    theta_sd = if (covariance && valid) sd else NULL,
    covariance = if (covariance && valid) cov else NULL,
    prediction = list(theta = unname(means), optional = NULL),
    diagnostics = list(fit_attempted = nrow(input$cross) > 0L, fit_valid = valid,
      covariance_valid = if (covariance && valid) TRUE else NA,
      failure_code = if (valid) NA_character_ else "synthetic_failure",
      uncertainty_scope = if (input$estimator == "fixed_shape_offset") "offset_only_conditional_on_fixed_shapes" else "joint_shapes_and_offset"),
    mode = stats::setNames(rep(0, length(coords)), coords))
}
