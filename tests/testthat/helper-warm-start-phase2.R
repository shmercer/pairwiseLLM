warm_phase2_fixture <- function() {
  x <- warm_core_features(20)
  theta <- warm_core_theta(x)
  plan <- make_warm_start_cv_plan(x$item_id, theta, "phase2", seed = 259L)
  list(x = x, theta = theta, plan = plan)
}

warm_phase2_fit <- function(f = warm_phase2_fixture(), ...) {
  fit_warm_start_model(f$x$item_id, f$theta, "phase2", features = f$x,
    alpha_grid = c(0, 1), cv_plan = f$plan, ...)
}

warm_phase2_rehash <- function(plan) {
  plan$digest <- pairwiseLLM:::.warm_start_prior_hash(unclass(plan)[setdiff(names(plan), "digest")])
  plan
}
