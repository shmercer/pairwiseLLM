warm_algorithm_fixture <- function(schema = "writing_features_v1", engines = c("glmnet", "pls", "svr_rbf"),
                                     singleton = FALSE) {
  for (engine in engines) testthat::skip_if_not_installed(if (engine == "svr_rbf") "e1071" else engine)
  f <- warm_svr_fixture(schema)
  if (singleton) {
    for (name in setdiff(names(f$x), c("item_id", "n_tokens"))) f$x[[name]] <- f$x[[name]][1]
    f$theta <- warm_core_theta(f$x)
  }
  f$plan <- make_warm_start_cv_plan(f$x$item_id, f$theta, "phase6", seed = 259L)
  components <- lapply(engines, function(engine) {
    args <- list(ids = f$x$item_id, theta = f$theta, task_id = "phase6", features = f$x,
      schema = schema, cv_plan = f$plan, engine = engine)
    if (engine == "glmnet") args$alpha_grid <- c(0, 1)
    if (engine == "pls") args$engine_control <- list(ncomp = if (singleton) 1L else 1:2)
    if (engine == "svr_rbf") args$engine_control <- list(cost = c(0.5, 2), gamma_multiplier = c(0.5, 1))
    do.call(fit_warm_start_model, args)
  })
  f$components <- stats::setNames(components, engines)
  f$ensemble <- do.call(ensemble_warm_start_algorithms, f$components)
  f
}
