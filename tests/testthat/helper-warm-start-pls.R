warm_pls_fixture <- function(schema = "writing_features_v1") {
  x <- warm_core_features(25L)
  if (schema == "writing_features_v2") {
    withr::local_seed(25904)
    definition <- warm_start_feature_schema(schema)
    for (name in setdiff(definition$feature, names(x))) {
      x[[name]] <- if (definition$type[definition$feature == name] == "integer") {
        sample.int(20L, nrow(x), replace = TRUE)
      } else {
        stats::runif(nrow(x))
      }
    }
    attr(x, "warm_start_schema") <- schema
  }
  theta <- warm_core_theta(x)
  list(x = x, theta = theta, schema = schema,
    plan = make_warm_start_cv_plan(x$item_id, theta, "phase4", seed = 259L))
}

warm_pls_fit <- function(f = warm_pls_fixture(), ...) {
  fit_warm_start_model(f$x$item_id, f$theta, "phase4", features = f$x,
    schema = f$schema, cv_plan = f$plan, engine = "pls", ...)
}
