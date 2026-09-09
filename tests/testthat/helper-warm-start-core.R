warm_core_features <- function(n = 30L) {
  withr::local_seed(3103)
  fields <- warm_start_feature_schema()$feature
  x <- as.data.frame(matrix(stats::runif(n * length(fields)), nrow = n))
  names(x) <- fields
  x$n_tokens <- seq_len(n) + 10L
  x$token_length_mean <- 2 + 10 * x$token_length_mean
  x$token_length_std <- 0.2 + x$token_length_std
  x$dale_chall_readability_score <- 5 + 20 * x$dale_chall_readability_score
  x <- data.frame(item_id = as.character(seq_len(n)), x, check.names = FALSE)
  attr(x, "warm_start_schema") <- "writing_features_v1"
  x
}

warm_core_theta <- function(x) {
  10 + 0.4 * x$n_tokens - 2 * x$token_length_mean + x$dale_chall_readability_score / 10
}

# Valid portable fixture constructed without glmnet or Python.
warm_core_model <- function() {
  x <- warm_core_features()
  preprocessing <- pairwiseLLM:::.warm_start_preprocess_fit(as.matrix(x[, -1]))
  coefficients <- stats::setNames(seq_along(preprocessing$retained) / 100, preprocessing$retained)
  pairwiseLLM:::.new_warm_start_model("writing_features_v1", preprocessing, coefficients, 0.2,
    pairwiseLLM:::.warm_start_outcome_fit(warm_core_theta(x)),
    list(task_id = "synthetic-assessment", n = nrow(x), alpha = 0.5, lambda = 0.1,
      n_nonzero = length(coefficients), engine = "glmnet", engine_version = "fixture",
      package_version = "1.3.1"))
}
