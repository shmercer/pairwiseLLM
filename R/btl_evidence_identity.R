# Fit-time evidence identity for reusable standalone references. Keep repeats;
# row order and incidental table attributes are not statistical differences.
.btl_evidence_identity <- function(results, ids) {
  validate_results_tbl(results)
  ids <- sort(.link_ids(ids, "fitted item IDs"), method = "radix")
  a <- .link_ids(results$A_id, "evidence A_id", unique = FALSE)
  b <- .link_ids(results$B_id, "evidence B_id", unique = FALSE)
  .link_check(all(c(a, b) %in% ids), "Evidence item IDs must belong to the fitted item domain.")
  rows <- tibble::tibble(
    pair_uid = as.character(results$pair_uid), A_item = a, B_item = b,
    y_A = as.integer(results$better_id == results$A_id),
    phase = as.character(results$phase),
    judge_scope = as.character(results[["judge_scope"]] %||% rep(NA_character_, nrow(results))))
  rows$judge_scope[!rows$judge_scope %in% c("shared", "within", "link")] <- NA_character_
  rows <- rows[do.call(order, c(as.list(rows), list(method = "radix", na.last = TRUE))), ]
  payload <- list(format_version = 1L, item_ids = ids, observations = rows)
  c(payload, list(n_observations = nrow(rows), evidence_hash = .link_hash(payload)))
}

.btl_reference_fit_config <- function(fit, config, seed) {
  prior <- fit$theta_prior
  prior <- tibble::tibble(item_id = as.character(prior$item_id),
    prior_mean = as.double(prior$prior_mean), prior_sd = as.double(prior$prior_sd))
  prior <- prior[order(prior$item_id, method = "radix"), ]
  inference <- fit$inference_contract
  for (key in c("phase_levels", "judge_scope_levels")) {
    inference[[key]] <- sort(inference[[key]], method = "radix")
  }
  list(model_variant = fit$model_variant, inference_contract = inference,
    theta_prior = prior, chains = fit$mcmc_config_used$chains,
    iter_warmup = as.integer(config$cmdstan$iter_warmup %||% 1000L),
    iter_sampling = as.integer(config$cmdstan$iter_sampling %||% 1000L), seed = seed)
}
