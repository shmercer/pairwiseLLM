# Provider-free downstream smoke. Arguments: package checkout, output JSON.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("Supply package checkout and output JSON paths.")
repo <- normalizePath(args[1], mustWork = TRUE)
output <- normalizePath(args[2], mustWork = FALSE)
pkgload::load_all(repo, quiet = TRUE)
revision <- system2("git", c("-C", shQuote(repo), "rev-parse", "HEAD"), stdout = TRUE)
dirty <- length(system2("git", c("-C", shQuote(repo), "status", "--porcelain"), stdout = TRUE)) > 0L
judge <- list(beta = .2, epsilon = .05, model_variant = "btl_e_b",
  link = "logit", source = "synthetic fixed judge")
identities <- list(hub = list(set_id = "H", items = data.frame(item_id = c("a", "b"))),
  spoke = list(set_id = "S", items = data.frame(item_id = c("a", "b"))))
# Independently integrate tiny Phase A posteriors. The same raw observations
# supply E3, posterior draws supply E2, and their means supply E1.
artifacts <- lapply(c("H", "S"), function(set) {
  y <- if (set == "H") c(rep(1L, 15), rep(0L, 5)) else c(rep(1L, 12), rep(0L, 8))
  rows <- data.frame(observation_id = paste0(set, seq_along(y)),
    A_set = set, A_item = "a", B_set = set, B_item = "b", y_A = y)
  u <- seq(-8, 8, length.out = 16001L)
  p <- judge$epsilon / 2 + (1 - judge$epsilon) * plogis(sqrt(2) * u + judge$beta)
  log_weight <- dnorm(u, log = TRUE) + sum(y) * log(p) + sum(1 - y) * log1p(-p)
  weight <- exp(log_weight - max(log_weight))
  cdf <- cumsum(weight) / sum(weight)
  draws_u <- approx(cdf, u, xout = (seq_len(2000L) - .5) / 2000, ties = "ordered")$y
  draws <- cbind(a = draws_u / sqrt(2), b = -draws_u / sqrt(2))
  list(set_id = set, n_items = 2L, n_pairs_committed = length(y), fit_model_id = "btl_e_b",
    phase_scope = "phase_a_set", phase_scope_set_id = set,
    items = data.frame(item_id = c("a", "b"), theta_raw_mean = colMeans(draws)),
    posterior_draws = draws, phase_a_within_set_evidence = rows)
})
phase_a <- stats::setNames(lapply(artifacts, function(x) list(artifact = x)), c("hub", "spoke"))
cross <- data.frame(observation_id = paste0("cross-", seq_len(16L)),
  A_set = "H", A_item = rep(c("a", "b"), 8), B_set = "S",
  B_item = rep(c("a", "a", "b", "b"), 4), y_A = rep(c(0L, 1L, 0L, 0L), 4))
ids <- c("fixed_shape_offset", "gaussian_posterior_bridge", "joint_offset")
results <- lapply(ids, function(id) {
  input <- prepare_link_input(id, identities$hub, identities$spoke, phase_a, cross, judge,
    provenance = list(source_commit = revision))
  result <- fit_link(input)
  stopifnot(result$diagnostics$fit_valid, identical(result$offset$identification, "cross_set"))
  probabilities <- predict_link(result, cross[, -6])
  stopifnot(length(probabilities) == nrow(cross), all(is.finite(probabilities)),
    all(probabilities > 0 & probabilities < 1))
  session <- start_link_session(input)
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  save_link_session(session, path)
  restored <- load_link_session(path, input)
  stopifnot(identical(session, restored), identical(session, resume_link_session(restored, input)))
  list(estimator_id = id, engine = "primary", required_phase_a = input$phase_a$hub$kind,
    package_version = result$provenance$package_version, source_commit = result$provenance$source_commit,
    result_fields = names(result), item_fields = names(result$items),
    diagnostic_fields = names(result$diagnostics), hashes = result$provenance$hashes,
    counts = result$provenance$counts, offset = result$offset,
    uncertainty_scope = result$diagnostics$uncertainty_scope,
    hash_scheme = result$provenance$hash_scheme, hash_engine_version = result$provenance$hash_engine_version,
    fit_valid = TRUE, save_resume_exact = TRUE, prediction_api = "predict_link(result, pairs)")
})
stopifnot(length(unique(vapply(results, function(x) x$hashes$cross, character(1)))) == 1L)
jsonlite::write_json(list(package = "pairwiseLLM", version = as.character(utils::packageVersion("pairwiseLLM")),
  commit = revision, dirty = dirty, R = R.version.string, provider_free = TRUE,
  common_cross_evidence = TRUE, estimators = results), output, auto_unbox = TRUE, pretty = TRUE, na = "null")
cat("E1, E2 and E3 smoke passed at", revision, "(dirty:", dirty, ")\n")
