#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

parse_args <- function(args) {
  out <- list()
  if (length(args) == 0L) {
    return(out)
  }
  for (arg in args) {
    if (!grepl("^--", arg)) {
      next
    }
    parts <- strsplit(sub("^--", "", arg), "=", fixed = TRUE)[[1L]]
    key <- parts[[1L]]
    value <- if (length(parts) >= 2L) paste(parts[-1L], collapse = "=") else "TRUE"
    out[[key]] <- value
  }
  out
}

as_int <- function(x, default) {
  if (is.null(x)) {
    return(as.integer(default))
  }
  as.integer(x)
}

as_num <- function(x, default) {
  if (is.null(x)) {
    return(as.double(default))
  }
  as.double(x)
}

as_chr <- function(x, default) {
  if (is.null(x) || is.na(x) || !nzchar(x)) {
    return(as.character(default))
  }
  as.character(x)
}

cli <- parse_args(args)
output_dir <- as_chr(cli$output_dir, tempfile("adaptive_link_calibration_"))

res <- pairwiseLLM:::.adaptive_linking_calibrate_offline(
  replicates = as_int(cli$replicates, 10L),
  seed = as_int(cli$seed, 1L),
  set_sizes = c(as_int(cli$hub_size, 8L), as_int(cli$spoke_size, 8L)),
  true_delta = as_num(cli$true_delta, -0.5),
  true_alpha = as_num(cli$true_alpha, 1.0),
  judge_b = as_num(cli$judge_b, 0.0),
  judge_eps = as_num(cli$judge_eps, 0.05),
  n_steps = as_int(cli$n_steps, 120L),
  btl_config = list(
    refit_pairs_target = as_int(cli$refit_pairs_target, 1L),
    stability_lag = as_int(cli$stability_lag, 1L),
    eap_reliability_min = as_num(cli$eap_reliability_min, 0.0),
    theta_corr_min = as_num(cli$theta_corr_min, 0.0),
    rank_spearman_min = as_num(cli$rank_spearman_min, 0.0)
  ),
  adaptive_config = list(
    probe_pairs_per_refit_per_spoke = as_int(cli$probe_pairs_per_refit_per_spoke, 2L),
    link_transform_escalation_refits_required = as_int(cli$link_transform_escalation_refits_required, 2L),
    min_cross_set_pairs_per_spoke_per_refit = as_int(cli$min_cross_set_pairs_per_spoke_per_refit, 5L)
  ),
  output_dir = output_dir,
  progress = "none"
)

cat("Calibration complete\n")
cat("ppc_calibration_id:", as.character(res$sidecar$ppc_calibration_id), "\n")
cat("cross_set_ppc_brier_max:", format(as.double(res$sidecar$cross_set_ppc_brier_max), digits = 6), "\n")
cat("Artifacts:\n")
cat("  summary_csv:", as.character(res$files$summary_csv), "\n")
cat("  replicates_csv:", as.character(res$files$replicates_csv), "\n")
cat("  sidecar_json:", as.character(res$files$sidecar_json), "\n")
