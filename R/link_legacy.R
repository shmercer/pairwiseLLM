# Recognition only: never normalize a legacy posterior into a supported estimator.
.link_reject_legacy <- function(state) {
  if (!is.list(state)) return(invisible(NULL))
  get <- function(x, key) if (is.list(x)) x[[key]] else NULL
  controller <- get(state, "controller")
  linking <- get(state, "linking")
  config <- get(get(state, "config"), "adaptive_config")
  modes <- c(get(controller, "link_estimation_mode"), get(config, "link_estimation_mode"),
    get(linking, "link_estimation_mode"))
  stage <- get(state, "link_stage_log")
  steps <- get(state, "step_log")
  log_modes <- c(get(stage, "link_estimation_mode"), get(steps, "link_estimation_mode"))
  legacy <- get(linking, "anchored_joint")
  accepted <- get(legacy, "accepted_state_by_spoke")
  initialized <- is.list(accepted) && length(accepted) > 0L
  phase_b <- identical(get(get(linking, "phase_a"), "phase"), "phase_b") ||
    NROW(stage) > 0L || initialized ||
    any(get(steps, "is_cross_set") %in% TRUE &
      get(steps, "run_mode") %in% c("link_one_spoke", "link_multi_spoke", "link_probe_holdout"))
  missing_identity <- !any(!is.na(modes) & nzchar(modes)) && is.null(get(linking, "estimator")) &&
    (get(controller, "run_mode") %||% "within_set") %in% c("link_one_spoke", "link_multi_spoke")
  legacy_mode <- any(c(modes, log_modes) %in% "anchored_joint") || !is.null(legacy) ||
    "anchored_joint_init_state_method" %in% names(stage) ||
    isTRUE(missing_identity)
  if (legacy_mode && phase_b) {
    rlang::abort(paste0("Unsupported legacy anchored-joint Phase B session. Restart linking from ",
      "compatible Phase A artifacts/evidence with an explicit E1, E2, or E3 estimator; ",
      "the Phase B posterior cannot be migrated."), class = "pairwiseLLM_unsupported_legacy_link_state")
  }
  invisible(NULL)
}
