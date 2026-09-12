task09_link_state <- function() {
  items <- tibble::tibble(item_id = letters[1:6], set_id = rep(1:3, each = 2L),
    global_item_id = paste0("g", letters[1:6]))
  state <- pairwiseLLM::adaptive_rank_start(items, seed = 91L,
    adaptive_config = list(run_mode = "link_multi_spoke", hub_id = 1L),
    now_fn = function() as.POSIXct("2026-09-11", tz = "UTC"))
  means <- stats::setNames(c(-1, 1, -0.5, 0.5, -0.2, 0.2), state$item_ids)
  draws <- outer(c(-0.3, -0.1, 0.1, 0.3), means, `+`)
  colnames(draws) <- state$item_ids
  state$btl_fit <- make_test_btl_fit(state$item_ids, draws = draws, model_variant = "btl_e_b")
  artifacts <- lapply(1:3, function(k) {
    ids <- state$item_ids[items$set_id == k]
    list(set_id = k, posterior_draws = draws[, ids, drop = FALSE],
      items = tibble::tibble(item_id = ids, global_item_id = paste0("g", ids),
        theta_raw_mean = unname(means[ids]), theta_raw_sd = rep(0.2, 2L)))
  })
  names(artifacts) <- as.character(1:3)
  artifacts <- lapply(artifacts, function(artifact) {
    add_test_phase_a_evidence(artifact, state, artifact$set_id)
  })
  state$linking$phase_a$artifacts <- artifacts
  state$linking$phase_a$set_status$status[] <- "ready"
  state$linking$phase_a$required_sets <- 1:3
  state$linking$phase_a$ready_for_phase_b <- TRUE
  state$linking$phase_a$strict_ready_for_phase_b <- TRUE
  state$linking$phase_a$ready_spokes <- 2:3
  state$linking$phase_a$active_spokes <- 2:3
  state$linking$phase_a$phase <- "phase_b"
  state$linking$phase_a$active_phase_a_set <- NA_integer_
  state$warm_start_done <- TRUE
  state
}

# Pass this only to retained internal transform contracts. Fresh public runs stay
# anchored-joint; no controller-normalization or selection helper is mocked.
task09_transform_controller <- function(state, mode = "shift_only") {
  controller <- state$controller
  controller$link_estimation_mode <- "transform"
  controller$link_transform_policy <- paste0("fixed_", mode)
  controller$link_refit_stats_by_spoke <- list(
    `2` = list(link_transform_state = mode, delta_spoke_mean = 0.3,
      log_alpha_spoke_mean = log(2)),
    `3` = list(link_transform_state = mode, delta_spoke_mean = -0.1,
      log_alpha_spoke_mean = log(0.5)))
  controller
}
