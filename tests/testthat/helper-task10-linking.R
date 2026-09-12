task10_link_state <- function(n_sets = 3L) {
  ids <- letters[seq_len(2L * n_sets)]
  items <- tibble::tibble(item_id = ids, set_id = rep(seq_len(n_sets), each = 2L),
    global_item_id = paste0("g", ids))
  now_fn <- function() as.POSIXct("2026-09-11", tz = "UTC")
  environment(now_fn) <- baseenv()
  state <- adaptive_rank_start(items, seed = 91L,
    adaptive_config = list(run_mode = if (n_sets == 2L) "link_one_spoke" else "link_multi_spoke",
      hub_id = 1L), now_fn = now_fn)
  means <- stats::setNames(c(-1, 1, -0.5, 0.5, -0.2, 0.2)[seq_along(ids)], ids)
  draws <- outer(c(-0.3, -0.1, 0.1, 0.3), means, `+`)
  colnames(draws) <- ids
  state$btl_fit <- make_test_btl_fit(ids, draws = draws, model_variant = "btl_e_b")
  artifacts <- lapply(seq_len(n_sets), function(k) {
    set_ids <- ids[items$set_id == k]
    art <- list(set_id = k, posterior_draws = draws[, set_ids, drop = FALSE],
      items = tibble::tibble(item_id = set_ids, global_item_id = paste0("g", set_ids),
        theta_raw_mean = unname(means[set_ids]), theta_raw_sd = rep(0.2, 2L)))
    add_test_phase_a_evidence(art, state, k)
  })
  names(artifacts) <- as.character(seq_len(n_sets))
  state$linking$phase_a$artifacts <- artifacts
  state$linking$phase_a$set_status$status[] <- "ready"
  state$linking$phase_a$required_sets <- seq_len(n_sets)
  state$linking$phase_a$ready_for_phase_b <- TRUE
  state$linking$phase_a$strict_ready_for_phase_b <- TRUE
  state$linking$phase_a$ready_spokes <- seq.int(2L, n_sets)
  state$linking$phase_a$active_spokes <- seq.int(2L, n_sets)
  state$linking$phase_a$phase <- "phase_b"
  state$linking$phase_a$active_phase_a_set <- NA_integer_
  state$warm_start_done <- TRUE
  .adaptive_anchored_joint_sync_scaffolding(state)
}
