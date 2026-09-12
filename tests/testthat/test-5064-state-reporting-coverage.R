test_that("rolling windows discard missing results and retain the newest valid evidence", {
  expect_identical(.adaptive_link_result_window_normalize(NULL, 2L), logical())
  expect_identical(.adaptive_link_result_window_normalize(list(TRUE, NA, FALSE), 0L),
    c(TRUE, FALSE))
  expect_identical(.adaptive_link_result_window_append(c(TRUE, FALSE), TRUE, 2L),
    c(FALSE, TRUE))
  for (bad in list(NA, logical(), c(TRUE, FALSE))) {
    expect_error(.adaptive_link_result_window_append(TRUE, bad, 2L), "single non-missing")
  }
  expect_error(.adaptive_scaled_count(6L, 1L, -1), "frac")
})

test_that("weighted budgets conserve units with stable ties and invalid-weight fallback", {
  allocate <- .adaptive_weighted_largest_remainder
  expect_identical(allocate(0L, c(a = 1, b = 2), c("b", "a"))$add, c(b = 0L, a = 0L))
  expect_identical(allocate(3L, c(a = NA, b = -1), c("b", "a"))$add, c(b = 2L, a = 1L))
  expect_identical(allocate(7L, c(a = 3, b = 1), c("a", "b"))$add, c(a = 5L, b = 2L))
  expect_error(.adaptive_link_compute_stage_targets(-1, list()), "non-negative")
  empty <- .adaptive_link_blocker_weights(tibble::tibble())
  expect_true(all(empty == 0))
  expect_equal(.adaptive_link_blocker_weights_for_spoke(list(), NA_integer_), empty)
  metrics <- tibble::tibble(probe_panel_shortfall = 15, probe_brier = 0.38,
    probe_pred_rmse_lagged = 0.03, theta_global_rmse_lagged = 0.10, delta_spoke_sd = 0.20,
    probe_edges_min_for_stop_used = -1, probe_brier_max_used = NA_real_)
  expect_equal(unname(.adaptive_link_blocker_weights(metrics)), c(0.5, 1, 1, 1, 1))
  empty[] <- NA_real_
  expect_equal(unname(.adaptive_link_blocker_stage_weights(empty)), rep(1, 4))
})

test_that("controller validation reports contradictory thresholds before starting a run", {
  validate <- function(x) .adaptive_validate_controller_config(x, 6L, rep(1:3, each = 2L))
  expect_error(validate(list(1)), "non-empty names")
  cases <- list(
    list(hub_anchor_required_phase_b = NA),
    list(run_mode = "link_multi_spoke", hub_id = 9L),
    list(stability_passes_required = 4L, stability_window_refits = 2L),
    list(probe_near_boundary_low = 0.8, probe_near_boundary_high = 0.2),
    list(probe_extreme_low = 0.8, probe_extreme_high = 0.2),
    list(probe_midrange_low = 0.8, probe_midrange_high = 0.2),
    list(probe_rank_bins = 2L, probe_rank_bins_hub_min = 3L))
  messages <- c("TRUE or FALSE", "hub_id.*match", "stability_passes_required.*<=",
    "probe_near_boundary_low.*less", "probe_extreme_low.*less",
    "probe_midrange_low.*less", "rank-bin minimums")
  for (i in seq_along(cases)) expect_error(validate(cases[[i]]), messages[[i]])
})

test_that("progress reports the first failed gate and exact HMC diagnostics", {
  row <- tibble::tibble(stop_decision = TRUE, diagnostics_pass = TRUE)
  values <- list(diagnostics_pass = TRUE, eap_pass = TRUE, lag_eligible = TRUE,
    theta_pass = TRUE, delta_pass = TRUE, rank_pass = TRUE,
    reliability_label = "reliability", theta_label = "theta", delta_label = "delta",
    rank_label = "rank")
  fields <- c("diagnostics_pass", "eap_pass", "lag_eligible", "theta_pass", "delta_pass", "rank_pass")
  labels <- c("diagnostics_pass", "reliability", "lag_eligible_scope", "theta", "delta", "rank")
  for (i in seq_along(fields)) {
    bad <- values
    bad[[fields[[i]]]] <- FALSE
    expect_identical(.adaptive_progress_phase_a_blocker(row, TRUE, bad), labels[[i]])
  }
  expect_true(is.na(.adaptive_progress_phase_a_blocker(row, TRUE, values)))
  row$stop_decision <- FALSE
  expect_identical(.adaptive_progress_phase_a_blocker(row, FALSE, values), "stop_pending")
  link <- tibble::tibble(spoke_id = 2L, link_fit_method = "hmc",
    link_diagnostics_divergences = 3L, link_diagnostics_divergences_pass = FALSE,
    link_diagnostics_max_rhat = 1.1, link_diagnostics_rhat_pass = FALSE,
    link_diagnostics_min_ess_bulk = 50, link_diagnostics_ess_pass = FALSE)
  lines <- .adaptive_progress_diagnostics_lines(row, link)
  expect_length(lines, 1L)
  expect_match(lines, "spoke=2 link divergences=3 fail.*max_rhat=1.100 fail.*min_ess_bulk=50 fail")
  state <- task09_link_state()
  state$controller$link_budget_refit_id <- .adaptive_link_refit_window_id(state)
  state$controller$link_budget_map <- list(`2` = list(B_spoke_refit_budget = NA_integer_),
    `3` = list(B_spoke_refit_budget = 0L))
  expect_identical(.adaptive_progress_refit_target(state, 7L), 7L)
  state$controller$link_budget_map[["3"]]$B_spoke_refit_budget <- 4L
  expect_identical(.adaptive_progress_refit_target(state, 7L), 4L)
})
test_that("link review exposes stopped and frozen spokes with audit effort metadata", {
  state <- task09_link_state()
  state$controller$link_stopped_by_spoke <- list(`2` = TRUE)
  state$controller$link_state_frozen_by_spoke <- list(`2` = TRUE)
  state$controller$link_epoch_id_by_spoke <- list(`2` = 1L)
  phase <- .adaptive_link_phase_context(state)
  expect_match(.adaptive_print_link_phase_line(state), "stopped_spokes=2")
  expect_match(.adaptive_print_link_phase_line(state), "frozen_spokes=2")
  row <- lapply(schema_link_stage_log, .adaptive_schema_typed_na)
  row$spoke_id <- 2L
  row$refit_id <- 1L
  row$link_estimation_mode <- "anchored_joint"
  row$link_fit_method <- "map_laplace"
  row$link_uncertainty_approximation <- "laplace_hessian"
  row$phase_b_global_metric_uncertainty_approximation <- "marginal_quantile_reconstruction"
  row$probe_only_blocker_trigger <- TRUE
  row$link_state_frozen <- TRUE
  row$probe_active_floor_used <- 2L
  row$probe_acceleration_mode_used <- "fixed_per_refit"
  row$probe_effort_base_cap <- 2L
  row$probe_effort_effective_cap <- 4L
  row$stop_blocker_codes <- "probe_count"
  state$link_stage_log <- tibble::as_tibble(row)
  line <- .adaptive_print_link_state_line(state, phase)
  expect_match(line, "probe_floor=2")
  expect_match(line, "probe_cap=2->4")
  expect_match(line, "probe_only_blocker=1/1")
  expect_match(line, "stopped_spokes=2")
  expect_match(line, "link_state_frozen=1/1")
  state$link_stage_log <- tibble::tibble(refit_id = 1L)
  expect_equal(nrow(.adaptive_latest_link_stage_rows(state)), 0L)
})

test_that("link item summaries preserve ordered hub and accepted-spoke uncertainty", {
  state <- task10_link_state()
  ids <- rev(state$item_ids)
  sets <- state$items$set_id[match(ids, state$item_ids)]
  means <- unname(state$btl_fit$theta_mean[ids])
  sds <- unname(state$btl_fit$theta_sd[ids])
  quantiles <- matrix(0, 5L, length(ids), dimnames = list(NULL, ids))
  f <- function(s, phase_a = FALSE) {
    .adaptive_link_item_raw_link_summaries(
      s, ids, sets, means, sds, quantiles, is_link_phase_a = phase_a)
  }
  out <- f(state)
  expect_identical(colnames(out$theta_link_quantiles), ids)
  expect_equal(out$theta_link_eap[sets == 1L], means[sets == 1L])
  expect_identical(out$theta_link_sd[sets == 1L], c(0, 0))
  for (spoke in 2:3) {
    accepted <- state$linking$anchored_joint$accepted_state_by_spoke[[as.character(spoke)]]
    expect_equal(out$theta_link_eap[sets == spoke],
      unname(accepted$theta_spoke_global_mean[ids[sets == spoke]]))
    expect_equal(out$theta_link_sd[sets == spoke],
      unname(accepted$theta_spoke_global_sd[ids[sets == spoke]]))
  }
  phase_a <- f(state, TRUE)
  expect_identical(phase_a$theta_link_eap, rep(NA_real_, length(ids)))
  expect_identical(phase_a$theta_link_sd, rep(NA_real_, length(ids)))
  expect_true(all(is.na(phase_a$theta_link_quantiles)))
  state$controller$run_mode <- "within_set"
  expect_identical(f(state), list(theta_link_eap = means, theta_link_sd = sds,
    theta_link_quantiles = quantiles))
})
