test_that("bt_run_adaptive_core_linking can use Rank Centrality as running engine and returns final estimates", {
  skip_if_not_installed("BradleyTerry2")

  samples <- tibble::tibble(
    ID = LETTERS[1:6],
    text = paste0("t", LETTERS[1:6])
  )

  # Deterministic ordering: later letters are "better"
  true_theta <- stats::setNames(seq_len(nrow(samples)), samples$ID)

  judge_fun <- function(pairs) {
    b <- ifelse(true_theta[pairs$ID1] >= true_theta[pairs$ID2], pairs$ID1, pairs$ID2)
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = b)
  }

  # Simple mock BT fit to provide SEs for sampling.
  fit_fun <- function(bt_data, ...) {
    bt_data <- tibble::as_tibble(bt_data)
    ids <- sort(unique(c(bt_data$object1, bt_data$object2)))
    wins <- stats::setNames(rep(0L, length(ids)), ids)
    n_j <- stats::setNames(rep(0L, length(ids)), ids)
    for (i in seq_len(nrow(bt_data))) {
      a <- bt_data$object1[[i]]
      b <- bt_data$object2[[i]]
      r <- bt_data$result[[i]]
      if (r == 1) wins[a] <- wins[a] + 1L else wins[b] <- wins[b] + 1L
      n_j[a] <- n_j[a] + 1L
      n_j[b] <- n_j[b] + 1L
    }
    theta <- as.numeric(wins - stats::median(wins))
    se <- 1 / sqrt(pmax(1L, as.integer(n_j)))
    list(
      engine = "mock",
      reliability = 0.95,
      theta = tibble::tibble(ID = names(wins), theta = theta, se = se),
      diagnostics = list(sepG = 3.5)
    )
  }

  out <- bt_run_adaptive_core_linking(
    samples = samples,
    batches = list(c("D", "E", "F")),
    judge_fun = judge_fun,
    core_ids = c("A", "B", "C"),
    seed_pairs = 1,
    fit_fun = fit_fun,
    engine = "mock",
    linking = "never",
    fit_engine_running = "rank_centrality",
    rc_smoothing = 0.5,
    rc_damping = 0.1,
    final_refit = TRUE,
    final_bt_bias_reduction = FALSE,
    round_size = 6,
    init_round_size = 6,
    max_rounds_per_batch = 1,
    forbid_repeats = FALSE,
    rel_se_p90_target = 0.8,
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    rel_se_p90_min_improve = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_
  )

  expect_true(length(out$fits) >= 1)
  expect_identical(out$fits[[1]]$engine_running, "rank_centrality")
  expect_true(all(c("theta_rc", "pi_rc", "theta_bt", "theta_bt_linked") %in% names(out$fits[[1]]$theta)))

  # Final estimates should include both BT and Rank Centrality columns.
  expect_true(nrow(out$estimates) >= 3)
  expect_true(all(c("theta_bt_firth", "se_bt_firth", "rank_bt_firth", "theta_rc", "pi_rc", "rank_rc") %in% names(out$estimates)))
})
