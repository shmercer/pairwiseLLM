test_that("bt_run_adaptive rejects invalid Rank Centrality tuning parameters", {
  samples <- tibble::tibble(ID = LETTERS[1:4], text = paste0("t", 1:4))

  judge_fun <- function(pairs) {
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  }

  fit_fun <- function(bt_data, ...) {
    bt_data <- tibble::as_tibble(bt_data)
    ids <- sort(unique(c(bt_data$object1, bt_data$object2)))
    tibble_theta <- tibble::tibble(ID = ids, theta = rep(0, length(ids)), se = rep(1, length(ids)))
    list(engine = "mock", reliability = 0.9, theta = tibble_theta, diagnostics = list(sepG = 3.5))
  }

  withr::local_seed(1)
  expect_error(
    pairwiseLLM::bt_run_adaptive(
      samples = samples,
      judge_fun = judge_fun,
      fit_fun = fit_fun,
      round_size = 2,
      max_rounds = 1,
      min_rounds = 1,
      seed_pairs = 1L,
      rc_smoothing = -0.1
    ),
    "rc_smoothing"
  )

  expect_error(
    pairwiseLLM::bt_run_adaptive(
      samples = samples,
      judge_fun = judge_fun,
      fit_fun = fit_fun,
      round_size = 2,
      max_rounds = 1,
      min_rounds = 1,
      seed_pairs = 1L,
      rc_damping = 1.1
    ),
    "rc_damping"
  )
})


test_that("bt_run_adaptive backfills theta from running fits when final refit is disabled", {
  samples <- tibble::tibble(ID = LETTERS[1:4], text = paste0("t", 1:4))

  judge_fun <- function(pairs) {
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  }

  # Simple BT-ish fitter: theta = win counts; se present.
  fit_fun <- function(bt_data, ...) {
    bt_data <- tibble::as_tibble(bt_data)
    ids <- sort(unique(c(bt_data$object1, bt_data$object2)))
    wins <- stats::setNames(rep(0L, length(ids)), ids)
    for (i in seq_len(nrow(bt_data))) {
      a <- bt_data$object1[[i]]
      b <- bt_data$object2[[i]]
      r <- bt_data$result[[i]]
      if (isTRUE(is.finite(r))) {
        if (r == 1) wins[a] <- wins[a] + 1L else wins[b] <- wins[b] + 1L
      }
    }
    theta <- as.numeric(wins - stats::median(wins))
    list(
      engine = "mock",
      reliability = 0.95,
      theta = tibble::tibble(ID = names(wins), theta = theta, se = rep(1, length(theta))),
      diagnostics = list(sepG = 3.5)
    )
  }

  withr::local_seed(1)
  out <- pairwiseLLM::bt_run_adaptive(
    samples = samples,
    judge_fun = judge_fun,
    fit_fun = fit_fun,
    engine = "mock",
    fit_engine_running = "bt",
    fit_engine_final = "none",
    final_refit = FALSE,
    final_bt_bias_reduction = FALSE,
    round_size = 3,
    max_rounds = 1,
    min_rounds = 1,
    seed_pairs = 1L
  )

  expect_true(is.data.frame(out$theta))
  expect_true(all(c("ID", "theta") %in% names(out$theta)))
  expect_true(is.list(out$fit_provenance))
  expect_true(!is.null(out$fit_provenance$fallback_used) && isTRUE(out$fit_provenance$fallback_used))
  expect_true(is.character(out$theta_engine) || is.na(out$theta_engine))

  # Should remain contract-valid.
  expect_silent(pairwiseLLM:::validate_pairwise_run_output(out))
})
