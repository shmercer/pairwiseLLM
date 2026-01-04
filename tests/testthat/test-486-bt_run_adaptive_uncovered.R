test_that("486-01 bt_run_adaptive can resume from a completed checkpoint", {
  chk_dir <- tempfile("chk_")
  dir.create(chk_dir)

  payload <- list(
    run_type = "adaptive",
    completed = TRUE,
    stop_reason = "max_rounds",
    random_seed = 123L,
    results = tibble::tibble(),
    fits = list(),
    rounds = tibble::tibble(),
    metrics = tibble::tibble(round = 1L, n_results = 0L, stopped = TRUE, stop_reason = "max_rounds")
  )

  pairwiseLLM:::.bt_write_checkpoint(checkpoint_dir = chk_dir, payload = payload)

  samples <- tibble::tibble(ID = c("a", "b"), text = c("A", "B"))

  out <- pairwiseLLM::bt_run_adaptive(
    samples = samples,
    judge_fun = function(pairs, ...) tibble::tibble(),
    resume_from = chk_dir
  )

  expect_equal(out$stop_reason, "max_rounds")
})

test_that("486-02 bt_run_adaptive falls back to the last running estimate when final_refit = FALSE", {
  samples <- tibble::tibble(
    ID = c("a", "b", "c"),
    text = c("A", "B", "C")
  )

  judge_fun <- function(pairs, ...) {
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  }

  fit_fun <- function(bt_data, engine, ...) {
    ids <- sort(unique(c(bt_data$object1, bt_data$object2)))

    wins <- tibble::tibble(ID = ids) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        wins = sum(bt_data$object1 == ID & bt_data$result == 1) +
          sum(bt_data$object2 == ID & bt_data$result == 0)
      ) |>
      dplyr::ungroup()

    theta <- wins |>
      dplyr::transmute(ID, theta = wins, se = 0.1)

    list(engine = "mock", theta = theta, reliability = NA_real_, diagnostics = list())
  }

  out <- pairwiseLLM::bt_run_adaptive(
    samples = samples,
    judge_fun = judge_fun,
    fit_fun = fit_fun,
    init_round_size = 1L,
    round_size = 1L,
    min_rounds = 0L,
    max_rounds = 1L,
    final_refit = FALSE,
    fit_engine_running = "rank_centrality",
    verbose = FALSE
  )

  expect_true(is.data.frame(out$theta))
  expect_equal(out$theta_engine, "rank_centrality")
  expect_true(all(out$theta$ID %in% samples$ID))
})
