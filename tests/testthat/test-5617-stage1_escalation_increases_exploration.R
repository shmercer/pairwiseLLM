# Workstream E: Stage 1 max-rounds fail-safe escalation

test_that("stage1 escalation increases exploration fraction and widens neighbor window", {
  withr::local_seed(1)

  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("tA", "tB", "tC", "tD")
  )

  judge_fun <- function(pairs) {
    tibble::tibble(
      ID1 = pairs$ID1,
      ID2 = pairs$ID2,
      better_id = pairs$ID1
    )
  }

  fit_fun <- function(bt_data, ...) {
    ids <- sort(unique(c(bt_data$object1, bt_data$object2)))
    list(
      engine = "mock",
      theta = tibble::tibble(ID = ids, theta = seq_along(ids), se = rep(0.1, length(ids))),
      reliability = 0.99,
      diagnostics = list(sepG = 10)
    )
  }

  capt <- new.env(parent = emptyenv())
  capt$calls <- list()

  testthat::local_mocked_bindings(
    select_adaptive_pairs = function(samples, n_pairs, explore_frac, k_neighbors, ...) {
      capt$calls[[length(capt$calls) + 1L]] <- list(
        explore_frac = explore_frac,
        k_neighbors = k_neighbors
      )

      tibble::tibble(
        ID1 = rep(samples$ID[1], n_pairs),
        text1 = rep(samples$text[1], n_pairs),
        ID2 = rep(samples$ID[2], n_pairs),
        text2 = rep(samples$text[2], n_pairs)
      )
    },
    .package = "pairwiseLLM"
  )

  out <- bt_run_adaptive(
    samples = samples,
    judge_fun = judge_fun,
    fit_fun = fit_fun,
    engine = "mock",
    fit_engine_running = "hybrid",
    init_round_size = 1,
    round_size = 1,
    min_rounds = 1,
    max_rounds = 2,
    seed_pairs = 1,
    # Prevent switching so escalation must activate on round 2.
    stage1_min_degree_median = 999,
    stage1_min_degree_min_lcc = 999,
    stage1_min_spearman = -1,
    stage1_k_conn = 1,
    stage1_k_stab = 1,
    stage1_explore_frac = 0.1,
    stage1_escalated_explore_frac = 0.5,
    k_neighbors = 1,
    stage1_escalated_k_neighbors = Inf,
    final_refit = FALSE
  )

  expect_equal(length(capt$calls), 2L)
  expect_equal(capt$calls[[1]]$explore_frac, 0.1)
  expect_equal(capt$calls[[2]]$explore_frac, 0.5)
  expect_equal(capt$calls[[1]]$k_neighbors, 1)
  expect_true(is.infinite(capt$calls[[2]]$k_neighbors))

  expect_true("stage1_escalated" %in% names(out$rounds))
  expect_true("stage1_escalation_round" %in% names(out$rounds))
  expect_true(any(out$rounds$stage1_escalated))
  expect_equal(max(out$rounds$stage1_escalation_round, na.rm = TRUE), 2L)
})
