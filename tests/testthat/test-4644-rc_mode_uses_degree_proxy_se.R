# SPDX-License-Identifier: MIT

test_that("RC mode: pairing uses degree-based proxy SE", {
  samples <- tibble::tibble(
    ID = as.character(1:6),
    text = paste0("sample_", 1:6)
  )

  # Create a small, partially connected graph with known degrees.
  # Unique undirected edges:
  #   1-2, 1-3, 2-3, 4-5
  # Degrees: 1=2, 2=2, 3=2, 4=1, 5=1, 6=0
  initial_results <- tibble::tibble(
    ID1 = c("1", "1", "2", "4"),
    ID2 = c("2", "3", "3", "5"),
    better_id = c("1", "1", "2", "4")
  )

  judge_fun <- function(pairs, ...) {
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  }

  # Minimal BT fit used internally even when running in RC mode.
  ids_local <- samples$ID
  fit_fun <- function(bt_data, ...) {
    list(
      engine = "bt",
      reliability = NA_real_,
      theta = tibble::tibble(ID = ids_local, theta = 0, se = 1),
      diagnostics = list()
    )
  }

  # Deterministic RC fit (values don't matter; just need a valid theta table).
  fit_rc_mock <- function(bt_data, ids = NULL, ...) {
    if (is.null(ids)) {
      ids <- sort(unique(c(as.character(bt_data$object1), as.character(bt_data$object2))))
    }
    list(
      theta = tibble::tibble(ID = ids, theta = seq_along(ids), pi = rep(1 / length(ids), length(ids))),
      reliability = NA_real_,
      diagnostics = list()
    )
  }

  captured <- new.env(parent = emptyenv())
  captured$theta <- NULL

  # Capture the theta table passed to the adaptive selector.
  select_mock <- function(samples, theta, ...) {
    captured$theta <- theta

    t1 <- samples$text[match("1", samples$ID)]
    t2 <- samples$text[match("6", samples$ID)]
    tibble::tibble(ID1 = "1", text1 = t1, ID2 = "6", text2 = t2)
  }

  testthat::local_mocked_bindings(
    select_adaptive_pairs = select_mock,
    fit_rank_centrality = fit_rc_mock,
    .package = "pairwiseLLM"
  )

  out <- bt_run_adaptive(
    samples = samples,
    judge_fun = judge_fun,
    fit_engine_running = "rank_centrality",
    fit_engine_final = "none",
    final_refit = FALSE,
    initial_results = initial_results,
    init_round_size = 0,
    round_size = 1,
    max_rounds = 1,
    min_rounds = 0,
    fit_fun = fit_fun,
    verbose = FALSE
  )

  # Ensure we actually captured a pairing theta table.
  expect_true(is.data.frame(captured$theta))
  expect_true(all(c("ID", "theta", "se", "theta_rc") %in% names(captured$theta)))

  # Expected SE proxy: 1/sqrt(pmax(degree, 1)).
  expected_deg <- c("1" = 2, "2" = 2, "3" = 2, "4" = 1, "5" = 1, "6" = 0)
  expected_se <- 1 / sqrt(pmax(expected_deg, 1))
  se_by_id <- expected_se[match(captured$theta$ID, names(expected_se))]
  se_by_id[is.na(se_by_id)] <- 1

  expect_equal(captured$theta$se, as.double(se_by_id), tolerance = 1e-12)

  # Pairing theta should be the RC theta.
  expect_equal(captured$theta$theta, captured$theta$theta_rc)

  # Smoke-check runner output.
  expect_true(is.data.frame(out$rounds))
})
