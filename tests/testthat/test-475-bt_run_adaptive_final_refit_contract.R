testthat::test_that("bt_run_adaptive default final_refit returns schema-valid estimates and theta", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("a", "b", "c")
  )

  # Deterministic judge that enforces A > B > C.
  judge_fun <- function(pairs) {
    testthat::expect_true(all(c("ID1", "ID2") %in% names(pairs)))
    out <- dplyr::mutate(
      pairs,
      better_id = dplyr::case_when(
        ID1 == "A" ~ "A",
        ID2 == "A" ~ "A",
        ID1 == "B" & ID2 == "C" ~ "B",
        ID1 == "C" & ID2 == "B" ~ "B",
        TRUE ~ ID1
      )
    )
    dplyr::select(out, ID1, ID2, better_id)
  }

  out <- pairwiseLLM::bt_run_adaptive(
    samples = samples,
    judge_fun = judge_fun,
    round_size = 3,
    init_round_size = 3,
    max_rounds = 1,
    seed_pairs = 123
  )

  testthat::expect_true(is.list(out))
  testthat::expect_true(!is.null(out$estimates))
  testthat::expect_true(!is.null(out$theta))
  testthat::expect_true(is.character(out$theta_engine) && length(out$theta_engine) == 1L)
  testthat::expect_true(is.list(out$fit_provenance) || is.null(out$fit_provenance))

  est <- out$estimates
  testthat::expect_s3_class(est, "tbl_df")
  testthat::expect_true(all(c(
    "ID",
    "theta_bt_firth", "se_bt_firth", "rank_bt_firth",
    "theta_rc", "rank_rc",
    "bt_engine_requested", "bt_engine_used", "bt_status", "bt_failure_reason"
  ) %in% names(est)))

  # theta is a compact convenience view of the final scale
  th <- out$theta
  testthat::expect_s3_class(th, "tbl_df")
  testthat::expect_true(all(c("ID", "theta", "se", "rank") %in% names(th)))
  testthat::expect_setequal(th$ID, samples$ID)
})


testthat::test_that("bt_run_adaptive records fallback when BT engine is unavailable", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("a", "b", "c")
  )

  judge_fun <- function(pairs) {
    dplyr::mutate(pairs, better_id = ID1) |>
      dplyr::select(ID1, ID2, better_id)
  }

  # Force BT dependencies to appear missing; Rank Centrality should still run.
  testthat::local_mocked_bindings(
    .require_ns = function(pkg, quietly = TRUE) {
      if (identical(pkg, "BradleyTerry2")) {
        return(FALSE)
      }
      if (identical(pkg, "rstanarm")) {
        return(FALSE)
      }
      TRUE
    },
    .package = "pairwiseLLM"
  )

  out <- pairwiseLLM::bt_run_adaptive(
    samples = samples,
    judge_fun = judge_fun,
    round_size = 3,
    init_round_size = 3,
    max_rounds = 1,
    seed_pairs = 1,
    fit_engine_final = "bt_firth"
  )

  testthat::expect_true(!is.null(out$estimates))
  testthat::expect_equal(out$theta_engine, "rank_centrality")

  est <- out$estimates
  testthat::expect_true(all(est$bt_status %in% c("fallback", "skipped", "succeeded")))
  testthat::expect_true(any(est$bt_status %in% c("fallback", "skipped")))
  testthat::expect_true(any(is.na(est$theta_bt_firth)))
})


testthat::test_that("bt_run_adaptive fit_engine_final='none' skips BT and uses Rank Centrality", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("a", "b", "c")
  )

  judge_fun <- function(pairs) {
    dplyr::mutate(pairs, better_id = ID1) |>
      dplyr::select(ID1, ID2, better_id)
  }

  out <- pairwiseLLM::bt_run_adaptive(
    samples = samples,
    judge_fun = judge_fun,
    round_size = 3,
    init_round_size = 3,
    max_rounds = 1,
    seed_pairs = 1,
    fit_engine_final = "none"
  )

  testthat::expect_equal(out$theta_engine, "rank_centrality")
  testthat::expect_true(!is.null(out$estimates))
  testthat::expect_true(all(out$estimates$bt_engine_requested == "none"))
  testthat::expect_true(all(out$estimates$bt_status == "skipped"))
})
