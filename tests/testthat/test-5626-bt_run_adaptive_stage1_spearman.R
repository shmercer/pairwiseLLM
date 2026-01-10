# Workstream E: stage1 hybrid stability edge cases

# This test covers two rarely-hit branches:
# 1) Spearman rank correlation is NA when ranks are constant; we treat it as 1.
# 2) Stage1 escalation can temporarily allow unordered repeats (forbid_repeats = FALSE).

testthat::test_that("hybrid stage1 handles NA spearman and can relax forbid_repeats", {
  withr::local_seed(123)

  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("a", "b", "c")
  )

  # Deterministic judge: always choose ID1.
  judge_fun <- function(pairs) {
    tibble::tibble(
      ID1 = pairs$ID1,
      ID2 = pairs$ID2,
      better_id = pairs$ID1
    )
  }

  # Deterministic BT fit: equal theta for all IDs.
  fit_fun <- function(bt_data, ...) {
    ids <- sort(unique(c(bt_data$object1, bt_data$object2)))
    theta <- tibble::tibble(
      ID = ids,
      theta = rep(0, length(ids)),
      se = rep(1, length(ids))
    )

    list(
      theta = theta,
      engine = "bradley_terry",
      reliability = NA_real_,
      diagnostics = list()
    )
  }

  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  calls$forbid_repeats <- logical(0)

  # Mock pair selection so we can (a) keep stage1 running and (b) capture args.
  select_mock <- function(samples, ..., forbid_repeats) {
    calls$n <- calls$n + 1L
    calls$forbid_repeats <- c(
      calls$forbid_repeats,
      if (is.null(forbid_repeats)) NA else isTRUE(forbid_repeats)
    )

    # Alternate between two deterministic pairs.
    if (calls$n == 1L) {
      tibble::tibble(ID1 = "A", ID2 = "B")
    } else {
      tibble::tibble(ID1 = "B", ID2 = "C")
    }
  }

  # Mock rank-centrality fits to be constant so Spearman becomes NA and is coerced to 1.
  rc_mock <- function(bt_data, ids, ...) {
    theta <- tibble::tibble(
      ID = ids,
      theta = rep(0, length(ids)),
      pi = rep(1 / length(ids), length(ids))
    )

    list(
      theta = theta,
      engine = "rank_centrality",
      reliability = NA_real_,
      diagnostics = list()
    )
  }

  testthat::local_mocked_bindings(
    select_adaptive_pairs = select_mock,
    fit_rank_centrality = rc_mock,
    .env = asNamespace("pairwiseLLM")
  )

  out <- testthat::expect_warning(
    pairwiseLLM::bt_run_adaptive(
      samples = samples,
      judge_fun = judge_fun,
      fit_fun = fit_fun,
      max_rounds = 2L,
      init_round_size = 1L,
      round_size = 1L,
      fit_engine_running = "hybrid",
      stage1_max_rounds = 1L,
      stage1_k_conn = 99L,
      stage1_k_stab = 99L,
      stage1_min_pct_nodes_with_degree_gt0 = 0,
      stage1_min_largest_component_frac = 0,
      stage1_min_degree_median = 0,
      stage1_min_degree_min = 0,
      stage1_min_degree_min_lcc = 0,
      stage1_min_spearman = 0,
      stage1_escalate_allow_unordered_repeats = TRUE,
      repeat_policy = "reverse_only",
      forbid_repeats = TRUE
    ),
    "`forbid_repeats` is deprecated",
    fixed = TRUE
  )

  # We can't assume internal round-metadata columns, but we *can* assert that
  # stage1 escalation relaxed repeat constraints when requested.
  testthat::expect_equal(length(calls$forbid_repeats), 2L)
  testthat::expect_equal(calls$forbid_repeats[[2]], FALSE)
})
