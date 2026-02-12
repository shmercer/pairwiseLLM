make_valid_pairs_tbl <- function() {
  pairwiseLLM:::.adaptive_empty_pairs_tbl()
}

make_valid_results_tbl <- function() {
  pairwiseLLM:::.adaptive_empty_results_tbl()
}

make_valid_failed_attempts_tbl <- function() {
  pairwiseLLM:::.adaptive_empty_failed_attempts_tbl()
}

make_legacy_schema_state <- function() {
  ids <- c("A", "B")
  z <- stats::setNames(c(0L, 0L), ids)
  structure(
    list(
      schema_version = 1L,
      ids = ids,
      N = 2L,
      texts = stats::setNames(c("a", "b"), ids),
      fit = NULL,
      deg = z,
      pos1 = z,
      pos2 = z,
      imb = z,
      pos_count = z,
      unordered_count = stats::setNames(integer(), character()),
      pair_count = stats::setNames(integer(), character()),
      pair_ordered_count = stats::setNames(integer(), character()),
      ordered_seen = stats::setNames(logical(), character()),
      history_pairs = make_valid_pairs_tbl(),
      history_results = make_valid_results_tbl(),
      failed_attempts = make_valid_failed_attempts_tbl(),
      results_seen = stats::setNames(logical(), character()),
      comparisons_scheduled = 0L,
      comparisons_observed = 0L,
      phase = "phase2",
      iter = 0L,
      budget_max = 10L,
      M1_target = 2L,
      last_check_at = 0L,
      new_since_refit = 0L,
      last_refit_at = 0L,
      posterior = list(U_dup_threshold = 0.2),
      mode = "warm_start",
      repair_attempts = 0L,
      stop_reason = NULL
    ),
    class = "adaptive_state"
  )
}

test_that("pairs/results/failed schemas validate and reject key malformed rows", {
  expect_true("base" %in% pairwiseLLM:::.adaptive_fallback_used_levels())
  expect_true("unknown" %in% pairwiseLLM:::.adaptive_starvation_reason_levels())

  expect_no_error(pairwiseLLM:::validate_pairs_tbl(make_valid_pairs_tbl()))
  expect_no_error(pairwiseLLM:::validate_results_tbl(make_valid_results_tbl()))
  expect_no_error(pairwiseLLM:::validate_failed_attempts_tbl(make_valid_failed_attempts_tbl()))

  with_optional <- tibble::tibble(
    pair_uid = "p1",
    unordered_key = "A:B",
    ordered_key = "A:B",
    A_id = "A",
    B_id = "B",
    A_text = "a",
    B_text = "b",
    phase = "phase2",
    iter = 1L,
    created_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
    backend = "openai",
    utility = 0.5,
    deg_A = 1L
  )
  expect_no_error(pairwiseLLM:::validate_pairs_tbl(with_optional))

  bad_pairs <- tibble::tibble(
    pair_uid = "p1",
    unordered_key = "A:B",
    ordered_key = "A:B",
    A_id = "A",
    B_id = "B",
    A_text = "a",
    B_text = "b",
    phase = "bad_phase",
    iter = 1L,
    created_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  )
  expect_error(pairwiseLLM:::validate_pairs_tbl(bad_pairs), "must be one of: phase1, phase2, phase3")

  bad_results <- tibble::tibble(
    pair_uid = "p1",
    unordered_key = "A:B",
    ordered_key = "A:B",
    A_id = "A",
    B_id = "A",
    better_id = "A",
    winner_pos = 1L,
    phase = "phase2",
    iter = 1L,
    received_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
    backend = "openai",
    model = "gpt"
  )
  expect_error(pairwiseLLM:::validate_results_tbl(bad_results), "must not contain self-pairs")
  bad_results$B_id <- "B"
  bad_results$better_id <- "Z"
  expect_error(pairwiseLLM:::validate_results_tbl(bad_results), "must match `A_id` or `B_id`")
  bad_results$better_id <- "A"
  bad_results$winner_pos <- 2L
  expect_error(pairwiseLLM:::validate_results_tbl(bad_results), "must align with `better_id`")

  bad_failed <- tibble::tibble(
    pair_uid = "p1",
    unordered_key = "A:B",
    ordered_key = "A:B",
    A_id = "A",
    B_id = "B",
    phase = "phase2",
    iter = 1L,
    attempted_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
    backend = "openai",
    model = "gpt",
    error_code = "unknown_code",
    error_detail = "x"
  )
  expect_error(pairwiseLLM:::validate_failed_attempts_tbl(bad_failed), "supported non-empty value")

  withr::local_options(pairwiseLLM.allowed_error_codes = c("custom_error"))
  bad_failed$error_code <- "custom_error"
  expect_no_error(pairwiseLLM:::validate_failed_attempts_tbl(bad_failed))
})

test_that("legacy btl mcmc state validator covers canonical and malformed branches", {
  state <- make_legacy_schema_state()
  expect_no_error(pairwiseLLM:::validate_btl_mcmc_state(state))
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(list()), "adaptive_state object")

  canonical <- pairwiseLLM::adaptive_rank_start(make_test_items(3), seed = 1L)
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(canonical), "not supported by this validator")

  bad_fast <- state
  bad_fast$fast_fit <- list()
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_fast), "no longer supported")

  bad_missing <- state
  bad_missing$fit <- NULL
  bad_missing$schema_version <- "1"
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_missing), "must be present")

  bad_schema <- state
  bad_schema$schema_version <- "1"
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_schema), "length-1 integer")

  bad_ids <- state
  bad_ids$ids <- 1:2
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_ids), "must be character")

  bad_n <- state
  bad_n$N <- 3L
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_n), "must equal length")

  bad_counts <- state
  bad_counts$pair_count <- stats::setNames(-1L, "A:B")
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_counts), "must be non-negative")

  bad_pair_ordered <- state
  bad_pair_ordered$pair_ordered_count <- stats::setNames(-1L, "A:B")
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_pair_ordered), "pair_ordered_count")

  bad_ordered_seen <- state
  bad_ordered_seen$ordered_seen <- 1L
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_ordered_seen), "ordered_seen")

  bad_results_seen <- state
  bad_results_seen$results_seen <- c(TRUE, FALSE)
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_results_seen), "results_seen")

  bad_schedule <- state
  bad_schedule$comparisons_scheduled <- 1L
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_schedule), "must equal nrow\\(history_pairs\\)")

  bad_observed <- state
  bad_observed$comparisons_observed <- 1L
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_observed), "cannot exceed")

  bad_phase <- state
  bad_phase$phase <- "bad_phase"
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_phase), "must be one of: phase1, phase2, phase3")

  bad_mode <- state
  bad_mode$mode <- "invalid_mode"
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_mode), "must be one of")

  bad_posterior <- state
  bad_posterior$posterior <- list()
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_posterior), "U_dup_threshold")

  bad_texts <- state
  bad_texts$texts <- c("a", "b")
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_texts), "named character vector")

  bad_text_names <- state
  bad_text_names$texts <- stats::setNames(c("a", "b"), c("A", "X"))
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_text_names), "names must match")

  bad_deg_eq <- state
  bad_deg_eq$deg <- stats::setNames(c(1L, 0L), state$ids)
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_deg_eq), "must equal `state\\$pos1 \\+ state\\$pos2`")

  bad_imb_eq <- state
  bad_imb_eq$imb <- stats::setNames(c(1L, 1L), state$ids)
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_imb_eq), "must equal `state\\$pos1 - state\\$pos2`")

  bad_unordered_type <- state
  bad_unordered_type$unordered_count <- 1
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_unordered_type), "unordered_count")

  bad_paircount_named <- state
  bad_paircount_named$pair_count <- as.integer(1L)
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_paircount_named), "must be named when non-empty")

  bad_pairkey <- state
  bad_pairkey$pair_count <- stats::setNames(1L, "A:A")
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_pairkey), "cannot include self-pairs")

  bad_pair_ordered_named <- state
  bad_pair_ordered_named$pair_ordered_count <- as.integer(1L)
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_pair_ordered_named), "must be named when non-empty")

  bad_results_seen_names <- state
  bad_results_seen_names$results_seen <- c(TRUE)
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_results_seen_names), "must be named when non-empty")

  bad_sched_len <- state
  bad_sched_len$comparisons_scheduled <- c(0L, 0L)
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_sched_len), "length-1 integer")

  bad_obs_len <- state
  bad_obs_len$comparisons_observed <- c(0L, 0L)
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_obs_len), "length-1 integer")

  bad_obs_rows <- state
  bad_obs_rows$history_pairs <- dplyr::bind_rows(state$history_pairs, make_valid_pairs_tbl()[0, , drop = FALSE])
  bad_obs_rows$comparisons_scheduled <- 1L
  bad_obs_rows$comparisons_observed <- 1L
  bad_obs_rows$history_results <- make_valid_results_tbl()
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_obs_rows), "must equal nrow\\(history_pairs\\)")

  bad_phase_len <- state
  bad_phase_len$phase <- c("phase2", "phase2")
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_phase_len), "length-1 character")

  bad_iter <- state
  bad_iter$iter <- "1"
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_iter), "length-1 integer")

  bad_budget <- state
  bad_budget$budget_max <- "10"
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_budget), "budget_max")

  bad_new_since <- state
  bad_new_since$new_since_refit <- -1L
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_new_since), "must be non-negative")

  bad_last_refit <- state
  bad_last_refit$last_refit_at <- 1L
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_last_refit), "cannot exceed `comparisons_observed`")

  bad_new_calc <- state
  bad_new_calc$comparisons_observed <- 1L
  bad_new_calc$comparisons_scheduled <- 1L
  bad_new_calc$history_pairs <- make_valid_pairs_tbl()[1, , drop = FALSE]
  bad_new_calc$history_results <- make_valid_results_tbl()[1, , drop = FALSE]
  bad_new_calc$new_since_refit <- 0L
  bad_new_calc$last_refit_at <- 0L
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_new_calc), "must be non-missing and match `A_id` or `B_id`")

  bad_stop_reason <- state
  bad_stop_reason$stop_reason <- c("a", "b")
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_stop_reason), "length-1 character value or NULL")
})

test_that("canonical adaptive state validator enforces core fields", {
  state <- pairwiseLLM::adaptive_rank_start(make_test_items(3), seed = 1L)
  expect_no_error(pairwiseLLM:::validate_state(state))

  bad <- state
  bad$meta$schema_version <- ""
  expect_error(pairwiseLLM:::validate_state(bad), "must be a non-empty string")

  bad2 <- state
  bad2$n_items <- as.integer(99L)
  expect_error(pairwiseLLM:::validate_state(bad2), "must match")

  bad3 <- state
  bad3$items <- list(a = 1)
  expect_error(pairwiseLLM:::validate_state(bad3), "must be a data frame")

  bad4 <- state
  bad4$history_pairs <- list()
  expect_error(pairwiseLLM:::validate_state(bad4), "must be a data frame")
})

test_that("legacy btl validator reaches deeper fit/count/phase checks", {
  state <- make_legacy_schema_state()

  bad_fit <- state
  bad_fit$fit <- list(theta_draws = matrix(1, nrow = 1, ncol = 1))
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_fit), "required fields")

  bad_deg_names <- state
  names(bad_deg_names$deg) <- NULL
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_deg_names), "named integer vector over `ids`")

  bad_pair_count_type <- state
  bad_pair_count_type$pair_count <- 1
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_pair_count_type), "pair_count")

  bad_pair_ordered_type <- state
  bad_pair_ordered_type$pair_ordered_count <- 1
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_pair_ordered_type), "pair_ordered_count")

  bad_mode_len <- state
  bad_mode_len$mode <- c("warm_start", "adaptive")
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_mode_len), "length-1 character value")
})

test_that("legacy btl validator covers remaining numeric/count/posterior guards", {
  state <- make_legacy_schema_state()

  bad_sched_neg <- state
  bad_sched_neg$comparisons_scheduled <- -1L
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_sched_neg), "must be non-negative")

  bad_obs_neg <- state
  bad_obs_neg$comparisons_observed <- -1L
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_obs_neg), "must be non-negative")

  bad_obs_rows <- state
  bad_obs_rows$comparisons_observed <- 1L
  bad_obs_rows$comparisons_scheduled <- 1L
  bad_obs_rows$history_pairs <- pairwiseLLM:::.adaptive_empty_pairs_tbl()[0, , drop = FALSE]
  bad_obs_rows$history_results <- pairwiseLLM:::.adaptive_empty_results_tbl()[0, , drop = FALSE]
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_obs_rows), "must equal nrow\\(history_pairs\\)")

  bad_unique_seen <- state
  bad_unique_seen$N <- 3L
  bad_unique_seen$ids <- c("A", "B", "C")
  bad_unique_seen$texts <- stats::setNames(c("a", "b", "c"), c("A", "B", "C"))
  z3 <- stats::setNames(c(0L, 0L, 0L), c("A", "B", "C"))
  bad_unique_seen$deg <- z3
  bad_unique_seen$pos1 <- z3
  bad_unique_seen$pos2 <- z3
  bad_unique_seen$imb <- z3
  bad_unique_seen$pos_count <- z3
  bad_unique_seen$pair_count <- stats::setNames(c(1L, 1L, 1L, 1L), c("A:B", "A:B", "A:C", "B:C"))
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_unique_seen), "more unique pairs than possible")

  bad_last_check <- state
  bad_last_check$last_check_at <- "0"
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_last_check), "last_check_at")

  bad_last_refit_len <- state
  bad_last_refit_len$last_refit_at <- c(0L, 0L)
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_last_refit_len), "length-1 integer")

  bad_new_gt_obs <- state
  bad_new_gt_obs$comparisons_observed <- 0L
  bad_new_gt_obs$new_since_refit <- 1L
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_new_gt_obs), "cannot exceed `comparisons_observed`")

  bad_posterior_type <- state
  bad_posterior_type$posterior <- 1L
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_posterior_type), "`state\\$posterior` must be a list")

  bad_u_dup_type <- state
  bad_u_dup_type$posterior <- list(U_dup_threshold = "bad")
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_u_dup_type), "numeric length 1")

  bad_repair_type <- state
  bad_repair_type$repair_attempts <- "0"
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_repair_type), "length-1 integer")

  bad_repair_neg <- state
  bad_repair_neg$repair_attempts <- -1L
  expect_error(pairwiseLLM:::validate_btl_mcmc_state(bad_repair_neg), "must be non-negative")
})

test_that("validate_state rejects non-adaptive and too-short canonical ids", {
  expect_error(pairwiseLLM:::validate_state(list()), "adaptive_state object")

  state <- pairwiseLLM::adaptive_rank_start(make_test_items(3), seed = 1L)
  bad_ids <- state
  bad_ids$item_ids <- "A"
  bad_ids$n_items <- 1L
  expect_error(pairwiseLLM:::validate_state(bad_ids), "at least two item ids")
})

test_that("validate_state enforces linking identifiers and mode guards", {
  items <- tibble::tibble(
    item_id = c("a", "b", "c", "d"),
    set_id = c(1L, 1L, 2L, 2L),
    global_item_id = c("ga", "gb", "gc", "gd")
  )
  state <- pairwiseLLM::adaptive_rank_start(
    items,
    seed = 1L,
    adaptive_config = list(
      run_mode = "link_multi_spoke",
      hub_id = 1L
    )
  )
  expect_no_error(pairwiseLLM:::validate_state(state))

  bad_global <- state
  bad_global$items$global_item_id[[1]] <- bad_global$items$global_item_id[[2]]
  expect_error(pairwiseLLM:::validate_state(bad_global), "global_item_id")

  bad_link <- state
  bad_link$linking$hub_id <- 99L
  expect_error(pairwiseLLM:::validate_state(bad_link), "must match one observed")

  bad_run_mode <- state
  bad_run_mode$linking$run_mode <- "invalid"
  expect_error(pairwiseLLM:::validate_state(bad_run_mode), "must be within_set, link_one_spoke, or link_multi_spoke")

  bad_items_cols <- state
  bad_items_cols$items <- dplyr::select(bad_items_cols$items, -set_id)
  expect_error(pairwiseLLM:::validate_state(bad_items_cols), "must include columns")

  bad_global_ids <- state
  bad_global_ids$global_item_ids <- "only_one"
  expect_error(pairwiseLLM:::validate_state(bad_global_ids), "one value per item")

  bad_link_list <- state
  bad_link_list$linking <- "oops"
  expect_error(pairwiseLLM:::validate_state(bad_link_list), "must be a list")

  bad_concurrent <- state
  bad_concurrent$controller$multi_spoke_mode <- "concurrent"
  bad_concurrent$controller$hub_lock_mode <- "free"
  expect_error(pairwiseLLM:::validate_state(bad_concurrent), "must be hard_lock or soft_lock")
})
