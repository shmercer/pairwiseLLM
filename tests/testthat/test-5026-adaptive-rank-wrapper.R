make_test_samples_df <- function(n = 6L) {
  tibble::tibble(
    ID = sprintf("S%02d", seq_len(n)),
    text = paste("sample", seq_len(n)),
    quality_score = seq_len(n)
  )
}

test_that("make_adaptive_judge_llm forwards model options and returns valid contract", {
  calls <- list()

  judge <- pairwiseLLM::make_adaptive_judge_llm(
    backend = "openai",
    model = "gpt-5.1",
    endpoint = "responses",
    judge_args = list(service_tier = "flex")
  )

  A <- tibble::tibble(item_id = "S01", text = "A")
  B <- tibble::tibble(item_id = "S02", text = "B")

  testthat::with_mocked_bindings(
    llm_compare_pair = function(...) {
      calls <<- append(calls, list(list(...)))
      tibble::tibble(better_id = "S02")
    },
    {
      out <- judge(A, B, state = list(), reasoning = "low")
      expect_true(isTRUE(out$is_valid))
      expect_identical(out$Y, 0L)
      expect_true(is.na(out$invalid_reason))
    },
    .env = asNamespace("pairwiseLLM")
  )

  expect_length(calls, 1L)
  call <- calls[[1L]]
  expect_identical(call$model, "gpt-5.1")
  expect_identical(call$service_tier, "flex")
  expect_identical(call$reasoning, "low")
})

test_that("make_adaptive_judge_llm returns invalid when response cannot be mapped", {
  judge <- pairwiseLLM::make_adaptive_judge_llm(
    backend = "openai",
    model = "gpt-5.1"
  )

  A <- tibble::tibble(item_id = "S01", text = "A")
  B <- tibble::tibble(item_id = "S02", text = "B")

  testthat::with_mocked_bindings(
    llm_compare_pair = function(...) tibble::tibble(better_id = NA_character_),
    {
      out <- judge(A, B, state = list())
      expect_false(isTRUE(out$is_valid))
      expect_identical(out$invalid_reason, "invalid_response")
    },
    .env = asNamespace("pairwiseLLM")
  )
})

test_that("make_adaptive_judge_llm allows custom traits without built-in trait key", {
  calls <- list()
  judge <- pairwiseLLM::make_adaptive_judge_llm(
    backend = "openai",
    model = "gpt-5.1",
    trait = "any-custom-key",
    trait_name = "Voice",
    trait_description = "Strength of writing voice."
  )

  A <- tibble::tibble(item_id = "S01", text = "A")
  B <- tibble::tibble(item_id = "S02", text = "B")

  testthat::with_mocked_bindings(
    llm_compare_pair = function(...) {
      calls <<- append(calls, list(list(...)))
      tibble::tibble(better_id = "S01")
    },
    {
      out <- judge(A, B, state = list())
      expect_true(isTRUE(out$is_valid))
      expect_identical(out$Y, 1L)
    },
    .env = asNamespace("pairwiseLLM")
  )

  expect_identical(calls[[1L]]$trait_name, "Voice")
  expect_identical(calls[[1L]]$trait_description, "Strength of writing voice.")
})

test_that("endpoint validation is only enforced for openai backend", {
  expect_error(
    pairwiseLLM::make_adaptive_judge_llm(
      backend = "openai",
      model = "gpt-5.1",
      endpoint = "bad-endpoint"
    )
  )

  expect_no_error(
    pairwiseLLM::make_adaptive_judge_llm(
      backend = "anthropic",
      model = "claude",
      endpoint = "bad-endpoint"
    )
  )
})

test_that("adaptive_rank runs end-to-end with user-supplied judge", {
  samples <- make_test_samples_df(6L)
  judge <- function(A, B, state, ...) {
    y <- as.integer(A$quality_score[[1L]] >= B$quality_score[[1L]])
    list(is_valid = TRUE, Y = y, invalid_reason = NA_character_)
  }

  out <- pairwiseLLM::adaptive_rank(
    data = samples,
    id_col = "ID",
    text_col = "text",
    judge = judge,
    n_steps = 4L,
    progress = "none",
    seed = 7L
  )

  expect_true(is.list(out))
  expect_true(inherits(out$state, "adaptive_state"))
  expect_equal(nrow(out$state$step_log), 4L)
  expect_s3_class(out$summary, "tbl_df")
  expect_true(all(c("step_log", "round_log", "item_log") %in% names(out$logs)))
})

test_that("adaptive_rank supports file inputs and resumability", {
  samples <- make_test_samples_df(5L)
  csv <- tempfile(fileext = ".csv")
  utils::write.csv(samples, csv, row.names = FALSE)
  session_dir <- tempfile("adaptive-session-")

  judge <- function(A, B, state, ...) {
    y <- as.integer(A$quality_score[[1L]] >= B$quality_score[[1L]])
    list(is_valid = TRUE, Y = y, invalid_reason = NA_character_)
  }

  first <- pairwiseLLM::adaptive_rank(
    data = csv,
    id_col = "ID",
    text_col = "text",
    judge = judge,
    n_steps = 2L,
    session_dir = session_dir,
    resume = FALSE,
    progress = "none"
  )

  second <- pairwiseLLM::adaptive_rank(
    data = csv,
    id_col = "ID",
    text_col = "text",
    judge = judge,
    n_steps = 1L,
    session_dir = session_dir,
    resume = TRUE,
    progress = "none"
  )

  expect_equal(nrow(second$state$step_log), nrow(first$state$step_log) + 1L)
})

test_that("adaptive_rank aborts loudly when saved artifacts cannot be resumed", {
  samples <- make_test_samples_df(4L)
  session_dir <- tempfile("adaptive-bad-session-")
  dir.create(session_dir, recursive = TRUE)
  saveRDS(list(bad = TRUE), file.path(session_dir, "state.rds"))

  judge <- function(A, B, state, ...) {
    y <- as.integer(A$quality_score[[1L]] >= B$quality_score[[1L]])
    list(is_valid = TRUE, Y = y, invalid_reason = NA_character_)
  }

  expect_error(
    pairwiseLLM::adaptive_rank(
      data = samples,
      id_col = "ID",
      text_col = "text",
      judge = judge,
      n_steps = 1L,
      session_dir = session_dir,
      resume = TRUE,
      progress = "none"
    ),
    "Failed to resume adaptive session from `session_dir`"
  )
})

test_that("adaptive_rank wrapper logs preserve anchor/local/repeat and underrep semantics", {
  samples <- make_test_samples_df(8L)
  judge <- function(A, B, state, ...) {
    y <- as.integer(A$quality_score[[1L]] >= B$quality_score[[1L]])
    list(is_valid = TRUE, Y = y, invalid_reason = NA_character_)
  }

  out <- pairwiseLLM::adaptive_rank(
    data = samples,
    id_col = "ID",
    text_col = "text",
    judge = judge,
    n_steps = 30L,
    btl_config = list(refit_pairs_target = 1000L),
    progress = "none",
    seed = 77L
  )

  step_log <- out$logs$step_log
  committed <- step_log[!is.na(step_log$pair_id), , drop = FALSE]
  staged <- committed[committed$round_stage != "warm_start", , drop = FALSE]
  expect_true(nrow(staged) > 0L)

  anchor_rows <- staged[staged$round_stage == "anchor_link", , drop = FALSE]
  if (nrow(anchor_rows) > 0L) {
    expect_true(all(xor(anchor_rows$is_anchor_i, anchor_rows$is_anchor_j)))
  }

  local_rows <- staged[staged$round_stage == "local_link", , drop = FALSE]
  if (nrow(local_rows) > 0L) {
    ok_local <- local_rows$dist_stratum == 0L | local_rows$is_anchor_i | local_rows$is_anchor_j
    expect_true(all(ok_local, na.rm = TRUE))
  }

  staged_by_round <- split(staged, staged$round_id)
  repeat_budget <- pairwiseLLM:::adaptive_defaults(out$state$n_items)$repeat_in_round_budget
  for (round_rows in staged_by_round) {
    repeat_used <- 0L
    uses <- list()
    for (idx in seq_len(nrow(round_rows))) {
      i_key <- as.character(round_rows$i[[idx]])
      j_key <- as.character(round_rows$j[[idx]])
      i_prev <- if (is.null(uses[[i_key]])) 0L else as.integer(uses[[i_key]])
      j_prev <- if (is.null(uses[[j_key]])) 0L else as.integer(uses[[j_key]])
      repeat_used <- repeat_used + as.integer(i_prev > 0L) + as.integer(j_prev > 0L)
      uses[[i_key]] <- i_prev + 1L
      uses[[j_key]] <- j_prev + 1L
    }
    expect_lte(repeat_used, repeat_budget)
  }

  deg <- stats::setNames(rep.int(0L, out$state$n_items), as.character(seq_len(out$state$n_items)))
  explore_rows_checked <- 0L
  for (idx in seq_len(nrow(step_log))) {
    row <- step_log[idx, , drop = FALSE]
    if (!is.na(row$pair_id[[1L]]) && row$round_stage[[1L]] != "warm_start") {
      if (isTRUE(row$is_explore_step[[1L]])) {
        d_min <- min(deg)
        i_key <- as.character(row$i[[1L]])
        j_key <- as.character(row$j[[1L]])
        is_underrep_endpoint <- deg[[i_key]] <= (d_min + 1L) || deg[[j_key]] <= (d_min + 1L)
        expect_true(is_underrep_endpoint)
        explore_rows_checked <- explore_rows_checked + 1L
      }
      deg[[as.character(row$i[[1L]])]] <- deg[[as.character(row$i[[1L]])]] + 1L
      deg[[as.character(row$j[[1L]])]] <- deg[[as.character(row$j[[1L]])]] + 1L
    }
  }
  expect_true(explore_rows_checked >= 1L)
})

test_that("adaptive_rank wrapper exposes top-band defaults and ceiling top-band size", {
  samples <- make_test_samples_df(15L)
  judge <- function(A, B, state, ...) {
    y <- as.integer(A$quality_score[[1L]] >= B$quality_score[[1L]])
    list(is_valid = TRUE, Y = y, invalid_reason = NA_character_)
  }

  out <- pairwiseLLM::adaptive_rank(
    data = samples,
    id_col = "ID",
    text_col = "text",
    judge = judge,
    n_steps = 1L,
    btl_config = list(refit_pairs_target = 1000L),
    progress = "none",
    seed = 9L
  )

  defaults <- pairwiseLLM:::adaptive_defaults(out$state$n_items)
  expect_equal(defaults$top_band_pct, 0.10)
  expect_equal(defaults$top_band_bins, 5L)

  proxy <- pairwiseLLM:::.adaptive_rank_proxy(out$state)
  strata <- pairwiseLLM:::.adaptive_assign_strata(proxy$scores, defaults)
  expect_equal(length(strata$top_band_ids), as.integer(ceiling(0.10 * out$state$n_items)))
})

test_that("adaptive_rank builds internal llm judge and forwards judge_call_args", {
  samples <- make_test_samples_df(4L)[, c("ID", "text")]
  calls <- list()

  testthat::with_mocked_bindings(
    llm_compare_pair = function(...) {
      args <- list(...)
      calls <<- append(calls, list(args))
      tibble::tibble(better_id = as.character(args$ID1))
    },
    {
      out <- pairwiseLLM::adaptive_rank(
        data = samples,
        id_col = "ID",
        text_col = "text",
        backend = "openai",
        model = "gpt-5.1",
        endpoint = "responses",
        judge_args = list(service_tier = "flex"),
        judge_call_args = list(reasoning = "low"),
        n_steps = 1L,
        progress = "none"
      )
      expect_true(inherits(out$state, "adaptive_state"))
      expect_equal(nrow(out$state$step_log), 1L)
    },
    .env = asNamespace("pairwiseLLM")
  )

  expect_length(calls, 1L)
  expect_identical(calls[[1L]]$service_tier, "flex")
  expect_identical(calls[[1L]]$reasoning, "low")
})

test_that("adaptive_rank ignores endpoint for non-openai backends", {
  samples <- make_test_samples_df(4L)[, c("ID", "text")]

  testthat::with_mocked_bindings(
    llm_compare_pair = function(...) tibble::tibble(better_id = list(...)$ID1),
    {
      out <- pairwiseLLM::adaptive_rank(
        data = samples,
        id_col = "ID",
        text_col = "text",
        backend = "anthropic",
        model = "claude-test",
        endpoint = "not-used-here",
        n_steps = 1L,
        progress = "none"
      )
      expect_true(inherits(out$state, "adaptive_state"))
      expect_equal(nrow(out$state$step_log), 1L)
    },
    .env = asNamespace("pairwiseLLM")
  )
})
