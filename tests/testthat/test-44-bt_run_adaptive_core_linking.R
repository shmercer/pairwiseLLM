test_that("bt_run_adaptive_core_linking selects core_ids when NULL (random) and returns fits", {
  samples <- tibble::tibble(
    ID = LETTERS[1:8],
    text = paste0("t", LETTERS[1:8])
  )
  true_theta <- stats::setNames(seq(2, -1.5, length.out = 8), samples$ID)

  judge_fun <- function(pairs) {
    b <- ifelse(true_theta[pairs$ID1] >= true_theta[pairs$ID2], pairs$ID1, pairs$ID2)
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = b)
  }

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
    # Batch has 4 IDs while core_size=3 => at least 1 new ID always
    batches = list(c("E", "F", "G", "H")),
    judge_fun = judge_fun,
    core_ids = NULL,
    core_method = "random",
    core_size = 3,
    seed_core = 123,
    seed_pairs = 1,
    fit_fun = fit_fun,
    engine = "mock",
    round_size = 3,
    init_round_size = 3,
    max_rounds_per_batch = 1,
    forbid_repeats = FALSE,
    rel_se_p90_target = 0.8,
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    rel_se_p90_min_improve = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_
  )

  expect_true(is.character(out$core_ids))
  expect_equal(length(out$core_ids), 3)
  expect_true(all(out$core_ids %in% samples$ID))
  expect_true(length(out$fits) >= 1)
  expect_true("batch1" %in% names(out$final_fits))
  expect_true("bootstrap" %in% names(out$final_fits))
  expect_equal(nrow(out$batch_summary), 1)
  expect_true(all(c("metrics", "state", "bt_data") %in% names(out)))
  expect_true(nrow(out$metrics) >= 1)
})

test_that("bt_run_adaptive_core_linking returns round_size_zero when no sampling is requested", {
  samples <- tibble::tibble(ID = LETTERS[1:6], text = paste0("t", LETTERS[1:6]))
  judge_never <- function(pairs) stop("judge_fun should not be called")

  out <- bt_run_adaptive_core_linking(
    samples = samples,
    batches = list(LETTERS[4:6]),
    core_ids = LETTERS[1:3],
    linking = "never",
    judge_fun = judge_never,
    fit_fun = function(...) stop("fit_fun should not be called"),
    engine = "mock",
    round_size = 0,
    init_round_size = 0,
    max_rounds_per_batch = 10,
    reliability_target = Inf,
    seed_pairs = 1
  )

  expect_identical(out$stop_reason, "round_size_zero")
  expect_identical(out$stop_round, 0L)
  expect_equal(nrow(out$results), 0L)
})

test_that("bt_run_adaptive_core_linking drift gating can prevent stopping (hits max_rounds)", {
  samples <- tibble::tibble(
    ID = LETTERS[1:8],
    text = paste0("t", LETTERS[1:8])
  )
  all_ids <- samples$ID
  true_theta <- stats::setNames(seq(2, -1.5, length.out = 8), samples$ID)

  judge_fun <- function(pairs) {
    b <- ifelse(true_theta[pairs$ID1] >= true_theta[pairs$ID2], pairs$ID1, pairs$ID2)
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = b)
  }

  # Fit fun that returns theta for all IDs and forces large core shifts each call.
  counter <- 0L
  fit_fun <- function(bt_data, ...) {
    counter <<- counter + 1L
    bt_data <- tibble::as_tibble(bt_data)

    wins <- stats::setNames(rep(0L, length(all_ids)), all_ids)
    n_j <- stats::setNames(rep(0L, length(all_ids)), all_ids)
    for (i in seq_len(nrow(bt_data))) {
      a <- bt_data$object1[[i]]
      b <- bt_data$object2[[i]]
      r <- bt_data$result[[i]]
      if (r == 1) wins[a] <- wins[a] + 1L else wins[b] <- wins[b] + 1L
      n_j[a] <- n_j[a] + 1L
      n_j[b] <- n_j[b] + 1L
    }

    theta <- as.numeric(wins - stats::median(wins))

    # Force big, alternating shifts on A/B/C so max abs shift is large
    if (counter %% 2L == 0L) {
      theta[all_ids %in% c("A", "B", "C")] <- c(5, -5, 0)
    } else {
      theta[all_ids %in% c("A", "B", "C")] <- c(-5, 5, 0)
    }

    se <- 1 / sqrt(pmax(1L, as.integer(n_j)))
    list(
      engine = "mock",
      reliability = 0.95,
      theta = tibble::tibble(ID = all_ids, theta = theta, se = se),
      diagnostics = list(sepG = 3.5)
    )
  }

  out <- bt_run_adaptive_core_linking(
    samples = samples,
    batches = list(c("D", "E")),
    judge_fun = judge_fun,
    core_ids = c("A", "B", "C"),
    seed_pairs = 11,
    fit_fun = fit_fun,
    engine = "mock",
    round_size = 3,
    init_round_size = 3,
    max_rounds_per_batch = 3,
    forbid_repeats = FALSE,
    # Make non-drift stopping trivially satisfied
    rel_se_p90_target = 10,
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    rel_se_p90_min_improve = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_,
    # Drift guardrail that will always fail given the forced shifts
    core_max_abs_shift_target = 1e-6
  )

  expect_equal(out$batch_summary$stop_reason[[1]], "max_rounds")
  expect_true(any(out$metrics$stop == FALSE))
})

test_that("bt_run_adaptive_core_linking validates core_ids uniqueness", {
  samples <- tibble::tibble(
    ID = LETTERS[1:5],
    text = paste0("t", LETTERS[1:5])
  )

  judge_fun <- function(pairs) tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)

  fit_fun <- function(bt_data, ...) {
    bt_data <- tibble::as_tibble(bt_data)
    ids <- sort(unique(c(bt_data$object1, bt_data$object2)))
    list(engine = "mock", reliability = 0.95, theta = tibble::tibble(ID = ids, theta = seq_along(ids), se = rep(1, length(ids))), diagnostics = list(sepG = 3.5))
  }

  expect_error(
    bt_run_adaptive_core_linking(
      samples = samples,
      batches = list("D"),
      judge_fun = judge_fun,
      core_ids = c("A", "A"),
      fit_fun = fit_fun,
      engine = "mock",
      round_size = 2,
      init_round_size = 2,
      max_rounds_per_batch = 1,
      rel_se_p90_target = 10,
      reliability_target = NA_real_,
      sepG_target = NA_real_,
      rel_se_p90_min_improve = NA_real_,
      max_item_misfit_prop = NA_real_,
      max_judge_misfit_prop = NA_real_
    ),
    "core_ids.*unique"
  )
})

test_that("bt_run_adaptive_core_linking input validation branches are covered", {
  judge_fun <- function(pairs) {
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  }

  # samples missing required columns
  expect_error(
    bt_run_adaptive_core_linking(
      samples = tibble::tibble(ID = c("A", "B")),
      batches = list("A"),
      judge_fun = judge_fun,
      core_ids = c("A", "B"),
      max_rounds_per_batch = 0,
      rel_se_p90_target = 0.7,
      reliability_target = NA_real_,
      sepG_target = NA_real_,
      rel_se_p90_min_improve = NA_real_,
      max_item_misfit_prop = NA_real_,
      max_judge_misfit_prop = NA_real_
    ),
    "samples.*ID.*text"
  )

  # judge_fun must be a function
  expect_error(
    bt_run_adaptive_core_linking(
      samples = tibble::tibble(ID = c("A", "B"), text = c("a", "b")),
      batches = list("A"),
      judge_fun = "nope",
      core_ids = c("A", "B"),
      max_rounds_per_batch = 0,
      rel_se_p90_target = 0.7,
      reliability_target = NA_real_,
      sepG_target = NA_real_,
      rel_se_p90_min_improve = NA_real_,
      max_item_misfit_prop = NA_real_,
      max_judge_misfit_prop = NA_real_
    ),
    "judge_fun.*function"
  )

  # batches can be a character vector (normalization branch),
  # but IDs must exist in samples
  expect_error(
    bt_run_adaptive_core_linking(
      samples = tibble::tibble(ID = c("A", "B"), text = c("a", "b")),
      batches = c("Z"),
      judge_fun = judge_fun,
      core_ids = c("A", "B"),
      max_rounds_per_batch = 0,
      rel_se_p90_target = 0.7,
      reliability_target = NA_real_,
      sepG_target = NA_real_,
      rel_se_p90_min_improve = NA_real_,
      max_item_misfit_prop = NA_real_,
      max_judge_misfit_prop = NA_real_
    ),
    "batch IDs.*samples\\$ID"
  )
})

test_that("bt_run_adaptive_core_linking core selection args branches are executed (core_size/core_pct)", {
  samples <- tibble::tibble(
    ID = LETTERS[1:6],
    text = paste0("t", LETTERS[1:6])
  )

  judge_fun <- function(pairs) {
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  }

  # core_size assignment line covered (then select_core_set errors because size=1)
  expect_error(
    bt_run_adaptive_core_linking(
      samples = samples,
      batches = list(c("E")),
      judge_fun = judge_fun,
      core_ids = NULL,
      core_method = "random",
      core_size = 1, # invalid on purpose
      seed_core = 1,
      max_rounds_per_batch = 0,
      rel_se_p90_target = 0.7,
      reliability_target = NA_real_,
      sepG_target = NA_real_,
      rel_se_p90_min_improve = NA_real_,
      max_item_misfit_prop = NA_real_,
      max_judge_misfit_prop = NA_real_
    ),
    "core_size"
  )

  # core_pct assignment line covered (then select_core_set errors because pct=0)
  expect_error(
    bt_run_adaptive_core_linking(
      samples = samples,
      batches = list(c("E")),
      judge_fun = judge_fun,
      core_ids = NULL,
      core_method = "random",
      core_pct = 0, # invalid on purpose
      seed_core = 1,
      max_rounds_per_batch = 0,
      rel_se_p90_target = 0.7,
      reliability_target = NA_real_,
      sepG_target = NA_real_,
      rel_se_p90_min_improve = NA_real_,
      max_item_misfit_prop = NA_real_,
      max_judge_misfit_prop = NA_real_
    ),
    "core_pct"
  )
})

test_that("bt_run_adaptive_core_linking validates core_ids uniqueness", {
  samples <- tibble::tibble(
    ID = LETTERS[1:4],
    text = paste0("t", LETTERS[1:4])
  )

  judge_fun <- function(pairs) {
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  }

  expect_error(
    bt_run_adaptive_core_linking(
      samples = samples,
      batches = list(c("C")),
      judge_fun = judge_fun,
      core_ids = c("A", "A"),
      max_rounds_per_batch = 0,
      rel_se_p90_target = 0.7,
      reliability_target = NA_real_,
      sepG_target = NA_real_,
      rel_se_p90_min_improve = NA_real_,
      max_item_misfit_prop = NA_real_,
      max_judge_misfit_prop = NA_real_
    ),
    "core_ids.*unique"
  )
})

test_that("bt_run_adaptive_core_linking bootstrap init_round_size validation and empty-state_row branch", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("a", "b", "c")
  )

  judge_fun <- function(pairs) {
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  }

  # init_round_size negative triggers the stop() branch
  expect_error(
    bt_run_adaptive_core_linking(
      samples = samples,
      batches = list(c("C")),
      judge_fun = judge_fun,
      core_ids = c("A", "B"),
      init_round_size = -1,
      max_rounds_per_batch = 0,
      rel_se_p90_target = 0.7,
      reliability_target = NA_real_,
      sepG_target = NA_real_,
      rel_se_p90_min_improve = NA_real_,
      max_item_misfit_prop = NA_real_,
      max_judge_misfit_prop = NA_real_
    ),
    "init_round_size.*non-negative"
  )

  # init_round_size=0 leaves results empty -> state_row(nrow(res)==0) branch,
  # then stops with "No BT fit could be computed..."
  expect_error(
    bt_run_adaptive_core_linking(
      samples = samples,
      batches = list(c("C")),
      judge_fun = judge_fun,
      core_ids = c("A", "B"),
      init_round_size = 0,
      max_rounds_per_batch = 0,
      rel_se_p90_target = 0.7,
      reliability_target = NA_real_,
      sepG_target = NA_real_,
      rel_se_p90_min_improve = NA_real_,
      max_item_misfit_prop = NA_real_,
      max_judge_misfit_prop = NA_real_
    ),
    "No BT fit could be computed"
  )
})

test_that("bt_run_adaptive_core_linking covers no_new_ids, no_pairs, no_results, stopped, and max_rounds paths", {
  # Shared samples
  samples <- tibble::tibble(
    ID = LETTERS[1:6],
    text = paste0("t", LETTERS[1:6])
  )

  # Deterministic judge (always picks ID1)
  judge_fun <- function(pairs) {
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  }

  # Minimal build_bt hook: always produces BT rows when results exist
  build_bt_fun_simple <- function(res, judge = NULL) {
    res <- tibble::as_tibble(res)
    if (nrow(res) == 0L) {
      return(tibble::tibble(object1 = character(), object2 = character(), result = integer()))
    }
    tibble::tibble(
      object1 = as.character(res$ID1),
      object2 = as.character(res$ID2),
      result  = as.integer(as.character(res$better_id) == as.character(res$ID1))
    )
  }

  # Mock BT fitter: theta = centered win counts, se = 1/sqrt(judgments)
  fit_fun <- function(bt_data, ...) {
    bt_data <- tibble::as_tibble(bt_data)
    ids <- sort(unique(c(bt_data$object1, bt_data$object2)))
    wins <- stats::setNames(rep(0L, length(ids)), ids)
    n_j <- stats::setNames(rep(0L, length(ids)), ids)
    for (i in seq_len(nrow(bt_data))) {
      a <- bt_data$object1[[i]]
      b <- bt_data$object2[[i]]
      r <- bt_data$result[[i]]
      if (isTRUE(is.finite(r))) {
        if (r == 1) wins[a] <- wins[a] + 1L else wins[b] <- wins[b] + 1L
        n_j[a] <- n_j[a] + 1L
        n_j[b] <- n_j[b] + 1L
      }
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

  init_res <- tibble::tibble(ID1 = "A", ID2 = "B", better_id = "A")

  # ---- no_new_ids path ----
  out_no_new <- bt_run_adaptive_core_linking(
    samples = samples,
    batches = list(c("A", "B")), # all already in core -> no_new_ids
    judge_fun = judge_fun,
    core_ids = c("A", "B", "C"),
    initial_results = init_res,
    fit_fun = fit_fun,
    build_bt_fun = build_bt_fun_simple,
    engine = "mock",
    max_rounds_per_batch = 0,
    rel_se_p90_target = 0.7,
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    rel_se_p90_min_improve = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_
  )
  expect_equal(out_no_new$batch_summary$stop_reason[[1]], "no_new_ids")
  expect_true(nrow(out_no_new$metrics) == 0L) # metrics_rows empty -> empty tibble branch

  # ---- no_pairs path (all possible core_new pairs already exist + forbid_repeats=TRUE) ----
  # core=A,B ; new=C ; possible core_new are (A,C) and (B,C); pre-fill both
  init_all_pairs <- tibble::tibble(
    ID1 = c("A", "B"),
    ID2 = c("C", "C"),
    better_id = c("A", "B")
  )
  out_no_pairs <- bt_run_adaptive_core_linking(
    samples = samples,
    batches = list(c("C")),
    judge_fun = judge_fun,
    core_ids = c("A", "B"),
    initial_results = init_all_pairs,
    fit_fun = fit_fun,
    build_bt_fun = build_bt_fun_simple,
    engine = "mock",
    round_size = 10,
    max_rounds_per_batch = 1,
    forbid_repeats = TRUE,
    within_batch_frac = 0,
    core_audit_frac = 0,
    rel_se_p90_target = 0.7,
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    rel_se_p90_min_improve = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_
  )
  expect_equal(out_no_pairs$batch_summary$stop_reason[[1]], "no_pairs")
  expect_true(nrow(out_no_pairs$metrics) == 0L)

  # ---- no_results path (force fit_from_results to return NULL after judging a round) ----
  # Must still produce a warm-start fit from initial_results, so:
  # - First build returns normal BT rows
  # - Subsequent builds return empty BT rows -> fit becomes NULL -> stop_reason "no_results"
  build_bt_drop_after_first <- local({
    first <- TRUE
    function(res, judge = NULL) {
      if (isTRUE(first)) {
        first <<- FALSE
        build_bt_fun_simple(res, judge = judge)
      } else {
        tibble::tibble(object1 = character(), object2 = character(), result = integer())
      }
    }
  })

  out_no_results <- bt_run_adaptive_core_linking(
    samples = samples,
    batches = list(c("D")),
    judge_fun = judge_fun,
    core_ids = c("A", "B", "C"),
    initial_results = init_res, # ensures warm-start fit exists before the round
    fit_fun = fit_fun,
    build_bt_fun = build_bt_drop_after_first,
    engine = "mock",
    round_size = 2,
    max_rounds_per_batch = 1,
    forbid_repeats = FALSE,
    rel_se_p90_target = 0.7,
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    rel_se_p90_min_improve = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_
  )
  expect_equal(out_no_results$batch_summary$stop_reason[[1]], "no_results")

  # ---- stopped path (use 2 new IDs so rel_se_p90 is computable; force stop) ----
  # Dedicated fitter for this subcase: distinct theta ensures theta_sd > 0 for new IDs.
  fit_fun_stop <- function(bt_data, ...) {
    bt_data <- tibble::as_tibble(bt_data)
    ids <- sort(unique(c(bt_data$object1, bt_data$object2)))
    list(
      engine = "mock",
      reliability = 0.95,
      theta = tibble::tibble(
        ID = ids,
        theta = seq_along(ids), # distinct values -> theta_sd finite when >= 2 items
        se = rep(0.1, length(ids))
      ),
      diagnostics = list(sepG = 3.5)
    )
  }

  out_stopped <- bt_run_adaptive_core_linking(
    samples = samples,
    batches = list(c("E", "F")), # 2 new IDs -> rel_se_p90 not NA
    judge_fun = judge_fun,
    core_ids = c("A", "B", "C"),
    initial_results = init_res,
    fit_fun = fit_fun_stop,
    build_bt_fun = build_bt_fun_simple,
    engine = "mock",
    round_size = 4,
    max_rounds_per_batch = 2,
    forbid_repeats = FALSE,
    rel_se_p90_target = 999, # trivially satisfied
    rel_se_p90_min_improve = NA_real_, # don't require improvement to stop
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_
  )
  expect_equal(out_stopped$batch_summary$stop_reason[[1]], "stopped")
  expect_equal(out_stopped$stop_reason, "stopped")
  expect_true(is.integer(out_stopped$stop_round) || is.na(out_stopped$stop_round))
  expect_true(nrow(out_stopped$metrics) >= 1L)

  # ---- max_rounds fallback via max_rounds_per_batch=0 (seq_len(0) => no loop) ----
  out_max0 <- bt_run_adaptive_core_linking(
    samples = samples,
    batches = list(c("F")),
    judge_fun = judge_fun,
    core_ids = c("A", "B", "C"),
    initial_results = init_res,
    fit_fun = fit_fun,
    build_bt_fun = build_bt_fun_simple,
    engine = "mock",
    max_rounds_per_batch = 0, # ensures stop_reason stays NA, then fallback sets max_rounds
    rel_se_p90_target = 0.7,
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    rel_se_p90_min_improve = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_
  )
  expect_equal(out_max0$batch_summary$stop_reason[[1]], "max_rounds")
  expect_equal(out_max0$stop_reason, "max_rounds")
  expect_true(is.integer(out_max0$stop_round) || is.na(out_max0$stop_round))
  expect_true(nrow(out_max0$metrics) == 0L)
})

test_that("bt_run_adaptive_core_linking allocation_fun can update allocations between rounds", {
  samples <- tibble::tibble(
    ID = LETTERS[1:8],
    text = paste0("t", LETTERS[1:8])
  )
  all_ids <- samples$ID
  true_theta <- stats::setNames(seq(2, -1.5, length.out = 8), samples$ID)

  judge_fun <- function(pairs) {
    b <- ifelse(true_theta[pairs$ID1] >= true_theta[pairs$ID2], pairs$ID1, pairs$ID2)
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = b)
  }

  # Simple fit that returns theta/se for all IDs
  fit_fun <- function(bt_data, ...) {
    bt_data <- tibble::as_tibble(bt_data)
    wins <- stats::setNames(rep(0L, length(all_ids)), all_ids)
    n_j <- stats::setNames(rep(0L, length(all_ids)), all_ids)
    for (i in seq_len(nrow(bt_data))) {
      a <- bt_data$object1[[i]]
      b <- bt_data$object2[[i]]
      r <- bt_data$result[[i]]
      if (r == 1) wins[a] <- wins[a] + 1L else wins[b] <- wins[b] + 1L
      n_j[a] <- n_j[a] + 1L
      n_j[b] <- n_j[b] + 1L
    }
    theta <- as.numeric(wins - stats::median(wins))
    se <- rep(1, length(all_ids))
    list(
      engine = "mock",
      reliability = NA_real_,
      theta = tibble::tibble(ID = all_ids, theta = theta, se = se),
      diagnostics = list(sepG = NA_real_)
    )
  }

  alloc <- function(state) {
    list(within_batch_frac = state$within_batch_frac + 0.2)
  }

  out <- bt_run_adaptive_core_linking(
    samples = samples,
    batches = list(c("D", "E")),
    judge_fun = judge_fun,
    core_ids = c("A", "B", "C"),
    seed_pairs = 11,
    fit_fun = fit_fun,
    engine = "mock",
    round_size = 4,
    init_round_size = 4,
    max_rounds_per_batch = 2,
    forbid_repeats = FALSE,
    within_batch_frac = 0.1,
    core_audit_frac = 0.1,
    allocation_fun = alloc,
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    rel_se_p90_target = 0,
    rel_se_p90_min_improve = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_
  )

  m <- dplyr::filter(out$metrics, batch_index == 1L, stage == "round")
  expect_equal(nrow(m), 2L)
  expect_equal(m$within_batch_frac[[1]], 0.1)
  expect_equal(m$within_batch_frac[[2]], 0.3)
  expect_equal(m$core_audit_frac[[1]], 0.1)
  expect_equal(m$core_audit_frac[[2]], 0.1)
})

test_that("bt_run_adaptive_core_linking errors when allocation_fun returns a non-list", {
  samples <- tibble::tibble(
    ID = LETTERS[1:6],
    text = paste0("t", LETTERS[1:6])
  )
  all_ids <- samples$ID

  judge_fun <- function(pairs) tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  fit_fun <- function(bt_data, ...) {
    list(
      engine = "mock",
      reliability = NA_real_,
      theta = tibble::tibble(ID = all_ids, theta = seq_along(all_ids), se = rep(1, length(all_ids))),
      diagnostics = list(sepG = NA_real_)
    )
  }

  bad_alloc <- function(state) 0.2

  expect_error(
    bt_run_adaptive_core_linking(
      samples = samples,
      batches = list(c("D")),
      judge_fun = judge_fun,
      core_ids = c("A", "B", "C"),
      seed_pairs = 11,
      fit_fun = fit_fun,
      engine = "mock",
      round_size = 4,
      init_round_size = 4,
      max_rounds_per_batch = 1,
      forbid_repeats = FALSE,
      allocation_fun = bad_alloc,
      reliability_target = NA_real_,
      sepG_target = NA_real_,
      rel_se_p90_target = 0,
      rel_se_p90_min_improve = NA_real_,
      max_item_misfit_prop = NA_real_,
      max_judge_misfit_prop = NA_real_
    ),
    "allocation_fun.*return"
  )
})

testthat::test_that("bt_run_adaptive_core_linking covers additional validation branches", {
  samples <- tibble::tibble(ID = c("A", "B"), text = c("a", "b"))
  judge_fun <- function(pairs) tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  fit_fun <- function(bt_data, ...) {
    list(
      engine = "mock",
      reliability = NA_real_,
      theta = tibble::tibble(ID = samples$ID, theta = c(0, 1), se = c(1, 1)),
      diagnostics = list(sepG = NA_real_)
    )
  }

  # samples with <2 rows
  one_row <- tibble::tibble(ID = "A", text = "a")
  testthat::expect_error(
    bt_run_adaptive_core_linking(
      samples = one_row,
      batches = list("A"),
      judge_fun = judge_fun,
      core_ids = c("A", "B"),
      fit_fun = fit_fun,
      round_size = 1,
      init_round_size = 0,
      max_rounds_per_batch = 0,
      rel_se_p90_target = 0,
      reliability_target = NA_real_,
      sepG_target = NA_real_,
      rel_se_p90_min_improve = NA_real_,
      max_item_misfit_prop = NA_real_,
      max_judge_misfit_prop = NA_real_
    ),
    "at least 2"
  )

  # core_ids length < 2
  testthat::expect_error(
    bt_run_adaptive_core_linking(
      samples = samples,
      batches = list("A"),
      judge_fun = judge_fun,
      core_ids = "A",
      fit_fun = fit_fun,
      round_size = 1,
      init_round_size = 0,
      max_rounds_per_batch = 0,
      rel_se_p90_target = 0,
      reliability_target = NA_real_,
      sepG_target = NA_real_,
      rel_se_p90_min_improve = NA_real_,
      max_item_misfit_prop = NA_real_,
      max_judge_misfit_prop = NA_real_
    ),
    "at least 2"
  )

  # invalid linking targets (must be scalar)
  testthat::expect_error(
    bt_run_adaptive_core_linking(
      samples = samples,
      batches = list("A"),
      judge_fun = judge_fun,
      core_ids = c("A", "B"),
      fit_fun = fit_fun,
      linking_cor_target = c(0.9, 0.8),
      round_size = 1,
      init_round_size = 0,
      max_rounds_per_batch = 0,
      rel_se_p90_target = 0,
      reliability_target = NA_real_,
      sepG_target = NA_real_,
      rel_se_p90_min_improve = NA_real_,
      max_item_misfit_prop = NA_real_,
      max_judge_misfit_prop = NA_real_
    ),
    "linking_cor_target"
  )

  testthat::expect_error(
    bt_run_adaptive_core_linking(
      samples = samples,
      batches = list("A"),
      judge_fun = judge_fun,
      core_ids = c("A", "B"),
      fit_fun = fit_fun,
      linking_min_n = 0,
      round_size = 1,
      init_round_size = 0,
      max_rounds_per_batch = 0,
      rel_se_p90_target = 0,
      reliability_target = NA_real_,
      sepG_target = NA_real_,
      rel_se_p90_min_improve = NA_real_,
      max_item_misfit_prop = NA_real_,
      max_judge_misfit_prop = NA_real_
    ),
    "linking_min_n"
  )
})

testthat::test_that("bt_run_adaptive_core_linking can record missing_theta and exercises drift reference selection", {
  samples <- tibble::tibble(ID = c("A", "B", "C", "D"), text = paste0("t", 1:4))
  judge_fun <- function(pairs) tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)

  # Fit with theta=NULL; we avoid rounds by providing no new ids in the batch.
  fit_no_theta <- function(bt_data, ...) {
    list(engine = "mock", reliability = NA_real_, theta = NULL, diagnostics = list(sepG = NA_real_))
  }

  out <- bt_run_adaptive_core_linking(
    samples = samples,
    batches = list(c("A", "B")),
    judge_fun = judge_fun,
    core_ids = c("A", "B"),
    fit_fun = fit_no_theta,
    round_size = 1,
    init_round_size = 1,
    max_rounds_per_batch = 1,
    rel_se_p90_target = 0,
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    rel_se_p90_min_improve = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_
  )
  testthat::expect_identical(out$final_fits$bootstrap$linking$reason, "missing_theta")

  # Exercise drift reference selection for b==1 (bootstrap_fit NULL via initial_results) and b>1.
  all_ids <- samples$ID
  fit_theta <- function(bt_data, ...) {
    list(
      engine = "mock",
      reliability = NA_real_,
      theta = tibble::tibble(ID = all_ids, theta = seq_along(all_ids), se = rep(1, length(all_ids))),
      diagnostics = list(sepG = NA_real_)
    )
  }

  out2 <- bt_run_adaptive_core_linking(
    samples = samples,
    batches = list(c("C"), c("D")),
    judge_fun = judge_fun,
    core_ids = c("A", "B"),
    initial_results = tibble::tibble(ID1 = "A", ID2 = "B", better_id = "A"),
    fit_fun = fit_theta,
    # explicitly pass drift targets so the "missing()" overrides execute
    core_theta_cor_target = 0.99,
    core_theta_spearman_target = 0.99,
    core_p90_abs_shift_target = 0.5,
    round_size = 1,
    init_round_size = 0,
    max_rounds_per_batch = 1,
    rel_se_p90_target = 0,
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    rel_se_p90_min_improve = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_
  )

  testthat::expect_true(all(c("batch1", "batch2") %in% names(out2$final_fits)))
})
