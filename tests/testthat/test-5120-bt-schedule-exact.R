test_that("bounded execution matches the frozen scalar bootstrap under all policies", {
  ref <- bt_reference_325()
  withr::local_seed(123L)
  original_rng <- .Random.seed
  for (strategy in c("random", "trueskill_p50", "trueskill_pollitt", "hybrid")) {
    args <- bt_schedule_args_325(strategy)
    before <- serialize(args$initial_state, NULL)
    expected <- bt_result_325(ref$bootstrap_bt_model, args)
    actual <- bt_result_325(bootstrap_bt_model, args)
    expect_identical(bt_scientific_325(actual), bt_scientific_325(expected), info = strategy)
    expect_identical(actual$replicates$n_scheduling_refits[actual$replicates$success],
      rep(2L, actual$n_success))
    expect_identical(.Random.seed, original_rng)
    expect_identical(serialize(args$initial_state, NULL), before)
  }
})

test_that("a refit failure retains every prior scalar return and no failed transaction", {
  ref <- bt_reference_325()
  for (at in c(12L, 24L)) {
    args <- bt_schedule_args_325(budget = 25L)
    args$schedule_fit_fn <- local({
      fail_at <- at
      function(state, config) {
        if (nrow(state$history_pairs) == fail_at) {
          rlang::abort("controlled refit failure", class = "controlled_refit_error",
            diagnostics = list(seed = config$cmdstan$seed, committed = fail_at))
        }
        bt_schedule_fit_325(state, config)
      }
    })
    expected <- bt_result_325(ref$bootstrap_bt_model, args)
    actual <- bt_result_325(bootstrap_bt_model, args)
    expect_identical(actual$replicates$n_comparisons, rep(at - 1L, 2L))
    expect_identical(bt_scientific_325(actual), bt_scientific_325(expected))
  }
})

test_that("native stop boundaries and B14 shadow stopping preserve B16 continuation", {
  ref <- bt_reference_325()
  args <- bt_schedule_args_325("random", budget = 96L)
  args$btl_config <- c(args$btl_config, list(stability_lag = 1L,
    eap_reliability_min = -1, theta_corr_min = -1, theta_sd_rel_change_max = Inf,
    rank_spearman_min = -1))
  expected <- bt_result_325(ref$bootstrap_bt_model, args)
  actual <- bt_result_325(bootstrap_bt_model, args)
  expect_identical(bt_scientific_325(actual), bt_scientific_325(expected))
  expect_identical(actual$replicates$n_comparisons, c(96L, 96L))
  expect_identical(actual$replicates$n_scheduling_refits, c(8L, 8L))
  expect_true(all(!is.na(actual$replicates$stop_boundary_step_id)))
  for (artifact in actual$artifacts) {
    expect_identical(artifact$state$round_log$step_id_at_refit, seq(12L, 96L, 12L))
    expect_true(84L %in% artifact$state$round_log$step_id_at_refit)
  }
})

test_that("package-default cadence and initialization boundary remain exact", {
  ref <- bt_reference_325()
  for (budget in c(11L, 12L, 21L)) {
    args <- bt_schedule_args_325(budget = budget)
    args$btl_config$refit_pairs_target <- NULL
    expected <- bt_result_325(ref$bootstrap_bt_model, args)
    actual <- bt_result_325(bootstrap_bt_model, args)
    expect_identical(bt_scientific_325(actual), bt_scientific_325(expected))
  }
})

test_that("predictive initialization, reservoir orientations and starvation match the frozen source", {
  ref <- bt_reference_325()
  ids <- sprintf("item%02d", 1:12)
  pairs <- t(utils::combn(ids, 2L))
  edges <- data.frame(A_id = pairs[, 2L], B_id = pairs[, 1L], Y = 0L)
  prior <- make_warm_start_prior(stats::setNames(seq(-1, 1, length.out = 12L), ids))
  for (strategy in c("random", "trueskill_p50", "trueskill_pollitt", "hybrid")) {
    args <- bt_schedule_args_325(strategy, warm_start_prior = prior, warm_start_mode = "both",
      replay_reservoir = make_adaptive_replay_reservoir(edges, ids))
    expected <- bt_result_325(ref$bootstrap_bt_model, args)
    actual <- bt_result_325(bootstrap_bt_model, args)
    expect_identical(bt_scientific_325(actual), bt_scientific_325(expected))
  }
  tree <- data.frame(A_id = ids[-12L], B_id = ids[-1L], Y = 0L)
  args <- bt_schedule_args_325("random", budget = 12L,
    replay_reservoir = make_adaptive_replay_reservoir(tree, ids))
  expected <- bt_result_325(ref$bootstrap_bt_model, args)
  actual <- bt_result_325(bootstrap_bt_model, args)
  expect_identical(bt_scientific_325(actual), bt_scientific_325(expected))
  expect_identical(actual$replicates$n_comparisons, c(11L, 11L))
  expect_identical(actual$replicates$failure_reason, rep("candidate_starvation", 2L))
})

test_that("the shared loop preserves stochastic invalid retries and per-attempt RNG", {
  ref <- bt_reference_325()
  judge <- function(A, B, state) {
    attempt <- nrow(state$step_log) + 1L
    if (attempt %in% c(2L, 11L, 12L, 15L, 26L)) {
      return(list(is_valid = FALSE, invalid_reason = "retry_fixture"))
    }
    list(is_valid = TRUE, Y = as.integer(stats::runif(1) < .5))
  }
  for (strategy in c("random", "trueskill_p50", "trueskill_pollitt", "hybrid")) {
    args <- bt_schedule_args_325(strategy)
    state <- pairwiseLLM:::.bt_bootstrap_initial_state(args$initial_state, names(args$object))
    # Bootstrap applies this override once, before its scalar replay loop.
    state <- pairwiseLLM:::.adaptive_apply_controller_config(state, list(max_pairs_after_stop = 100L))
    run <- function(scalar) withr::with_seed(325L, {
      current <- unserialize(serialize(state, NULL))
      trace <- list()
      record <- function(s) {
        trace[[length(trace) + 1L]] <<- list(
          state = bt_scientific_325(list(artifacts = list(list(state = s)))), rng = .Random.seed)
      }
      if (scalar) {
        for (i in seq_len(30L)) {
          current <- ref$adaptive_rank_run_live(current, judge, n_steps = 1L,
            fit_fn = bt_schedule_fit_325, btl_config = args$btl_config, progress = "none")
          record(current)
          if (isTRUE(current$meta$stop_decision)) break
        }
      } else {
        context <- pairwiseLLM:::.adaptive_run_prepare(current, judge, 30L,
          btl_config = args$btl_config, progress = "none")
        context$cfg$iteration_completed <- record
        current <- pairwiseLLM:::.adaptive_run_execute(context$state, judge, context$n_steps,
          bt_schedule_fit_325, context$btl_cfg, context$cfg)
        # Terminal returns occur before the normal end-of-iteration observer.
        if (length(trace) < nrow(current$step_log)) record(current)
      }
      list(state = bt_scientific_325(list(artifacts = list(list(state = current)))),
        trace = trace, rng = .Random.seed)
    })
    expect_identical(run(FALSE), run(TRUE), info = strategy)
  }
})

test_that("the attempt cap crosses neither initialization nor native refit boundaries", {
  # N=257 makes the default initial round longer than the 64-attempt cap.
  # A cheap callback isolates scheduler scaling from dense final estimation.
  ref <- bt_reference_325()
  args <- bt_schedule_args_325("trueskill_p50", n = 257L, budget = 258L)
  args$estimator <- function(bt_data, item_ids) stats::setNames(rep(0, length(item_ids)), item_ids)
  args$estimator_args <- list()
  expected <- bt_result_325(ref$bootstrap_bt_model, args)
  execute <- pairwiseLLM:::.adaptive_run_execute
  attempts <- integer()
  local_mocked_bindings(.adaptive_run_execute = function(state, judge, n_steps, ...) {
    attempts <<- c(attempts, n_steps)
    execute(state, judge, n_steps, ...)
  }, .package = "pairwiseLLM")
  actual <- bt_result_325(bootstrap_bt_model, args)
  expect_identical(bt_scientific_325(actual), bt_scientific_325(expected))
  expect_identical(actual$replicates$n_comparisons, c(258L, 258L))
  expect_identical(actual$replicates$n_scheduling_refits, c(1L, 1L))
  expect_true(64L %in% attempts)
  expect_true(all(attempts <= 64L))
})
