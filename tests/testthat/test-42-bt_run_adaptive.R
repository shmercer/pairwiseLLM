test_that("bt_run_adaptive validates inputs and judge output", {
  samples <- tibble::tibble(ID = c("A", "B"), text = c("a", "b"))

  expect_error(
    bt_run_adaptive(samples[, "ID"], judge_fun = function(x) x, round_size = 1),
    "samples.*ID, text"
  )

  expect_error(
    bt_run_adaptive(samples, judge_fun = "not a function", round_size = 1),
    "judge_fun.*function"
  )

  # judge_fun missing required columns
  bad_judge <- function(pairs) tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2)
  expect_error(
    bt_run_adaptive(
      samples,
      judge_fun = bad_judge,
      fit_fun = function(bt_data, ...) {
        list(
          engine = "mock",
          reliability = 0.95,
          theta = tibble::tibble(ID = c("A", "B"), theta = c(0, 0), se = c(1, 1)),
          diagnostics = list(sepG = 3.5)
        )
      },
      engine = "mock",
      round_size = 1,
      init_round_size = 1,
      max_rounds = 1,
      rel_se_p90_target = NA_real_,
      rel_se_p90_min_improve = NA_real_
    ),
    "judge_fun.*columns.*better_id"
  )

  # judge column required when judge= is provided
  good_no_judge <- function(pairs) {
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  }
  expect_error(
    bt_run_adaptive(
      samples,
      judge_fun = good_no_judge,
      judge = "model",
      fit_fun = function(bt_data, ...) {
        list(
          engine = "mock",
          reliability = 0.95,
          theta = tibble::tibble(ID = c("A", "B"), theta = c(0, 0), se = c(1, 1)),
          diagnostics = list(sepG = 3.5)
        )
      },
      engine = "mock",
      round_size = 1,
      init_round_size = 1,
      max_rounds = 1,
      rel_se_p90_target = NA_real_,
      rel_se_p90_min_improve = NA_real_
    ),
    "include a `model` column|must include a `model` column|judge.*provided"
  )
})

test_that("bt_run_adaptive maps positional better_id labels to real IDs", {
  samples <- tibble::tibble(ID = c("A", "B", "C"), text = c("a", "b", "c"))

  # returns positional labels instead of literal IDs
  judge_fun <- function(pairs) {
    tibble::tibble(
      ID1 = pairs$ID1,
      ID2 = pairs$ID2,
      better_id = rep("SAMPLE_1", nrow(pairs))
    )
  }

  fit_fun <- function(bt_data, ...) {
    list(
      engine = "mock",
      reliability = 0.95,
      theta = tibble::tibble(ID = c("A", "B", "C"), theta = c(0, 0, 0), se = c(1, 1, 1)),
      diagnostics = list(sepG = 3.5)
    )
  }

  out <- bt_run_adaptive(
    samples = samples,
    judge_fun = judge_fun,
    fit_fun = fit_fun,
    engine = "mock",
    round_size = 1,
    init_round_size = 2,
    max_rounds = 1,
    rel_se_p90_target = NA_real_,
    rel_se_p90_min_improve = NA_real_,
    seed_pairs = 1
  )

  expect_true(nrow(out$results) > 0L)
  expect_true(all(out$results$better_id %in% c(out$results$ID1, out$results$ID2)))
})

test_that("bt_run_adaptive runs rounds and forbids unordered repeats", {
  samples <- tibble::tibble(
    ID = LETTERS[1:6],
    text = paste0("t", LETTERS[1:6])
  )
  true_theta <- stats::setNames(c(3, 2, 1, 0, -1, -2), samples$ID)

  judge_fun <- function(pairs) {
    simulate_bt_judge(
      pairs,
      true_theta = true_theta,
      deterministic = TRUE,
      seed = 1
    )
  }

  fit_fun <- function(bt_data, ...) {
    bt_data <- as.data.frame(bt_data)
    ids <- sort(unique(c(as.character(bt_data[[1]]), as.character(bt_data[[2]]))))
    wins <- stats::setNames(rep(0L, length(ids)), ids)
    n_j <- stats::setNames(rep(0L, length(ids)), ids)

    for (i in seq_len(nrow(bt_data))) {
      a <- as.character(bt_data[[1]][i])
      b <- as.character(bt_data[[2]][i])
      r <- as.numeric(bt_data[[3]][i])
      if (is.finite(r)) {
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

  # Use the new repeat-policy interface. The legacy `forbid_repeats` argument is
  # deprecated and can generate noisy warnings in some environments.
  out <- bt_run_adaptive(
      samples = samples,
      judge_fun = judge_fun,
      fit_fun = fit_fun,
      engine = "mock",
      round_size = 3,
      init_round_size = 3,
      max_rounds = 3,
      repeat_policy = "forbid_unordered",
      rel_se_p90_target = NA_real_,
      rel_se_p90_min_improve = NA_real_,
      seed_pairs = 123
    )

  keys <- paste0(
    pmin(out$results$ID1, out$results$ID2),
    "||",
    pmax(out$results$ID1, out$results$ID2)
  )
  expect_equal(length(keys), length(unique(keys)))
})

test_that("bt_run_adaptive supports judge column when judge= is provided", {
  samples <- tibble::tibble(
    ID = LETTERS[1:5],
    text = paste0("t", LETTERS[1:5])
  )
  true_theta <- stats::setNames(c(2, 1, 0, -1, -2), samples$ID)

  judge_fun <- function(pairs) {
    simulate_bt_judge(
      pairs,
      true_theta = true_theta,
      deterministic = TRUE,
      seed = 10,
      judges = c("m1", "m2"),
      judge_col = "model"
    )
  }

  fit_fun <- function(bt_data, ...) {
    list(
      engine = "mock",
      reliability = 0.95,
      theta = tibble::tibble(ID = samples$ID, theta = rep(0, 5), se = rep(1, 5)),
      diagnostics = list(sepG = 3.5)
    )
  }

  out <- bt_run_adaptive(
    samples = samples,
    judge_fun = judge_fun,
    judge = "model",
    fit_fun = fit_fun,
    engine = "mock",
    round_size = 2,
    init_round_size = 2,
    max_rounds = 1,
    rel_se_p90_target = NA_real_,
    rel_se_p90_min_improve = NA_real_
  )

  expect_true("judge" %in% names(out$bt_data))
})

test_that("bt_run_adaptive restores RNG state when seed_pairs is provided (handles missing .Random.seed)", {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- NULL
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)

  # Ensure "missing seed" branch is real if had_seed is FALSE
  if (!had_seed && exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    rm(".Random.seed", envir = .GlobalEnv)
  }

  on.exit(
    {
      if (had_seed) {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    },
    add = TRUE
  )

  samples <- tibble::tibble(ID = c("A", "B", "C"), text = c("a", "b", "c"))
  true_theta <- c(A = 1, B = 0, C = -1)

  judge_fun <- function(pairs) simulate_bt_judge(pairs, true_theta, deterministic = TRUE, seed = 1)

  fit_fun <- function(bt_data, ...) {
    list(
      engine = "mock",
      reliability = 0.95,
      theta = tibble::tibble(ID = c("A", "B", "C"), theta = c(0, 0, 0), se = c(1, 1, 1)),
      diagnostics = list(sepG = 3.5)
    )
  }

  bt_run_adaptive(
    samples = samples,
    judge_fun = judge_fun,
    fit_fun = fit_fun,
    engine = "mock",
    round_size = 1,
    init_round_size = 1,
    max_rounds = 1,
    seed_pairs = 123,
    rel_se_p90_target = NA_real_,
    rel_se_p90_min_improve = NA_real_
  )

  # Always assert something (prevents "empty test")
  if (had_seed) {
    expect_true(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
    expect_identical(get(".Random.seed", envir = .GlobalEnv, inherits = FALSE), old_seed)
  } else {
    expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  }
})

test_that("bt_run_adaptive maps A/B winner labels", {
  samples <- tibble::tibble(ID = c("A", "B", "C"), text = c("a", "b", "c"))

  judge_fun <- function(pairs) {
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = "A") # maps to ID1
  }

  fit_fun <- function(bt_data, ...) {
    list(
      engine = "mock",
      reliability = 0.95,
      theta = tibble::tibble(ID = samples$ID, theta = rep(0, 3), se = rep(1, 3)),
      diagnostics = list(sepG = 3.5)
    )
  }

  out <- bt_run_adaptive(
    samples = samples,
    judge_fun = judge_fun,
    fit_fun = fit_fun,
    engine = "mock",
    round_size = 1,
    init_round_size = 1,
    max_rounds = 1,
    rel_se_p90_target = NA_real_,
    rel_se_p90_min_improve = NA_real_
  )

  expect_true(all(out$results$better_id %in% c(out$results$ID1, out$results$ID2)))
})

test_that("bt_run_adaptive maps 1/0 better_id encodings", {
  samples <- tibble::tibble(ID = c("A", "B", "C"), text = c("a", "b", "c"))

  judge_fun <- function(pairs) {
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = ifelse(seq_len(nrow(pairs)) %% 2 == 1, "1", "0"))
  }

  fit_fun <- function(bt_data, ...) {
    list(
      engine = "mock",
      reliability = 0.95,
      theta = tibble::tibble(ID = samples$ID, theta = rep(0, 3), se = rep(1, 3)),
      diagnostics = list(sepG = 3.5)
    )
  }

  out <- bt_run_adaptive(
    samples = samples,
    judge_fun = judge_fun,
    fit_fun = fit_fun,
    engine = "mock",
    round_size = 1,
    init_round_size = 2,
    max_rounds = 1,
    rel_se_p90_target = NA_real_,
    rel_se_p90_min_improve = NA_real_
  )

  expect_true(all(out$results$better_id %in% c(out$results$ID1, out$results$ID2)))
})

test_that("bt_run_adaptive normalizes common winner encodings in initial_results", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("a", "b", "c")
  )

  initial_results <- tibble::tibble(
    ID1 = c("A", "B", "C", "A"),
    ID2 = c("B", "C", "A", "C"),
    better_id = c("SAMPLE_1 is better", "0", "Winner: A", "N/A")
  )

  out <- bt_run_adaptive(
    samples = samples,
    judge_fun = function(pairs) stop("judge_fun should not be called"),
    initial_results = initial_results,
    init_round_size = 0,
    max_rounds = 0
  )

  expect_identical(out$results$better_id, c("A", "C", "A", NA_character_))
})

test_that("bt_run_adaptive bootstrap works with repeats allowed and no position balancing", {
  samples <- tibble::tibble(
    ID = LETTERS[1:6],
    text = paste0("t", LETTERS[1:6])
  )
  true_theta <- stats::setNames(c(3, 2, 1, 0, -1, -2), samples$ID)

  judge_fun <- function(pairs) {
    simulate_bt_judge(pairs, true_theta = true_theta, deterministic = TRUE, seed = 999)
  }

  fit_fun <- function(bt_data, ...) {
    list(
      engine = "mock",
      reliability = 0.95,
      theta = tibble::tibble(ID = samples$ID, theta = rep(0, 6), se = rep(1, 6)),
      diagnostics = list(sepG = 3.5)
    )
  }

  out <- bt_run_adaptive(
    samples = samples,
    judge_fun = judge_fun,
    fit_fun = fit_fun,
    engine = "mock",
    init_round_size = 10,
    round_size = 0,
    max_rounds = 0,
    repeat_policy = "allow",
    balance_positions = FALSE,
    seed_pairs = 123
  )

  expect_equal(nrow(out$pairs_bootstrap), 10)
  expect_equal(nrow(out$results), 10)
  expect_true(all(!is.na(out$results$better_id)))
})

test_that("bt_run_adaptive breaks when judge returns no rows for an adaptive round", {
  samples <- tibble::tibble(
    ID = LETTERS[1:6],
    text = paste0("t", LETTERS[1:6])
  )
  true_theta <- stats::setNames(c(3, 2, 1, 0, -1, -2), samples$ID)

  i <- 0L
  judge_fun <- function(pairs) {
    i <<- i + 1L
    if (i == 1L) {
      simulate_bt_judge(pairs, true_theta = true_theta, deterministic = TRUE, seed = 1)
    } else {
      tibble::tibble(ID1 = character(), ID2 = character(), better_id = character())
    }
  }

  fit_fun <- function(bt_data, ...) {
    list(
      engine = "mock",
      reliability = 0.95,
      theta = tibble::tibble(ID = samples$ID, theta = rep(0, 6), se = rep(1, 6)),
      diagnostics = list(sepG = 3.5)
    )
  }

  out <- bt_run_adaptive(
    samples = samples,
    judge_fun = judge_fun,
    fit_fun = fit_fun,
    engine = "mock",
    init_round_size = 2,
    round_size = 2,
    max_rounds = 2,
    rel_se_p90_target = NA_real_,
    rel_se_p90_min_improve = NA_real_,
    seed_pairs = 10
  )

  expect_true(nrow(out$results) >= 2)
})


test_that("bt_run_adaptive errors on duplicate sample IDs", {
  samples <- tibble::tibble(ID = c("A", "A"), text = c("a", "b"))
  expect_error(
    bt_run_adaptive(samples = samples, judge_fun = function(pairs) pairs, max_rounds = 0),
    "must be unique"
  )
})

test_that("bt_run_adaptive covers key input-validation branches", {
  # <2 rows
  samples1 <- tibble::tibble(ID = "A", text = "a")
  expect_error(
    bt_run_adaptive(samples1, judge_fun = function(x) x, round_size = 1),
    "at least 2"
  )

  # missing/empty IDs
  samples_bad <- tibble::tibble(ID = c("A", NA_character_), text = c("a", "b"))
  expect_error(
    bt_run_adaptive(samples_bad, judge_fun = function(x) x, round_size = 1),
    "non-missing"
  )

  # duplicate IDs
  samples_dup <- tibble::tibble(ID = c("A", "A"), text = c("a", "b"))
  expect_error(
    bt_run_adaptive(samples_dup, judge_fun = function(x) x, round_size = 1),
    "unique"
  )

  # judge invalid
  samples2 <- tibble::tibble(ID = c("A", "B"), text = c("a", "b"))
  expect_error(
    bt_run_adaptive(samples2, judge_fun = "nope", round_size = 1),
    "judge_fun.*function"
  )

  # invalid judge arg
  expect_error(
    bt_run_adaptive(samples2, judge_fun = function(x) x, judge = 1, round_size = 1),
    "`judge` must be"
  )

  # negative round parameters
  expect_error(
    bt_run_adaptive(samples2, judge_fun = function(x) x, round_size = -1),
    "non-negative"
  )
  expect_error(
    bt_run_adaptive(samples2, judge_fun = function(x) x, init_round_size = -1, round_size = 1),
    "non-negative"
  )
  expect_error(
    bt_run_adaptive(samples2, judge_fun = function(x) x, max_rounds = -1, round_size = 1),
    "non-negative"
  )
})

test_that("bt_run_adaptive returns empty outputs when no bootstrap and no initial_results", {
  samples <- tibble::tibble(ID = c("A", "B", "C"), text = c("a", "b", "c"))

  out <- bt_run_adaptive(
    samples = samples,
    judge_fun = function(pairs) pairs, # never called
    fit_fun = function(bt_data, ...) stop("should not be called"),
    engine = "mock",
    init_round_size = 0,
    round_size = 1,
    max_rounds = 5,
    rel_se_p90_target = NA_real_,
    rel_se_p90_min_improve = NA_real_
  )

  expect_equal(nrow(out$results), 0L)
  expect_equal(nrow(out$pairs_bootstrap), 0L)
  expect_true(is.data.frame(out$rounds))
  expect_equal(nrow(out$rounds), 0L)
  expect_length(out$fits, 0L)
})

test_that("bt_run_adaptive breaks when round_size==0 (but stop==FALSE)", {
  samples <- tibble::tibble(ID = c("A", "B", "C"), text = c("a", "b", "c"))
  true_theta <- c(A = 1, B = 0, C = -1)

  judge_fun <- function(pairs) simulate_bt_judge(pairs, true_theta = true_theta, deterministic = TRUE, seed = 1)

  # Make stop hard to satisfy
  fit_fun <- function(bt_data, ...) {
    list(
      engine = "mock",
      reliability = 0.0,
      theta = tibble::tibble(ID = c("A", "B", "C"), theta = c(0, 0, 0), se = c(10, 10, 10)),
      diagnostics = list(sepG = 0.0)
    )
  }

  out <- bt_run_adaptive(
    samples = samples,
    judge_fun = judge_fun,
    fit_fun = fit_fun,
    engine = "mock",
    init_round_size = 1,
    round_size = 0,
    max_rounds = 10,
    reliability_target = 0.99,
    sepG_target = 10,
    rel_se_p90_target = 0.0001,
    rel_se_p90_min_improve = NA_real_
  )

  expect_true(nrow(out$rounds) >= 1L)
  expect_equal(out$rounds$n_new_pairs_scored[1], 0L)
})

test_that("bt_run_adaptive breaks when no new pairs available (2 items, forbid repeats)", {
  samples <- tibble::tibble(ID = c("A", "B"), text = c("a", "b"))
  true_theta <- c(A = 1, B = 0)
  judge_fun <- function(pairs) simulate_bt_judge(pairs, true_theta, deterministic = TRUE, seed = 1)

  fit_fun <- function(bt_data, ...) {
    list(
      engine = "mock",
      reliability = 0.0,
      theta = tibble::tibble(ID = c("A", "B"), theta = c(0, 0), se = c(10, 10)),
      diagnostics = list(sepG = 0.0)
    )
  }

  out <- bt_run_adaptive(
    samples = samples,
    judge_fun = judge_fun,
    fit_fun = fit_fun,
    engine = "mock",
    init_round_size = 1,
    round_size = 1,
    max_rounds = 5,
    repeat_policy = "forbid_unordered",
    reliability_target = 0.99,
    sepG_target = 10,
    rel_se_p90_target = 0.0001,
    rel_se_p90_min_improve = NA_real_
  )

  expect_equal(nrow(out$results), 1L)
  expect_equal(nrow(out$rounds), 1L)
})

test_that("bt_run_adaptive breaks when judge returns 0 new results in an adaptive round", {
  samples <- tibble::tibble(ID = c("A", "B", "C"), text = c("a", "b", "c"))
  true_theta <- c(A = 1, B = 0, C = -1)

  calls <- 0L
  judge_fun <- function(pairs) {
    calls <<- calls + 1L
    if (calls == 1L) {
      simulate_bt_judge(pairs, true_theta, deterministic = TRUE, seed = 1)
    } else {
      tibble::tibble(ID1 = character(), ID2 = character(), better_id = character())
    }
  }

  fit_fun <- function(bt_data, ...) {
    list(
      engine = "mock",
      reliability = 0.0,
      theta = tibble::tibble(ID = c("A", "B", "C"), theta = c(0, 0, 0), se = c(10, 10, 10)),
      diagnostics = list(sepG = 0.0)
    )
  }

  out <- bt_run_adaptive(
    samples = samples,
    judge_fun = judge_fun,
    fit_fun = fit_fun,
    engine = "mock",
    init_round_size = 1,
    round_size = 1,
    max_rounds = 5,
    reliability_target = 0.99,
    sepG_target = 10,
    rel_se_p90_target = 0.0001,
    rel_se_p90_min_improve = NA_real_
  )

  expect_true(nrow(out$rounds) >= 1L)
  expect_equal(calls, 2L)
})

test_that("simulate_bt_judge covers deterministic/stochastic + judge labeling", {
  pairs <- tibble::tibble(
    ID1 = c("A", "A", "B", "C"),
    text1 = c("a", "a", "b", "c"),
    ID2 = c("B", "C", "C", "A"),
    text2 = c("b", "c", "c", "a")
  )

  true_theta <- c(A = 0, B = 0, C = 0)

  # deterministic ties use RNG
  out_det <- simulate_bt_judge(
    pairs,
    true_theta = true_theta,
    deterministic = TRUE,
    seed = 123,
    judges = c("m1", "m2"),
    judge_col = "model",
    round_robin = TRUE
  )
  expect_true(all(out_det$better_id %in% c(out_det$ID1, out_det$ID2)))
  expect_equal(out_det$model, c("m1", "m2", "m1", "m2"))

  # stochastic branch
  out_sto <- simulate_bt_judge(
    pairs,
    true_theta = c(A = 2, B = 0, C = -2),
    deterministic = FALSE,
    seed = 123,
    judges = "mX",
    judge_col = "model",
    round_robin = FALSE
  )
  expect_true(all(out_sto$better_id %in% c(out_sto$ID1, out_sto$ID2)))
  expect_true(all(out_sto$model == "mX"))

  # empty pairs returns empty tibble (and keeps judge col if requested)
  out_empty <- simulate_bt_judge(
    pairs[0, ],
    true_theta = c(A = 1, B = 0, C = -1),
    deterministic = TRUE,
    seed = 1,
    judges = "m",
    judge_col = "model"
  )
  expect_equal(nrow(out_empty), 0L)
  expect_true("model" %in% names(out_empty))
})

test_that("check_positional_bias covers inconsistent-pair branch and df-input branch", {
  main <- tibble::tibble(ID1 = c("A", "A"), ID2 = c("B", "B"), better_id = c("A", "A"))
  rev <- tibble::tibble(ID1 = c("B", "B"), ID2 = c("A", "A"), better_id = c("B", "B"))

  cons <- compute_reverse_consistency(main, rev)
  expect_true(is.list(cons))
  expect_true(nrow(cons$details) >= 1L)
  expect_true(any(!cons$details$is_consistent))

  bias1 <- check_positional_bias(cons)
  expect_true(is.list(bias1))
  expect_true(is.numeric(bias1$mean_signed))

  bias2 <- check_positional_bias(cons$details)
  expect_true(is.list(bias2))
})


test_that("bt_run_adaptive returns stop metadata and tags fits", {
  samples <- tibble::tibble(ID = c("A", "B", "C"), text = c("a", "b", "c"))

  # No results, no bootstrap => no fit.
  judge_never <- function(pairs) stop("judge_fun should not be called")
  out0 <- bt_run_adaptive(
    samples = samples,
    judge_fun = judge_never,
    init_round_size = 0,
    max_rounds = 3
  )
  expect_identical(out0$stop_reason, "no_new_pairs")
  expect_true(is.null(out0$final_fit))
  expect_equal(length(out0$fits), 0L)

  # If both init_round_size and round_size are 0, stop_reason should be
  # round_size_zero (not no_new_pairs).
  out00 <- bt_run_adaptive(
    samples = samples,
    judge_fun = judge_never,
    init_round_size = 0,
    round_size = 0,
    max_rounds = 10
  )
  expect_identical(out00$stop_reason, "pair_budget_exhausted")
  expect_identical(out00$stop_round, 0L)
  expect_true(is.null(out00$final_fit))
  expect_equal(length(out00$fits), 0L)

  # Provide a minimal starting result; round_size==0 should stop after fitting.
  initial_results <- tibble::tibble(ID1 = "A", ID2 = "B", better_id = "A")
  fit_fun <- function(bt_data, ...) {
    ids <- sort(unique(c(bt_data[[1]], bt_data[[2]])))
    list(engine = "mock", reliability = 0, theta = tibble::tibble(ID = ids, theta = 0, se = 1), diagnostics = list(sepG = 0))
  }
  out1 <- bt_run_adaptive(
    samples = samples,
    judge_fun = judge_never,
    initial_results = initial_results,
    fit_fun = fit_fun,
    engine = "mock",
    round_size = 0,
    max_rounds = 2,
    rel_se_p90_target = 0,
    rel_se_p90_min_improve = NA_real_
  )
  expect_identical(out1$stop_reason, "pair_budget_exhausted")
  expect_identical(out1$stop_round, 1L)
  expect_true(is.list(attr(out1$fits[[1]], "bt_run_adaptive")))
  expect_identical(attr(out1$fits[[1]], "bt_run_adaptive")$stop_reason, "pair_budget_exhausted")
  expect_identical(out1$final_fit, out1$fits[[length(out1$fits)]])
})

test_that("bt_run_adaptive stop_reason reflects common termination paths", {
  # no_pairs when repeats are forbidden and only one unordered pair exists.
  samples2 <- tibble::tibble(ID = c("A", "B"), text = c("a", "b"))
  initial_results <- tibble::tibble(ID1 = "A", ID2 = "B", better_id = "A")
  fit_fun <- function(bt_data, ...) {
    list(engine = "mock", reliability = 0, theta = tibble::tibble(ID = c("A", "B"), theta = c(0, 1), se = 1), diagnostics = list(sepG = 0))
  }
  judge_fun <- function(pairs) tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  out_np <- bt_run_adaptive(
    samples = samples2,
    judge_fun = judge_fun,
    initial_results = initial_results,
    fit_fun = fit_fun,
    engine = "mock",
    max_rounds = 5,
    round_size = 1,
    rel_se_p90_target = 0,
    rel_se_p90_min_improve = NA_real_,
    repeat_policy = "forbid_unordered",
    balance_positions = FALSE
  )
  expect_identical(out_np$stop_reason, "no_new_pairs")
  expect_identical(out_np$rounds$stop_reason[[1]], "no_new_pairs")

  # no_new_pairs when judge returns 0 new results.
  judge_empty <- function(pairs) tibble::tibble(ID1 = character(), ID2 = character(), better_id = character())
  out_nr <- bt_run_adaptive(
    samples = tibble::tibble(ID = c("A", "B", "C"), text = c("a", "b", "c")),
    judge_fun = judge_empty,
    initial_results = tibble::tibble(ID1 = "A", ID2 = "B", better_id = "A"),
    fit_fun = function(bt_data, ...) {
      list(engine = "mock", reliability = 0, theta = tibble::tibble(ID = c("A", "B", "C"), theta = c(0, 1, 2), se = 1), diagnostics = list(sepG = 0))
    },
    engine = "mock",
    max_rounds = 5,
    round_size = 1,
    rel_se_p90_target = 0,
    rel_se_p90_min_improve = NA_real_,
    forbid_repeats = FALSE,
    balance_positions = FALSE
  )
  expect_identical(out_nr$stop_reason, "no_new_pairs")
  expect_identical(out_nr$rounds$stop_reason[[1]], "no_new_pairs")

  # stopped when stop criteria are satisfied.
  fit_stop <- function(bt_data, ...) {
    list(
      engine = "mock",
      reliability = 0.95,
      theta = tibble::tibble(ID = c("A", "B", "C", "D"), theta = c(-2, -1, 1, 2), se = rep(0.05, 4)),
      diagnostics = list(sepG = 3.5)
    )
  }
  samples4 <- tibble::tibble(ID = c("A", "B", "C", "D"), text = c("a", "b", "c", "d"))
  out_stop <- bt_run_adaptive(
    samples = samples4,
    judge_fun = function(pairs) tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1),
    init_round_size = 2,
    max_rounds = 5,
    min_rounds = 1L,
    round_size = 2,
    fit_fun = fit_stop,
    engine = "mock",
    # When stop_min_* are left as NA, bt_run_adaptive applies internal graph-health
    # gating defaults (see work package B). Disable gating here so this test focuses
    # on the precision termination path.
    stop_min_degree = 0L,
    stop_min_largest_component_frac = 0
  )
  expect_identical(out_stop$stop_reason, "precision_reached")
  expect_true(isTRUE(out_stop$rounds$stop[[1]]))
  expect_identical(attr(out_stop$final_fit, "bt_run_adaptive")$stop_reason, "precision_reached")

  # max_rounds when stop criteria are never met and rounds exhaust.
  fit_never <- function(bt_data, ...) {
    ids <- sort(unique(c(bt_data[[1]], bt_data[[2]])))
    list(engine = "mock", reliability = 0, theta = tibble::tibble(ID = ids, theta = seq_along(ids), se = 1), diagnostics = list(sepG = 0))
  }
  out_max <- bt_run_adaptive(
    samples = tibble::tibble(ID = c("A", "B", "C"), text = c("a", "b", "c")),
    judge_fun = function(pairs) tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1),
    init_round_size = 1,
    max_rounds = 2,
    round_size = 1,
    fit_fun = fit_never,
    engine = "mock",
    forbid_repeats = FALSE,
    balance_positions = FALSE,
    rel_se_p90_target = 0,
    rel_se_p90_min_improve = NA_real_
  )
  expect_identical(out_max$stop_reason, "max_rounds_reached")
  expect_identical(out_max$rounds$stop_reason[[nrow(out_max$rounds)]], "max_rounds_reached")
  expect_identical(attr(out_max$final_fit, "bt_run_adaptive")$stop_reason, "max_rounds_reached")
})

test_that("bt_run_adaptive returns state snapshots per round", {
  samples <- tibble::tibble(ID = c("A", "B", "C"), text = c("a", "b", "c"))

  initial_results <- tibble::tibble(ID1 = "A", ID2 = "B", better_id = "A")

  fit_fun <- function(bt_data, ...) {
    ids <- sort(unique(c(bt_data[[1]], bt_data[[2]])))
    list(
      engine = "mock",
      reliability = 0,
      theta = tibble::tibble(ID = ids, theta = seq_along(ids), se = rep(1, length(ids))),
      diagnostics = list(sepG = 0)
    )
  }

  # round_size==0 forces early stop; state should still have one row
  out <- bt_run_adaptive(
    samples = samples,
    judge_fun = function(pairs) stop("should not be called"),
    initial_results = initial_results,
    fit_fun = fit_fun,
    engine = "mock",
    round_size = 0,
    max_rounds = 2,
    rel_se_p90_target = 0,
    rel_se_p90_min_improve = NA_real_
  )

  expect_true("state" %in% names(out))
  expect_true(is.data.frame(out$state))
  expect_equal(nrow(out$state), 1L)
  expect_true(all(c("n_results", "n_unique_unordered_pairs", "pos_imbalance_max", "round", "stop_reason") %in% names(out$state)))
  expect_equal(out$state$n_self_pairs[[1]], 0L)
})
