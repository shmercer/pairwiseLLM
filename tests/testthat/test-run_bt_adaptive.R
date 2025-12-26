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
  good_no_judge <- function(pairs) tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
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
    "missing judge column"
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

  out <- bt_run_adaptive(
    samples = samples,
    judge_fun = judge_fun,
    fit_fun = fit_fun,
    engine = "mock",
    round_size = 3,
    init_round_size = 3,
    max_rounds = 3,
    forbid_repeats = TRUE,
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

test_that("bt_run_adaptive can run post-stop reverse audit and returns perfect consistency under deterministic judge", {
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
      seed = 999
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
    fit_fun = fit_fun,
    engine = "mock",
    round_size = 5,
    init_round_size = 4,
    max_rounds = 5,
    reverse_audit = TRUE,
    reverse_pct = 1,
    reverse_seed = 1,
    rel_se_p90_target = 1000,
    rel_se_p90_min_improve = NA_real_
  )

  expect_true(is.list(out$reverse_audit))
  expect_true(nrow(out$reverse_audit$pairs_reversed) >= 1L)
  expect_true(is.list(out$reverse_audit$consistency))
  expect_equal(out$reverse_audit$consistency$summary$prop_consistent, 1)
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
