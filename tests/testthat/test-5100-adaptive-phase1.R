test_that("phase1_generate_pairs is reproducible under seed", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("alpha beta", "charlie", "delta echo foxtrot", "golf hotel")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())

  withr::local_seed(123)
  out1 <- pairwiseLLM:::phase1_generate_pairs(state, n_pairs = 6, seed = 99)
  withr::local_seed(123)
  out2 <- pairwiseLLM:::phase1_generate_pairs(state, n_pairs = 6, seed = 99)

  expect_equal(out1$pairs, out2$pairs)
  expect_equal(out1$state$deg, out2$state$deg)
})

test_that("phase1_generate_pairs respects duplicate policy", {
  samples <- tibble::tibble(
    ID = LETTERS[1:6],
    text = c(
      "alpha beta", "charlie delta", "echo foxtrot", "golf hotel",
      "india juliet", "kilo lima"
    )
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())

  withr::local_seed(42)
  out <- pairwiseLLM:::phase1_generate_pairs(state, n_pairs = 20, seed = 42)

  counts <- table(out$pairs$unordered_key)
  expect_true(all(counts <= 2L))

  for (key in names(counts)[counts == 2L]) {
    rows <- out$pairs[out$pairs$unordered_key == key, ]
    expect_equal(length(unique(rows$ordered_key)), 2L)
    reverse_key <- paste(rows$B_id[1L], rows$A_id[1L], sep = ":")
    expect_true(reverse_key %in% rows$ordered_key)
  }
})

test_that("phase1_generate_pairs keeps imbalance bounded", {
  samples <- tibble::tibble(
    ID = LETTERS[1:8],
    text = paste("text", LETTERS[1:8])
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())

  withr::local_seed(7)
  out <- pairwiseLLM:::phase1_generate_pairs(state, n_pairs = 24, seed = 7)

  expect_true(max(abs(out$state$imb)) <= 3L)
})

test_that("phase1_generate_pairs hits soft mix targets", {
  texts <- vapply(
    1:30,
    function(n) paste(rep("word", n), collapse = " "),
    character(1L)
  )
  samples <- tibble::tibble(
    ID = sprintf("ID%02d", 1:30),
    text = texts
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())

  withr::local_seed(123)
  out <- pairwiseLLM:::phase1_generate_pairs(state, n_pairs = 120, seed = 123, bins = 6)

  strategy <- out$pairs[["strategy"]]
  structured <- strategy != "random"
  structured_rate <- mean(structured)
  within_rate <- mean(strategy[structured] == "within")
  adjacent_rate <- mean(strategy[structured] == "adjacent")

  expect_true(structured_rate >= 0.60 && structured_rate <= 0.80)
  expect_true(within_rate >= 0.40 && within_rate <= 0.60)
  expect_true(adjacent_rate >= 0.40 && adjacent_rate <= 0.60)
})

test_that("degree bias favors low-degree items", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "beta", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  state$deg[["A"]] <- 0L
  state$deg[["B"]] <- 6L
  state$deg[["C"]] <- 6L

  withr::local_seed(22)
  picks <- replicate(200, pairwiseLLM:::.phase1_pick_focus_id(state))
  counts <- table(picks)

  expect_true(counts[["A"]] > counts[["B"]])
  expect_true(counts[["A"]] > counts[["C"]])
})

test_that("phase1_generate_pairs warns and returns early on infeasible state", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "beta")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  state$unordered_count[["A:B"]] <- 2L
  state$ordered_seen <- c("A:B" = TRUE, "B:A" = TRUE)

  withr::local_seed(1)
  expect_warning(
    out <- pairwiseLLM:::phase1_generate_pairs(
      state,
      n_pairs = 1,
      max_attempts_per_pair = 5,
      seed = 1
    ),
    "Generated"
  )
  expect_equal(nrow(out$pairs), 0L)
})

test_that("phase1 helpers cover empty text and bin fallback cases", {
  counts <- pairwiseLLM:::.phase1_word_count(c("", "  ", "alpha beta"))
  expect_equal(unname(counts), c(0L, 0L, 2L))

  fn <- pairwiseLLM:::.phase1_compute_bins
  env <- rlang::env_clone(environment(fn))
  rlang::env_bind(env, cut = function(...) c(1L, NA_integer_))
  fn2 <- rlang::new_function(rlang::fn_fmls(fn), rlang::fn_body(fn), env = env)
  out <- fn2(c(1, 2), bins = 2)
  expect_equal(out$bins, 1)
})

test_that("phase1_prepare_bins rejects unnamed bins", {
  samples <- tibble::tibble(ID = c("A", "B"), text = c("alpha", "beta"))
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  state$wc <- c(1L, 1L)
  state$bin <- c(1L, 1L)
  state$bin_edges <- c(1, 1)

  expect_error(
    pairwiseLLM:::.phase1_prepare_bins(state, bins = 2),
    "named integer vector"
  )
})

test_that("phase1_attempt_pair handles missing opponents per strategy", {
  samples <- tibble::tibble(ID = "A", text = "alpha")
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  state <- pairwiseLLM:::.phase1_prepare_bins(state, bins = 2)

  res_within <- pairwiseLLM:::.phase1_attempt_pair(state, "within")
  res_adj <- pairwiseLLM:::.phase1_attempt_pair(state, "adjacent")
  res_rand <- pairwiseLLM:::.phase1_attempt_pair(state, "random")

  expect_false(res_within$success)
  expect_false(res_adj$success)
  expect_false(res_rand$success)
  expect_equal(res_within$reason, "no_opponent")
  expect_equal(res_adj$reason, "no_opponent")
  expect_equal(res_rand$reason, "no_opponent")
})

test_that("phase1_generate_pairs validates arguments and handles zero demand", {
  samples <- tibble::tibble(ID = c("A", "B", "C"), text = c("a b", "c d", "e f"))
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2))

  expect_error(
    pairwiseLLM:::phase1_generate_pairs(state, n_pairs = 1, mix_struct = 1),
    "mix_struct"
  )
  expect_error(
    pairwiseLLM:::phase1_generate_pairs(state, n_pairs = 1, within_adj_split = 0),
    "within_adj_split"
  )
  expect_error(
    pairwiseLLM:::phase1_generate_pairs(state, n_pairs = 1, max_attempts_per_pair = 0),
    "max_attempts_per_pair"
  )

  out <- pairwiseLLM:::phase1_generate_pairs(state, n_pairs = NULL, seed = 1)
  expect_equal(nrow(out$pairs), state$M1_target)

  out_empty <- pairwiseLLM:::phase1_generate_pairs(out$state, n_pairs = 0)
  expect_equal(nrow(out_empty$pairs), 0L)
})
