# Independent pre-vectorization source, copied byte-for-byte from
# 589cf73e073bcd9017ca14e5b3f8ceed62fce022. Never regenerate from the optimized
# implementation or edit these fixtures to accommodate an equivalence failure.
direct_scalar_reference_326 <- function(path = testthat::test_path("fixtures", "direct-trueskill-326")) {
  reference <- new.env(parent = asNamespace("pairwiseLLM"))
  sys.source(file.path(path, "adaptive_trueskill.R"), envir = reference)
  sys.source(file.path(path, "adaptive_pairing_strategy.R"), envir = reference)
  reference$probabilities <- eval(quote(function(focal, partners, trueskill_state) {
    vapply(partners, function(partner) {
      trueskill_win_probability(focal, partner, trueskill_state)
    }, numeric(1L))
  }), envir = reference)
  reference
}

direct_fixture_326 <- function(n = 8L, strategy = "trueskill_p50", distribution = "heterogeneous") {
  ids <- sprintf("item%03d", rev(seq_len(n)))
  clock <- function() as.POSIXct("2026-10-10", tz = "UTC")
  environment(clock) <- baseenv()
  # Avoid first-use JIT changing the serialized clock during strict comparisons.
  clock <- compiler::cmpfun(clock)
  state <- pairwiseLLM::adaptive_rank_start(ids, seed = 326L, now_fn = clock,
    adaptive_config = list(pairing_strategy = strategy))
  if (distribution == "heterogeneous") {
    state$trueskill_state$items$mu <- 25 + sin(seq_len(n) * 0.73) * 6
    state$trueskill_state$items$sigma <- 1 + (seq_len(n) %% 17) / 3
  }
  state
}

direct_judge_326 <- function(A, B, state, ...) {
  # Stable across implementations; neither elapsed time nor RNG enters outcomes.
  index <- as.integer(sub("item", "", c(A$item_id[[1L]], B$item_id[[1L]])))
  quality <- sin(index * 0.71)
  list(is_valid = TRUE, Y = as.integer(quality[[1L]] > quality[[2L]]))
}

direct_run_326 <- function(state, n_steps = 2L * state$n_items) {
  pairwiseLLM::adaptive_rank_run_live(state, direct_judge_326, n_steps = n_steps,
    btl_config = list(refit_pairs_target = 5000L), progress = "none")
}
