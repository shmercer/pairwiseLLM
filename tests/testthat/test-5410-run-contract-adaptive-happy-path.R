test_that("run contract: adaptive happy path validates", {
  withr::local_seed(123)

  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "beta", "gamma")
  )

  judge_fun <- function(pairs, ...) {
    # Deterministic: choose the lexicographically smaller ID as the winner.
    tibble::tibble(
      ID1 = pairs$ID1,
      ID2 = pairs$ID2,
      better_id = ifelse(pairs$ID1 < pairs$ID2, pairs$ID1, pairs$ID2)
    )
  }

  fit_fun <- function(bt_data, ...) {
    list(
      engine = "mock",
      reliability = 0.95,
      theta = tibble::tibble(
        ID = c("A", "B", "C"),
        theta = c(0, 0, 0),
        se = c(1, 1, 1)
      ),
      diagnostics = list(sepG = 3.5),
      provenance = list(source = "unit-test")
    )
  }

  out <- pairwiseLLM::bt_run_adaptive(
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

  expect_silent(pairwiseLLM:::validate_pairwise_run_output(out))

  required_fields <- c(
    "results",
    "estimates",
    "theta",
    "theta_engine",
    "fit_provenance",
    "stop_reason",
    "stop_round",
    "pairing_diagnostics"
  )
  expect_true(all(required_fields %in% names(out)))
})
