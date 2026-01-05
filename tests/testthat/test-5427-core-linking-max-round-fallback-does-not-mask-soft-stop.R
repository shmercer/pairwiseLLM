test_that("core-linking max-round fallback does not mask precision stop on the final allowed round", {
  samples <- tibble::tibble(
    ID = LETTERS[1:5],
    text = paste("text", LETTERS[1:5])
  )

  # Use two new IDs so rel_se_p90 is defined (sd(theta) finite) when metrics are computed on `new_ids`.
  batches <- list(c("C", "D"))
  core_ids <- c("A", "B")

  true_theta <- c(A = 2, B = 1, C = 0, D = -1, E = -2)
  judge_fun <- function(pairs) simulate_bt_judge(pairs, true_theta, deterministic = TRUE)

  mock_fit <- function(bt_data, ...) {
    ids <- sort(unique(c(bt_data$object1, bt_data$object2)))
    list(
      engine = "mock",
      reliability = NA_real_,
      theta = tibble::tibble(
        ID = ids,
        theta = seq_along(ids),
        se = rep(0.05, length(ids))
      ),
      diagnostics = list(sepG = NA_real_)
    )
  }

  out <- bt_run_core_linking(
    samples = samples,
    batches = batches,
    core_ids = core_ids,
    judge_fun = judge_fun,
    fit_fun = mock_fit,
    engine = "mock",
    seed = 1,
    # Ensure both new IDs appear in the BT table on the (single) allowed round.
    round_size = 2,
    max_rounds_per_batch = 1,
    min_rounds = 1,
    within_batch_frac = 0,
    core_audit_frac = 0,
    k_neighbors = Inf,
    min_judgments = 1,
    # disable thresholds that require sirt diagnostics
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_,
    rel_se_p90_target = 0.8,
    # ensure graph gating doesn't block precision stopping (not the focus here)
    stop_min_degree = 0,
    stop_min_largest_component_frac = 0,
    verbose = FALSE
  )

  expect_equal(nrow(out$batch_summary), 1L)
  expect_equal(out$batch_summary$stop_reason[[1]], "precision_reached")

  s1 <- dplyr::filter(out$state, batch_index == 1L, round_index == 1L)
  expect_equal(nrow(s1), 1L)
  expect_equal(s1$stop_reason[[1]], "precision_reached")
})
