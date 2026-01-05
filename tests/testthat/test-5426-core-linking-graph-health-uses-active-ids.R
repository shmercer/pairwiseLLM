
test_that("core-linking graph health uses active IDs (unintroduced IDs do not block stopping)", {
  samples <- tibble::tibble(
    ID = LETTERS[1:5],
    text = paste("text", LETTERS[1:5])
  )

  # Only C is introduced via the requested batches. D/E exist in `samples` but are
  # not introduced, and must not create isolated nodes in graph gating.
  batches <- list(c("C"))
  core_ids <- c("A", "B")

  true_theta <- c(A = 2, B = 1, C = 0, D = -1, E = -2)
  judge_fun <- function(pairs) simulate_bt_judge(pairs, true_theta, deterministic = TRUE)

  round <- 0
  mock_fit <- function(bt_data, ...) {
    round <<- round + 1
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
    round_size = 1,
    # Only need a single round for this test; stopping behavior is not the focus
    # (precision metrics on a single new ID can be undefined for some engines).
    max_rounds_per_batch = 1,
    min_rounds = 1,
    within_batch_frac = 0,
    core_audit_frac = 0,
    k_neighbors = Inf,
    min_judgments = 0,
    # disable thresholds that require sirt diagnostics
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_,
    rel_se_p90_target = 0.8,
    verbose = FALSE
  )

  expect_equal(nrow(out$batch_summary), 1L)
  expect_equal(out$batch_summary$stop_reason[[1]], "max_rounds_reached")
  expect_equal(out$batch_summary$rounds_used[[1]], 1L)

  m1 <- dplyr::filter(out$metrics, batch_index == 1L, round_index == 1L)
  expect_equal(nrow(m1), 1L)

  # If graph gating were computed over all sample IDs, D/E would be isolated and
  # we'd see degree_min == 0 and largest_component_frac < 1.
  expect_gte(as.double(m1$degree_min[[1]]), 1)
  expect_equal(as.double(m1$largest_component_frac[[1]]), 1)
})
