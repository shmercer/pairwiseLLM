test_that("legacy adaptive calibration fails before writing fitted selector thresholds", {
  directory <- withr::local_tempdir()
  expect_error(pairwiseLLM:::.adaptive_linking_calibrate_offline(replicates = 1L,
    seed = 280L, set_sizes = c(10L, 6L), n_steps = 1L,
    btl_config = test_link_btl_config(), output_dir = directory, progress = "none"),
    class = "pairwiseLLM_link_selector_unvalidated")
  expect_length(list.files(directory), 0L)
})

test_that("calibration helper branches validate inputs and fallback behavior", {
  canon_df <- pairwiseLLM:::.adaptive_calibration_canonicalize(
    tibble::tibble(z = 1, a = list(list(k = 2)))
  )
  expect_true(is.list(canon_df))
  expect_true(all(c("z", "a") %in% names(canon_df)))

  expect_error(pairwiseLLM:::.adaptive_calibration_parse_set_sizes(c(3L)), "length 2")
  expect_error(pairwiseLLM:::.adaptive_calibration_parse_set_sizes(c(1L, 3L)), "must be >= 2")

  expect_error(pairwiseLLM:::.adaptive_calibration_truth(
    tibble::tibble(item_id = c("h1", "s1"), set_id = c(1L, 2L)),
    seed = NA_integer_,
    true_delta = 0,
    true_alpha = 1
  ), "single integer")
  expect_error(pairwiseLLM:::.adaptive_calibration_truth(
    tibble::tibble(item_id = c("h1", "s1"), set_id = c(1L, 2L)),
    seed = 1L,
    true_delta = NA_real_,
    true_alpha = 1
  ), "must be finite")
  expect_error(pairwiseLLM:::.adaptive_calibration_truth(
    tibble::tibble(item_id = c("h1", "s1"), set_id = c(1L, 2L)),
    seed = 1L,
    true_delta = 0,
    true_alpha = 0
  ), "> 0")

  bad_judge <- pairwiseLLM:::.adaptive_calibration_judge(
    theta_global = c(h1 = 0.1),
    judge_b = 0,
    judge_eps = 0.01,
    judge_seed = 1L
  )
  bad_out <- bad_judge(
    A = tibble::tibble(item_id = "missing"),
    B = tibble::tibble(item_id = "h1"),
    state = list(step_log = tibble::tibble())
  )
  expect_false(isTRUE(bad_out$is_valid))
  expect_identical(bad_out$invalid_reason, "missing_true_theta")

  expect_error(
    pairwiseLLM:::.adaptive_calibration_judge(
      c(h1 = 0.1),
      judge_b = NA_real_,
      judge_eps = 0.01,
      judge_seed = 1L
    ),
    "finite"
  )
  expect_error(
    pairwiseLLM:::.adaptive_calibration_judge(c(h1 = 0.1), judge_b = 0, judge_eps = 1, judge_seed = 1L),
    "\\[0, 1\\)"
  )

  expect_error(pairwiseLLM:::.adaptive_calibration_fit_fn(c(a = 0), fit_seed = 1L, n_draws = 10L), ">= 20")
  expect_error(pairwiseLLM:::.adaptive_calibration_fit_fn(c(a = 0), fit_seed = 1L, draw_sd = 0), "> 0")

  fit_missing <- pairwiseLLM:::.adaptive_calibration_fit_fn(c(a = 0), fit_seed = 1L)
  expect_error(fit_missing(list(item_ids = c("a", "b"), step_log = tibble::tibble()), list()), "all state items")

  empty_metrics <- pairwiseLLM:::.adaptive_calibration_extract_replicate_metrics(
    list(link_stage_log = pairwiseLLM:::new_link_stage_log()),
    replicate_id = 1L
  )
  expect_equal(nrow(empty_metrics), 0L)

  expect_error(
    pairwiseLLM:::.adaptive_calibration_summarize(
      metrics = tibble::tibble(replicate_id = 1L, eligible = FALSE, ppc_brier_cross_active = NA_real_),
      replicate_count = 1L,
      seed = 1L,
      config_payload = list()
    ),
    "no eligible"
  )

  expect_error(
    pairwiseLLM:::.adaptive_calibration_write_artifacts(
      summary_tbl = tibble::tibble(),
      metrics_tbl = tibble::tibble(),
      sidecar_payload = list(),
      output_dir = NA_character_
    ),
    "output_dir"
  )

  expect_error(pairwiseLLM:::.adaptive_linking_calibrate_offline(replicates = 0L), ">= 1")
  expect_error(
    pairwiseLLM:::.adaptive_calibration_run_replicate(
      replicate_id = 1L,
      seed = 1L,
      set_sizes = c(3L, 3L),
      true_delta = 0,
      true_alpha = 1,
      judge_b = 0,
      judge_eps = 0.1,
      n_steps = 0L
    ),
    ">= 1"
  )

  testthat::local_mocked_bindings(
    .adaptive_calibration_default_artifact_path = function() NA_character_,
    .package = "pairwiseLLM"
  )
  fallback_missing <- pairwiseLLM:::.adaptive_linking_default_calibration()
  expect_identical(fallback_missing$ppc_calibration_id, "default_p95_brier_active")

  testthat::local_mocked_bindings(
    .adaptive_calibration_default_artifact_path = function() "x.json",
    .package = "pairwiseLLM"
  )
  testthat::local_mocked_bindings(
    read_json = function(path, simplifyVector = TRUE) NULL,
    .package = "jsonlite"
  )
  fallback_bad_payload <- pairwiseLLM:::.adaptive_linking_default_calibration()
  expect_identical(fallback_bad_payload$ppc_calibration_id, "default_p95_brier_active")

  testthat::local_mocked_bindings(
    .adaptive_calibration_default_artifact_path = function() "x.json",
    .package = "pairwiseLLM"
  )
  testthat::local_mocked_bindings(
    read_json = function(path, simplifyVector = TRUE) {
      list(cross_set_ppc_brier_max = 99, ppc_calibration_id = "")
    },
    .package = "jsonlite"
  )
  fallback_bad_fields <- pairwiseLLM:::.adaptive_linking_default_calibration()
  expect_identical(fallback_bad_fields$cross_set_ppc_brier_max, 0.20)
  expect_identical(fallback_bad_fields$ppc_calibration_id, "default_p95_brier_active")

  fallback <- pairwiseLLM:::.adaptive_linking_default_calibration()
  expect_true(is.finite(as.double(fallback$cross_set_ppc_brier_max)))
  expect_true(nzchar(as.character(fallback$ppc_calibration_id)))
})
