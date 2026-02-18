test_that("offline linking calibration is deterministic and writes required artifacts", {
  out_dir <- withr::local_tempdir()

  run_a <- pairwiseLLM:::.adaptive_linking_calibrate_offline(
    replicates = 2L,
    seed = 11L,
    set_sizes = c(3L, 3L),
    true_delta = -0.4,
    true_alpha = 1.1,
    judge_b = 0.05,
    judge_eps = 0.03,
    n_steps = 30L,
    output_dir = out_dir,
    progress = "none"
  )
  run_b <- pairwiseLLM:::.adaptive_linking_calibrate_offline(
    replicates = 2L,
    seed = 11L,
    set_sizes = c(3L, 3L),
    true_delta = -0.4,
    true_alpha = 1.1,
    judge_b = 0.05,
    judge_eps = 0.03,
    n_steps = 30L,
    output_dir = withr::local_tempdir(),
    progress = "none"
  )

  expect_identical(
    as.character(run_a$sidecar$ppc_calibration_id),
    as.character(run_b$sidecar$ppc_calibration_id)
  )
  expect_equal(
    as.double(run_a$sidecar$cross_set_ppc_brier_max),
    as.double(run_b$sidecar$cross_set_ppc_brier_max),
    tolerance = 1e-12
  )

  expect_true(file.exists(run_a$files$summary_csv))
  expect_true(file.exists(run_a$files$replicates_csv))
  expect_true(file.exists(run_a$files$sidecar_json))

  summary_tbl <- utils::read.csv(run_a$files$summary_csv, stringsAsFactors = FALSE)
  expect_true(all(c("ppc_calibration_id", "cross_set_ppc_brier_max", "quantile_p95") %in% names(summary_tbl)))

  rep_tbl <- utils::read.csv(run_a$files$replicates_csv, stringsAsFactors = FALSE)
  expect_true(all(c("replicate_id", "refit_id", "spoke_id", "ppc_brier_cross_active", "eligible") %in% names(rep_tbl)))

  sidecar <- jsonlite::read_json(run_a$files$sidecar_json, simplifyVector = TRUE)
  expect_true(all(c(
    "cross_set_ppc_brier_max", "ppc_calibration_id", "calibration_quantile",
    "run_metadata", "summary_stats", "config"
  ) %in% names(sidecar)))
  expect_true(all(c("lambda", "ordering_mode") %in% names(sidecar$config$d_opt_knobs)))
  expect_true("probe_pairs_per_refit_per_spoke" %in% names(sidecar$config))
  expect_true(all(c("b", "eps", "model") %in% names(sidecar$config$judge_settings)))
})

test_that("offline calibration reuses canonical production selection utilities", {
  calls <- new.env(parent = emptyenv())
  calls$generate <- 0L
  calls$select <- 0L

  orig_generate <- getFromNamespace("generate_stage_candidates_from_state", "pairwiseLLM")
  orig_select <- getFromNamespace("select_next_pair", "pairwiseLLM")

  testthat::local_mocked_bindings(
    generate_stage_candidates_from_state = function(...) {
      calls$generate <- as.integer(calls$generate + 1L)
      orig_generate(...)
    },
    select_next_pair = function(...) {
      calls$select <- as.integer(calls$select + 1L)
      orig_select(...)
    },
    .package = "pairwiseLLM"
  )

  pairwiseLLM:::.adaptive_linking_calibrate_offline(
    replicates = 2L,
    seed = 17L,
    set_sizes = c(3L, 3L),
    n_steps = 50L,
    progress = "none"
  )

  expect_gt(calls$generate, 0L)
  expect_gt(calls$select, 0L)
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
