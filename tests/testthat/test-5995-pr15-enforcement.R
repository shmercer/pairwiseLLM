testthat::test_that("PR15 gate: no legacy scheduler is invoked from public API", {
  legacy_names <- c("phase1_generate_pairs", "select_pairs_from_candidates")
  ns <- asNamespace("pairwiseLLM")
  legacy_restore <- list()
  for (nm in legacy_names) {
    if (exists(nm, envir = ns, inherits = FALSE)) {
      legacy_restore[[nm]] <- get(nm, envir = ns, inherits = FALSE)
      assign(nm, function(...) testthat::fail(paste0("Legacy `", nm, "()` invoked.")), envir = ns)
    }
  }
  on.exit({
    for (nm in names(legacy_restore)) {
      assign(nm, legacy_restore[[nm]], envir = ns)
    }
  }, add = TRUE)

  mock_submit <- function(pairs, model, trait_name, trait_description,
                          prompt_template, backend, ...) {
    ids1 <- pairs$ID1
    ids2 <- pairs$ID2
    results <- tibble::tibble(
      pair_uid = pairs$pair_uid,
      unordered_key = pairwiseLLM:::make_unordered_key(ids1, ids2),
      ordered_key = pairwiseLLM:::make_ordered_key(ids1, ids2),
      A_id = ids1,
      B_id = ids2,
      better_id = ids1,
      winner_pos = as.integer(1L),
      phase = as.character(pairs$phase),
      iter = as.integer(pairs$iter),
      received_at = as.POSIXct("2026-01-12 00:00:00", tz = "UTC"),
      backend = as.character(backend),
      model = as.character(model)
    )
    list(
      results = results,
      failed_pairs = tibble::tibble(),
      failed_attempts = pairwiseLLM:::.adaptive_empty_failed_attempts_tbl()
    )
  }

  withr::local_seed(123)
  out <- testthat::with_mocked_bindings(
    adaptive_rank_start(
      samples = tibble::tibble(
        ID = c("A", "B", "C", "D"),
        text = c("alpha", "bravo", "charlie", "delta")
      ),
      model = "gpt-test",
      trait_name = "quality",
      trait_description = "Which is better?",
      backend = "openai",
      mode = "live",
      adaptive = list(
        d1 = 2L,
        M1_target = 2L,
        budget_max = 6L,
        bins = 2L,
        batch_overrides = list(BATCH1 = 2L, BATCH2 = 2L)
      ),
      seed = 123
    ),
    submit_llm_pairs = mock_submit,
    .adaptive_get_refit_fit = function(state, adaptive, batch_size, seed, allow_refit = TRUE) {
      list(
        state = state,
        fit = list(
          theta_mean = stats::setNames(rep(0, state$N), state$ids),
          theta_draws = matrix(0, nrow = 2, ncol = state$N, dimnames = list(NULL, state$ids)),
          diagnostics = list()
        ),
        refit_performed = TRUE
      )
    },
    diagnostics_gate = function(fit, config, near_stop) TRUE,
    .adaptive_theta_summary_from_fit = function(fit, state) {
      tibble::tibble(item_id = state$ids, theta_sd = rep(0.1, length(state$ids)))
    },
    generate_candidates = function(theta_summary, state, config, allow_repeats = FALSE) {
      tibble::tibble(i = "A", j = "B")
    },
    compute_pair_utility = function(draws, candidates, epsilon_mean) {
      tibble::tibble(
        i_id = "A",
        j_id = "B",
        unordered_key = "A:B",
        utility = 0.2,
        utility_raw = 0.2,
        p_mean = 0.5
      )
    },
    apply_degree_penalty = function(utilities, state) utilities,
    select_batch = function(state, candidates_with_utility, config, seed = NULL, exploration_only = FALSE) {
      tibble::tibble(
        A_id = character(),
        B_id = character(),
        unordered_key = character(),
        utility = double(),
        utility_raw = double()
      )
    },
    .adaptive_schedule_repair_pairs = function(...) {
      testthat::fail("Legacy `.adaptive_schedule_repair_pairs()` invoked.")
    },
    .package = "pairwiseLLM"
  )

  testthat::expect_s3_class(out$state, "adaptive_state")
})

testthat::test_that("PR15 gate: no internal *_v3 function definitions remain", {
  ns <- asNamespace("pairwiseLLM")
  v3_suffix_objects <- grep("_v3$", ls(ns, all.names = TRUE), value = TRUE)
  testthat::expect_length(v3_suffix_objects, 0L)

  find_pkg_root <- function(start_dir) {
    dir <- normalizePath(start_dir, winslash = "/", mustWork = FALSE)
    while (!is.na(dir) && nzchar(dir)) {
      if (file.exists(file.path(dir, "DESCRIPTION"))) {
        return(dir)
      }
      parent <- dirname(dir)
      if (identical(parent, dir)) {
        break
      }
      dir <- parent
    }
    NA_character_
  }

  pkg_root <- find_pkg_root(testthat::test_path(".."))
  r_dir <- if (!is.na(pkg_root)) file.path(pkg_root, "R") else NA_character_
  r_files <- if (!is.na(r_dir) && dir.exists(r_dir)) {
    list.files(r_dir, pattern = "\\.R$", full.names = TRUE)
  } else {
    character()
  }
  if (length(r_files) == 0L) {
    testthat::skip("No package source `R/*.R` files available to scan in this test environment.")
  }

  has_v3_suffix_def <- function(path) {
    lines <- readLines(path, warn = FALSE)
    any(grepl("[._[:alnum:]]+_v3\\s*<-\\s*function\\b", lines))
  }
  offenders <- r_files[vapply(r_files, has_v3_suffix_def, logical(1L))]
  testthat::expect_length(offenders, 0L)
})

testthat::test_that("PR15 gate: round_log builder matches contract schema", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(state$N, list())

  fit <- list(
    theta_draws = matrix(
      c(0.1, -0.1, 0.2, -0.2),
      nrow = 2,
      byrow = TRUE,
      dimnames = list(NULL, state$ids)
    )
  )
  metrics <- list(
    theta_sd_median_S = 0.1,
    tau = 0.2,
    theta_sd_pass = TRUE,
    U0 = 0.5,
    U_pass = TRUE,
    diagnostics_pass = TRUE
  )
  stop_out <- list(stop_decision = FALSE, stop_reason = NA_character_)

  row <- pairwiseLLM:::build_round_log_row(
    state = state,
    fit = fit,
    metrics = metrics,
    stop_out = stop_out,
    config = state$config$v3
  )

  schema <- pairwiseLLM:::round_log_schema()
  testthat::expect_s3_class(row, "tbl_df")
  testthat::expect_identical(colnames(row), colnames(schema))
  testthat::expect_true("stop_reason" %in% names(row))
  testthat::expect_true(is.character(row$stop_reason))
})

testthat::test_that("PR15 gate: stop_reason is present and non-empty when stopped", {
  config <- pairwiseLLM:::adaptive_v3_config(3L, list(checks_passed_target = 1L))
  state <- pairwiseLLM:::adaptive_state_new(
    samples = tibble::tibble(ID = c("A", "B", "C"), text = c("a", "b", "c")),
    config = list(),
    seed = 1
  )
  state$config$v3 <- config

  metrics <- list(
    hard_cap_reached = TRUE,
    diagnostics_pass = TRUE,
    theta_sd_pass = TRUE,
    U_pass = TRUE,
    U0 = 0.5
  )

  stop_out <- pairwiseLLM:::should_stop(metrics = metrics, state = state, config = config)

  testthat::expect_true(isTRUE(stop_out$stop_decision))
  testthat::expect_true(is.character(stop_out$stop_reason))
  testthat::expect_true(length(stop_out$stop_reason) == 1L)
  testthat::expect_false(is.na(stop_out$stop_reason))
  testthat::expect_true(nzchar(stop_out$stop_reason))
})
