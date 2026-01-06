testthat::test_that("5437 pairing_diagnostics type checks error with informative messages", {
  validate <- getFromNamespace("validate_pairwise_run_output", "pairwiseLLM")

  base_diag <- tibble::tibble(
    round = 1L,
    n_pairs_planned = 2L,
    n_pairs_completed = 2L,
    degree_min = 0.0,
    largest_component_frac = 1.0,
    rms_theta_delta = 0.0,
    topk_overlap = 1.0,
    stop = FALSE,
    stop_reason = "max_rounds",
    stop_blocked_by = NA_character_,
    stop_blocked_candidates = NA_character_
  )

  base_run <- function(diag_tbl) {
    list(
      results = tibble::tibble(),
      # Provide a schema-valid empty estimates tibble so validation proceeds
      # to the pairing_diagnostics type checks.
      estimates = tibble::tibble(
        ID = character(),
        theta_rc = double(),
        rank_rc = integer(),
        pi_rc = double(),
        theta_bt_firth = double(),
        se_bt_firth = double(),
        rank_bt_firth = integer(),
        bt_engine_requested = character(),
        bt_engine_used = character(),
        bt_status = character(),
        bt_failure_reason = character()
      ),
      theta = tibble::tibble(
        ID = character(),
        theta = double(),
        se = double(),
        rank = integer()
      ),
      theta_engine = NA_character_,
      fit_provenance = list(),
      stop_reason = "max_rounds",
      stop_round = 1L,
      pairing_diagnostics = diag_tbl
    )
  }

  cases <- list(
    list(
      name = "n_pairs_planned must be integer",
      diag = dplyr::mutate(base_diag, n_pairs_planned = as.double(2)),
      msg = "pairing_diagnostics\\$n_pairs_planned"
    ),
    list(
      name = "n_pairs_completed must be integer",
      diag = dplyr::mutate(base_diag, n_pairs_completed = as.double(2)),
      msg = "pairing_diagnostics\\$n_pairs_completed"
    ),
    list(
      name = "degree_min must be double",
      diag = dplyr::mutate(base_diag, degree_min = as.integer(1)),
      msg = "pairing_diagnostics\\$degree_min"
    ),
    list(
      name = "largest_component_frac must be double",
      diag = dplyr::mutate(base_diag, largest_component_frac = as.integer(1)),
      msg = "pairing_diagnostics\\$largest_component_frac"
    ),
    list(
      name = "rms_theta_delta must be double",
      diag = dplyr::mutate(base_diag, rms_theta_delta = as.integer(0)),
      msg = "pairing_diagnostics\\$rms_theta_delta"
    ),
    list(
      name = "topk_overlap must be double",
      diag = dplyr::mutate(base_diag, topk_overlap = as.integer(1)),
      msg = "pairing_diagnostics\\$topk_overlap"
    ),
    list(
      name = "stop must be logical",
      diag = dplyr::mutate(base_diag, stop = "FALSE"),
      msg = "pairing_diagnostics\\$stop"
    ),
    list(
      name = "stop_reason must be character",
      diag = dplyr::mutate(base_diag, stop_reason = 1L),
      msg = "pairing_diagnostics\\$stop_reason"
    ),
    list(
      name = "stop_blocked_by must be character",
      diag = dplyr::mutate(base_diag, stop_blocked_by = 1L),
      msg = "pairing_diagnostics\\$stop_blocked_by"
    ),
    list(
      name = "stop_blocked_candidates must be character",
      diag = dplyr::mutate(base_diag, stop_blocked_candidates = 1L),
      msg = "pairing_diagnostics\\$stop_blocked_candidates"
    )
  )

  for (cc in cases) {
    testthat::expect_error(
      validate(base_run(cc$diag)),
      cc$msg,
      info = cc$name
    )
  }
})
