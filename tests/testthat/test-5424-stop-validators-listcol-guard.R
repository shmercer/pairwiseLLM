test_that("rounds schema validator rejects list-cols in stop_blocked_*", {
  validate_rounds <- pairwiseLLM:::.validate_rounds_schema

  rounds_base <- tibble::tibble(
    round = 1L,
    n_new_pairs_scored = 0L,
    n_total_results = 0L,
    degree_min = 0,
    largest_component_frac = 1,
    rms_theta_delta = NA_real_,
    topk_overlap = NA_real_,
    stop = FALSE,
    stop_reason = NA_character_,
    stop_blocked_by = NA_character_,
    stop_blocked_candidates = NA_character_
  )

  expect_silent(validate_rounds(rounds_base))

  rounds_list_by <- dplyr::mutate(rounds_base, stop_blocked_by = list("graph_unhealthy"))
  expect_error(validate_rounds(rounds_list_by), "stop_blocked_by")

  rounds_list_cand <- dplyr::mutate(rounds_base, stop_blocked_candidates = list("stability_reached"))
  expect_error(validate_rounds(rounds_list_cand), "stop_blocked_candidates")
})

