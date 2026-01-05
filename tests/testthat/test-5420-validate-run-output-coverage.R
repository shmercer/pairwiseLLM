test_that("validate_pairwise_run_output covers schema/type error paths", {
  validate <- pairwiseLLM:::validate_pairwise_run_output

  base_out <- function(...) {
  out <- list(
    results = tibble::tibble(),
    estimates = NULL,
    theta = NULL,
    theta_engine = NA_character_,
    fit_provenance = list(),
    stop_reason = NA_character_,
    stop_round = NA_integer_,
    pairing_diagnostics = NULL
  )

  dots <- list(...)
  if (length(dots) > 0L) {
    for (nm in names(dots)) {
      out[nm] <- list(dots[[nm]])
    }
  }
  out
}

  expect_error(validate("nope"), "must be a list")
  expect_error(validate(base_out(results = 1)), "tibble or NULL")
  expect_error(validate(base_out(theta_engine = c("a", "b"))), "`theta_engine`")
  expect_error(validate(base_out(stop_round = 1.2)), "`stop_round`")
  expect_error(validate(base_out(fit_provenance = 1)), "`fit_provenance` must be a list")

  # theta schema/type
  expect_error(validate(base_out(theta = tibble::tibble(ID = "A"))), "must include columns `ID` and `theta`")
  expect_error(validate(base_out(theta = tibble::tibble(ID = 1, theta = 0))), "`theta\\$ID` must be character")
  expect_error(validate(base_out(theta = tibble::tibble(ID = "A", theta = "0"))), "`theta\\$theta` must be double")
  expect_error(validate(base_out(theta = tibble::tibble(ID = "A", theta = 0, se = "x"))), "`theta\\$se` must be double")
  expect_error(validate(base_out(theta = tibble::tibble(ID = "A", theta = 0, rank = 1.5))), "`theta\\$rank` must be integer")
})


test_that("validate_pairwise_run_output covers warning/strict branches", {
  validate <- pairwiseLLM:::validate_pairwise_run_output

  base_out <- function(...) {
  out <- list(
    results = tibble::tibble(),
    estimates = NULL,
    theta = NULL,
    theta_engine = NA_character_,
    fit_provenance = list(),
    stop_reason = NA_character_,
    stop_round = NA_integer_,
    pairing_diagnostics = NULL
  )

  dots <- list(...)
  if (length(dots) > 0L) {
    for (nm in names(dots)) {
      out[nm] <- list(dots[[nm]])
    }
  }
  out
}

  # theta NULL but results exist -> warn (or error under strict)
  out_warn <- base_out(results = tibble::tibble(ID1 = "A", ID2 = "B", better_id = "A"))
  expect_warning(validate(out_warn), "Theta is NULL")
  expect_error(validate(out_warn, strict = TRUE), "`theta` is NULL")

  # pairing_diagnostics schema/type checks
  pd_missing <- tibble::tibble(round = 1L)
  expect_error(validate(base_out(pairing_diagnostics = pd_missing)), "missing required column")

  pd_bad_types <- tibble::tibble(
    round = 1.2,
    n_pairs_planned = 1L,
    n_pairs_completed = 1L,
    degree_min = 0,
    largest_component_frac = 1,
    rms_theta_delta = NA_real_,
    topk_overlap = NA_real_,
    stop = TRUE,
    stop_reason = NA_character_,
    stop_blocked_by = NA_character_,
    stop_blocked_candidates = NA_character_
  )
  expect_error(validate(base_out(pairing_diagnostics = pd_bad_types)), "pairing_diagnostics\\$round")

  # stop reason taxonomy: warn vs strict abort
  expect_warning(validate(base_out(stop_reason = "made_up_reason")), "Non-standard stop_reason")
  expect_error(validate(base_out(stop_reason = "made_up_reason"), strict = TRUE), "Non-standard `stop_reason`")
})
