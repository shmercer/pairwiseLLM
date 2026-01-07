# Additional bt_run_adaptive coverage for branches that are hard to hit in
# end-to-end tests.

# NOTE: This file mocks bindings inside the pairwiseLLM namespace.
#
# We intentionally avoid `testthat::local_mocked_bindings()` because in some
# `test_file()` / full-suite flows it can fail to restore locked namespace
# bindings, which then breaks later tests. Instead we use a small helper that
# performs explicit unlock/assign/lock and guaranteed restoration via `on.exit()`.

with_ns_mocks <- function(.ns, ..., code) {
  mocks <- list(...)
  nms <- names(mocks)
  if (length(nms) == 0L || anyNA(nms) || any(nms == "")) {
    stop("with_ns_mocks() requires named mocks.", call. = FALSE)
  }

  old <- lapply(nms, function(nm) get(nm, envir = .ns, inherits = FALSE))
  old <- stats::setNames(old, nms)
  was_locked <- vapply(nms, function(nm) bindingIsLocked(nm, .ns), logical(1))

  # Apply mocks.
  for (nm in nms) {
    if (isTRUE(was_locked[[nm]])) unlockBinding(nm, .ns)
    assign(nm, mocks[[nm]], envir = .ns)
    if (isTRUE(was_locked[[nm]])) lockBinding(nm, .ns)
  }

  on.exit({
    for (nm in rev(nms)) {
      # Restore original binding and locking state.
      if (bindingIsLocked(nm, .ns)) unlockBinding(nm, .ns)
      assign(nm, old[[nm]], envir = .ns)
      if (isTRUE(was_locked[[nm]])) lockBinding(nm, .ns)
    }
  }, add = TRUE)

  force(code)
  invisible(TRUE)
}

test_that("5470-01 bt_run_adaptive normalizes list columns in resumed rounds and handles start_round > max_rounds", {
  samples <- tibble::tibble(ID = c("a", "b"), text = c("A", "B"))

  # Minimal checkpoint payload; bt_run_adaptive only validates run_type + ids.
  chkdir <- withr::local_tempdir()
  chk <- list(
    run_type = "adaptive",
    ids = samples$ID,
    timestamp = Sys.time(),
    completed = FALSE,
    next_round = 2L,
    results = tibble::tibble(ID1 = "a", ID2 = "b", better_id = "a"),
    fits = list(),
    state = tibble::tibble(round = 1L),
    rounds = tibble::tibble(
      round = 1L,
      results_n = 1L,
      stop_reason = list("precision_reached"),
      stop_blocked_by = list("graph"),
      stop_blocked_candidates = list(c("precision_reached")),
      stop = list(TRUE)
    )
  )
  saveRDS(chk, file = file.path(chkdir, "run_state.rds"))

  out <- withCallingHandlers(
    pairwiseLLM::bt_run_adaptive(
      samples,
      judge_fun = function(pairs, ...) tibble::tibble(),
      fit_fun = function(bt_data, ...) list(theta = tibble::tibble(ID = samples$ID, theta = c(0, 0), se = c(NA_real_, NA_real_))),
      resume_from = chkdir,
      max_rounds = 1L,
      round_size = 1L,
      final_refit = FALSE
    ),
    warning = function(w) {
      # This test intentionally exercises resume/normalization behavior. In this
      # minimal checkpoint scenario, theta may be absent; the validator warning
      # is advisory, so we muffle it while allowing other warnings through.
      if (grepl("Theta is NULL but the run produced results/estimates", conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )

  expect_equal(out$stop_reason, "max_rounds_reached")
  expect_true(is.character(out$rounds$stop_reason))
  expect_true(is.character(out$rounds$stop_blocked_by))
  expect_true(is.character(out$rounds$stop_blocked_candidates))
  expect_true(is.logical(out$rounds$stop))
})


test_that("5470-02 bt_run_adaptive evaluates graph gates and increments stability streak", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("a", "b", "c")
  )

  initial_results <- tibble::tibble(ID1 = "A", ID2 = "B", better_id = "A")

  judge_fun <- function(pairs, ...) {
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  }

fit_fun <- function(bt_data, ...) {
  ids <- unique(c(bt_data$object1, bt_data$object2))
  tibble_theta <- tibble::tibble(
    ID = ids,
    theta = rep(0, length(ids)),
    se = rep(0.2, length(ids))
  )
  list(theta = tibble_theta, diagnostics = list())
}

  # Deterministic pairing; keep it simple.
  mock_select_pairs <- function(samples, results, round_size, ...) {
    out <- tibble::tibble(
      ID1 = "A", text1 = "a",
      ID2 = "C", text2 = "c"
    )
    attr(out, "pairing_diagnostics") <- tibble::tibble(round = 1L, n_selected = 1L)
    out
  }

mock_stop_metrics <- function(...) {
  # A stable sequence so the second round increments stability_streak.
  tibble::tibble(
    engine = "bt",
    n_items = 3L,
    n_total_items = 3L,
    theta_sd = NA_real_,
    se_mean = NA_real_,
    se_max = NA_real_,
    rel_se_mean = NA_real_,
    rel_se_p90 = NA_real_,
    reliability = NA_real_,
    sepG = NA_real_,
    item_misfit_prop = NA_real_,
    judge_misfit_prop = NA_real_,
    n_matched = NA_integer_,
    rms_theta_delta = 0,
    topk_overlap = 1,
    rank_corr = NA_real_
  )
}

  mock_should_stop <- function(...) {
    list(stop = FALSE, reason = "not_stopping", details = list())
  }

  with_ns_mocks(
    asNamespace("pairwiseLLM"),
    select_adaptive_pairs = mock_select_pairs,
    bt_stop_metrics = mock_stop_metrics,
    bt_should_stop = mock_should_stop,
    code = {
      out <- pairwiseLLM::bt_run_adaptive(
        samples,
        initial_results = initial_results,
        judge_fun = judge_fun,
        fit_fun = fit_fun,
        max_rounds = 2L,
        init_round_size = 0L,
        round_size = 1L,
        stop_min_degree = 0,
        stop_min_largest_component_frac = 0,
        stop_stability_rounds = 99L,
        stop_stability_rms = 0,
        stop_topk_overlap = 1,
        final_refit = FALSE,
        repeat_policy = "allow"
      )

      expect_equal(nrow(out$rounds), 2)
      expect_equal(out$rounds$stability_streak[2], 1L)
      expect_true(all(is.finite(out$rounds$degree_min)))
      expect_true(all(is.finite(out$rounds$largest_component_frac)))
    }
  )
})


test_that("5470-03 bt_run_adaptive emits n_pairs_planned even when pairing diagnostics omit n_selected", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("a", "b", "c")
  )

  initial_results <- tibble::tibble(ID1 = "A", ID2 = "B", better_id = "A")

  judge_fun <- function(pairs, ...) {
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  }

fit_fun <- function(bt_data, ...) {
  ids <- unique(c(bt_data$object1, bt_data$object2))
  tibble_theta <- tibble::tibble(
    ID = ids,
    theta = rep(0, length(ids)),
    se = rep(0.2, length(ids))
  )
  list(theta = tibble_theta, diagnostics = list())
}

  mock_select_pairs <- function(samples, results, round_size, ...) {
    out <- tibble::tibble(
      ID1 = "A", text1 = "a",
      ID2 = "C", text2 = "c"
    )
    # Intentionally omit `n_selected`.
    attr(out, "pairing_diagnostics") <- tibble::tibble(round = 1L, n_candidates = 1L)
    out
  }

  with_ns_mocks(
    asNamespace("pairwiseLLM"),
    select_adaptive_pairs = mock_select_pairs,
    bt_should_stop = function(...) list(stop = FALSE, reason = "not_stopping", details = list()),
    code = {
      out <- pairwiseLLM::bt_run_adaptive(
        samples,
        initial_results = initial_results,
        judge_fun = judge_fun,
        fit_fun = fit_fun,
        max_rounds = 1L,
        init_round_size = 0L,
        round_size = 1L,
        final_refit = FALSE,
        repeat_policy = "allow"
      )

      expect_s3_class(out, "pairwiseLLM_run")
      expect_true(is.data.frame(out$pairing_diagnostics))
      expect_true("n_pairs_planned" %in% names(out$pairing_diagnostics))
      expect_true(all(is.na(out$pairing_diagnostics$n_pairs_planned)))
    }
  )
})


test_that("5470-04 bt_run_adaptive sets fit_provenance to list when final_refit returns NULL provenance", {
  samples <- tibble::tibble(ID = c("a", "b"), text = c("A", "B"))
  initial_results <- tibble::tibble(ID1 = "a", ID2 = "b", better_id = "a")

  mock_final <- function(...) {
    estimates <- pairwiseLLM:::.make_estimates_tbl(
      ids = c("a", "b"),
      theta_rc = c(NA_real_, NA_real_),
      rank_rc = c(NA_integer_, NA_integer_),
      pi_rc = c(NA_real_, NA_real_),
      theta_bt_firth = c(0.1, -0.1),
      se_bt_firth = c(0.2, 0.2),
      rank_bt_firth = c(1L, 2L),
      bt_engine_requested = c("bt_firth", "bt_firth"),
      bt_engine_used = c("bt_firth", "bt_firth"),
      bt_status = c("succeeded", "succeeded"),
      bt_failure_reason = c(NA_character_, NA_character_)
    )
    list(
      estimates = estimates,
      bt_fit = NULL,
      rc_fit = NULL,
      diagnostics = list(bt_status = "succeeded", bt_engine_used = "bt_firth"),
      provenance = NULL
    )
  }

  with_ns_mocks(
    asNamespace("pairwiseLLM"),
    compute_final_estimates = mock_final,
    code = {
      out <- pairwiseLLM::bt_run_adaptive(
        samples,
        initial_results = initial_results,
        judge_fun = function(pairs, ...) tibble::tibble(),
        fit_fun = function(bt_data, ...) list(theta = tibble::tibble(ID = c("a", "b"), theta = c(0, 0), se = c(0.2, 0.2))),
        max_rounds = 0L,
        round_size = 0L,
        init_round_size = 0L,
        final_refit = TRUE
      )

      expect_equal(out$theta_engine, "bt_firth")
      expect_type(out$fit_provenance, "list")
    }
  )
})


test_that("5470-05 bt_run_adaptive fills fit_provenance when final_refit yields no theta and fallback is used", {
  samples <- tibble::tibble(ID = c("a", "b"), text = c("A", "B"))
  initial_results <- tibble::tibble(ID1 = "a", ID2 = "b", better_id = "a")

  fit_fun <- function(bt_data, ...) {
    list(theta = tibble::tibble(ID = c("a", "b"), theta = c(0.25, -0.25), se = c(0.2, 0.2)), diagnostics = list())
  }

  mock_final <- function(...) {
    empty_ids <- character(0)
    list(
      estimates = pairwiseLLM:::.make_estimates_tbl(
        ids = empty_ids,
        theta_rc = numeric(0),
        rank_rc = integer(0),
        pi_rc = numeric(0),
        theta_bt_firth = numeric(0),
        se_bt_firth = numeric(0),
        rank_bt_firth = integer(0),
        bt_engine_requested = character(0),
        bt_engine_used = character(0),
        bt_status = character(0),
        bt_failure_reason = character(0)
      ),
      bt_fit = NULL,
      rc_fit = NULL,
      diagnostics = list(),
      provenance = NULL
    )
  }

  with_ns_mocks(
    asNamespace("pairwiseLLM"),
    compute_final_estimates = mock_final,
    code = {
      out <- pairwiseLLM::bt_run_adaptive(
        samples,
        initial_results = initial_results,
        judge_fun = function(pairs, ...) tibble::tibble(),
        fit_fun = fit_fun,
        max_rounds = 1L,
        round_size = 0L,
        init_round_size = 0L,
        final_refit = TRUE
      )

      expect_type(out$fit_provenance, "list")
      expect_true(isTRUE(out$fit_provenance$fallback_used))
      expect_true(is.data.frame(out$theta))
      expect_equal(nrow(out$theta), 2)
    }
  )
})
