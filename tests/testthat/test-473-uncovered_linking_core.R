# Focused coverage tests for files touched in this thread:
# - R/bt_linking.R
# - R/core_set.R
# - R/bt_stopping.R
# - R/bt_run_adaptive_core_linking.R

test_that("bt_link_thetas covers named-numeric path, core-id intersection, and validation errors", {
  ref <- tibble::tibble(ID = c("A", "B", "C"), theta = c(0, 1, 2), se = c(0.1, 0.1, 0.1))

  # Named numeric vector path (bt_linking.R:66) + ids=NULL intersection path (line ~97)
  # Also forces b fallback branch (sd_cur ~ 0 -> b=1; line ~119)
  cur_named <- c(A = 1, B = 1, C = 1)
  lk <- bt_link_thetas(cur_named, ref, min_n = 3L)
  expect_equal(lk$n_core, 3L)
  expect_equal(lk$b, 1)
  expect_true(all(is.na(lk$theta$se)))

  # Invalid current type -> error message branch (line ~71-74)
  expect_error(
    bt_link_thetas(current = list(x = 1), reference = ref),
    "`current` must be a fit.*named numeric vector"
  )

  # ids must be character (line ~85)
  expect_error(
    bt_link_thetas(current = ref, reference = ref, ids = 1:3),
    "`ids` must be a character"
  )

  # min_n must be >= 2 (line ~89)
  expect_error(
    bt_link_thetas(current = ref, reference = ref, min_n = 1),
    "`min_n` must be a single integer >= 2"
  )
})

test_that("bt_link_thetas covers a=0 fallback when core means are not finite", {
  cur <- tibble::tibble(ID = c("A", "B", "C"), theta = c(0, 1, 2), se = c(0.2, 0.2, 0.2))
  ref_all_na <- tibble::tibble(
    ID = c("A", "B", "C"),
    theta = c(NA_real_, NA_real_, NA_real_),
    se = c(0.1, 0.1, 0.1)
  )

  # mu_ref becomes NaN (not finite) -> a fallback to 0 (line ~124)
  lk <- bt_link_thetas(cur, ref_all_na, min_n = 2L)
  expect_equal(lk$a, 0)
  # b also falls back to 1 because sd_ref is not finite
  expect_equal(lk$b, 1)
})

test_that(".bt_should_apply_linking returns FALSE for NULL or non-1-row drift inputs", {
  expect_false(pairwiseLLM:::.bt_should_apply_linking(NULL))

  drift2 <- tibble::tibble(
    core_theta_cor = c(0.9, 0.9),
    core_p90_abs_shift = c(0.1, 0.1),
    core_max_abs_shift = c(0.1, 0.1)
  )
  expect_false(pairwiseLLM:::.bt_should_apply_linking(drift2))
})

test_that("bt_stop_metrics covers drift input validation, ids validation, and all-NA SE summaries", {
  fit <- list(
    engine = "mock",
    theta = tibble::tibble(ID = c("A", "B"), theta = c(0, 1), se = c(NA_real_, NA_real_))
  )

  # Provide only prev_fit (or only core_ids) -> error (line ~88)
  # `prev_fit` can be used without `core_ids` to compute stability metrics.
  expect_no_error(bt_stop_metrics(fit, prev_fit = fit, core_ids = NULL))

  # Providing `core_ids` without `prev_fit` is still invalid (drift metrics require both).
  expect_error(
    bt_stop_metrics(fit, prev_fit = NULL, core_ids = c("A")),
    "requires `prev_fit`"
  )

  # ids must be character (line ~94)
  expect_error(
    bt_stop_metrics(fit, ids = 1),
    "`ids` must be a character"
  )

  # ids must exist in fit$theta$ID (line ~99)
  expect_error(
    bt_stop_metrics(fit, ids = "Z"),
    "All `ids` must be present"
  )

  # all-NA se -> safe_mean/safe_max/safe_q NA branches (lines ~132/~138/~144)
  m <- bt_stop_metrics(fit)
  expect_true(is.na(m$se_mean))
  expect_true(is.na(m$se_max))
  expect_true(is.na(m$se_p50))
  expect_true(is.na(m$se_p90))
  expect_true(is.na(m$se_p95))
})

test_that("bt_should_stop errors when prev_metrics lacks rel_se_p90", {
  m <- tibble::tibble(
    reliability = 0.90,
    sepG = 3.0,
    rel_se_p90 = 0.30,
    item_misfit_prop = 0,
    judge_misfit_prop = 0
  )
  prev_bad <- tibble::tibble(reliability = 0.90, sepG = 3.0)

  expect_error(
    bt_should_stop(m, prev_metrics = prev_bad, rel_se_p90_min_improve = 0.01),
    "prev_metrics.*rel_se_p90"
  )
})

test_that("select_core_set validates clara_* args regardless of method", {
  samples <- tibble::tibble(ID = c("A", "B"), text = c("a", "b"))

  expect_error(
    select_core_set(samples, core_size = 2, method = "random", clara_threshold = 1),
    "`clara_threshold` must be a single integer >= 2"
  )
  expect_error(
    select_core_set(samples, core_size = 2, method = "random", clara_samples = 0),
    "`clara_samples` must be a single integer >= 1"
  )
  expect_error(
    select_core_set(samples, core_size = 2, method = "random", clara_sampsize = 1),
    "`clara_sampsize` must be NULL or a single integer >= 2"
  )
})

test_that("internal embedding medoid selectors cover k bounds and PAM/CLARA/auto paths", {
  set.seed(1)
  emb <- matrix(rnorm(6 * 2), nrow = 6, ncol = 2)

  # k out of range (core_set.R)
  expect_error(
    pairwiseLLM:::.select_medoids_from_embeddings(emb, k = 1, method = "pam"),
    "`k` must be between 2"
  )

  # NOTE:
  # In this package version, 'cluster' is in Imports, so it is loaded when
  # pairwiseLLM is loaded. That means the "cluster missing" error branch:
  #   if (!requireNamespace("cluster", quietly=TRUE)) stop(...)
  # is not reachable in CI tests, and should not be asserted here.

  testthat::skip_if_not_installed("cluster")

  # auto -> PAM path (n <= threshold)
  out_auto_pam <- pairwiseLLM:::.select_medoids_from_embeddings(
    emb, k = 2, method = "auto", clara_threshold = 10L
  )
  expect_equal(length(out_auto_pam$medoids), 2L)
  expect_true(all(out_auto_pam$medoids %in% seq_len(nrow(emb))))

  # auto -> CLARA path (n > threshold)
  out_auto_clara <- pairwiseLLM:::.select_medoids_from_embeddings(
    emb, k = 2, method = "auto", clara_threshold = 5L, clara_samples = 2L
  )
  expect_equal(length(out_auto_clara$medoids), 2L)
  expect_true(all(out_auto_clara$medoids %in% seq_len(nrow(emb))))

  # explicit PAM path
  out_pam <- pairwiseLLM:::.select_medoids_from_embeddings(emb, k = 2, method = "pam")
  expect_equal(length(out_pam$medoids), 2L)

  # explicit CLARA path + sampsize heuristic branch
  out_clara1 <- pairwiseLLM:::.select_medoids_clara(emb, k = 2, samples = 2L, sampsize = NULL)
  expect_equal(length(out_clara1$medoids), 2L)

  # explicit CLARA path + explicit sampsize
  out_clara2 <- pairwiseLLM:::.select_medoids_clara(emb, k = 2, samples = 2L, sampsize = 5L)
  expect_equal(length(out_clara2$medoids), 2L)
})

test_that("bt_run_adaptive_core_linking batch validation covers type and empty-batch branches", {
  samples <- tibble::tibble(ID = c("A", "B"), text = c("a", "b"))
  judge_fun <- function(pairs) tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)

  # batches wrong type (not list, not character)
  expect_error(
    bt_run_adaptive_core_linking(
      samples = samples,
      batches = 1L,
      judge_fun = judge_fun,
      core_ids = c("A", "B"),
      init_round_size = 1,
      max_rounds_per_batch = 0,
      rel_se_p90_target = NA_real_,
      reliability_target = NA_real_,
      sepG_target = NA_real_,
      rel_se_p90_min_improve = NA_real_,
      max_item_misfit_prop = NA_real_,
      max_judge_misfit_prop = NA_real_
    ),
    "`batches` must be a non-empty list"
  )

  # empty batch element
  expect_error(
    bt_run_adaptive_core_linking(
      samples = samples,
      batches = list(character(0)),
      judge_fun = judge_fun,
      core_ids = c("A", "B"),
      init_round_size = 1,
      max_rounds_per_batch = 0,
      rel_se_p90_target = NA_real_,
      reliability_target = NA_real_,
      sepG_target = NA_real_,
      rel_se_p90_min_improve = NA_real_,
      max_item_misfit_prop = NA_real_,
      max_judge_misfit_prop = NA_real_
    ),
    "Each batch.*at least 1 ID"
  )

  # batch IDs must exist in samples
  expect_error(
    bt_run_adaptive_core_linking(
      samples = samples,
      batches = list("Z"),
      judge_fun = judge_fun,
      core_ids = c("A", "B"),
      init_round_size = 1,
      max_rounds_per_batch = 0,
      rel_se_p90_target = NA_real_,
      reliability_target = NA_real_,
      sepG_target = NA_real_,
      rel_se_p90_min_improve = NA_real_,
      max_item_misfit_prop = NA_real_,
      max_judge_misfit_prop = NA_real_
    ),
    "All batch IDs must be present"
  )
})

test_that("bt_run_adaptive_core_linking: linking never/always/auto affect batch fit as expected", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("a", "b", "c", "d")
  )
  core_ids <- c("A", "B", "C")
  batches <- list(c("D"))

  judge_fun <- function(pairs) {
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  }

  # Fit fun ignores bt_data (avoids ID1/ID2 vs object1/object2 issues).
  # It returns a *different core ordering* after the first call to create real drift:
  # - bootstrap: A=1, B=2, C=3
  # - batch:     A=1, B=3, C=2  (swap B/C)
  make_fit_fun <- function() {
    i <- 0L
    function(bt_data, ...) {
      i <<- i + 1L
      ids <- samples$ID

      theta <- if (i == 1L) {
        c(A = 1, B = 2, C = 3, D = 4)
      } else {
        c(A = 1, B = 3, C = 2, D = 4)
      }

      list(
        engine = "mock",
        reliability = NA_real_,
        theta = tibble::tibble(ID = names(theta), theta = as.numeric(theta), se = rep(0.2, length(theta))),
        diagnostics = list(sepG = NA_real_)
      )
    }
  }

  common_args <- list(
    samples = samples,
    batches = batches,
    judge_fun = judge_fun,
    core_ids = core_ids,
    engine = "mock",
    init_round_size = 2,
    round_size = 2,
    max_rounds_per_batch = 1,
    forbid_repeats = FALSE,
    # disable other stopping thresholds so we always do the one batch round
    rel_se_p90_target = NA_real_,
    reliability_target = NA_real_,
    sepG_target = NA_real_,
    rel_se_p90_min_improve = NA_real_,
    max_item_misfit_prop = NA_real_,
    max_judge_misfit_prop = NA_real_
  )

  # linking = never => no theta_linked column on batch1 fit
  out_never <- do.call(
    bt_run_adaptive_core_linking,
    c(common_args, list(
      fit_fun = make_fit_fun(),
      linking = "never"
    ))
  )
  expect_identical(out_never$final_fits$batch1$linking$reason, "never")
  expect_false("theta_linked" %in% names(out_never$final_fits$batch1$theta))

  # linking = always => theta_linked present on batch1 fit
  out_always <- do.call(
    bt_run_adaptive_core_linking,
    c(common_args, list(
      fit_fun = make_fit_fun(),
      linking = "always"
    ))
  )
  expect_identical(out_always$final_fits$batch1$linking$reason, "always")
  expect_true("theta_linked" %in% names(out_always$final_fits$batch1$theta))
  expect_true("se_linked" %in% names(out_always$final_fits$batch1$theta))

  # linking = auto should trigger because drift exists (B/C swapped)
  out_auto <- do.call(
    bt_run_adaptive_core_linking,
    c(common_args, list(
      fit_fun = make_fit_fun(),
      linking = "auto",
      linking_min_n = 2, # allow linking with 3 core IDs
      # Make trigger easy: any slight cor drop will trip it
      linking_cor_target = 0.9999,
      linking_p90_abs_shift_target = 0.0,
      linking_max_abs_shift_target = 0.0
    ))
  )
  expect_identical(out_auto$final_fits$batch1$linking$reason, "auto_trigger")
  expect_true("theta_linked" %in% names(out_auto$final_fits$batch1$theta))
})
