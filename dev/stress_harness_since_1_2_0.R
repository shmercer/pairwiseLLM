# Stress harness for pairwiseLLM workflows added/changed since v1.2.0
#
# This script is intended for interactive/manual QA prior to running live LLM/API jobs.
#
# Usage (after installing the package):
#   source(system.file("dev/stress_harness_since_1_2_0.R", package = "pairwiseLLM"))
#
# Notes:
# - Uses simulated/mock judges by default (no network calls).
# - Attempts "real" BT fitting via fit_bt_model(engine="auto") when suggested
#   packages are installed; otherwise falls back to a deterministic mock fit.
# - Prints a compact PASS/FAIL/SKIP summary at the end.

suppressPackageStartupMessages({
  library(pairwiseLLM)
  library(tibble)
  library(dplyr)
})

options(warn = 1)

# -------------------------------------------------------------------------
# Harness helpers
# -------------------------------------------------------------------------

.now <- function() as.numeric(proc.time()[["elapsed"]])

.h_msg <- function(...) cat(..., "\n", sep = "")

.h_div <- function(title = NULL) {
  if (!is.null(title)) {
    .h_msg("\n", strrep("=", 78))
    .h_msg("== ", title)
    .h_msg(strrep("=", 78))
  } else {
    .h_msg("\n", strrep("-", 78))
  }
}

.h_fmt_sec <- function(x) sprintf("%.2fs", x)

.h_status <- function(x) {
  x <- toupper(x)
  if (x %in% c("PASS", "FAIL", "SKIP")) x else "UNKNOWN"
}

.h_summary <- tibble::tibble(
  case = character(),
  status = character(),
  seconds = numeric(),
  details = character()
)

.h_add <- function(case, status, seconds, details = "") {
  .h_summary <<- dplyr::bind_rows(
    .h_summary,
    tibble::tibble(case = case, status = .h_status(status), seconds = seconds, details = details)
  )
  invisible(NULL)
}

.h_expect <- function(ok, msg = "Expectation failed.") {
  if (!isTRUE(ok)) stop(msg, call. = FALSE)
  invisible(TRUE)
}

.h_expect_named <- function(x, nms, msg = NULL) {
  missing <- setdiff(nms, names(x))
  if (length(missing) > 0L) {
    if (is.null(msg)) msg <- paste0("Missing names: ", paste(missing, collapse = ", "))
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

.h_run <- function(case, expr, validate = NULL, skip_if = NULL, skip_msg = NULL) {
  t0 <- .now()
  if (is.function(skip_if) && isTRUE(skip_if())) {
    .h_add(case, "SKIP", .now() - t0, ifelse(is.null(skip_msg), "Skipped.", skip_msg))
    return(invisible(NULL))
  }
  out <- NULL
  err <- NULL
  tryCatch({
    out <- force(expr)
    if (is.function(validate)) validate(out)
  }, error = function(e) {
    err <<- e
  })
  dt <- .now() - t0
  if (is.null(err)) {
    .h_add(case, "PASS", dt, "")
    return(invisible(out))
  } else {
    .h_add(case, "FAIL", dt, conditionMessage(err))
    return(invisible(NULL))
  }
}

# -------------------------------------------------------------------------
# Capability checks (optional)
# -------------------------------------------------------------------------

.has_pkg <- function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
}

.can_real_fit <- function() {
  # fit_bt_model(engine="auto") will use sirt or BradleyTerry2 if installed.
  .has_pkg("sirt") || .has_pkg("BradleyTerry2")
}

# -------------------------------------------------------------------------
# Fixtures
# -------------------------------------------------------------------------

.make_samples <- function(n = 24, prefix = "S") {
  ids <- paste0(prefix, sprintf("%03d", seq_len(n)))
  tibble::tibble(
    ID = ids,
    text = paste0("text_", ids, " ", replicate(n, paste(rep("word", 20), collapse = " "), simplify = TRUE))
  )
}

# Small default fixture (keeps runs fast)
samples_24 <- tibble::tibble(ID = LETTERS[1:24], text = paste0("text_", LETTERS[1:24]))
true_theta_24 <- stats::setNames(seq(2, -3.0, length.out = nrow(samples_24)), samples_24$ID)

# Larger fixture for scaling / clustering / repeat exhaustion
samples_80 <- .make_samples(80, prefix = "I")
true_theta_80 <- stats::setNames(seq(2, -3.0, length.out = nrow(samples_80)), samples_80$ID)

# Embeddings fixture (purely synthetic, used for core selection workflows)
set.seed(1)
.emb_24 <- matrix(rnorm(nrow(samples_24) * 8), nrow = nrow(samples_24), ncol = 8)
rownames(.emb_24) <- samples_24$ID

.emb_80 <- matrix(rnorm(nrow(samples_80) * 16), nrow = nrow(samples_80), ncol = 16)
rownames(.emb_80) <- samples_80$ID

# -------------------------------------------------------------------------
# Judge factories
# -------------------------------------------------------------------------

judge_deterministic <- function(true_theta, seed = 1) {
  function(pairs) {
    simulate_bt_judge(pairs, true_theta = true_theta, deterministic = TRUE, seed = seed)
  }
}

judge_noisy <- function(true_theta, seed = 1) {
  function(pairs) {
    simulate_bt_judge(pairs, true_theta = true_theta, deterministic = FALSE, seed = seed)
  }
}

judge_missing <- function(true_theta, p_missing = 0.10, seed = 1) {
  function(pairs) {
    res <- simulate_bt_judge(pairs, true_theta = true_theta, deterministic = FALSE, seed = seed)
    set.seed(seed)
    k <- ceiling(p_missing * nrow(res))
    if (k > 0L) {
      idx <- sample.int(nrow(res), k)
      res$better_id[idx] <- NA_character_
    }
    res
  }
}

judge_invalid <- function(true_theta, p_bad = 0.05, seed = 1) {
  function(pairs) {
    res <- simulate_bt_judge(pairs, true_theta = true_theta, deterministic = FALSE, seed = seed)
    set.seed(seed)
    k <- ceiling(p_bad * nrow(res))
    if (k > 0L) {
      idx <- sample.int(nrow(res), k)
      res$better_id[idx] <- "NOT_AN_ID"
    }
    res
  }
}

judge_flaky_stop <- function(true_theta, every = 5L, seed = 1) {
  i <- 0L
  function(pairs) {
    i <<- i + 1L
    if (i %% every == 0L) stop("transient judge error", call. = FALSE)
    simulate_bt_judge(pairs, true_theta = true_theta, deterministic = FALSE, seed = seed)
  }
}

judge_two_judges <- function(true_theta_a, true_theta_b, seed_a = 1, seed_b = 2) {
  function(pairs) {
    rbind(
      mutate(simulate_bt_judge(pairs, true_theta = true_theta_a, deterministic = FALSE, seed = seed_a), judge = "A"),
      mutate(simulate_bt_judge(pairs, true_theta = true_theta_b, deterministic = FALSE, seed = seed_b), judge = "B")
    )
  }
}

# -------------------------------------------------------------------------
# Fit functions
# -------------------------------------------------------------------------

fit_mock <- function(bt_data, ...) {
  bt_data <- as.data.frame(bt_data)
  # bt_data can be 3 cols (o1,o2,winner) or 4 cols (+judge)
  o1 <- bt_data[[1]]
  o2 <- bt_data[[2]]
  ids <- sort(unique(c(o1, o2)))
  list(
    engine = "mock",
    reliability = NA_real_,
    theta = tibble::tibble(ID = ids, theta = seq_along(ids), se = rep(1, length(ids))),
    diagnostics = list(sepG = NA_real_)
  )
}

make_fit_mock_drifting <- function() {
  local({
    .i <- 0L
    function(bt_data, ...) {
      .i <<- .i + 1L
      bt_data <- as.data.frame(bt_data)
      o1 <- bt_data[[1]]
      o2 <- bt_data[[2]]
      ids <- sort(unique(c(o1, o2)))
      base <- seq_along(ids)

      # Baseline fit uses scale=1, offset=0; subsequent fits drift.
      scale <- if (.i == 1L) 1 else 1.8
      offset <- if (.i == 1L) 0 else 7

      list(
        engine = "mock_drifting",
        reliability = NA_real_,
        theta = tibble::tibble(ID = ids, theta = base * scale + offset, se = rep(1, length(ids))),
        diagnostics = list(sepG = NA_real_)
      )
    }
  })
}



fit_real_or_mock <- function(bt_data, engine = "auto", verbose = FALSE, return_diagnostics = TRUE, ...) {
  if (.can_real_fit()) {
    fit_bt_model(bt_data, engine = engine, verbose = verbose, return_diagnostics = return_diagnostics, ...)
  } else {
    fit_mock(bt_data, ...)
  }
}

# -------------------------------------------------------------------------
# Begin harness
# -------------------------------------------------------------------------

.h_div("pairwiseLLM stress harness (since v1.2.0)")
.h_msg("R: ", R.version.string)
.h_msg("pairwiseLLM: ", as.character(utils::packageVersion("pairwiseLLM")))
.h_msg("Real BT fit available: ", ifelse(.can_real_fit(), "YES", "NO (using mock fit)"))

# -------------------------------------------------------------------------
# 0) Project environment helpers
# -------------------------------------------------------------------------

.h_div("Project environment helpers")

.h_run(
  "project env: print project .Renviron instructions (no file writing)",
  {
    tmp <- withr::local_tempdir()
    f <- file.path(tmp, ".Renviron")
    entry1 <- suppressMessages(set_project_reticulate_python("/tmp/python", file = f, overwrite = TRUE))
    entry2 <- suppressMessages(set_project_reticulate_python("/tmp/other", file = f, overwrite = TRUE))
    entry3 <- suppressMessages(set_project_embeddings_cache_dir("./.cache/pairwiseLLM", file = f, overwrite = TRUE))
    list(entry1 = entry1, entry2 = entry2, entry3 = entry3, file = f, file_exists = file.exists(f))
  },
  validate = function(out) {
    .h_expect(identical(out$entry1, 'RETICULATE_PYTHON="/tmp/python"'))
    .h_expect(identical(out$entry2, 'RETICULATE_PYTHON="/tmp/other"'))
    .h_expect(identical(out$entry3, 'PAIRWISELLM_EMBEDDINGS_CACHE_DIR="./.cache/pairwiseLLM"'))
    .h_expect(!isTRUE(out$file_exists), "Expected no file writing; .Renviron should not be created.")
  }
)

# -------------------------------------------------------------------------
# 1) Core-set selection utilities
# -------------------------------------------------------------------------

.h_div("Core-set selection utilities")

.h_run(
  "select_core_set: random (seeded) is stable",
  {
    s1 <- select_core_set(samples_24, core_size = 6, method = "random", seed = 1)
    s2 <- select_core_set(samples_24, core_size = 6, method = "random", seed = 1)
    list(s1 = s1, s2 = s2)
  },
  validate = function(out) {
    .h_expect(is.data.frame(out$s1))
    .h_expect(is.data.frame(out$s2))
    .h_expect(nrow(out$s1) == 6L)
    .h_expect(nrow(out$s2) == 6L)
    .h_expect(identical(out$s1$ID, out$s2$ID), "Random core_set IDs should be deterministic with the same seed.")
  }
)

.h_run(
  "select_core_set: token_stratified returns requested size",
  {
    select_core_set(samples_24, core_size = 8, method = "token_stratified")
  },
  validate = function(out) {
    .h_expect(is.data.frame(out))
    .h_expect(nrow(out) == 8L)
    .h_expect(length(unique(out$ID)) == 8L)
  }
)

.h_run(
  "select_core_set: embeddings (pam/auto) returns requested size",
  {
    select_core_set(samples_24, core_size = 7, method = "auto", embeddings = .emb_24, seed = 1)
  },
  validate = function(out) {
    .h_expect(is.data.frame(out))
    .h_expect(nrow(out) == 7L)
    .h_expect(length(unique(out$ID)) == 7L)
  }
)

.h_run(
  "select_core_set: clara path (larger n) returns requested size",
  {
    # force CLARA by lowering threshold
    select_core_set(samples_80, core_size = 12, method = "auto", embeddings = .emb_80, seed = 1, clara_threshold = 20L)
  },
  validate = function(out) {
    .h_expect(is.data.frame(out))
    .h_expect(nrow(out) == 12L)
    .h_expect(length(unique(out$ID)) == 12L)
  }
)

# -------------------------------------------------------------------------
# 1) bt_run_adaptive
# -------------------------------------------------------------------------

.h_div("bt_run_adaptive")

judge_ok_24 <- judge_deterministic(true_theta_24, seed = 1)

.h_run(
  "adaptive: end-to-end + checkpointing (max_rounds stop)",
  {
    tmp <- tempfile("pairwiseLLM_chk_")
    dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
    out <- bt_run_adaptive(
      samples = samples_24,
      judge_fun = judge_ok_24,
      fit_fun = fit_mock,
      engine = "mock",
      round_size = 20,
      init_round_size = 20,
      max_rounds = 3,
      reliability_target = Inf,
      checkpoint_dir = tmp,
      checkpoint_store_fits = FALSE,
      seed_pairs = 1
    )
    list(out = out, tmp = tmp, files = list.files(tmp))
  },
  validate = function(x) {
    out <- x$out
    .h_expect_named(out, c("results", "rounds", "state", "stop_reason"))
    .h_expect(identical(out$stop_reason, "max_rounds"))
    .h_expect(file.exists(file.path(x$tmp, "run_state.rds")))
    .h_expect(any(grepl("run_state_round_001", x$files)))
  }
)

.h_run(
  "adaptive: resume completed run is idempotent",
  {
    tmp <- tempfile("pairwiseLLM_chk_")
    dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
    out1 <- bt_run_adaptive(
      samples = samples_24,
      judge_fun = judge_ok_24,
      fit_fun = fit_mock,
      engine = "mock",
      round_size = 10,
      init_round_size = 10,
      max_rounds = 2,
      reliability_target = Inf,
      checkpoint_dir = tmp,
      checkpoint_store_fits = FALSE,
      seed_pairs = 1
    )
    out2 <- bt_run_adaptive(
      samples = samples_24,
      judge_fun = judge_ok_24,
      fit_fun = fit_mock,
      engine = "mock",
      round_size = 10,
      init_round_size = 10,
      max_rounds = 2,
      reliability_target = Inf,
      resume_from = tmp,
      checkpoint_dir = tmp,
      checkpoint_store_fits = FALSE,
      seed_pairs = 1
    )
    list(out1 = out1, out2 = out2)
  },
  validate = function(x) {
    .h_expect(identical(x$out1$stop_reason, x$out2$stop_reason))
    .h_expect(nrow(x$out1$results) == nrow(x$out2$results))
  }
)

.h_run(
  "adaptive: resume mismatch message uses Requested/Checkpoint",
  {
    tmp <- tempfile("pairwiseLLM_chk_")
    dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
    bt_run_adaptive(
      samples = samples_24,
      judge_fun = judge_ok_24,
      fit_fun = fit_mock,
      engine = "mock",
      round_size = 10,
      init_round_size = 10,
      max_rounds = 1,
      reliability_target = Inf,
      checkpoint_dir = tmp,
      checkpoint_store_fits = FALSE,
      seed_pairs = 1
    )
    samples_other <- tibble::tibble(ID = c("X", "Y", "Z"), text = c("x", "y", "z"))
    tryCatch(
      bt_run_adaptive(
        samples = samples_other,
        judge_fun = judge_ok_24,
        fit_fun = fit_mock,
        engine = "mock",
        max_rounds = 1,
        resume_from = tmp
      ),
      error = function(e) conditionMessage(e)
    )
  },
  validate = function(msg) {
    .h_expect(grepl("Requested:", msg, fixed = TRUE))
    .h_expect(grepl("Checkpoint:", msg, fixed = TRUE))
    .h_expect(grepl("samples\\$ID", msg))
  }
)

.h_run(
  "adaptive: reverse_audit adds reversed duplicates (unordered pair repeats)",
  {
    out <- bt_run_adaptive(
      samples = samples_24,
      judge_fun = judge_ok_24,
      fit_fun = fit_mock,
      engine = "mock",
      round_size = 30,
      init_round_size = 30,
      max_rounds = 2,
      reliability_target = Inf,
      seed_pairs = 1,
      reverse_audit = TRUE,
      reverse_pct = 0.50,
      reverse_seed = 1
    )
    out
  },
  validate = function(out) {
    .h_expect(is.list(out$reverse_audit), "Expected out$reverse_audit to be a list when reverse_audit=TRUE.")
    rev_pairs <- out$reverse_audit$pairs_reversed
    .h_expect(is.data.frame(rev_pairs))
    .h_expect(nrow(rev_pairs) > 0L, "Expected at least one reversed pair when reverse_audit=TRUE.")

    res <- as.data.frame(out$results)
    res <- res[!is.na(res$better_id), , drop = FALSE]
    main_keys <- paste(res$ID1, res$ID2, sep = "||")
    # reversed pairs should correspond to an existing main pair with order swapped
    rev_swapped_keys <- paste(rev_pairs$ID2, rev_pairs$ID1, sep = "||")
    hit_n <- sum(rev_swapped_keys %in% main_keys)
    .h_expect(hit_n > 0L, "Expected at least some reversed pairs to match existing main pairs with swapped order.")
  }
)

.h_run(
  "adaptive: round_size=0 stops with round_size_zero",
  {
    bt_run_adaptive(
      samples = samples_24,
      judge_fun = judge_ok_24,
      fit_fun = fit_mock,
      engine = "mock",
      round_size = 0,
      init_round_size = 0,
      max_rounds = 10,
      reliability_target = Inf,
      seed_pairs = 1
    )
  },
  validate = function(out) .h_expect(identical(out$stop_reason, "round_size_zero"))
)

.h_run(
  "adaptive: forbid_repeats exhaustion yields clean stop (tiny n, big demand)",
  {
    s8 <- tibble::tibble(ID = LETTERS[1:8], text = paste0("t_", LETTERS[1:8]))
    th8 <- stats::setNames(seq(1, -1, length.out = nrow(s8)), s8$ID)
    out <- bt_run_adaptive(
      samples = s8,
      judge_fun = judge_deterministic(th8, seed = 1),
      fit_fun = fit_mock,
      engine = "mock",
      round_size = 200,      # exceeds unique pairs
      init_round_size = 200,
      max_rounds = 5,
      reliability_target = Inf,
      forbid_repeats = TRUE,
      balance_positions = TRUE,
      seed_pairs = 1
    )
    out
  },
  validate = function(out) {
    .h_expect(out$stop_reason %in% c("no_pairs", "no_new_results", "max_rounds"),
              paste0("Unexpected stop_reason: ", out$stop_reason))
  }
)

# -------------------------------------------------------------------------
# 2) bt_run_core_linking
# -------------------------------------------------------------------------

.h_div("bt_run_core_linking")

batches_24 <- list(LETTERS[6:12], LETTERS[13:18])
core_ids_24 <- LETTERS[1:5]

.h_run(
  "core_linking: end-to-end (never linking) + stop_reason",
  {
    out <- bt_run_core_linking(
      samples = samples_24,
      batches = batches_24,
      core_ids = core_ids_24,
      linking = "never",
      judge_fun = judge_ok_24,
      fit_fun = fit_mock,
      engine = "mock",
      round_size = 12,
      max_rounds_per_batch = 2,
      within_batch_frac = 1,
      core_audit_frac = 0,
      reliability_target = Inf,
      checkpoint_store_fits = FALSE,
      seed_pairs = 1,
      verbose = TRUE
    )
    out
  },
  validate = function(out) {
    .h_expect_named(out, c("batch_summary", "stop_reason", "results"))
    .h_expect(is.character(out$stop_reason) && length(out$stop_reason) == 1L)
    .h_expect(nrow(out$batch_summary) == 2L)
  }
)

.h_run(
  "core_linking: resume mismatch (core_ids) includes Requested/Checkpoint",
  {
    tmp <- tempfile("pairwiseLLM_chk_")
    dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
    bt_run_core_linking(
      samples = samples_24,
      batches = batches_24,
      core_ids = core_ids_24,
      linking = "never",
      judge_fun = judge_ok_24,
      fit_fun = fit_mock,
      engine = "mock",
      round_size = 6,
      max_rounds_per_batch = 1,
      within_batch_frac = 1,
      core_audit_frac = 0,
      reliability_target = Inf,
      checkpoint_dir = tmp,
      checkpoint_store_fits = FALSE,
      seed_pairs = 1,
      verbose = FALSE
    )
    core_ids2 <- LETTERS[1:4]
    tryCatch(
      bt_run_core_linking(
        samples = samples_24,
        batches = batches_24,
        core_ids = core_ids2,
        linking = "never",
        judge_fun = judge_ok_24,
        fit_fun = fit_mock,
        engine = "mock",
        max_rounds_per_batch = 1,
        resume_from = tmp,
        seed_pairs = 1
      ),
      error = function(e) conditionMessage(e)
    )
  },
  validate = function(msg) {
    .h_expect(grepl("Requested:", msg, fixed = TRUE))
    .h_expect(grepl("Checkpoint:", msg, fixed = TRUE))
    .h_expect(grepl("core_ids", msg))
  }
)

# -------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# 2b) Linking diagnostics / metrics (pre/post drift + parameters)
# ---------------------------------------------------------------------------

.h_run("bt_link_thetas() basic properties", {
  fit1 <- list(theta = tibble::tibble(ID = LETTERS[1:5], theta = 1:5, se = rep(1, 5)))
  fit2 <- list(theta = tibble::tibble(ID = LETTERS[1:5], theta = (1:5) * 2 + 10, se = rep(1, 5)))
  lk <- bt_link_thetas(fit2, fit1, ids = LETTERS[1:5], method = "mean_sd")
  stopifnot(is.list(lk), all(c("a", "b", "theta", "n_core") %in% names(lk)))
  stopifnot(nrow(lk$theta) == 5L)
})

.h_run("bt_run_core_linking() linking=always reduces baseline drift (mock drifting fitter)", {
  fit_fun <- make_fit_mock_drifting()
  out <- bt_run_core_linking(
    data = sample_data,
    id_col = "ID",
    text_col = "text",
    judge_fun = judge_mock,
    fit_fun = fit_fun,
    batch_size = 12,
    round_size = 6,
    within_batch_frac = 0.6,
    core_ids = LETTERS[1:5],
    max_rounds_per_batch = 2,
    linking = "always",
    linking_min_n = 5,
    linking_cor_target = 0.999,
    linking_p90_abs_shift_target = 0.01,
    show_progress = FALSE
  )

  stopifnot(is.data.frame(out$metrics), "linking_applied" %in% names(out$metrics))
  stopifnot(any(out$metrics$linking_applied, na.rm = TRUE))
  stopifnot(all(c("linking_p90_abs_shift", "linking_post_p90_abs_shift") %in% names(out$metrics)))

  pre <- out$metrics$linking_p90_abs_shift
  post <- out$metrics$linking_post_p90_abs_shift
  stopifnot(any(is.finite(pre) & is.finite(post) & post < pre, na.rm = TRUE))

  stopifnot(all(c("theta_original", "theta_linked") %in% names(out$results)))
  stopifnot(any(abs(out$results$theta - out$results$theta_original) > 1e-8, na.rm = TRUE))
})

.h_run("bt_run_core_linking() linking=auto does not trigger under stable fits", {
  out <- bt_run_core_linking(
    data = sample_data,
    id_col = "ID",
    text_col = "text",
    judge_fun = judge_mock,
    fit_fun = fit_mock,
    batch_size = 10,
    round_size = 6,
    within_batch_frac = 0.6,
    core_ids = LETTERS[1:5],
    max_rounds_per_batch = 1,
    linking = "auto",
    linking_min_n = 5,
    linking_cor_target = 0.99,
    linking_p90_abs_shift_target = 0.01,
    show_progress = FALSE
  )
  stopifnot(is.data.frame(out$metrics), "linking_applied" %in% names(out$metrics))
  stopifnot(!any(out$metrics$linking_applied, na.rm = TRUE))
})

# 3) bt_run_adaptive_core_linking (allocation/linking/core selection)
# -------------------------------------------------------------------------

.h_div("bt_run_adaptive_core_linking")

judge_ok_80 <- judge_deterministic(true_theta_80, seed = 1)

.h_run(
  "adaptive_core_linking: end-to-end (core_ids provided) + stop_reason",
  {
    out <- bt_run_adaptive_core_linking(
      samples = samples_24,
      batches = batches_24,
      core_ids = core_ids_24,
      linking = "never",
      judge_fun = judge_ok_24,
      fit_fun = fit_mock,
      engine = "mock",
      round_size = 12,
      max_rounds_per_batch = 2,
      within_batch_frac = 1,
      core_audit_frac = 0,
      reliability_target = Inf,
      checkpoint_store_fits = FALSE,
      seed_pairs = 1,
      verbose = TRUE
    )
    out
  },
  validate = function(out) {
    .h_expect_named(out, c("batch_summary", "stop_reason", "results", "core_ids"))
    .h_expect(identical(out$stop_reason, "max_rounds"))
  }
)

.h_run("adaptive_core_linking: linking=always reduces baseline drift (mock drifting fitter)", {
  fit_fun <- make_fit_mock_drifting()
  out <- bt_run_adaptive_core_linking(
    data = sample_data,
    id_col = "ID",
    text_col = "text",
    judge_fun = judge_mock,
    fit_fun = fit_fun,
    batch_size = 12,
    round_size = 6,
    within_batch_frac = 0.6,
    core_ids = LETTERS[1:5],
    max_rounds_per_batch = 2,
    linking = "always",
    linking_min_n = 5,
    linking_cor_target = 0.999,
    linking_p90_abs_shift_target = 0.01,
    show_progress = FALSE
  )

  stopifnot(is.data.frame(out$metrics), "linking_applied" %in% names(out$metrics))
  stopifnot(any(out$metrics$linking_applied, na.rm = TRUE))
  stopifnot(all(c("linking_p90_abs_shift", "linking_post_p90_abs_shift") %in% names(out$metrics)))

  pre <- out$metrics$linking_p90_abs_shift
  post <- out$metrics$linking_post_p90_abs_shift
  stopifnot(any(is.finite(pre) & is.finite(post) & post < pre, na.rm = TRUE))
})

.h_run(
  "adaptive_core_linking: core selection token_stratified (core_ids=NULL)",
  {
    out <- bt_run_adaptive_core_linking(
      samples = samples_80,
      batches = list(samples_80$ID[1:30], samples_80$ID[31:60]),
      core_ids = NULL,
      core_method = "token_stratified",
      core_size = 10,
      linking = "never",
      judge_fun = judge_ok_80,
      fit_fun = fit_mock,
      engine = "mock",
      round_size = 20,
      max_rounds_per_batch = 1,
      within_batch_frac = 1,
      core_audit_frac = 0,
      reliability_target = Inf,
      checkpoint_store_fits = FALSE,
      seed_core = 1,
      seed_pairs = 1
    )
    out
  },
  validate = function(out) {
    .h_expect(length(out$core_ids) == 10L)
    .h_expect(nrow(out$batch_summary) == 2L)
  }
)

.h_run(
  "adaptive_core_linking: core selection embeddings/auto (core_ids=NULL)",
  {
    out <- bt_run_adaptive_core_linking(
      samples = samples_24,
      batches = batches_24,
      core_ids = NULL,
      core_method = "auto",
      core_size = 8,
      embeddings = .emb_24,
      linking = "never",
      judge_fun = judge_ok_24,
      fit_fun = fit_mock,
      engine = "mock",
      round_size = 10,
      max_rounds_per_batch = 1,
      within_batch_frac = 1,
      core_audit_frac = 0,
      reliability_target = Inf,
      checkpoint_store_fits = FALSE,
      seed_core = 1,
      seed_pairs = 1
    )
    out
  },
  validate = function(out) .h_expect(length(out$core_ids) == 8L)
)

.h_run(
  "adaptive_core_linking: allocation=precision_ramp runs",
  {
    out <- bt_run_adaptive_core_linking(
      samples = samples_24,
      batches = batches_24,
      core_ids = core_ids_24,
      linking = "never",
      judge_fun = judge_ok_24,
      fit_fun = fit_mock,
      engine = "mock",
      allocation = "precision_ramp",
      round_size = 12,
      max_rounds_per_batch = 2,
      within_batch_frac = 0.80,
      core_audit_frac = 0.10,
      reliability_target = Inf,
      checkpoint_store_fits = FALSE,
      seed_pairs = 1
    )
    out
  },
  validate = function(out) .h_expect(nrow(out$batch_summary) == 2L)
)

.h_run(
  "adaptive_core_linking: allocation=audit_on_drift runs",
  {
    out <- bt_run_adaptive_core_linking(
      samples = samples_24,
      batches = batches_24,
      core_ids = core_ids_24,
      linking = "never",
      judge_fun = judge_ok_24,
      fit_fun = fit_mock,
      engine = "mock",
      allocation = "audit_on_drift",
      round_size = 12,
      max_rounds_per_batch = 2,
      within_batch_frac = 0.80,
      core_audit_frac = 0.10,
      reliability_target = Inf,
      checkpoint_store_fits = FALSE,
      seed_pairs = 1
    )
    out
  },
  validate = function(out) .h_expect(nrow(out$batch_summary) == 2L)
)

.h_run(
  "adaptive_core_linking: round_size=0 stops with round_size_zero",
  {
    bt_run_adaptive_core_linking(
      samples = samples_24,
      batches = batches_24,
      core_ids = core_ids_24,
      linking = "never",
      judge_fun = judge_ok_24,
      fit_fun = fit_mock,
      engine = "mock",
      round_size = 0,
      max_rounds_per_batch = 10,
      reliability_target = Inf,
      seed_pairs = 1
    )
  },
  validate = function(out) .h_expect(identical(out$stop_reason, "round_size_zero"))
)

.h_run(
  "adaptive_core_linking: seed alias works (seed=1)",
  {
    # Should not partial-match; seed is now a formal arg alias for seed_pairs.
    bt_run_adaptive_core_linking(
      samples = samples_24,
      batches = batches_24,
      core_ids = core_ids_24,
      linking = "never",
      judge_fun = judge_ok_24,
      fit_fun = fit_mock,
      engine = "mock",
      round_size = 6,
      max_rounds_per_batch = 1,
      within_batch_frac = 1,
      core_audit_frac = 0,
      reliability_target = Inf,
      checkpoint_store_fits = FALSE,
      seed = 1,
      verbose = FALSE
    )
  },
  validate = function(out) .h_expect(nrow(out$batch_summary) == 2L)
)

# -------------------------------------------------------------------------
# 4) Adverse judge outputs (missing/invalid/flaky) + diagnostics behavior
# -------------------------------------------------------------------------

.h_div("Adverse judge outputs / robustness")

.h_run(
  "adaptive: missing winners tolerated (some NA better_id)",
  {
    out <- bt_run_adaptive(
      samples = samples_24,
      judge_fun = judge_missing(true_theta_24, p_missing = 0.20, seed = 1),
      fit_fun = fit_mock,
      engine = "mock",
      round_size = 30,
      init_round_size = 30,
      max_rounds = 2,
      reliability_target = Inf,
      seed_pairs = 1
    )
    out
  },
  validate = function(out) {
    .h_expect(any(is.na(out$results$better_id)))
  }
)

.h_run(
  "adaptive: invalid winners trigger clear error (diagnostics present)",
  {
    tryCatch(
      bt_run_adaptive(
        samples = samples_24,
        judge_fun = judge_invalid(true_theta_24, p_bad = 0.20, seed = 1),
        fit_fun = fit_mock,
        engine = "mock",
        round_size = 30,
        init_round_size = 30,
        max_rounds = 1,
        reliability_target = Inf,
        seed_pairs = 1
      ),
      error = function(e) conditionMessage(e)
    )
  },
  validate = function(msg) {
    .h_expect(is.character(msg))
    .h_expect(grepl("invalid", tolower(msg)) || grepl("better_id", msg))
  }
)

.h_run(
  "adaptive: flaky judge errors propagate (no silent corruption)",
  {
    tryCatch(
      bt_run_adaptive(
        samples = samples_24,
        judge_fun = judge_flaky_stop(true_theta_24, every = 2L, seed = 1),
        fit_fun = fit_mock,
        engine = "mock",
        round_size = 10,
        init_round_size = 10,
        max_rounds = 5,
        reliability_target = Inf,
        seed_pairs = 1
      ),
      error = function(e) conditionMessage(e)
    )
  },
  validate = function(msg) .h_expect(grepl("transient judge error", msg, fixed = TRUE))
)

# -------------------------------------------------------------------------
# 5) Multi-judge plumbing (4-column bt_data) with real fitter if available
# -------------------------------------------------------------------------

.h_div("Multi-judge plumbing")

theta_b <- true_theta_24
theta_b[names(theta_b) %in% LETTERS[1:5]] <- theta_b[names(theta_b) %in% LETTERS[1:5]] + 1

.h_run(
  "adaptive: two judges + fit_bt_model(engine=auto) (skips if unavailable)",
  {
    out <- bt_run_adaptive(
      samples = samples_24,
      judge_fun = judge_two_judges(true_theta_24, theta_b),
      fit_fun = fit_real_or_mock,
      engine = "auto",
      round_size = 20,
      init_round_size = 20,
      max_rounds = 2,
      reliability_target = Inf,
      seed_pairs = 1
    )
    out
  },
  skip_if = function() !.can_real_fit(),
  skip_msg = "Requires 'sirt' or 'BradleyTerry2' to exercise 4-column judge BT fits.",
  validate = function(out) {
    .h_expect(nrow(out$results) > 0L)
    .h_expect("judge" %in% names(out$results))
  }
)

# -------------------------------------------------------------------------
# Summary
# -------------------------------------------------------------------------

.h_div("Summary")
.h_summary <- .h_summary %>%
  mutate(seconds = round(seconds, 3)) %>%
  arrange(factor(status, levels = c("FAIL", "SKIP", "PASS")), desc(seconds), case)

print(.h_summary, n = nrow(.h_summary))

n_fail <- sum(.h_summary$status == "FAIL")
n_skip <- sum(.h_summary$status == "SKIP")
n_pass <- sum(.h_summary$status == "PASS")

.h_msg("\nTotals: PASS=", n_pass, " | SKIP=", n_skip, " | FAIL=", n_fail)

if (n_fail > 0L) {
  .h_msg("\nOne or more cases FAILED. Scroll up for details, or inspect .h_summary.")
} else {
  .h_msg("\nAll stress-harness cases passed.")
}

invisible(.h_summary)
