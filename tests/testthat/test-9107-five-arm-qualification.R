test_that("five arms respect exposure budgets and the exact connected-tree boundary", {
  withr::local_seed(9107L)
  rng <- .Random.seed
  for (n in c(8L, 9L)) {
    budgets <- c(0, 0.5, 1, 2)
    targets <- floor(budgets * n / 2)
    # Independent expected counts exercise both quarter- and half-pair rounding.
    expect_equal(targets, c(0, 2, 4, n))
    expect_true(all(2 * targets / n <= budgets))
    expect_true(all(targets[2:3] < n - 1L))
    checkpoints <- sort(unique(c(targets, n - 2L, n - 1L, n)))
    for (topology in c("dense", "sparse")) {
      f <- five_arm_fixture(n, topology)
      starts <- lapply(names(five_arm_modes), function(arm) five_arm_start(f, arm))
      names(starts) <- names(five_arm_modes)
      expect_identical(starts$cold$warm_start_pairs, starts$estimation$warm_start_pairs)
      expect_identical(starts$cold$warm_start_pairs, starts$legacy_graph$warm_start_pairs)
      expect_identical(starts$selection$warm_start_pairs, starts$full$warm_start_pairs)
      expect_false(identical(starts$legacy_graph$warm_start_pairs, starts$full$warm_start_pairs))
      expect_identical(starts$cold$trueskill_state, starts$estimation$trueskill_state)
      expect_identical(starts$legacy_graph$trueskill_state, starts$selection$trueskill_state)
      expect_identical(starts$legacy_graph$trueskill_state, starts$full$trueskill_state)
      expect_identical(starts$legacy_graph$predictive_prior, starts$full$predictive_prior)
      expect_identical(starts$legacy_graph$meta$trueskill_mapping, starts$full$meta$trueskill_mapping)
      finals <- list()
      for (arm in names(starts)) {
        initial <- starts[[arm]]
        expect_identical(initial$meta$seed, 317L)
        expect_equal(nrow(initial$warm_start_pairs), n - 1L)
        expect_equal(nrow(initial$history_pairs), 0L)
        expect_null(initial$btl_fit)
        if (!arm %in% c("cold", "estimation")) {
          ts <- initial$trueskill_state$items
          expect_equal(ts$mu, unname(25 + (25 / 3) * f$prediction[ts$item_id]))
          expect_equal(ts$sigma, unname((25 / 3) * f$sd[ts$item_id]))
          expect_identical(initial$meta$trueskill_mapping$sd_source, "prior_object")
          expect_identical(initial$meta$trueskill_mapping$sd_rule, "per_item")
          expect_identical(initial$meta$trueskill_mapping$calibration, "upstream_not_verified")
        }
        state <- initial
        for (m in seq.int(0L, n + 3L)) {
          if (m > 0L) {
            before <- state
            state <- five_arm_run(state, f)
            if (m >= n) expect_five_arm_pollitt(before, state, f)
          }
          if (m %in% checkpoints) {
            expect_equal(nrow(state$history_pairs), m)
            expect_equal(pairwiseLLM::summarize_adaptive(state)$committed_pairs, m)
            expect_identical(state$warm_start_done, m >= n - 1L)
            expect_equal(five_arm_components(state), max(1L, n - m))
            expect_equal(state$warm_start_idx, min(m + 1L, n))
            expect_identical(state$warm_start_pairs, initial$warm_start_pairs)
            expect_equal(sum(state$step_log$round_stage == "warm_start"), min(m, n - 1L))
            expect_five_arm_history(state, f)
          }
          if (m == n - 1L) {
            expect_identical(state$history_pairs$A_id, initial$warm_start_pairs$i_id)
            expect_identical(state$history_pairs$B_id, initial$warm_start_pairs$j_id)
          }
        }
        expect_null(state$btl_fit)
        expect_equal(nrow(state$round_log), 0L)
        finals[[arm]] <- state
      }
      # With refits disabled, BTL destination alone cannot alter collection.
      for (arms in list(c("cold", "estimation"), c("selection", "full"))) {
        for (field in c("history_pairs", "trueskill_state")) {
          expect_identical(finals[[arms[1]]][[field]], finals[[arms[2]]][[field]])
        }
        expect_identical(finals[[arms[1]]]$step_log[c("A_id", "B_id", "Y", "target_distance")],
          finals[[arms[2]]]$step_log[c("A_id", "B_id", "Y", "target_distance")])
      }
    }
  }
  expect_identical(.Random.seed, rng)
})

test_that("matched seeds and scalar or heterogeneous priors reach the correct BTL destination", {
  withr::local_seed(9107L)
  captured <- list()
  testthat::local_mocked_bindings(fit_bayes_btl_mcmc = function(results, ids,
      model_variant, cmdstan, warm_start_prior = NULL) {
    captured[[length(captured) + 1L]] <<- list(results = results, prior = warm_start_prior)
    list(fit = make_test_btl_fit(ids, draws = outer(seq_len(10) * 0.005, seq_along(ids), "+")))
  }, .package = "pairwiseLLM")
  for (heterogeneous in c(FALSE, TRUE)) {
    f <- five_arm_fixture(heterogeneous = heterogeneous)
    prior <- pairwiseLLM::make_warm_start_prior(f$prediction, prior_sd = f$sd)
    for (seed in c(41L, 317L)) {
      for (arm in names(five_arm_modes)) {
        state <- five_arm_start(f, arm, seed)
        expect_identical(state, five_arm_start(f, arm, seed))
        judge <- pairwiseLLM::make_adaptive_judge_replay(
          pairwiseLLM::make_adaptive_replay_reservoir(f$primary, f$ids))
        out <- pairwiseLLM::adaptive_rank_run_live(state, judge, n_steps = 7L,
          btl_config = list(refit_pairs_target = 7L), progress = "none")
        expect_equal(nrow(out$round_log), 1L)
        call <- tail(captured, 1L)[[1L]]
        expect_identical(call$prior, if (arm %in% c("estimation", "legacy_graph", "full")) prior else NULL)
        expect_equal(nrow(call$results), 7L)
        expect_five_arm_history(out, f)
      }
    }
  }
  expect_length(captured, 20L)
})

test_that("held-out labels, reversal outcomes and human scores cannot leak into five-arm replay", {
  f <- five_arm_fixture(topology = "sparse")
  primary_keys <- five_arm_key(f$primary$A_id, f$primary$B_id)
  expect_length(intersect(primary_keys, five_arm_key(f$heldout$A_id, f$heldout$B_id)), 0L)
  # Audit reversals intentionally share unordered pairs, but never presentation rows.
  expect_length(intersect(paste(f$primary$A_id, f$primary$B_id),
    paste(f$reversals$A_id, f$reversals$B_id)), 0L)
  variants <- lapply(c("heldout", "reversals", "human_scores", "all"), function(layer) {
    changed <- f
    for (field in intersect(c(layer, if (layer == "all") names(f)), c("heldout", "reversals"))) {
      changed[[field]]$Y <- 1L - rev(changed[[field]]$Y)
      changed[[field]] <- changed[[field]][rev(seq_len(nrow(changed[[field]]))), ]
    }
    if (layer %in% c("human_scores", "all")) changed$human_scores <- -100 * rev(f$human_scores)
    changed
  })
  for (arm in names(five_arm_modes)) {
    initial <- five_arm_start(f, arm)
    # Running mutates an internal memo environment; keep the initial reference untouched.
    expected <- five_arm_run(five_arm_start(f, arm), f, 11L)
    for (changed in variants) {
      actual_initial <- five_arm_start(changed, arm)
      expect_identical(actual_initial, initial)
      expect_identical(five_arm_run(actual_initial, changed, 11L), expected)
    }
    expect_five_arm_history(expected, f)
  }
})

test_that("selectable Y changes judged updates but not the frozen initialization or tree", {
  f <- five_arm_fixture()
  changed <- f
  changed$primary$Y <- 1L - changed$primary$Y
  changed_judge <- pairwiseLLM::make_adaptive_judge_replay(
    pairwiseLLM::make_adaptive_replay_reservoir(changed$primary, changed$ids))
  for (arm in names(five_arm_modes)) {
    a <- five_arm_start(f, arm)
    b <- five_arm_start(changed, arm)
    for (field in c("predictive_prior", "trueskill_state", "warm_start_pairs", "bootstrap")) {
      expect_identical(a[[field]], b[[field]])
    }
    expect_identical(a$replay_reservoir$manifest_digest, b$replay_reservoir$manifest_digest)
    expect_false(identical(a$meta$replay_reservoir_digest, b$meta$replay_reservoir_digest))
    judged_a <- five_arm_run(a, f, 7L)
    judged_b <- five_arm_run(b, changed, 7L)
    expect_identical(judged_a$step_log[c("A_id", "B_id")], judged_b$step_log[c("A_id", "B_id")])
    expect_identical(judged_a$step_log$Y, 1L - judged_b$step_log$Y)
    expect_false(identical(judged_a$trueskill_state, judged_b$trueskill_state))
    expect_identical(judged_b$warm_start_pairs, a$warm_start_pairs)
    expect_error(pairwiseLLM::adaptive_rank_run_live(judged_a, changed_judge, progress = "none"),
      "judge identity mismatch")
  }
})

test_that("invalid attempts around N-1 preserve evidence budgets and the next committed decision", {
  f <- five_arm_fixture()
  for (arm in names(five_arm_modes)) {
    state <- five_arm_run(five_arm_start(f, arm), f, 6L)
    for (m in 6:8) {
      before <- state
      bad <- five_arm_invalid(five_arm_invalid(before, f), f)
      for (field in c("history_pairs", "warm_start_idx", "warm_start_done", "warm_start_pairs",
        "trueskill_state", "predictive_prior", "bootstrap")) expect_identical(bad[[field]], before[[field]])
      summary <- pairwiseLLM::summarize_adaptive(bad)
      expect_equal(summary$committed_pairs, m)
      expect_equal(summary$steps_attempted, nrow(before$step_log) + 2L)
      state <- five_arm_run(before, f)
      retried <- five_arm_run(bad, f)
      expect_identical(retried$history_pairs, state$history_pairs)
      expect_identical(retried$trueskill_state, state$trueskill_state)
      expect_identical(tail(retried$step_log[c("A_id", "B_id", "Y", "target_distance")], 1L),
        tail(state$step_log[c("A_id", "B_id", "Y", "target_distance")], 1L))
      expect_equal(nrow(retried$history_pairs), m + 1L)
      expect_five_arm_history(retried, f)
    }
  }
})

test_that("every arm continues identically in a fresh process before and after connectivity", {
  withr::local_seed(9107L)
  rng <- .Random.seed
  f <- five_arm_fixture()
  root <- withr::local_tempdir()
  cases <- list()
  for (arm in names(five_arm_modes)) {
    initial <- five_arm_start(f, arm)
    for (m in c(0L, 3L, 7L, 8L)) {
      before <- five_arm_run(initial, f, m)
      if (m %in% c(3L, 7L)) before <- five_arm_invalid(before, f)
      path <- file.path(root, paste(arm, m, sep = "-"))
      pairwiseLLM::save_adaptive_session(before, path)
      restored <- pairwiseLLM::adaptive_rank_resume(path)
      restored$config$session_dir <- NULL
      after <- five_arm_run(before, f, 2L)
      expect_identical(five_arm_run(restored, f, 2L)$step_log, after$step_log)
      cases[[length(cases) + 1L]] <- list(path = path, before = before, after = after)
    }
  }
  config <- file.path(root, "config.rds")
  result <- file.path(root, "result.rds")
  dev_path <- if (pkgload::is_dev_package("pairwiseLLM")) getNamespaceInfo("pairwiseLLM", "path") else NULL
  saveRDS(list(dev_path = dev_path, libpaths = .libPaths(), result = result,
    reservoir = pairwiseLLM::make_adaptive_replay_reservoir(f$primary, f$ids),
    paths = lapply(cases, `[[`, "path")), config)
  code <- paste(
    "cfg <- readRDS(commandArgs(TRUE)[[1]])",
    ".libPaths(cfg$libpaths)",
    "if (!is.null(cfg$dev_path)) pkgload::load_all(cfg$dev_path,quiet=TRUE) else library(pairwiseLLM)",
    "testthat::test_that('five-arm resume never initializes again', {",
    "fail <- function(...) stop('forbidden reinitialization or fit')",
    # Legacy validation reconstructs its seeded tree to verify the saved queue.
    # Predictive validation must never rebuild or rescore its frozen tree.
    "historical <- pairwiseLLM:::.adaptive_reservoir_bootstrap",
    "validate_historical <- function(state) {",
    "if (identical(state$meta$bootstrap_policy,'predictive_connected')) fail()",
    "historical(state) }",
    "testthat::local_mocked_bindings(.warm_start_adaptive_init=fail, adaptive_rank_start=fail,",
    "extract_warm_start_features=fail, make_warm_start_prior=fail, .warm_start_prior_resolve_model=fail,",
    "predict.pairwiseLLM_warm_model=fail, .adaptive_predictive_tree=fail,",
    ".adaptive_reservoir_bootstrap=validate_historical, fit_bayes_btl_mcmc=fail, .package='pairwiseLLM')",
    "stopifnot(!exists('.Random.seed',envir=.GlobalEnv,inherits=FALSE))",
    "judge <- pairwiseLLM::make_adaptive_judge_replay(cfg$reservoir)",
    "out <- lapply(cfg$paths, function(path) {",
    "s <- pairwiseLLM::adaptive_rank_resume(path)",
    "list(before=s, after=pairwiseLLM::adaptive_rank_run_live(s,judge,n_steps=2L,",
    "btl_config=list(refit_pairs_target=5000L),progress='none')) })",
    "stopifnot(!exists('.Random.seed',envir=.GlobalEnv,inherits=FALSE))",
    "saveRDS(out,cfg$result)", "testthat::expect_length(out,length(cfg$paths))", "})", sep = "\n")
  output <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
    c("--vanilla", "-e", shQuote(code), shQuote(config)), stdout = TRUE, stderr = TRUE))
  expect_true(is.null(attr(output, "status")), info = paste(output, collapse = "\n"))
  expect_true(file.exists(result))
  if (!file.exists(result)) return(invisible(NULL))
  actual <- readRDS(result)
  for (i in seq_along(cases)) {
    for (part in c("before", "after")) {
      a <- actual[[i]][[part]]
      b <- cases[[i]][[part]]
      for (field in c("bootstrap", "warm_start_pairs", "warm_start_idx", "warm_start_done",
        "predictive_prior", "trueskill_state", "step_log", "round_log")) expect_identical(a[[field]], b[[field]])
      expect_identical(a$history_pairs[c("A_id", "B_id")], b$history_pairs[c("A_id", "B_id")])
      expect_false(any(a$history_pairs$is_probe_step))
      expect_identical(pairwiseLLM::summarize_adaptive(a, include_bootstrap = TRUE),
        pairwiseLLM::summarize_adaptive(b, include_bootstrap = TRUE))
      expect_five_arm_history(a, f)
    }
  }
  expect_identical(.Random.seed, rng)
})

test_that("legacy sessions and public audit fields remain compatible across the five arms", {
  f <- five_arm_fixture()
  for (arm in names(five_arm_modes)) {
    state <- five_arm_run(five_arm_start(f, arm), f, 7L)
    path <- withr::local_tempdir()
    pairwiseLLM::save_adaptive_session(state, path)
    ordinary <- pairwiseLLM::summarize_adaptive(state)
    expect_identical(names(ordinary), c("n_items", "steps_attempted", "committed_pairs", "n_refits",
      "last_stop_decision", "last_stop_reason"))
    audited <- pairwiseLLM::summarize_adaptive(state, include_bootstrap = TRUE)
    expect_identical(audited[names(ordinary)], ordinary)
    audit <- audited$bootstrap[[1L]]
    expect_identical(names(audit), c("policy", "version", "seed", "digest", "trueskill_mapping_digest",
      "manifest_digest", "diagnostics"))
    expect_identical(audit$policy, state$meta$bootstrap_policy)
    expect_identical(audit$version, 1L)
    expect_identical(audit$seed, 317L)
    expect_identical(audit$manifest_digest, state$replay_reservoir$manifest_digest)
    other_policy <- if (arm %in% c("selection", "full")) "shuffled_connected" else "predictive_connected"
    expect_error(pairwiseLLM::adaptive_rank(data.frame(item_id = f$ids, text = "synthetic"),
      session_dir = path, bootstrap_policy = other_policy,
      judge = pairwiseLLM::make_adaptive_judge_replay(
        pairwiseLLM::make_adaptive_replay_reservoir(f$primary, f$ids)),
      progress = "none"), "Cannot change.*bootstrap_policy")
    if (arm %in% c("selection", "full")) {
      expect_identical(audit$digest, state$bootstrap$digest)
      expect_identical(audit$diagnostics, state$bootstrap$diagnostics)
      expect_identical(audit$trueskill_mapping_digest, state$meta$trueskill_mapping$digest)
      original <- tools::md5sum(list.files(path, full.names = TRUE, recursive = TRUE))
      bad <- state
      bad$warm_start_pairs <- bad$warm_start_pairs[7:1, ]
      expect_error(pairwiseLLM::save_adaptive_session(bad, path, overwrite = TRUE), "digest integrity")
      expect_identical(tools::md5sum(names(original)), original)
    } else {
      expect_true(is.na(audit$digest))
      expect_null(audit$diagnostics)
      # Recreate pre-policy metadata on disk, preserving coherent mapping if present.
      state$meta$bootstrap_policy <- state$meta$bootstrap_policy_version <- NULL
      metadata <- readRDS(file.path(path, "metadata.rds"))
      metadata$bootstrap_policy <- metadata$bootstrap_policy_version <- metadata$bootstrap_digest <- NULL
      saveRDS(state, file.path(path, "state.rds"))
      saveRDS(metadata, file.path(path, "metadata.rds"))
      legacy <- pairwiseLLM::adaptive_rank_resume(path)
      expect_identical(five_arm_run(legacy, f, 2L)$step_log, five_arm_run(state, f, 2L)$step_log)
      expect_identical(legacy$warm_start_pairs, state$warm_start_pairs)
      expect_identical(legacy$trueskill_state, state$trueskill_state)
    }
  }
})
