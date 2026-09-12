test_that("pooled judge cache identity preserves named evidence and ignores set ordering", {
  state <- task09_link_state()
  hashes <- c(`1` = "hash-one", `2` = "hash-two", `3` = "hash-three")
  pooled <- .adaptive_phase_a_pooled_judge_state_from_fit(list(beta_mean = 0, epsilon_mean = 0), "btl_e_b",
    1:3, hashes, 3L, 3L)
  valid <- function(value, evidence = hashes) {
    .adaptive_phase_a_pooled_judge_state_valid(value, 3:1, "btl_e_b", evidence)
  }
  expect_identical(pooled$evidence_hash_by_set, hashes)
  expect_true(valid(pooled))
  expect_true(valid(pooled, hashes[3:1]))
  expect_false(valid(pooled, c(`1` = "changed", hashes[-1])))
  expect_false(valid(NULL))
  bad <- pooled
  bad$required_sets <- 1:2
  expect_false(valid(bad))
  bad <- pooled
  bad$model_variant <- "btl"
  expect_false(valid(bad))
  bad <- pooled
  bad$evidence_hash_by_set <- unname(hashes)
  expect_false(valid(bad))
})

test_that("pooled judge inputs require artifacts and preserve the observed presentation", {
  state <- task09_link_state()
  artifacts <- state$linking$phase_a$artifacts
  inputs <- .adaptive_phase_a_pooled_judge_results(state, artifacts, 1:3, state$controller)
  expect_setequal(inputs$ids, state$item_ids)
  expect_equal(nrow(inputs$results), 3L)
  expect_true(all(inputs$results$judge_scope == "shared"))
  expect_identical(inputs$results$ordered_key,
    make_ordered_key(inputs$results$A_id, inputs$results$B_id))
  expect_error(.adaptive_phase_a_pooled_judge_results(state, artifacts[-1], 1:3,
    state$controller), "finalized artifact.*1")
  expect_error(.adaptive_phase_a_pooled_judge_refit(state,
    list(phase_a_pooled_judge_fit_fn = 1), state$controller), "must be a function")
  state$items <- state$items[c(1, 3, 5), ]
  state$item_ids <- as.character(state$items$item_id)
  for (key in names(artifacts)) {
    artifacts[[key]]$items <- artifacts[[key]]$items[1, ]
    artifacts[[key]]$phase_a_within_set_evidence <- .adaptive_phase_a_empty_within_set_evidence()
    artifacts[[key]]$phase_a_within_set_evidence_hash <- NULL
    artifacts[[key]]$n_pairs_committed <- 0L
  }
  state$linking$phase_a$artifacts <- artifacts
  pooled <- .adaptive_phase_a_pooled_judge_refit(state, list(), state$controller)
  expect_equal(pooled$beta_mean, 0)
  expect_equal(pooled$epsilon_mean, 0)
  expect_true(pooled$diagnostics$no_phase_a_within_set_edges)
})

test_that("anchored scaffolding repairs fresh metadata but rejects corruption on resume", {
  state <- .adaptive_anchored_joint_sync_scaffolding(task09_link_state())
  base <- state$linking$anchored_joint$fisher_t0_by_spoke[["2"]]
  changes <- list(free_block_dim = 9L, n_link_active_pairs = -1L,
    anchored_joint_init_state_method = "invalid")
  for (field in names(changes)) {
    bad <- state
    bad$linking$anchored_joint$fisher_t0_by_spoke[["2"]][[field]] <- changes[[field]]
    repaired <- .adaptive_anchored_joint_sync_scaffolding(bad)
    expect_identical(repaired$linking$anchored_joint$fisher_t0_by_spoke[["2"]][[field]],
      base[[field]])
    bad$meta$resumed_from_session <- TRUE
    expect_error(.adaptive_anchored_joint_sync_scaffolding(bad), "resume anchored-joint invariant")
  }
})
test_that("pooled judge refit is reused until authoritative evidence changes", {
  state <- task09_link_state()
  calls <- 0L
  config <- list(phase_a_pooled_judge_fit_fn = function(results, ids, ...) {
    calls <<- calls + 1L
    expect_setequal(ids, state$item_ids)
    fit <- state$btl_fit
    fit$beta_mean <- 0.1
    fit$epsilon_mean <- 0.05
    fit
  })
  first <- .adaptive_phase_a_pooled_judge_refit(state, config, state$controller)
  state$linking$phase_a$pooled_judge_state <- first
  expect_identical(.adaptive_phase_a_pooled_judge_refit(state, config, state$controller), first)
  expect_identical(calls, 1L)
  evidence <- state$linking$phase_a$artifacts[["2"]]$phase_a_within_set_evidence
  evidence$y_A <- 1L - evidence$y_A
  state$linking$phase_a$artifacts[["2"]]$phase_a_within_set_evidence <- evidence
  second <- .adaptive_phase_a_pooled_judge_refit(state, config, state$controller)
  expect_identical(calls, 2L)
  expect_false(identical(second$evidence_hash_by_set, first$evidence_hash_by_set))
})
