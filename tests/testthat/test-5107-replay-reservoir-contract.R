test_that("reservoirs validate unordered observations and canonical identity", {
  ids <- letters[1:4]
  edges <- tibble::tibble(A_id = c("b", "a", "d", "c"),
    B_id = c("a", "c", "a", "d"), Y = c(1L, 0L, 1L, 1L))
  reservoir <- make_adaptive_replay_reservoir(edges, ids)
  expect_s3_class(reservoir, "pairwiseLLM_replay_reservoir")
  reordered <- make_adaptive_replay_reservoir(edges[4:1, ], rev(ids))
  expect_identical(reservoir, reordered)
  annotated <- edges
  annotated$request_id <- letters[1:4]
  expect_identical(make_adaptive_replay_reservoir(annotated, ids)$manifest, reservoir$manifest)
  expect_error(make_adaptive_replay_reservoir(rbind(edges, edges[1, ]), ids), "Duplicate ordered")
  reverse <- edges[1, ]
  reverse$A_id <- "a"
  reverse$B_id <- "b"
  expect_error(make_adaptive_replay_reservoir(rbind(edges, reverse), ids), "one observation per unordered")
  expect_error(make_adaptive_replay_reservoir(edges[-1, ], ids), "disconnected")
  expect_error(make_adaptive_replay_reservoir(edges[0, ], ids), "disconnected")
  expect_error(make_adaptive_replay_reservoir(transform(edges, Y = 2), ids), "Y must")
  changed <- edges
  changed$Y[1] <- 0L
  expect_false(identical(make_adaptive_replay_reservoir(changed, ids)$manifest$digest,
    reservoir$manifest$digest))
  bad <- reservoir
  bad$outcomes$Y[1] <- 1L - bad$outcomes$Y[1]
  expect_error(pairwiseLLM:::.adaptive_reservoir_validate(bad), "integrity mismatch")
  expect_error(pairwiseLLM:::.adaptive_reservoir_validate(list()), "must be created")
})

test_that("sparse connected graphs share a seeded tree across modes and strategies", {
  withr::local_seed(258)
  rng <- .Random.seed
  ids <- letters[1:5]
  # A star is connected but has no path visiting every vertex exactly once.
  edges <- tibble::tibble(A_id = c("b", "a", "d", "a"),
    B_id = c("a", "c", "a", "e"), Y = c(1L, 0L, 1L, 0L))
  reservoir <- make_adaptive_replay_reservoir(edges, ids)
  prior <- make_warm_start_prior(stats::setNames(seq(-1, 1, length.out = 5), ids))
  tree <- NULL
  for (mode in c("cold", "btl_only", "trueskill_only", "both")) {
    for (strategy in c("hybrid", "random", "trueskill_p50", "trueskill_pollitt")) {
      state <- adaptive_rank_start(ids, seed = 38L, replay_reservoir = reservoir,
        warm_start_mode = mode, warm_start_prior = if (mode == "cold") NULL else prior,
        adaptive_config = list(pairing_strategy = strategy))
      expect_false("Y" %in% names(state$replay_reservoir$edges))
      expect_identical(state$meta$replay_reservoir_digest, reservoir$manifest$digest)
      if (is.null(tree)) tree <- state$warm_start_pairs
      expect_identical(state$warm_start_pairs, tree)
      expect_equal(nrow(tree), length(ids) - 1L)
      expect_no_error(pairwiseLLM:::.adaptive_reservoir_validate_state(state))
    }
  }
  expect_identical(.Random.seed, rng)
  expect_error(adaptive_rank_start(c(ids, "z"), replay_reservoir = reservoir), "panel exactly")
  expect_error(adaptive_rank_start(tibble::tibble(item_id = ids, set_id = c(1L, 1L, 2L, 2L, 2L)),
    replay_reservoir = reservoir,
    adaptive_config = list(run_mode = "link_one_spoke", link_estimation_mode = "fixed_shape_offset")), "within_set")
})

test_that("reservoir judges preserve presentation and consume only committed history", {
  ids <- c("a", "b")
  reservoir <- make_adaptive_replay_reservoir(data.frame(A_id = "b", B_id = "a", Y = 1L), ids)
  state <- adaptive_rank_start(ids, replay_reservoir = reservoir)
  judge <- make_adaptive_judge_replay(reservoir)
  a <- tibble::tibble(item_id = "a")
  b <- tibble::tibble(item_id = "b")
  expect_identical(judge(b, a, state)$Y, 1L)
  expect_identical(judge(b, a, state)$Y, 1L)
  expect_error(judge(a, b, state), "frozen observed orientation")
  expect_error(judge(b, a), "state bound")
  expect_error(judge(NULL, a, state), "one-row")
  expect_error(make_adaptive_judge_replay(reservoir, strict_use = FALSE), "strict_use")
  expect_error(make_adaptive_judge_replay(reservoir, c("a", "c")), "panel exactly")
  state$history_pairs <- tibble::tibble(A_id = "b", B_id = "a")
  expect_error(judge(b, a, state), "already committed")
})

test_that("initial reservoir sessions preserve manifests and reject tampering", {
  dir <- withr::local_tempdir()
  ids <- letters[1:3]
  reservoir <- make_adaptive_replay_reservoir(
    data.frame(A_id = c("b", "c"), B_id = c("a", "b"), Y = c(0L, 1L)), ids)
  state <- adaptive_rank_start(ids, seed = 56L, replay_reservoir = reservoir)
  save_adaptive_session(state, dir, overwrite = TRUE)
  restored <- adaptive_rank_resume(dir)
  expect_identical(restored$replay_reservoir, state$replay_reservoir)
  expect_identical(restored$warm_start_pairs, state$warm_start_pairs)
  expect_identical(validate_session_dir(dir)$replay_reservoir_digest, reservoir$manifest$digest)
  for (field in c("digest", "manifest_digest")) {
    bad <- state
    bad$replay_reservoir[[field]] <- "changed"
    expect_error(save_adaptive_session(bad, dir, overwrite = TRUE), "integrity mismatch")
  }
  bad <- state
  bad$warm_start_idx <- 2L
  expect_error(save_adaptive_session(bad, dir, overwrite = TRUE), "bootstrap progress")
  bad <- state
  bad$replay_reservoir <- NULL
  expect_error(save_adaptive_session(bad, dir, overwrite = TRUE), "manifest is missing")
  path <- file.path(dir, "metadata.rds")
  metadata <- readRDS(path)
  metadata$replay_reservoir_digest <- "changed"
  saveRDS(metadata, path)
  expect_error(adaptive_rank_resume(dir), "metadata integrity")
})

test_that("reservoir identity guards reject malformed state and ambiguous history keys", {
  collision <- data.frame(A_id = c("a:b", "a", "a"), B_id = c("c", "b:c", "a:b"), Y = 1L)
  expect_error(make_adaptive_replay_reservoir(collision, c("a:b", "c", "a", "b:c")),
    "ambiguous adaptive history keys")
  # Punctuation itself is valid when the existing adaptive keys remain unique.
  reservoir <- make_adaptive_replay_reservoir(
    data.frame(A_id = "a:b", B_id = "é", Y = 1L), c("é", "a:b"))
  state <- adaptive_rank_start(c("é", "a:b"), replay_reservoir = reservoir)
  bad <- state
  bad$replay_reservoir$version <- 2L
  expect_error(pairwiseLLM:::.adaptive_reservoir_validate_state(bad), "state integrity")
  bad <- state
  bad$history_pairs <- tibble::tibble(A_id = c("a:b", "a:b"), B_id = c("é", "é"))
  expect_error(pairwiseLLM:::.adaptive_reservoir_validate_state(bad), "repeated, foreign, or reversed")
  bad <- state
  bad$replay_reservoir$edges$A_id <- "é"
  bad$replay_reservoir$edges$B_id <- "a:b"
  expect_error(pairwiseLLM:::.adaptive_reservoir_validate_state(bad), "manifest.*integrity")
  expect_error(pairwiseLLM:::.adaptive_reservoir_assert_edge(state, character(), "é"), "allowed edge")
  expect_error(pairwiseLLM:::.adaptive_assign_order_for_state(state,
    tibble::tibble(i = "missing", j = "é"), NULL, NULL, NULL), "outside the replay reservoir")
  judge <- make_adaptive_judge_replay(reservoir)
  bad_judge <- judge
  attr(bad_judge, "replay_reservoir_validate_history") <- NULL
  expect_error(adaptive_rank_run_live(state, bad_judge, progress = "none"), "matching reservoir replay judge")
  # A drifted in-memory manifest must never cause a judge to infer the reverse.
  expect_error(judge(tibble::tibble(item_id = "é"), tibble::tibble(item_id = "a:b"), bad),
    "No frozen outcome")
})
