test_that("observed evidence corrects poor priors without excluding prior ranks", {
  withr::local_seed(705L)
  rng <- .Random.seed
  for (pattern in c("good", "noisy", "misplaced", "local", "reversed")) {
    f <- task07_fixture(pattern)
    prior_mean <- stats::setNames(f$prior$prior_mean, f$prior$item_id)
    for (strategy in c("hybrid", "random", "trueskill_p50", "trueskill_pollitt")) {
      run <- task07_run(f, strategy = strategy)
      state <- run$state
      rows <- state$step_log[state$step_log$status == "ok", ]
      post <- rows[rows$round_stage != "warm_start", ]
      label <- paste(pattern, strategy)
      expect_identical(run$initial$trueskill_state$items$mu,
        25 + (25 / 3) * f$prior$prior_mean, info = label)
      expect_identical(run$initial$trueskill_state$items$sigma, rep(25 / 3, 8L))
      expect_true(all(is.finite(state$trueskill_state$items$mu)))
      expect_true(all(is.finite(state$trueskill_state$items$sigma)))
      expect_setequal(c(post$A_id, post$B_id), f$ids)
      expect_identical(anyDuplicated(rows[c("A_id", "B_id")]), 0L)
      expect_true(all(table(rows$unordered_key) <= 2L))
      expect_identical(nrow(rows), length(run$before))
      expected_y <- f$outcomes$Y[match(paste(rows$A_id, rows$B_id),
        paste(f$outcomes$A_id, f$outcomes$B_id))]
      expect_identical(rows$Y, expected_y)
      winners <- ifelse(post$Y == 1L, post$A_id, post$B_id)
      losers <- ifelse(post$Y == 1L, post$B_id, post$A_id)
      if (pattern != "good" && strategy != "random") {
        expect_true(any(prior_mean[winners] < prior_mean[losers]), info = label)
      }
      for (k in seq_len(nrow(rows))) {
        before <- run$before[[k]]$ts$items
        after <- if (k < nrow(rows)) run$before[[k + 1L]]$ts$items else state$trueskill_state$items
        winner <- if (rows$Y[[k]] == 1L) rows$A_id[[k]] else rows$B_id[[k]]
        loser <- if (rows$Y[[k]] == 1L) rows$B_id[[k]] else rows$A_id[[k]]
        expect_gt(after$mu[match(winner, after$item_id)], before$mu[match(winner, before$item_id)])
        expect_lt(after$mu[match(loser, after$item_id)], before$mu[match(loser, before$item_id)])
        untouched <- !before$item_id %in% c(winner, loser)
        expect_identical(after[untouched, ], before[untouched, ])
        if (rows$round_stage[[k]] == "direct_pairing") {
          counts <- run$before[[k]]$counts
          focal <- rows$i_id[[k]]
          expect_equal(unname(counts$deg[focal]), min(counts$deg))
          legal <- setdiff(f$ids, focal)
          keys <- pairwiseLLM:::make_unordered_key(focal, legal)
          prior_count <- counts$pair_count[keys]
          legal <- legal[is.na(prior_count) | prior_count < 2L]
          if (strategy != "random") {
            probabilities <- vapply(legal, function(id) {
              pairwiseLLM:::trueskill_win_probability(focal, id, run$before[[k]]$ts)
            }, numeric(1L))
            distances <- if (strategy == "trueskill_p50") {
              abs(probabilities - 0.5)
            } else {
              pmin(abs(probabilities - 1 / 3), abs(probabilities - 2 / 3))
            }
            expect_identical(rows$j_id[[k]], legal[order(distances, legal)[[1L]]])
            # A/B reversal evaluates the complementary normal CDF; use absolute error.
            expect_lte(abs(rows$target_distance[[k]] - min(distances)), 1e-14)
          } else {
            expect_true(rows$j_id[[k]] %in% legal)
            expect_true(is.na(rows$target_distance[[k]]))
          }
        }
      }
      if (strategy == "hybrid") {
        expect_setequal(post$round_stage, c("anchor_link", "long_link", "mid_link", "local_link"))
        expect_gt(length(state$round$stage_quotas), 0L)
      } else {
        expect_identical(unique(post$round_stage), "direct_pairing")
        expect_length(state$round$stage_quotas, 0L)
      }
      expect_history_state_matches_history(state)
    }
  }
  expect_identical(.Random.seed, rng)
})
