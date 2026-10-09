# Independent synthetic evidence layers; no study artifacts or model training.
five_arm_fixture <- function(n = 8L, topology = "dense", heterogeneous = TRUE) {
  ids <- letters[seq_len(n)]
  endpoints <- t(utils::combn(seq_len(n), 2L))
  gap <- endpoints[, 2L] - endpoints[, 1L]
  # Both graphs connect all items and have enough unused edges after the tree.
  selectable <- if (topology == "dense") gap != n - 1L else gap <= 2L
  reversed <- seq_len(nrow(endpoints)) %% 2L == 0L
  observations <- data.frame(
    A_id = ids[ifelse(reversed, endpoints[, 2L], endpoints[, 1L])],
    B_id = ids[ifelse(reversed, endpoints[, 1L], endpoints[, 2L])],
    Y = as.integer(seq_len(nrow(endpoints)) %% 3L == 0L))
  primary <- observations[selectable, ]
  reversal <- data.frame(A_id = primary$B_id, B_id = primary$A_id,
    Y = as.integer(seq_len(nrow(primary)) %% 2L))
  list(ids = ids, primary = primary, heldout = observations[!selectable, ],
    reversals = reversal, human_scores = stats::setNames(seq_len(n) %% 4L, ids),
    prediction = stats::setNames(seq(-1.3, 1.3, length.out = n), ids),
    # Deliberately permute names to make alignment observable.
    sd = if (heterogeneous) stats::setNames(seq(0.2, 0.9, length.out = n), rev(ids)) else 0.5)
}

five_arm_modes <- c(cold = "cold", estimation = "btl_only", legacy_graph = "both",
  selection = "trueskill_only", full = "both")

five_arm_start <- function(f, arm, seed = 317L) {
  now <- function() as.POSIXct("2026-10-09", tz = "UTC")
  environment(now) <- baseenv()
  # Select the primary layer explicitly. Evaluation labels never enter either API.
  reservoir <- pairwiseLLM::make_adaptive_replay_reservoir(f$primary, f$ids)
  prior <- if (arm == "cold") NULL else
    pairwiseLLM::make_warm_start_prior(f$prediction, prior_sd = f$sd)
  pairwiseLLM::adaptive_rank_start(f$ids, seed = seed, now_fn = now,
    replay_reservoir = reservoir, warm_start_prior = prior,
    warm_start_mode = five_arm_modes[[arm]],
    warm_start_trueskill = if (arm %in% c("cold", "estimation")) NULL else "predictive_distribution",
    bootstrap_policy = if (arm %in% c("selection", "full")) "predictive_connected" else "shuffled_connected",
    adaptive_config = list(pairing_strategy = "trueskill_pollitt"))
}

five_arm_run <- function(state, f, steps = 1L) {
  if (steps == 0L) return(state)
  reservoir <- pairwiseLLM::make_adaptive_replay_reservoir(f$primary, f$ids)
  pairwiseLLM::adaptive_rank_run_live(state, pairwiseLLM::make_adaptive_judge_replay(reservoir),
    n_steps = steps, btl_config = list(refit_pairs_target = 5000L), progress = "none")
}

five_arm_invalid <- function(state, f) {
  judge <- function(...) list(is_valid = FALSE, invalid_reason = "synthetic retry")
  attributes(judge) <- attributes(pairwiseLLM::make_adaptive_judge_replay(
    pairwiseLLM::make_adaptive_replay_reservoir(f$primary, f$ids)))
  pairwiseLLM::adaptive_rank_run_live(state, judge, n_steps = 1L,
    btl_config = list(refit_pairs_target = 5000L), progress = "none")
}

five_arm_key <- function(a, b) paste(pmin(a, b), pmax(a, b), sep = "/")

five_arm_components <- function(state) {
  # Small independent reachability oracle, including isolated items at B=0.
  reach <- diag(TRUE, length(state$item_ids))
  history <- state$history_pairs
  for (k in seq_len(nrow(history))) {
    i <- match(history$A_id[[k]], state$item_ids)
    j <- match(history$B_id[[k]], state$item_ids)
    connected <- reach[i, ] | reach[j, ]
    reach[connected, connected] <- TRUE
  }
  nrow(unique(reach))
}

expect_five_arm_history <- function(state, f) {
  log <- state$step_log[!is.na(state$step_log$pair_id), ]
  keys <- five_arm_key(log$A_id, log$B_id)
  rows <- match(keys, five_arm_key(f$primary$A_id, f$primary$B_id))
  testthat::expect_false(anyNA(rows))
  testthat::expect_identical(anyDuplicated(keys), 0L)
  testthat::expect_identical(log$A_id, f$primary$A_id[rows])
  testthat::expect_identical(log$B_id, f$primary$B_id[rows])
  testthat::expect_identical(log$Y, f$primary$Y[rows])
  testthat::expect_identical(state$history_pairs$A_id, log$A_id)
  testthat::expect_identical(state$history_pairs$B_id, log$B_id)
  testthat::expect_equal(nrow(pairwiseLLM::adaptive_results_history(state)), nrow(log))
  testthat::expect_length(intersect(keys, five_arm_key(f$heldout$A_id, f$heldout$B_id)), 0L)
}

# Oracle uses only committed endpoints, frozen legal edges and scalar normal math.
# It does not call the package's degree, candidate, probability or distance helpers.
expect_five_arm_pollitt <- function(before, after, f) {
  history <- before$history_pairs
  degree <- stats::setNames(tabulate(match(c(history$A_id, history$B_id), f$ids),
    nbins = length(f$ids)), f$ids)
  unused <- f$primary[!five_arm_key(f$primary$A_id, f$primary$B_id) %in%
    five_arm_key(history$A_id, history$B_id), ]
  eligible <- unique(c(unused$A_id, unused$B_id))
  log <- tail(after$step_log, 1L)
  focal <- before$item_ids[[log$i]]
  partner <- before$item_ids[[log$j]]
  testthat::expect_equal(degree[[focal]], min(degree[eligible]))
  partners <- sort(c(unused$B_id[unused$A_id == focal], unused$A_id[unused$B_id == focal]))
  ts <- before$trueskill_state
  mu <- stats::setNames(ts$items$mu, ts$items$item_id)
  sigma <- stats::setNames(ts$items$sigma, ts$items$item_id)
  probability <- function(a, b) {
    stats::pnorm((mu[a] - mu[b]) / sqrt(sigma[a]^2 + sigma[b]^2 + 2 * ts$beta^2))
  }
  p <- probability(focal, partners)
  distance <- pmin(abs(p - 1 / 3), abs(p - 2 / 3))
  testthat::expect_identical(partner, partners[order(distance, partners)[[1L]]])
  testthat::expect_equal(log$p_ij, unname(probability(log$A_id, log$B_id)), tolerance = 1e-14)
  # Complementing the recorded orientation can differ by a few ulps near a target.
  testthat::expect_lte(abs(log$target_distance - min(distance)), 1e-14)
  testthat::expect_identical(log$deg_i, unname(degree[[focal]]))
  testthat::expect_identical(log$round_stage, "direct_pairing")
  testthat::expect_identical(log$pairing_strategy, "trueskill_pollitt")
}
