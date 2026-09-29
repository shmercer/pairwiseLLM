# Run from the package root at the pre-fix commit. No providers or fitting.
baseline_sha <- "565dad453407a84bdd31321a9f50bf0d067b9c91"
stopifnot(identical(system2("git", c("rev-parse", "HEAD"), stdout = TRUE), baseline_sha))
stopifnot(length(system2("git", c("diff", "--name-only", "--", "R"), stdout = TRUE)) == 0L)
pkgload::load_all(".", quiet = TRUE)

fixture_dir <- "tests/testthat/fixtures/selector-296"
dir.create(fixture_dir, recursive = TRUE, showWarnings = FALSE)
now_fn <- function() as.POSIXct("2026-09-29", tz = "UTC")
environment(now_fn) <- baseenv()
make_state <- function(kind = "sparse") {
  n <- if (kind == "exhausted") 2L else 6L
  state <- pairwiseLLM:::new_adaptive_state(as.character(seq_len(n)), now_fn = now_fn)
  state$trueskill_state <- pairwiseLLM:::new_trueskill_state(tibble::tibble(
    item_id = as.character(seq_len(n)), mu = rep(25, n), sigma = rep(25 / 3, n)
  ))
  if (kind == "exhausted") {
    history <- tibble::tibble(A_id = rep("1", 3L), B_id = rep("2", 3L))
  } else {
    pairs <- if (kind == "relaxed") {
      t(utils::combn(as.character(2:6), 2L))
    } else {
      cbind(as.character(2:6), as.character(c(3:6, 2)))
    }
    history <- tibble::tibble(A_id = rep(pairs[, 1L], each = 2L), B_id = rep(pairs[, 2L], each = 2L))
    if (kind != "quota") {
      history <- dplyr::bind_rows(tibble::tibble(A_id = c("1", "1"), B_id = c("2", "2")), history)
    }
  }
  state$history_pairs <- history
  state$history_state <- pairwiseLLM:::.adaptive_history_state_rebuild(history, state$item_ids)
  state$warm_start_pairs <- tibble::tibble(i_id = character(), j_id = character())
  state$warm_start_idx <- 1L
  state$warm_start_done <- TRUE
  state$meta$seed <- 10L
  state$round$staged_active <- TRUE
  state$round$stage_index <- match("local_link", state$round$stage_order)
  state$round$per_round_item_uses[["1"]] <- 1L
  state$round$repeat_in_round_used <- state$round$repeat_in_round_budget
  state
}
states <- stats::setNames(lapply(c("sparse", "relaxed", "quota", "exhausted"), make_state),
  c("sparse", "relaxed", "quota", "exhausted"))
# Freeze a seed that takes coverage override at every fallback stage.
for (seed in seq_len(100000L)) {
  quota_draws <- vapply(seq_len(5L), function(stage) {
    pairwiseLLM:::.adaptive_with_seed(
      pairwiseLLM:::.adaptive_stage_seed(seed, 1L, stage, 1L), stats::runif(1L)
    )
  }, numeric(1L))
  if (all(quota_draws < 0.20)) break
}
stopifnot(all(quota_draws < 0.20))
states$quota$meta$seed <- seed
failures <- lapply(states, function(state) pairwiseLLM:::select_next_pair(state, step_id = 1L))
stopifnot(all(vapply(failures, function(out) out$candidate_starved, logical(1L))))

successful <- list()
for (kind in c("sparse", "relaxed", "quota", "identified")) {
  for (seed in seq_len(12L)) {
    state <- states[[if (kind == "identified") "sparse" else kind]]
    state$meta$seed <- seed
    if (kind == "identified") state$controller$global_identified <- TRUE
    out <- pairwiseLLM:::select_next_pair(state, step_id = 1L)
    if (!out$candidate_starved) {
      successful[[paste(kind, seed, sep = "-")]] <- list(state = state, selection = out)
    }
  }
}
# Add successful exploration with all endpoints represented.
state <- states$sparse
state$history_pairs <- tibble::tibble(A_id = character(), B_id = character())
state$history_state <- pairwiseLLM:::.adaptive_history_state_rebuild(state$history_pairs, state$item_ids)
state$round$per_round_item_uses[] <- 0L
for (seed in seq_len(12L)) {
  state$meta$seed <- seed
  successful[[paste0("fresh-", seed)]] <- list(
    state = state, selection = pairwiseLLM:::select_next_pair(state, step_id = 1L)
  )
}
saveRDS(list(sha = baseline_sha, states = states, failures = failures, successful = successful),
  file.path(fixture_dir, "baseline.rds"), compress = "xz", version = 3L)
session_state <- states$sparse
for (idx in seq_len(nrow(session_state$history_pairs))) {
  A <- as.integer(session_state$history_pairs$A_id[[idx]])
  B <- as.integer(session_state$history_pairs$B_id[[idx]])
  session_state$step_log <- pairwiseLLM:::append_step_log(session_state$step_log, list(
    step_id = idx, pair_id = idx, timestamp = now_fn(), status = "ok", Y = 1L,
    i = min(A, B), j = max(A, B), A = A, B = B,
    A_id = as.character(A), B_id = as.character(B), is_probe_step = FALSE
  ))
}
pairwiseLLM::save_adaptive_session(session_state, file.path(fixture_dir, "session"))
cat("Saved", length(successful), "successful baselines; quota seed", states$quota$meta$seed, "\n")
