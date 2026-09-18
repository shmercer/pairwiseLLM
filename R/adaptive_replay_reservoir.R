# Sparse replay contracts. Unobserved outcomes never enter adaptive state.

.adaptive_reservoir_ordered_key <- function(a, b) {
  if (length(a) == 0L) return(character())
  .adaptive_replay_ordered_key(enc2utf8(a), enc2utf8(b))
}

.adaptive_reservoir_key <- function(a, b) {
  .adaptive_reservoir_ordered_key(pmin(a, b), pmax(a, b))
}

.adaptive_reservoir_tree <- function(edges, item_ids, seed = NULL) {
  order <- seq_len(nrow(edges))
  if (!is.null(seed)) order <- withr::with_seed(seed, sample.int(length(order)))
  parent <- seq_along(item_ids)
  root <- function(i) {
    while (parent[[i]] != i) i <- parent[[i]]
    i
  }
  a <- match(edges$A_id, item_ids)
  b <- match(edges$B_id, item_ids)
  selected <- integer(length(item_ids) - 1L)
  n <- 0L
  for (row in order) {
    ra <- root(a[[row]])
    rb <- root(b[[row]])
    if (ra == rb) next
    parent[[rb]] <- ra
    n <- n + 1L
    selected[[n]] <- row
    if (n == length(selected)) break
  }
  if (n != length(selected)) {
    rlang::abort("Replay reservoir graph is disconnected; bootstrap requires all panel items to be connected.")
  }
  selected
}

.adaptive_reservoir_manifest_hash <- function(item_ids, edges) {
  .warm_start_prior_hash(list(version = 1L, item_ids = item_ids,
    A_id = edges$A_id, B_id = edges$B_id))
}

#' Create a sparse frozen adaptive replay reservoir
#'
#' Each allowed unordered edge has exactly one observed presentation and binary
#' outcome. Subset to the primary observation layer before construction; exclude
#' held-out edges and separate reversal audits. The allowed graph must connect
#' every panel item. Reservoir replay supports ordinary within-set runs only.
#'
#' All strategies and warm-start modes share a seeded spanning-tree bootstrap of
#' `N - 1` allowed edges. Subsequent selection uses unused allowed edges, and
#' commits their stored presentation without reversing or complementing outcomes.
#' Consumption follows committed history, so discarded/failed transactions do
#' not consume observations. Existing statistical stopping rules still apply.
#'
#' State stores an outcome-free manifest and identity; retain the reservoir to
#' recreate the judge on resume. Identity includes the panel, edge membership,
#' presentation and Y, but excludes row order and ancillary metadata. Changed
#' identities are rejected before replay. Do not edit a constructed reservoir.
#' IDs that make distinct allowed edges collide in the existing colon-separated
#' adaptive history keys are rejected; use unambiguous panel IDs in that case.
#'
#' @inheritParams validate_adaptive_replay
#' @return A `pairwiseLLM_replay_reservoir` object for `replay_reservoir` in
#'   [adaptive_rank_start()] or [adaptive_rank()], and for
#'   [make_adaptive_judge_replay()].
#' @examples
#' ids <- c("a", "b", "c", "d")
#' frozen <- data.frame(A_id = c("b", "a", "d", "c"),
#'   B_id = c("a", "c", "a", "d"), Y = c(1L, 0L, 1L, 1L))
#' reservoir <- make_adaptive_replay_reservoir(frozen, ids)
#' state <- adaptive_rank_start(ids, seed = 42, replay_reservoir = reservoir,
#'   adaptive_config = list(pairing_strategy = "random"))
#' state <- adaptive_rank_run_live(state, make_adaptive_judge_replay(reservoir),
#'   n_steps = 4L, progress = "none")
#' @family adaptive ranking
#' @export
make_adaptive_replay_reservoir <- function(outcomes, item_ids) {
  outcomes <- validate_adaptive_replay(outcomes, item_ids, complete = FALSE)
  item_ids <- sort(unname(enc2utf8(item_ids)), method = "radix")
  outcomes$A_id <- enc2utf8(outcomes$A_id)
  outcomes$B_id <- enc2utf8(outcomes$B_id)
  keys <- .adaptive_reservoir_key(outcomes$A_id, outcomes$B_id)
  if (anyDuplicated(keys)) {
    rlang::abort("Replay reservoir requires exactly one observation per unordered edge, including reverse rows.")
  }
  # Canonical order uses panel indices, independent of input rows and locale.
  a <- match(outcomes$A_id, item_ids)
  b <- match(outcomes$B_id, item_ids)
  outcomes <- outcomes[order(pmin(a, b), pmax(a, b)), , drop = FALSE]
  edges <- outcomes[, c("A_id", "B_id")]
  .adaptive_reservoir_tree(edges, item_ids)
  # Existing adaptive history uses colon-separated keys. Reject ambiguous
  # manifests instead of allowing distinct observed edges to alias silently.
  if (anyDuplicated(make_unordered_key(edges$A_id, edges$B_id))) {
    rlang::abort("Replay reservoir IDs produce ambiguous adaptive history keys; use unambiguous panel IDs.")
  }
  manifest <- list(version = 1L, item_ids = item_ids, edges = edges,
    manifest_digest = .adaptive_reservoir_manifest_hash(item_ids, edges),
    digest = .warm_start_prior_hash(list(version = 1L, item_ids = item_ids,
      A_id = edges$A_id, B_id = edges$B_id, Y = outcomes$Y)))
  structure(list(manifest = manifest, outcomes = outcomes),
    class = "pairwiseLLM_replay_reservoir")
}

.adaptive_reservoir_validate <- function(reservoir) {
  if (!inherits(reservoir, "pairwiseLLM_replay_reservoir") || !is.list(reservoir) ||
    !is.list(reservoir$manifest)) {
    rlang::abort("`replay_reservoir` must be created by make_adaptive_replay_reservoir().")
  }
  rebuilt <- make_adaptive_replay_reservoir(reservoir$outcomes, reservoir$manifest$item_ids)
  if (!identical(rebuilt$manifest, reservoir$manifest)) {
    rlang::abort("Replay reservoir integrity mismatch; reconstruct it from the frozen observations.")
  }
  rebuilt
}

.adaptive_reservoir_active <- function(state) !is.null(state$replay_reservoir)

.adaptive_reservoir_bind <- function(state, reservoir) {
  if (is.null(reservoir)) return(state)
  reservoir <- .adaptive_reservoir_validate(reservoir)
  if (!setequal(state$item_ids, reservoir$manifest$item_ids)) {
    rlang::abort("Replay reservoir item IDs must match the adaptive panel exactly.")
  }
  state$replay_reservoir <- reservoir$manifest
  state$meta$replay_reservoir_digest <- reservoir$manifest$digest
  .adaptive_reservoir_check_mode(state)
  state
}

.adaptive_reservoir_check_mode <- function(state) {
  if (.adaptive_reservoir_active(state) &&
    !identical(state$controller$run_mode %||% "within_set", "within_set")) {
    rlang::abort("Replay reservoirs require ordinary `run_mode = \"within_set\"`; linking is unsupported.")
  }
}

.adaptive_reservoir_bootstrap <- function(state) {
  manifest <- state$replay_reservoir
  rows <- .adaptive_reservoir_tree(manifest$edges, manifest$item_ids, state$meta$seed)
  tibble::tibble(i_id = manifest$edges$A_id[rows], j_id = manifest$edges$B_id[rows])
}

.adaptive_reservoir_assert_edge <- function(state, A_id, B_id) {
  if (!.adaptive_reservoir_active(state)) return(invisible(NULL))
  edges <- state$replay_reservoir$edges
  if (length(A_id) != 1L || length(B_id) != 1L || anyNA(c(A_id, B_id)) ||
    !any(edges$A_id == A_id & edges$B_id == B_id)) {
    rlang::abort("Replay reservoir selection must use an allowed edge in its frozen observed orientation.")
  }
  key <- .adaptive_reservoir_key(A_id, B_id)
  history <- state$history_pairs
  if (key %in% .adaptive_reservoir_key(history$A_id, history$B_id)) {
    rlang::abort("Replay reservoir unordered edge already committed; each edge may be used only once.")
  }
  invisible(NULL)
}

.adaptive_reservoir_validate_state <- function(state, metadata = NULL) {
  manifest <- state$replay_reservoir
  if (is.null(manifest)) {
    if (!is.null(state$meta$replay_reservoir_digest) || !is.null(metadata$replay_reservoir_digest)) {
      rlang::abort("Replay reservoir manifest is missing from the saved state.")
    }
    return(invisible(NULL))
  }
  .adaptive_reservoir_check_mode(state)
  if (!is.list(manifest) || !identical(manifest$version, 1L) ||
    !identical(sort(enc2utf8(state$item_ids), method = "radix"), manifest$item_ids) ||
    !is.data.frame(manifest$edges) || !identical(names(manifest$edges), c("A_id", "B_id")) ||
    !is.character(manifest$digest) || length(manifest$digest) != 1L ||
    is.na(manifest$digest) || !nzchar(manifest$digest) ||
    !identical(state$meta$replay_reservoir_digest, manifest$digest)) {
    rlang::abort("Replay reservoir state integrity mismatch.")
  }
  # Reuse graph/ID validation without importing any unconsumed outcomes.
  dummy <- manifest$edges
  dummy$Y <- rep(0L, nrow(dummy))
  canonical <- make_adaptive_replay_reservoir(dummy, manifest$item_ids)$manifest
  if (!identical(canonical$edges, manifest$edges) ||
    !identical(canonical$manifest_digest, manifest$manifest_digest) ||
    (!is.null(metadata) && (!identical(metadata$replay_reservoir_digest, manifest$digest) ||
      !identical(metadata$replay_manifest_digest, manifest$manifest_digest)))) {
    rlang::abort("Replay reservoir manifest or session metadata integrity mismatch.")
  }
  history <- state$history_pairs
  observed <- if (nrow(history) == 0L) character() else
    .adaptive_reservoir_ordered_key(history$A_id, history$B_id)
  allowed <- .adaptive_reservoir_ordered_key(manifest$edges$A_id, manifest$edges$B_id)
  if (anyDuplicated(observed) || any(!observed %in% allowed)) {
    rlang::abort("Replay reservoir committed history contains repeated, foreign, or reversed edges.")
  }
  committed <- state$step_log[!is.na(state$step_log$pair_id), , drop = FALSE]
  logged <- if (nrow(committed) == 0L) character() else
    .adaptive_reservoir_ordered_key(committed$A_id, committed$B_id)
  if (!identical(logged, observed) || anyNA(committed$Y) || any(!committed$Y %in% c(0L, 1L))) {
    rlang::abort("Replay reservoir committed log and history integrity mismatch.")
  }
  bootstrap <- .adaptive_reservoir_bootstrap(state)
  n_done <- min(nrow(history), nrow(bootstrap))
  if (!identical(state$warm_start_pairs, bootstrap) ||
    !identical(state$warm_start_idx, as.integer(n_done + 1L)) ||
    !identical(state$warm_start_done, n_done == nrow(bootstrap)) ||
    !identical(observed[seq_len(n_done)],
      .adaptive_reservoir_ordered_key(bootstrap$i_id, bootstrap$j_id)[seq_len(n_done)])) {
    rlang::abort("Replay reservoir bootstrap progress integrity mismatch.")
  }
  invisible(NULL)
}

.adaptive_reservoir_check_judge <- function(state, judge, validate_history = TRUE) {
  identity <- attr(judge, "replay_reservoir_digest", exact = TRUE)
  if (!identical(identity, state$meta$replay_reservoir_digest)) {
    rlang::abort("Replay reservoir judge identity mismatch; recreate the judge from the saved reservoir.")
  }
  if (.adaptive_reservoir_active(state) && validate_history) {
    validate <- attr(judge, "replay_reservoir_validate_history", exact = TRUE)
    if (!is.function(validate)) rlang::abort("A matching reservoir replay judge is required.")
    validate(state)
  }
}

.adaptive_reservoir_judge <- function(reservoir, item_ids, strict_use) {
  if (!strict_use) rlang::abort("Reservoir replay requires `strict_use = TRUE`.")
  reservoir <- .adaptive_reservoir_validate(reservoir)
  if (!setequal(item_ids, reservoir$manifest$item_ids) || anyDuplicated(item_ids)) {
    rlang::abort("Replay reservoir item IDs must match the panel exactly.")
  }
  outcomes <- reservoir$outcomes
  identity <- reservoir$manifest$digest
  judge <- function(A, B, state = NULL, ...) {
    if (!inherits(state, "adaptive_state") ||
      !identical(state$meta$replay_reservoir_digest, identity)) {
      rlang::abort("Reservoir replay requires a state bound to the same reservoir identity.")
    }
    if (!is.data.frame(A) || !is.data.frame(B) || nrow(A) != 1L || nrow(B) != 1L ||
      !"item_id" %in% names(A) || !"item_id" %in% names(B)) {
      rlang::abort("Replay A and B must be one-row data frames containing item_id.")
    }
    .adaptive_reservoir_assert_edge(state, A$item_id, B$item_id)
    row <- which(outcomes$A_id == A$item_id & outcomes$B_id == B$item_id)
    if (length(row) != 1L) rlang::abort("No frozen outcome for the requested reservoir orientation.")
    list(is_valid = TRUE, Y = outcomes$Y[[row]], invalid_reason = NA_character_, judge_backend = "replay")
  }
  attr(judge, "replay_reservoir_digest") <- identity
  attr(judge, "replay_reservoir_validate_history") <- function(state) {
    committed <- state$step_log[!is.na(state$step_log$pair_id), , drop = FALSE]
    if (nrow(committed) > 0L) {
      rows <- match(.adaptive_reservoir_ordered_key(committed$A_id, committed$B_id),
        .adaptive_reservoir_ordered_key(outcomes$A_id, outcomes$B_id))
      if (anyNA(rows) || !identical(committed$Y, outcomes$Y[rows])) {
        rlang::abort("Replay reservoir committed outcomes differ from the frozen observations.")
      }
    }
    invisible(NULL)
  }
  judge
}
