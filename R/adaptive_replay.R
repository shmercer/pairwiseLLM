# Length-prefix the first ID so punctuation in IDs cannot alias another pair.
# Existing presentation/history keys remain unchanged.
.adaptive_replay_ordered_key <- function(A_id, B_id) {
  paste0(nchar(A_id, type = "bytes"), ":", A_id, B_id)
}

.adaptive_replay_flag <- function(value, name) {
  if (!is.logical(value) || length(value) != 1L || is.na(value) || !is.null(dim(value))) {
    rlang::abort(paste0("`", name, "` must be TRUE or FALSE."))
  }
}

.adaptive_replay_ids <- function(ids, name) {
  if (!is.character(ids) || !is.null(dim(ids)) || anyNA(ids) || any(!nzchar(trimws(ids)))) {
    rlang::abort(paste0("`", name, "` must contain non-missing, non-blank character IDs."))
  }
}

#' Validate frozen directed judgments for an adaptive panel
#'
#' Validate one primary observation per ordered pair. No rows are dropped or
#' reordered; optional metadata columns are retained and do not determine lookup.
#' Subset to one panel and one primary judgment layer before validation.
#'
#' @param outcomes Data frame with character `A_id`, character `B_id`, and binary
#'   `Y` (one means presented A wins). `Y` accepts logical, numeric zero/one, or
#'   character `"0"`/`"1"`; factors, missing values, and other values are rejected.
#' @param item_ids Unique non-blank character IDs for the active panel, with at
#'   least two items. IDs must match the adaptive state's item IDs.
#' @param complete Logical; require all `N * (N - 1)` ordered pairs, including both
#'   orientations of every unordered pair. Default `TRUE`. `FALSE` permits a
#'   partial table; requests for absent orientations still fail during replay.
#' @return A tibble retaining the input rows and metadata, with integer `Y`.
#' @examples
#' outcomes <- data.frame(A_id = c("a", "b"), B_id = c("b", "a"), Y = c(1L, 1L))
#' validate_adaptive_replay(outcomes, item_ids = c("a", "b"))
#' @family adaptive ranking
#' @seealso [make_adaptive_judge_replay()]
#' @export
validate_adaptive_replay <- function(outcomes, item_ids, complete = TRUE) {
  .adaptive_replay_flag(complete, "complete")
  .adaptive_replay_ids(item_ids, "item_ids")
  if (length(item_ids) < 2L || anyDuplicated(item_ids)) {
    rlang::abort("`item_ids` must contain at least two unique IDs; remove duplicate panel IDs.")
  }
  required <- c("A_id", "B_id", "Y")
  if (!is.data.frame(outcomes) || !all(required %in% names(outcomes)) ||
    anyDuplicated(names(outcomes))) {
    rlang::abort("`outcomes` must be a data frame with unique column names including A_id, B_id, Y.")
  }
  out <- tibble::as_tibble(outcomes)
  .adaptive_replay_ids(out$A_id, "outcomes$A_id")
  .adaptive_replay_ids(out$B_id, "outcomes$B_id")
  fail_row <- function(mask, reason) {
    row <- which(mask)[[1L]]
    rlang::abort(paste0(reason, " at row ", row, ": (", out$A_id[[row]],
      ", ", out$B_id[[row]], ")."))
  }
  foreign <- !out$A_id %in% item_ids | !out$B_id %in% item_ids
  if (any(foreign)) {
    fail_row(foreign, "Outcome ID does not belong to the active panel")
  }
  self <- out$A_id == out$B_id
  if (any(self)) {
    fail_row(self, "Self-pair is not allowed")
  }
  y <- out$Y
  if (!(is.numeric(y) || is.logical(y) || is.character(y)) ||
    is.object(y) || !is.null(dim(y))) {
    rlang::abort("`outcomes$Y` must be a binary logical, numeric, or character vector.")
  }
  invalid_y <- is.na(y) | !y %in% c(0, 1)
  if (any(invalid_y)) {
    fail_row(invalid_y, "Y must be exactly 0 or 1")
  }
  out$Y <- as.integer(y)
  keys <- .adaptive_replay_ordered_key(out$A_id, out$B_id)
  duplicate <- duplicated(keys)
  if (any(duplicate)) {
    fail_row(duplicate, "Duplicate ordered key; keep exactly one primary judgment")
  }
  n <- length(item_ids)
  if (complete && nrow(out) != as.double(n) * (n - 1)) {
    counts <- tabulate(match(out$A_id, item_ids), nbins = n)
    a <- item_ids[[which(counts != n - 1L)[[1L]]]]
    b <- setdiff(item_ids, c(a, out$B_id[out$A_id == a]))[[1L]]
    rlang::abort(paste0("Incomplete directed matrix: expected ", as.double(n) * (n - 1),
      " rows; found ", nrow(out), ". Missing ordered key: (", a, ", ", b, ")."))
  }
  out
}

#' Create an offline judge from frozen directed outcomes
#'
#' Replay the stored result for the exact presented `(A_id, B_id)`. Reverse
#' judgments are independent stored observations: they are never inferred by
#' complementing the forward result. No provider calls, random draws, or
#' step-dependent outcomes are used.
#'
#' For study runs, set `adaptive_config = list(dup_max_obs_relaxed = 2L)` when
#' creating the adaptive state. This prevents hybrid's relaxed third observation
#' at selection time. Normal presentation balancing and repeat reversal remain
#' active. Direct strategies already cap unordered pairs at two observations.
#'
#' Create a fresh judge for each independent replicate. Strict use records each
#' successful lookup in the closure, even if the caller subsequently discards the
#' updated state. The judge is not saved in an adaptive session. To resume, create
#' a new judge from the same matrix and pass the loaded state to the runner;
#' strict use also rejects keys already present in that state's committed history.
#' The matrix and its provenance must be retained separately by the caller.
#'
#' @inheritParams validate_adaptive_replay
#' @param strict_use Logical; reject repeated use of an exact ordered judgment.
#'   Default `TRUE`. `FALSE` permits repeated lookups for non-study inspection.
#' @return A function `judge(A, B, state = NULL, ...)` compatible with
#'   [adaptive_rank_run_live()]. A and B are one-row data frames with `item_id`.
#'   The result contains `is_valid = TRUE`, integer `Y`, and
#'   `judge_backend = "replay"`. Missing keys and strict reuse raise errors.
#' @examples
#' ids <- c("a", "b", "c")
#' outcomes <- expand.grid(A_id = ids, B_id = ids, stringsAsFactors = FALSE)
#' outcomes <- outcomes[outcomes$A_id != outcomes$B_id, ]
#' outcomes$Y <- as.integer(outcomes$A_id < outcomes$B_id)
#' judge <- make_adaptive_judge_replay(outcomes, ids)
#' state <- adaptive_rank_start(ids, seed = 42,
#'   adaptive_config = list(dup_max_obs_relaxed = 2L))
#' state <- adaptive_rank_run_live(state, judge, n_steps = 3L, progress = "none")
#' @family adaptive ranking
#' @seealso [validate_adaptive_replay()], [adaptive_rank()]
#' @export
make_adaptive_judge_replay <- function(outcomes, item_ids, strict_use = TRUE, complete = TRUE) {
  .adaptive_replay_flag(strict_use, "strict_use")
  outcomes <- validate_adaptive_replay(outcomes, item_ids, complete = complete)
  # Force the panel binding now, so later caller changes cannot alter a judge.
  item_ids <- as.character(item_ids)
  keys <- .adaptive_replay_ordered_key(outcomes$A_id, outcomes$B_id)
  lookup <- list2env(stats::setNames(as.list(seq_along(keys)), keys), parent = emptyenv())
  usage <- new.env(parent = emptyenv())
  usage$used <- rep(FALSE, nrow(outcomes))
  read_item <- function(item, name) {
    if (!is.data.frame(item) || nrow(item) != 1L || !"item_id" %in% names(item)) {
      rlang::abort(paste0("`", name, "` must be a one-row data frame containing item_id."))
    }
    .adaptive_replay_ids(item$item_id, paste0(name, "$item_id"))
    item$item_id[[1L]]
  }
  function(A, B, state = NULL, ...) {
    a <- read_item(A, "A")
    b <- read_item(B, "B")
    if (!is.null(state) && (!inherits(state, "adaptive_state") ||
      length(state$item_ids) != length(item_ids) || !setequal(state$item_ids, item_ids))) {
      rlang::abort("Replay state item IDs must match the active panel exactly.")
    }
    key <- .adaptive_replay_ordered_key(a, b)
    row <- get0(key, envir = lookup, inherits = FALSE)
    if (is.null(row)) {
      rlang::abort(paste0("No frozen outcome for ordered key: (", a, ", ", b, ")."))
    }
    history <- state$history_pairs
    in_history <- !is.null(history) && any(history$A_id == a & history$B_id == b)
    if (strict_use && (usage$used[[row]] || isTRUE(in_history))) {
      rlang::abort(paste0("Frozen ordered judgment already used: (", a, ", ", b,
        "). Use each orientation at most once per study run."))
    }
    usage$used[[row]] <- TRUE
    list(is_valid = TRUE, Y = outcomes$Y[[row]], invalid_reason = NA_character_,
      judge_backend = "replay")
  }
}
