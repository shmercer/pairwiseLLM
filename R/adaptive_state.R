# -------------------------------------------------------------------------
# Adaptive v2 state constructor.
# -------------------------------------------------------------------------

.adaptive_state_normalize_items <- function(items) {
  if (is.null(items)) {
    rlang::abort("`items` must be provided.")
  }
  if (is.vector(items) && !is.list(items)) {
    items <- tibble::tibble(item_id = items)
  } else if (is.data.frame(items)) {
    items <- tibble::as_tibble(items)
  } else {
    rlang::abort("`items` must be a vector or data frame.")
  }

  if (!"item_id" %in% names(items)) {
    if ("id" %in% names(items)) {
      items$item_id <- items$id
    } else if ("ID" %in% names(items)) {
      items$item_id <- items$ID
    } else {
      rlang::abort("`items` must include an `item_id` column.")
    }
  }

  items <- dplyr::relocate(items, "item_id")
  item_id <- as.character(items$item_id)
  if (any(is.na(item_id) | item_id == "")) {
    rlang::abort("`items$item_id` must be non-missing.")
  }
  if (anyDuplicated(item_id)) {
    rlang::abort("`items$item_id` must be unique.")
  }
  items$item_id <- item_id

  items
}

#' @keywords internal
#' @noRd
new_adaptive_state <- function(items, now_fn = function() Sys.time()) {
  force(now_fn)
  if (!is.function(now_fn)) {
    rlang::abort("`now_fn` must be a function.")
  }
  items <- .adaptive_state_normalize_items(items)
  item_ids <- as.character(items$item_id)
  item_index <- stats::setNames(seq_along(item_ids), item_ids)
  history_pairs <- tibble::tibble(
    A_id = character(),
    B_id = character()
  )

  state <- structure(
    list(
      item_ids = item_ids,
      item_index = item_index,
      n_items = as.integer(length(item_ids)),
      items = items,
      history_pairs = history_pairs,
      step_log = new_step_log(now_fn = now_fn),
      round_log = new_round_log(),
      item_log = new_item_log(items),
      trueskill_state = new_trueskill_state(items),
      btl_fit = NULL,
      config = list(),
      meta = list(schema_version = "v2-0", now_fn = now_fn, seed = 1L)
    ),
    class = "adaptive_state"
  )

  state
}
