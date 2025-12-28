# Internal helpers: state schema template/alignment for runners
#' @keywords internal
.bt_state_template <- function() {
  tibble::tibble(
    batch_index = integer(),
    round_index = integer(),
    stage = character(),
    stop = logical(),
    stop_reason = character(),
    n_new_ids = integer(),
    n_results = integer(),
    n_unique_unordered_pairs = integer(),
    n_unique_unordered_pairs_in_ids = integer(),
    n_ids = integer(),
    n_ids_seen = integer(),
    min_appearances = double(),
    p10_appearances = double(),
    median_appearances = double(),
    p90_appearances = double(),
    max_appearances = double(),
    appear_p50 = double(),
    appear_p90 = double(),
    appear_p95 = double(),
    pos_imbalance_max = double(),
    n_self_pairs = integer(),
    n_missing_better_id = integer(),
    n_judges = integer(),
    new_n_results = integer(),
    new_n_unique_unordered_pairs = integer(),
    new_n_unique_unordered_pairs_in_ids = integer(),
    new_n_ids = integer(),
    new_n_ids_seen = integer(),
    new_min_appearances = double(),
    new_p10_appearances = double(),
    new_median_appearances = double(),
    new_p90_appearances = double(),
    new_max_appearances = double(),
    new_appear_p50 = double(),
    new_appear_p90 = double(),
    new_appear_p95 = double(),
    new_pos_imbalance_max = double(),
    new_n_self_pairs = integer(),
    new_n_missing_better_id = integer(),
    new_n_judges = integer()
  )
}

#' @keywords internal
.bt_align_state <- function(state) {
  tmpl <- .bt_state_template()

  if (is.null(state)) {
    return(tmpl)
  }
  state <- tibble::as_tibble(state)
  if (nrow(state) == 0L) {
    return(tmpl)
  }

  # Add any missing template columns with correct NA type
  for (nm in setdiff(names(tmpl), names(state))) {
    proto <- tmpl[[nm]]
    if (is.integer(proto)) {
      state[[nm]] <- rep(NA_integer_, nrow(state))
    } else if (is.double(proto)) {
      state[[nm]] <- rep(NA_real_, nrow(state))
    } else if (is.logical(proto)) {
      state[[nm]] <- rep(NA, nrow(state))
    } else if (is.character(proto)) {
      state[[nm]] <- rep(NA_character_, nrow(state))
    } else {
      state[[nm]] <- rep(NA, nrow(state))
    }
  }

  # Coerce common columns to template types (best-effort)
  for (nm in intersect(names(tmpl), names(state))) {
    proto <- tmpl[[nm]]
    x <- state[[nm]]
    if (is.integer(proto) && !is.integer(x)) state[[nm]] <- suppressWarnings(as.integer(x))
    if (is.double(proto) && !is.double(x)) state[[nm]] <- suppressWarnings(as.double(x))
    if (is.logical(proto) && !is.logical(x)) state[[nm]] <- suppressWarnings(as.logical(x))
    if (is.character(proto) && !is.character(x)) state[[nm]] <- suppressWarnings(as.character(x))
  }

  # Canonical order first, preserve any extra columns afterwards
  dplyr::select(state, dplyr::all_of(names(tmpl)), dplyr::everything())
}
