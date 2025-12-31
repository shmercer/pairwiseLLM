.bt_apply_exhaustion_fallback <- function(
  pairs,
  samples,
  core_ids,
  new_ids,
  seen_ids,
  round_size,
  forbidden_keys,
  exhaustion_fallback = c("none", "cross_batch_new_new", "targeted_repeats", "both"),
  exhaustion_min_pairs_frac = 0.5,
  exhaustion_spectral_gap_threshold = 0,
  ...,
  within_batch_frac,
  core_audit_frac,
  k_neighbors,
  balance_positions,
  include_text,
  forbid_repeats,
  verbose
) {
  exhaustion_fallback <- match.arg(exhaustion_fallback)

  # Nothing to do.
  if (identical(exhaustion_fallback, "none")) {
    return(pairs)
  }
  if (nrow(pairs) >= ceiling(round_size * exhaustion_min_pairs_frac)) {
    return(pairs)
  }

  # For now, spectral-gap gating is a no-op unless caller provides a value > 0.
  # (This is intentionally conservative; fallback should be opt-in by `exhaustion_fallback`.)
  if (isTRUE(exhaustion_spectral_gap_threshold > 0)) {
    # We don't currently have a stable, engine-agnostic spectral-gap signal available
    # at this point in the runner. Keep the hook for future expansion.
    # If the threshold is positive, still allow fallback; do not short-circuit.
  }

  n_need <- max(1L, ceiling(round_size * exhaustion_min_pairs_frac))
  n_have <- nrow(pairs)
  n_add <- max(0L, n_need - n_have)
  if (n_add <= 0L) {
    return(pairs)
  }

  # Helper to create unordered pair keys compatible with `forbidden_keys`.

  make_cross_batch_new_new <- function() {
    extra_ids <- setdiff(unique(seen_ids), c(core_ids, new_ids))
    if (length(extra_ids) == 0) {
      return(NULL)
    }

    cand <- tidyr::expand_grid(ID1 = new_ids, ID2 = extra_ids)
    cand <- dplyr::filter(cand, .data$ID1 != .data$ID2)
    if (nrow(cand) == 0) {
      return(NULL)
    }

    cand$key <- .unordered_pair_key(cand$ID1, cand$ID2)
    if (length(forbidden_keys) > 0) {
      cand <- dplyr::filter(cand, !(.data$key %in% forbidden_keys))
    }
    cand$key <- NULL
    if (nrow(cand) == 0) {
      return(NULL)
    }

    # Sample up to the remaining needed pairs.
    take <- min(n_add, nrow(cand))
    idx <- sample.int(nrow(cand), size = take)
    out <- cand[idx, , drop = FALSE]
    out$pair_type <- "fallback_cross_batch_new_new"
    out
  }

  make_targeted_repeats <- function() {
    # Last resort: allow repeats among the current batch new IDs.
    if (length(new_ids) < 2) {
      return(NULL)
    }
    cand <- t(utils::combn(new_ids, 2))
    cand <- tibble::tibble(ID1 = cand[, 1], ID2 = cand[, 2])
    if (nrow(cand) == 0) {
      return(NULL)
    }

    # If repeats are forbidden, do nothing.
    if (isTRUE(forbid_repeats)) {
      return(NULL)
    }

    take <- min(n_add, nrow(cand))
    idx <- sample.int(nrow(cand), size = take)
    out <- cand[idx, , drop = FALSE]
    out$pair_type <- "fallback_targeted_repeats"
    out
  }

  fb <- NULL
  if (identical(exhaustion_fallback, "cross_batch_new_new")) {
    fb <- make_cross_batch_new_new()
  } else if (identical(exhaustion_fallback, "targeted_repeats")) {
    fb <- make_targeted_repeats()
  } else if (identical(exhaustion_fallback, "both")) {
    fb <- make_cross_batch_new_new()
    if (is.null(fb) || nrow(fb) == 0) fb <- make_targeted_repeats()
  }

  if (is.null(fb) || nrow(fb) == 0) {
    return(pairs)
  }

  # Attach texts if requested (mirrors `bt_core_link_round`).
  if (isTRUE(include_text)) {
    fb <- add_pair_texts(fb, samples)
  }

  # Combine and return.
  out <- dplyr::bind_rows(pairs, fb)
  out
}
