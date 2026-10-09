# Bootstrap configuration is separate from predictive destinations and later pairing.
.adaptive_bootstrap_policy <- function(policy) {
  if (!is.character(policy) || length(policy) != 1L || !is.null(dim(policy)) ||
      is.na(policy) || !policy %in% c("shuffled_connected", "predictive_connected")) {
    rlang::abort("`bootstrap_policy` must be shuffled_connected or predictive_connected.")
  }
  unname(policy)
}

.adaptive_bootstrap_saved_policy <- function(state) {
  .adaptive_bootstrap_policy(state$meta$bootstrap_policy %||% "shuffled_connected")
}

.adaptive_bootstrap_check_inputs <- function(state, policy, trueskill) {
  if (policy == "predictive_connected" &&
      (!.adaptive_reservoir_active(state) ||
       !identical(state$controller$run_mode, "within_set") ||
       !identical(unname(trueskill), "predictive_distribution") ||
       !identical(.adaptive_pairing_strategy(state), "trueskill_pollitt"))) {
    rlang::abort(paste0("Predictive bootstrap requires a selectable replay reservoir, ",
      "within_set mode, warm_start_trueskill = predictive_distribution, and ",
      "pairing_strategy = trueskill_pollitt."))
  }
  invisible(NULL)
}

.adaptive_bootstrap_hash <- function(record, pairs) {
  # Match the prior's portable XDR convention without changing historical hashes.
  record$digest <- NULL
  bytes <- serialize(list(record = record, pairs = pairs), NULL, version = 2, xdr = TRUE)
  as.character(openssl::sha256(bytes[-(7:14)]))
}

.adaptive_bootstrap_init <- function(state, policy) {
  state$meta$bootstrap_policy <- policy
  state$meta$bootstrap_policy_version <- 1L
  if (policy == "shuffled_connected") {
    state$warm_start_pairs <- if (.adaptive_reservoir_active(state)) {
      .adaptive_reservoir_bootstrap(state)
    } else {
      .adaptive_build_warm_start_pairs(state$item_ids, state$meta$seed)
    }
  } else {
    .warm_start_adaptive_validate(state)
    .adaptive_bootstrap_check_inputs(state, policy, state$meta$warm_start_trueskill)
    initial <- .warm_start_trueskill_distribution(state$predictive_prior, state$item_ids)
    tree_policy <- .adaptive_predictive_tree_policy()
    pairs <- .adaptive_predictive_tree(state$item_ids, state$replay_reservoir$edges,
      initial, state$meta$seed, tree_policy)
    diagnostics <- attr(pairs, "tree_diagnostics")
    attr(pairs, "tree_diagnostics") <- NULL
    state$warm_start_pairs <- pairs
    record <- list(format_version = 1L, policy = policy, hash_algorithm = "sha256-xdr2-v1",
      seed = state$meta$seed, item_ids = state$item_ids,
      predictive_prior_digest = state$predictive_prior$digest,
      trueskill_mapping = state$meta$trueskill_mapping, initial_prediction = initial,
      manifest_digest = state$replay_reservoir$manifest_digest,
      tree_policy = tree_policy, diagnostics = diagnostics)
    record$digest <- .adaptive_bootstrap_hash(record, pairs)
    state$bootstrap <- record
    state$meta$bootstrap_digest <- record$digest
  }
  state$warm_start_idx <- 1L
  state$warm_start_done <- nrow(state$warm_start_pairs) == 0L
  state
}

.adaptive_bootstrap_validate <- function(state, metadata = NULL) {
  fields <- c("bootstrap_policy", "bootstrap_policy_version", "bootstrap_digest")
  values <- lapply(fields, function(field) state$meta[[field]])
  if (!is.null(metadata) &&
      !identical(values, lapply(fields, function(field) metadata[[field]]))) {
    rlang::abort("Session metadata bootstrap integrity mismatch.")
  }
  # Only a completely absent descriptor is a legacy shuffled session.
  if (all(vapply(values, is.null, logical(1)))) {
    if (!is.null(state$bootstrap)) rlang::abort("Bootstrap policy metadata is missing.")
    return(invisible(NULL))
  }
  policy <- .adaptive_bootstrap_policy(state$meta$bootstrap_policy)
  if (!identical(state$meta$bootstrap_policy_version, 1L)) {
    rlang::abort("Unsupported or missing bootstrap policy version.")
  }
  record <- state$bootstrap
  if (policy == "shuffled_connected") {
    if (!is.null(record) || !is.null(state$meta$bootstrap_digest)) {
      rlang::abort("Shuffled bootstrap conflicts with predictive bootstrap identity.")
    }
    return(invisible(NULL))
  }
  .adaptive_bootstrap_check_inputs(state, policy, state$meta$warm_start_trueskill)
  .warm_start_adaptive_validate(state)
  expected_fields <- c("format_version", "policy", "hash_algorithm", "seed", "item_ids",
    "predictive_prior_digest", "trueskill_mapping", "initial_prediction", "manifest_digest",
    "tree_policy", "diagnostics", "digest")
  if (!is.list(record) || !identical(names(record), expected_fields) ||
      !identical(record$format_version, 1L) || !identical(record$policy, policy) ||
      !identical(record$hash_algorithm, "sha256-xdr2-v1") ||
      !identical(record$digest, state$meta$bootstrap_digest) ||
      !identical(record$digest, .adaptive_bootstrap_hash(record, state$warm_start_pairs))) {
    rlang::abort("Predictive bootstrap record or queue digest integrity mismatch.")
  }
  seed <- state$meta$initialization_seed %||% state$meta$seed
  if (!is.integer(seed) || length(seed) != 1L || is.na(seed) || !identical(record$seed, seed) ||
      !identical(record$item_ids, state$item_ids) ||
      !identical(state$items$item_id, state$item_ids) ||
      !identical(state$item_index, stats::setNames(seq_along(state$item_ids), state$item_ids)) ||
      !identical(record$predictive_prior_digest, state$predictive_prior$digest) ||
      !identical(record$trueskill_mapping, state$meta$trueskill_mapping) ||
      !identical(record$initial_prediction,
        .warm_start_trueskill_distribution(state$predictive_prior, state$item_ids)) ||
      !identical(record$manifest_digest, state$replay_reservoir$manifest_digest) ||
      !identical(record$tree_policy, .adaptive_predictive_tree_policy())) {
    rlang::abort("Predictive bootstrap frozen inputs, seed, or item mapping integrity mismatch.")
  }
  pairs <- state$warm_start_pairs
  if (!tibble::is_tibble(pairs) || !identical(names(pairs), c("i_id", "j_id")) ||
      nrow(pairs) != length(state$item_ids) - 1L ||
      !is.character(pairs$i_id) || !is.character(pairs$j_id) ||
      anyNA(pairs) || any(pairs$i_id == pairs$j_id)) {
    rlang::abort("Predictive bootstrap requires an ordered N - 1 edge queue.")
  }
  keys <- .adaptive_reservoir_ordered_key(pairs$i_id, pairs$j_id)
  edges <- state$replay_reservoir$edges
  if (anyDuplicated(.adaptive_reservoir_key(pairs$i_id, pairs$j_id)) ||
      any(!keys %in% .adaptive_reservoir_ordered_key(edges$A_id, edges$B_id))) {
    rlang::abort("Predictive bootstrap contains duplicate, foreign, or reversed edges.")
  }
  # Connectivity verification only: no probability scores, RNG, or tree selection.
  parent <- seq_along(state$item_ids)
  root <- function(i) {
    while (parent[[i]] != i) i <- parent[[i]]
    i
  }
  a <- match(pairs$i_id, state$item_ids)
  b <- match(pairs$j_id, state$item_ids)
  if (anyNA(a) || anyNA(b)) rlang::abort("Predictive bootstrap has foreign item IDs.")
  for (i in seq_along(a)) {
    ra <- root(a[[i]])
    rb <- root(b[[i]])
    if (ra == rb) rlang::abort("Predictive bootstrap queue is not a connected tree.")
    parent[[rb]] <- ra
  }
  invisible(NULL)
}

.adaptive_bootstrap_audit <- function(state) {
  .adaptive_bootstrap_validate(state)
  list(policy = .adaptive_bootstrap_saved_policy(state),
    version = state$meta$bootstrap_policy_version %||% 1L,
    seed = state$meta$initialization_seed %||% state$meta$seed,
    digest = state$meta$bootstrap_digest %||% NA_character_,
    trueskill_mapping_digest = state$meta$trueskill_mapping$digest %||% NA_character_,
    manifest_digest = state$replay_reservoir$manifest_digest %||% NA_character_,
    diagnostics = state$bootstrap$diagnostics %||% NULL)
}
