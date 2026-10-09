#' Frozen predictive spanning-tree policy
#'
#' Version 1 uses Pollitt-inspired probability targets, a degree cap starting at
#' two and doubling only when stalled, and locally seeded exact-score ties.
#' The parameters are fixed for this version; changing them requires a new policy.
#' @return A versioned policy list.
#' @keywords internal
.adaptive_predictive_tree_policy <- function() {
  list(version = 1L, targets = c(1 / 3, 2 / 3), initial_degree_cap = 2L,
    degree_cap_multiplier = 2L, ties = "canonical_seeded_permutation_v1")
}

#' Build an outcome-blind frozen predictive spanning tree
#'
#' This internal builder is independent of adaptive initialization and persistence.
#' Supply only selectable primary endpoints, for example
#' `reservoir$manifest$edges`, and the initial calibrated TrueSkill distribution.
#' Calibration is an upstream responsibility. No outcomes, reservoir identities,
#' histories, held-out edges, reversal audits, or evolved ratings are consumed.
#' A future caller must build once at initialization and retain the returned queue
#' for all bootstrap steps, rather than rebuild it from updated ratings.
#'
#' IDs are normalized to UTF-8 and sorted by radix order. Each unordered edge is
#' scored in canonical endpoint order using the same numeric kernel as
#' `trueskill_win_probability()`. Its preference is
#' `min(abs(p - 1/3), abs(p - 2/3))`. Ascending preference is primary; only exact
#' ties use a seeded permutation of canonical edges. No tolerance or rounding is
#' used. Mersenne-Twister, Inversion, and Rejection RNG kinds are fixed locally;
#' the caller's RNG state and kinds are restored.
#'
#' With cap two, scan edges in that fixed order, deferring edges whose addition
#' would exceed the cap, discarding cycles, and accepting component-joining edges.
#' Degrees only increase, so deferred edges cannot become eligible in that pass.
#' If incomplete after a full pass, double the cap and scan the deferred edges in
#' the same order. Previously selected edges are never replaced. This greedy
#' safeguard is not a globally minimum-degree or minimum-weight tree algorithm.
#' At a cap of at least `N - 1`, all remaining degree constraints are vacuous,
#' guaranteeing completion for every connected permitted graph.
#'
#' Degree checks precede union-find checks. Each edge is scored once and checked
#' for a cycle at most once, using union by size and path compression. Sorting
#' once and at most logarithmically many deferred scans cost `O(E log E)` time
#' and `O(E + N)` space for a connected graph. No outcome-bearing validator runs.
#'
#' @param item_ids At least two unique nonblank character IDs.
#' @param edges Data frame containing exactly character `A_id` and `B_id`
#'   columns, with one recorded orientation per allowed unordered edge.
#' @param initial_prediction List containing exactly `item_id`, `mu`, `sigma`,
#'   and scalar `beta`, as returned by `.warm_start_trueskill_distribution()`.
#'   IDs must match the panel exactly; locations must be finite and SDs/beta
#'   finite and positive. Row order does not matter.
#' @param seed Explicit finite scalar integer in R's integer range.
#' @param policy The fixed version-1 policy from `.adaptive_predictive_tree_policy()`.
#' @return A tibble with `N - 1` rows and columns `i_id`, `j_id`, in selection
#'   order and recorded A/B orientation. The `tree_diagnostics` attribute contains
#'   `policy`, `seed`, `relaxations` (old/new caps, selected edges, components),
#'   `degrees` (canonical item IDs and degrees), `degree_histogram` (degree and
#'   item count), and `operations` (probability evaluations, candidate visits,
#'   component checks, passes). Diagnostics contain no timestamps or outcomes.
#' @keywords internal
.adaptive_predictive_tree <- function(item_ids, edges, initial_prediction, seed,
                                      policy = .adaptive_predictive_tree_policy()) {
  if (!identical(policy, .adaptive_predictive_tree_policy())) {
    rlang::abort("Unsupported predictive tree policy; use the fixed version-1 policy.")
  }
  if (!is.numeric(seed) || length(seed) != 1L || !is.null(dim(seed)) ||
      !is.finite(seed) || abs(seed) > .Machine$integer.max || seed != trunc(seed)) {
    rlang::abort("`seed` must be an explicit finite scalar integer in R's integer range.")
  }
  seed <- unname(as.integer(seed))
  .adaptive_replay_ids(item_ids, "item_ids")
  item_ids <- sort(unname(enc2utf8(item_ids)), method = "radix")
  n <- length(item_ids)
  if (n < 2L || anyDuplicated(item_ids)) {
    rlang::abort("`item_ids` must contain at least two unique IDs.")
  }
  if (!is.data.frame(edges) || length(names(edges)) != 2L ||
      !setequal(names(edges), c("A_id", "B_id"))) {
    rlang::abort("`edges` must contain exactly A_id and B_id columns, without outcomes.")
  }
  .adaptive_replay_ids(edges$A_id, "edges$A_id")
  .adaptive_replay_ids(edges$B_id, "edges$B_id")
  a_id <- unname(enc2utf8(edges$A_id))
  b_id <- unname(enc2utf8(edges$B_id))
  a <- match(a_id, item_ids)
  b <- match(b_id, item_ids)
  if (anyNA(a) || anyNA(b)) rlang::abort("Edge endpoints must belong to `item_ids`.")
  if (any(a == b)) rlang::abort("Self-pairs are not allowed in a predictive tree.")
  lo <- pmin(a, b)
  hi <- pmax(a, b)
  canonical <- order(lo, hi, method = "radix")
  lo <- lo[canonical]
  hi <- hi[canonical]
  e <- length(lo)
  if (e > 1L && any(lo[-1L] == lo[-e] & hi[-1L] == hi[-e])) {
    rlang::abort("Each unordered edge must occur once; duplicate or reversed edges are not allowed.")
  }
  if (e < n - 1L) rlang::abort("Predictive tree graph is disconnected.")
  a_id <- a_id[canonical]
  b_id <- b_id[canonical]

  prediction <- initial_prediction
  fields <- c("item_id", "mu", "sigma", "beta")
  if (!is.list(prediction) || length(names(prediction)) != length(fields) ||
      !setequal(names(prediction), fields)) {
    rlang::abort("`initial_prediction` must contain exactly item_id, mu, sigma, and beta.")
  }
  .adaptive_replay_ids(prediction$item_id, "initial_prediction$item_id")
  prediction_ids <- enc2utf8(prediction$item_id)
  if (anyDuplicated(prediction_ids) || !setequal(prediction_ids, item_ids)) {
    rlang::abort("Initial prediction IDs must match the panel exactly, without duplicates.")
  }
  for (field in c("mu", "sigma", "beta")) {
    values <- prediction[[field]]
    expected <- if (field == "beta") 1L else n
    if (!is.numeric(values) || !is.null(dim(values)) || length(values) != expected ||
        any(!is.finite(values)) || (field != "mu" && any(values <= 0))) {
      rlang::abort(paste0("Invalid initial prediction `", field,
        "`: require finite numeric values, positive for sigma and beta, with the expected length."))
    }
  }
  index <- match(item_ids, prediction_ids)
  mu <- unname(prediction$mu[index])
  sigma <- unname(prediction$sigma[index])
  p <- .trueskill_win_probability_values(mu[lo], mu[hi], sigma[lo], sigma[hi],
    unname(prediction$beta), check_finite = TRUE)
  preference <- pmin(abs(p - policy$targets[[1L]]), abs(p - policy$targets[[2L]]))
  tie_rank <- withr::with_seed(seed, sample.int(e), .rng_kind = "Mersenne-Twister",
    .rng_normal_kind = "Inversion", .rng_sample_kind = "Rejection")
  pending <- order(preference, tie_rank, method = "radix")

  # Keep union-find vectors local: passing them through updating helpers can
  # copy an entire vector at each union in R.
  parent <- seq_len(n)
  size <- rep.int(1L, n)
  degree <- integer(n)
  selected <- integer(n - 1L)
  count <- 0L
  cap <- policy$initial_degree_cap
  passes <- 0L
  visits <- 0
  checks <- 0
  relaxations <- list()
  repeat {
    passes <- passes + 1L
    deferred <- integer(length(pending))
    n_deferred <- 0L
    for (edge in pending) {
      visits <- visits + 1
      i <- lo[[edge]]
      j <- hi[[edge]]
      if (degree[[i]] >= cap || degree[[j]] >= cap) {
        n_deferred <- n_deferred + 1L
        deferred[[n_deferred]] <- edge
        next
      }
      checks <- checks + 1
      ri <- i
      rj <- j
      while (parent[[ri]] != ri) {
        parent[[ri]] <- parent[[parent[[ri]]]]
        ri <- parent[[ri]]
      }
      while (parent[[rj]] != rj) {
        parent[[rj]] <- parent[[parent[[rj]]]]
        rj <- parent[[rj]]
      }
      if (ri == rj) next
      if (size[[ri]] < size[[rj]]) {
        parent[[ri]] <- rj
        size[[rj]] <- size[[rj]] + size[[ri]]
      } else {
        parent[[rj]] <- ri
        size[[ri]] <- size[[ri]] + size[[rj]]
      }
      count <- count + 1L
      selected[[count]] <- edge
      degree[[i]] <- degree[[i]] + 1L
      degree[[j]] <- degree[[j]] + 1L
      if (count == n - 1L) break
    }
    if (count == n - 1L) break
    if (n_deferred == 0L || cap >= n - 1L) {
      rlang::abort("Predictive tree graph is disconnected.")
    }
    next_cap <- as.double(cap) * policy$degree_cap_multiplier
    relaxations[[length(relaxations) + 1L]] <- tibble::tibble(
      old_cap = as.double(cap), new_cap = next_cap,
      selected_edges = count, components = n - count)
    cap <- next_cap
    pending <- deferred[seq_len(n_deferred)]
  }
  histogram <- tabulate(degree, nbins = max(degree))
  present <- which(histogram > 0L)
  out <- tibble::tibble(i_id = a_id[selected], j_id = b_id[selected])
  attr(out, "tree_diagnostics") <- list(policy = policy, seed = seed,
    relaxations = dplyr::bind_rows(tibble::tibble(old_cap = double(), new_cap = double(),
      selected_edges = integer(), components = integer()), relaxations),
    degrees = tibble::tibble(item_id = item_ids, degree = degree),
    degree_histogram = tibble::tibble(degree = present, n_items = histogram[present]),
    operations = list(probability_evaluations = e, candidate_visits = visits,
      component_checks = checks, passes = passes))
  out
}
