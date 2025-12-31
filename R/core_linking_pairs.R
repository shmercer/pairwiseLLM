#' Select core-linking pairs for BT scaling across batches/waves
#'
#' Selects pairs intended to **link a new batch** of samples to an existing
#' "core set" (core bank) so that BT ability estimates remain on a common scale
#' across waves/batches. Optionally, it can also select:
#' \itemize{
#'   \item **within-batch pairs** (new↔new) to improve local ordering of the new batch, and
#'   \item **core audit pairs** (core↔core) to monitor core stability over time.
#' }
#'
#' This function does not run any LLM calls. It only proposes which pairs
#' should be judged next, given current BT estimates and constraints.
#'
#' @details
#' Pair selection is round-based. The requested \code{round_size} is split into:
#' \itemize{
#'   \item \code{core_audit_frac} of pairs from core↔core,
#'   \item \code{within_batch_frac} of remaining pairs from new↔new,
#'   \item and the remainder from core↔new.
#' }
#'
#' If \code{forbid_repeats = TRUE}, the function avoids generating unordered
#' duplicates that already exist in \code{existing_pairs} (and within the
#' newly selected round). If \code{balance_positions = TRUE}, it attempts to
#' keep items balanced in first vs second position across the accumulated
#' (existing + newly selected) pairs.
#'
#' @param samples A tibble/data.frame with columns \code{ID} and \code{text}.
#' @param theta A tibble/data.frame with columns \code{ID}, \code{theta}, \code{se}.
#'   Rows may be missing for some IDs; missing \code{theta}/\code{se} are allowed.
#' @param core_ids Character vector of IDs designating the core set. Must be a
#'   non-empty subset of \code{samples$ID}.
#' @param new_ids Optional character vector of IDs designating the "new batch".
#'   If \code{NULL}, uses \code{setdiff(samples$ID, core_ids)}.
#' @param round_size Integer number of pairs to select. Can be \code{0}.
#' @param within_batch_frac Fraction (0..1) of non-audit pairs allocated to new↔new.
#' @param core_audit_frac Fraction (0..1) of pairs allocated to core↔core.
#' @param k_neighbors Integer controlling how strongly pairing is localized by
#'   current \code{theta}: when both sides have non-missing \code{theta}, the
#'   opponent is chosen from among the \code{k_neighbors} closest candidates.
#' @param min_judgments Minimum number of total appearances (across both positions)
#'   an item should have before it is deprioritized. Used as a soft priority rule.
#' @param existing_pairs Optional data.frame of already-judged pairs. Accepted column
#'   schemas are either \code{ID1}/\code{ID2} or \code{object1}/\code{object2}.
#' @param forbid_keys Optional character vector of precomputed unordered pair keys to forbid.
#'   This is an advanced option intended for callers who already maintain a key set.
#'   Keys must match the internal format (as produced by \code{pair_key()}).
#' @param forbid_repeats Logical; if \code{TRUE} (default) do not repeat unordered pairs.
#' @param balance_positions Logical; if \code{TRUE} (default), attempt to balance
#'   first vs second position frequencies.
#' @param seed Optional integer seed. When provided, RNG state is restored to its
#'   prior value (or returned to "uninitialized" if it was missing).
#'
#' @return A tibble with columns:
#' \itemize{
#'   \item \code{ID1}, \code{ID2}: the pair (order reflects position balancing)
#'   \item \code{pair_type}: one of \code{"core_new"}, \code{"new_new"}, \code{"core_core"}
#' }
#'
#' @examples
#' # Minimal example using synthetic theta
#' samples <- tibble::tibble(
#'   ID = paste0("S", 1:12),
#'   text = paste("Text", 1:12)
#' )
#' theta <- tibble::tibble(
#'   ID = samples$ID,
#'   theta = rnorm(nrow(samples)),
#'   se = runif(nrow(samples), 0.2, 0.8)
#' )
#' core_ids <- paste0("S", 1:4)
#' pairs <- select_core_link_pairs(
#'   samples = samples,
#'   theta = theta,
#'   core_ids = core_ids,
#'   round_size = 10,
#'   seed = 1
#' )
#' pairs
#'
#' @import tibble
#' @import dplyr
#' @export
select_core_link_pairs <- function(samples,
                                   theta,
                                   core_ids,
                                   new_ids = NULL,
                                   round_size = 100,
                                   within_batch_frac = 0.25,
                                   core_audit_frac = 0.05,
                                   k_neighbors = 10,
                                   min_judgments = 12,
                                   existing_pairs = NULL,
                                   forbid_keys = character(0),
                                   forbid_repeats = TRUE,
                                   balance_positions = TRUE,
                                   seed = NULL) {
  samples <- tibble::as_tibble(samples)
  if (!all(c("ID", "text") %in% names(samples))) {
    stop("`samples` must have columns 'ID' and 'text'.", call. = FALSE)
  }

  theta <- tibble::as_tibble(theta)
  if (!all(c("ID", "theta", "se") %in% names(theta))) {
    stop("`theta` must have columns 'ID', 'theta', and 'se'.", call. = FALSE)
  }

  ids <- as.character(samples$ID)
  if (length(ids) < 2L) stop("`samples` must contain at least 2 rows.", call. = FALSE)
  if (anyNA(ids) || any(ids == "")) stop("`samples$ID` must be non-missing and non-empty.", call. = FALSE)
  if (any(duplicated(ids))) stop("`samples$ID` must be unique.", call. = FALSE)

  core_ids <- as.character(core_ids)
  if (length(core_ids) < 1L) stop("`core_ids` must contain at least 1 ID.", call. = FALSE)
  if (anyNA(core_ids) || any(core_ids == "")) stop("`core_ids` must be non-missing and non-empty.", call. = FALSE)
  if (any(duplicated(core_ids))) stop("`core_ids` must be unique.", call. = FALSE)
  if (!all(core_ids %in% ids)) stop("All `core_ids` must be present in `samples$ID`.", call. = FALSE)

  if (is.null(new_ids)) {
    new_ids <- setdiff(ids, core_ids)
  } else {
    new_ids <- as.character(new_ids)
  }
  if (anyNA(new_ids) || any(new_ids == "")) stop("`new_ids` must be non-missing and non-empty.", call. = FALSE)
  if (any(duplicated(new_ids))) stop("`new_ids` must be unique.", call. = FALSE)
  if (!all(new_ids %in% ids)) stop("All `new_ids` must be present in `samples$ID`.", call. = FALSE)

  if (!is.numeric(round_size) || length(round_size) != 1L || is.na(round_size)) {
    stop("`round_size` must be a single non-missing numeric/integer.", call. = FALSE)
  }
  round_size <- as.integer(round_size)
  if (round_size < 0L) stop("`round_size` must be >= 0.", call. = FALSE)

  if (!is.numeric(within_batch_frac) || length(within_batch_frac) != 1L ||
    is.na(within_batch_frac) || within_batch_frac < 0 || within_batch_frac > 1) {
    stop("`within_batch_frac` must be a single numeric value in [0, 1].", call. = FALSE)
  }
  if (!is.numeric(core_audit_frac) || length(core_audit_frac) != 1L ||
    is.na(core_audit_frac) || core_audit_frac < 0 || core_audit_frac > 1) {
    stop("`core_audit_frac` must be a single numeric value in [0, 1].", call. = FALSE)
  }
  if (within_batch_frac + core_audit_frac > 1 + 1e-12) {
    stop("`within_batch_frac + core_audit_frac` must be <= 1.", call. = FALSE)
  }

  # Allow NULL / Inf as a convenience for "all neighbors".
  if (is.null(k_neighbors) || isTRUE(is.infinite(k_neighbors))) k_neighbors <- Inf
  if (!is.numeric(k_neighbors) || length(k_neighbors) != 1L || is.na(k_neighbors) || k_neighbors < 1) {
    stop("`k_neighbors` must be a single numeric/integer >= 1 (or NULL/Inf for all).", call. = FALSE)
  }
  # We'll clamp to (n_ids - 1) later once we know the full candidate ID set.

  # Allow NULL to disable the minimum-judgments heuristic.
  if (is.null(min_judgments)) min_judgments <- 0L
  min_judgments <- as.integer(min_judgments)
  if (is.na(min_judgments) || min_judgments < 0L) stop("`min_judgments` must be an integer >= 0.", call. = FALSE)

  # early return
  if (round_size == 0L) {
    return(tibble::tibble(ID1 = character(0), ID2 = character(0), pair_type = character(0)))
  }

  out <- .with_seed_restore(
    seed,
    f = function() {
      pair_key <- function(a, b) {
        .unordered_pair_key(a, b)
      }

      existing <- .normalize_existing_pairs(existing_pairs, err_arg = "existing_pairs")
      existing <- dplyr::filter(existing, !is.na(.data$ID1), !is.na(.data$ID2), .data$ID1 != "", .data$ID2 != "")
      existing <- dplyr::filter(existing, .data$ID1 %in% ids, .data$ID2 %in% ids, .data$ID1 != .data$ID2)

      existing_keys <- pair_key(existing$ID1, existing$ID2)
      # Combine with caller-provided forbids.
      if (is.null(forbid_keys)) forbid_keys <- character(0)
      if (!is.character(forbid_keys)) forbid_keys <- as.character(forbid_keys)
      forbid_keys <- forbid_keys[!is.na(forbid_keys)]
      existing_keys <- unique(c(existing_keys, forbid_keys))

      # counts + position balance from existing pairs
      all_seen <- c(existing$ID1, existing$ID2)
      n_j <- as.integer(table(factor(all_seen, levels = ids)))
      n_pos1 <- as.integer(table(factor(existing$ID1, levels = ids)))
      n_pos2 <- as.integer(table(factor(existing$ID2, levels = ids)))
      imbalance <- n_pos1 - n_pos2

      # theta/se aligned to ids
      theta0 <- theta[, c("ID", "theta", "se")]
      theta0$ID <- as.character(theta0$ID)
      theta_map <- theta0[match(ids, theta0$ID), , drop = FALSE]
      cur_theta <- theta_map$theta
      cur_se <- theta_map$se

      # priority: ensure low-judgment items are preferred; tie-break by larger SE (or missing SE)
      priority_score <- function(id_vec) {
        idx <- match(id_vec, ids)
        need <- pmax(0L, min_judgments - n_j[idx])
        se_val <- cur_se[idx]
        se_val[is.na(se_val)] <- suppressWarnings(max(se_val, na.rm = TRUE))
        se_val[!is.finite(se_val)] <- 0
        # large weight on "need", then SE
        as.numeric(need) * 1e6 + as.numeric(se_val) * 1e3
      }

      pick_focus <- function(pool_ids) {
        if (length(pool_ids) == 0L) {
          return(NA_character_)
        }
        sc <- priority_score(pool_ids)
        mx <- max(sc)
        top <- pool_ids[sc == mx]
        if (length(top) == 1L) {
          return(top)
        }
        sample(top, size = 1L)
      }

      choose_opponent <- function(focus_id, opponent_pool, forbid_keys, allow_theta = TRUE) {
        if (length(opponent_pool) == 0L) {
          return(NA_character_)
        }
        opponent_pool <- setdiff(opponent_pool, focus_id)
        if (length(opponent_pool) == 0L) {
          return(NA_character_)
        }

        f_idx <- match(focus_id, ids)
        f_th <- cur_theta[f_idx]

        cand <- opponent_pool
        if (allow_theta && !is.na(f_th)) {
          o_idx <- match(cand, ids)
          o_th <- cur_theta[o_idx]
          ok <- !is.na(o_th)
          if (any(ok)) {
            cand_ok <- cand[ok]
            d <- abs(o_th[ok] - f_th)
            ord <- order(d)
            cand_ok <- cand_ok[ord]
            cand <- cand_ok[seq_len(min(k_neighbors, length(cand_ok)))]
          }
        } else {
          # randomize candidate order
          cand <- sample(cand, size = length(cand))
        }

        # try candidates in order
        for (opp in cand) {
          key <- pair_key(focus_id, opp)
          if (isTRUE(forbid_repeats) && key %in% forbid_keys) next
          return(opp)
        }
        NA_character_
      }

      place_pair <- function(a, b) {
        if (!isTRUE(balance_positions)) {
          return(c(a, b))
        }
        ia <- match(a, ids)
        ib <- match(b, ids)
        # put more-overused-as-ID1 item into ID2
        if (imbalance[ia] > imbalance[ib]) {
          return(c(b, a))
        }
        if (imbalance[ib] > imbalance[ia]) {
          return(c(a, b))
        }
        # tie -> random
        if (sample(c(TRUE, FALSE), 1L)) c(a, b) else c(b, a)
      }

      add_pair <- function(focus_id, pool, type, forbid_keys) {
        opp <- choose_opponent(focus_id, pool, forbid_keys, allow_theta = TRUE)
        if (is.na(opp)) {
          return(NULL)
        }
        placed <- place_pair(focus_id, opp)
        id1 <- placed[1]
        id2 <- placed[2]

        # update running stats (existing + selected)
        i1 <- match(id1, ids)
        i2 <- match(id2, ids)
        n_j[i1] <<- n_j[i1] + 1L
        n_j[i2] <<- n_j[i2] + 1L
        imbalance[i1] <<- imbalance[i1] + 1L
        imbalance[i2] <<- imbalance[i2] - 1L

        tibble::tibble(ID1 = id1, ID2 = id2, pair_type = type)
      }

      # allocate counts
      n_audit <- as.integer(floor(round_size * core_audit_frac))
      remain <- round_size - n_audit
      n_within <- as.integer(floor(remain * within_batch_frac))
      n_link <- remain - n_within

      out <- list()
      forbid_keys <- existing_keys

      # core↔core audit
      if (n_audit > 0L && length(core_ids) >= 2L) {
        for (i in seq_len(n_audit)) {
          focus <- pick_focus(core_ids)
          if (is.na(focus)) break
          row <- add_pair(focus, setdiff(core_ids, focus), "core_core", forbid_keys)
          if (is.null(row)) break
          forbid_keys <- c(forbid_keys, pair_key(row$ID1, row$ID2))
          out[[length(out) + 1L]] <- row
        }
      }

      # new↔new within-batch
      if (n_within > 0L && length(new_ids) >= 2L) {
        for (i in seq_len(n_within)) {
          focus <- pick_focus(new_ids)
          if (is.na(focus)) break
          row <- add_pair(focus, setdiff(new_ids, focus), "new_new", forbid_keys)
          if (is.null(row)) break
          forbid_keys <- c(forbid_keys, pair_key(row$ID1, row$ID2))
          out[[length(out) + 1L]] <- row
        }
      }

      # core↔new linking
      if (n_link > 0L && length(core_ids) >= 1L && length(new_ids) >= 1L) {
        for (i in seq_len(n_link)) {
          focus <- pick_focus(new_ids)
          if (is.na(focus)) break
          row <- add_pair(focus, core_ids, "core_new", forbid_keys)
          if (is.null(row)) break
          forbid_keys <- c(forbid_keys, pair_key(row$ID1, row$ID2))
          out[[length(out) + 1L]] <- row
        }
      }

      if (length(out) == 0L) {
        tibble::tibble(ID1 = character(0), ID2 = character(0), pair_type = character(0))
      } else {
        dplyr::bind_rows(out)
      }
    },
    arg_name = "seed"
  )

  out
}


#' Propose a core-linking round given an existing BT fit
#'
#' Convenience wrapper around \code{\link{select_core_link_pairs}} that uses
#' \code{fit$theta} from a model fitted by \code{\link{fit_bt_model}}.
#'
#' @param samples A tibble/data.frame with columns \code{ID} and \code{text}.
#' @param fit A list returned by \code{\link{fit_bt_model}} that contains
#'   a \code{$theta} tibble with columns \code{ID}, \code{theta}, \code{se}.
#' @param core_ids Character vector of core IDs.
#' @param include_text If TRUE, attach `text1`/`text2` columns by joining
#'   against `samples`.
#' @param ... Passed through to `select_core_link_pairs()` (e.g., `existing_pairs`,
#'   `round_size`, `seed`, etc.).
#'
#' @return A list with:
#' \describe{
#'   \item{pairs}{Tibble of proposed pairs (ID1, ID2, pair_type; plus text columns if requested).}
#'   \item{plan}{One-row tibble summarizing how many of each pair_type were returned.}
#' }
#'
#' @examples
#' samples <- tibble::tibble(ID = paste0("S", 1:8), text = paste("t", 1:8))
#' theta <- tibble::tibble(ID = samples$ID, theta = rnorm(8), se = runif(8, 0.2, 0.6))
#' fit <- list(theta = theta)
#' out <- bt_core_link_round(samples, fit, core_ids = paste0("S", 1:3), round_size = 6, seed = 1)
#' out$plan
#' head(out$pairs)
#'
#' @export
bt_core_link_round <- function(samples, fit, core_ids, include_text = FALSE, ...) {
  if (!is.list(fit) || is.null(fit$theta)) {
    stop("`fit` must be a list (from `fit_bt_model()`) containing `$theta`.", call. = FALSE)
  }
  dots <- rlang::list2(...)
  pairs <- do.call(
    select_core_link_pairs,
    c(list(samples = samples, theta = fit$theta, core_ids = core_ids), dots)
  )

  # Fallbacks for "no_pairs": try to relax the most common causes of premature
  # exhaustion while preserving forbid_repeats. In small smoke tests it is easy
  # to run out of pairs when kNN restrictions or strict position-balancing leave
  # no feasible edges.
  if (nrow(pairs) == 0L) {
    n_ids <- nrow(fit$theta)

    # 1) Expand the neighbor set to "all" if k_neighbors was limiting.
    k_in <- dots$k_neighbors
    if (is.null(k_in) || isTRUE(is.infinite(k_in)) || isTRUE(k_in >= (n_ids - 1L))) {
      # already "all"
    } else {
      dots2 <- dots
      dots2$k_neighbors <- n_ids - 1L
      pairs2 <- do.call(select_core_link_pairs, c(list(samples = samples, theta = fit$theta, core_ids = core_ids), dots2))
      if (nrow(pairs2) > 0L) {
        pairs <- pairs2
        attr(pairs, "fallback") <- "expanded_k_neighbors"
      }
    }

    # 2) If still empty, relax position balancing.
    if (nrow(pairs) == 0L && isTRUE(dots$balance_positions)) {
      dots2 <- dots
      dots2$balance_positions <- FALSE
      pairs2 <- do.call(select_core_link_pairs, c(list(samples = samples, theta = fit$theta, core_ids = core_ids), dots2))
      if (nrow(pairs2) > 0L) {
        pairs <- pairs2
        attr(pairs, "fallback") <- "disabled_balance_positions"
      }
    }
  }
  if (isTRUE(include_text) && nrow(pairs) > 0L) {
    pairs <- add_pair_texts(pairs, samples = samples)
  }
  plan <- tibble::tibble(
    n_total = nrow(pairs),
    n_core_new = sum(pairs$pair_type == "core_new"),
    n_new_new = sum(pairs$pair_type == "new_new"),
    n_core_core = sum(pairs$pair_type == "core_core")
  )
  list(pairs = pairs, plan = plan)
}
