#' Run a round-based adaptive BT workflow end-to-end
#'
#' This exported runner implements the core adaptive loop:
#' \enumerate{
#' \item propose pairs (initial random bootstrap, then adaptive),
#' \item score pairs using a provided judge function (LLM/human/simulator),
#' \item append results,
#' \item fit a BT model,
#' \item compute stopping metrics and decide whether to stop,
#' \item repeat until stopping criteria are met or \code{max_rounds} is reached.
#' }
#'
#' \strong{No reverse-order checks during adaptive sampling.}
#' Optionally, after stopping (or hitting \code{max_rounds}), you can run a
#' post-hoc reverse-order audit on a random subset of already-judged pairs and
#' compute forward-vs-reverse consistency via \code{\link{compute_reverse_consistency}}.
#'
#' The default modeling function is \code{\link{fit_bt_model}} and the default
#' stopping+pairing helper is \code{\link{bt_adaptive_round}}.
#'
#' @param samples A tibble/data.frame with columns \code{ID} and \code{text}.
#' \code{ID} must be unique and non-missing; \code{text} is the content shown to the judge.
#'
#' @param judge_fun A function that accepts a tibble of pairs with columns
#' \code{ID1}, \code{text1}, \code{ID2}, \code{text2} and returns a tibble with
#' columns \code{ID1}, \code{ID2}, and \code{better_id}.
#' If \code{judge} is provided, the returned tibble must also include that judge column.
#' \code{better_id} may be returned as the literal winning ID (\code{ID1} or \code{ID2})
#' or as common positional labels (e.g., \code{SAMPLE_1}/\code{SAMPLE_2},
#' \code{ID1}/\code{ID2}, \code{1}/\code{2}, \code{0}/\code{1}, \code{A}/\code{B},
#' \code{LEFT}/\code{RIGHT}); these are normalized to the corresponding IDs.
#' Blank/NA-like winners are treated as missing (\code{NA}) and are ignored in scoring.
#'
#' @param initial_results Optional tibble/data.frame of already-scored pairs with columns
#' \code{ID1}, \code{ID2}, \code{better_id} (and optional judge column). When provided,
#' these results are used as the starting state (and no bootstrap is performed unless
#' \code{initial_results} is empty).
#'
#' @param judge Optional character scalar giving the name of the column in results that
#' identifies the judge/backend/model (e.g., \code{"gpt4o"} vs \code{"claude"}).
#' When provided, the column must be present in outputs from \code{judge_fun} and is
#' passed to \code{\link{build_bt_data}} so engines that support judge effects can use it.
#'
#' @param engine Character scalar passed to \code{fit_fun} as its \code{engine} argument.
#' Default \code{"sirt"}.
#' @param fit_verbose Logical; passed to \code{fit_fun} as \code{verbose}. Default \code{FALSE}.
#' @param return_diagnostics Logical; passed to \code{fit_fun}. If \code{TRUE}, attempt to
#' return engine-specific diagnostics (e.g., item fit, separation/reliability). Default \code{TRUE}.
#' @param include_residuals Logical; passed to \code{fit_fun}. If \code{TRUE}, request
#' residual/probability outputs when supported (may increase compute/memory). Default \code{FALSE}.
#'
#' @param round_size Integer. Number of new pairs to propose and score in each adaptive round.
#' If \code{0}, the runner will fit once (if possible) and then stop without proposing new pairs.
#' @param init_round_size Integer. Number of bootstrap (random) pairs to score before the first
#' model fit when \code{initial_results} is \code{NULL} or empty. Default: \code{round_size}.
#' @param max_rounds Integer. Maximum number of adaptive rounds to run (excluding the bootstrap
#' scoring step). Default \code{50}.
#'
#' @param se_probs Numeric vector of probabilities in (0, 1) used when summarizing the
#' distribution of standard errors for stopping diagnostics (e.g., median, 90th percentile).
#' Passed to \code{\link{bt_adaptive_round}}.
#' @param fit_bounds Numeric length-2 vector giving acceptable infit/outfit (or analogous) bounds
#' when available. Passed to \code{\link{bt_adaptive_round}}.
#'
#' @param reliability_target Numeric. Target reliability/separation-based criterion used by
#' \code{\link{bt_adaptive_round}} for stopping decisions.
#' @param sepG_target Numeric. Target separation index (or analogous) used by
#' \code{\link{bt_adaptive_round}} for stopping decisions.
#' @param rel_se_p90_target Numeric. Target value for the 90th percentile of item SE (or a
#' comparable uncertainty summary) used for stopping. Passed to \code{\link{bt_adaptive_round}}.
#' @param rel_se_p90_min_improve Numeric. Minimum required improvement in the uncertainty summary
#' relative to the previous round; if improvement falls below this, stopping may be allowed
#' (depending on other criteria). Passed to \code{\link{bt_adaptive_round}}.
#' @param max_item_misfit_prop Numeric between 0 and 1 (inclusive). Maximum
#' allowed proportion of item misfit flags
#' (based on \code{fit_bounds}/diagnostics) before stopping is disallowed. Passed to
#' \code{\link{bt_adaptive_round}}.
#' @param max_judge_misfit_prop Numeric between 0 and 1 (inclusive). Maximum
#' allowed proportion of judge misfit flags before stopping is disallowed
#' (when judge diagnostics are available). Passed to \code{\link{bt_adaptive_round}}.
#'
#' @param k_neighbors Integer. When ability estimates are available, restrict candidate pair
#' selection to approximately local neighborhoods in \code{theta} (e.g., near neighbors) to focus
#' comparisons where they are most informative. If \code{theta} is not available (early rounds),
#' selection falls back to non-theta heuristics. Passed to \code{\link{bt_adaptive_round}} /
#' \code{\link{select_adaptive_pairs}}.
#' @param min_judgments Integer. Minimum number of total judgments per item to prioritize before
#' focusing on adaptive informativeness/uncertainty. Passed to \code{\link{bt_adaptive_round}}.
#' @param forbid_repeats Logical. If \code{TRUE}, unordered pairs (A,B) are not repeated across
#' rounds. Passed to \code{\link{bt_adaptive_round}} / \code{\link{select_adaptive_pairs}}.
#' @param balance_positions Logical. If \code{TRUE}, attempt to balance how often each item appears
#' in the first vs second position (\code{ID1} vs \code{ID2}) to mitigate positional bias.
#' Passed to \code{\link{bt_adaptive_round}} / \code{\link{select_adaptive_pairs}}.
#'
#' @param seed_pairs Optional integer seed used for bootstrap pair generation as
#' \code{seed_pairs}, and for adaptive rounds as \code{seed_pairs + round}.
#' The RNG state is restored to its prior value (or returned to "uninitialized" if it was missing).
#' Note: this controls pair selection reproducibility; it does not control randomness inside
#' \code{judge_fun} unless your \code{judge_fun} uses it explicitly.
#'
#' @param reverse_audit Logical. If \code{TRUE}, run a post-stop reverse-order audit by selecting
#' a subset of forward-scored pairs, reversing their order, and re-scoring them. This does not
#' affect adaptive sampling decisions (it is post-hoc).
#' @param reverse_pct Numeric between 0 and 1 (inclusive). Proportion of eligible unique forward pairs to reverse
#' for the audit. Eligible pairs are unique unordered forward pairs with non-missing
#' \code{better_id}. Ignored if \code{n_reverse} is provided.
#' @param n_reverse Optional integer. Number of eligible unique forward pairs to reverse for the
#' audit. If provided, overrides \code{reverse_pct}.
#' @param reverse_seed Optional integer seed used only for selecting which pairs to reverse
#' (RNG state is restored afterward).
#'
#' @param fit_fun Function used to fit the BT model. Default \code{\link{fit_bt_model}}.
#' Primarily intended as a test hook; most users should keep the default.
#' @param build_bt_fun Function used to convert results into BT data. Default
#' \code{\link{build_bt_data}}. Primarily intended as a test hook.
#' @param ... Additional arguments passed through to \code{fit_fun}.
#'
#' @return A list with elements:
#' \describe{
#' \item{results}{All accumulated forward-direction results (ID1, ID2, better_id, ...).}
#' \item{bt_data}{BT data built from \code{results}.}
#' \item{fits}{List of per-round fit objects (one per adaptive round).}
#' \item{rounds}{A tibble summarizing each adaptive round (metrics + stop flag).}
#' \item{pairs_bootstrap}{Pairs used in the bootstrap scoring step (may be empty).}
#' \item{reverse_audit}{NULL unless \code{reverse_audit=TRUE}; then contains audit
#' pairs, reverse results, and consistency outputs.}
#' }
#'
#' @examples
#' # Minimal self-contained example that does not require sirt:
#' samples <- tibble::tibble(
#'   ID = c("A", "B", "C", "D"),
#'   text = paste("text", c("A", "B", "C", "D"))
#' )
#'
#' # A tiny "judge" simulator (deterministic by latent ability):
#' true_theta <- c(A = 2, B = 1, C = 0, D = -1)
#' judge_fun <- function(pairs) {
#'   simulate_bt_judge(pairs, true_theta = true_theta, deterministic = TRUE, seed = 1)
#' }
#'
#' # A tiny fit function (test-style), so the example runs without external engines:
#' fit_fun <- function(bt_data, ...) {
#'   bt_data <- as.data.frame(bt_data)
#'   ids <- sort(unique(c(bt_data[[1]], bt_data[[2]])))
#'   wins <- stats::setNames(rep(0L, length(ids)), ids)
#'   n_j <- stats::setNames(rep(0L, length(ids)), ids)
#'   for (i in seq_len(nrow(bt_data))) {
#'     a <- as.character(bt_data[[1]][i])
#'     b <- as.character(bt_data[[2]][i])
#'     r <- as.numeric(bt_data[[3]][i])
#'     if (is.finite(r)) {
#'       if (r == 1) wins[a] <- wins[a] + 1L else wins[b] <- wins[b] + 1L
#'       n_j[a] <- n_j[a] + 1L
#'       n_j[b] <- n_j[b] + 1L
#'     }
#'   }
#'   theta <- as.numeric(wins - stats::median(wins))
#'   se <- 1 / sqrt(pmax(1L, as.integer(n_j)))
#'   list(
#'     engine = "mock",
#'     reliability = 0.95,
#'     theta = tibble::tibble(ID = names(wins), theta = theta, se = se),
#'     diagnostics = list(sepG = 3.5)
#'   )
#' }
#'
#' out <- bt_run_adaptive(
#'   samples = samples,
#'   judge_fun = judge_fun,
#'   fit_fun = fit_fun,
#'   engine = "mock",
#'   round_size = 2,
#'   init_round_size = 2,
#'   max_rounds = 2,
#'   rel_se_p90_target = NA_real_,
#'   rel_se_p90_min_improve = NA_real_
#' )
#' out$rounds
#'
#' @import tibble
#' @import dplyr
#' @export
bt_run_adaptive <- function(samples,
                            judge_fun,
                            initial_results = NULL,
                            judge = NULL,
                            engine = "sirt",
                            fit_verbose = FALSE,
                            return_diagnostics = TRUE,
                            include_residuals = FALSE,
                            round_size = 50,
                            init_round_size = round_size,
                            max_rounds = 50,
                            se_probs = c(0.5, 0.9, 0.95),
                            fit_bounds = c(0.7, 1.3),
                            reliability_target = 0.90,
                            sepG_target = 3.0,
                            rel_se_p90_target = 0.30,
                            rel_se_p90_min_improve = 0.01,
                            max_item_misfit_prop = 0.05,
                            max_judge_misfit_prop = 0.05,
                            k_neighbors = 10,
                            min_judgments = 12,
                            forbid_repeats = TRUE,
                            balance_positions = TRUE,
                            seed_pairs = NULL,
                            reverse_audit = FALSE,
                            reverse_pct = 0.10,
                            n_reverse = NULL,
                            reverse_seed = NULL,
                            fit_fun = fit_bt_model,
                            build_bt_fun = build_bt_data,
                            ...) {
  samples <- tibble::as_tibble(samples)
  if (!all(c("ID", "text") %in% names(samples))) {
    stop("`samples` must contain columns: ID, text", call. = FALSE)
  }

  ids <- as.character(samples$ID)
  if (length(ids) < 2L) stop("`samples` must contain at least 2 rows.", call. = FALSE)
  if (anyNA(ids) || any(ids == "")) stop("`samples$ID` must be non-missing and non-empty.", call. = FALSE)
  if (any(duplicated(ids))) stop("`samples$ID` must be unique.", call. = FALSE)

  if (!is.function(judge_fun)) stop("`judge_fun` must be a function.", call. = FALSE)

  if (!is.null(judge)) {
    if (!is.character(judge) || length(judge) != 1L || is.na(judge) || nchar(judge) == 0L) {
      stop("`judge` must be a non-empty character scalar when provided.", call. = FALSE)
    }
  }

  round_size <- as.integer(round_size)
  if (is.na(round_size) || round_size < 0L) stop("`round_size` must be a non-negative integer.", call. = FALSE)

  init_round_size <- as.integer(init_round_size)
  if (is.na(init_round_size) || init_round_size < 0L) stop("`init_round_size` must be a non-negative integer.", call. = FALSE)

  max_rounds <- as.integer(max_rounds)
  if (is.na(max_rounds) || max_rounds < 0L) stop("`max_rounds` must be a non-negative integer.", call. = FALSE)

  # CRAN-safe seed wrapper (restore old RNG state, or remove if uninitialized)
  .with_seed_restore <- function(seed, f) {
    if (is.null(seed)) {
      return(f())
    }
    seed <- as.integer(seed)
    if (length(seed) != 1L || is.na(seed)) stop("`seed_pairs`/`reverse_seed` must be a single integer.", call. = FALSE)

    had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    old_seed <- NULL
    if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)

    on.exit(
      {
        if (had_seed) {
          assign(".Random.seed", old_seed, envir = .GlobalEnv)
        } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
          rm(".Random.seed", envir = .GlobalEnv)
        }
      },
      add = TRUE
    )
    set.seed(seed)
    f()
  }

  # helper: validate judge results (and normalize positional winner labels)
  .validate_results <- function(res) {
    res <- tibble::as_tibble(res)
    req <- c("ID1", "ID2", "better_id")
    miss <- setdiff(req, names(res))
    if (length(miss) > 0L) {
      stop("`judge_fun` output must contain columns: ", paste(req, collapse = ", "), call. = FALSE)
    }

    res$ID1 <- trimws(as.character(res$ID1))
    res$ID2 <- trimws(as.character(res$ID2))
    res$better_id <- trimws(as.character(res$better_id))

    # Treat common "missing winner" encodings as NA (avoids false "invalid" errors)
    # NOTE: build_bt_data() will drop these rows later (result==NA -> filtered out).
    missing_tokens <- c("", "NA", "N/A", "NULL", "NONE", "<NA>")
    tok_upper <- toupper(res$better_id)
    res$better_id[tok_upper %in% missing_tokens] <- NA_character_

    bad_ids <- !(res$ID1 %in% ids) | !(res$ID2 %in% ids)
    if (any(bad_ids, na.rm = TRUE)) stop("`judge_fun` returned IDs not present in `samples$ID`.", call. = FALSE)

    # Helper: normalize a single winner string to ID1/ID2 (or NA if unrecognized)
    normalize_winner <- function(win, id1, id2) {
      if (is.na(win)) {
        return(NA_character_)
      }

      w <- trimws(as.character(win))
      w <- gsub('^["\']+|["\']+$', "", w)

      # Exact match (case-sensitive + case-insensitive)
      if (identical(w, id1)) {
        return(id1)
      }
      if (identical(w, id2)) {
        return(id2)
      }
      if (toupper(w) == toupper(id1)) {
        return(id1)
      }
      if (toupper(w) == toupper(id2)) {
        return(id2)
      }

      w_up <- toupper(w)

      # If winner string contains exactly one of the IDs
      has1 <- grepl(toupper(id1), w_up, fixed = TRUE)
      has2 <- grepl(toupper(id2), w_up, fixed = TRUE)
      if (isTRUE(has1) && !isTRUE(has2)) {
        return(id1)
      }
      if (isTRUE(has2) && !isTRUE(has1)) {
        return(id2)
      }

      # Robust token mapping (handles "SAMPLE_1 is better", "Option A", "1"/"0", TRUE/FALSE)
      tok <- gsub("[^A-Z0-9]+", "", w_up)

      # Treat 1/TRUE as ID1, and 0/2/FALSE as ID2 (covers BT result encodings)
      map1 <- tok %in% c("1", "TRUE", "S1", "SAMPLE1", "ID1", "A", "OPTIONA", "CHOICEA", "WINNER1", "FIRST", "LEFT")
      map2 <- tok %in% c("0", "2", "FALSE", "S2", "SAMPLE2", "ID2", "B", "OPTIONB", "CHOICEB", "WINNER2", "SECOND", "RIGHT")

      if (isTRUE(map1) && !isTRUE(map2)) {
        return(id1)
      }
      if (isTRUE(map2) && !isTRUE(map1)) {
        return(id2)
      }

      NA_character_
    }

    # Normalize any non-ID winners
    needs_map <- !is.na(res$better_id) & !(res$better_id == res$ID1 | res$better_id == res$ID2)
    if (any(needs_map, na.rm = TRUE)) {
      mapped <- mapply(
        normalize_winner,
        win = res$better_id[needs_map],
        id1 = res$ID1[needs_map],
        id2 = res$ID2[needs_map],
        USE.NAMES = FALSE
      )

      if (any(is.na(mapped))) {
        bad_vals <- unique(res$better_id[needs_map])[seq_len(min(5, sum(needs_map)))]
        stop(
          "`judge_fun` returned `better_id` values that are not equal to ID1 or ID2. ",
          "Examples: ", paste(bad_vals, collapse = ", "),
          call. = FALSE
        )
      }

      res$better_id[needs_map] <- mapped
    }

    ok_better <- is.na(res$better_id) | (res$better_id == res$ID1) | (res$better_id == res$ID2)
    if (any(!ok_better, na.rm = TRUE)) {
      stop("`judge_fun` returned `better_id` values that are not equal to ID1 or ID2.", call. = FALSE)
    }

    if (!is.null(judge)) {
      if (!(judge %in% names(res))) stop("`judge_fun` output is missing judge column: ", judge, call. = FALSE)
      if (any(is.na(res[[judge]]))) stop("`judge_fun` output has missing values in judge column: ", judge, call. = FALSE)
      res[[judge]] <- as.character(res[[judge]])
    }

    res
  }

  # helper: map texts for ID1/ID2
  text_map <- stats::setNames(as.character(samples$text), as.character(samples$ID))

  .add_texts <- function(pairs_id) {
    pairs_id <- tibble::as_tibble(pairs_id)
    tibble::tibble(
      ID1 = as.character(pairs_id$ID1),
      text1 = unname(text_map[as.character(pairs_id$ID1)]),
      ID2 = as.character(pairs_id$ID2),
      text2 = unname(text_map[as.character(pairs_id$ID2)])
    )
  }

  # helper: random bootstrap pairs (unique unordered, optional position balancing)
  .bootstrap_pairs <- function(n_pairs, existing_pairs, seed) {
    n_pairs <- as.integer(n_pairs)
    if (n_pairs <= 0L) {
      return(tibble::tibble(ID1 = character(), text1 = character(), ID2 = character(), text2 = character()))
    }

    existing_pairs <- tibble::as_tibble(existing_pairs)
    if (nrow(existing_pairs) == 0L) {
      pos1_counts <- stats::setNames(integer(length(ids)), ids)
      pos2_counts <- stats::setNames(integer(length(ids)), ids)
    } else {
      pos1_counts <- stats::setNames(integer(length(ids)), ids)
      pos2_counts <- stats::setNames(integer(length(ids)), ids)
      tab1 <- table(existing_pairs$ID1[existing_pairs$ID1 %in% ids])
      tab2 <- table(existing_pairs$ID2[existing_pairs$ID2 %in% ids])
      pos1_counts[names(tab1)] <- as.integer(tab1)
      pos2_counts[names(tab2)] <- as.integer(tab2)
    }

    imbalance <- pos1_counts - pos2_counts

    key_fun <- function(a, b) {
      lo <- ifelse(a <= b, a, b)
      hi <- ifelse(a <= b, b, a)
      paste0(lo, "\r", hi)
    }

    existing_key <- character()
    if (nrow(existing_pairs) > 0L) {
      existing_key <- unique(key_fun(existing_pairs$ID1, existing_pairs$ID2))
    }

    out_ID1 <- character()
    out_ID2 <- character()
    out_key <- character()

    # CRAN-safe seed handling; run loop in this scope (lintr-friendly)
    had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    old_seed <- NULL
    if (!is.null(seed)) {
      seed <- as.integer(seed)
      if (length(seed) != 1L || is.na(seed)) stop("`seed` must be a single integer.", call. = FALSE)
      if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)

      on.exit(
        {
          if (had_seed) {
            assign(".Random.seed", old_seed, envir = .GlobalEnv)
          } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
            rm(".Random.seed", envir = .GlobalEnv)
          }
        },
        add = TRUE
      )
      set.seed(seed)
    }

    max_tries <- max(200L, n_pairs * 50L)
    tries <- 0L
    while (length(out_ID1) < n_pairs && tries < max_tries) {
      tries <- tries + 1L
      pair <- sample(ids, size = 2L, replace = FALSE)
      a <- pair[[1]]
      b <- pair[[2]]
      k <- key_fun(a, b)
      if (isTRUE(forbid_repeats) && (k %in% c(existing_key, out_key))) next

      if (isTRUE(balance_positions)) {
        ia <- match(a, ids)
        ib <- match(b, ids)
        if (imbalance[ia] > imbalance[ib]) {
          id1 <- b
          id2 <- a
        } else if (imbalance[ib] > imbalance[ia]) {
          id1 <- a
          id2 <- b
        } else {
          if (stats::runif(1) < 0.5) {
            id1 <- a
            id2 <- b
          } else {
            id1 <- b
            id2 <- a
          }
        }
      } else {
        id1 <- a
        id2 <- b
      }

      imbalance[match(id1, ids)] <- imbalance[match(id1, ids)] + 1L
      imbalance[match(id2, ids)] <- imbalance[match(id2, ids)] - 1L

      out_ID1 <- c(out_ID1, id1)
      out_ID2 <- c(out_ID2, id2)
      out_key <- c(out_key, k)
    }

    .add_texts(tibble::tibble(ID1 = out_ID1, ID2 = out_ID2))
  }

  # normalize initial results
  if (is.null(initial_results)) {
    results <- tibble::tibble(ID1 = character(), ID2 = character(), better_id = character())
  } else {
    results <- .validate_results(initial_results)
    results <- tibble::as_tibble(results)
  }

  # bootstrap scoring step (if no initial results)
  pairs_bootstrap <- tibble::tibble(ID1 = character(), text1 = character(), ID2 = character(), text2 = character())
  if (nrow(results) == 0L && init_round_size > 0L) {
    pairs_bootstrap <- .bootstrap_pairs(
      n_pairs = init_round_size,
      existing_pairs = results,
      seed = seed_pairs
    )

    if (nrow(pairs_bootstrap) > 0L) {
      res0 <- judge_fun(pairs_bootstrap)
      res0 <- .validate_results(res0)
      results <- dplyr::bind_rows(results, res0)
    }
  }

  fits <- list()
  rounds_list <- list()
  prev_metrics <- NULL

  # adaptive rounds
  for (r in seq_len(max_rounds)) {
    if (nrow(results) == 0L) break

    bt_data <- if (is.null(judge)) {
      build_bt_fun(results, judge = NULL)
    } else {
      build_bt_fun(results, judge = judge)
    }

    fit <- fit_fun(
      bt_data,
      engine = engine,
      verbose = fit_verbose,
      return_diagnostics = return_diagnostics,
      include_residuals = include_residuals,
      ...
    )

    fits[[length(fits) + 1L]] <- fit

    round_out <- bt_adaptive_round(
      samples = samples,
      fit = fit,
      existing_pairs = results,
      prev_metrics = prev_metrics,
      round_size = round_size,
      se_probs = se_probs,
      fit_bounds = fit_bounds,
      reliability_target = reliability_target,
      sepG_target = sepG_target,
      rel_se_p90_target = rel_se_p90_target,
      rel_se_p90_min_improve = rel_se_p90_min_improve,
      max_item_misfit_prop = max_item_misfit_prop,
      max_judge_misfit_prop = max_judge_misfit_prop,
      k_neighbors = k_neighbors,
      min_judgments = min_judgments,
      forbid_repeats = forbid_repeats,
      balance_positions = balance_positions,
      seed = if (is.null(seed_pairs)) NULL else (as.integer(seed_pairs) + as.integer(r))
    )

    metrics <- round_out$metrics
    decision <- round_out$decision
    pairs_next <- round_out$pairs_next

    rounds_list[[length(rounds_list) + 1L]] <- dplyr::bind_cols(
      tibble::tibble(
        round = as.integer(r),
        n_new_pairs_scored = 0L,
        n_total_results = as.integer(nrow(results)),
        stop = isTRUE(decision$stop)
      ),
      metrics
    )

    prev_metrics <- metrics

    if (isTRUE(decision$stop)) break
    if (round_size == 0L) break
    if (nrow(pairs_next) == 0L) break

    res_next <- judge_fun(pairs_next)
    res_next <- .validate_results(res_next)

    n_added <- nrow(res_next)
    if (n_added > 0L) {
      results <- dplyr::bind_rows(results, res_next)
      rounds_list[[length(rounds_list)]][["n_new_pairs_scored"]] <- as.integer(n_added)
      rounds_list[[length(rounds_list)]][["n_total_results"]] <- as.integer(nrow(results))
    } else {
      break
    }
  }

  rounds_tbl <- if (length(rounds_list) == 0L) tibble::tibble() else dplyr::bind_rows(rounds_list)

  bt_data_final <- if (nrow(results) == 0L) {
    tibble::tibble(object1 = character(), object2 = character(), result = numeric())
  } else if (is.null(judge)) {
    build_bt_fun(results, judge = NULL)
  } else {
    build_bt_fun(results, judge = judge)
  }

  # optional post-stop reverse audit (no effect on adaptive loop)
  reverse_out <- NULL
  if (isTRUE(reverse_audit) && nrow(results) > 0L) {
    uniq_pairs <- results |>
      dplyr::filter(!is.na(.data$better_id)) |>
      dplyr::transmute(ID1 = as.character(.data$ID1), ID2 = as.character(.data$ID2)) |>
      dplyr::distinct()

    uniq_pairs_txt <- .add_texts(uniq_pairs)

    n_all <- nrow(uniq_pairs_txt)
    k <- NULL
    if (!is.null(n_reverse)) {
      k <- as.integer(n_reverse)
      if (is.na(k) || k < 0L) stop("`n_reverse` must be a non-negative integer.", call. = FALSE)
      k <- min(k, n_all)
    } else {
      if (!is.numeric(reverse_pct) || length(reverse_pct) != 1L || is.na(reverse_pct)) {
        stop("`reverse_pct` must be a single numeric value when `n_reverse` is NULL.", call. = FALSE)
      }
      if (reverse_pct <= 0) k <- 0L else if (reverse_pct >= 1) k <- n_all else k <- as.integer(round(n_all * reverse_pct))
      k <- min(max(k, 0L), n_all)
    }

    rev_pairs <- uniq_pairs_txt[0, , drop = FALSE]
    if (k > 0L && n_all > 0L) {
      idx <- .with_seed_restore(reverse_seed, function() sample.int(n_all, size = k, replace = FALSE))
      sel <- uniq_pairs_txt[idx, , drop = FALSE]
      rev_pairs <- tibble::tibble(
        ID1 = sel$ID2,
        text1 = sel$text2,
        ID2 = sel$ID1,
        text2 = sel$text1
      )
    }

    rev_results <- tibble::tibble(ID1 = character(), ID2 = character(), better_id = character())
    consistency <- NULL
    if (nrow(rev_pairs) > 0L) {
      rev_results <- judge_fun(rev_pairs)
      rev_results <- .validate_results(rev_results)

      # Ensure we compute consistency only on audited unordered keys (avoids n_pairs==0)
      add_key <- function(df) {
        df |>
          dplyr::mutate(
            key = paste(
              pmin(as.character(.data$ID1), as.character(.data$ID2)),
              pmax(as.character(.data$ID1), as.character(.data$ID2)),
              sep = "||"
            )
          )
      }

      main_audit <- add_key(results) |>
        dplyr::filter(!is.na(.data$better_id))

      rev_audit <- add_key(rev_results)

      main_audit <- dplyr::semi_join(
        main_audit,
        dplyr::select(rev_audit, "key"),
        by = "key"
      ) |>
        dplyr::select(-"key")

      rev_audit <- dplyr::select(rev_audit, -"key")

      consistency <- compute_reverse_consistency(main_audit, rev_audit)
    }

    reverse_out <- list(
      pairs_reversed = rev_pairs,
      reverse_results = rev_results,
      consistency = consistency
    )
  }

  list(
    results = results,
    bt_data = bt_data_final,
    fits = fits,
    rounds = rounds_tbl,
    pairs_bootstrap = pairs_bootstrap,
    reverse_audit = reverse_out
  )
}

#' Simulate a judge for BT pairwise comparisons
#'
#' This helper produces synthetic pairwise comparison results for testing,
#' vignettes, and benchmarking. It takes a table of pairs (with texts) and returns
#' \code{better_id} outcomes based on a latent "true ability" vector.
#'
#' The simulator can be deterministic (always pick higher true ability) or
#' stochastic using a Bradley–Terry / logistic probability model.
#'
#' @param pairs A tibble/data.frame with columns \code{ID1}, \code{text1}, \code{ID2}, \code{text2}.
#' @param true_theta Named numeric vector of latent abilities. Names must include
#' the IDs used in \code{pairs}. Missing IDs are treated as 0.
#' @param judges Optional character vector of judge identifiers. If length > 1,
#' each row is assigned a judge (round-robin by default).
#' @param judge_col Optional character scalar column name to use for judge labels.
#' If \code{NULL} (default), no judge column is returned.
#' @param deterministic Logical; if \code{TRUE}, always choose the higher \code{true_theta}.
#' If equal, break ties at random (seed-controlled). Default \code{FALSE}.
#' @param seed Optional integer seed; RNG state is restored afterwards.
#' @param round_robin Logical; if \code{TRUE} (default) and \code{length(judges) > 1},
#' assign judges in repeating order. If \code{FALSE}, assign judges uniformly at random.
#'
#' @return A tibble with columns \code{ID1}, \code{ID2}, \code{better_id}, and optionally
#' a judge column.
#'
#' @examples
#' pairs <- tibble::tibble(
#'   ID1 = c("A", "B"),
#'   text1 = c("a", "b"),
#'   ID2 = c("C", "D"),
#'   text2 = c("c", "d")
#' )
#' true_theta <- c(A = 2, B = 1, C = 0, D = -1)
#' simulate_bt_judge(pairs, true_theta, deterministic = TRUE, seed = 1)
#'
#' @export
simulate_bt_judge <- function(pairs,
                              true_theta,
                              judges = "judge_1",
                              judge_col = NULL,
                              deterministic = FALSE,
                              seed = NULL,
                              round_robin = TRUE) {
  pairs <- tibble::as_tibble(pairs)
  req <- c("ID1", "text1", "ID2", "text2")
  miss <- setdiff(req, names(pairs))
  if (length(miss) > 0L) stop("`pairs` must contain columns: ", paste(req, collapse = ", "), call. = FALSE)

  if (!is.numeric(true_theta) || is.null(names(true_theta))) {
    stop("`true_theta` must be a named numeric vector.", call. = FALSE)
  }

  if (!is.null(judge_col)) {
    if (!is.character(judge_col) || length(judge_col) != 1L || is.na(judge_col) || nchar(judge_col) == 0L) {
      stop("`judge_col` must be a non-empty character scalar when provided.", call. = FALSE)
    }
  }

  judges <- as.character(judges)
  if (length(judges) < 1L || anyNA(judges) || any(judges == "")) {
    stop("`judges` must contain at least one non-missing, non-empty label.", call. = FALSE)
  }

  n <- nrow(pairs)
  if (n == 0L) {
    out0 <- tibble::tibble(ID1 = character(), ID2 = character(), better_id = character())
    if (!is.null(judge_col)) out0[[judge_col]] <- character()
    return(out0)
  }

  id1 <- trimws(as.character(pairs$ID1))
  id2 <- trimws(as.character(pairs$ID2))

  th1 <- true_theta[id1]
  th2 <- true_theta[id2]
  th1[is.na(th1)] <- 0
  th2[is.na(th2)] <- 0

  # CRAN-safe seed handling (restore previous RNG state)
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- NULL
  if (!is.null(seed)) {
    seed <- as.integer(seed)
    if (length(seed) != 1L || is.na(seed)) stop("`seed` must be a single integer.", call. = FALSE)
    if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)

    on.exit(
      {
        if (had_seed) {
          assign(".Random.seed", old_seed, envir = .GlobalEnv)
        } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
          rm(".Random.seed", envir = .GlobalEnv)
        }
      },
      add = TRUE
    )
    set.seed(seed)
  }

  better <- character(n)

  if (isTRUE(deterministic)) {
    for (i in seq_len(n)) {
      if (th1[i] > th2[i]) {
        better[i] <- id1[i]
      } else if (th2[i] > th1[i]) {
        better[i] <- id2[i]
      } else {
        better[i] <- if (stats::runif(1) < 0.5) id1[i] else id2[i]
      }
    }
  } else {
    for (i in seq_len(n)) {
      d <- th1[i] - th2[i]
      p <- 1 / (1 + exp(-d))
      better[i] <- if (stats::runif(1) < p) id1[i] else id2[i]
    }
  }

  out <- tibble::tibble(
    ID1 = id1,
    ID2 = id2,
    better_id = better
  )

  if (!is.null(judge_col)) {
    if (length(judges) == 1L) {
      out[[judge_col]] <- rep(judges[[1]], n)
    } else if (isTRUE(round_robin)) {
      out[[judge_col]] <- judges[((seq_len(n) - 1L) %% length(judges)) + 1L]
    } else {
      out[[judge_col]] <- judges[sample.int(length(judges), size = n, replace = TRUE)]
    }
  }

  out
}
