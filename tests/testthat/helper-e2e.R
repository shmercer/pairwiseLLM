# Deterministic end-to-end harness for Adaptive Pairing v3 (PR16)

e2e_make_samples <- function(N = 12L) {
  N <- as.integer(N)
  tibble::tibble(
    ID = sprintf("I%02d", seq_len(N)),
    text = paste("item", sprintf("I%02d", seq_len(N)))
  )
}

e2e_theta_true <- function(ids) {
  ids <- as.character(ids)
  theta <- seq(-0.8, 0.8, length.out = length(ids))
  stats::setNames(as.double(theta), ids)
}

e2e_locked_adaptive_config <- function() {
  list(
    batch_overrides = list(
      BATCH2 = 4L,
      BATCH3 = 4L
    ),
    budget_max = 100L,
    batch_size = 4L,
    refit_B = 4L,
    checks_passed_target = 2L,
    hard_cap_frac = 0.40,
    write_outputs = FALSE,
    explore_rate = 0.25,
    min_degree = 3L
  )
}

e2e_mock_submit_from_theta <- function(theta_true,
                                      received_at = as.POSIXct("2026-01-15 00:00:00", tz = "UTC")) {
  theta_true <- stats::setNames(as.double(theta_true), names(theta_true))
  if (is.null(names(theta_true)) || any(is.na(names(theta_true))) || any(names(theta_true) == "")) {
    rlang::abort("`theta_true` must be a named numeric vector.")
  }

  function(pairs, model, trait_name, trait_description, prompt_template, backend, ...) {
    pairs <- tibble::as_tibble(pairs)
    if (!all(c("ID1", "ID2", "pair_uid", "phase", "iter") %in% names(pairs))) {
      rlang::abort("`pairs` must contain ID1, ID2, pair_uid, phase, iter.")
    }

    ids1 <- as.character(pairs$ID1)
    ids2 <- as.character(pairs$ID2)
    p <- stats::plogis(theta_true[ids1] - theta_true[ids2])
    win1 <- stats::rbinom(length(p), size = 1L, prob = p)

    better_id <- ifelse(win1 == 1L, ids1, ids2)
    winner_pos <- ifelse(better_id == ids1, 1L, 2L)

    unordered_key <- pairwiseLLM:::make_unordered_key(ids1, ids2)
    ordered_key <- pairwiseLLM:::make_ordered_key(ids1, ids2)

    list(
      results = tibble::tibble(
        pair_uid = as.character(pairs$pair_uid),
        unordered_key = unordered_key,
        ordered_key = ordered_key,
        A_id = ids1,
        B_id = ids2,
        better_id = as.character(better_id),
        winner_pos = as.integer(winner_pos),
        phase = as.character(pairs$phase),
        iter = as.integer(pairs$iter),
        received_at = received_at,
        backend = as.character(backend),
        model = as.character(model)
      ),
      failed_pairs = tibble::tibble(),
      failed_attempts = pairwiseLLM:::.adaptive_empty_failed_attempts_tbl()
    )
  }
}

e2e_is_connected <- function(ids, unordered_keys) {
  ids <- as.character(ids)
  unordered_keys <- as.character(unordered_keys)
  unordered_keys <- unordered_keys[!is.na(unordered_keys) & unordered_keys != ""]
  if (length(ids) <= 1L) return(TRUE)
  if (length(unordered_keys) == 0L) return(FALSE)

  parts <- strsplit(unordered_keys, ":", fixed = TRUE)
  ok_len <- lengths(parts) == 2L
  if (!all(ok_len)) {
    rlang::abort("`unordered_keys` must contain only `A:B` keys.")
  }

  left <- vapply(parts, `[[`, character(1L), 1L)
  right <- vapply(parts, `[[`, character(1L), 2L)
  edges <- tibble::tibble(a = left, b = right)
  edges <- edges[edges$a != edges$b, , drop = FALSE]

  neigh <- lapply(ids, function(id) character())
  names(neigh) <- ids
  for (k in seq_len(nrow(edges))) {
    a <- edges$a[[k]]
    b <- edges$b[[k]]
    neigh[[a]] <- c(neigh[[a]], b)
    neigh[[b]] <- c(neigh[[b]], a)
  }

  start <- ids[[1L]]
  seen <- stats::setNames(rep(FALSE, length(ids)), ids)
  queue <- start
  seen[[start]] <- TRUE
  while (length(queue) > 0L) {
    cur <- queue[[1L]]
    queue <- queue[-1L]
    nxt <- unique(neigh[[cur]])
    nxt <- nxt[!seen[nxt]]
    if (length(nxt) > 0L) {
      seen[nxt] <- TRUE
      queue <- c(queue, nxt)
    }
  }

  all(seen)
}

e2e_run_locked_scenario <- function(seed) {
  withr::local_seed(seed)

  samples <- e2e_make_samples(12L)
  theta_true <- e2e_theta_true(samples$ID)
  adaptive <- e2e_locked_adaptive_config()

  log_env <- new.env(parent = emptyenv())
  log_env$batch_index <- 0L
  log_env$selections <- list()

  orig_select_batch <- pairwiseLLM:::select_batch
  wrap_select_batch <- function(state, candidates_with_utility, config, seed = NULL, exploration_only = FALSE) {
    log_env$batch_index <- as.integer(log_env$batch_index + 1L)
    selected <- orig_select_batch(
      state = state,
      candidates_with_utility = candidates_with_utility,
      config = config,
      seed = seed,
      exploration_only = exploration_only
    )

    keys <- as.character(selected$unordered_key)
    count_before <- vapply(keys, function(key) {
      val <- state$pair_count[[key]]
      if (is.null(val) || is.na(val)) 0L else as.integer(val)
    }, integer(1L))

    logged <- dplyr::mutate(
      tibble::as_tibble(selected),
      batch_index = log_env$batch_index,
      count_before = as.integer(count_before),
      U_dup_threshold = as.double(rlang::`%||%`(state$posterior$U_dup_threshold, NA_real_)),
      diagnostics_pass = as.logical(rlang::`%||%`(state$posterior$diagnostics_pass, NA)),
      exploration_only = isTRUE(exploration_only)
    )
    log_env$selections[[log_env$batch_index]] <- logged

    selected
  }

  mock_submit <- e2e_mock_submit_from_theta(theta_true)
  mock_mcmc_fit <- function(bt_data, config, seed = NULL) {
    force(config)
    force(seed)
    ids <- as.character(bt_data$item_id %||% seq_len(bt_data$N))
    theta_draws <- matrix(0, nrow = 4L, ncol = length(ids), dimnames = list(NULL, ids))
    list(
      draws = list(theta = theta_draws),
      theta_summary = tibble::tibble(item_id = ids, theta_mean = rep(0, length(ids))),
      epsilon_summary = tibble::tibble(epsilon_mean = 0.1),
      diagnostics = list(
        divergences = 0L,
        max_rhat = 1,
        min_ess_bulk = 1000,
        min_ess_tail = 1000
      )
    )
  }

  out <- testthat::with_mocked_bindings(
    pairwiseLLM::adaptive_rank_run_live(
      samples = samples,
      model = "gpt-test",
      trait_name = "quality",
      trait_description = "Which is better?",
      backend = "openai",
      adaptive = adaptive,
      submission = list(),
      paths = list(output_dir = withr::local_tempdir()),
      seed = seed,
      max_iterations = 200L
    ),
    submit_llm_pairs = mock_submit,
    select_batch = wrap_select_batch,
    .fit_bayes_btl_mcmc_adaptive = mock_mcmc_fit
  )

  selection_log <- dplyr::bind_rows(log_env$selections)

  list(
    out = out,
    selection_log = selection_log,
    theta_true = theta_true,
    adaptive = adaptive
  )
}
