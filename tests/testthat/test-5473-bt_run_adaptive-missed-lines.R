test_that("bt_run_adaptive validates stage1 params, uses embeddings, and normalizes resume list-cols", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("a", "b", "c")
  )

  # Deterministic judge: better_id is always the lexicographically smaller ID.
  judge_fun <- function(pairs) {
    pairs <- tibble::as_tibble(pairs)
    tibble::tibble(
      ID1 = as.character(pairs$ID1),
      ID2 = as.character(pairs$ID2),
      better_id = ifelse(as.character(pairs$ID1) < as.character(pairs$ID2),
        as.character(pairs$ID1),
        as.character(pairs$ID2)
      )
    )
  }

  # Minimal deterministic fit function.
  fit_fun <- function(bt_data, ...) {
    bt_data <- tibble::as_tibble(bt_data)
    ids <- sort(unique(c(as.character(bt_data$object1), as.character(bt_data$object2))))
    tibble_theta <- tibble::tibble(ID = ids, theta = seq_along(ids), se = rep(1, length(ids)))
    list(
      engine = "test",
      theta = tibble_theta,
      reliability = 0.5,
      diagnostics = list()
    )
  }

  # ---- stage1 validation branches ----
  expect_error(
    bt_run_adaptive(
      samples,
      judge_fun = judge_fun,
      fit_fun = fit_fun,
      init_round_size = 0,
      max_rounds = 0,
      stage1_k_conn = 0
    ),
    "stage1_k_conn"
  )
  expect_error(
    bt_run_adaptive(
      samples,
      judge_fun = judge_fun,
      fit_fun = fit_fun,
      init_round_size = 0,
      max_rounds = 0,
      stage1_k_stab = 0
    ),
    "stage1_k_stab"
  )
  expect_error(
    bt_run_adaptive(
      samples,
      judge_fun = judge_fun,
      fit_fun = fit_fun,
      init_round_size = 0,
      max_rounds = 0,
      stage1_max_rounds = 0
    ),
    "stage1_max_rounds"
  )
  expect_error(
    bt_run_adaptive(
      samples,
      judge_fun = judge_fun,
      fit_fun = fit_fun,
      init_round_size = 0,
      max_rounds = 0,
      stage1_min_spearman = "nope"
    ),
    "stage1_min_spearman"
  )
  expect_error(
    bt_run_adaptive(
      samples,
      judge_fun = judge_fun,
      fit_fun = fit_fun,
      init_round_size = 0,
      max_rounds = 0,
      stage1_min_pct_nodes_with_degree_gt0 = 2
    ),
    "stage1_min_pct_nodes_with_degree_gt0"
  )
  expect_error(
    bt_run_adaptive(
      samples,
      judge_fun = judge_fun,
      fit_fun = fit_fun,
      init_round_size = 0,
      max_rounds = 0,
      stage1_min_largest_component_frac = -0.1
    ),
    "stage1_min_largest_component_frac"
  )
  expect_error(
    bt_run_adaptive(
      samples,
      judge_fun = judge_fun,
      fit_fun = fit_fun,
      init_round_size = 0,
      max_rounds = 0,
      stage1_min_degree_median = NA_real_
    ),
    "stage1_min_degree_median"
  )
  expect_error(
    bt_run_adaptive(
      samples,
      judge_fun = judge_fun,
      fit_fun = fit_fun,
      init_round_size = 0,
      max_rounds = 0,
      stage1_min_degree_min = NA_real_
    ),
    "stage1_min_degree_min"
  )

  # ---- hit embeddings branch (embed_far_k coercion + neighbors compute) ----
  embeddings <- matrix(
    c(0, 0, 1, 0, 0, 1),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(samples$ID, c("x", "y"))
  )

  out_embed <- bt_run_adaptive(
    samples,
    judge_fun = judge_fun,
    fit_fun = fit_fun,
    build_bt_fun = build_bt_data,
    init_round_size = 2,
    max_rounds = 1,
    embeddings = embeddings,
    embed_k = 1,
    embed_far_k = 1,
    seed_pairs = 1,
    seed = 1
  )
  expect_true(is.list(out_embed))
  expect_true("results" %in% names(out_embed))

  # ---- resume: list-col normalization branches (NULL -> NA / FALSE) ----
  tmp <- withr::local_tempdir()

  chk_rounds <- tibble::tibble(
    round = 1L,
    stage = "stage2_bt",
    # list-cols with NULL elements trigger vapply NULL-handling branches
    stop_reason = list(NULL),
    stop_blocked_by = list(NULL),
    stop_blocked_candidates = list(NULL),
    stop = list(NULL),
    conn_streak = NA_integer_,
    stab_streak = NA_integer_,
    stage1_rounds = NA_integer_,
    stage2_rounds = NA_integer_
  )

  chk <- list(
    run_type = "adaptive",
    ids = samples$ID,
    results = tibble::tibble(ID1 = character(), ID2 = character(), better_id = character()),
    pairs_bootstrap = tibble::tibble(ID1 = character(), text1 = character(), ID2 = character(), text2 = character()),
    fits = list(),
    final_fit = NULL,
    prev_metrics = NULL,
    stop_reason = NA_character_,
    stop_round = NA_integer_,
    rounds = chk_rounds,
    state = tibble::tibble(),
    next_round = 1L,
    completed = FALSE
  )

  saveRDS(chk, file.path(tmp, "run_state.rds"), compress = "xz")

  out_resume <- bt_run_adaptive(
    samples,
    judge_fun = judge_fun,
    fit_fun = fit_fun,
    init_round_size = 0,
    max_rounds = 0,
    resume_from = tmp,
    checkpoint_dir = tmp
  )

  expect_true(is.list(out_resume))
  expect_true(is.character(out_resume$rounds$stop_reason))
  expect_true(is.character(out_resume$rounds$stop_blocked_by))
  expect_true(is.character(out_resume$rounds$stop_blocked_candidates))
  expect_true(is.logical(out_resume$rounds$stop))
})


test_that("bt_run_adaptive bootstrap can skip duplicates when forbid_repeats is TRUE", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("a", "b")
  )

  judge_fun <- function(pairs) {
    pairs <- tibble::as_tibble(pairs)
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  }

  fit_fun <- function(bt_data, ...) {
    ids <- sort(unique(c(as.character(bt_data$object1), as.character(bt_data$object2))))
    list(
      engine = "test",
      theta = tibble::tibble(ID = ids, theta = seq_along(ids), se = rep(1, length(ids))),
      reliability = 0.5,
      diagnostics = list()
    )
  }

  # With 2 IDs there is only one unordered pair; requesting 2 bootstrap pairs forces
  # a duplicate attempt, which is skipped when forbid_repeats is TRUE.
  out <- bt_run_adaptive(
    samples,
    judge_fun = judge_fun,
    fit_fun = fit_fun,
    build_bt_fun = build_bt_data,
    forbid_repeats = TRUE,
    init_round_size = 2,
    max_rounds = 0,
    seed_pairs = 1,
    seed = 1
  )

  expect_true(is.list(out))
  expect_true(nrow(out$pairs_bootstrap) <= 1)
})


test_that("bt_run_adaptive uniqueness enforcement runs when repeat_policy is none", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("a", "b", "c")
  )

  judge_fun <- function(pairs) {
    pairs <- tibble::as_tibble(pairs)
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  }

  fit_fun <- function(bt_data, ...) {
    ids <- sort(unique(c(as.character(bt_data$object1), as.character(bt_data$object2))))
    list(
      engine = "test",
      theta = tibble::tibble(ID = ids, theta = seq_along(ids), se = rep(1, length(ids))),
      reliability = 0.5,
      diagnostics = list()
    )
  }

  # This path forces the branch where repeat_policy %in% c("none", "forbid_unordered")
  # and there are no duplicates (so length(keep) == nrow(results)).
  out <- bt_run_adaptive(
    samples,
    judge_fun = judge_fun,
    fit_fun = fit_fun,
    build_bt_fun = build_bt_data,
    repeat_policy = "none",
    init_round_size = 2,
    max_rounds = 1,
    seed_pairs = 1,
    seed = 1
  )

  expect_true(is.list(out))
  expect_true(all(c("pair_key", "direction") %in% names(out$results)))
})


test_that("bt_run_adaptive can switch to stage2 immediately and increments stage2_rounds", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("a", "b", "c", "d")
  )

  judge_fun <- function(pairs) {
    pairs <- tibble::as_tibble(pairs)
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  }

  fit_fun <- function(bt_data, ...) {
    ids <- sort(unique(c(as.character(bt_data$object1), as.character(bt_data$object2))))
    list(
      engine = "test",
      theta = tibble::tibble(ID = ids, theta = seq_along(ids), se = rep(1, length(ids))),
      reliability = 0.5,
      diagnostics = list()
    )
  }

  out <- bt_run_adaptive(
    samples,
    judge_fun = judge_fun,
    fit_fun = fit_fun,
    build_bt_fun = build_bt_data,
    fit_engine_running = "hybrid",
    stage1_k_conn = 1L,
    stage1_k_stab = 1L,
    # Make stage1 connectivity and stability thresholds trivially satisfied.
    stage1_min_pct_nodes_with_degree_gt0 = 0,
    stage1_min_largest_component_frac = 0,
    stage1_min_degree_median = 0,
    stage1_min_degree_min = 0,
    stage1_min_spearman = -1,
    init_round_size = 3,
    max_rounds = 1,
    seed_pairs = 1,
    seed = 1
  )

  expect_true(any(out$rounds$stage %in% c("stage1_rc", "stage2_bt")))
  expect_true("stage2_rounds" %in% names(out$rounds))
  expect_true(any(is.finite(out$rounds$stage2_rounds)))
})
