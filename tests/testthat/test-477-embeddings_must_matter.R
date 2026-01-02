# -------------------------------------------------------------------------
# PR5: "Embeddings must matter" regression test
# -------------------------------------------------------------------------

test_that("Embedding neighbors materially influence selection under quota", {
  ids <- sprintf("ID%02d", 1:30)

  samples <- tibble::tibble(
    ID = ids,
    text = paste("Sample", ids)
  )

  theta <- tibble::tibble(
    ID = ids,
    theta = rep(0, length(ids)),
    se = rep(1, length(ids))
  )

  # 3 tight clusters in an embedding space represented only by neighbor lists.
  clusters <- rep(1:3, each = 10)
  cluster_ids <- split(ids, clusters)

  k <- 5
  neighbors_true <- lapply(seq_along(ids), function(i) {
    id <- ids[[i]]
    cl <- clusters[[i]]
    pool <- setdiff(cluster_ids[[as.character(cl)]], id)
    utils::head(pool, k)
  })
  names(neighbors_true) <- ids

  # Permute neighbor lists across anchors (same lists, wrong assignment).
  withr::local_seed(1)
  perm <- sample.int(length(ids))
  neighbors_perm <- neighbors_true[perm]
  names(neighbors_perm) <- ids

  pairs_true <- select_adaptive_pairs(
    samples = samples,
    theta = theta,
    embedding_neighbors = neighbors_true,
    n_pairs = 24,
    k_neighbors = 2,
    min_judgments = 0,
    forbid_repeats = FALSE,
    balance_positions = FALSE,
    embed_quota_frac = 0.50,
    seed = 1
  )

  pairs_perm <- select_adaptive_pairs(
    samples = samples,
    theta = theta,
    embedding_neighbors = neighbors_perm,
    n_pairs = 24,
    k_neighbors = 2,
    min_judgments = 0,
    forbid_repeats = FALSE,
    balance_positions = FALSE,
    embed_quota_frac = 0.50,
    seed = 1
  )

  pair_key <- function(df) {
    paste(pmin(df$ID1, df$ID2), pmax(df$ID1, df$ID2), sep = "|")
  }

  jaccard <- function(a, b) {
    u <- union(a, b)
    if (length(u) == 0L) {
      return(1)
    }
    length(intersect(a, b)) / length(u)
  }

  embed_hit_rate <- function(df, nbrs) {
    if (nrow(df) == 0L) {
      return(NA_real_)
    }
    hits <- mapply(
      function(a, b) {
        (b %in% nbrs[[a]]) || (a %in% nbrs[[b]])
      },
      df$ID1,
      df$ID2
    )
    mean(as.logical(hits))
  }

  keys_true <- pair_key(pairs_true)
  keys_perm <- pair_key(pairs_perm)

  # Selection should be sensitive to the embedding structure.
  expect_lt(jaccard(keys_true, keys_perm), 0.70)

  # Neighbor-hit rate should drop when the neighbor lists are misassigned.
  hit_true <- embed_hit_rate(pairs_true, neighbors_true)
  hit_perm <- embed_hit_rate(pairs_perm, neighbors_true)

  expect_true(is.finite(hit_true) && is.finite(hit_perm))
  expect_gte(hit_true - hit_perm, 0.15)
})
