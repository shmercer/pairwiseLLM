# Select a core set of items for BT linking

Selects a representative "core bank" of samples from a larger pool,
intended to be reused across waves/batches for linking Bradley-Terry
(BT) scales.

## Usage

``` r
select_core_set(
  samples,
  core_size = NULL,
  core_pct = 0.1,
  method = c("embeddings", "token_stratified", "random"),
  embeddings = NULL,
  distance = c("cosine", "euclidean"),
  seed = NULL
)
```

## Arguments

- samples:

  A tibble/data.frame with columns `ID` and `text`.

- core_size:

  Integer number of core items to select. If `NULL`, uses
  `core_pct * nrow(samples)` (clamped to `[2, n]`).

- core_pct:

  Proportion used when `core_size` is `NULL`. Must be in `(0, 1]`.

- method:

  Selection method: `"embeddings"`, `"token_stratified"`, or `"random"`.

- embeddings:

  Optional numeric matrix of embeddings (rows correspond to samples).
  Required when `method = "embeddings"`. If `rownames(embeddings)` are
  present, they must contain all sample IDs and will be used to align
  rows. Otherwise `nrow(embeddings)` must equal `nrow(samples)` and rows
  are assumed to be in the same order as `samples`.

- distance:

  Distance used within embeddings selection. For `"cosine"`, embeddings
  are L2-normalized and Euclidean distance is applied.

- seed:

  Optional integer seed. When provided, RNG state is restored to its
  prior value (or returned to "uninitialized" if it was missing).

## Value

A tibble with columns:

- `ID`: selected core IDs

- `method`: selection method

- `core_rank`: 1..core_size

- `word_count`: word count (only populated for token_stratified)

- `cluster`: k-means cluster label (only for embeddings)

- `centroid_dist`: squared distance to cluster centroid (embeddings)

## Details

This function does not run any LLM calls. It selects items using one of:

- `"embeddings"`: k-means clustering on a supplied embedding matrix,
  then chooses one medoid (nearest-to-centroid item) per cluster.

- `"token_stratified"`: picks items spaced across the distribution of
  word counts (fast fallback when embeddings are not available).

- `"random"`: uniform random sample without replacement.

## Examples

``` r
data("example_writing_samples", package = "pairwiseLLM")

# Token-stratified (no embeddings needed)
core_len <- select_core_set(example_writing_samples, core_size = 4, method = "token_stratified")
core_len
#> # A tibble: 4 × 6
#>   ID    method           core_rank word_count cluster centroid_dist
#>   <chr> <chr>                <int>      <int>   <int>         <dbl>
#> 1 S01   token_stratified         1         11      NA            NA
#> 2 S10   token_stratified         2         29      NA            NA
#> 3 S15   token_stratified         3         34      NA            NA
#> 4 S20   token_stratified         4         49      NA            NA

# Embeddings-based (user supplies embeddings matrix)
set.seed(1)
emb <- matrix(rnorm(nrow(example_writing_samples) * 8), ncol = 8)
rownames(emb) <- example_writing_samples$ID
core_emb <- select_core_set(
  example_writing_samples,
  core_size = 5,
  method = "embeddings",
  embeddings = emb,
  seed = 1
)
core_emb
#> # A tibble: 5 × 6
#>   ID    method     core_rank word_count cluster centroid_dist
#>   <chr> <chr>          <int>      <int>   <int>         <dbl>
#> 1 S19   embeddings         1         NA       1         0.328
#> 2 S20   embeddings         2         NA       2         0.126
#> 3 S13   embeddings         3         NA       3         0.142
#> 4 S05   embeddings         4         NA       4         0.325
#> 5 S09   embeddings         5         NA       5         0.409
```
