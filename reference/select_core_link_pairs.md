# Select core-linking pairs for BT scaling across batches/waves

Selects pairs intended to **link a new batch** of samples to an existing
"core set" (core bank) so that BT ability estimates remain on a common
scale across waves/batches. Optionally, it can also select:

- **within-batch pairs** (new↔new) to improve local ordering of the new
  batch, and

- **core audit pairs** (core↔core) to monitor core stability over time.

## Usage

``` r
select_core_link_pairs(
  samples,
  theta,
  core_ids,
  new_ids = NULL,
  round_size = 100,
  within_batch_frac = 0.25,
  core_audit_frac = 0.05,
  k_neighbors = 10,
  min_judgments = 12,
  existing_pairs = NULL,
  forbid_repeats = TRUE,
  balance_positions = TRUE,
  seed = NULL
)
```

## Arguments

- samples:

  A tibble/data.frame with columns `ID` and `text`.

- theta:

  A tibble/data.frame with columns `ID`, `theta`, `se`. Rows may be
  missing for some IDs; missing `theta`/`se` are allowed.

- core_ids:

  Character vector of IDs designating the core set. Must be a non-empty
  subset of `samples$ID`.

- new_ids:

  Optional character vector of IDs designating the "new batch". If
  `NULL`, uses `setdiff(samples$ID, core_ids)`.

- round_size:

  Integer number of pairs to select. Can be `0`.

- within_batch_frac:

  Fraction (0..1) of non-audit pairs allocated to new↔new.

- core_audit_frac:

  Fraction (0..1) of pairs allocated to core↔core.

- k_neighbors:

  Integer controlling how strongly pairing is localized by current
  `theta`: when both sides have non-missing `theta`, the opponent is
  chosen from among the `k_neighbors` closest candidates.

- min_judgments:

  Minimum number of total appearances (across both positions) an item
  should have before it is deprioritized. Used as a soft priority rule.

- existing_pairs:

  Optional data.frame of already-judged pairs. Accepted column schemas
  are either `ID1`/`ID2` or `object1`/`object2`.

- forbid_repeats:

  Logical; if `TRUE` (default) do not repeat unordered pairs.

- balance_positions:

  Logical; if `TRUE` (default), attempt to balance first vs second
  position frequencies.

- seed:

  Optional integer seed. When provided, RNG state is restored to its
  prior value (or returned to "uninitialized" if it was missing).

## Value

A tibble with columns:

- `ID1`, `ID2`: the pair (order reflects position balancing)

- `pair_type`: one of `"core_new"`, `"new_new"`, `"core_core"`

## Details

This function does not run any LLM calls. It only proposes which pairs
should be judged next, given current BT estimates and constraints.

Pair selection is round-based. The requested `round_size` is split into:

- `core_audit_frac` of pairs from core↔core,

- `within_batch_frac` of remaining pairs from new↔new,

- and the remainder from core↔new.

If `forbid_repeats = TRUE`, the function avoids generating unordered
duplicates that already exist in `existing_pairs` (and within the newly
selected round). If `balance_positions = TRUE`, it attempts to keep
items balanced in first vs second position across the accumulated
(existing + newly selected) pairs.

## Examples

``` r
# Minimal example using synthetic theta
samples <- tibble::tibble(
  ID = paste0("S", 1:12),
  text = paste("Text", 1:12)
)
theta <- tibble::tibble(
  ID = samples$ID,
  theta = rnorm(nrow(samples)),
  se = runif(nrow(samples), 0.2, 0.8)
)
core_ids <- paste0("S", 1:4)
pairs <- select_core_link_pairs(
  samples = samples,
  theta = theta,
  core_ids = core_ids,
  round_size = 10,
  seed = 1
)
pairs
#> # A tibble: 10 × 3
#>    ID1   ID2   pair_type
#>    <chr> <chr> <chr>    
#>  1 S6    S8    new_new  
#>  2 S5    S7    new_new  
#>  3 S11   S1    core_new 
#>  4 S12   S3    core_new 
#>  5 S3    S9    core_new 
#>  6 S4    S10   core_new 
#>  7 S1    S6    core_new 
#>  8 S7    S2    core_new 
#>  9 S8    S1    core_new 
#> 10 S2    S11   core_new 
```
