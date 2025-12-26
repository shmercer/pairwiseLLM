# Select adaptive pairs for the next round of comparisons

This helper proposes a new set of pairs for a round-based adaptive
workflow. It uses current BT estimates (`theta` and `se`) to prioritize
pairs that are expected to be most informative (roughly: pairs with
predicted win probability near 0.5) while also prioritizing items that
have not yet been judged `min_judgments` times.

## Usage

``` r
select_adaptive_pairs(
  samples,
  theta,
  existing_pairs = NULL,
  n_pairs,
  k_neighbors = 10,
  min_judgments = 12,
  forbid_repeats = TRUE,
  balance_positions = TRUE,
  seed = NULL
)
```

## Arguments

- samples:

  A tibble/data frame with columns `ID` and `text`.

- theta:

  A tibble/data frame with columns `ID`, `theta`, and `se` (typically
  `bt_fit$theta` from
  [`fit_bt_model`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md)).

- existing_pairs:

  Optional data frame containing previously judged pairs. Supported
  formats:

  - `ID1`, `ID2` (e.g., pairs table or LLM results)

  - `object1`, `object2` (e.g., BT data)

  If `NULL` (default), the function assumes no prior pairs.

- n_pairs:

  Integer number of new pairs to return for the next round.

- k_neighbors:

  Integer number of adjacent neighbors (in sorted-theta order) to
  consider for each item when generating candidate pairs. Default is 10.

- min_judgments:

  Integer minimum desired number of judgments per item. Items below this
  threshold are prioritized. Default is 12.

- forbid_repeats:

  Logical; if `TRUE` (default), do not return pairs that have already
  appeared in `existing_pairs` (unordered).

- balance_positions:

  Logical; if `TRUE` (default), orient each selected pair so that items
  with more historical appearances in position 1 (ID1) are more likely
  to be placed in position 2 (ID2), and vice versa.

- seed:

  Optional integer seed for reproducibility. If `NULL` (default), the
  current RNG state is used and not modified.

## Value

A tibble with columns `ID1`, `text1`, `ID2`, `text2`. Extra columns are
not returned, to keep the output directly compatible with
[`submit_llm_pairs`](https://shmercer.github.io/pairwiseLLM/reference/submit_llm_pairs.md).

## Details

Candidate pairs are generated efficiently by sorting items by `theta`
and considering only `k_neighbors` adjacent items for each item. This
avoids enumerating all `N*(N-1)/2` pairs and scales to large `N`.

The function can also:

- forbid repeated pairings (unordered) across rounds,

- balance positions (ID1 vs ID2) to reduce positional bias,

- accept existing pairs in either `ID1/ID2` or `object1/object2` format.

## Examples

``` r
samples <- tibble::tibble(
  ID = c("A", "B", "C", "D"),
  text = paste("Sample", c("A", "B", "C", "D"))
)

theta <- tibble::tibble(
  ID = c("A", "B", "C", "D"),
  theta = c(0.0, 0.1, 2.0, 2.1),
  se = c(0.5, 0.5, 0.3, 0.3)
)

# First round: no existing pairs
select_adaptive_pairs(samples, theta, n_pairs = 2, seed = 1)
#> # A tibble: 2 × 4
#>   ID1   text1    ID2   text2   
#>   <chr> <chr>    <chr> <chr>   
#> 1 A     Sample A B     Sample B
#> 2 C     Sample C D     Sample D

# Later: forbid repeats against existing pairs
existing <- tibble::tibble(ID1 = "A", ID2 = "B")
select_adaptive_pairs(samples, theta, existing_pairs = existing, n_pairs = 2, seed = 1)
#> # A tibble: 2 × 4
#>   ID1   text1    ID2   text2   
#>   <chr> <chr>    <chr> <chr>   
#> 1 C     Sample C D     Sample D
#> 2 B     Sample B C     Sample C
```
