# Check positional bias and bootstrap consistency reliability

This function diagnoses positional bias in LLM-based paired comparison
data and provides a bootstrapped confidence interval for the overall
consistency of forward vs. reverse comparisons.

## Usage

``` r
check_positional_bias(
  consistency,
  n_boot = 1000,
  conf_level = 0.95,
  seed = NULL
)
```

## Arguments

- consistency:

  Either:

  - A list returned by
    [`compute_reverse_consistency()`](https://shmercer.github.io/pairwiseLLM/reference/compute_reverse_consistency.md)
    that contains a `$details` tibble; or

  - A tibble/data frame with columns `key`, `ID1_main`, `ID2_main`,
    `better_id_main`, `ID1_rev`, `ID2_rev`, `better_id_rev`, and
    `is_consistent`.

- n_boot:

  Integer, number of bootstrap resamples for estimating the distribution
  of the overall consistency proportion. Default is 1000.

- conf_level:

  Confidence level for the bootstrap interval. Default is 0.95.

- seed:

  Optional integer seed for reproducible bootstrapping. If `NULL`
  (default), the current RNG state is used.

## Value

A list with two elements:

- summary:

  A tibble with:

  - `n_pairs`: number of unordered pairs

  - `prop_consistent`: observed proportion of consistent pairs

  - `boot_mean`: mean of bootstrap consistency proportions

  - `boot_lwr`, `boot_upr`: bootstrap confidence interval

  - `p_sample1_main`: p-value from a binomial test for the null
    hypothesis that SAMPLE_1 wins 50\\ main (forward) comparisons

  - `p_sample1_rev`: analogous p-value for the reverse comparisons

  - `p_sample1_overall`: p-value from a binomial test for the null that
    position 1 wins 50\\ *all* (forward + reverse) comparisons

  - `total_pos1_wins`: total number of wins by position 1 across
    forward + reverse comparisons

  - `total_comparisons`: total number of valid forward + reverse
    comparisons included in the overall test

  - `n_inconsistent`: number of pairs with inconsistent forward vs.
    reverse outcomes

  - `n_inconsistent_pos1_bias`: among inconsistent pairs, how many times
    the winner is in position 1 in both directions

  - `n_inconsistent_pos2_bias`: analogous for position 2

- details:

  The input `details` tibble augmented with:

  - `winner_pos_main`: `"pos1"` or `"pos2"` (or `NA`) indicating which
    position won in the main direction

  - `winner_pos_rev`: analogous for the reversed direction

  - `is_pos1_bias`: logical; `TRUE` if the pair is inconsistent and
    position 1 wins in both directions

  - `is_pos2_bias`: analogous for position 2

## Details

It is designed to work with the output of
[`compute_reverse_consistency`](https://shmercer.github.io/pairwiseLLM/reference/compute_reverse_consistency.md),
but will also accept a tibble that looks like its `$details` component.
