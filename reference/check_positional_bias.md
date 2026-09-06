# Check positional preference and bootstrap reversal agreement

This function diagnoses positional preference in LLM-based paired
comparison data and provides a bootstrapped confidence interval for the
overall agreement of forward vs. reverse comparisons.

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

  - `p_sample1_overall`: p-value from the paired exact test that
    position-1 and position-2 inconsistencies are equally likely

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

Each row of `details` is one unordered pair after any duplicate
judgments have been reduced to a per-direction majority by
[`compute_reverse_consistency()`](https://shmercer.github.io/pairwiseLLM/reference/compute_reverse_consistency.md).
The agreement estimate and its percentile bootstrap interval therefore
use unordered pairs as the unit of analysis and treat those rows as
independently resampled units. This assumption may be inappropriate when
pairs share items.

The direction-specific binomial tests likewise treat unordered-pair
outcomes within a direction as independent. The overall test is paired:
among inconsistent pairs, it compares the number for which position 1
won both presentations with the number for which position 2 won both.
This is the exact conditional form of McNemar's test. It returns `NA`
when there are no informative inconsistent pairs. A large p-value is not
evidence that positional preference is absent.

## See also

[`compute_reverse_consistency()`](https://shmercer.github.io/pairwiseLLM/reference/compute_reverse_consistency.md)

Other bias and consistency:
[`compute_reverse_consistency()`](https://shmercer.github.io/pairwiseLLM/reference/compute_reverse_consistency.md)

## Examples

``` r
# Simple synthetic example
main <- tibble::tibble(
  ID1       = c("S1", "S1", "S2"),
  ID2       = c("S2", "S3", "S3"),
  better_id = c("S1", "S3", "S2")
)

rev <- tibble::tibble(
  ID1       = c("S2", "S3", "S3"),
  ID2       = c("S1", "S1", "S2"),
  better_id = c("S1", "S3", "S2")
)

rc <- compute_reverse_consistency(main, rev)
rc$summary
#> # A tibble: 1 × 3
#>   n_pairs n_consistent prop_consistent
#>     <int>        <int>           <dbl>
#> 1       3            3               1

bias <- check_positional_bias(rc)
bias$summary
#> # A tibble: 1 × 13
#>   n_pairs prop_consistent boot_mean boot_lwr boot_upr p_sample1_main
#>     <int>           <dbl>     <dbl>    <dbl>    <dbl>          <dbl>
#> 1       3               1         1        1        1              1
#> # ℹ 7 more variables: p_sample1_rev <dbl>, p_sample1_overall <dbl>,
#> #   total_pos1_wins <int>, total_comparisons <int>, n_inconsistent <int>,
#> #   n_inconsistent_pos1_bias <int>, n_inconsistent_pos2_bias <int>
```
