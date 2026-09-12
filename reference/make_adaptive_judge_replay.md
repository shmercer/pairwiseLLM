# Create an offline judge from frozen directed outcomes

Replay the stored result for the exact presented `(A_id, B_id)`. Reverse
judgments are independent stored observations: they are never inferred
by complementing the forward result. No provider calls, random draws, or
step-dependent outcomes are used.

## Usage

``` r
make_adaptive_judge_replay(
  outcomes,
  item_ids,
  strict_use = TRUE,
  complete = TRUE
)
```

## Arguments

- outcomes:

  Data frame with character `A_id`, character `B_id`, and binary `Y`
  (one means presented A wins). `Y` accepts logical, numeric zero/one,
  or character `"0"`/`"1"`; factors, missing values, and other values
  are rejected.

- item_ids:

  Unique non-blank character IDs for the active panel, with at least two
  items. IDs must match the adaptive state's item IDs.

- strict_use:

  Logical; reject repeated use of an exact ordered judgment. Default
  `TRUE`. `FALSE` permits repeated lookups for non-study inspection.

- complete:

  Logical; require all `N * (N - 1)` ordered pairs, including both
  orientations of every unordered pair. Default `TRUE`. `FALSE` permits
  a partial table; requests for absent orientations still fail during
  replay.

## Value

A function `judge(A, B, state = NULL, ...)` compatible with
[`adaptive_rank_run_live()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_run_live.md).
A and B are one-row data frames with `item_id`. The result contains
`is_valid = TRUE`, integer `Y`, and `judge_backend = "replay"`. Missing
keys and strict reuse raise errors.

## Details

For study runs, set `adaptive_config = list(dup_max_obs_relaxed = 2L)`
when creating the adaptive state. This prevents hybrid's relaxed third
observation at selection time. Normal presentation balancing and repeat
reversal remain active. Direct strategies already cap unordered pairs at
two observations.

Create a fresh judge for each independent replicate. Strict use records
each successful lookup in the closure, even if the caller subsequently
discards the updated state. The judge is not saved in an adaptive
session. To resume, create a new judge from the same matrix and pass the
loaded state to the runner; strict use also rejects keys already present
in that state's committed history. The matrix and its provenance must be
retained separately by the caller.

## See also

[`validate_adaptive_replay()`](https://shmercer.github.io/pairwiseLLM/reference/validate_adaptive_replay.md),
[`adaptive_rank()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank.md)

Other adaptive ranking:
[`adaptive_rank()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank.md),
[`adaptive_rank_resume()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_resume.md),
[`adaptive_rank_run_live()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_run_live.md),
[`adaptive_rank_start()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_start.md),
[`make_adaptive_judge_llm()`](https://shmercer.github.io/pairwiseLLM/reference/make_adaptive_judge_llm.md),
[`summarize_adaptive()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_adaptive.md),
[`validate_adaptive_replay()`](https://shmercer.github.io/pairwiseLLM/reference/validate_adaptive_replay.md)

## Examples

``` r
ids <- c("a", "b", "c")
outcomes <- expand.grid(A_id = ids, B_id = ids, stringsAsFactors = FALSE)
outcomes <- outcomes[outcomes$A_id != outcomes$B_id, ]
outcomes$Y <- as.integer(outcomes$A_id < outcomes$B_id)
judge <- make_adaptive_judge_replay(outcomes, ids)
state <- adaptive_rank_start(ids, seed = 42,
  adaptive_config = list(dup_max_obs_relaxed = 2L))
state <- adaptive_rank_run_live(state, judge, n_steps = 3L, progress = "none")
```
