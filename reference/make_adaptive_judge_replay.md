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

  A directed outcome data frame (see
  [`validate_adaptive_replay()`](https://shmercer.github.io/pairwiseLLM/reference/validate_adaptive_replay.md))
  or a
  [`make_adaptive_replay_reservoir()`](https://shmercer.github.io/pairwiseLLM/reference/make_adaptive_replay_reservoir.md)
  object.

- item_ids:

  Panel IDs. Required for data-frame input; inferred from a reservoir
  when omitted, or checked for agreement when supplied.

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

For directed-table studies, set
`adaptive_config = list(dup_max_obs_relaxed = 2L)` when creating the
adaptive state. This prevents hybrid's relaxed third observation at
selection time. Normal presentation balancing and repeat reversal remain
active. Direct strategies already cap unordered pairs at two
observations.

Create a fresh judge for each independent replicate. Strict use records
each successful lookup in the closure, even if the caller subsequently
discards the updated state. The judge is not saved in an adaptive
session. To resume, create a new judge from the same matrix and pass the
loaded state to the runner; strict use also rejects keys already present
in that state's committed history. The matrix and its provenance must be
retained separately by the caller.

A
[`make_adaptive_replay_reservoir()`](https://shmercer.github.io/pairwiseLLM/reference/make_adaptive_replay_reservoir.md)
object instead enables sparse, single-observation replay. Bind that
object through `replay_reservoir` when creating state. The judge then
requires matching reservoir identity, uses committed unordered-edge
history for consumption, and preserves the stored orientation.
Discarding an updated state does not consume an observation. Recreate a
matching judge after loading a session; state contains only the
outcome-free manifest. `complete` applies only to directed data-frame
input; `strict_use = FALSE` is unsupported for reservoirs.

## See also

[`validate_adaptive_replay()`](https://shmercer.github.io/pairwiseLLM/reference/validate_adaptive_replay.md),
[`adaptive_rank()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank.md)

Other adaptive ranking:
[`adaptive_rank()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank.md),
[`adaptive_rank_resume()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_resume.md),
[`adaptive_rank_run_live()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_run_live.md),
[`adaptive_rank_start()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_start.md),
[`make_adaptive_judge_llm()`](https://shmercer.github.io/pairwiseLLM/reference/make_adaptive_judge_llm.md),
[`make_adaptive_replay_reservoir()`](https://shmercer.github.io/pairwiseLLM/reference/make_adaptive_replay_reservoir.md),
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
