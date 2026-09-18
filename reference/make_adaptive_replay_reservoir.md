# Create a sparse frozen adaptive replay reservoir

Each allowed unordered edge has exactly one observed presentation and
binary outcome. Subset to the primary observation layer before
construction; exclude held-out edges and separate reversal audits. The
allowed graph must connect every panel item. Reservoir replay supports
ordinary within-set runs only.

## Usage

``` r
make_adaptive_replay_reservoir(outcomes, item_ids)
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

## Value

A `pairwiseLLM_replay_reservoir` object for `replay_reservoir` in
[`adaptive_rank_start()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_start.md)
or
[`adaptive_rank()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank.md),
and for
[`make_adaptive_judge_replay()`](https://shmercer.github.io/pairwiseLLM/reference/make_adaptive_judge_replay.md).

## Details

All strategies and warm-start modes share a seeded spanning-tree
bootstrap of `N - 1` allowed edges. Subsequent selection uses unused
allowed edges, and commits their stored presentation without reversing
or complementing outcomes. Consumption follows committed history, so
discarded/failed transactions do not consume observations. Existing
statistical stopping rules still apply.

State stores an outcome-free manifest and identity; retain the reservoir
to recreate the judge on resume. Identity includes the panel, edge
membership, presentation and Y, but excludes row order and ancillary
metadata. Changed identities are rejected before replay. Do not edit a
constructed reservoir. IDs that make distinct allowed edges collide in
the existing colon-separated adaptive history keys are rejected; use
unambiguous panel IDs in that case.

## See also

[`make_adaptive_judge_replay()`](https://shmercer.github.io/pairwiseLLM/reference/make_adaptive_judge_replay.md),
[`adaptive_rank_start()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_start.md),
[`adaptive_rank_resume()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_resume.md)

Other adaptive ranking:
[`adaptive_rank()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank.md),
[`adaptive_rank_resume()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_resume.md),
[`adaptive_rank_run_live()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_run_live.md),
[`adaptive_rank_start()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_start.md),
[`make_adaptive_judge_llm()`](https://shmercer.github.io/pairwiseLLM/reference/make_adaptive_judge_llm.md),
[`make_adaptive_judge_replay()`](https://shmercer.github.io/pairwiseLLM/reference/make_adaptive_judge_replay.md),
[`summarize_adaptive()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_adaptive.md),
[`validate_adaptive_replay()`](https://shmercer.github.io/pairwiseLLM/reference/validate_adaptive_replay.md)

## Examples

``` r
ids <- c("a", "b", "c", "d")
frozen <- data.frame(A_id = c("b", "a", "d", "c"),
  B_id = c("a", "c", "a", "d"), Y = c(1L, 0L, 1L, 1L))
reservoir <- make_adaptive_replay_reservoir(frozen, ids)
state <- adaptive_rank_start(ids, seed = 42, replay_reservoir = reservoir,
  adaptive_config = list(pairing_strategy = "random"))
state <- adaptive_rank_run_live(state, make_adaptive_judge_replay(reservoir),
  n_steps = 4L, progress = "none")
```
