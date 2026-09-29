# Selector recovery baselines (#296)

Generated with unmodified package source at
`565dad453407a84bdd31321a9f50bf0d067b9c91` (pairwiseLLM 1.6.0), R 4.6.1,
using `data-raw/issue-296-selector-fixtures.R` from the repository root.

`baseline.rds` contains small synthetic states, four original starvation outputs,
and 51 successful outputs covering exploration, exploitation, fallback selection
and global identification. Seeds and the clock are fixed. `session/` is a session
saved by the pre-fix package for the sparse six-item false-starvation state.

The sparse fixture has five viable final-fallback pairs. The relaxed fixture has
four viable pairs at `dup_relax`, followed by an empty `global_safe` pool. The quota
fixture uses seed 1111 to exercise empty coverage-override narrowing at every stage.
The exhausted fixture has no viable pairs. No provider or sampler was used, and
these fixtures contain no study data.
