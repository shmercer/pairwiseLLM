# Build an outcome-blind frozen predictive spanning tree

This internal builder is independent of adaptive initialization and
persistence. Supply only selectable primary endpoints, for example
`reservoir$manifest$edges`, and the initial calibrated TrueSkill
distribution. Calibration is an upstream responsibility. No outcomes,
reservoir identities, histories, held-out edges, reversal audits, or
evolved ratings are consumed. Adaptive start builds once at
initialization and retains the returned queue for all bootstrap steps,
rather than rebuild it from updated ratings.

## Usage

``` r
.adaptive_predictive_tree(
  item_ids,
  edges,
  initial_prediction,
  seed,
  policy = .adaptive_predictive_tree_policy()
)
```

## Arguments

- item_ids:

  At least two unique nonblank character IDs.

- edges:

  Data frame containing exactly character `A_id` and `B_id` columns,
  with one recorded orientation per allowed unordered edge.

- initial_prediction:

  List containing exactly `item_id`, `mu`, `sigma`, and scalar `beta`,
  as returned by `.warm_start_trueskill_distribution()`. IDs must match
  the panel exactly; locations must be finite and SDs/beta finite and
  positive. Row order does not matter.

- seed:

  Explicit finite scalar integer in R's integer range.

- policy:

  The fixed version-1 policy from
  [`.adaptive_predictive_tree_policy()`](https://shmercer.github.io/pairwiseLLM/reference/dot-adaptive_predictive_tree_policy.md).

## Value

A tibble with `N - 1` rows and columns `i_id`, `j_id`, in selection
order and recorded A/B orientation. The `tree_diagnostics` attribute
contains `policy`, `seed`, `relaxations` (old/new caps, selected edges,
components), `degrees` (canonical item IDs and degrees),
`degree_histogram` (degree and item count), and `operations`
(probability evaluations, candidate visits, component checks, passes).
Diagnostics contain no timestamps or outcomes.

## Details

IDs are normalized to UTF-8 and sorted by radix order. Each unordered
edge is scored in canonical endpoint order using the same numeric kernel
as `trueskill_win_probability()`. Its preference is
`min(abs(p - 1/3), abs(p - 2/3))`. Ascending preference is primary; only
exact ties use a seeded permutation of canonical edges. No tolerance or
rounding is used. Mersenne-Twister, Inversion, and Rejection RNG kinds
are fixed locally; the caller's RNG state and kinds are restored.

With cap two, scan edges in that fixed order, deferring edges whose
addition would exceed the cap, discarding cycles, and accepting
component-joining edges. Degrees only increase, so deferred edges cannot
become eligible in that pass. If incomplete after a full pass, double
the cap and scan the deferred edges in the same order. Previously
selected edges are never replaced. This greedy safeguard is not a
globally minimum-degree or minimum-weight tree algorithm. At a cap of at
least `N - 1`, all remaining degree constraints are vacuous,
guaranteeing completion for every connected permitted graph.

Degree checks precede union-find checks. Each edge is scored once and
checked for a cycle at most once, using union by size and path
compression. Sorting once and at most logarithmically many deferred
scans cost `O(E log E)` time and `O(E + N)` space for a connected graph.
No outcome-bearing validator runs.
