# Frozen predictive spanning-tree policy

Version 1 uses Pollitt-inspired probability targets, a degree cap
starting at two and doubling only when stalled, and locally seeded
exact-score ties. The parameters are fixed for this version; changing
them requires a new policy.

## Usage

``` r
.adaptive_predictive_tree_policy()
```

## Value

A versioned policy list.
