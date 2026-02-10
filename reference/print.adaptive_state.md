# Print an adaptive state summary.

S3 method for printing `adaptive_state` objects.

## Usage

``` r
# S3 method for class 'adaptive_state'
print(x, ...)
```

## Arguments

- x:

  An `adaptive_state` object.

- ...:

  Unused.

## Value

`x`, invisibly.

## See also

[`summarize_adaptive()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_adaptive.md)

## Examples

``` r
state <- adaptive_rank_start(c("a", "b", "c"), seed = 1)
print(state)
#> Adaptive state
#> items: 3
#> steps: 0 (committed=0)
#> refits: 0
#> last stop: continue
```
