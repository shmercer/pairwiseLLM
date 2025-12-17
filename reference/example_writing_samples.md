# Example dataset of writing samples

A small set of 20 writing samples on the topic "Why is writing
assessment difficult?", intended for use in examples and tests involving
pairing and LLM-based comparisons. The samples vary in quality,
approximately from very weak to very strong, and a simple numeric
quality score is included to support simulated comparison outcomes.

## Usage

``` r
data("example_writing_samples")
```

## Format

A tibble with 20 rows and 3 variables:

- ID:

  Character ID for each sample (e.g., `"S01"`).

- text:

  Character string with the writing sample.

- quality_score:

  Integer from 1 to 10 indicating the intended relative quality of the
  sample (higher = better).

## Examples

``` r
data("example_writing_samples")
example_writing_samples
#> # A tibble: 20 × 3
#>    ID    text                                                      quality_score
#>    <chr> <chr>                                                             <int>
#>  1 S01   "Writing assessment is hard. People write different thin…             1
#>  2 S02   "It is hard to grade writing. Some are long and some are…             2
#>  3 S03   "Assessing writing is difficult because everyone writes …             3
#>  4 S04   "Grading essays is tough work. You have to read a lot. S…             4
#>  5 S05   "Writing assessment is challenging because teachers must…             5
#>  6 S06   "It is difficult to assess writing because it is subject…             6
#>  7 S07   "Writing assessment is difficult because writing is a co…             7
#>  8 S08   "A paper with strong ideas might have weak grammar, whil…             8
#>  9 S09   "Assessing writing is difficult because the construct is…             9
#> 10 S10   "The difficulty in writing assessment lies in consistenc…            10
#> 11 S11   "Writing assessment is difficult because we are trying t…            11
#> 12 S12   "Evaluating writing is challenging because no rubric can…            12
#> 13 S13   "Writing assessment is difficult because it is context-d…            13
#> 14 S14   "The challenge of writing assessment is distinguishing b…            14
#> 15 S15   "Writing assessment is difficult because it sits at the …            15
#> 16 S16   "Assessing writing is inherently difficult because it re…            16
#> 17 S17   "Writing assessment is challenging because of the trade-…            17
#> 18 S18   "The fundamental difficulty in writing assessment is cog…            18
#> 19 S19   "Writing assessment is difficult because it asks us to q…            19
#> 20 S20   "Writing assessment is inherently problematic because it…            20
```
