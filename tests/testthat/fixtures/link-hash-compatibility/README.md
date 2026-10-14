# Legacy linking hashes

`rlang-1.2.0.rds` contains an explicit E1 input, result, session, and standalone
rubric reference produced from unchanged pairwiseLLM source at
`2526ab393d994530573af71bf15d89371c5f6228`, using R 4.6.1 and rlang 1.2.0.
Its MD5 is `b7e3656d85e26bf3c218c61c37f3cb61`.

The input/result/session record their producing hash engine. The historical
standalone reference does not; tests must not infer its engine from its hash or
replace missing provenance. The original recorded hashes are retained exactly.
All evidence is synthetic. The standalone sampler boundary is mocked using the
unchanged test helper; no LLM requests, Python, or actual sampling are involved.

Generation command, run with isolated rlang 1.2.0 and compatible httr2 1.2.1:

```sh
Rscript --vanilla scripts/generate-legacy-link-hash-fixture.R \
  /tmp/pairwiseLLM-dependency-validation/legacy-source \
  /tmp/pairwiseLLM-dependency-validation/legacy-library \
  tests/testthat/fixtures/link-hash-compatibility/rlang-1.2.0.rds
```

The source directory is a Git archive of the exact SHA above. Routine tests only
read this fixture. Do not regenerate it from the implementation being tested.
