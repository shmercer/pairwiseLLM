# Dependency compatibility validation — 2026-10-14

Base: `2526ab393d994530573af71bf15d89371c5f6228` (`origin/master`, verified before branching).
Branch: `fix/dependency-compatibility`; isolated worktree:
`/tmp/pairwiseLLM-dependency-compatibility`.
No assigned GitHub issue; the user's dependency compatibility request defines this change.

## Environments and dependency policy

| Environment | rlang | withr | glmnet | Focused result |
| --- | --- | --- | --- | --- |
| R 4.6.1, local Linux | 1.3.0 | 3.0.3 | 5.0 | 48 files passed, 3 skips |
| R 4.5.3, isolated Ubuntu 24.04 container | 1.3.0 | 3.0.3 | 4.1-10 | 48 files passed, 4 skips |

`DESCRIPTION` requires rlang >= 1.3.0 in Imports and withr >= 3.0.3 in Suggests.
glmnet remains optional and unversioned. No algorithm, numerical tolerance, model
default, package version, or original persisted identity was changed.

The standard r-lib Actions resolver reads DESCRIPTION. The existing native
macOS fallback now permits upgrades and uses `dependencies = TRUE`: this includes
direct Suggests while restricting transitive installs to required dependencies.
There are no extra CI jobs, duplicated installers, or additional package requirements.

## Executed validation

All final commands use the isolated worktree unless otherwise indicated. Local
artifacts and complete logs are under `/tmp/pairwiseLLM-dependency-validation`.

1. Regenerated help with `Rscript --vanilla -e 'roxygen2::roxygenise()'`.
2. Ran focused tests with this exact filter on both R versions:

   ```r
   filter <- "0008|010[4-9]|011[3-9]|0208|507[0-9]|508[0-9]|509[0-9]|5111|5116|9200|9205"
   # Release:
   devtools::test(filter = filter, reporter = "summary", stop_on_failure = TRUE)
   # Oldrel, in the offline container with /source mounted read-only:
   pkgload::load_all()
   testthat::test_dir("tests/testthat", filter = filter, reporter = "summary", stop_on_failure = TRUE)
   ```

   Both runs passed all 48 selected files. Three skips require the explicitly
   provisioned Python integration (`PAIRWISELLM_TEST_PYTHON`). Oldrel additionally
   skips the historical numerical refit whose frozen oracle specifically requires
   glmnet 5.0; its legacy deployment checks and ordinary glmnet 4.1-10 fitting
   regressions still ran.
3. Ran the new seed tests plus selector/TrueSkill recovery together:
   `devtools::test(filter="0008|5111|5116", reporter="summary", stop_on_failure=TRUE)` — passed.
4. Ran a deliberate negative trial with isolated withr 3.0.2:
   `devtools::test(filter="0008", reporter="summary", stop_on_failure=TRUE)` —
   four failures as expected, including leaked caller seeds after success/error.
   Supported withr 3.0.3 passes the same assertions. No assertions were relaxed.
5. Generated the frozen legacy fixture from unchanged base source using isolated
   rlang 1.2.0 and compatible httr2 1.2.1. See its README and generation script.
   The generator mocks the sampler boundary and uses only synthetic evidence.
6. Built the package, including all vignettes:

   ```sh
   R CMD build --no-manual /tmp/pairwiseLLM-dependency-compatibility
   ```

   The resulting `pairwiseLLM_1.6.0.tar.gz` was checked on both R versions.
   Final package-check results are recorded below.
7. Ran scoped covr line coverage using `covr::package_coverage(type="none")`,
   installed only into a temporary validation library, and this exact test code:

   ```r
   testthat::test_dir("/tmp/pairwiseLLM-dependency-compatibility/tests/testthat",
     filter = "0208|507[0-9]|508[0-9]|509[0-9]|9200|9205",
     package = "pairwiseLLM", reporter = "summary", stop_on_failure = TRUE)
   ```

   Source-line coverage was computed with `covr::tally_coverage(..., by="line")`:

   | Materially changed R file | Line coverage |
   | --- | ---: |
   | `R/link_contract.R` | 100% |
   | `R/link_result.R` | 98.47% |
   | `R/link_session.R` | 100% |
   | `R/rubric_reference.R` | 100% |

   The four uncovered lines in `link_result.R` are the unimplemented backend
   dispatch errors for fitting/prediction, not the changed hash-validation logic.
   The repeated coverage invocation reported that its temporary install directory
   already existed; instrumentation and tests completed successfully.
8. Ran DESCRIPTION checks using `desc::desc_get_deps()` and installed versions:
   verified both floors and their Imports/Suggests placement, plus unversioned
   optional glmnet. A native-installer policy dry run using
   `remotes:::upgradable_packages()` confirmed `upgrade="always"` selects both
   stale dependencies; `upgrade="never"` selects neither.
9. Ran `lintr::lint()` on all changed R/test files and the new generation script.
   Changed-line findings: zero. Whole-file findings: 49, exactly the count in
   the corresponding unchanged base files. Ran `git diff --check` successfully.
10. Ran checkout documentation/integration regressions:
    `devtools::test(filter="0026|9005|9204", reporter="summary", stop_on_failure=TRUE)` —
    all three files passed, with one skip for unavailable historical Task 07
    tasklist audit evidence. These cover source/Roxygen and rubric docs plus the
    executable offline integrated CJ vignette.

## Full package checks

Both final checks returned **Status: OK**, with zero errors, warnings, or notes.

| Check | Passing assertions | Skips | Manual |
| --- | ---: | ---: | --- |
| R 4.6.1 | 36,503 | 57 | PDF manual checked successfully |
| R 4.5.3 | 36,407 | 58 | `--no-manual`; LaTeX absent in the validation image |

Exact release command (includes the full suite, examples, rebuilt vignettes,
and PDF manual):

```sh
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 R CMD check \
  --output=/tmp/pairwiseLLM-dependency-validation/release-final-check \
  /tmp/pairwiseLLM-dependency-validation/pairwiseLLM_1.6.0.tar.gz
```

Exact oldrel invocation (includes the full suite, examples and rebuilt vignettes):

```sh
docker run --rm --name pairwisellm-dependency-oldrel-check-final --network host \
  -e R_LIBS_USER=/validation/oldrel-library \
  -e OPENBLAS_NUM_THREADS=1 -e OMP_NUM_THREADS=1 \
  -v /tmp/pairwiseLLM-dependency-validation:/validation \
  -v /tmp/pairwiseLLM-dependency-validation/oldrel-library:/usr/local/lib/R/site-library:ro \
  -w /validation pairwisellm-dependency-validation:oldrel \
  R CMD check --no-manual --output=/validation/oldrel-final-check \
  /validation/pairwiseLLM_1.6.0.tar.gz
```

The derived image adds only libuv/NLopt system headers to
`pairwisellm-linux-ci:oldrel`; all R dependencies live in the mounted isolated
library. Both checks used the same final source archive. Test elapsed times were
1,093.653 seconds (release) and 1,082.281 seconds (oldrel).

Skips comprise opt-in Python/real Stan tests and repository-only evidence/docs
absent from installed packages; oldrel adds the version-specific glmnet 5.0 frozen
refit. The full suite did not opt into large BT refits. Source-documentation
regressions are also run separately from the checkout.

## Setup corrections and limits

The first oldrel setup recursively selected unnecessary transitive Suggests and
was stopped during setup. The bounded replacement installed direct package check
dependencies and their required dependencies. Missing libuv/NLopt headers were
supplied only in a derived validation image. The original images and host analysis
libraries were preserved. The final oldrel check exposes its isolated dependencies
as the container's site library so R's off-search-path loading check can find them.

An initial new nondefault-RNG test leaked its test setup's RNG kind when the caller
had no seed. That setup was corrected by establishing a local seed before changing
kinds. The subsequent focused run passed; the initial package check was interrupted
and its archive rebuilt before the final checks. Production algorithms were untouched.

Real Python extraction, opt-in large BT refits, and real E3 Stan qualification
were not enabled. No paid providers, production evidence, real sampler recovery,
study dependency repinning, merging, or releases were performed.

## Artifact compatibility

Failed checks with corroborated pre-1.3.0 `link-v1-rlang` provenance raise
`pairwiseLLM_link_hash_incompatibility`, retaining the general linking-contract
error superclass. Condition fields identify stored scheme, stored engine version,
and current engine version. Current-artifact tampering, unestablished version
differences, malformed metadata, and unknown schemes retain ordinary contract errors.

The frozen legacy reference has no engine provenance: failure cannot establish
which engine produced it. Compatible references without the new descriptive fields
remain accepted when their existing hashes verify. New descriptive provenance does
not enter the reference identity-hash payload. Original files/hashes are preserved,
including after rejected load, resume, and overwrite attempts.

Rebuilding uses original saved evidence and compatible Phase A payloads, with new
artifacts saved separately. Completed standalone fits whose fit-time evidence
identity no longer verifies cannot automatically be reused. Investigate using their
producing environment before separately deciding whether a refit is needed. No
validation path automatically submits LLM requests, samples models, or migrates hashes.

## Changed files

* Dependency metadata and CI: `DESCRIPTION`, `.github/workflows/R-CMD-check.yaml`.
* Runtime validation/provenance: `R/link_contract.R`, `R/link_result.R`,
  `R/link_session.R`, `R/rubric_reference.R`.
* Regressions: `tests/testthat/test-0008-seed-helpers.R`,
  `tests/testthat/test-0208-rubric-reference.R`,
  `tests/testthat/test-5070-link-input-contract.R`,
  `tests/testthat/test-5086-link-session.R`.
* Frozen evidence/reproduction: `tests/testthat/fixtures/link-hash-compatibility/README.md`,
  `tests/testthat/fixtures/link-hash-compatibility/rlang-1.2.0.rds`,
  `scripts/generate-legacy-link-hash-fixture.R`.
* Documentation: `NEWS.md`, `tests/testthat/README.md`, `inst/linking-contract.md`,
  `vignettes/linking-sessions.Rmd`, `man/prepare_link_input.Rd`,
  `man/prepare_linked_rubric_reference.Rd`, `man/save_link_session.Rd`.
* Validation record: `data-raw/dependency-compatibility/validation.md`.
