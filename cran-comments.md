## Release

This is an update from CRAN version 1.1.0 to version 1.3.1.

## Test environments

- Local Pop!_OS 24.04 LTS, x86_64-pc-linux-gnu, R 4.6.1 (R-devel):
  `R CMD check --as-cran --no-manual pairwiseLLM_1.3.1.tar.gz`
  completed with 0 errors, 0 warnings, and 0 notes.
- Local minimum-dependency check with `_R_CHECK_DEPENDS_ONLY_=true`: status OK.
- Local suggested-dependency check with `_R_CHECK_SUGGESTS_ONLY_=true`: status OK.

-Winbuilder: completed with 0 errors, 0 warnings, and 1 note [R Under development (unstable) (2026-09-04 r90492 ucrt)]

-Rhub on GitHub CI with macOS-latest(release), windows-latest(release), ubuntu-latest(devel), ubuntu-latest(release), ubuntu-latest(oldrel-1): completed with 0 errors, 0 warnings, and 0 notes.

## R CMD check results

0 errors | 0 warnings | 0 notes 

## Reverse dependencies

The current CRAN release has no reverse dependencies.

## Internet access and privacy

The package can send user-supplied text to external large-language-model services
only when the user explicitly calls a live provider workflow. Documentation labels
these calls and explains credential and submitted-text handling. Checks, tests, and
evaluated vignette examples do not require credentials or make live provider calls;
tests mock external requests.

## External software and parallelism

CmdStan and Ollama are optional. Neither is installed or invoked automatically.
Examples and tests use at most two concurrent workers during checks. Optional
Bayesian functionality was tested locally with `cmdstanr` 0.9.0 and CmdStan
2.38.0 using two chains and two parallel chains.
