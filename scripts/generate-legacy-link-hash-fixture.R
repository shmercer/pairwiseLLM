# Generate once from unchanged source at 2526ab393d994530573af71bf15d89371c5f6228.
# Usage: Rscript --vanilla scripts/generate-legacy-link-hash-fixture.R BASE_SOURCE LEGACY_LIBRARY OUTPUT
# Never install into an analysis library or regenerate during routine tests.
args <- commandArgs(TRUE)
stopifnot(length(args) == 3L)
.libPaths(c(args[[2L]], .libPaths()))
stopifnot(utils::packageVersion("rlang") == "1.2.0")
pkgload::load_all(args[[1L]], quiet = TRUE)
source(file.path(args[[1L]], "tests/testthat/helper-link-contract.R"))
source(file.path(args[[1L]], "tests/testthat/helper-rubric-reference.R"))
input <- link_contract_input(edges = 2L)
result <- pairwiseLLM::fit_link(input)
session <- pairwiseLLM::start_link_session(input)
reference <- testthat::with_mocked_bindings(
  rubric_reference_prepare(rubric_reference_completed()),
  .fit_bayes_btl_mcmc_adaptive = rubric_reference_sampler, .package = "pairwiseLLM")
fixture <- list(input = input, result = result, session = session, reference = reference)
dir.create(dirname(args[[3L]]), recursive = TRUE, showWarnings = FALSE)
saveRDS(fixture, args[[3L]], version = 3)
cat(R.version.string, "\nrlang", as.character(utils::packageVersion("rlang")), "\n")
print(tools::md5sum(args[[3L]]))
