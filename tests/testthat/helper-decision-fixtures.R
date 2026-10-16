# Synthetic decision fixtures only; never contacts a provider.
decision_333_fixture <- function(name) {
  jsonlite::fromJSON(testthat::test_path("fixtures", "decision-333", name),
                     simplifyVector = FALSE)
}

decision_333_request <- function(provider = "openai", ...) {
  args <- list(
    ID1 = "A_with_vs_delimiter", text1 = "Synthetic sample A.\nSAMPLE_2: a literal label.",
    ID2 = "B:02", text2 = "Synthetic sample B says \"hello\".",
    trait_name = "Organization",
    trait_description = "Logical sequencing and connections.\nExclude spelling and grammar.",
    pair_uid = "pair-forward-333", provider = provider
  )
  do.call(pairwiseLLM:::.build_decision_request, utils::modifyList(args, list(...), keep.null = TRUE))
}

decision_333_response <- function(provider = "openai", p = c(0.3, 0.7),
                                  choice = "SAMPLE_2", confidence = 0.2) {
  response <- decision_333_fixture(paste0(provider, "-response.json"))
  answer <- list(type = "choice", choice = choice, confidence = confidence)
  if (provider == "openai") {
    answer$name <- "writing_trait"
    answer$probabilities <- list(list(value = "SAMPLE_1", probability = p[1L]),
                                 list(value = "SAMPLE_2", probability = p[2L]))
    response$answers <- list(answer)
  } else {
    answer$probabilities <- list(SAMPLE_1 = p[1L], SAMPLE_2 = p[2L])
    response$answers <- list(writing_trait = answer)
  }
  response
}

decision_333_pairs <- function(requests) {
  dplyr::bind_rows(lapply(requests, function(request) {
    p <- request$provenance
    evidence <- jsonlite::fromJSON(request$body$input %||% request$body$state)
    tibble::tibble(ID1 = p$ID1, text1 = evidence$SAMPLE_1, ID2 = p$ID2,
                   text2 = evidence$SAMPLE_2, pair_uid = request$pair_uid)
  }))
}
