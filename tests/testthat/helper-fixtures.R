# tests/testthat/helper-fixtures.R

fixture_ids <- function(n) {
  as.character(seq_len(n))
}

fixture_empty_results <- function() {
  tibble::tibble(
    ID1 = character(),
    ID2 = character(),
    better_id = character()
  )
}

fixture_results_from_edges <- function(edges) {
  edges <- tibble::as_tibble(edges)

  # Allow callers to provide only ID1/ID2 (most tests do)
  if (!("better_id" %in% names(edges))) {
    # deterministic "winner" for fixture purposes
    edges$better_id <- edges$ID1
  }

  # Make sure required cols exist and are character
  edges <- dplyr::transmute(
    edges,
    ID1 = as.character(.data$ID1),
    ID2 = as.character(.data$ID2),
    better_id = as.character(.data$better_id)
  )

  edges
}
