# Shared fixtures/helpers for deterministic tests

fixture_ids <- function(n) {
  paste0("ID", seq_len(n))
}

fixture_empty_results <- function() {
  tibble::tibble(ID1 = character(), ID2 = character(), better_id = character())
}

fixture_results_from_edges <- function(edges) {
  edges <- tibble::as_tibble(edges)
  if (!all(c("ID1", "ID2") %in% names(edges))) {
    stop("`edges` must have columns ID1 and ID2.", call. = FALSE)
  }

  tibble::tibble(
    ID1 = as.character(edges$ID1),
    ID2 = as.character(edges$ID2),
    better_id = as.character(edges$ID1)
  )
}
