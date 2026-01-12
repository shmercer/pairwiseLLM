test_that(".ap_make_component_bridge_pairs returns empty output for invalid inputs", {
  ids <- c("A", "B")

  # Missing names on component/degree vectors
  gs1 <- list(component_id = c(1L, 2L), degree = c(0L, 0L))
  out1 <- pairwiseLLM:::.ap_make_component_bridge_pairs(gs1, ids, n_bridge = 1L)
  expect_equal(nrow(out1), 0L)

  # All component ids missing for requested ids
  gs2 <- list(
    component_id = setNames(c(NA_integer_, NA_integer_), ids),
    degree = setNames(c(0L, 0L), ids)
  )
  out2 <- pairwiseLLM:::.ap_make_component_bridge_pairs(gs2, ids, n_bridge = 1L)
  expect_equal(nrow(out2), 0L)

  # Only one component present
  gs3 <- list(
    component_id = setNames(c(1L, 1L), ids),
    degree = setNames(c(0L, 0L), ids)
  )
  out3 <- pairwiseLLM:::.ap_make_component_bridge_pairs(gs3, ids, n_bridge = 1L)
  expect_equal(nrow(out3), 0L)

  # Component sizes indicate an LCC, but the LCC has zero ids after filtering
  # (simulate by providing ids not in the named vectors)
  gs4 <- list(
    component_id = setNames(c(1L, 2L), c("X", "Y")),
    degree = setNames(c(0L, 0L), c("X", "Y"))
  )
  out4 <- pairwiseLLM:::.ap_make_component_bridge_pairs(gs4, ids, n_bridge = 1L)
  expect_equal(nrow(out4), 0L)
})
