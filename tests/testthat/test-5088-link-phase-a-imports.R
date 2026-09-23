link_import_fixture <- function(estimator = "joint_offset") {
  args <- link_contract_args(estimator, 2L)
  artifacts <- lapply(c("hub", "spoke"), function(k) {
    id <- args[[k]]
    rows <- tibble::tibble(pair_id = 1L, step_id = 1L, A_item = "a", B_item = "b", y_A = 1L)
    list(set_id = id$set_id, fit_model_id = "btl_e_b", n_items = 2L, n_pairs_committed = 1L,
      items = data.frame(item_id = c("a", "b"), theta_raw_mean = c(-1, 1)),
      posterior_draws = cbind(a = c(-1, -2, -1.5), b = c(1, 2, 1.5)),
      phase_a_within_set_evidence = rows,
      phase_a_within_set_evidence_hash = pairwiseLLM:::.adaptive_phase_a_hash_object(rows))
  })
  args$phase_a <- stats::setNames(lapply(artifacts, function(a) list(artifact = a)), c("hub", "spoke"))
  args
}

test_that("historical artifacts support each estimator's own evidence contract", {
  for (id in c("fixed_shape_offset", "gaussian_posterior_bridge", "joint_offset")) {
    args <- link_import_fixture(id)
    input <- do.call(prepare_link_input, args)
    expect_true(fit_link(input)$diagnostics$fit_valid)
    expect_identical(input$phase_a$hub$source$artifact_hash, pairwiseLLM:::.link_hash(args$phase_a$hub$artifact))
    expect_identical(input$counts$source_hub, 1L)
    expect_identical(input$counts$phase_a_hub, if (id == "joint_offset") 1L else 0L)
    if (id == "joint_offset") expect_false(anyDuplicated(c(input$phase_a$hub$value$observation_id,
      input$phase_a$spoke$value$observation_id)) > 0L)
    args$phase_a$hub$artifact$posterior_draws <- NULL
    if (id == "gaussian_posterior_bridge") {
      expect_error(do.call(prepare_link_input, args), "posterior item draws")
    } else expect_s3_class(do.call(prepare_link_input, args), "pairwiseLLM_link_input")
    args$phase_a$hub$artifact$phase_a_within_set_evidence <- NULL
    if (id == "joint_offset") expect_error(do.call(prepare_link_input, args), "exact raw within-set rows")
    if (id == "fixed_shape_offset") expect_s3_class(do.call(prepare_link_input, args), "pairwiseLLM_link_input")
  }
  args <- link_contract_args("joint_offset")
  args$phase_a$hub <- list(points = c(a = -1, b = 1))
  expect_error(do.call(prepare_link_input, args), "E3 requires exact raw within-set observations")
})

test_that("E3 imports reject stale evidence, summaries-as-evidence and incompatible artifacts", {
  args <- link_import_fixture()
  a <- args$phase_a$hub$artifact
  changes <- list(
    function(x) { x$set_id <- "other"; x },
    function(x) { x$fit_model_id <- "btl"; x },
    function(x) { x$phase_scope <- "phase_b"; x },
    function(x) { x$phase_scope_set_id <- "S"; x },
    function(x) { x$n_items <- 4L; x },
    function(x) { x$items$item_id <- c("z", "b"); x },
    function(x) { x$phase_a_within_set_evidence$y_A <- 0L; x },
    function(x) { x$n_pairs_committed <- 2L; x },
    function(x) { x$phase_a_within_set_evidence_hash <- NULL; x$phase_a_within_set_evidence$y_A <- .5; x },
    function(x) { x$phase_a_within_set_evidence_hash <- NULL; x$phase_a_within_set_evidence$pair_id <- .5; x })
  for (change in changes) {
    args$phase_a$hub$artifact <- change(a)
    expect_error(do.call(prepare_link_input, args), class = "pairwiseLLM_link_contract_error")
  }
  args <- link_import_fixture()
  input <- do.call(prepare_link_input, args)
  args$phase_a$hub$source <- input$phase_a$hub$source
  expect_identical(do.call(prepare_link_input, args), input)
  args$phase_a$hub$source$n_observations <- 9L
  expect_error(do.call(prepare_link_input, args), "Artifact source mismatch")
  args <- link_import_fixture()
  args$phase_a$hub$observations <- input$phase_a$hub$value
  expect_error(do.call(prepare_link_input, args), "single-use evidence")
  args <- link_import_fixture()
  args$phase_a$hub$artifact$phase_a_within_set_evidence <- input$phase_a$hub$value
  args$phase_a$hub$artifact$phase_a_within_set_evidence_hash <- NULL
  expect_identical(do.call(prepare_link_input, args)$phase_a$hub$value, input$phase_a$hub$value)
  args <- link_import_fixture()
  args$phase_a$hub$artifact$phase_a_within_set_evidence <- args$phase_a$hub$artifact$phase_a_within_set_evidence[FALSE, ]
  args$phase_a$hub$artifact$phase_a_within_set_evidence_hash <- NULL
  args$phase_a$hub$artifact$n_pairs_committed <- 0L
  expect_identical(do.call(prepare_link_input, args)$counts$phase_a_hub, 0L)
})

test_that("optional artifact labels do not invalidate historical missing metadata", {
  args <- link_import_fixture()
  args$phase_a$hub$artifact$phase_scope_set_id <- "H"
  args$phase_a$hub$artifact$trait <- NA_character_
  args$phase_a$hub$artifact$orientation <- NA_character_
  input <- do.call(prepare_link_input, args)
  expect_identical(input$phase_a$hub$source$trait, NA_character_)
  expect_identical(input$phase_a$hub$source$orientation, NA_character_)
  args$phase_a$hub$artifact$orientation <- "lower_is_better"
  expect_error(do.call(prepare_link_input, args), "orientation")
  args$phase_a$hub$artifact$orientation <- "higher_is_better"
  args$phase_a$hub$artifact$trait <- "organization"
  input <- do.call(prepare_link_input, args)
  expect_identical(input$phase_a$hub$source$orientation, "higher_is_better")
  expect_identical(input$phase_a$hub$source$trait, "organization")
})


test_that("E3 verifies the stored evidence representation before normalizing rows", {
  for (table_class in c("tibble", "data.frame")) {
    args <- link_import_fixture()
    a <- args$phase_a$hub$artifact
    rows <- a$phase_a_within_set_evidence
    if (table_class == "data.frame") rows <- as.data.frame(rows)
    # Equivalent tables need not serialize identically. Historical artifacts hash
    # the original representation, including its attribute order and row names.
    attributes(rows) <- rev(attributes(rows))
    hash <- pairwiseLLM:::.adaptive_phase_a_hash_object(rows)
    expect_false(identical(hash,
      pairwiseLLM:::.adaptive_phase_a_hash_object(tibble::as_tibble(rows))))
    a$phase_a_within_set_evidence <- rows
    a$phase_a_within_set_evidence_hash <- hash
    args$phase_a$hub$artifact <- a
    input <- do.call(prepare_link_input, args)
    expect_identical(input$phase_a$hub$source$evidence_hash, hash)
    expect_identical(input$phase_a$hub$value$y_A, rows$y_A)
    args$phase_a$hub$artifact$phase_a_within_set_evidence$y_A <- 0L
    expect_error(do.call(prepare_link_input, args), "raw evidence hash mismatch")
  }
})
