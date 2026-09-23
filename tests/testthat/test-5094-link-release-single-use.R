test_that("doubling raw Phase A contributes exactly one additional E3 likelihood", {
  args <- link_e3_args()
  first <- do.call(prepare_link_input, args)
  for (k in c("hub", "spoke")) {
    repeated <- args$phase_a[[k]]$observations
    repeated$observation_id <- paste0("repeat-", repeated$observation_id)
    args$phase_a[[k]]$observations <- rbind(args$phase_a[[k]]$observations, repeated)
  }
  doubled <- do.call(prepare_link_input, args)
  q <- seq(-.3, .5, length.out = ncol(first$item_transform))
  objective1 <- link_e3_objective(first)
  objective2 <- link_e3_objective(doubled)
  raw <- first
  raw$cross <- first$cross[FALSE, ]
  prior_only <- raw
  prior_only$phase_a$hub$value <- raw$phase_a$hub$value[FALSE, ]
  prior_only$phase_a$spoke$value <- raw$phase_a$spoke$value[FALSE, ]
  expect_equal(objective2(q) - objective1(q),
    link_e3_objective(raw)(q) - link_e3_objective(prior_only)(q), tolerance = 1e-10)
  fit1 <- fit_link(first)
  fit2 <- fit_link(doubled)
  expect_true(fit2$diagnostics$fit_valid)
  expect_equal(fit2$diagnostics$optimization$objective,
    unname(objective2(fit2$continuation$mode)), tolerance = 1e-10)
  kernel <- pairwiseLLM:::.link_e3_kernel(doubled)
  whitened <- as.double(solve(kernel$lower, q - kernel$mean))
  expect_equal(pairwiseLLM:::.link_gaussian_objective(whitened, kernel)$value,
    objective2(q), tolerance = 1e-10)
  expect_identical(fit2$provenance$counts$phase_a_hub, 2L * first$counts$phase_a_hub)
  expect_false(isTRUE(all.equal(fit1$items$theta_link_sd, fit2$items$theta_link_sd)))
  args$phase_a$hub$points <- c(a = -1, b = 0, c = 1)
  expect_error(do.call(prepare_link_input, args), class = "pairwiseLLM_link_contract_error")
})

test_that("E1 and E2 never replay raw outcomes stored alongside their Phase A summaries", {
  for (id in link_release_ids[1:2]) {
    args <- link_release_args(id)
    args$phase_a <- lapply(c("hub", "spoke"), function(k) {
      draws <- link_e2_args()$phase_a[[k]]$draws
      artifact <- link_e2_artifact(args[[k]], draws)
      artifact$items$theta_raw_mean <- colMeans(draws)
      list(artifact = artifact)
    }) |> stats::setNames(c("hub", "spoke"))
    first <- do.call(prepare_link_input, args)
    before <- fit_link(first)
    for (k in c("hub", "spoke")) {
      args$phase_a[[k]]$artifact$phase_a_within_set_evidence$y_A <- c(0L, 1L)
    }
    second <- do.call(prepare_link_input, args)
    after <- fit_link(second)
    expect_identical(before$items, after$items)
    expect_identical(before$offset, after$offset)
    expect_identical(second$counts$phase_a_hub, 0L)
    expect_identical(second$counts$phase_a_spoke, 0L)
    expect_false(identical(first$phase_a$hub$source$artifact_hash, second$phase_a$hub$source$artifact_hash))
    args$phase_a$hub$observations <- link_e3_args()$phase_a$hub$observations
    expect_error(do.call(prepare_link_input, args), class = "pairwiseLLM_link_contract_error")
  }
})
