test_that("all E1--E3 common results transport hub-only calibration to spoke scores", {
  skip_if_not_installed("ordinal")
  for (id in c("fixed_shape_offset", "gaussian_posterior_bridge", "joint_offset")) {
    data <- rubric_linked_fixture(estimator = id, reference_shift = .25)
    fit <- rubric_linked_fit(data)
    session <- data$state
    result <- session$linking$estimator$accepted_state_by_spoke[["2"]]
    before <- serialize(session, NULL)
    prediction <- stats::predict(fit, session)
    expect_identical(prediction, stats::predict(fit, result))
    expect_identical(serialize(session, NULL), before)
    items <- result$items[result$items$set_id == "2", ]
    expect_equal(prediction$theta, items$theta_link_mean + mean(data$reference$items$theta_raw_mean))
    expect_identical(prediction$theta_sd, items$theta_link_sd)
    expect_identical(prediction$item_id, items$global_item_id)
    z <- (prediction$theta - fit$transformation$center) / fit$transformation$scale
    direct <- stats::predict(fit$backend$model, data.frame(z = z), type = "prob")$fit
    expect_equal(unname(do.call(rbind, prediction$probabilities)), unname(direct), tolerance = 1e-12)
    expect_identical(attr(prediction, "linking")$provenance$estimator_id, id)
    expect_identical(attr(prediction, "linking")$provenance$uncertainty_scope, result$diagnostics$uncertainty_scope)
    changed <- data$reference
    changed$items$theta_raw_mean <- changed$items$theta_raw_mean + 1
    other_fit <- fit_rubric_calibration(changed, data$rubric, method = "ordinal_linear",
      calibration_design = "linked_anchors", levels = data$levels)
    expect_error(stats::predict(other_fit, session), "stored rubric reference hub")
    input <- result$continuation$input
    raw <- lapply(input$phase_a, function(a) stats::setNames(list(a$value), a$kind))
    no_cross <- prepare_link_input(id, input$hub, input$spoke, raw,
      input$cross[FALSE, ], input$judge)
    expect_error(stats::predict(fit, fit_link(no_cross)), "cross-set identified")
  }
})

test_that("declared trait and orientation provenance survive linking and prevent incompatible calibration", {
  skip_if_not_installed("ordinal")
  data <- rubric_linked_fixture()
  fit <- rubric_linked_fit(data)
  r <- data$state$linking$estimator$accepted_state_by_spoke[["2"]]
  input <- r$continuation$input
  raw <- lapply(input$phase_a, function(a) c(stats::setNames(list(a$value), a$kind), list(source = a$source)))
  raw$spoke$source$trait <- "mechanics"
  other <- prepare_link_input(input$estimator, input$hub, input$spoke, raw, input$cross, input$judge)
  expect_error(stats::predict(fit, fit_link(other)), "Trait mismatch")
  raw$spoke$source$orientation <- "lower_is_better"
  expect_error(prepare_link_input(input$estimator, input$hub, input$spoke, raw, input$cross, input$judge),
    "orientation")
})
