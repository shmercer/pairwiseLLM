#' Save a standalone Bayesian ranking as a reusable rubric reference
#'
#' Prepare a completed reference ranking before scoring any new samples. This
#' supports reference rankings fitted from pooled historical comparisons as well
#' as a single collection of comparisons. The original scores and comparisons
#' are saved together so later linking can check that it uses the same reference.
#'
#' @param cj A completed [fit_bayes_btl_mcmc()] result. With several refits, the
#'   last refit is used. It must record evidence identity at fit time. Older
#'   results must be refitted from their original comparisons first; they can
#'   still be used for ordinary same-set rubric calibration.
#' @param evidence The exact comparison rows used for that refit, in the same
#'   results-table format supplied to [fit_bayes_btl_mcmc()]. For a subset fit,
#'   supply only that subset. See [build_btl_results_data()].
#' @param set_id A single reference-set identifier, as in [prepare_link_input()].
#' @param items Optional data frame with `item_id` and `global_item_id`, mapping
#'   every fitted item to a unique ID across sets. When omitted, fitted item IDs
#'   are also used as global IDs.
#' @param trait The trait being ranked, such as `"organization"`. Required when
#'   the completed result does not already record it.
#' @param provenance Optional named list of serializable study/source metadata,
#'   such as `source_commit`. Stored separately from computed identity fields.
#'
#' @return A serializable `pairwiseLLM_linked_rubric_reference`. Supply it to
#'   [fit_rubric_calibration()] with `calibration_design = "linked_anchors"`.
#'   `items` holds global IDs, source IDs, frozen scores and SDs. For linking,
#'   `hub` supplies the set/item mapping; `points`, `posterior_draws`, and
#'   `evidence` supply the E1, E2, and E3 hub inputs, respectively. Attach `source`
#'   to each input to carry the same computed `reference_hash`. `judge` contains
#'   the shared bias/lapse settings from the accepted fit. Diagnostics and
#'   reliability are retained; failed diagnostics warn as in ordinary calibration.
#' @details
#' The supplied comparisons must match the evidence recorded when the fit was
#' made. Compatible item names alone are insufficient. Changed comparisons
#' require a new fit and a new reference. Save the result with [saveRDS()] and
#' restore it with [readRDS()]; no live sampler is needed for later prediction.
#'
#' This constructor supports standalone fits with shared judge parameters.
#' Import-ready adaptive Phase A artifacts continue to work directly with
#' [fit_rubric_calibration()]. Neither route changes the original reference
#' scores to match the distribution of a new set.
#' @seealso [prepare_link_input()], [fit_rubric_calibration()]
#' @family rubric calibration
#' @export
prepare_linked_rubric_reference <- function(cj, evidence, set_id, items = NULL,
                                           trait = NULL, provenance = list()) {
  normalized <- .rubric_normalize_cj(cj, trait, scale_status = "within_set")
  .link_check(identical(normalized$estimation_mode, "fixed"),
    "Supply a completed standalone fit_bayes_btl_mcmc() result.")
  fit <- utils::tail(cj$fits, 1L)[[1L]]
  recorded <- fit$evidence_identity
  .link_check(is.list(recorded) && identical(recorded$format_version, 1L) &&
    is.list(fit$reference_fit_config),
    paste0("This standalone fit has no verified fit-time evidence identity. ",
      "Refit from the original evidence with fit_bayes_btl_mcmc() before preparing a linked reference."))
  ids <- normalized$items$item_id
  identity <- .btl_evidence_identity(evidence, ids)
  .link_check(identical(identity, recorded),
    "Reference evidence does not match the evidence recorded by the completed fit; supply its exact fitted rows.")
  .rubric_choice(fit$inference_contract$judge_param_mode, "global_shared", "judge_param_mode")
  configuration <- .btl_reference_fit_config(fit,
    list(cmdstan = fit$reference_fit_config[c("iter_warmup", "iter_sampling")]), fit$reference_fit_config$seed)
  .link_check(identical(fit$reference_fit_config, configuration),
    "Recorded fit configuration does not match the completed model, prior, or inference contract.")
  if (is.null(items)) items <- data.frame(item_id = ids, global_item_id = ids)
  hub <- .link_identity(list(set_id = set_id, items = items), "reference hub")
  .link_check(setequal(hub$items$item_id, ids) && !anyNA(hub$items$global_item_id),
    "Reference items must map the exact fitted item domain to complete global item IDs.")
  .link_fields(provenance, names(provenance), label = "provenance")
  .link_check(.link_data_only(provenance), "Reference provenance must contain serializable data only.")
  index <- match(hub$items$item_id, ids)
  metric <- .rubric_items(ids[index], normalized$items$theta[index], normalized$items$theta_sd[index],
    hub$items$global_item_id, rep(hub$set_id, length(ids)))
  metric <- metric[order(metric$item_id, method = "radix"), ]
  draws <- .rubric_draws(fit$theta_draws, hub$items$item_id, hub$items$item_id)
  rownames(draws) <- NULL
  storage.mode(draws) <- "double"
  judge <- .link_judge(list(model_variant = fit$model_variant,
    beta = if (model_has_b(fit$model_variant)) fit$beta_mean else 0,
    epsilon = if (model_has_e(fit$model_variant)) fit$epsilon_mean else 0,
    link = "logit", source = "standalone Bayesian reference"))
  contract <- list(model_variant = fit$model_variant, judge_param_mode = "global_shared",
    judge = judge, configuration = fit$reference_fit_config)
  rows <- identity$observations
  observations <- tibble::tibble(
    observation_id = paste0("reference:", nchar(hub$set_id, type = "bytes"), ":", hub$set_id, ":", seq_len(nrow(rows))),
    A_set = hub$set_id, A_item = rows$A_item, B_set = hub$set_id, B_item = rows$B_item, y_A = rows$y_A)
  out <- structure(list(format_version = 1L, source_type = "standalone_bayes_btl",
    set_id = hub$set_id, items = metric, hub = hub,
    points = stats::setNames(normalized$items$theta[index], hub$items$item_id),
    posterior_draws = draws, judge = judge, trait = normalized$trait, orientation = normalized$orientation,
    fit_contract = contract, fit_contract_hash = .link_hash(contract),
    fit_evidence = identity, evidence = observations, evidence_hash = .link_hash(observations),
    n_observations = nrow(observations), diagnostics = normalized$diagnostics, reliability = normalized$reliability,
    provenance = list(package_version = as.character(utils::packageVersion("pairwiseLLM")), supplied = provenance)),
    class = "pairwiseLLM_linked_rubric_reference")
  out$reference_hash <- .rubric_standalone_hash(out)
  out$source <- .link_source(list(reference_hash = out$reference_hash, evidence_hash = out$evidence_hash,
    n_observations = out$n_observations, trait = out$trait, orientation = out$orientation))
  .rubric_validate_standalone_reference(out)
  out
}

.rubric_standalone_hash <- function(x) {
  # The constructor canonicalizes all identity fields; provenance is descriptive.
  .link_hash(unclass(x)[setdiff(names(x), c("reference_hash", "source", "provenance"))])
}

.rubric_validate_standalone_reference <- function(x) {
  .link_check(inherits(x, "pairwiseLLM_linked_rubric_reference") && identical(x$format_version, 1L) &&
    identical(x$source_type, "standalone_bayes_btl") && .link_data_only(unclass(x)),
    "Invalid frozen standalone rubric reference.")
  .link_check(identical(x$reference_hash, .rubric_standalone_hash(x)) &&
    identical(x$fit_contract_hash, .link_hash(x$fit_contract)) &&
    identical(x$evidence_hash, .link_hash(x$evidence)),
    "Frozen reference identity has changed; prepare a new reference from its completed fit and exact evidence.")
  .link_check(identical(x$source, .link_source(list(reference_hash = x$reference_hash,
    evidence_hash = x$evidence_hash, n_observations = x$n_observations,
    trait = x$trait, orientation = x$orientation))), "Frozen reference source identity does not match its contents.")
  invisible(x)
}

.rubric_cj_standalone_reference <- function(x, trait) {
  .rubric_validate_standalone_reference(x)
  .rubric_new_cj(x$items, x$fit_contract$model_variant, "phase_a", "phase_a_reference",
    .rubric_trait(trait, x$trait), fit_contract = x$fit_contract, fit_contract_hash = x$fit_contract_hash,
    provenance = c(list(finalization = "verified_standalone_reference"), x$provenance),
    diagnostics = x$diagnostics, reliability = x$reliability,
    posterior_draws = .rubric_draws(x$posterior_draws, x$items$source_item_id, x$items$item_id), reference = x)
}

.rubric_standalone_link_match <- function(reference, input) {
  .link_check(identical(input$hub, reference$hub) &&
    identical(input$phase_a$hub$source, reference$source),
    "Linked prediction requires the stored rubric reference hub identity and source.")
  .link_check(identical(input$judge[c("beta", "epsilon", "model_variant", "link")],
    reference$judge[c("beta", "epsilon", "model_variant", "link")]),
    "Linked prediction judge settings do not match the frozen reference contract.")
  a <- input$phase_a$hub
  raw <- switch(a$kind, points = reference$points, draws = reference$posterior_draws,
    observations = reference$evidence)
  expected <- .link_phase_a(c(stats::setNames(list(raw), a$kind), list(source = reference$source)),
    reference$hub, a$kind)
  # Check both the estimator payload and the origin removed by normalization.
  .link_check(identical(a, expected),
    "Linked prediction hub payload does not match the frozen reference points, draws, or evidence.")
  invisible(TRUE)
}
