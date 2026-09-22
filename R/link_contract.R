# Standalone linking contracts. Never depend on adaptive or anchored-joint state.

.link_check <- function(ok, message) {
  if (!isTRUE(ok)) rlang::abort(message, class = "pairwiseLLM_link_contract_error")
  invisible(TRUE)
}

.link_fields <- function(x, allowed, required = character(), label) {
  .link_check(is.list(x) && (length(x) == 0L ||
    (!is.null(names(x)) && !anyNA(names(x)) && !anyDuplicated(names(x)) &&
      all(nzchar(names(x))))), paste0(label, " must be a named list or table."))
  .link_check(all(names(x) %in% allowed) && all(required %in% names(x)),
    paste0(label, " has missing or incompatible fields; allowed: ", paste(allowed, collapse = ", "), "."))
}

.link_ids <- function(x, label, unique = TRUE, empty = FALSE) {
  .link_check((is.character(x) || is.numeric(x)) && is.null(dim(x)) &&
    (empty || length(x) > 0L) && !anyNA(x) &&
    (!is.numeric(x) || all(is.finite(x))) && all(nzchar(trimws(as.character(x)))),
    paste0(label, " must contain nonempty, nonmissing IDs."))
  x <- enc2utf8(as.character(x))
  .link_check(!unique || !anyDuplicated(x), paste0(label, " must contain unique IDs."))
  x
}

.link_scalar <- function(x, label, lower = -Inf, upper = Inf, missing = FALSE) {
  .link_check(is.numeric(x) && length(x) == 1L && is.null(dim(x)) &&
    ((missing && is.na(x) && !is.nan(x)) ||
      (is.finite(x) && x >= lower && x <= upper)), paste0("Invalid ", label, "."))
  as.double(x)
}

.link_hash <- function(x) {
  paste0("link-v1-rlang-", rlang::hash(x))
}

.link_identity <- function(x, label) {
  .link_fields(x, c("set_id", "items"), c("set_id", "items"), label)
  set_id <- .link_ids(x$set_id, paste0(label, " set_id"))
  .link_check(length(set_id) == 1L, "Each set must have one set_id.")
  .link_check(is.data.frame(x$items), "items must be a data frame.")
  .link_fields(x$items, c("item_id", "global_item_id"), "item_id", "items")
  ids <- .link_ids(x$items$item_id, "item_id")
  global <- rep(NA_character_, length(ids))
  if (!is.null(x$items$global_item_id)) {
    present <- !is.na(x$items$global_item_id)
    global[present] <- .link_ids(x$items$global_item_id[present], "global_item_id", empty = TRUE)
  }
  items <- tibble::tibble(item_id = ids, global_item_id = global)
  list(set_id = set_id, items = items[order(ids, method = "radix"), , drop = FALSE])
}

.link_observations <- function(x, hub, spoke = NULL, outcomes = TRUE) {
  fields <- c("observation_id", "A_set", "A_item", "B_set", "B_item")
  if (outcomes) fields <- c(fields, "y_A")
  .link_check(is.data.frame(x), "Evidence/pairs must be an explicit data frame, including at zero rows.")
  .link_fields(x, fields, fields, "Evidence/pairs")
  out <- lapply(fields[fields != "y_A"], function(k) {
    .link_ids(x[[k]], k, unique = k == "observation_id", empty = TRUE)
  })
  names(out) <- fields[fields != "y_A"]
  out <- tibble::as_tibble(out)
  in_set <- function(set, item, identity) {
    set == identity$set_id & item %in% identity$items$item_id
  }
  ah <- in_set(out$A_set, out$A_item, hub)
  bh <- in_set(out$B_set, out$B_item, hub)
  legal <- if (is.null(spoke)) {
    ah & bh & out$A_item != out$B_item
  } else {
    (ah & in_set(out$B_set, out$B_item, spoke)) |
      (bh & in_set(out$A_set, out$A_item, spoke))
  }
  .link_check(all(legal), "Evidence/pairs contain illegal set/item endpoints or self comparisons.")
  if (outcomes) {
    .link_check(is.numeric(x$y_A) && !anyNA(x$y_A) && all(x$y_A %in% c(0, 1)),
      "y_A must contain exact binary outcomes, without missing values.")
    out$y_A <- as.integer(x$y_A)
  }
  out
}

.link_source <- function(x = list()) {
  .link_fields(x, c("artifact_hash", "evidence_hash", "n_observations"), label = "Phase A source")
  out <- list(artifact_hash = NA_character_, evidence_hash = NA_character_, n_observations = NA_integer_)
  for (k in intersect(names(x), c("artifact_hash", "evidence_hash"))) {
    .link_check(is.character(x[[k]]) && length(x[[k]]) == 1L &&
      (is.na(x[[k]]) || nzchar(x[[k]])), "Source hashes must be scalar strings or NA_character_.")
    out[[k]] <- x[[k]]
  }
  if (!is.null(x$n_observations)) {
    n <- .link_scalar(x$n_observations, "source n_observations", 0, .Machine$integer.max, missing = TRUE)
    .link_check(is.na(n) || n == floor(n), "Source n_observations must be an integer.")
    out$n_observations <- as.integer(n)
  }
  out
}

.link_phase_a <- function(x, identity, kind) {
  .link_fields(x, c(kind, "source"), kind, "Phase A input (single-use evidence)")
  source <- .link_source(x$source %||% list())
  ids <- identity$items$item_id
  centering <- "none"
  removed <- NULL
  if (kind == "observations") {
    value <- .link_observations(x[[kind]], identity)
    if (!is.na(source$n_observations)) {
      .link_check(source$n_observations == nrow(value), "Phase A observation count mismatch.")
    }
    source$n_observations <- nrow(value)
  } else if (kind == "points") {
    value <- x[[kind]]
    .link_check(is.numeric(value) && is.null(dim(value)) && all(is.finite(value)),
      "Phase A points must be a finite named numeric vector.")
    nms <- .link_ids(names(value), "Phase A point names")
    .link_check(setequal(nms, ids), "Phase A point IDs must exactly match the set.")
    value <- as.double(value[match(ids, nms)])
    removed <- mean(value)
    value <- stats::setNames(value - removed, ids)
    centering <- "subtract_set_mean"
  } else {
    value <- x[[kind]]
    .link_check(is.matrix(value) && is.numeric(value) && nrow(value) >= 2L &&
      all(is.finite(value)), "Phase A draws must be a finite matrix with at least two rows.")
    nms <- .link_ids(colnames(value), "Phase A draw column names")
    .link_check(setequal(nms, ids), "Phase A draw IDs must exactly match the set.")
    value <- value[, match(ids, nms), drop = FALSE]
    storage.mode(value) <- "double"
    rownames(value) <- NULL
    colnames(value) <- ids
    removed <- rowMeans(value)
    value <- value - removed
    centering <- "subtract_each_draw_set_mean"
  }
  list(kind = kind, value = value, source = source,
    centering = list(method = centering, removed = removed))
}

.link_judge <- function(x) {
  fields <- c("beta", "epsilon", "model_variant", "link", "source")
  .link_fields(x, fields, fields, "judge")
  beta <- .link_scalar(x$beta, "judge beta")
  epsilon <- .link_scalar(x$epsilon, "judge epsilon", 0, 1)
  model <- normalize_model_variant(x$model_variant)
  .link_check(identical(x$link, "logit"), "Only the logit judge link is supported.")
  .link_check(model_has_b(model) || beta == 0, "beta must be zero for a model without positional bias.")
  .link_check(model_has_e(model) || epsilon == 0, "epsilon must be zero for a model without lapse.")
  source <- .link_ids(x$source, "judge source")
  .link_check(length(source) == 1L, "judge source must be a scalar.")
  list(beta = beta, epsilon = epsilon, model_variant = model, link = "logit", source = source)
}

.link_resolve <- function(estimator) {
  ids <- c("fixed_shape_offset", "gaussian_posterior_bridge", "joint_offset")
  .link_check(is.character(estimator) && length(estimator) == 1L &&
    !is.na(estimator) && estimator %in% ids, paste0("Choose an explicit estimator: ", paste(ids, collapse = ", "), "."))
  if (estimator == "fixed_shape_offset") return(list(id = estimator, version = "1",
    kind = "points", fit = .link_e1_fit, predict = .link_e1_predict,
    validate_control = .link_e1_control))
  list(id = estimator, version = "1", kind = c("points", "draws", "observations")[[match(estimator, ids)]],
    fit = NULL, predict = NULL,
    validate_control = function(x) {
      .link_fields(x, character(), label = "estimator controls (not yet implemented)")
      x
    })
}

.link_control <- function(x, backend, coordinates) {
  .link_fields(x, c("delta_prior", "estimator", "initial"), label = "control")
  prior <- x$delta_prior %||% list(mean = 0, sd = 5)
  .link_fields(prior, c("mean", "sd"), c("mean", "sd"), "delta_prior")
  prior <- list(mean = .link_scalar(prior$mean, "delta prior mean"),
    sd = .link_scalar(prior$sd, "delta prior SD", 0))
  .link_check(prior$sd > 0, "delta prior SD must be positive.")
  initial <- x$initial
  if (!is.null(initial)) {
    .link_check(is.numeric(initial) && is.null(dim(initial)) && all(is.finite(initial)) &&
      identical(names(initial), coordinates), "initial must be a finite named free-coordinate vector.")
    initial <- stats::setNames(as.double(initial), coordinates)
  }
  list(delta_prior = prior, estimator = backend$validate_control(x$estimator %||% list()), initial = initial)
}

#' Prepare explicit evidence for a linking estimator
#'
#' These development interfaces define the common contract for E1--E3. They do
#' not select pairs, contact providers, or depend on adaptive state. E1 is
#' implemented with deterministic quadrature; E2 and E3 report an unavailable
#' estimator instead of running a legacy linker.
#'
#' @param estimator Required exact ID: `fixed_shape_offset`,
#'   `gaussian_posterior_bridge`, or `joint_offset`. There is no default.
#' @param hub,spoke Lists with scalar `set_id` and an `items` data frame containing
#'   unique `item_id` and optional `global_item_id`. Local IDs may overlap between
#'   sets; supplied global IDs must be unique across both sets. Items are ordered
#'   hub first, then spoke, and by bytewise item ID within each set.
#' @param phase_a Named list with `hub` and `spoke` entries. Each entry contains
#'   exactly one statistical payload: E1 `points` (named numeric vector), E2
#'   `draws` (draws by named item columns), or E3 `observations` (table as below).
#'   Points and each draw are separately centered, with removed means recorded.
#'   Optional `source` metadata contains `artifact_hash`, `evidence_hash`, and
#'   `n_observations`; unavailable values remain typed missing. External source
#'   hashes are assertions of provenance, distinct from computed payload hashes.
#'   E1 also accepts `list(artifact = artifact)` in either set entry, mutually
#'   exclusive with `points`. Supply an in-memory canonical Phase A artifact
#'   (use [readRDS()] explicitly for files). Its `set_id`, `fit_model_id`,
#'   `n_items`, `n_pairs_committed`, and `items` are checked. Item-aligned
#'   `items$theta_raw_mean` values are the EAP source; global IDs must match when
#'   supplied. Phase B summaries are rejected. The original artifact is hashed,
#'   and its declared within-set evidence hash/count are retained as provenance;
#'   raw outcomes, posterior draws, and marginal SDs are not used for E1
#'   inference or retained in normalized input. Artifact `source` fields, if
#'   supplied, must agree with the extracted metadata. This extracts statistical
#'   inputs; it does not run adaptive Phase A quality/reliability gates.
#' @param cross Explicit active cross-set observations, including an empty table
#'   at zero budget. Evidence tables contain `observation_id`, `A_set`, `A_item`,
#'   `B_set`, `B_item`, and numeric binary `y_A` (one means A won). IDs identify
#'   judgments, not pairs, and must be unique across all raw evidence supplied.
#'   Repeated judgments of a pair use different IDs. All rows are used in supplied
#'   order; invalid rows are rejected rather than dropped. Phase A observations
#'   use the same columns and must be within the corresponding set.
#' @param judge Named list with finite `beta`, `epsilon` in `[0,1]`,
#'   `model_variant` (`btl`, `btl_b`, `btl_e`, or `btl_e_b`), `link = "logit"`,
#'   and scalar `source`. Omitted model components must have value zero.
#' @param control List with `delta_prior = list(mean = 0, sd = 5)`, numerical
#'   `estimator` controls, and optional named `initial` free-coordinate vector.
#'   E1 accepts positive `rel_tol = 1e-9`, `abs_tol = 1e-11`,
#'   `quantile_tol = 1e-8`, and integer `subdivisions = 1000L`. The subdivision
#'   limit bounds the number of quadrature panels and CDF integration/root
#'   iterations. Tolerances apply to normalized mass and moments in prior-SD
#'   coordinates; `quantile_tol` is in delta units. Effective defaults are logged
#'   with every fit. Other estimators currently accept no numerical controls.
#'   Initial values are optimization hints only; E1 does not use them.
#' @param provenance List with optional `source_commit` (package source revision;
#'   unknown is `NA_character_`) and `expected`, a named subset of the computed
#'   `hashes` and `counts` lists against which to reconcile inputs. Hashes use a
#'   versioned `rlang::hash()` scheme; the producing rlang version is recorded.
#' @return A versioned `pairwiseLLM_link_input` list with normalized identities,
#'   evidence, coordinates, controls, hashes, counts, and provenance. Raw Phase A
#'   likelihood counts are zero for E1/E2; source observation counts are separate.
#' @examples
#' hub <- list(set_id = "H", items = data.frame(item_id = c("h1", "h2")))
#' spoke <- list(set_id = "S", items = data.frame(item_id = c("s1", "s2")))
#' cross <- data.frame(observation_id = character(), A_set = character(),
#'   A_item = character(), B_set = character(), B_item = character(), y_A = integer())
#' input <- prepare_link_input("fixed_shape_offset", hub, spoke,
#'   phase_a = list(hub = list(points = c(h1 = -1, h2 = 1)),
#'     spoke = list(points = c(s1 = -.5, s2 = .5))), cross = cross,
#'   judge = list(beta = 0, epsilon = 0, model_variant = "btl",
#'     link = "logit", source = "frozen Phase A"))
#' input$counts
#' fit <- fit_link(input)
#' fit$offset
#' predict_link(fit, data.frame(observation_id = "held-out-1",
#'   A_set = "H", A_item = "h1", B_set = "S", B_item = "s2"))
#' @export
prepare_link_input <- function(estimator, hub, spoke, phase_a, cross, judge,
                               control = list(), provenance = list()) {
  if (missing(estimator)) .link_check(FALSE, "An explicit estimator is required.")
  backend <- .link_resolve(estimator)
  hub <- .link_identity(hub, "hub")
  spoke <- .link_identity(spoke, "spoke")
  .link_check(hub$set_id != spoke$set_id, "hub and spoke set IDs must differ.")
  globals <- c(hub$items$global_item_id, spoke$items$global_item_id)
  .link_check(!anyDuplicated(globals[!is.na(globals)]), "Global item IDs must be unique across sets.")
  .link_fields(phase_a, c("hub", "spoke"), c("hub", "spoke"), "phase_a")
  judge <- .link_judge(judge)
  if (estimator == "fixed_shape_offset") {
    phase_a$hub <- .link_e1_artifact(phase_a$hub, hub, judge)
    phase_a$spoke <- .link_e1_artifact(phase_a$spoke, spoke, judge)
  }
  phase_a <- list(hub = .link_phase_a(phase_a$hub, hub, backend$kind),
    spoke = .link_phase_a(phase_a$spoke, spoke, backend$kind))
  cross <- .link_observations(cross, hub, spoke)
  if (backend$kind == "observations") {
    ids <- c(phase_a$hub$value$observation_id, phase_a$spoke$value$observation_id, cross$observation_id)
    .link_check(!anyDuplicated(ids), "Duplicate observation IDs reuse evidence across likelihood blocks.")
  }
  basis <- list(hub = .link_basis(hub$items$item_id), spoke = .link_basis(spoke$items$item_id))
  transform <- .link_item_transform(basis, estimator)
  control <- .link_control(control, backend, colnames(transform))
  .link_fields(provenance, c("source_commit", "expected"), label = "provenance")
  commit <- provenance$source_commit %||% NA_character_
  .link_check(is.character(commit) && length(commit) == 1L &&
    (is.na(commit) || nzchar(commit)), "source_commit must be a scalar string or NA_character_.")
  counts <- list(phase_a_hub = if (backend$kind == "observations") nrow(phase_a$hub$value) else 0L,
    phase_a_spoke = if (backend$kind == "observations") nrow(phase_a$spoke$value) else 0L,
    cross = nrow(cross), source_hub = phase_a$hub$source$n_observations,
    source_spoke = phase_a$spoke$source$n_observations)
  hashes <- list(phase_a_hub = .link_hash(phase_a$hub), phase_a_spoke = .link_hash(phase_a$spoke),
    cross = .link_hash(cross), judge = .link_hash(judge),
    config = .link_hash(list(estimator = estimator, hub = hub, spoke = spoke, control = control)))
  hashes$input <- .link_hash(list(hashes = hashes, counts = counts))
  expected <- provenance$expected %||% list()
  .link_fields(expected, c("hashes", "counts"), label = "expected")
  for (kind in names(expected)) {
    actual <- if (kind == "hashes") hashes else counts
    .link_fields(expected[[kind]], names(actual), label = paste("expected", kind))
    for (k in names(expected[[kind]])) {
      .link_check(isTRUE(all.equal(expected[[kind]][[k]], actual[[k]])),
        paste0("Input ", kind, " mismatch: ", k, "."))
    }
  }
  structure(list(schema_version = 1L, estimator = estimator, hub = hub, spoke = spoke,
    phase_a = phase_a, cross = cross, judge = judge, control = control,
    basis = basis, item_transform = transform, counts = counts, hashes = hashes,
    provenance = list(package_version = as.character(utils::packageVersion("pairwiseLLM")),
      source_commit = commit, hash_scheme = "link-v1-rlang",
      hash_engine_version = as.character(utils::packageVersion("rlang")))), class = "pairwiseLLM_link_input")
}
