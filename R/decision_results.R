.decision_error_codes <- c("invalid_choice", "invalid_probabilities", "invalid_confidence",
                           "malformed_decision", "decision_refusal",
                           "choice_probability_mismatch", "decision_identity_mismatch")

.decision_object <- function(x) {
  is.list(x) && !is.data.frame(x) && !is.null(names(x)) &&
    all(nzchar(names(x))) && !anyDuplicated(names(x))
}

.decision_number <- function(x) {
  is.numeric(x) && length(x) == 1L && is.null(dim(x)) && is.finite(x)
}

.decision_fail <- function(code, detail) {
  rlang::abort(detail, class = "pairwiseLLM_decision_error", error_code = code)
}

.decision_read_json <- function(x) {
  if (is.character(x)) {
    if (!.decision_string(x)) .decision_fail("malformed_decision", "Expected a JSON object.")
    # Unlike fromJSON(), parse_json() never interprets strings as files or URLs.
    x <- tryCatch(jsonlite::parse_json(x, simplifyVector = FALSE),
                  error = function(e) .decision_fail("malformed_decision", "Invalid JSON response or provenance."))
  }
  if (!.decision_object(x)) .decision_fail("malformed_decision", "Expected an object with unique field names.")
  x
}

.decision_probabilities <- function(x) {
  labels <- c("SAMPLE_1", "SAMPLE_2")
  if (!.decision_object(x) || length(x) != 2L || !setequal(names(x), labels) ||
      !all(vapply(x, .decision_number, logical(1L)))) {
    .decision_fail("invalid_probabilities", "Expected exactly two uniquely labeled finite numeric probabilities.")
  }
  stats::setNames(vapply(x[labels], as.numeric, numeric(1L)), labels)
}

.decision_validate_values <- function(choice, probabilities, confidence) {
  if (!.decision_string(choice) || !choice %in% c("SAMPLE_1", "SAMPLE_2")) {
    .decision_fail("invalid_choice", "Expected an explicitly selected SAMPLE_1 or SAMPLE_2.")
  }
  if (any(probabilities < 0 | probabilities > 1) || abs(sum(probabilities) - 1) > 1e-6) {
    .decision_fail("invalid_probabilities", "Probabilities must be in [0,1] and sum to 1 within 1e-6; no renormalization is performed.")
  }
  if (!.decision_number(confidence) || confidence < 0 || confidence > 1) {
    .decision_fail("invalid_confidence", "A finite numeric confidence in [0,1] is required.")
  }
  if (max(probabilities) - probabilities[[choice]] > 1e-8) {
    .decision_fail("choice_probability_mismatch", "The explicitly selected sample has lower probability than the alternative (tolerance 1e-8); do not automatically retry.")
  }
  invisible(NULL)
}

.decision_column_defaults <- function(n) {
  numeric_names <- c("p_sample_1", "p_sample_2", "p_chosen", "decision_confidence")
  character_names <- c(
    "probabilities_json", "decision_question", "decision_type",
    "decision_template_version", "endpoint", "request_fingerprint",
    "request_provenance_json", "requested_model", "resolved_model",
    "provider_request_id", "trait_name", "decision_error_code", "decision_error_detail"
  )
  c(stats::setNames(rep(list(rep(NA_real_, n)), length(numeric_names)), numeric_names),
    stats::setNames(rep(list(rep(NA_character_, n)), length(character_names)), character_names))
}

.decision_rows <- function(tbl) {
  decision <- rep(FALSE, nrow(tbl))
  for (name in c("decision_type", "decision_template_version", "request_fingerprint",
                 "request_provenance_json", "probabilities_json", "decision_error_code",
                 "p_sample_1", "p_sample_2", "p_chosen", "decision_confidence")) {
    if (name %in% names(tbl)) decision <- decision | !is.na(tbl[[name]])
  }
  if ("endpoint" %in% names(tbl)) {
    decision <- decision | tbl$endpoint %in% c("decisions", "systemone", "/v1/decisions", "/v1/systemone")
  }
  decision
}

#' Parse offline provider decisions and preserve binary probability evidence
#'
#' Convert one provider response to a row accepted by the shared normalizer.
#' This is an internal, pure converter, with no HTTP or automatic retries.
#'
#' @param response JSON character scalar or an unsimplified parsed JSON object
#'   (as returned by `jsonlite::fromJSON(..., simplifyVector = FALSE)`). Strings
#'   are parsed literally; filenames and URLs are never read. OpenAI requires an
#'   `answers` array containing exactly the requested named answer;
#'   TypeSafe requires an `answers` map with exactly the requested key. Unexpected
#'   nesting, duplicate keys/options, extra answers, and non-choice answers fail.
#' @param request An unmodified request from [decision_request].
#' @param provider_request_id Optional provider-assigned request ID, distinct
#'   from the scheduled pair UID. `NULL` records `NA`.
#' @param include_raw Logical scalar; retain raw successful responses when true.
#'   Failed decision responses are always retained, including mismatches.
#'
#' @return A one-row tibble retaining `ID1`, `ID2`, actual `pair_uid`/`custom_id`,
#'   `better_sample`, `better_id`, integer `winner_pos`, `model`, and usage
#'   (`prompt_tokens`, `completion_tokens`, `total_tokens`; `NA` when unavailable).
#'   `model` is the resolved model when supplied, otherwise the requested model;
#'   `requested_model` and `resolved_model` distinguish them without claiming
#'   resolution when the provider omits it. Additive scalar columns are:
#'   \describe{
#'     \item{p_sample_1, p_sample_2}{Probabilities for the presented IDs, never
#'       sorted IDs. Reversed presentations therefore reverse their ID mapping.}
#'     \item{p_chosen}{Probability for the explicitly returned choice.}
#'     \item{decision_confidence}{Independent provider confidence, never inferred
#'       from `p_chosen` or the distribution. Both current wire schemas require it.}
#'     \item{probabilities_json}{Complete binary map in SAMPLE_1/SAMPLE_2 order,
#'       preserving numeric values. For malformed containers, the original
#'       probability representation is kept when serializable.}
#'     \item{decision_question, decision_type, decision_template_version,
#'       endpoint, request_fingerprint}{Scientific request identity.}
#'     \item{request_provenance_json}{Sample-free provenance from the constructor;
#'       includes the full trait definition, instructions, choice definitions,
#'       and text hashes. It can still contain sensitive custom instruction text.}
#'     \item{provider_request_id, trait_name}{Transport identity and focal trait
#'       for diagnostics, alongside presentation IDs and ordered keys.}
#'     \item{decision_error_code, decision_error_detail}{`NA` on success, otherwise
#'       a fixed, sample-free reason. The shared normalizer moves failures to
#'       `failed_pairs` and `failed_attempts` before its generic winner filter.}
#'   }
#'
#' @details
#' Exactly two finite numeric probabilities in `[0,1]` are required, summing to 1
#' within absolute tolerance `1e-6`. Values are never renormalized, rounded,
#' clipped, complemented, or fabricated. Exact 0, 0.5, and 1 are retained.
#' Confidence must be a finite numeric scalar in `[0,1]`; missing confidence fails.
#' Historical generative rows receive `NA` decision fields in the normalizer.
#'
#' The returned choice is authoritative: no argmax substitution is made. If the
#' alternative exceeds its probability by more than `1e-8`, the row fails with
#' `choice_probability_mismatch`. Both probabilities, original selection, and
#' raw response remain available for later provider/trait/order analysis. Equal
#' probabilities retain the explicit selection. Mismatches must not be retried
#' without an explicit future policy. Other failure codes are `invalid_choice`,
#' `invalid_probabilities`, `invalid_confidence`, `malformed_decision`,
#' `decision_refusal`, and `decision_identity_mismatch` (during normalization).
#' OpenAI's explicit `type = "refusal"` is distinct from malformed output. The
#' current TypeSafe schema has no refusal variant; unknown answer types there
#' remain malformed rather than being guessed from prose.
#'
#' Only normalized successful `results` are modeling evidence; retained failed
#' choices are diagnostics. Existing BT/Bayesian/adaptive code is unchanged and
#' does not consume probabilities as weights, fractional wins, or extra outcomes.
#' Raw responses may contain sensitive provider content. Do not log them publicly.
#'
#' @examples
#' request <- pairwiseLLM:::.build_decision_request(
#'   "A", "Synthetic sample A.", "B", "Synthetic sample B.",
#'   "Organization", "Logical sequencing of ideas.", "example", "typesafe"
#' )
#' response <- list(
#'   model = "jev-1.13.0",
#'   answers = list(writing_trait = list(
#'     type = "choice", choice = "SAMPLE_2", confidence = 0.4,
#'     probabilities = list(SAMPLE_1 = 0.3, SAMPLE_2 = 0.7)
#'   ))
#' )
#' pairwiseLLM:::.parse_decision_response(response, request)$p_chosen
#'
#' @seealso [decision_request], [build_bt_data()]
#' @name decision_results
#' @keywords internal
.parse_decision_response <- function(response, request, provider_request_id = NULL,
                                     include_raw = FALSE) {
  if (!is.list(request) || !.decision_object(request$provenance) ||
      !.decision_string(request$pair_uid) ||
      !identical(request$request_fingerprint, .decision_fingerprint(request$provenance))) {
    rlang::abort("`request` must be an unmodified decision request.")
  }
  if (!is.logical(include_raw) || length(include_raw) != 1L ||
      !is.null(dim(include_raw)) || is.na(include_raw)) {
    rlang::abort("`include_raw` must be TRUE or FALSE.")
  }
  if (!is.null(provider_request_id) && !.decision_string(provider_request_id)) {
    rlang::abort("`provider_request_id` must be NULL or a nonempty character scalar.")
  }
  p <- request$provenance
  out <- c(list(ID1 = p$ID1, ID2 = p$ID2, pair_uid = request$pair_uid,
                custom_id = request$pair_uid, better_sample = NA_character_,
                better_id = NA_character_, winner_pos = NA_integer_,
                model = p$requested_model, prompt_tokens = NA_real_,
                completion_tokens = NA_real_, total_tokens = NA_real_),
           .decision_column_defaults(1L))
  for (name in c("decision_question", "decision_type", "decision_template_version",
                 "endpoint", "requested_model", "trait_name")) out[[name]] <- p[[name]]
  out$request_fingerprint <- request$request_fingerprint
  out$request_provenance_json <- .decision_json(p)
  out$provider_request_id <- provider_request_id %||% NA_character_
  failure <- tryCatch({
    parsed <- .decision_read_json(response)
    if (!is.null(parsed$error)) .decision_fail("malformed_decision", "Provider returned an error envelope.")
    if (!is.null(parsed$model)) {
      if (!.decision_string(parsed$model)) .decision_fail("malformed_decision", "Invalid resolved model.")
      out$model <- out$resolved_model <- parsed$model
    }
    usage <- parsed$usage
    if (!is.null(usage)) {
      if (!.decision_object(usage)) .decision_fail("malformed_decision", "Invalid usage object.")
      for (name in c("input_tokens", "output_tokens", "total_tokens")) {
        value <- usage[[name]]
        if (!is.null(value) && (!.decision_number(value) || value < 0 || value != floor(value))) {
          .decision_fail("malformed_decision", "Invalid token count.")
        }
      }
      out$prompt_tokens <- as.numeric(usage$input_tokens %||% NA_real_)
      out$completion_tokens <- as.numeric(usage$output_tokens %||% NA_real_)
      out$total_tokens <- as.numeric(usage$total_tokens %||% NA_real_)
    }
    answers <- parsed$answers
    if (p$provider == "openai") {
      if (!is.list(answers) || !is.null(names(answers)) || length(answers) != 1L) {
        .decision_fail("malformed_decision", "Expected exactly one answer in an array.")
      }
      answer <- answers[[1L]]
      if (!.decision_object(answer) || !identical(answer$name, p$decision_question)) {
        .decision_fail("malformed_decision", "Missing or unexpected answer name.")
      }
    } else {
      if (!.decision_object(answers) || !identical(names(answers), p$decision_question)) {
        .decision_fail("malformed_decision", "Expected exactly the requested answer key in a map.")
      }
      answer <- answers[[p$decision_question]]
    }
    if (!.decision_object(answer)) .decision_fail("malformed_decision", "Invalid answer object.")
    if (p$provider == "openai" && identical(answer$type, "refusal")) {
      .decision_fail("decision_refusal", "Provider refused the requested decision.")
    }
    if (!identical(answer$type, "choice")) .decision_fail("malformed_decision", "Expected a choice answer.")
    if (.decision_string(answer$choice)) out$better_sample <- answer$choice
    if (out$better_sample %in% c("SAMPLE_1", "SAMPLE_2")) {
      out$winner_pos <- match(out$better_sample, c("SAMPLE_1", "SAMPLE_2"))
      out$better_id <- c(p$ID1, p$ID2)[out$winner_pos]
    }
    if (.decision_number(answer$confidence)) out$decision_confidence <- as.numeric(answer$confidence)
    out$probabilities_json <- .decision_json(answer$probabilities)
    probabilities <- answer$probabilities
    if (p$provider == "openai") {
      if (!is.list(probabilities) || !is.null(names(probabilities)) ||
          !all(vapply(probabilities, function(x) {
            .decision_object(x) && .decision_string(x$value) && .decision_number(x$probability)
          }, logical(1L)))) {
        .decision_fail("invalid_probabilities", "Expected an array of value/probability objects.")
      }
      probabilities <- stats::setNames(lapply(probabilities, `[[`, "probability"),
                                       vapply(probabilities, `[[`, character(1L), "value"))
    }
    probabilities <- .decision_probabilities(probabilities)
    out$p_sample_1 <- probabilities[[1L]]
    out$p_sample_2 <- probabilities[[2L]]
    out$probabilities_json <- .decision_json(as.list(probabilities))
    if (out$better_sample %in% names(probabilities)) out$p_chosen <- probabilities[[out$better_sample]]
    .decision_validate_values(answer$choice, probabilities, answer$confidence)
    NULL
  }, pairwiseLLM_decision_error = function(e) e)
  if (!is.null(failure)) {
    out$decision_error_code <- failure$error_code
    out$decision_error_detail <- conditionMessage(failure)
  }
  out <- tibble::as_tibble(out)
  if (include_raw || !is.null(failure)) out$raw_response <- list(response)
  out
}

#' Validate decision rows before generic result alignment
#'
#' Adds typed missing decision columns to legacy rows. Decision rows require
#' unique, nonempty actual pair UIDs in both tables; unmatchable or duplicate UIDs
#' abort because assigning a failure to a particular occurrence would be ambiguous.
#' Matched decisions are checked against ordered IDs, exact text hashes, requested
#' provider/model, and their recorded provenance before probabilities are accepted.
#' Missing sample text is an identity failure. No row-order fallback is permitted.
#' Parser failures retain their classification; other inconsistent rows receive a
#' recorded decision failure. Only legacy rows retain historical alignment rules.
#'
#' @param raw Parsed result tibble, possibly mixing decision and generative rows.
#' @param pairs Scheduled pair tibble with IDs, texts and actual pair UIDs.
#' @param backend,model Expected provider and requested model character scalars.
#' @return The raw tibble with additive decision columns and classified failures.
#' @keywords internal
#' @noRd
.decision_prepare_results <- function(raw, pairs, backend, model) {
  decision <- .decision_rows(raw)
  defaults <- .decision_column_defaults(nrow(raw))
  for (name in setdiff(names(defaults), names(raw))) raw[[name]] <- defaults[[name]]
  if (!any(decision)) return(raw)
  if (!"pair_uid" %in% names(pairs) || anyNA(pairs$pair_uid) ||
      any(!nzchar(as.character(pairs$pair_uid))) || anyDuplicated(pairs$pair_uid)) {
    rlang::abort("Decision normalization requires unique nonempty scheduled `pair_uid` values.")
  }
  if (!"pair_uid" %in% names(raw) || anyNA(raw$pair_uid[decision]) ||
      anyDuplicated(raw$pair_uid[decision]) ||
      any(!raw$pair_uid[decision] %in% pairs$pair_uid)) {
    rlang::abort("Decision rows require unique actual `pair_uid` values matching scheduled pairs; provider IDs and row order cannot substitute.")
  }
  if (!"custom_id" %in% names(raw)) raw$custom_id <- NA_character_
  for (i in which(decision)) {
    pair <- pairs[match(raw$pair_uid[i], pairs$pair_uid), , drop = FALSE]
    failure <- tryCatch({
      p <- .decision_read_json(raw$request_provenance_json[i])
      endpoint <- if (identical(p$provider, "openai")) "/v1/decisions" else "/v1/systemone"
      metadata <- c("decision_question", "decision_type", "decision_template_version", "endpoint", "requested_model", "trait_name")
      if (!identical(p$contract_version, "decision-contract-v1") ||
          !identical(p$decision_type, "choice") || !identical(p$decision_question, "writing_trait") ||
          !identical(p$provider, backend) || !identical(p$endpoint, endpoint) ||
          !identical(p$requested_model, model) ||
          !identical(raw$request_fingerprint[i], .decision_fingerprint(p)) ||
          !all(vapply(metadata, function(name) identical(raw[[name]][i], p[[name]]), logical(1L))) ||
          !all(vapply(c("ID1", "ID2"), function(name) {
            identical(raw[[name]][i], as.character(pair[[name]])) &&
              identical(p[[name]], as.character(pair[[name]]))
          }, logical(1L))) ||
          !.decision_string(pair$text1, TRUE) || !.decision_string(pair$text2, TRUE) ||
          !identical(p$text1_sha256, .decision_sha256(pair$text1)) ||
          !identical(p$text2_sha256, .decision_sha256(pair$text2)) ||
          (!is.na(raw$custom_id[i]) && !identical(raw$custom_id[i], raw$pair_uid[i]))) {
        .decision_fail("decision_identity_mismatch", "Decision provenance does not match the scheduled pair, presentation order, provider, or requested model.")
      }
      # Preserve parser classifications (and original fields) on known failures.
      if (is.na(raw$decision_error_code[i])) {
        probabilities <- .decision_probabilities(.decision_read_json(raw$probabilities_json[i]))
        .decision_validate_values(raw$better_sample[i], probabilities, raw$decision_confidence[i])
        if (!identical(as.numeric(c(raw$p_sample_1[i], raw$p_sample_2[i], raw$p_chosen[i])),
                       unname(c(probabilities, probabilities[[raw$better_sample[i]]])))) {
          .decision_fail("invalid_probabilities", "Probability columns disagree with the complete distribution.")
        }
        expected_id <- c(p$ID1, p$ID2)[match(raw$better_sample[i], c("SAMPLE_1", "SAMPLE_2"))]
        if (!identical(raw$better_id[i], expected_id)) {
          .decision_fail("decision_identity_mismatch", "Selected label and sample ID disagree.")
        }
      }
      NULL
    }, pairwiseLLM_decision_error = function(e) e)
    if (!is.null(failure)) {
      raw$decision_error_code[i] <- failure$error_code
      raw$decision_error_detail[i] <- conditionMessage(failure)
    }
    raw$custom_id[i] <- raw$pair_uid[i]
  }
  raw
}
