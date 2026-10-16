#' Live TypeSafe Jev comparison for a single pair of samples
#'
#' Compare two writing samples on one trait using TypeSafe's binary Choice API,
#' `POST https://api.typesafe.ai/v1/systemone`. The default is the exact approved
#' `decision_cj_v1` instructions, with the complete trait description preserved.
#' This opt-in helper uses the same scientific request, integrity validator,
#' response parser, and binary winner semantics as OpenAI Decisions.
#'
#' @param ID1,ID2 Distinct nonempty character scalar sample IDs, in presentation
#'   order. Reversing the presentation reverses the sample-to-position mapping.
#' @param text1,text2 Nonmissing character scalar sample texts. Empty strings are
#'   allowed. Whitespace, Unicode characters, and line endings are preserved.
#' @param model Requested model identifier. Defaults to the explicit version
#'   `"jev-1.13.0"`. Other `jev-<major>.<minor>.<patch>` identifiers and the
#'   documented aliases `"jev-latest"` and `"jev-preview"` are accepted.
#'   Identifiers are checked for syntax, not availability; TypeSafe determines
#'   whether the account can use them. No model-list preflight, version allowlist,
#'   substitution, or automatic fallback is used. Prefer versioned identifiers
#'   for reproducible research: aliases can resolve differently over time.
#' @param trait_name Nonempty character scalar trait label.
#' @param trait_description Nonempty character scalar containing the full trait
#'   definition, including construct boundaries; inserted verbatim.
#' @param prompt_template Must be omitted or `NULL`. Generative templates are
#'   rejected. Use `decision_template` in `...` for custom decision instructions.
#' @param api_key Optional character scalar TypeSafe key. `NULL` or `""` uses
#'   `TYPESAFE_API_KEY` through the shared credential resolver. Explicit keys take
#'   precedence; missing, malformed, whitespace-containing, or control-character
#'   keys fail without printing their values. Persist credentials in `.Renviron`
#'   and restart R, or use `Sys.setenv(TYPESAFE_API_KEY = "YOUR_KEY_HERE")` for
#'   the current session. [check_llm_api_keys()] reports presence only.
#' @param include_raw Logical scalar. Retain successful parsed responses when
#'   `TRUE`. Failed decision responses are always retained for diagnosis.
#' @param ... Only `pair_uid` and `decision_template` are supported, and must be
#'   uniquely named. `pair_uid` identifies the scheduled pair occurrence; its
#'   default is `"LIVE_<ID1>_vs_<ID2>"`. Custom `decision_template` instructions
#'   must contain `{TRAIT_NAME}` and `{TRAIT_DESCRIPTION}`, and must not contain
#'   sample placeholders or generative output tags. Custom instructions are
#'   fingerprinted. Sampling, reasoning, token limits, caching, storage, tiers,
#'   arbitrary URLs/headers, and long-context/image controls are unsupported,
#'   including explicitly supplied `NULL` settings. Unknown settings fail before
#'   credential resolution or HTTP.
#'
#' @return A one-row tibble with the shared [decision_results] columns:
#'   \describe{
#'     \item{ID1, ID2, pair_uid, custom_id}{Presentation and scheduled pair identity.}
#'     \item{better_sample, better_id, winner_pos}{Explicit selected sample label,
#'       corresponding ID, and integer presentation position.}
#'     \item{p_sample_1, p_sample_2, p_chosen, probabilities_json}{Both provider
#'       probabilities, the selected probability, and the full binary map.}
#'     \item{decision_confidence}{Independent provider-reported confidence; never
#'       inferred from or substituted for the selected probability.}
#'     \item{requested_model, resolved_model, model}{Requested identifier,
#'       provider-reported identifier (`NA` when absent), and resolved identifier
#'       when supplied, otherwise the requested identifier. Missing resolution
#'       metadata does not establish which model weights served an alias.}
#'     \item{prompt_tokens, completion_tokens, total_tokens}{Reported input,
#'       output, and total usage; unavailable counts are `NA`, not inferred.
#'       Shared cached/write/reasoning token columns are also retained if supplied.}
#'     \item{endpoint, decision_question, decision_type,
#'       decision_template_version, request_fingerprint,
#'       request_provenance_json}{Scientific identity. Provenance includes provider,
#'       full trait/instructions/criteria, ordered IDs, and exact text hashes.}
#'     \item{provider_request_id}{Optional `x-request-id` header, otherwise `NA`.}
#'     \item{object_type, status_code, error_message}{`"decision"`, HTTP status,
#'       and sample-free decision failure detail (`NA` on success).}
#'     \item{decision_error_code, decision_error_detail}{Shared response-validation
#'       failure classification and detail; `NA` on success.}
#'     \item{thoughts, content}{`NA`; this endpoint does not generate assessment prose.}
#'     \item{retry_failures}{List-column of preceding transient attempt records.}
#'     \item{raw_response}{Optional parsed response, always present on decision
#'       failure, including malformed output and inconsistent probabilities.}
#'   }
#'
#' @details
#' The wire request contains `model`, `state` (a JSON string with exactly the
#' ordered `SAMPLE_1` and `SAMPLE_2` texts), and a `questions` map with one
#' `writing_trait` entry. Its type is `choice`, with rendered instructions and
#' symmetric `SAMPLE_1`/`SAMPLE_2` criteria descriptions. Responses use an
#' `answers` map with `choice`, a complete `probabilities` map, and `confidence`.
#'
#' The shared integrity validator checks the complete scientific body against
#' recorded provenance and its SHA-256 fingerprint before provider operations.
#' The serialized JSON is checked again, attached as raw UTF-8 bytes, and checked
#' with the URL/content type before every HTTP attempt. Altered requests fail
#' with `pairwiseLLM_decision_request_error`, without repair or HTTP. This is an
#' integrity/reproducibility guard, not an authenticated signature.
#'
#' Exactly two finite probabilities in `[0,1]`, summing to 1 within `1e-6`, and
#' finite confidence in `[0,1]` are required. Values are never renormalized,
#' clipped, rounded, complemented, or fabricated. Exact 0, 0.5, and 1 survive.
#' The explicit choice is retained; if the alternative exceeds its probability
#' by more than `1e-8`, the result fails with `choice_probability_mismatch`.
#' Missing confidence, malformed/unknown answer types, incomplete distributions,
#' and invalid usage are classified by the shared parser. TypeSafe has no
#' documented refusal variant; unknown types are malformed. These response
#' failures are not retried and are excluded by the shared normalizer before
#' modeling. Only successful normalized results are model evidence.
#'
#' HTTP 408, 429, all 5xx statuses (including 529), and timeouts receive up to
#' three total attempts with exponential delays of 0.5 and 1 second. Requests
#' retain identical validated bytes. Other transport failures and permanent
#' HTTP errors, including authentication, unavailable models, and schema errors,
#' abort. Conditions have sample-free/key-free messages and `retry_failures`;
#' HTTP conditions also carry `status_code`. Backoff uses the package's shared
#' retry helper; it does not currently interpret `Retry-After` headers.
#'
#' Only the standard text endpoint is supported: no SDK is required, and no
#' compression, long-context/image opt-in, managed Batch, bulk execution, or
#' resumption is implemented here. The provider enforces body/context limits.
#' This feature has provider-free validation, not an authorized live availability
#' check. Model probabilities do not establish validated educational assessment
#' accuracy and do not become BT/adaptive weights, fractional wins, or judgments.
#'
#' Sample text is sent to TypeSafe. Keys are omitted from diagnostics. Request
#' bodies and raw responses can contain private text; sample-free provenance
#' still includes possibly sensitive custom instructions and trait definitions.
#' Avoid logging them publicly. This helper provides no data-retention control.
#'
#' @examples
#' \dontrun{
#' # Requires TYPESAFE_API_KEY and incurs provider usage charges.
#' result <- typesafe_compare_pair_live(
#'   "A", "Synthetic sample A.", "B", "Synthetic sample B.",
#'   trait_name = "Organization",
#'   trait_description = "Logical sequencing and connections between ideas."
#' )
#' result$better_id
#' result$p_sample_1
#'
#' # The same backend through the public dispatcher:
#' llm_compare_pair(
#'   "A", "Synthetic sample A.", "B", "Synthetic sample B.",
#'   model = "jev-1.13.0", backend = "typesafe",
#'   trait_name = "Organization",
#'   trait_description = "Logical sequencing and connections between ideas."
#' )
#' }
#'
#' @seealso [llm_compare_pair()], [check_llm_api_keys()], [decision_request],
#'   [decision_results], [openai_compare_pair_live()],
#'   [TypeSafe API reference](https://docs.typesafe.ai/api),
#'   [TypeSafe models](https://docs.typesafe.ai/models)
#' @family live backends
#' @export
typesafe_compare_pair_live <- function(ID1, text1, ID2, text2,
                                       model = "jev-1.13.0",
                                       trait_name, trait_description,
                                       prompt_template = NULL, api_key = NULL,
                                       include_raw = FALSE, ...) {
  dots <- list(...)
  if (length(dots) && (is.null(names(dots)) || anyNA(names(dots)) ||
      any(!nzchar(names(dots))) || anyDuplicated(names(dots)))) {
    rlang::abort("TypeSafe arguments must be uniquely named.")
  }
  if (length(setdiff(names(dots), c("pair_uid", "decision_template")))) {
    rlang::abort("Unsupported TypeSafe settings; see `typesafe_compare_pair_live()`.")
  }
  if (!.decision_string(model) ||
      !grepl("^jev-([0-9]+\\.[0-9]+\\.[0-9]+|latest|preview)$", model)) {
    rlang::abort("`model` must be a versioned Jev ID (jev-<major>.<minor>.<patch>), 'jev-latest', or 'jev-preview'.")
  }
  if (!is.logical(include_raw) || length(include_raw) != 1L ||
      !is.null(dim(include_raw)) || is.na(include_raw)) {
    rlang::abort("`include_raw` must be TRUE or FALSE.")
  }
  request <- .build_decision_request(
    ID1, text1, ID2, text2, trait_name, trait_description,
    pair_uid = dots[["pair_uid"]] %||% .pairwiseLLM_make_custom_id(ID1, ID2),
    provider = "typesafe", model = model,
    decision_template = dots[["decision_template"]], prompt_template = prompt_template
  )
  .validate_decision_request(request, provider = "typesafe", model = model)
  payload <- charToRaw(enc2utf8(.decision_json(request$body)))
  .validate_decision_request(request, .decision_read_json(rawToChar(payload)),
                             provider = "typesafe", model = model)
  req <- .typesafe_request(api_key) |>
    httr2::req_body_raw(payload, type = "application/json")
  validate_request <- function(req) {
    .validate_decision_request(request, provider = "typesafe", model = model)
    if (!identical(req$url, "https://api.typesafe.ai/v1/systemone") ||
        !identical(req$body$type, "raw") || !identical(req$body$data, payload) ||
        !identical(req$body$content_type, "application/json")) {
      .decision_request_error()
    }
    invisible(NULL)
  }
  validate_request(req)
  resp <- .typesafe_req_perform(req, validate_request)
  response <- tryCatch(httr2::resp_body_json(resp, simplifyVector = FALSE),
                        error = function(e) httr2::resp_body_string(resp))
  out <- .parse_decision_response(
    response, request, provider_request_id = httr2::resp_header(resp, "x-request-id"),
    include_raw = include_raw
  )
  out$object_type <- "decision"
  out$status_code <- httr2::resp_status(resp)
  out$error_message <- out$decision_error_detail
  out$thoughts <- out$content <- NA_character_
  out$retry_failures <- list(attr(resp, "retry_failures") %||% tibble::tibble())
  out
}

.typesafe_request <- function(api_key) {
  key <- .typesafe_api_key(api_key)
  httr2::request("https://api.typesafe.ai/v1/systemone") |>
    httr2::req_auth_bearer_token(key) |>
    # Handle HTTP errors after shared retries, without echoing provider bodies.
    httr2::req_error(is_error = function(resp) FALSE)
}

.typesafe_req_perform <- function(req, validate_request) {
  resp <- tryCatch(
    .retry_httr2_request(req, validate_request = validate_request,
                         transient_status = c(408L, 429L, 500:599)),
    error = function(e) {
      if (inherits(e, "pairwiseLLM_decision_request_error")) stop(e)
      failures <- attr(e, "retry_failures") %||% tibble::tibble()
      if (nrow(failures)) {
        failures$error_detail <- ifelse(failures$error_code == "timeout",
                                         "TypeSafe request timed out.", "TypeSafe transport failed.")
      }
      error <- rlang::error_cnd(
        class = c("pairwiseLLM_typesafe_transport_error", "pairwiseLLM_retry_error"),
        message = "TypeSafe transport failed; check connectivity and retry explicitly.",
        retry_failures = failures
      )
      attr(error, "retry_failures") <- failures
      rlang::cnd_signal(error)
    }
  )
  status <- httr2::resp_status(resp)
  if (status >= 400L) {
    guidance <- if (status %in% c(401L, 403L)) {
      "Check TYPESAFE_API_KEY and account permissions."
    } else if (status %in% c(400L, 404L, 422L)) {
      "Check the requested Jev model's availability and the request schema/context limits."
    } else {
      "Check TypeSafe service status and request limits before retrying explicitly."
    }
    failures <- dplyr::bind_rows(attr(resp, "retry_failures"), tibble::tibble(
      error_code = "http_error", error_detail = paste0("HTTP ", status), attempted_at = Sys.time()
    ))
    error <- rlang::error_cnd(
      class = "pairwiseLLM_typesafe_http_error",
      message = paste0("TypeSafe HTTP ", status, ". ", guidance),
      status_code = status, retry_failures = failures
    )
    attr(error, "retry_failures") <- failures
    rlang::cnd_signal(error)
  }
  resp
}
