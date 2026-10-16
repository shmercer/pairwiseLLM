# Frozen instruction text approved in issue #333. No trailing newline.
.decision_cj_v1 <- "You are an expert writing assessor. Determine which writing sample demonstrates stronger performance on the specified writing trait.

TRAIT: {TRAIT_NAME}

TRAIT GUIDANCE:
{TRAIT_DESCRIPTION}

ASSESSMENT RULES:
- Consider the overall pattern of relevant strengths and limitations in each sample.
- Judge only {TRAIT_NAME}, without compensating for strengths or weaknesses in other writing traits.
- Do not prefer a sample merely because it is longer, takes a particular stance, or discusses more sophisticated or interesting subject matter. Consider content only when relevant to {TRAIT_NAME}.
- Evaluate both samples using the same criteria, regardless of their presentation order.
- Select the sample demonstrating stronger {TRAIT_NAME}, even if the difference is small."

.decision_string <- function(x, allow_empty = FALSE) {
  is.character(x) && length(x) == 1L && is.null(dim(x)) &&
    !is.na(x) && (allow_empty || nzchar(trimws(x)))
}

.decision_json <- function(x) {
  as.character(jsonlite::toJSON(x, auto_unbox = TRUE, digits = 17,
                              null = "null", na = "null"))
}

.decision_sha256 <- function(x) {
  paste0(openssl::sha256(charToRaw(enc2utf8(x))))
}

.decision_fingerprint <- function(provenance) {
  paste0("decision-v1-sha256-", .decision_sha256(.decision_json(provenance)))
}

.decision_render <- function(template, trait_name, trait_description) {
  matches <- gregexpr("\\{TRAIT_NAME\\}|\\{TRAIT_DESCRIPTION\\}", template)
  tokens <- regmatches(template, matches)[[1L]]
  values <- c("{TRAIT_NAME}" = trait_name,
              "{TRAIT_DESCRIPTION}" = trait_description)
  regmatches(template, matches) <- list(unname(values[tokens]))
  template
}

#' Construct an offline comparative-judgment decision request
#'
#' Internal foundation for single-trait binary decision endpoints. This helper
#' constructs a request without HTTP, credentials, retries, persistence, or model
#' availability checks. Live decision helpers validate its body before HTTP;
#' managed Batch and bulk decision execution are not supported.
#'
#' @param ID1,ID2 Distinct, nonempty character scalar sample IDs, in presentation
#'   order. IDs may recur across different pairs; each scheduled occurrence must
#'   have a distinct `pair_uid` when results are normalized.
#' @param text1,text2 Nonmissing character scalar sample texts. Empty text is
#'   allowed. Exact characters, including whitespace and line endings, are kept.
#' @param trait_name,trait_description Nonempty character scalars. The complete
#'   description, including construct boundaries, is inserted verbatim. Inserted
#'   content is never interpreted again as template syntax.
#' @param pair_uid Nonempty character scalar identifying the actual scheduled
#'   pair occurrence. It is separate from any provider request ID.
#' @param provider Either `"openai"` or `"typesafe"`.
#' @param model Requested model ID. `NULL` selects `"gpt-6-luna"` for OpenAI or
#'   pinned `"jev-1.13.0"` for TypeSafe. Explicit alternatives are recorded, not
#'   checked for account availability. Aliases do not guarantee immutable weights.
#' @param decision_template `NULL` selects the exact approved `decision_cj_v1`
#'   instructions. A custom character scalar must contain `{TRAIT_NAME}` and
#'   `{TRAIT_DESCRIPTION}` and must not embed sample placeholders or the
#'   generative `<BETTER_SAMPLE>` output tag. Custom text is fingerprinted.
#' @param prompt_template Must be `NULL`. An explicitly supplied generative
#'   template is rejected. Future decision endpoint wrappers must distinguish
#'   omitted/defaulted generative arguments (use `decision_cj_v1`) from explicitly
#'   supplied generative templates (error), and forward custom instructions using
#'   `decision_template`. Historical [set_prompt_template()] behavior is unchanged.
#'
#' @return A list with `body` (provider wire representation), `pair_uid`,
#'   `provenance`, and `request_fingerprint`. The fixed question `writing_trait`
#'   has type `choice` and exactly `SAMPLE_1`/`SAMPLE_2` options. Symmetric option
#'   descriptions differ only by sample label. Shared evidence is one JSON string
#'   with those two keys; JSON quoting gives explicit boundaries even when sample
#'   text contains labels, quotes, or newlines. OpenAI uses `/v1/decisions`, `input`
#'   and a `questions`/`choices` array; TypeSafe uses `/v1/systemone`, `state` and a
#'   `questions`/`criteria` map. Samples are never placed in instructions.
#'
#' @details
#' The built-in instruction value is UTF-8, LF-separated, without a trailing
#' newline; its SHA-256 is
#' `06fdf52ff1b9b9128f7e52189053226ad1f15f4ea4a54266ea55a78399ef22af`.
#'
#' Fingerprint scheme `decision-v1-sha256` hashes UTF-8 compact JSON, with fixed
#' field/choice order and unboxed scalars. Provenance includes contract version,
#' provider, endpoint, requested model, question/type, ordered IDs, SHA-256 hashes
#' of both exact UTF-8 texts and the evidence rendering, complete trait definition,
#' template version/text, rendered instructions, and both choice definitions.
#' No Unicode normalization, trimming, or line-ending conversion is performed.
#' Pair UID, provider request ID, response/resolved model, and timestamps are not
#' scientific fingerprint inputs. Future resumption must verify both pair UID and
#' fingerprint, and inspect resolved-model metadata when aliases are used; this
#' helper does not implement resumption or guarantee provider reproducibility.
#' A fingerprint is an identity/integrity check, not an authenticated signature.
#' Live transports must independently check the body against this provenance
#' and check the final serialized payload before every HTTP attempt.
#'
#' The body contains private sample text and must not be logged publicly.
#' Provenance contains text hashes, not sample contents, but custom trait/template
#' text can itself be sensitive. These model probabilities do not establish
#' validated educational assessment accuracy. No statistical model uses them as
#' fractional wins, weights, or additional judgments.
#'
#' @examples
#' # Entirely offline; this does not submit a request.
#' request <- pairwiseLLM:::.build_decision_request(
#'   "A", "A short synthetic sample.", "B", "Another synthetic sample.",
#'   "Organization", "Logical sequencing and connections between ideas.",
#'   pair_uid = "example-forward", provider = "openai"
#' )
#' request$request_fingerprint
#' request$body$questions[[1]]$type
#'
#' @seealso [decision_results], [set_prompt_template()]
#' @name decision_request
#' @keywords internal
.build_decision_request <- function(ID1, text1, ID2, text2,
                                    trait_name, trait_description, pair_uid,
                                    provider = c("openai", "typesafe"),
                                    model = NULL, decision_template = NULL,
                                    prompt_template = NULL) {
  provider <- match.arg(provider)
  strings <- list(ID1 = ID1, ID2 = ID2, trait_name = trait_name,
                  trait_description = trait_description, pair_uid = pair_uid)
  for (name in names(strings)) {
    if (!.decision_string(strings[[name]])) {
      rlang::abort(paste0("`", name, "` must be a nonempty character scalar."))
    }
  }
  if (identical(ID1, ID2)) rlang::abort("Decision requests must not contain self-pairs.")
  if (!.decision_string(text1, TRUE) || !.decision_string(text2, TRUE)) {
    rlang::abort("`text1` and `text2` must be nonmissing character scalars.")
  }
  if (!is.null(prompt_template)) {
    rlang::abort("Generative `prompt_template` is not supported for decisions; use `decision_template`.")
  }
  if (is.null(model)) model <- if (provider == "openai") "gpt-6-luna" else "jev-1.13.0"
  if (!.decision_string(model)) rlang::abort("`model` must be a nonempty character scalar.")
  template <- decision_template %||% .decision_cj_v1
  required <- c("{TRAIT_NAME}", "{TRAIT_DESCRIPTION}")
  if (!.decision_string(template) ||
      !all(vapply(required, grepl, logical(1L), x = template, fixed = TRUE)) ||
      grepl("\\{SAMPLE_[12]\\}|<\\s*/?\\s*BETTER_SAMPLE", template, ignore.case = TRUE)) {
    rlang::abort("`decision_template` must contain trait placeholders and must not be a generative/sample template.")
  }
  instructions <- .decision_render(template, trait_name, trait_description)
  labels <- c("SAMPLE_1", "SAMPLE_2")
  choices <- lapply(labels, function(label) {
    list(value = label, description = paste0(label, " demonstrates stronger ", trait_name, "."))
  })
  evidence <- .decision_json(list(SAMPLE_1 = text1, SAMPLE_2 = text2))
  endpoint <- if (provider == "openai") "/v1/decisions" else "/v1/systemone"
  provenance <- list(
    contract_version = "decision-contract-v1", provider = provider,
    endpoint = endpoint, requested_model = model,
    decision_question = "writing_trait", decision_type = "choice",
    ID1 = ID1, ID2 = ID2,
    text1_sha256 = .decision_sha256(text1), text2_sha256 = .decision_sha256(text2),
    evidence_sha256 = .decision_sha256(evidence),
    trait_name = trait_name, trait_description = trait_description,
    decision_template_version = if (identical(template, .decision_cj_v1)) {
      "decision_cj_v1"
    } else {
      paste0("custom-sha256-", .decision_sha256(template))
    },
    decision_template = template, decision_instructions = instructions,
    choices = choices
  )
  question <- list(type = "choice", instructions = instructions)
  if (provider == "openai") {
    question$name <- provenance$decision_question
    question$choices <- choices
    body <- list(model = model, input = evidence, questions = list(question))
  } else {
    question$criteria <- stats::setNames(lapply(choices, `[[`, "description"), labels)
    body <- list(model = model, state = evidence,
                 questions = stats::setNames(list(question), provenance$decision_question))
  }
  list(body = body, pair_uid = pair_uid, provenance = provenance,
       request_fingerprint = .decision_fingerprint(provenance))
}

.decision_request_error <- function() {
  rlang::abort(
    "Decision request integrity check failed; use an unmodified decision request.",
    class = "pairwiseLLM_decision_request_error"
  )
}

# Validate independently of the builder and its mutable output. Expected fields
# are for comparison only; the supplied scientific body is never repaired.
# Both existing provider representations share the same evidence/provenance checks.
.validate_decision_request <- function(request, body = request$body,
                                        provider = NULL, model = NULL) {
  valid <- tryCatch({
    if (!.decision_object(request) || !.decision_object(request$provenance) ||
        !.decision_string(request$pair_uid) || !.decision_object(body) ||
        !identical(request$request_fingerprint,
                   .decision_fingerprint(request$provenance))) {
      .decision_request_error()
    }
    p <- request$provenance
    if ((!is.null(provider) && !identical(p$provider, provider)) ||
        (!is.null(model) && !identical(p$requested_model, model))) {
      .decision_request_error()
    }
    fields <- c("contract_version", "provider", "endpoint", "requested_model",
                "decision_question", "decision_type", "ID1", "ID2",
                "text1_sha256", "text2_sha256", "evidence_sha256", "trait_name",
                "trait_description", "decision_template_version", "decision_template",
                "decision_instructions", "choices")
    if (!identical(names(p), fields) ||
        !all(vapply(p[setdiff(fields, "choices")], .decision_string, logical(1L))) ||
        !identical(p$contract_version, "decision-contract-v1") ||
        !p$provider %in% c("openai", "typesafe") || identical(p$ID1, p$ID2) ||
        !identical(p$decision_question, "writing_trait") || !identical(p$decision_type, "choice")) {
      .decision_request_error()
    }
    openai <- p$provider == "openai"
    endpoint <- if (openai) "/v1/decisions" else "/v1/systemone"
    field <- if (openai) "input" else "state"
    template <- p$decision_template
    required <- c("{TRAIT_NAME}", "{TRAIT_DESCRIPTION}")
    version <- if (identical(template, .decision_cj_v1)) "decision_cj_v1" else {
      paste0("custom-sha256-", .decision_sha256(template))
    }
    if (!identical(p$endpoint, endpoint) ||
        !all(vapply(required, grepl, logical(1L), x = template, fixed = TRUE)) ||
        grepl("\\{SAMPLE_[12]\\}|<\\s*/?\\s*BETTER_SAMPLE", template, ignore.case = TRUE) ||
        !identical(p$decision_template_version, version) ||
        !identical(p$decision_instructions,
                   .decision_render(template, p$trait_name, p$trait_description)) ||
        !.decision_string(body[[field]])) {
      .decision_request_error()
    }
    evidence <- jsonlite::parse_json(body[[field]], simplifyVector = FALSE)
    if (!.decision_object(evidence) ||
        !identical(names(evidence), c("SAMPLE_1", "SAMPLE_2")) ||
        !all(vapply(evidence, .decision_string, logical(1L), allow_empty = TRUE)) ||
        !identical(p$text1_sha256, .decision_sha256(evidence$SAMPLE_1)) ||
        !identical(p$text2_sha256, .decision_sha256(evidence$SAMPLE_2)) ||
        !identical(p$evidence_sha256, .decision_sha256(body[[field]]))) {
      .decision_request_error()
    }
    labels <- c("SAMPLE_1", "SAMPLE_2")
    choices <- lapply(labels, function(label) {
      list(value = label, description = paste0(label, " demonstrates stronger ", p$trait_name, "."))
    })
    question <- list(type = p$decision_type, instructions = p$decision_instructions)
    if (openai) {
      question$name <- p$decision_question
      question$choices <- p$choices
      questions <- list(question)
    } else {
      question$criteria <- stats::setNames(lapply(choices, `[[`, "description"), labels)
      questions <- stats::setNames(list(question), p$decision_question)
    }
    expected <- list(model = p$requested_model)
    expected[[field]] <- body[[field]]
    expected$questions <- questions
    identical(p$choices, choices) && identical(body, expected)
  }, error = function(e) FALSE)
  if (!isTRUE(valid)) .decision_request_error()
  invisible(NULL)
}
