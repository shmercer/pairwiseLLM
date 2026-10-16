# Shared scientific preparation and response conversion for both live transports.
# Keep credentials and sample-bearing bodies in memory, outside checkpoints.
.decision_live_spec <- function(ID1, text1, ID2, text2, model, trait_name,
                                trait_description, prompt_template, provider,
                                include_raw, dots) {
  if (provider == "openai") {
    controls <- .openai_decision_controls(dots)
    if (!.decision_string(model) || !identical(model, "gpt-6-luna")) {
      rlang::abort("The Decisions endpoint currently supports only `model = \"gpt-6-luna\"`.")
    }
  } else {
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
    controls <- list()
  }
  .decision_live_flag(include_raw, "include_raw")
  request <- .build_decision_request(
    ID1, text1, ID2, text2, trait_name, trait_description,
    pair_uid = dots[["pair_uid"]] %||% .pairwiseLLM_make_custom_id(ID1, ID2),
    provider = provider, model = model,
    decision_template = dots[["decision_template"]], prompt_template = prompt_template
  )
  .validate_decision_request(request, provider = provider, model = model)
  body <- request$body
  if (!is.null(controls$prompt_cache_options)) {
    body$prompt_cache_options <- controls$prompt_cache_options
  }
  spec <- list(request = request, provider = provider, model = model,
               controls = controls, payload = charToRaw(enc2utf8(.decision_json(body))))
  .decision_live_validate(spec)
  spec
}

.decision_live_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || !is.null(dim(x)) || is.na(x)) {
    rlang::abort(paste0("`", name, "` must be TRUE or FALSE."))
  }
}

.decision_live_url <- function(provider) {
  if (provider == "openai") paste0(.openai_base_url(), "/decisions") else {
    "https://api.typesafe.ai/v1/systemone"
  }
}

.decision_live_validate <- function(spec, req = NULL) {
  .validate_decision_request(spec$request, provider = spec$provider, model = spec$model)
  wire <- .decision_read_json(rawToChar(spec$payload))
  if (spec$provider == "openai") {
    if (!identical(wire$prompt_cache_options, spec$controls$prompt_cache_options)) {
      .decision_request_error()
    }
    wire <- wire[setdiff(names(wire), "prompt_cache_options")]
  }
  .validate_decision_request(spec$request, wire, provider = spec$provider, model = spec$model)
  if (!is.null(req) &&
      (!identical(req$url, .decision_live_url(spec$provider)) ||
       (!is.null(req$method) && !identical(req$method, "POST")) ||
       !identical(req$body$type, "raw") || !identical(req$body$data, spec$payload) ||
       !identical(req$body$content_type, "application/json"))) {
    .decision_request_error()
  }
  invisible(NULL)
}

.decision_live_http <- function(spec, api_key) {
  .decision_live_validate(spec)
  req <- if (spec$provider == "openai") .openai_request("/decisions", api_key) else {
    .typesafe_request(api_key)
  }
  req <- httr2::req_body_raw(req, spec$payload, type = "application/json")
  .decision_live_validate(spec, req)
  req
}

.decision_live_body <- function(resp, provider) {
  tryCatch(
    if (provider == "openai") .openai_resp_body_json(resp, simplifyVector = FALSE) else {
      httr2::resp_body_json(resp, simplifyVector = FALSE)
    }, error = function(e) httr2::resp_body_string(resp)
  )
}

.decision_live_response <- function(resp, spec, include_raw) {
  response <- .decision_live_body(resp, spec$provider)
  out <- .parse_decision_response(
    response, spec$request, provider_request_id = httr2::resp_header(resp, "x-request-id"),
    include_raw = include_raw
  )
  out$object_type <- "decision"
  out$status_code <- httr2::resp_status(resp)
  out$error_message <- out$decision_error_detail
  out$thoughts <- out$content <- NA_character_
  out$retry_failures <- list(attr(resp, "retry_failures") %||% tibble::tibble())
  if (spec$provider == "openai") out$request_controls_json <- .decision_json(spec$controls)
  out
}
