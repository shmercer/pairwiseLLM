# -------------------------------------------------------------------------
# High-level adaptive workflow helpers.
# -------------------------------------------------------------------------

.adaptive_rank_merge_args <- function(base_args, override_args) {
  if (length(override_args) == 0L) {
    return(base_args)
  }
  if (is.null(names(override_args)) || any(names(override_args) == "")) {
    rlang::abort("All extra argument lists must be named.")
  }
  dup <- intersect(names(base_args), names(override_args))
  if (length(dup) > 0L) {
    base_args[dup] <- NULL
  }
  c(base_args, override_args)
}

.adaptive_rank_resolve_trait <- function(trait, trait_name, trait_description) {
  if (is.null(trait_name) && is.null(trait_description)) {
    if (!is.character(trait) || length(trait) != 1L || is.na(trait) || !nzchar(trait)) {
      rlang::abort("`trait` must be a single non-empty string when custom trait fields are not supplied.")
    }
    return(trait_description(name = trait))
  }
  if (!is.null(trait_name) && !is.null(trait_description)) {
    return(trait_description(
      custom_name = trait_name,
      custom_description = trait_description
    ))
  }
  rlang::abort(
    "Provide both `trait_name` and `trait_description` for a custom trait, or neither to use `trait`."
  )
}

.adaptive_rank_read_data <- function(data, id_col, text_col) {
  if (is.data.frame(data)) {
    return(read_samples_df(data, id_col = id_col, text_col = text_col))
  }

  if (!is.character(data) || length(data) != 1L || is.na(data) || !nzchar(data)) {
    rlang::abort("`data` must be a data frame or a single file/directory path.")
  }

  if (dir.exists(data)) {
    return(read_samples_dir(path = data))
  }

  if (!file.exists(data)) {
    rlang::abort("`data` path does not exist.")
  }

  ext <- tolower(tools::file_ext(data))
  parsed <- if (identical(ext, "csv")) {
    utils::read.csv(data, stringsAsFactors = FALSE, check.names = FALSE)
  } else if (ext %in% c("tsv", "txt")) {
    utils::read.delim(data, stringsAsFactors = FALSE, check.names = FALSE)
  } else if (identical(ext, "rds")) {
    readRDS(data)
  } else {
    rlang::abort("Unsupported file extension for `data`. Use .csv, .tsv, .txt, .rds, or a directory of .txt files.")
  }

  read_samples_df(parsed, id_col = id_col, text_col = text_col)
}

#' Build an LLM judge function for adaptive ranking
#'
#' @description
#' Creates a judge function compatible with [adaptive_rank_run_live()] by
#' wrapping [llm_compare_pair()] and converting provider responses into
#' adaptive binary outcomes (`Y` in `{0,1}`).
#'
#' @details
#' The returned function has signature `judge(A, B, state, ...)` and enforces
#' the adaptive transactional contract:
#' it returns `is_valid = TRUE` with `Y` in `{0,1}` when the model response
#' identifies one of the two presented items, and returns `is_valid = FALSE`
#' otherwise.
#'
#' Model configuration is split into:
#' \itemize{
#'   \item fixed build-time options via `judge_args`,
#'   \item per-run overrides via `judge_call_args` in [adaptive_rank()],
#'   \item optional per-step overrides via `...` passed through
#'         [adaptive_rank_run_live()].
#' }
#' Collectively this supports all `llm_compare_pair()` options, including
#' backend-specific parameters such as OpenAI `reasoning` and `service_tier`.
#'
#' @param backend Backend passed to [llm_compare_pair()].
#' @param model Model identifier passed to [llm_compare_pair()].
#' @param trait Built-in trait key used when no custom trait is supplied.
#'   Ignored when both `trait_name` and `trait_description` are supplied.
#' @param trait_name Optional custom trait display name.
#' @param trait_description Optional custom trait definition.
#' @param prompt_template Prompt template string. Defaults to
#'   [set_prompt_template()].
#' @param endpoint Endpoint family passed to [llm_compare_pair()].
#'   Only used when `backend = "openai"`; ignored otherwise.
#' @param api_key Optional API key passed to [llm_compare_pair()].
#' @param include_raw Logical; forwarded to [llm_compare_pair()].
#' @param text_col Name of the text column expected in adaptive item rows.
#' @param judge_args Named list of additional fixed arguments forwarded to
#'   [llm_compare_pair()]. Use this for provider-specific controls such as
#'   `reasoning`, `service_tier`, `temperature`, `top_p`, `logprobs`, `host`,
#'   or `include_thoughts`.
#'
#' @return A function `judge(A, B, state, ...)` returning a list with fields
#'   `is_valid`, `Y`, and `invalid_reason`.
#'
#' @examples
#' judge <- make_adaptive_judge_llm(
#'   backend = "openai",
#'   model = "gpt-5.1",
#'   endpoint = "responses",
#'   judge_args = list(
#'     reasoning = "low",
#'     service_tier = "flex",
#'     include_thoughts = FALSE
#'   )
#' )
#'
#' @seealso [adaptive_rank()], [adaptive_rank_run_live()], [llm_compare_pair()]
#'
#' @family adaptive ranking
#' @export
make_adaptive_judge_llm <- function(
    backend = c("openai", "anthropic", "gemini", "together", "ollama"),
    model,
    trait = "overall_quality",
    trait_name = NULL,
    trait_description = NULL,
    prompt_template = set_prompt_template(),
    endpoint = "chat.completions",
    api_key = NULL,
    include_raw = FALSE,
    text_col = "text",
    judge_args = list()
) {
  backend <- match.arg(backend)
  if (identical(backend, "openai")) {
    endpoint <- match.arg(endpoint, c("chat.completions", "responses"))
  } else {
    endpoint <- as.character(endpoint)[1L]
    if (is.na(endpoint) || !nzchar(endpoint)) {
      endpoint <- "chat.completions"
    }
  }

  if (!is.character(model) || length(model) != 1L || is.na(model) || !nzchar(model)) {
    rlang::abort("`model` must be a single non-empty string.")
  }
  if (!is.character(text_col) || length(text_col) != 1L || is.na(text_col) || !nzchar(text_col)) {
    rlang::abort("`text_col` must be a single non-empty string.")
  }
  if (!is.list(judge_args)) {
    rlang::abort("`judge_args` must be a named list.")
  }
  if (length(judge_args) > 0L && (is.null(names(judge_args)) || any(names(judge_args) == ""))) {
    rlang::abort("`judge_args` must be a named list.")
  }

  trait_info <- .adaptive_rank_resolve_trait(trait, trait_name, trait_description)

  function(A, B, state, ...) {
    invalid <- function(reason) {
      list(is_valid = FALSE, Y = NA_integer_, invalid_reason = reason)
    }

    if (!is.data.frame(A) || !is.data.frame(B) || nrow(A) != 1L || nrow(B) != 1L) {
      return(invalid("invalid_items"))
    }
    if (!"item_id" %in% names(A) || !"item_id" %in% names(B)) {
      return(invalid("invalid_items"))
    }
    if (!text_col %in% names(A) || !text_col %in% names(B)) {
      return(invalid("missing_text_column"))
    }

    A_id <- as.character(A$item_id[[1L]])
    B_id <- as.character(B$item_id[[1L]])
    A_text <- as.character(A[[text_col]][[1L]])
    B_text <- as.character(B[[text_col]][[1L]])

    if (is.na(A_id) || !nzchar(A_id) || is.na(B_id) || !nzchar(B_id)) {
      return(invalid("invalid_items"))
    }
    if (is.na(A_text) || is.na(B_text)) {
      return(invalid("missing_text"))
    }

    runtime_args <- list(...)
    if (length(runtime_args) > 0L && (is.null(names(runtime_args)) || any(names(runtime_args) == ""))) {
      return(invalid("invalid_runtime_args"))
    }
    merged_extra <- .adaptive_rank_merge_args(judge_args, runtime_args)

    base_args <- list(
      ID1 = A_id,
      text1 = A_text,
      ID2 = B_id,
      text2 = B_text,
      model = model,
      trait_name = trait_info$name,
      trait_description = trait_info$description,
      prompt_template = prompt_template,
      backend = backend,
      endpoint = endpoint,
      api_key = api_key,
      include_raw = include_raw
    )
    call_args <- .adaptive_rank_merge_args(base_args, merged_extra)

    res <- tryCatch(
      do.call(llm_compare_pair, call_args),
      error = function(e) {
        structure(list(error = conditionMessage(e)), class = "adaptive_judge_error")
      }
    )
    if (inherits(res, "adaptive_judge_error")) {
      return(invalid("llm_error"))
    }
    if (!is.data.frame(res) || nrow(res) < 1L || !"better_id" %in% names(res)) {
      return(invalid("invalid_response"))
    }

    better_id <- as.character(res$better_id[[1L]])
    if (is.na(better_id) || !better_id %in% c(A_id, B_id)) {
      return(invalid("invalid_response"))
    }

    list(
      is_valid = TRUE,
      Y = as.integer(identical(better_id, A_id)),
      invalid_reason = NA_character_
    )
  }
}

#' Run adaptive ranking end-to-end from data and model settings
#'
#' @description
#' High-level workflow wrapper that reads sample data, constructs an LLM judge,
#' starts or resumes adaptive state, runs [adaptive_rank_run_live()], and
#' returns state plus summary outputs.
#'
#' @details
#' This helper is designed for end users who want one entry point for adaptive
#' runs. It supports:
#' \itemize{
#'   \item data input from a data frame, file (`.csv`, `.tsv`, `.txt`, `.rds`),
#'         or a directory of `.txt` files;
#'   \item model/backend configuration through [make_adaptive_judge_llm()];
#'   \item all adaptive runtime controls exposed by [adaptive_rank_run_live()];
#'   \item resumability via `session_dir` and `resume`;
#'   \item optional saving of run outputs to an `.rds` artifact.
#' }
#'
#' Model options:
#' use `judge_args` (fixed) and `judge_call_args` (per-run overrides) to pass
#' any additional [llm_compare_pair()] arguments, including provider-specific
#' controls such as `reasoning`, `service_tier`, `temperature`, `top_p`,
#' `logprobs`, `include_thoughts`, or `host`.
#'
#' Adaptive options:
#' all key controls from [adaptive_rank_run_live()] are available directly:
#' `n_steps`, `fit_fn`, `btl_config`, `progress`, `progress_redraw_every`,
#' `progress_show_events`, `progress_errors`, `session_dir`, and
#' `persist_item_log`.
#'
#' @param data Data source: a data frame/tibble, a file path (`.csv`, `.tsv`,
#'   `.txt`, `.rds`), or a directory containing `.txt` files.
#' @param id_col ID column selector for tabular inputs. Passed to
#'   [read_samples_df()].
#' @param text_col Text column selector for tabular inputs. Passed to
#'   [read_samples_df()].
#' @param backend Backend passed to [make_adaptive_judge_llm()].
#' @param model Model passed to [make_adaptive_judge_llm()].
#' @param trait Built-in trait key used when no custom trait is supplied.
#'   Ignored when both `trait_name` and `trait_description` are supplied.
#' @param trait_name Optional custom trait display name.
#' @param trait_description Optional custom trait definition.
#' @param prompt_template Prompt template string. Defaults to
#'   [set_prompt_template()].
#' @param endpoint Endpoint family passed to [make_adaptive_judge_llm()].
#'   Only used when `backend = "openai"`; ignored otherwise.
#' @param api_key Optional API key passed to [make_adaptive_judge_llm()].
#' @param include_raw Logical; forwarded to [make_adaptive_judge_llm()].
#' @param judge_args Named list of fixed additional arguments forwarded to
#'   [llm_compare_pair()] by the generated judge.
#' @param judge_call_args Named list of additional arguments forwarded to the
#'   judge at run time through [adaptive_rank_run_live()].
#' @param n_steps Number of adaptive steps to execute.
#' @param fit_fn Optional fit override passed to [adaptive_rank_run_live()].
#' @param btl_config Optional BTL configuration list passed to
#'   [adaptive_rank_run_live()].
#' @param session_dir Optional session directory for persistence/resume.
#' @param persist_item_log Logical; write per-refit item logs when `TRUE`.
#' @param resume Logical; when `TRUE` and `session_dir` contains a valid session,
#'   resume from disk; otherwise initialize a new state.
#' @param seed Integer seed used when creating a new adaptive state.
#' @param progress Progress mode for [adaptive_rank_run_live()].
#' @param progress_redraw_every Redraw interval for progress output.
#' @param progress_show_events Logical; show step events.
#' @param progress_errors Logical; show invalid-step events.
#' @param save_outputs Logical; when `TRUE`, save returned outputs as `.rds`.
#' @param output_file Optional output `.rds` path. If `NULL` and
#'   `save_outputs = TRUE`, defaults to `file.path(session_dir, "adaptive_outputs.rds")`
#'   when `session_dir` is set, otherwise to a temporary file.
#' @param judge Optional prebuilt judge function with contract
#'   `judge(A, B, state, ...)`. If supplied, model/trait/template options are
#'   ignored and this function is used directly.
#'
#' @return A list with:
#' \describe{
#'   \item{state}{Final \code{adaptive_state}.}
#'   \item{summary}{Run-level summary from [summarize_adaptive()].}
#'   \item{refits}{Per-refit summary from [summarize_refits()].}
#'   \item{items}{Item summary from [summarize_items()].}
#'   \item{logs}{Canonical logs from [adaptive_get_logs()].}
#'   \item{output_file}{Saved output path when `save_outputs = TRUE`, otherwise
#'     `NULL`.}
#' }
#'
#' @examples
#' data("example_writing_samples", package = "pairwiseLLM")
#'
#' out <- adaptive_rank(
#'   data = example_writing_samples[1:8, c("ID", "text", "quality_score")],
#'   id_col = "ID",
#'   text_col = "text",
#'   model = "gpt-5.1",
#'   judge = function(A, B, state, ...) {
#'     y <- as.integer(A$quality_score[[1]] >= B$quality_score[[1]])
#'     list(is_valid = TRUE, Y = y, invalid_reason = NA_character_)
#'   },
#'   n_steps = 4,
#'   progress = "none"
#' )
#'
#' out$summary
#' head(out$logs$step_log)
#'
#' \dontrun{
#' # Live run with OpenAI gpt-5.1 + flex priority.
#' live <- adaptive_rank(
#'   data = example_writing_samples[1:12, c("ID", "text")],
#'   backend = "openai",
#'   model = "gpt-5.1",
#'   endpoint = "responses",
#'   judge_args = list(
#'     reasoning = "low",
#'     service_tier = "flex",
#'     include_thoughts = FALSE
#'   ),
#'   btl_config = list(
#'     refit_pairs_target = 20L,
#'     ess_bulk_min = 500,
#'     eap_reliability_min = 0.92
#'   ),
#'   n_steps = 120,
#'   session_dir = file.path(tempdir(), "adaptive-live"),
#'   persist_item_log = TRUE,
#'   resume = TRUE,
#'   progress = "all",
#'   save_outputs = TRUE
#' )
#'
#' print(live$state)
#' live$summary
#' }
#'
#' @seealso [make_adaptive_judge_llm()], [adaptive_rank_run_live()],
#'   [adaptive_rank_start()], [adaptive_rank_resume()], [llm_compare_pair()]
#'
#' @family adaptive ranking
#' @export
adaptive_rank <- function(
    data,
    id_col = 1,
    text_col = 2,
    backend = c("openai", "anthropic", "gemini", "together", "ollama"),
    model = NULL,
    trait = "overall_quality",
    trait_name = NULL,
    trait_description = NULL,
    prompt_template = set_prompt_template(),
    endpoint = "chat.completions",
    api_key = NULL,
    include_raw = FALSE,
    judge_args = list(),
    judge_call_args = list(),
    n_steps = 1L,
    fit_fn = NULL,
    btl_config = NULL,
    session_dir = NULL,
    persist_item_log = FALSE,
    resume = TRUE,
    seed = 1L,
    progress = c("all", "refits", "steps", "none"),
    progress_redraw_every = 10L,
    progress_show_events = TRUE,
    progress_errors = TRUE,
    save_outputs = FALSE,
    output_file = NULL,
    judge = NULL
) {
  backend <- match.arg(backend)
  if (identical(backend, "openai")) {
    endpoint <- match.arg(endpoint, c("chat.completions", "responses"))
  } else {
    endpoint <- as.character(endpoint)[1L]
    if (is.na(endpoint) || !nzchar(endpoint)) {
      endpoint <- "chat.completions"
    }
  }
  progress <- match.arg(progress)

  if (!is.list(judge_args) || (length(judge_args) > 0L &&
    (is.null(names(judge_args)) || any(names(judge_args) == "")))) {
    rlang::abort("`judge_args` must be a named list.")
  }
  if (!is.list(judge_call_args) || (length(judge_call_args) > 0L &&
    (is.null(names(judge_call_args)) || any(names(judge_call_args) == "")))) {
    rlang::abort("`judge_call_args` must be a named list.")
  }
  if (!is.logical(resume) || length(resume) != 1L || is.na(resume)) {
    rlang::abort("`resume` must be TRUE or FALSE.")
  }
  if (!is.logical(save_outputs) || length(save_outputs) != 1L || is.na(save_outputs)) {
    rlang::abort("`save_outputs` must be TRUE or FALSE.")
  }
  if (!is.null(output_file) &&
    (!is.character(output_file) || length(output_file) != 1L || is.na(output_file) || !nzchar(output_file))) {
    rlang::abort("`output_file` must be NULL or a single non-empty string.")
  }
  if (!is.null(judge) && !is.function(judge)) {
    rlang::abort("`judge` must be NULL or a function.")
  }
  if (is.null(judge) &&
    (!is.character(model) || length(model) != 1L || is.na(model) || !nzchar(model))) {
    rlang::abort("`model` must be a single non-empty string when `judge` is NULL.")
  }

  samples <- .adaptive_rank_read_data(data, id_col = id_col, text_col = text_col)
  items <- dplyr::rename(samples, item_id = ID)

  if (!"text" %in% names(items)) {
    rlang::abort("Input data must include a text column after normalization.")
  }

  loaded_state <- NULL
  if (isTRUE(resume) && !is.null(session_dir) && dir.exists(session_dir)) {
    loaded_state <- tryCatch(
      adaptive_rank_resume(session_dir),
      error = function(e) NULL
    )
  }

  state <- loaded_state
  if (is.null(state)) {
    state <- adaptive_rank_start(
      items = items,
      seed = seed,
      session_dir = session_dir,
      persist_item_log = persist_item_log
    )
  } else {
    loaded_ids <- as.character(state$item_ids)
    input_ids <- as.character(items$item_id)
    if (!identical(loaded_ids, input_ids)) {
      rlang::abort("Input `data` IDs do not match IDs in resumed session.")
    }
  }

  if (is.null(judge)) {
    judge <- make_adaptive_judge_llm(
      backend = backend,
      model = model,
      trait = trait,
      trait_name = trait_name,
      trait_description = trait_description,
      prompt_template = prompt_template,
      endpoint = endpoint,
      api_key = api_key,
      include_raw = include_raw,
      text_col = "text",
      judge_args = judge_args
    )
  }

  run_args <- list(
    state = state,
    judge = judge,
    n_steps = n_steps,
    fit_fn = fit_fn,
    btl_config = btl_config,
    session_dir = session_dir,
    persist_item_log = persist_item_log,
    progress = progress,
    progress_redraw_every = progress_redraw_every,
    progress_show_events = progress_show_events,
    progress_errors = progress_errors
  )
  run_args <- c(run_args, judge_call_args)
  state <- do.call(adaptive_rank_run_live, run_args)

  logs <- adaptive_get_logs(state)
  out <- list(
    state = state,
    summary = summarize_adaptive(state),
    refits = summarize_refits(list(round_log = logs$round_log)),
    items = summarize_items(list(item_log_list = logs$item_log)),
    logs = logs,
    output_file = NULL
  )

  if (isTRUE(save_outputs)) {
    target <- output_file
    if (is.null(target)) {
      target <- if (!is.null(session_dir)) {
        file.path(session_dir, "adaptive_outputs.rds")
      } else {
        tempfile("adaptive_outputs_", fileext = ".rds")
      }
    }
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    saveRDS(out, target)
    out$output_file <- target
  }

  out
}
