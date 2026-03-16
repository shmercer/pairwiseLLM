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

.adaptive_rank_validate_linking_config <- function(items, adaptive_config) {
  if (is.null(adaptive_config)) {
    return(invisible(NULL))
  }
  if (!is.list(adaptive_config)) {
    rlang::abort("`adaptive_config` must be NULL or a named list.")
  }

  run_mode <- as.character(adaptive_config$run_mode %||% "within_set")
  phase_a_mode <- as.character(adaptive_config$phase_a_mode %||% "run")
  valid_run_modes <- c("within_set", "link_one_spoke", "link_multi_spoke")
  valid_phase_a_modes <- c("run", "import", "mixed")
  if (!run_mode %in% valid_run_modes) {
    rlang::abort("`adaptive_config$run_mode` must be within_set, link_one_spoke, or link_multi_spoke.")
  }
  if (!phase_a_mode %in% valid_phase_a_modes) {
    rlang::abort("`adaptive_config$phase_a_mode` must be run, import, or mixed.")
  }
  if (identical(run_mode, "within_set") && !identical(phase_a_mode, "run")) {
    rlang::abort("`adaptive_config$phase_a_mode` can only be import/mixed when linking run_mode is enabled.")
  }

  set_ids <- if ("set_id" %in% names(items)) {
    suppressWarnings(as.integer(items$set_id))
  } else {
    rep.int(1L, nrow(items))
  }
  set_ids <- unique(set_ids[is.finite(set_ids)])
  if (run_mode %in% c("link_one_spoke", "link_multi_spoke")) {
    if (length(set_ids) < 2L) {
      rlang::abort(
        paste0(
          "Linking run modes require multi-set input. ",
          "Provide `set_id` with at least two unique sets in `data`."
        )
      )
    }
    hub_id <- as.integer(adaptive_config$hub_id %||% 1L)
    if (!hub_id %in% set_ids) {
      rlang::abort("`adaptive_config$hub_id` must match one observed `set_id` in `data`.")
    }
    spoke_ids <- setdiff(set_ids, hub_id)
    if (identical(run_mode, "link_one_spoke") && length(spoke_ids) != 1L) {
      rlang::abort("`adaptive_config$run_mode = \"link_one_spoke\"` requires exactly one spoke set.")
    }
  }

  invisible(NULL)
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
#' @param backend Backend passed to [llm_compare_pair()]. Choices are
#'   `"openai"`, `"anthropic"`, `"gemini"`, `"together"`, and `"ollama"`.
#'   Default is `"openai"`.
#' @param model Model identifier passed to [llm_compare_pair()]. Required.
#' @param trait Built-in trait key used when no custom trait is supplied.
#'   Ignored when both `trait_name` and `trait_description` are supplied.
#'   Default is `"overall_quality"`.
#' @param trait_name Optional custom trait display name.
#' @param trait_description Optional custom trait definition.
#' @param prompt_template Prompt template string. Defaults to
#'   [set_prompt_template()].
#' @param endpoint Endpoint family passed to [llm_compare_pair()].
#'   Only used when `backend = "openai"`; choices are `"chat.completions"` and
#'   `"responses"`. Default is `"chat.completions"`. Ignored for other
#'   backends.
#' @param api_key Optional API key passed to [llm_compare_pair()].
#' @param include_raw Logical; forwarded to [llm_compare_pair()]. Default is
#'   `FALSE`.
#' @param text_col Name of the text column expected in adaptive item rows.
#'   Default is `"text"`.
#' @param judge_args Named list of additional fixed arguments forwarded to
#'   [llm_compare_pair()]. Use this for provider-specific controls such as
#'   `reasoning`, `service_tier`, `temperature`, `top_p`, `logprobs`, `host`,
#'   or `include_thoughts`. Default is `list()`.
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
#' `n_steps`, `fit_fn`, `adaptive_config`, `btl_config`, `progress`,
#' `progress_redraw_every`, `progress_show_events`, `progress_errors`,
#' `session_dir`, and `persist_item_log`.
#' Use `adaptive_config` for identifiability-gated controller behavior and
#' `btl_config` for inference/diagnostics cadence only.
#'
#' Linking run modes:
#' `run_mode = "within_set"` is the single-set workflow.
#' `run_mode = "link_one_spoke"` and `run_mode = "link_multi_spoke"` require
#' multi-set input (`set_id`/`global_item_id`), enforce hub↔spoke routing
#' defaults, and preserve Phase A artifact gating before Phase B cross-set
#' comparisons begin. `link_estimation_mode = "transform"` remains the default
#' wrapper behavior; use `link_estimation_mode = "anchored_joint"` together
#' with `hub_lock_mode = "hard_lock"` for the alternative anchored-joint
#' Phase B fit.
#'
#' Selection semantics:
#' pair selection is TrueSkill-driven in one-pair transactional steps.
#' Rolling anchors are refreshed from current score proxies and anchor-link
#' routing compares exactly one anchor endpoint with one non-anchor endpoint.
#' Long/mid-link routing excludes anchor-anchor and anchor-non-anchor pairs,
#' while local-link routing admits same-stratum pairs and anchor-involving
#' pairs according to stage bounds.
#'
#' Wrapper-visible defaults include top-band refinement
#' (`top_band_pct = 0.10`, `top_band_bins = 5`) with top-band size computed as
#' `ceiling(top_band_pct * N)`.
#'
#' Exposure and repeat routing:
#' under-represented routing is degree-based (`deg <= D_min + 1`), while
#' repeat-pressure gating is based on recent exposure (bottom-quantile
#' `recent_deg` with quantile default `0.25`) and per-endpoint repeat slot
#' accounting.
#'
#' Inference separation:
#' BTL refits are used for posterior inference, diagnostics, stop logic, and
#' the long-link posterior gate after an accepted refit is available.
#' They are not used to choose the next pair.
#'
#' Resume behavior:
#' when `resume = TRUE` and `session_dir` already contains adaptive artifacts,
#' failed session loads abort with an actionable error instead of starting a
#' fresh run silently.
#'
#' @param data Data source: a data frame/tibble, a file path (`.csv`, `.tsv`,
#'   `.txt`, `.rds`), or a directory containing `.txt` files.
#' @param id_col ID column selector for tabular inputs. Passed to
#'   [read_samples_df()]. Default is `1`.
#' @param text_col Text column selector for tabular inputs. Passed to
#'   [read_samples_df()]. Default is `2`.
#' @param backend Backend passed to [make_adaptive_judge_llm()]. Choices are
#'   `"openai"`, `"anthropic"`, `"gemini"`, `"together"`, and `"ollama"`.
#'   Default is `"openai"`.
#' @param model Model passed to [make_adaptive_judge_llm()]. Required when
#'   `judge` is `NULL`. Default is `NULL`.
#' @param trait Built-in trait key used when no custom trait is supplied.
#'   Ignored when both `trait_name` and `trait_description` are supplied.
#'   Default is `"overall_quality"`.
#' @param trait_name Optional custom trait display name.
#' @param trait_description Optional custom trait definition.
#' @param prompt_template Prompt template string. Defaults to
#'   [set_prompt_template()].
#' @param endpoint Endpoint family passed to [make_adaptive_judge_llm()].
#'   Only used when `backend = "openai"`; choices are `"chat.completions"` and
#'   `"responses"`. Default is `"chat.completions"`. Ignored for other
#'   backends.
#' @param api_key Optional API key passed to [make_adaptive_judge_llm()].
#'   Default is `NULL`.
#' @param include_raw Logical; forwarded to [make_adaptive_judge_llm()].
#'   Default is `FALSE`.
#' @param judge_args Named list of fixed additional arguments forwarded to
#'   [llm_compare_pair()] by the generated judge. Default is `list()`.
#' @param judge_call_args Named list of additional arguments forwarded to the
#'   judge at run time through [adaptive_rank_run_live()]. Default is `list()`.
#' @param n_steps Maximum number of attempted adaptive steps to execute in this
#'   call. The run may return earlier due to candidate starvation or BTL stop
#'   criteria. Attempted invalid steps also count toward this limit.
#' @param fit_fn Optional fit override passed to [adaptive_rank_run_live()].
#' @param adaptive_config Optional named list of adaptive controller overrides.
#'   Unknown fields and invalid values abort with actionable errors.
#'
#'   Supported keys (with defaults) include:
#'   \describe{
#'   \item{`global_identified_reliability_min`}{Global EAP reliability threshold
#'     used to mark the run as globally identified after a refit. Default is
#'     `0.80`.}
#'   \item{`global_identified_rank_corr_min`}{Minimum Spearman correlation
#'     between the TrueSkill rank proxy and the BTL posterior mean ranks used
#'     to mark the run as globally identified after a refit. Default is `0.90`.}
#'   \item{`p_long_low`}{Lower bound for long-link posterior win probability
#'     gating after global identifiability when an accepted posterior refit is
#'     available. Before posterior availability, the gate falls back
#'     deterministically to TrueSkill. Default is `0.10`.}
#'   \item{`p_long_high`}{Upper bound for long-link posterior win probability
#'     gating after global identifiability when an accepted posterior refit is
#'     available. Before posterior availability, the gate falls back
#'     deterministically to TrueSkill. Default is `0.90`.}
#'   \item{`long_taper_mult`}{Multiplier controlling long-link quota tapering
#'     after global identifiability. Default is `0.25`.}
#'   \item{`long_frac_floor`}{Floor fraction for long-link quota after tapering.
#'     Default is `0.02`.}
#'   \item{`mid_bonus_frac`}{Fraction of tapered long-link quota reallocated to
#'     mid-links. Default is `0.20`.}
#'   \item{`explore_taper_mult`}{Multiplier controlling exploration tapering
#'     after global identifiability. Default is `0.50`.}
#'   \item{`boundary_k`}{Top/bottom band size used by boundary-priority routing
#'     after global identifiability. Default is `20L`.}
#'   \item{`boundary_window`}{Lookback window (steps) used by boundary-priority
#'     routing after global identifiability. Default is
#'     `max(10L, ceiling(0.05 * N))` where `N` is the number of items.}
#'   \item{`boundary_frac`}{Fraction of local-stage steps eligible for
#'     boundary-priority routing after global identifiability. Default is `0.15`.}
#'   \item{`p_star_override_margin`}{Near-tie probability margin for star-cap
#'     override consideration. Default is `0.05`.}
#'   \item{`star_override_budget_per_round`}{Per-round budget of star-cap
#'     overrides allowed by the near-tie rule. Default is `1L`.}
#'
#'   \item{`run_mode`}{Run mode. Choices are `"within_set"` (single-set),
#'     `"link_one_spoke"` (hub + one spoke), and `"link_multi_spoke"` (hub +
#'     multiple spokes). Default is `"within_set"`. Linking modes require
#'     multi-set inputs with `set_id` and `global_item_id` in `data`.}
#'   \item{`hub_id`}{Hub `set_id` for linking modes. Default is `1L`.}
#'   \item{`link_estimation_mode`}{Phase B estimation family. Choices are
#'     `"transform"` (default) and `"anchored_joint"`. `"transform"` preserves
#'     the existing shift/shift-scale linking workflow. `"anchored_joint"` uses
#'     a hub-fixed, spoke-free full-evidence Phase B fit, requires
#'     `hub_lock_mode = "hard_lock"`, and does not accept transform-only config
#'     fields.}
#'   \item{`link_transform_policy`}{Only used when
#'     `link_estimation_mode = "transform"`. Allowed spoke transform family.
#'     Choices are `"auto"` (start shift-only then possibly escalate),
#'     `"fixed_shift_only"` (offset only), and `"fixed_shift_scale"` (offset +
#'     scale). Default is `"auto"`. Earlier `link_transform_mode` values are
#'     accepted for compatibility and normalized internally.}
#'   \item{`link_refit_mode`}{Only used when
#'     `link_estimation_mode = "transform"`. Linking refit mode. Choices are
#'     `"shift_only"` (fit transform with within-set abilities treated as fixed
#'     inputs) and `"joint_refit"` (jointly estimate hub/spoke abilities and
#'     transform parameters). Default is `"shift_only"`.}
#'   \item{`shift_only_theta_treatment`}{Only used when
#'     `link_refit_mode = "shift_only"`. Choices are
#'     `"fixed_eap_plugin_var"` (treat Phase A means with artifact SD plug-in
#'     variance when available) and `"fixed_eap"` (fallback when artifact SDs are
#'     unavailable). Default is `"fixed_eap_plugin_var"`.}
#'   \item{`judge_param_mode`}{How judge-noise parameters are handled across
#'     phases. Choices are `"global_shared"` (single shared judge parameter set)
#'     and `"phase_specific"` (separate within-set and link-phase judge
#'     parameters). Default is `"global_shared"`.}
#'   \item{`within_phase_b_within_set_steps_allowed`}{Linking-spec maintenance
#'     toggle for scheduling within-set comparisons after a set enters Phase B.
#'     The public field is accepted for config parity, but the current runtime
#'     does not implement that maintenance path and aborts explicitly if Phase B
#'     would begin with this set to `TRUE`. Default is `FALSE`.}
#'   \item{`hub_lock_mode`}{Controls hub behavior in Phase B fits. In
#'     `link_estimation_mode = "transform"`, this is only used when
#'     `link_refit_mode = "joint_refit"` and chooses between `"hard_lock"`
#'     (hub anchored to Phase A) and `"soft_lock"` (regularize toward Phase A).
#'     In `link_estimation_mode = "anchored_joint"`, the only supported value is
#'     `"hard_lock"`. Default is `"soft_lock"`.}
#'   \item{`hub_lock_kappa`}{Only used when `hub_lock_mode = "soft_lock"`.
#'     Regularization strength in `[0,1]`. Default is `0.75`.}
#'   \item{`anchored_joint_spoke_prior_scale`}{Scale multiplier for anchored-
#'     joint spoke priors. Default is `1.0`.}
#'   \item{`anchored_joint_sd_floor`}{Lower bound applied to anchored-joint
#'     spoke prior SDs derived from Phase A artifacts. Default is `0.02`.}
#'   \item{`anchored_joint_spoke_prior_fallback_sd`}{Fallback anchored-joint
#'     spoke prior SD used when artifact-level SDs are unavailable. Default is
#'     `1.0`.}
#'
#'   \item{`link_identified_reliability_min`}{Minimum
#'     `reliability_link_global` value on the linking-active item domain used
#'     to mark a spoke as identified. Default is `0.80`.}
#'   \item{`link_stop_reliability_min`}{Minimum `reliability_link_global` value
#'     on the linking-active item domain used to permit linking stop. Default
#'     is `0.90`.}
#'   \item{`link_rank_corr_min`}{Minimum Spearman rank correlation between
#'     TrueSkill and transformed BTL posterior mean ranks on the linking-active
#'     item domain. Default is `0.90`.}
#'   \item{`delta_sd_max`}{Maximum allowed posterior SD of the shift parameter
#'     \eqn{\\delta_s}, expressed as a multiplier of `SD(theta_hub_eap)` computed
#'     from the current hub posterior mean. Default is `0.10`.}
#'   \item{`delta_change_max`}{Maximum allowed absolute change in \eqn{\\delta_s}
#'     over the lag window used for linking stability. Default is `0.05`.}
#'   \item{`log_alpha_sd_max`}{Only used for `"shift_scale"` spokes. Maximum
#'     allowed posterior SD of `log(alpha_s)`. Default is `0.10`.}
#'   \item{`log_alpha_change_max`}{Only used for `"shift_scale"` spokes. Maximum
#'     allowed absolute change in `log(alpha_s)` over the lag window used for
#'     linking stability. Default is `0.05`.}
#'   \item{`link_transform_escalation_window_refits`}{Only used when
#'     `link_transform_policy = "auto"`. Number of eligible refits retained in
#'     the rolling escalation window. Default is `3L`.}
#'   \item{`link_transform_escalation_passes_required`}{Only used when
#'     `link_transform_policy = "auto"`. Minimum number of passing eligible
#'     refits required within the rolling escalation window. Default is `2L`.}
#'   \item{`link_transform_escalation_refits_required`}{Backward-compatible
#'     alias for older persisted/configured states. When supplied without the
#'     new rolling-window fields, it seeds both escalation window parameters.}
#'   \item{`link_transform_escalation_is_one_way`}{Only used when
#'     `link_transform_policy = "auto"`. When `TRUE`, escalation is one-way
#'     (shift-only can become shift+scale but not revert). Default is `TRUE`.}
#'
#'   \item{`max_pairs_after_stop`}{Stop-boundary budget: when `0L`, the run stops
#'     immediately after the first refit with `stop_decision = TRUE`. Values
#'     `> 0L` allow that many additional committed comparisons after the first
#'     stop boundary before deterministic termination. Default is `0L`.}
#'
#'   \item{`probe_panel_edges`}{Optional explicit planned held-out probe target
#'     per spoke. When omitted, the normative default formula is used:
#'     `clamp(40, 160, ceiling(0.25 * N_spoke_phase_b_start))`. When supplied,
#'     the value must be a positive integer and becomes the canonical planned
#'     target recorded in Phase B logs.}
#'   \item{`probe_pairs_per_refit_per_spoke`}{Base held-out probe collection cap
#'     per spoke per refit window while the spoke remains active in Phase B.
#'     The controller may exceed this only through documented probe
#'     acceleration/reallocation rules used to satisfy the held-out minimum.
#'     Under the current normative design, no additional cross-set work is
#'     scheduled after a spoke freezes. Default is `2L`.}
#'   \item{`probe_edges_min_for_stop`}{Minimum realized held-out probe edges
#'     required before Phase B stop or escalation can be evaluated. Default is
#'     `30L`.}
#'   \item{`probe_brier_delta_min`}{Minimum held-out probe Brier improvement
#'     required for auto escalation from shift-only to shift-scale. Default is
#'     `0.005`.}
#'   \item{`probe_brier_max`}{Maximum held-out probe Brier score allowed by the
#'     Phase B stop gate. Default is `0.19`.}
#'   \item{`probe_pred_rmse_max`}{Maximum lagged held-out probe prediction RMSE
#'     allowed by the Phase B stop gate. Default is `0.015`.}
#'   \item{`theta_global_rmse_max`}{Maximum lagged transformed-score RMSE on the
#'     configured spoke scope allowed by the Phase B stop gate. Default is
#'     `0.05`.}
#'   \item{`theta_global_rmse_scope`}{Scope used for transformed-score lagged
#'     RMSE. Choices are `"direct_evidence_spoke"` (default),
#'     `"all_spoke_items"`, and `"min_cross_set_edges_k"`.}
#'   \item{`min_cross_set_edges_k`}{Only used when
#'     `theta_global_rmse_scope = "min_cross_set_edges_k"`. Minimum number of
#'     committed cross-set edges per spoke item required to enter the RMSE
#'     scope. Default is `1L`.}
#'   \item{`stability_window_refits`}{Number of eligible refits retained in the
#'     rolling stop window. Default is `3L`.}
#'   \item{`stability_passes_required`}{Minimum number of passing eligible
#'     refits required within the rolling stop window. Default is `2L`.}
#'   \item{`stability_consecutive_k`}{Backward-compatible alias for older
#'     persisted/configured states. When supplied without the new rolling-window
#'     fields, it seeds both stop-window parameters.}
#'   \item{`min_refits_in_phase_b`}{Minimum refit index within Phase B before
#'     linking stop can be evaluated. Default is `3L`.}
#'   \item{`hub_theta_rmse_max`}{Maximum lagged hub-theta RMSE allowed for
#'     `hub_lock_mode = "soft_lock"` to count as anchored. Default is `0.02`.}
#'   \item{`logalpha_sd_guardrail`}{Maximum temporary alternative-fit
#'     `sd(log(alpha_s))` allowed for auto escalation. Default is `0.10`.}
#'   \item{`shift_scale_min_cross_set_edges`}{Minimum realized linking-active
#'     non-probe cross-set edges within the current epoch required before a
#'     shift-scale alternative may be considered. Default is `18L`.}
#'   \item{`shift_scale_min_distinct_spoke_items_per_bin`}{Minimum number of
#'     distinct spoke items with realized linking-active exposure required in
#'     each spoke quantile bin before a shift-scale alternative may be
#'     considered. Default is `2L`.}
#'   \item{`reliability_var_mu_epsilon`}{Degeneracy guard for the active-domain
#'     variance of posterior transformed-score means used in linking
#'     reliability. Default is `1e-6`.}
#'   \item{`reliability_total_var_epsilon`}{Degeneracy guard for the total
#'     active-domain transformed-score variance used in linking reliability.
#'     Default is `1e-6`.}
#'   \item{`hub_anchor_required_phase_b`}{Controls the normative `HubEligible`
#'     domain used for Phase B held-out probe construction. When `TRUE`
#'     (default), planned probes are drawn from the hub anchor pool; when
#'     `FALSE`, they are drawn from the full hub set.}
#'   \item{`spoke_quantile_coverage_bins`}{Cross-set coverage control: number of
#'     quantile bins used to ensure spoke items across the score distribution
#'     receive cross-set exposure within each refit window. Default is `3L`.}
#'   \item{`spoke_quantile_coverage_min_per_bin_per_refit`}{Cross-set coverage
#'     control: minimum cross-set comparisons per quantile bin per refit
#'     window. Default is `1L`.}
#'   \item{`allow_spoke_spoke_cross_set`}{When `TRUE`, allow spoke↔spoke
#'     cross-set comparisons. Default is `FALSE` (hub↔spoke only).}
#'   \item{`multi_spoke_mode`}{Only used when `run_mode = "link_multi_spoke"`.
#'     Choices are `"independent"` (fit each spoke separately) and
#'     `"concurrent"` (enforce per-refit spoke budgets and stronger hub locking
#'     requirements). Default is `"concurrent"`.}
#'   \item{`min_cross_set_pairs_per_spoke_per_refit`}{Only used in concurrent
#'     multi-spoke linking. Minimum cross-set committed comparisons per spoke
#'     per refit window. Default is `5L`.}
#'   \item{`cross_set_utility`}{Cross-set selection utility. Currently only
#'     `"linking_d_optimal"` is supported. Default is `"linking_d_optimal"`.}
#'
#'   \item{`phase_a_mode`}{Phase A handling for linking modes. Choices are
#'     `"run"` (compute within-set Phase A artifacts in-run), `"import"`
#'     (require user-supplied artifacts), and `"mixed"` (import where provided,
#'     otherwise run). Default is `"run"`.}
#'   \item{`phase_a_import_failure_policy`}{Only used when any set is configured
#'     for Phase A import. Choices are `"fail_fast"` (abort on invalid/missing
#'     artifacts) and `"fallback_to_run"` (switch that set to Phase A run if the
#'     import fails validation). Default is `"fail_fast"`.}
#'   \item{`phase_a_required_reliability_min`}{Minimum within-set EAP reliability
#'     required for Phase A artifacts to be considered ready (unless an imported
#'     artifact explicitly marks `quality_gate_accepted = TRUE`). Default is
#'     `0.80`.}
#'   \item{`phase_a_compatible_model_ids`}{Character vector of allowed model
#'     identifiers for imported Phase A artifacts (e.g., `"btl_e_b"`). Default is
#'     `"btl_e_b"`.}
#'   \item{`phase_a_compatible_config_hashes`}{Character vector of additional
#'     accepted Phase A config hashes for imported artifacts. Default is
#'     `character()`.}
#'   \item{`phase_a_artifacts`}{Named list mapping `set_id` to an imported Phase A
#'     artifact (list) or a `.rds` path containing one. Default is `list()`.}
#'   \item{`phase_a_set_source`}{Optional named character vector mapping `set_id`
#'     to `"run"` or `"import"` to force the source for specific sets. Default is
#'     `character()`.}
#'   }
#'
#'   Wrapper preflight validates linking mode combinations against supplied data
#'   and aborts early for incompatible `run_mode`/set structure combinations.
#' @param btl_config Optional named list passed to [adaptive_rank_run_live()]
#'   to control BTL refit cadence, stopping diagnostics, and selected
#'   round-log diagnostics. Supported fields:
#'   \describe{
#'   \item{`refit_pairs_target`}{Minimum new committed comparisons required
#'     before the next BTL refit. Default is `ceiling(N / 2)` clamped to
#'     `[20L, 5000L]`. In linking Phase A, `N` is the active Phase A set size.}
#'   \item{`model_variant`}{BTL likelihood variant used for inference only.
#'     Choices are `"btl"` (no lapse, no position bias), `"btl_e"` (lapse),
#'     `"btl_b"` (position bias), and `"btl_e_b"` (lapse + position bias).
#'     Default is `"btl_e_b"`.}
#'   \item{`ess_bulk_min`}{Minimum bulk effective sample size required for
#'     diagnostics to pass. Default is `max(400, round(20 * sqrt(N)))`.}
#'   \item{`ess_bulk_min_near_stop`}{Stricter bulk ESS requirement used when a
#'     run is close to stopping. Default is `max(1000, round(50 * sqrt(N)))`.}
#'   \item{`max_rhat`}{Maximum allowed split-\eqn{\\hat{R}}. Default is `1.01`.}
#'   \item{`divergences_max`}{Maximum allowed divergent transitions. Default is
#'     `0L`.}
#'   \item{`eap_reliability_min`}{Minimum EAP reliability required to permit
#'     stopping. Default is `0.90`.}
#'   \item{`stability_lag`}{Lag (in refits) used for stability checks. Default
#'     is `2L`.}
#'   \item{`theta_corr_min`}{Minimum lagged correlation of posterior means
#'     required by stability checks. Default is `0.95`.}
#'   \item{`theta_sd_rel_change_max`}{Maximum relative change in posterior SD
#'     allowed by stability checks. Default is `0.10`.}
#'   \item{`rank_spearman_min`}{Minimum lagged Spearman rank correlation
#'     required by stability checks. Default is `0.95`.}
#'   \item{`near_tie_p_low`}{Lower bound of the near-tie probability band used
#'     for round logging only. Default is `0.40`.}
#'   \item{`near_tie_p_high`}{Upper bound of the near-tie probability band used
#'     for round logging only. Default is `0.60`.}
#'   }
#'   Defaults depend on the current item count `N` and are merged with user
#'   overrides.
#' @param session_dir Optional session directory for persistence/resume.
#'   Default is `NULL`.
#' @param persist_item_log Logical; write per-refit item logs when `TRUE`.
#'   Default is `FALSE`.
#' @param resume Logical; when `TRUE` and `session_dir` contains a valid session,
#'   resume from disk; otherwise initialize a new state.
#'   Default is `TRUE`.
#' @param seed Integer seed used when creating a new adaptive state. Default is
#'   `1L`.
#' @param progress Progress mode for [adaptive_rank_run_live()]. Choices are
#'   `"all"`, `"refits"`, `"steps"`, and `"none"`. Default is `"all"`.
#' @param progress_redraw_every Redraw interval for progress output. Default is
#'   `10L`.
#' @param progress_show_events Logical; show step events. Default is `TRUE`.
#' @param progress_errors Logical; show invalid-step events. Default is `TRUE`.
#' @param save_outputs Logical; when `TRUE`, save returned outputs as `.rds`.
#'   Default is `FALSE`.
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
#'   \item{items}{Item summary from [summarize_items()], sorted by a usable
#'     canonical rank column (`rank_link` for linking runs when available,
#'     otherwise `rank_raw`).}
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
#'   adaptive_config = list(
#'     explore_taper_mult = 0.40,
#'     star_override_budget_per_round = 2L
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
#'
#' # Wrapper-driven linking workflow (hub + one spoke).
#' linking_samples <- example_writing_samples[1:12, c("ID", "text")]
#' linking_samples$set_id <- rep(c(1L, 2L), each = 6L)
#' linking_samples$global_item_id <- paste0("g_", linking_samples$ID)
#'
#' link_out <- adaptive_rank(
#'   data = linking_samples,
#'   id_col = "ID",
#'   text_col = "text",
#'   backend = "openai",
#'   model = "gpt-5.1",
#'   adaptive_config = list(
#'     run_mode = "link_one_spoke",
#'     hub_id = 1L,
#'     phase_a_mode = "run",
#'     probe_panel_edges = 48L,
#'     hub_anchor_required_phase_b = TRUE,
#'     max_pairs_after_stop = 0L
#'   ),
#'   n_steps = 200,
#'   session_dir = file.path(tempdir(), "adaptive-link"),
#'   resume = TRUE,
#'   progress = "refits"
#' )
#'
#' # Anchored-joint is an explicit alternative, not the default:
#' # adaptive_config = list(
#' #   run_mode = "link_one_spoke",
#' #   hub_id = 1L,
#' #   phase_a_mode = "run",
#' #   link_estimation_mode = "anchored_joint",
#' #   hub_lock_mode = "hard_lock"
#' # )
#'
#' names(link_out$logs)
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
    adaptive_config = NULL,
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
  items <- samples
  names(items)[names(items) == "ID"] <- "item_id"

  if (!"text" %in% names(items)) {
    rlang::abort("Input data must include a text column after normalization.")
  }
  .adaptive_rank_validate_linking_config(items = items, adaptive_config = adaptive_config)

  loaded_state <- NULL
  if (isTRUE(resume) && !is.null(session_dir) && dir.exists(session_dir)) {
    paths <- .adaptive_session_paths(session_dir)
    has_saved_artifacts <- any(file.exists(c(
      paths$state,
      paths$step_log,
      paths$round_log,
      paths$metadata,
      paths$btl_fit
    ))) || dir.exists(paths$item_log_dir) || dir.exists(paths$phase_a_artifact_dir)

    if (isTRUE(has_saved_artifacts)) {
      loaded_state <- tryCatch(
        adaptive_rank_resume(session_dir),
        error = function(e) {
          rlang::abort(
            c(
              "Failed to resume adaptive session from `session_dir`.",
              i = "Set `resume = FALSE` to initialize a new session explicitly.",
              x = conditionMessage(e)
            )
          )
        }
      )
    }
  }

  state <- loaded_state
  if (is.null(state)) {
    state <- adaptive_rank_start(
      items = items,
      seed = seed,
      adaptive_config = adaptive_config,
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
    adaptive_config = adaptive_config,
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
  item_sort_by <- "rank_raw"
  if (length(logs$item_log) > 0L && is.data.frame(logs$item_log[[1L]])) {
    item_view <- tibble::as_tibble(logs$item_log[[1L]])
    item_cols <- names(item_view)
    rank_link_ok <- "rank_link" %in% item_cols &&
      any(is.finite(as.double(item_view$rank_link)), na.rm = TRUE) &&
      !all(is.na(item_view$theta_link_eap %||% rep(NA_real_, nrow(item_view))))
    if (isTRUE(rank_link_ok)) {
      item_sort_by <- "rank_link"
    } else if ("rank_raw" %in% item_cols) {
      item_sort_by <- "rank_raw"
    } else if ("rank_mean" %in% item_cols) {
      item_sort_by <- "rank_mean"
    }
  }
  out <- list(
    state = state,
    summary = summarize_adaptive(state),
    refits = summarize_refits(list(round_log = logs$round_log)),
    items = summarize_items(list(item_log_list = logs$item_log), sort_by = item_sort_by),
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
