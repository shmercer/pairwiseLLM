#' pairwiseLLM: Pairwise comparisons and adaptive ranking with LLM judges
#'
#' @description
#' `pairwiseLLM` supports (1) constructing paired-comparison designs over items,
#' (2) collecting binary judgments from LLMs (live or via provider batch APIs),
#' and (3) fitting ranking models (Elo, Bradley–Terry) including an adaptive pairing
#' Bayesian BTL workflow.
#'
#' @details
#' ## Typical workflow
#' 1. **Load items** (the things you want to rank) using [read_samples_df()] or
#'    [read_samples_dir()].
#' 2. **Create a pairing design** with [make_pairs()] / [sample_pairs()] (and
#'    optional reverse pairs via [sample_reverse_pairs()]).
#' 3. **Build prompts** using [build_prompt()] and a template managed via
#'    [list_prompt_templates()], [set_prompt_template()], [get_prompt_template()],
#'    [register_prompt_template()], and [remove_prompt_template()].
#' 4. **Collect judgments**:
#'    - Live: [submit_llm_pairs()], [submit_openai_pairs_live()],
#'      [submit_anthropic_pairs_live()], [submit_gemini_pairs_live()],
#'      [submit_ollama_pairs_live()], [submit_together_pairs_live()].
#'    - Batch: [run_openai_batch_pipeline()], [run_anthropic_batch_pipeline()],
#'      [run_gemini_batch_pipeline()], plus provider helpers and parsers.
#' 5. **Assemble modeling data** with [build_bt_data()] or [build_elo_data()].
#' 6. **Fit / summarize**:
#'    - Elo: [fit_elo_model()]
#'    - Bradley–Terry: [fit_bt_model()]
#'    - Bayesian BTL (MCMC): [fit_bayes_btl_mcmc()]
#'    - Summaries: [summarize_bt_fit()], [summarize_items()],
#'      [summarize_refits()]
#'
#' ## Package organization
#' The exported functions are intended to be read as a set of modules:
#'
#' ### 1) Samples / items
#' - [read_samples_df()], [read_samples_dir()]
#'
#' ### 2) Pair construction and ordering
#' - [make_pairs()], [sample_pairs()], [sample_reverse_pairs()]
#' - Order helpers: [randomize_pair_order()], [alternate_pair_order()]
#' - Consistency checks: [compute_reverse_consistency()]
#'
#' ### 3) Prompt templates
#' - [build_prompt()], [trait_description()]
#' - Template registry: [list_prompt_templates()], [get_prompt_template()],
#'   [set_prompt_template()], [register_prompt_template()],
#'   [remove_prompt_template()]
#'
#' ### 4) LLM submission (live + batch)
#' - Provider-agnostic: [submit_llm_pairs()], [llm_compare_pair()],
#'   [llm_submit_pairs_batch()], [llm_submit_pairs_multi_batch()],
#'   [llm_download_batch_results()], [llm_resume_multi_batches()]
#' - OpenAI batch: [run_openai_batch_pipeline()], [build_openai_batch_requests()],
#'   [openai_upload_batch_file()], [write_openai_batch_file()],
#'   [openai_create_batch()], [openai_get_batch()],
#'   [openai_poll_batch_until_complete()], [openai_download_batch_output()],
#'   [parse_openai_batch_output()]
#' - Anthropic batch: [run_anthropic_batch_pipeline()],
#'   [build_anthropic_batch_requests()], [anthropic_create_batch()],
#'   [anthropic_get_batch()], [anthropic_poll_batch_until_complete()],
#'   [anthropic_download_batch_results()], [parse_anthropic_batch_output()]
#' - Gemini batch: [run_gemini_batch_pipeline()], [build_gemini_batch_requests()],
#'   [gemini_create_batch()], [gemini_get_batch()],
#'   [gemini_poll_batch_until_complete()], [gemini_download_batch_results()],
#'   [parse_gemini_batch_output()]
#' - Live provider helpers: [openai_compare_pair_live()],
#'   [anthropic_compare_pair_live()], [gemini_compare_pair_live()],
#'   [ollama_compare_pair_live()], [together_compare_pair_live()]
#'
#' ### 5) Modeling utilities and diagnostics
#' - Data builders: [build_bt_data()], [build_elo_data()]
#' - Fits: [fit_bt_model()], [fit_elo_model()], [fit_bayes_btl_mcmc()]
#' - Diagnostics: [check_positional_bias()]
#'
#' ### 6) Adaptive Bayesian pairing + ranking loop
#' - Entry / control: [adaptive_rank_start()],
#'   [adaptive_rank_run_live()], [adaptive_rank_resume()]
#'
#' ## Design notes (adaptive Bayesian BTL)
#' The adaptive design targets stable, auditable rankings using Bayesian
#' Bradley–Terry–Luce inference with MCMC, supports position bias and lapse-rate
#' variants, and is intended to be robust for noisy LLM judges. The recommended
#' default model includes both global position bias and lapse rate.
#'
#' The adaptive loop enforces hard invariants such as connectivity, duplicate
#' control with order reversal for repeated unordered pairs, and approximate
#' 50/50 position balance across items.
#'
#' Adaptive decision-making is gated on strict MCMC diagnostics.
#'
#' When candidate generation is starved by constraints, the design specifies a
#' deterministic fallback ladder (expanding windows, alternative anchor pools,
#' controlled duplicate relaxation, and a global backstop).
#'
#' Stopping and refits are logged so that stop decisions are reproducible from the
#' `round_log` fields (an explicit stop audit trail).
#'
#' @seealso
#' - For interactive exploration: start with [adaptive_rank_run_live()] or
#'   [run_openai_batch_pipeline()].
#'
#' @keywords internal
#' @docType package
#' @name pairwiseLLM
"_PACKAGE"
