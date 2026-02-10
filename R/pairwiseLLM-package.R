#' pairwiseLLM: Pairwise comparisons and adaptive ranking with LLM judges
#'
#' @description
#' `pairwiseLLM` supports (1) constructing paired-comparison designs over items,
#' (2) collecting binary judgments from LLMs (live or via provider batch APIs),
#' and (3) fitting ranking models (Elo, Bradley–Terry) including an adaptive,
#' Bayesian BTL (MCMC) workflow.
#'
#' @details
#' ## Typical workflow (most users)
#' 1. **Load items** using [read_samples_df()] or [read_samples_dir()].
#' 2. **Create a pairing design** with [make_pairs()] or [sample_pairs()],
#'    optionally adding reversals via [sample_reverse_pairs()].
#' 3. **(Optional) Control ordering / bias** with [randomize_pair_order()],
#'    [alternate_pair_order()], and diagnostics like [check_positional_bias()]
#'    or reversal agreement via [compute_reverse_consistency()].
#' 4. **Build prompts** with [build_prompt()] + [trait_description()], using
#'    templates managed by [list_prompt_templates()], [get_prompt_template()],
#'    [set_prompt_template()], [register_prompt_template()], and
#'    [remove_prompt_template()].
#' 5. **Collect judgments**:
#'    - Live: [submit_llm_pairs()] or provider wrappers like
#'      [submit_openai_pairs_live()], [submit_anthropic_pairs_live()],
#'      [submit_gemini_pairs_live()], [submit_ollama_pairs_live()],
#'      [submit_together_pairs_live()].
#'    - Batch (recommended for scale): [run_openai_batch_pipeline()],
#'      [run_anthropic_batch_pipeline()], [run_gemini_batch_pipeline()].
#' 6. **Assemble modeling data** with [build_bt_data()] or [build_elo_data()]
#'    (and for some pipelines, [build_btl_results_data()]).
#' 7. **Fit / summarize**:
#'    - Elo: [fit_elo_model()]
#'    - Bradley–Terry: [fit_bt_model()]
#'    - Bayesian BTL (MCMC): [fit_bayes_btl_mcmc()]
#'    - Summaries: [summarize_bt_fit()], [summarize_items()], [summarize_refits()]
#'
#' ## Adaptive Bayesian pairing + ranking (end-to-end loop)
#' If you want the package to both **choose pairs** and **fit Bayesian BTL**
#' in an auditable loop, use the adaptive workflow:
#' - Start a session with [adaptive_rank_start()] (or use the wrapper [adaptive_rank()]).
#' - Run live rounds with [adaptive_rank_run_live()].
#' - Resume an existing run with [adaptive_rank_resume()].
#' - Persist / reload sessions with [save_adaptive_session()], [load_adaptive_session()],
#'   and validate directories with [validate_session_dir()].
#'
#' ## Exported functions by task
#'
#' ### 1) Read / define items to rank
#' - [read_samples_df()], [read_samples_dir()]
#'
#' ### 2) Create pair designs and manage ordering
#' - Pair construction: [make_pairs()], [sample_pairs()], [sample_reverse_pairs()]
#' - Order helpers: [randomize_pair_order()], [alternate_pair_order()]
#' - Consistency / bias: [compute_reverse_consistency()], [check_positional_bias()]
#'
#' ### 3) Prompts and template registry
#' - Prompt building: [build_prompt()], [trait_description()]
#' - Templates: [list_prompt_templates()], [get_prompt_template()],
#'   [set_prompt_template()], [register_prompt_template()], [remove_prompt_template()]
#'
#' ### 4) LLM judging (provider-agnostic core)
#' - Utilities: [check_llm_api_keys()], [estimate_llm_pairs_cost()]
#' - Live submission: [submit_llm_pairs()], [llm_compare_pair()]
#' - Batch orchestration (generic): [llm_submit_pairs_batch()],
#'   [llm_submit_pairs_multi_batch()], [llm_download_batch_results()],
#'   [llm_resume_multi_batches()]
#'
#' ### 5) Provider-specific: live helpers
#' - OpenAI: [submit_openai_pairs_live()], [openai_compare_pair_live()]
#' - Anthropic: [submit_anthropic_pairs_live()], [anthropic_compare_pair_live()]
#' - Gemini: [submit_gemini_pairs_live()], [gemini_compare_pair_live()]
#' - Ollama: [submit_ollama_pairs_live()], [ollama_compare_pair_live()],
#'   [ensure_only_ollama_model_loaded()]
#' - Together: [submit_together_pairs_live()], [together_compare_pair_live()]
#'
#' ### 6) Provider-specific: batch pipelines + low-level batch helpers
#' - OpenAI pipeline: [run_openai_batch_pipeline()]
#'   - Build / write / upload: [build_openai_batch_requests()],
#'     [write_openai_batch_file()], [openai_upload_batch_file()]
#'   - Manage jobs: [openai_create_batch()], [openai_get_batch()],
#'     [openai_poll_batch_until_complete()]
#'   - Download / parse: [openai_download_batch_output()], [parse_openai_batch_output()]
#' - Anthropic pipeline: [run_anthropic_batch_pipeline()]
#'   - Build: [build_anthropic_batch_requests()]
#'   - Manage jobs: [anthropic_create_batch()], [anthropic_get_batch()],
#'     [anthropic_poll_batch_until_complete()]
#'   - Download / parse: [anthropic_download_batch_results()],
#'     [parse_anthropic_batch_output()]
#' - Gemini pipeline: [run_gemini_batch_pipeline()]
#'   - Build: [build_gemini_batch_requests()]
#'   - Manage jobs: [gemini_create_batch()], [gemini_get_batch()],
#'     [gemini_poll_batch_until_complete()]
#'   - Download / parse: [gemini_download_batch_results()],
#'     [parse_gemini_batch_output()]
#'
#' ### 7) Modeling: build data, fit models, summarize
#' - Data builders: [build_bt_data()], [build_elo_data()], [build_btl_results_data()]
#' - Fits: [fit_bt_model()], [fit_elo_model()], [fit_bayes_btl_mcmc()]
#' - Summaries: [summarize_bt_fit()], [summarize_items()], [summarize_refits()]
#'
#' ### 8) Adaptive workflow: run control, persistence, and summaries
#' - Control: [adaptive_rank()], [adaptive_rank_start()],
#'   [adaptive_rank_run_live()], [adaptive_rank_resume()]
#' - Session IO: [save_adaptive_session()], [load_adaptive_session()], [validate_session_dir()]
#' - Adaptive summaries: [summarize_adaptive()]
#' - Logging accessors (for audit / analysis): [adaptive_get_logs()],
#'   [adaptive_round_log()], [adaptive_step_log()], [adaptive_item_log()],
#'   [adaptive_results_history()]
#' - Advanced: [make_adaptive_judge_llm()]
#'
#' ## Design notes (adaptive Bayesian BTL)
#' The adaptive design targets stable, auditable rankings using Bayesian
#' Bradley–Terry–Luce inference with MCMC, supports position bias and lapse-rate
#' variants, and is intended to be robust for noisy LLM judges.
#'
#' The adaptive loop enforces connectivity, duplicate control with order reversal
#' for repeated unordered pairs, and approximate 50/50 position balance across items.
#' Stopping and refits are logged so stop decisions are reproducible from the
#' `round_log` fields (an explicit stop audit trail).
#'
#' @seealso
#' - For end-to-end adaptive ranking: [adaptive_rank_run_live()] or [adaptive_rank()].
#' - For large-scale provider batching: [run_openai_batch_pipeline()].
#'
#' @keywords internal
#' @docType package
#' @name pairwiseLLM
"_PACKAGE"
