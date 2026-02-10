# pairwiseLLM: Pairwise comparisons and adaptive ranking with LLM judges

`pairwiseLLM` supports (1) constructing paired-comparison designs over
items, (2) collecting binary judgments from LLMs (live or via provider
batch APIs), and (3) fitting ranking models (Elo, Bradley–Terry)
including an adaptive, Bayesian BTL (MCMC) workflow.

## Details

### Typical workflow (most users)

1.  **Load items** using
    [`read_samples_df()`](https://shmercer.github.io/pairwiseLLM/reference/read_samples_df.md)
    or
    [`read_samples_dir()`](https://shmercer.github.io/pairwiseLLM/reference/read_samples_dir.md).

2.  **Create a pairing design** with
    [`make_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/make_pairs.md)
    or
    [`sample_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/sample_pairs.md),
    optionally adding reversals via
    [`sample_reverse_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/sample_reverse_pairs.md).

3.  **(Optional) Control ordering / bias** with
    [`randomize_pair_order()`](https://shmercer.github.io/pairwiseLLM/reference/randomize_pair_order.md),
    [`alternate_pair_order()`](https://shmercer.github.io/pairwiseLLM/reference/alternate_pair_order.md),
    and diagnostics like
    [`check_positional_bias()`](https://shmercer.github.io/pairwiseLLM/reference/check_positional_bias.md)
    or reversal agreement via
    [`compute_reverse_consistency()`](https://shmercer.github.io/pairwiseLLM/reference/compute_reverse_consistency.md).

4.  **Build prompts** with
    [`build_prompt()`](https://shmercer.github.io/pairwiseLLM/reference/build_prompt.md) +
    [`trait_description()`](https://shmercer.github.io/pairwiseLLM/reference/trait_description.md),
    using templates managed by
    [`list_prompt_templates()`](https://shmercer.github.io/pairwiseLLM/reference/list_prompt_templates.md),
    [`get_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/get_prompt_template.md),
    [`set_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/set_prompt_template.md),
    [`register_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/register_prompt_template.md),
    and
    [`remove_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/remove_prompt_template.md).

5.  **Collect judgments**:

    - Live:
      [`submit_llm_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/submit_llm_pairs.md)
      or provider wrappers like
      [`submit_openai_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_openai_pairs_live.md),
      [`submit_anthropic_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_anthropic_pairs_live.md),
      [`submit_gemini_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_gemini_pairs_live.md),
      [`submit_ollama_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_ollama_pairs_live.md),
      [`submit_together_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_together_pairs_live.md).

    - Batch (recommended for scale):
      [`run_openai_batch_pipeline()`](https://shmercer.github.io/pairwiseLLM/reference/run_openai_batch_pipeline.md),
      [`run_anthropic_batch_pipeline()`](https://shmercer.github.io/pairwiseLLM/reference/run_anthropic_batch_pipeline.md),
      [`run_gemini_batch_pipeline()`](https://shmercer.github.io/pairwiseLLM/reference/run_gemini_batch_pipeline.md).

6.  **Assemble modeling data** with
    [`build_bt_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_bt_data.md)
    or
    [`build_elo_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_elo_data.md)
    (and for some pipelines,
    [`build_btl_results_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_btl_results_data.md)).

7.  **Fit / summarize**:

    - Elo:
      [`fit_elo_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_elo_model.md)

    - Bradley–Terry:
      [`fit_bt_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md)

    - Bayesian BTL (MCMC):
      [`fit_bayes_btl_mcmc()`](https://shmercer.github.io/pairwiseLLM/reference/fit_bayes_btl_mcmc.md)

    - Summaries:
      [`summarize_bt_fit()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_bt_fit.md),
      [`summarize_items()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_items.md),
      [`summarize_refits()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_refits.md)

### Adaptive Bayesian pairing + ranking (end-to-end loop)

If you want the package to both **choose pairs** and **fit Bayesian
BTL** in an auditable loop, use the adaptive workflow:

- Start a session with
  [`adaptive_rank_start()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_start.md)
  (or use the wrapper
  [`adaptive_rank()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank.md)).

- Run live rounds with
  [`adaptive_rank_run_live()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_run_live.md).

- Resume an existing run with
  [`adaptive_rank_resume()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_resume.md).

- Persist / reload sessions with
  [`save_adaptive_session()`](https://shmercer.github.io/pairwiseLLM/reference/save_adaptive_session.md),
  [`load_adaptive_session()`](https://shmercer.github.io/pairwiseLLM/reference/load_adaptive_session.md),
  and validate directories with
  [`validate_session_dir()`](https://shmercer.github.io/pairwiseLLM/reference/validate_session_dir.md).

### Exported functions by task

#### 1) Read / define items to rank

- [`read_samples_df()`](https://shmercer.github.io/pairwiseLLM/reference/read_samples_df.md),
  [`read_samples_dir()`](https://shmercer.github.io/pairwiseLLM/reference/read_samples_dir.md)

#### 2) Create pair designs and manage ordering

- Pair construction:
  [`make_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/make_pairs.md),
  [`sample_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/sample_pairs.md),
  [`sample_reverse_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/sample_reverse_pairs.md)

- Order helpers:
  [`randomize_pair_order()`](https://shmercer.github.io/pairwiseLLM/reference/randomize_pair_order.md),
  [`alternate_pair_order()`](https://shmercer.github.io/pairwiseLLM/reference/alternate_pair_order.md)

- Consistency / bias:
  [`compute_reverse_consistency()`](https://shmercer.github.io/pairwiseLLM/reference/compute_reverse_consistency.md),
  [`check_positional_bias()`](https://shmercer.github.io/pairwiseLLM/reference/check_positional_bias.md)

#### 3) Prompts and template registry

- Prompt building:
  [`build_prompt()`](https://shmercer.github.io/pairwiseLLM/reference/build_prompt.md),
  [`trait_description()`](https://shmercer.github.io/pairwiseLLM/reference/trait_description.md)

- Templates:
  [`list_prompt_templates()`](https://shmercer.github.io/pairwiseLLM/reference/list_prompt_templates.md),
  [`get_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/get_prompt_template.md),
  [`set_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/set_prompt_template.md),
  [`register_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/register_prompt_template.md),
  [`remove_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/remove_prompt_template.md)

#### 4) LLM judging (provider-agnostic core)

- Utilities:
  [`check_llm_api_keys()`](https://shmercer.github.io/pairwiseLLM/reference/check_llm_api_keys.md),
  [`estimate_llm_pairs_cost()`](https://shmercer.github.io/pairwiseLLM/reference/estimate_llm_pairs_cost.md)

- Live submission:
  [`submit_llm_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/submit_llm_pairs.md),
  [`llm_compare_pair()`](https://shmercer.github.io/pairwiseLLM/reference/llm_compare_pair.md)

- Batch orchestration (generic):
  [`llm_submit_pairs_batch()`](https://shmercer.github.io/pairwiseLLM/reference/llm_submit_pairs_batch.md),
  [`llm_submit_pairs_multi_batch()`](https://shmercer.github.io/pairwiseLLM/reference/llm_submit_pairs_multi_batch.md),
  [`llm_download_batch_results()`](https://shmercer.github.io/pairwiseLLM/reference/llm_download_batch_results.md),
  [`llm_resume_multi_batches()`](https://shmercer.github.io/pairwiseLLM/reference/llm_resume_multi_batches.md)

#### 5) Provider-specific: live helpers

- OpenAI:
  [`submit_openai_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_openai_pairs_live.md),
  [`openai_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/openai_compare_pair_live.md)

- Anthropic:
  [`submit_anthropic_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_anthropic_pairs_live.md),
  [`anthropic_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_compare_pair_live.md)

- Gemini:
  [`submit_gemini_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_gemini_pairs_live.md),
  [`gemini_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_compare_pair_live.md)

- Ollama:
  [`submit_ollama_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_ollama_pairs_live.md),
  [`ollama_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/ollama_compare_pair_live.md),
  [`ensure_only_ollama_model_loaded()`](https://shmercer.github.io/pairwiseLLM/reference/ensure_only_ollama_model_loaded.md)

- Together:
  [`submit_together_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_together_pairs_live.md),
  [`together_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/together_compare_pair_live.md)

#### 6) Provider-specific: batch pipelines + low-level batch helpers

- OpenAI pipeline:
  [`run_openai_batch_pipeline()`](https://shmercer.github.io/pairwiseLLM/reference/run_openai_batch_pipeline.md)

  - Build / write / upload:
    [`build_openai_batch_requests()`](https://shmercer.github.io/pairwiseLLM/reference/build_openai_batch_requests.md),
    [`write_openai_batch_file()`](https://shmercer.github.io/pairwiseLLM/reference/write_openai_batch_file.md),
    [`openai_upload_batch_file()`](https://shmercer.github.io/pairwiseLLM/reference/openai_upload_batch_file.md)

  - Manage jobs:
    [`openai_create_batch()`](https://shmercer.github.io/pairwiseLLM/reference/openai_create_batch.md),
    [`openai_get_batch()`](https://shmercer.github.io/pairwiseLLM/reference/openai_get_batch.md),
    [`openai_poll_batch_until_complete()`](https://shmercer.github.io/pairwiseLLM/reference/openai_poll_batch_until_complete.md)

  - Download / parse:
    [`openai_download_batch_output()`](https://shmercer.github.io/pairwiseLLM/reference/openai_download_batch_output.md),
    [`parse_openai_batch_output()`](https://shmercer.github.io/pairwiseLLM/reference/parse_openai_batch_output.md)

- Anthropic pipeline:
  [`run_anthropic_batch_pipeline()`](https://shmercer.github.io/pairwiseLLM/reference/run_anthropic_batch_pipeline.md)

  - Build:
    [`build_anthropic_batch_requests()`](https://shmercer.github.io/pairwiseLLM/reference/build_anthropic_batch_requests.md)

  - Manage jobs:
    [`anthropic_create_batch()`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_create_batch.md),
    [`anthropic_get_batch()`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_get_batch.md),
    [`anthropic_poll_batch_until_complete()`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_poll_batch_until_complete.md)

  - Download / parse:
    [`anthropic_download_batch_results()`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_download_batch_results.md),
    [`parse_anthropic_batch_output()`](https://shmercer.github.io/pairwiseLLM/reference/parse_anthropic_batch_output.md)

- Gemini pipeline:
  [`run_gemini_batch_pipeline()`](https://shmercer.github.io/pairwiseLLM/reference/run_gemini_batch_pipeline.md)

  - Build:
    [`build_gemini_batch_requests()`](https://shmercer.github.io/pairwiseLLM/reference/build_gemini_batch_requests.md)

  - Manage jobs:
    [`gemini_create_batch()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_create_batch.md),
    [`gemini_get_batch()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_get_batch.md),
    [`gemini_poll_batch_until_complete()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_poll_batch_until_complete.md)

  - Download / parse:
    [`gemini_download_batch_results()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_download_batch_results.md),
    [`parse_gemini_batch_output()`](https://shmercer.github.io/pairwiseLLM/reference/parse_gemini_batch_output.md)

#### 7) Modeling: build data, fit models, summarize

- Data builders:
  [`build_bt_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_bt_data.md),
  [`build_elo_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_elo_data.md),
  [`build_btl_results_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_btl_results_data.md)

- Fits:
  [`fit_bt_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md),
  [`fit_elo_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_elo_model.md),
  [`fit_bayes_btl_mcmc()`](https://shmercer.github.io/pairwiseLLM/reference/fit_bayes_btl_mcmc.md)

- Summaries:
  [`summarize_bt_fit()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_bt_fit.md),
  [`summarize_items()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_items.md),
  [`summarize_refits()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_refits.md)

#### 8) Adaptive workflow: run control, persistence, and summaries

- Control:
  [`adaptive_rank()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank.md),
  [`adaptive_rank_start()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_start.md),
  [`adaptive_rank_run_live()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_run_live.md),
  [`adaptive_rank_resume()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_resume.md)

- Session IO:
  [`save_adaptive_session()`](https://shmercer.github.io/pairwiseLLM/reference/save_adaptive_session.md),
  [`load_adaptive_session()`](https://shmercer.github.io/pairwiseLLM/reference/load_adaptive_session.md),
  [`validate_session_dir()`](https://shmercer.github.io/pairwiseLLM/reference/validate_session_dir.md)

- Adaptive summaries:
  [`summarize_adaptive()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_adaptive.md)

- Logging accessors (for audit / analysis):
  [`adaptive_get_logs()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_get_logs.md),
  [`adaptive_round_log()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_round_log.md),
  [`adaptive_step_log()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_step_log.md),
  [`adaptive_item_log()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_item_log.md),
  [`adaptive_results_history()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_results_history.md)

- Advanced:
  [`make_adaptive_judge_llm()`](https://shmercer.github.io/pairwiseLLM/reference/make_adaptive_judge_llm.md)

### Design notes (adaptive Bayesian BTL)

The adaptive design targets stable, auditable rankings using Bayesian
Bradley–Terry–Luce inference with MCMC, supports position bias and
lapse-rate variants, and is intended to be robust for noisy LLM judges.

The adaptive loop enforces connectivity, duplicate control with order
reversal for repeated unordered pairs, and approximate 50/50 position
balance across items. Stopping and refits are logged so stop decisions
are reproducible from the `round_log` fields (an explicit stop audit
trail).

## See also

- For end-to-end adaptive ranking:
  [`adaptive_rank_run_live()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_run_live.md)
  or
  [`adaptive_rank()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank.md).

- For large-scale provider batching:
  [`run_openai_batch_pipeline()`](https://shmercer.github.io/pairwiseLLM/reference/run_openai_batch_pipeline.md).

## Author

**Maintainer**: Sterett H. Mercer <sterett.mercer@ubc.ca>
([ORCID](https://orcid.org/0000-0002-7940-4221))
