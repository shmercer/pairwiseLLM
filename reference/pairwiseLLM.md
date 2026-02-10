# pairwiseLLM: Pairwise comparisons and adaptive ranking with LLM judges

`pairwiseLLM` supports (1) constructing paired-comparison designs over
items, (2) collecting binary judgments from LLMs (live or via provider
batch APIs), and (3) fitting ranking models (Elo, Bradley–Terry)
including an adaptive pairing Bayesian BTL workflow.

## Details

### Typical workflow

1.  **Load items** (the things you want to rank) using
    [`read_samples_df()`](https://shmercer.github.io/pairwiseLLM/reference/read_samples_df.md)
    or
    [`read_samples_dir()`](https://shmercer.github.io/pairwiseLLM/reference/read_samples_dir.md).

2.  **Create a pairing design** with
    [`make_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/make_pairs.md)
    /
    [`sample_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/sample_pairs.md)
    (and optional reverse pairs via
    [`sample_reverse_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/sample_reverse_pairs.md)).

3.  **Build prompts** using
    [`build_prompt()`](https://shmercer.github.io/pairwiseLLM/reference/build_prompt.md)
    and a template managed via
    [`list_prompt_templates()`](https://shmercer.github.io/pairwiseLLM/reference/list_prompt_templates.md),
    [`set_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/set_prompt_template.md),
    [`get_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/get_prompt_template.md),
    [`register_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/register_prompt_template.md),
    and
    [`remove_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/remove_prompt_template.md).

4.  **Collect judgments**:

    - Live:
      [`submit_llm_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/submit_llm_pairs.md),
      [`submit_openai_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_openai_pairs_live.md),
      [`submit_anthropic_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_anthropic_pairs_live.md),
      [`submit_gemini_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_gemini_pairs_live.md),
      [`submit_ollama_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_ollama_pairs_live.md),
      [`submit_together_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_together_pairs_live.md).

    - Batch:
      [`run_openai_batch_pipeline()`](https://shmercer.github.io/pairwiseLLM/reference/run_openai_batch_pipeline.md),
      [`run_anthropic_batch_pipeline()`](https://shmercer.github.io/pairwiseLLM/reference/run_anthropic_batch_pipeline.md),
      [`run_gemini_batch_pipeline()`](https://shmercer.github.io/pairwiseLLM/reference/run_gemini_batch_pipeline.md),
      plus provider helpers and parsers.

5.  **Assemble modeling data** with
    [`build_bt_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_bt_data.md)
    or
    [`build_elo_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_elo_data.md).

6.  **Fit / summarize**:

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

### Package organization

The exported functions are intended to be read as a set of modules:

#### 1) Samples / items

- [`read_samples_df()`](https://shmercer.github.io/pairwiseLLM/reference/read_samples_df.md),
  [`read_samples_dir()`](https://shmercer.github.io/pairwiseLLM/reference/read_samples_dir.md)

#### 2) Pair construction and ordering

- [`make_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/make_pairs.md),
  [`sample_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/sample_pairs.md),
  [`sample_reverse_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/sample_reverse_pairs.md)

- Order helpers:
  [`randomize_pair_order()`](https://shmercer.github.io/pairwiseLLM/reference/randomize_pair_order.md),
  [`alternate_pair_order()`](https://shmercer.github.io/pairwiseLLM/reference/alternate_pair_order.md)

- Consistency checks:
  [`compute_reverse_consistency()`](https://shmercer.github.io/pairwiseLLM/reference/compute_reverse_consistency.md)

#### 3) Prompt templates

- [`build_prompt()`](https://shmercer.github.io/pairwiseLLM/reference/build_prompt.md),
  [`trait_description()`](https://shmercer.github.io/pairwiseLLM/reference/trait_description.md)

- Template registry:
  [`list_prompt_templates()`](https://shmercer.github.io/pairwiseLLM/reference/list_prompt_templates.md),
  [`get_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/get_prompt_template.md),
  [`set_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/set_prompt_template.md),
  [`register_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/register_prompt_template.md),
  [`remove_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/remove_prompt_template.md)

#### 4) LLM submission (live + batch)

- Provider-agnostic:
  [`submit_llm_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/submit_llm_pairs.md),
  [`llm_compare_pair()`](https://shmercer.github.io/pairwiseLLM/reference/llm_compare_pair.md),
  [`llm_submit_pairs_batch()`](https://shmercer.github.io/pairwiseLLM/reference/llm_submit_pairs_batch.md),
  [`llm_submit_pairs_multi_batch()`](https://shmercer.github.io/pairwiseLLM/reference/llm_submit_pairs_multi_batch.md),
  [`llm_download_batch_results()`](https://shmercer.github.io/pairwiseLLM/reference/llm_download_batch_results.md),
  [`llm_resume_multi_batches()`](https://shmercer.github.io/pairwiseLLM/reference/llm_resume_multi_batches.md)

- OpenAI batch:
  [`run_openai_batch_pipeline()`](https://shmercer.github.io/pairwiseLLM/reference/run_openai_batch_pipeline.md),
  [`build_openai_batch_requests()`](https://shmercer.github.io/pairwiseLLM/reference/build_openai_batch_requests.md),
  [`openai_upload_batch_file()`](https://shmercer.github.io/pairwiseLLM/reference/openai_upload_batch_file.md),
  [`write_openai_batch_file()`](https://shmercer.github.io/pairwiseLLM/reference/write_openai_batch_file.md),
  [`openai_create_batch()`](https://shmercer.github.io/pairwiseLLM/reference/openai_create_batch.md),
  [`openai_get_batch()`](https://shmercer.github.io/pairwiseLLM/reference/openai_get_batch.md),
  [`openai_poll_batch_until_complete()`](https://shmercer.github.io/pairwiseLLM/reference/openai_poll_batch_until_complete.md),
  [`openai_download_batch_output()`](https://shmercer.github.io/pairwiseLLM/reference/openai_download_batch_output.md),
  [`parse_openai_batch_output()`](https://shmercer.github.io/pairwiseLLM/reference/parse_openai_batch_output.md)

- Anthropic batch:
  [`run_anthropic_batch_pipeline()`](https://shmercer.github.io/pairwiseLLM/reference/run_anthropic_batch_pipeline.md),
  [`build_anthropic_batch_requests()`](https://shmercer.github.io/pairwiseLLM/reference/build_anthropic_batch_requests.md),
  [`anthropic_create_batch()`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_create_batch.md),
  [`anthropic_get_batch()`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_get_batch.md),
  [`anthropic_poll_batch_until_complete()`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_poll_batch_until_complete.md),
  [`anthropic_download_batch_results()`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_download_batch_results.md),
  [`parse_anthropic_batch_output()`](https://shmercer.github.io/pairwiseLLM/reference/parse_anthropic_batch_output.md)

- Gemini batch:
  [`run_gemini_batch_pipeline()`](https://shmercer.github.io/pairwiseLLM/reference/run_gemini_batch_pipeline.md),
  [`build_gemini_batch_requests()`](https://shmercer.github.io/pairwiseLLM/reference/build_gemini_batch_requests.md),
  [`gemini_create_batch()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_create_batch.md),
  [`gemini_get_batch()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_get_batch.md),
  [`gemini_poll_batch_until_complete()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_poll_batch_until_complete.md),
  [`gemini_download_batch_results()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_download_batch_results.md),
  [`parse_gemini_batch_output()`](https://shmercer.github.io/pairwiseLLM/reference/parse_gemini_batch_output.md)

- Live provider helpers:
  [`openai_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/openai_compare_pair_live.md),
  [`anthropic_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_compare_pair_live.md),
  [`gemini_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_compare_pair_live.md),
  [`ollama_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/ollama_compare_pair_live.md),
  [`together_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/together_compare_pair_live.md)

#### 5) Modeling utilities and diagnostics

- Data builders:
  [`build_bt_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_bt_data.md),
  [`build_elo_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_elo_data.md)

- Fits:
  [`fit_bt_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md),
  [`fit_elo_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_elo_model.md),
  [`fit_bayes_btl_mcmc()`](https://shmercer.github.io/pairwiseLLM/reference/fit_bayes_btl_mcmc.md)

- Diagnostics:
  [`check_positional_bias()`](https://shmercer.github.io/pairwiseLLM/reference/check_positional_bias.md)

#### 6) Adaptive Bayesian pairing + ranking loop

- Entry / control:
  [`adaptive_rank_start()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_start.md),
  [`adaptive_rank_run_live()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_run_live.md),
  [`adaptive_rank_resume()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_resume.md)

### Design notes (adaptive Bayesian BTL)

The adaptive design targets stable, auditable rankings using Bayesian
Bradley–Terry–Luce inference with MCMC, supports position bias and
lapse-rate variants, and is intended to be robust for noisy LLM judges.
The recommended default model includes both global position bias and
lapse rate.

The adaptive loop enforces hard invariants such as connectivity,
duplicate control with order reversal for repeated unordered pairs, and
approximate 50/50 position balance across items.

Adaptive decision-making is gated on strict MCMC diagnostics.

When candidate generation is starved by constraints, the design
specifies a deterministic fallback ladder (expanding windows,
alternative anchor pools, controlled duplicate relaxation, and a global
backstop).

Stopping and refits are logged so that stop decisions are reproducible
from the `round_log` fields (an explicit stop audit trail).

## See also

- For interactive exploration: start with
  [`adaptive_rank_run_live()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_run_live.md)
  or
  [`run_openai_batch_pipeline()`](https://shmercer.github.io/pairwiseLLM/reference/run_openai_batch_pipeline.md).

## Author

**Maintainer**: Sterett H. Mercer <sterett.mercer@ubc.ca>
([ORCID](https://orcid.org/0000-0002-7940-4221))
