# Package index

## All functions

- [`add_pair_texts()`](https://shmercer.github.io/pairwiseLLM/reference/add_pair_texts.md)
  : Add text columns (text1/text2) to a pairs table
- [`alternate_pair_order()`](https://shmercer.github.io/pairwiseLLM/reference/alternate_pair_order.md)
  : Deterministically alternate sample order in pairs
- [`anthropic_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_compare_pair_live.md)
  : Live Anthropic (Claude) comparison for a single pair of samples
- [`anthropic_create_batch()`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_create_batch.md)
  : Create an Anthropic Message Batch
- [`anthropic_download_batch_results()`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_download_batch_results.md)
  : Download Anthropic Message Batch results (.jsonl)
- [`anthropic_get_batch()`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_get_batch.md)
  : Retrieve an Anthropic Message Batch by ID
- [`anthropic_poll_batch_until_complete()`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_poll_batch_until_complete.md)
  : Poll an Anthropic Message Batch until completion
- [`bt_adaptive_round()`](https://shmercer.github.io/pairwiseLLM/reference/bt_adaptive_round.md)
  : Run one adaptive round: compute metrics, decide stopping, and
  propose next pairs
- [`bt_core_link_round()`](https://shmercer.github.io/pairwiseLLM/reference/bt_core_link_round.md)
  : Propose a core-linking round given an existing BT fit
- [`bt_drift_metrics()`](https://shmercer.github.io/pairwiseLLM/reference/bt_drift_metrics.md)
  : Compute drift metrics between two theta estimates
- [`bt_run_adaptive()`](https://shmercer.github.io/pairwiseLLM/reference/bt_run_adaptive.md)
  : Run a round-based adaptive BT workflow end-to-end
- [`bt_run_core_linking()`](https://shmercer.github.io/pairwiseLLM/reference/bt_run_core_linking.md)
  : Run a core-linking batch workflow end-to-end (round-based)
- [`bt_should_stop()`](https://shmercer.github.io/pairwiseLLM/reference/bt_should_stop.md)
  : Decide whether to stop adaptive sampling based on stop metrics
- [`bt_stop_metrics()`](https://shmercer.github.io/pairwiseLLM/reference/bt_stop_metrics.md)
  : Compute stopping metrics from a Bradley–Terry model fit
- [`build_anthropic_batch_requests()`](https://shmercer.github.io/pairwiseLLM/reference/build_anthropic_batch_requests.md)
  : Build Anthropic Message Batch requests from a tibble of pairs
- [`build_bt_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_bt_data.md)
  : Build Bradley-Terry comparison data from pairwise results
- [`build_elo_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_elo_data.md)
  : Build EloChoice comparison data from pairwise results
- [`build_gemini_batch_requests()`](https://shmercer.github.io/pairwiseLLM/reference/build_gemini_batch_requests.md)
  : Build Gemini batch requests from a tibble of pairs
- [`build_openai_batch_requests()`](https://shmercer.github.io/pairwiseLLM/reference/build_openai_batch_requests.md)
  : Build OpenAI batch JSONL lines for paired comparisons
- [`build_prompt()`](https://shmercer.github.io/pairwiseLLM/reference/build_prompt.md)
  : Build a concrete LLM prompt from a template
- [`check_llm_api_keys()`](https://shmercer.github.io/pairwiseLLM/reference/check_llm_api_keys.md)
  : Check configured API keys for LLM backends
- [`check_positional_bias()`](https://shmercer.github.io/pairwiseLLM/reference/check_positional_bias.md)
  : Check positional bias and bootstrap consistency reliability
- [`compute_reverse_consistency()`](https://shmercer.github.io/pairwiseLLM/reference/compute_reverse_consistency.md)
  : Compute consistency between forward and reverse pair comparisons
- [`ensure_only_ollama_model_loaded()`](https://shmercer.github.io/pairwiseLLM/reference/ensure_only_ollama_model_loaded.md)
  : Ensure only one Ollama model is loaded in memory
- [`estimate_llm_pairs_cost()`](https://shmercer.github.io/pairwiseLLM/reference/estimate_llm_pairs_cost.md)
  : Estimate LLM token usage and cost for a set of pairwise comparisons
- [`example_openai_batch_output`](https://shmercer.github.io/pairwiseLLM/reference/example_openai_batch_output.md)
  : Example OpenAI Batch output (JSONL lines)
- [`example_writing_pairs`](https://shmercer.github.io/pairwiseLLM/reference/example_writing_pairs.md)
  : Example dataset of paired comparisons for writing samples
- [`example_writing_samples`](https://shmercer.github.io/pairwiseLLM/reference/example_writing_samples.md)
  : Example dataset of writing samples
- [`fit_bt_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md)
  : Fit a Bradley–Terry model with sirt and fallback to BradleyTerry2
- [`fit_elo_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_elo_model.md)
  : Fit an EloChoice model to pairwise comparison data
- [`gemini_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_compare_pair_live.md)
  : Live Google Gemini comparison for a single pair of samples
- [`gemini_create_batch()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_create_batch.md)
  : Create a Gemini Batch job from request objects
- [`gemini_download_batch_results()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_download_batch_results.md)
  : Download Gemini Batch results to a JSONL file
- [`gemini_get_batch()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_get_batch.md)
  : Retrieve a Gemini Batch job by name
- [`gemini_poll_batch_until_complete()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_poll_batch_until_complete.md)
  : Poll a Gemini Batch job until completion
- [`get_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/get_prompt_template.md)
  : Retrieve a named prompt template
- [`list_prompt_templates()`](https://shmercer.github.io/pairwiseLLM/reference/list_prompt_templates.md)
  : List available prompt templates
- [`llm_compare_pair()`](https://shmercer.github.io/pairwiseLLM/reference/llm_compare_pair.md)
  : Backend-agnostic live comparison for a single pair of samples
- [`llm_download_batch_results()`](https://shmercer.github.io/pairwiseLLM/reference/llm_download_batch_results.md)
  : Extract results from a pairwiseLLM batch object
- [`llm_resume_multi_batches()`](https://shmercer.github.io/pairwiseLLM/reference/llm_resume_multi_batches.md)
  : Resume polling and download results for multiple batch jobs
- [`llm_submit_pairs_batch()`](https://shmercer.github.io/pairwiseLLM/reference/llm_submit_pairs_batch.md)
  : Submit pairs to an LLM backend via batch API
- [`llm_submit_pairs_multi_batch()`](https://shmercer.github.io/pairwiseLLM/reference/llm_submit_pairs_multi_batch.md)
  : Multi‑batch submission and polling wrappers
- [`make_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/make_pairs.md)
  : Create all unordered pairs of writing samples
- [`ollama_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/ollama_compare_pair_live.md)
  : Live Ollama comparison for a single pair of samples
- [`openai_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/openai_compare_pair_live.md)
  : Live OpenAI comparison for a single pair of samples
- [`openai_create_batch()`](https://shmercer.github.io/pairwiseLLM/reference/openai_create_batch.md)
  : Create an OpenAI batch from an uploaded file
- [`openai_download_batch_output()`](https://shmercer.github.io/pairwiseLLM/reference/openai_download_batch_output.md)
  : Download the output file for a completed batch
- [`openai_get_batch()`](https://shmercer.github.io/pairwiseLLM/reference/openai_get_batch.md)
  : Retrieve an OpenAI batch
- [`openai_poll_batch_until_complete()`](https://shmercer.github.io/pairwiseLLM/reference/openai_poll_batch_until_complete.md)
  : Poll an OpenAI batch until it completes or fails
- [`openai_upload_batch_file()`](https://shmercer.github.io/pairwiseLLM/reference/openai_upload_batch_file.md)
  : Upload a JSONL batch file to OpenAI
- [`parse_anthropic_batch_output()`](https://shmercer.github.io/pairwiseLLM/reference/parse_anthropic_batch_output.md)
  : Parse Anthropic Message Batch output into a tibble
- [`parse_gemini_batch_output()`](https://shmercer.github.io/pairwiseLLM/reference/parse_gemini_batch_output.md)
  : Parse Gemini batch JSONL output into a tibble of pairwise results
- [`parse_openai_batch_output()`](https://shmercer.github.io/pairwiseLLM/reference/parse_openai_batch_output.md)
  : Parse an OpenAI Batch output JSONL file
- [`print(`*`<pairwiseLLM_cost_estimate>`*`)`](https://shmercer.github.io/pairwiseLLM/reference/print.pairwiseLLM_cost_estimate.md)
  : Print a pairwiseLLM cost estimate
- [`randomize_pair_order()`](https://shmercer.github.io/pairwiseLLM/reference/randomize_pair_order.md)
  : Randomly assign samples to positions SAMPLE_1 and SAMPLE_2
- [`read_samples_df()`](https://shmercer.github.io/pairwiseLLM/reference/read_samples_df.md)
  : Read writing samples from a data frame
- [`read_samples_dir()`](https://shmercer.github.io/pairwiseLLM/reference/read_samples_dir.md)
  : Read writing samples from a directory of .txt files
- [`register_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/register_prompt_template.md)
  : Register a named prompt template
- [`remove_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/remove_prompt_template.md)
  : Remove a registered prompt template
- [`run_anthropic_batch_pipeline()`](https://shmercer.github.io/pairwiseLLM/reference/run_anthropic_batch_pipeline.md)
  : Run an Anthropic batch pipeline for pairwise comparisons
- [`run_gemini_batch_pipeline()`](https://shmercer.github.io/pairwiseLLM/reference/run_gemini_batch_pipeline.md)
  : Run a Gemini batch pipeline for pairwise comparisons
- [`run_openai_batch_pipeline()`](https://shmercer.github.io/pairwiseLLM/reference/run_openai_batch_pipeline.md)
  : Run a full OpenAI batch pipeline for pairwise comparisons
- [`sample_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/sample_pairs.md)
  : Randomly sample pairs of writing samples
- [`sample_reverse_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/sample_reverse_pairs.md)
  : Sample reversed versions of a subset of pairs
- [`select_adaptive_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/select_adaptive_pairs.md)
  : Select adaptive pairs for the next round of comparisons
- [`select_core_link_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/select_core_link_pairs.md)
  : Select core-linking pairs for BT scaling across batches/waves
- [`select_core_set()`](https://shmercer.github.io/pairwiseLLM/reference/select_core_set.md)
  : Select a core set of items for BT linking
- [`set_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/set_prompt_template.md)
  : Get or set a prompt template for pairwise comparisons
- [`simulate_bt_judge()`](https://shmercer.github.io/pairwiseLLM/reference/simulate_bt_judge.md)
  : Simulate a judge for BT pairwise comparisons
- [`submit_anthropic_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_anthropic_pairs_live.md)
  : Live Anthropic (Claude) comparisons for a tibble of pairs
- [`submit_gemini_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_gemini_pairs_live.md)
  : Live Google Gemini comparisons for a tibble of pairs
- [`submit_llm_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/submit_llm_pairs.md)
  : Backend-agnostic live comparisons for a tibble of pairs
- [`submit_ollama_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_ollama_pairs_live.md)
  : Live Ollama comparisons for a tibble of pairs
- [`submit_openai_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_openai_pairs_live.md)
  : Live OpenAI comparisons for a tibble of pairs
- [`submit_together_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_together_pairs_live.md)
  : Live Together.ai comparisons for a tibble of pairs
- [`summarize_bt_fit()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_bt_fit.md)
  : Summarize a Bradley–Terry model fit
- [`together_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/together_compare_pair_live.md)
  : Live Together.ai comparison for a single pair of samples
- [`trait_description()`](https://shmercer.github.io/pairwiseLLM/reference/trait_description.md)
  : Get a trait name and description for prompts
- [`write_openai_batch_file()`](https://shmercer.github.io/pairwiseLLM/reference/write_openai_batch_file.md)
  : Write an OpenAI batch table to a JSONL file
