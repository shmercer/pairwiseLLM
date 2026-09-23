# Package index

## Start here

New users: follow the [Getting Started
guide](https://shmercer.github.io/pairwiseLLM/articles/getting-started.md)
for an offline example and a first collection workflow.

- [`pairwiseLLM-package`](https://shmercer.github.io/pairwiseLLM/reference/pairwiseLLM.md)
  [`pairwiseLLM`](https://shmercer.github.io/pairwiseLLM/reference/pairwiseLLM.md)
  : pairwiseLLM: Pairwise comparisons and adaptive ranking with LLM
  judges

## Prepare writing samples and pairs

Create pair tables locally before making provider requests.

- [`read_samples_df()`](https://shmercer.github.io/pairwiseLLM/reference/read_samples_df.md)
  : Read writing samples from a data frame
- [`read_samples_dir()`](https://shmercer.github.io/pairwiseLLM/reference/read_samples_dir.md)
  : Read writing samples from a directory of .txt files
- [`make_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/make_pairs.md)
  : Create all unordered pairs of writing samples
- [`sample_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/sample_pairs.md)
  : Randomly sample pairs of writing samples
- [`sample_reverse_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/sample_reverse_pairs.md)
  : Sample reversed versions of a subset of pairs
- [`randomize_pair_order()`](https://shmercer.github.io/pairwiseLLM/reference/randomize_pair_order.md)
  : Randomly assign samples to positions SAMPLE_1 and SAMPLE_2
- [`alternate_pair_order()`](https://shmercer.github.io/pairwiseLLM/reference/alternate_pair_order.md)
  : Deterministically alternate sample order in pairs

## Choose a trait and prepare prompts

- [`trait_description()`](https://shmercer.github.io/pairwiseLLM/reference/trait_description.md)
  : Get a trait name and description for prompts
- [`set_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/set_prompt_template.md)
  : Read or validate a prompt template for pairwise comparisons
- [`build_prompt()`](https://shmercer.github.io/pairwiseLLM/reference/build_prompt.md)
  : Build a concrete LLM prompt from a template
- [`register_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/register_prompt_template.md)
  : Register a named prompt template
- [`get_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/get_prompt_template.md)
  : Retrieve a named prompt template
- [`list_prompt_templates()`](https://shmercer.github.io/pairwiseLLM/reference/list_prompt_templates.md)
  : List available prompt templates
- [`remove_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/remove_prompt_template.md)
  : Remove a registered prompt template

## Collect comparisons

Use the generic helpers first. Cost estimation runs a paid pilot; see
[provider controls and
recovery](https://shmercer.github.io/pairwiseLLM/articles/provider-controls-and-recovery.md).

- [`check_llm_api_keys()`](https://shmercer.github.io/pairwiseLLM/reference/check_llm_api_keys.md)
  : Check configured API keys for LLM backends
- [`submit_llm_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/submit_llm_pairs.md)
  : Backend-agnostic live comparisons for a tibble of pairs
- [`llm_compare_pair()`](https://shmercer.github.io/pairwiseLLM/reference/llm_compare_pair.md)
  : Backend-agnostic live comparison for a single pair of samples
- [`llm_submit_pairs_batch()`](https://shmercer.github.io/pairwiseLLM/reference/llm_submit_pairs_batch.md)
  : Submit pairs to an LLM backend via batch API
- [`llm_download_batch_results()`](https://shmercer.github.io/pairwiseLLM/reference/llm_download_batch_results.md)
  : Extract results from a pairwiseLLM batch object
- [`llm_submit_pairs_multi_batch()`](https://shmercer.github.io/pairwiseLLM/reference/llm_submit_pairs_multi_batch.md)
  : Multi‑batch submission and polling wrappers
- [`llm_resume_multi_batches()`](https://shmercer.github.io/pairwiseLLM/reference/llm_resume_multi_batches.md)
  : Resume polling and download results for multiple batch jobs
- [`estimate_llm_pairs_cost()`](https://shmercer.github.io/pairwiseLLM/reference/estimate_llm_pairs_cost.md)
  : Estimate LLM token usage and cost for a set of pairwise comparisons
- [`print(`*`<pairwiseLLM_cost_estimate>`*`)`](https://shmercer.github.io/pairwiseLLM/reference/print.pairwiseLLM_cost_estimate.md)
  : Print a pairwiseLLM cost estimate

## Turn comparisons into rankings

BT and Elo use recorded winners; Bayesian BTL additionally requires
CmdStan.

- [`build_bt_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_bt_data.md)
  : Build Bradley-Terry comparison data from pairwise results

- [`fit_bt_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md)
  : Fit a Bradley–Terry model with sirt and fallback to BradleyTerry2

- [`summarize_bt_fit()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_bt_fit.md)
  : Summarize a Bradley–Terry model fit

- [`build_elo_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_elo_data.md)
  : Build EloChoice comparison data from pairwise results

- [`fit_elo_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_elo_model.md)
  : Fit an EloChoice model to pairwise comparison data

- [`build_btl_results_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_btl_results_data.md)
  :

  Build canonical `results_tbl` data for Bayesian BTL MCMC

- [`fit_bayes_btl_mcmc()`](https://shmercer.github.io/pairwiseLLM/reference/fit_bayes_btl_mcmc.md)
  : Full Bayesian BTL inference via CmdStanR (adaptive-compatible)

## Choose comparisons adaptively

Start with adaptive_rank(); see the [practical
guide](https://shmercer.github.io/pairwiseLLM/articles/adaptive-pairing.md).

- [`adaptive_rank()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank.md)
  : Run adaptive ranking end-to-end from data and model settings
- [`make_adaptive_judge_llm()`](https://shmercer.github.io/pairwiseLLM/reference/make_adaptive_judge_llm.md)
  : Build an LLM judge function for adaptive ranking

## Inspect rankings and diagnostics

- [`print(`*`<adaptive_state>`*`)`](https://shmercer.github.io/pairwiseLLM/reference/print.adaptive_state.md)
  : Print an adaptive state summary.
- [`adaptive_get_logs()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_get_logs.md)
  : Retrieve canonical adaptive logs.
- [`adaptive_step_log()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_step_log.md)
  : Adaptive step log accessor.
- [`adaptive_round_log()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_round_log.md)
  : Adaptive round log accessor.
- [`adaptive_item_log()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_item_log.md)
  : Adaptive item log accessor.
- [`adaptive_results_history()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_results_history.md)
  : Adaptive results history in build_bt_data() format.
- [`summarize_adaptive()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_adaptive.md)
  : Summarize an adaptive state.
- [`summarize_refits()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_refits.md)
  : Summarize adaptive refits
- [`summarize_items()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_items.md)
  : Summarize adaptive items

## Save and resume adaptive sessions

- [`save_adaptive_session()`](https://shmercer.github.io/pairwiseLLM/reference/save_adaptive_session.md)
  : Save an adaptive session to disk.
- [`validate_session_dir()`](https://shmercer.github.io/pairwiseLLM/reference/validate_session_dir.md)
  : Validate an adaptive session directory.
- [`load_adaptive_session()`](https://shmercer.github.io/pairwiseLLM/reference/load_adaptive_session.md)
  : Load an adaptive session from disk.

## Link separately ranked sets

Use saved comparisons to link sets, inspect results, and save your work;
choose a method explicitly.

- [`prepare_link_input()`](https://shmercer.github.io/pairwiseLLM/reference/prepare_link_input.md)
  : Prepare explicit evidence for a linking estimator
- [`start_link_session()`](https://shmercer.github.io/pairwiseLLM/reference/start_link_session.md)
  [`resume_link_session()`](https://shmercer.github.io/pairwiseLLM/reference/start_link_session.md)
  : Run and resume explicit-evidence linking sessions
- [`save_link_session()`](https://shmercer.github.io/pairwiseLLM/reference/save_link_session.md)
  [`load_link_session()`](https://shmercer.github.io/pairwiseLLM/reference/save_link_session.md)
  : Save and load exact linking sessions
- [`summary(`*`<pairwiseLLM_link_result>`*`)`](https://shmercer.github.io/pairwiseLLM/reference/summary.pairwiseLLM_link_result.md)
  [`summary(`*`<pairwiseLLM_link_session>`*`)`](https://shmercer.github.io/pairwiseLLM/reference/summary.pairwiseLLM_link_result.md)
  [`print(`*`<pairwiseLLM_link_result>`*`)`](https://shmercer.github.io/pairwiseLLM/reference/summary.pairwiseLLM_link_result.md)
  [`print(`*`<pairwiseLLM_link_session>`*`)`](https://shmercer.github.io/pairwiseLLM/reference/summary.pairwiseLLM_link_result.md)
  : Inspect explicit-evidence linking results and sessions
- [`fit_link()`](https://shmercer.github.io/pairwiseLLM/reference/fit_link.md)
  : Fit a linker using prepared explicit evidence
- [`predict_link()`](https://shmercer.github.io/pairwiseLLM/reference/predict_link.md)
  : Predict oriented hub-spoke comparison probabilities

## Convert Bayesian rankings to rubric levels

Requires completed Bayesian CJ results. See the [rubric
guide](https://shmercer.github.io/pairwiseLLM/articles/rubric-calibration.md)
for human labels and interpretation.

- [`prepare_linked_rubric_reference()`](https://shmercer.github.io/pairwiseLLM/reference/prepare_linked_rubric_reference.md)
  : Save a standalone Bayesian ranking as a reusable rubric reference
- [`fit_rubric_calibration()`](https://shmercer.github.io/pairwiseLLM/reference/fit_rubric_calibration.md)
  : Fit a rubric calibration to completed comparative judgments
- [`predict(`*`<pairwiseLLM_rubric_calibration>`*`)`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_rubric_calibration.md)
  : Predict rubric scores from a calibration
- [`evaluate_rubric_predictions()`](https://shmercer.github.io/pairwiseLLM/reference/evaluate_rubric_predictions.md)
  : Evaluate rubric predictions on observed ordered labels

## Check consistency and position bias

- [`compute_reverse_consistency()`](https://shmercer.github.io/pairwiseLLM/reference/compute_reverse_consistency.md)
  : Compute consistency between forward and reverse pair comparisons
- [`check_positional_bias()`](https://shmercer.github.io/pairwiseLLM/reference/check_positional_bias.md)
  : Check positional preference and bootstrap reversal agreement

## Advanced: predictive warm start

Use the [warm-start
guide](https://shmercer.github.io/pairwiseLLM/articles/adaptive-warm-start.md)
to go from features to validated models and starting scores.

- [`ensemble_warm_start_algorithms()`](https://shmercer.github.io/pairwiseLLM/reference/ensemble_warm_start_algorithms.md)
  : Average algorithms trained on the same task
- [`ensemble_warm_start_models()`](https://shmercer.github.io/pairwiseLLM/reference/ensemble_warm_start_models.md)
  : Combine independently trained warm-start models
- [`extract_warm_start_features()`](https://shmercer.github.io/pairwiseLLM/reference/extract_warm_start_features.md)
  : Extract frozen writing features for warm-start prediction
- [`fit_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_warm_start_model.md)
  : Train a task-specific warm-start model with nested validation
- [`make_warm_start_cv_plan()`](https://shmercer.github.io/pairwiseLLM/reference/make_warm_start_cv_plan.md)
  : Construct reusable warm-start cross-validation partitions
- [`make_warm_start_prior()`](https://shmercer.github.io/pairwiseLLM/reference/make_warm_start_prior.md)
  : Convert warm-start predictions to Bayesian BTL priors
- [`summary(`*`<pairwiseLLM_warm_model>`*`)`](https://shmercer.github.io/pairwiseLLM/reference/pairwiseLLM_warm_model.md)
  [`print(`*`<pairwiseLLM_warm_model>`*`)`](https://shmercer.github.io/pairwiseLLM/reference/pairwiseLLM_warm_model.md)
  : Portable task-specific warm-start models
- [`predict(`*`<pairwiseLLM_warm_algorithm_ensemble>`*`)`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_warm_algorithm_ensemble.md)
  : Predict with a same-task algorithm ensemble
- [`predict(`*`<pairwiseLLM_warm_ensemble>`*`)`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_warm_ensemble.md)
  : Predict with an equal-weight warm-start ensemble
- [`predict(`*`<pairwiseLLM_warm_model>`*`)`](https://shmercer.github.io/pairwiseLLM/reference/predict.pairwiseLLM_warm_model.md)
  : Predict relative quality from a portable warm-start model
- [`prepare_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/prepare_warm_start_model.md)
  : Prepare metadata or a summary-only warm-start artifact
- [`register_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/register_warm_start_model.md)
  [`remove_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/register_warm_start_model.md)
  [`list_warm_start_models()`](https://shmercer.github.io/pairwiseLLM/reference/register_warm_start_model.md)
  : Register, inspect, or remove user warm-start models
- [`save_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/save_warm_start_model.md)
  [`load_warm_start_model()`](https://shmercer.github.io/pairwiseLLM/reference/save_warm_start_model.md)
  : Save or load a portable warm-start model
- [`summary(`*`<pairwiseLLM_warm_algorithm_ensemble>`*`)`](https://shmercer.github.io/pairwiseLLM/reference/summary.pairwiseLLM_warm_algorithm_ensemble.md)
  [`print(`*`<pairwiseLLM_warm_algorithm_ensemble>`*`)`](https://shmercer.github.io/pairwiseLLM/reference/summary.pairwiseLLM_warm_algorithm_ensemble.md)
  : Inspect a same-task algorithm ensemble
- [`summary(`*`<pairwiseLLM_warm_ensemble>`*`)`](https://shmercer.github.io/pairwiseLLM/reference/summary.pairwiseLLM_warm_ensemble.md)
  [`print(`*`<pairwiseLLM_warm_ensemble>`*`)`](https://shmercer.github.io/pairwiseLLM/reference/summary.pairwiseLLM_warm_ensemble.md)
  : Inspect a warm-start ensemble
- [`summary(`*`<pairwiseLLM_warm_predictions>`*`)`](https://shmercer.github.io/pairwiseLLM/reference/summary.pairwiseLLM_warm_predictions.md)
  [`print(`*`<pairwiseLLM_warm_predictions>`*`)`](https://shmercer.github.io/pairwiseLLM/reference/summary.pairwiseLLM_warm_predictions.md)
  : Inspect ensemble predictions
- [`warm_start_coefficients()`](https://shmercer.github.io/pairwiseLLM/reference/warm_start_coefficients.md)
  : Inspect calibrated standardized warm-start coefficients
- [`warm_start_feature_schema()`](https://shmercer.github.io/pairwiseLLM/reference/warm_start_feature_schema.md)
  : Inspect the frozen warm-start writing feature schema
- [`warm_start_python_status()`](https://shmercer.github.io/pairwiseLLM/reference/warm_start_python_status.md)
  : Check the optional warm-start feature environment

## Advanced: adaptive lifecycle and replay

- [`make_adaptive_replay_reservoir()`](https://shmercer.github.io/pairwiseLLM/reference/make_adaptive_replay_reservoir.md)
  : Create a sparse frozen adaptive replay reservoir
- [`make_adaptive_judge_replay()`](https://shmercer.github.io/pairwiseLLM/reference/make_adaptive_judge_replay.md)
  : Create an offline judge from frozen directed outcomes
- [`validate_adaptive_replay()`](https://shmercer.github.io/pairwiseLLM/reference/validate_adaptive_replay.md)
  : Validate frozen directed judgments for an adaptive panel
- [`adaptive_rank_start()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_start.md)
  : Adaptive ranking
- [`adaptive_rank_run_live()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_run_live.md)
  : Adaptive ranking live runner
- [`adaptive_rank_resume()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_resume.md)
  : Adaptive ranking resume

## Provider-specific live helpers

- [`openai_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/openai_compare_pair_live.md)
  : Live OpenAI comparison for a single pair of samples
- [`submit_openai_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_openai_pairs_live.md)
  : Live OpenAI comparisons for a tibble of pairs
- [`anthropic_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_compare_pair_live.md)
  : Live Anthropic (Claude) comparison for a single pair of samples
- [`submit_anthropic_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_anthropic_pairs_live.md)
  : Live Anthropic (Claude) comparisons for a tibble of pairs
- [`gemini_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_compare_pair_live.md)
  : Live Google Gemini comparison for a single pair of samples
- [`submit_gemini_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_gemini_pairs_live.md)
  : Live Google Gemini comparisons for a tibble of pairs
- [`vertex_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/vertex_compare_pair_live.md)
  : Live Vertex AI Gemini comparison for a single pair of samples
- [`submit_vertex_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_vertex_pairs_live.md)
  : Live Vertex AI Gemini comparisons for a tibble of pairs
- [`together_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/together_compare_pair_live.md)
  : Live Together.ai comparison for a single pair of samples
- [`submit_together_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_together_pairs_live.md)
  : Live Together.ai comparisons for a tibble of pairs
- [`ollama_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/ollama_compare_pair_live.md)
  : Live Ollama comparison for a single pair of samples
- [`submit_ollama_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_ollama_pairs_live.md)
  : Live Ollama comparisons for a tibble of pairs

## Provider-specific batch helpers

- [`build_openai_batch_requests()`](https://shmercer.github.io/pairwiseLLM/reference/build_openai_batch_requests.md)
  : Build OpenAI batch JSONL lines for paired comparisons
- [`write_openai_batch_file()`](https://shmercer.github.io/pairwiseLLM/reference/write_openai_batch_file.md)
  : Write an OpenAI batch table to a JSONL file
- [`openai_upload_batch_file()`](https://shmercer.github.io/pairwiseLLM/reference/openai_upload_batch_file.md)
  : Upload a JSONL batch file to OpenAI
- [`openai_create_batch()`](https://shmercer.github.io/pairwiseLLM/reference/openai_create_batch.md)
  : Create an OpenAI batch from an uploaded file
- [`openai_get_batch()`](https://shmercer.github.io/pairwiseLLM/reference/openai_get_batch.md)
  : Retrieve an OpenAI batch
- [`openai_poll_batch_until_complete()`](https://shmercer.github.io/pairwiseLLM/reference/openai_poll_batch_until_complete.md)
  : Poll an OpenAI batch until it completes or fails
- [`openai_download_batch_output()`](https://shmercer.github.io/pairwiseLLM/reference/openai_download_batch_output.md)
  : Download the output file for a completed batch
- [`openai_download_batch_errors()`](https://shmercer.github.io/pairwiseLLM/reference/openai_download_batch_errors.md)
  : Download the error file for an OpenAI batch
- [`run_openai_batch_pipeline()`](https://shmercer.github.io/pairwiseLLM/reference/run_openai_batch_pipeline.md)
  : Run a full OpenAI batch pipeline for pairwise comparisons
- [`build_anthropic_batch_requests()`](https://shmercer.github.io/pairwiseLLM/reference/build_anthropic_batch_requests.md)
  : Build Anthropic Message Batch requests from a tibble of pairs
- [`anthropic_create_batch()`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_create_batch.md)
  : Create an Anthropic Message Batch
- [`anthropic_get_batch()`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_get_batch.md)
  : Retrieve an Anthropic Message Batch by ID
- [`anthropic_poll_batch_until_complete()`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_poll_batch_until_complete.md)
  : Poll an Anthropic Message Batch until completion
- [`anthropic_download_batch_results()`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_download_batch_results.md)
  : Download Anthropic Message Batch results (.jsonl)
- [`run_anthropic_batch_pipeline()`](https://shmercer.github.io/pairwiseLLM/reference/run_anthropic_batch_pipeline.md)
  : Run an Anthropic batch pipeline for pairwise comparisons
- [`build_gemini_batch_requests()`](https://shmercer.github.io/pairwiseLLM/reference/build_gemini_batch_requests.md)
  : Build Gemini batch requests from a tibble of pairs
- [`gemini_create_batch()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_create_batch.md)
  : Create a Gemini Batch job from request objects
- [`gemini_get_batch()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_get_batch.md)
  : Retrieve a Gemini Batch job by name
- [`gemini_poll_batch_until_complete()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_poll_batch_until_complete.md)
  : Poll a Gemini Batch job until completion
- [`gemini_download_batch_results()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_download_batch_results.md)
  : Download Gemini Batch results to a JSONL file
- [`run_gemini_batch_pipeline()`](https://shmercer.github.io/pairwiseLLM/reference/run_gemini_batch_pipeline.md)
  : Run a Gemini batch pipeline for pairwise comparisons

## Parse downloaded batch output

- [`parse_openai_batch_output()`](https://shmercer.github.io/pairwiseLLM/reference/parse_openai_batch_output.md)
  : Parse an OpenAI Batch output JSONL file
- [`parse_anthropic_batch_output()`](https://shmercer.github.io/pairwiseLLM/reference/parse_anthropic_batch_output.md)
  : Parse Anthropic Message Batch output into a tibble
- [`parse_gemini_batch_output()`](https://shmercer.github.io/pairwiseLLM/reference/parse_gemini_batch_output.md)
  : Parse Gemini batch JSONL output into a tibble of pairwise results

## Manage local Ollama resources

- [`ensure_only_ollama_model_loaded()`](https://shmercer.github.io/pairwiseLLM/reference/ensure_only_ollama_model_loaded.md)
  : Ensure only one Ollama model is loaded in memory

## Example data

Synthetic texts, bundled comparison outcomes, Bayesian input rows, and a
parser fixture for offline exploration.

- [`example_writing_samples`](https://shmercer.github.io/pairwiseLLM/reference/example_writing_samples.md)
  : Example dataset of writing samples
- [`example_writing_samples1000`](https://shmercer.github.io/pairwiseLLM/reference/example_writing_samples1000.md)
  : Synthetic Writing Samples with Controlled Quality Levels (N = 1000)
- [`example_writing_pairs`](https://shmercer.github.io/pairwiseLLM/reference/example_writing_pairs.md)
  : Example dataset of paired comparisons for writing samples
- [`example_writing_results`](https://shmercer.github.io/pairwiseLLM/reference/example_writing_results.md)
  : Example canonical results table for writing comparisons
- [`example_openai_batch_output`](https://shmercer.github.io/pairwiseLLM/reference/example_openai_batch_output.md)
  : Example OpenAI Batch output (JSONL lines)
