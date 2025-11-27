## code to prepare `example_writing_samples`, `example_writing_pairs`,
## and `example_openai_batch_output` datasets

library(tibble)
library(dplyr)
library(usethis)

# 1) Ten writing samples about "why writing assessment is difficult"
example_writing_samples <- tibble::tibble(
  ID = sprintf("S%02d", 1:10),
  text = c(
    # S01: very weak
    "Writing assessment is hard. People write different things. It is confusing.",
    # S02: weak
    "Assessing writing is difficult because everyone writes differently and it can be hard to decide what is good or bad.",
    # S03: weak-medium
    "Writing assessment is challenging because teachers must judge ideas, organization, grammar, and style all at once. Different raters may focus on different things.",
    # S04: medium
    "Writing assessment is difficult because writing is a complex skill. Raters must consider ideas, organization, style, and conventions, and these features do not always align. A paper with strong ideas might have weak grammar, while another has flawless sentences but no clear argument.",
    # S05: medium-good
    "Assessing writing is difficult because the construct is multidimensional and partly subjective. Even with detailed rubrics, raters interpret criteria differently, and their judgments can be influenced by their experience, fatigue, or expectations for a particular grade level.",
    # S06: good
    "Writing assessment is difficult because we are trying to compress a rich, multi-dimensional performance into a single score. Raters must weigh content, organization, style, and mechanics, while also dealing with time pressure. As a result, reliability is hard to achieve without extensive training and moderation.",
    # S07: good
    "Evaluating writing is challenging because no rubric can fully capture what makes a text effective for a particular audience and purpose. Two essays might receive the same score for very different reasons. This creates problems for feedback, grading, and large-scale testing, where high-stakes decisions depend on noisy scores.",
    # S08: very good
    "Writing assessment is difficult because it sits at the intersection of measurement and interpretation. Raters must translate complex judgments about ideas, structure, voice, and language into discrete rubric categories. Small differences in interpretation, or in how much weight is given to each dimension, can lead to substantial score variation.",
    # S09: excellent
    "Assessing writing is inherently difficult because it requires balancing consistency with sensitivity to context. A rubric can describe general features of quality, but individual texts vary in genre, audience, and purpose. Raters must decide whether unconventional choices are weaknesses or sophisticated responses to the task, which makes strict standardization almost impossible.",
    # S10: excellent
    "Writing assessment is difficult because it asks us to quantify something fundamentally qualitative. To evaluate a piece of writing, raters integrate judgments about content, organization, style, and conventions, while also considering task demands and reader expectations. Even with analytic rubrics and training, scores reflect both the text and the rater's theory of good writing, which introduces error and bias."
  ),
  # simple true quality score to drive simulated pair outcomes
  quality_score = 1:10
)

# 2) Complete paired comparison table implied by quality_score
pairs_matrix <- utils::combn(example_writing_samples$ID, 2)
example_writing_pairs <- tibble::tibble(
  ID1 = pairs_matrix[1, ],
  ID2 = pairs_matrix[2, ]
) |>
  left_join(
    example_writing_samples |>
      select(ID, q1 = quality_score),
    by = c("ID1" = "ID")
  ) |>
  left_join(
    example_writing_samples |>
      select(ID, q2 = quality_score),
    by = c("ID2" = "ID")
  ) |>
  mutate(
    better_id = if_else(q1 >= q2, ID1, ID2)
  ) |>
  select(ID1, ID2, better_id)

# 3) Example OpenAI Batch output lines (JSONL format)
# Based on current documented/observed output structure.
example_openai_batch_output <- c(
  # Line 1: SAMPLE_1 is better
  '{"id": "batch_req_aaa111", "custom_id": "EXP_S01_vs_S02", "response": {"status_code": 200, "request_id": "req_111aaa", "body": {"id": "chatcmpl-111aaa", "object": "chat.completion", "created": 1753322001, "model": "o3-2025-04-16", "choices": [{"index": 0, "message": {"role": "assistant", "content": "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>", "refusal": null, "annotations": []}, "finish_reason": "stop"}], "usage": {"prompt_tokens": 440, "completion_tokens": 95, "total_tokens": 535, "prompt_tokens_details": {"cached_tokens": 0, "audio_tokens": 0}, "completion_tokens_details": {"reasoning_tokens": 64, "audio_tokens": 0, "accepted_prediction_tokens": 0, "rejected_prediction_tokens": 0}}, "system_fingerprint": null}}, "error": null}',
  # Line 2: SAMPLE_2 is better
  '{"id": "batch_req_bbb222", "custom_id": "EXP_S01_vs_S03", "response": {"status_code": 200, "request_id": "req_222bbb", "body": {"id": "chatcmpl-222bbb", "object": "chat.completion", "created": 1753322002, "model": "o3-2025-04-16", "choices": [{"index": 0, "message": {"role": "assistant", "content": "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>", "refusal": null, "annotations": []}, "finish_reason": "stop"}], "usage": {"prompt_tokens": 452, "completion_tokens": 90, "total_tokens": 542, "prompt_tokens_details": {"cached_tokens": 0, "audio_tokens": 0}, "completion_tokens_details": {"reasoning_tokens": 60, "audio_tokens": 0, "accepted_prediction_tokens": 0, "rejected_prediction_tokens": 0}}, "system_fingerprint": null}}, "error": null}',
  # Line 3: an error case (status_code null, error populated)
  '{"id": "batch_req_ccc333", "custom_id": "EXP_S02_vs_S03", "response": {"status_code": null, "request_id": "", "body": null}, "error": {"code": "rate_limit_exceeded", "message": "Request was not processed in batch due to rate limiting."}}'
)

# Save datasets into the package
usethis::use_data(example_writing_samples, overwrite = TRUE)
usethis::use_data(example_writing_pairs,   overwrite = TRUE)
usethis::use_data(example_openai_batch_output, overwrite = TRUE)
