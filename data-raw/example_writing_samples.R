library(tibble)
library(dplyr)
library(usethis)

# 1) Twenty writing samples about "why writing assessment is difficult"
# Ranges from S01 (Very Weak) to S20 (Excellent)
example_writing_samples <- tibble::tibble(
  ID = sprintf("S%02d", 1:20),
  text = c(
    # S01: Very Weak
    "Writing assessment is hard. People write different things. It is
    confusing.",
    # S02
    "It is hard to grade writing. Some are long and some are short. I do not
    know which is best.",
    # S03
    "Assessing writing is difficult because everyone writes differently and it
    can be hard to decide what is good or bad.",
    # S04
    "Grading essays is tough work. You have to read a lot. Sometimes the
    handwriting is bad or the grammar is wrong, and that makes it hard to give
    a score.",
    # S05
    "Writing assessment is challenging because teachers must judge ideas,
    organization, grammar, and style all at once. Different raters may focus
    on different things.",
    # S06
    "It is difficult to assess writing because it is subjective. One teacher
    might like a creative style while another teacher wants a strict structure.
    This makes the scores unfair sometimes.",
    # S07
    "Writing assessment is difficult because writing is a complex skill. Raters
    must consider ideas, organization, style, and conventions, and these
    features do not always align.",
    # S08
    "A paper with strong ideas might have weak grammar, while another has
    flawless sentences but no clear argument. Deciding which one deserves a
    higher score is a major challenge in assessment.",
    # S09
    "Assessing writing is difficult because the construct is multidimensional.
    Even with detailed rubrics, raters interpret criteria differently, and their
    judgments can be influenced by fatigue or expectations.",
    # S10
    "The difficulty in writing assessment lies in consistency. Because raters
    bring their own background knowledge and preferences to the task, achieving
    high inter-rater reliability requires extensive training and calibration.",
    # S11
    "Writing assessment is difficult because we are trying to compress a rich,
    multi-dimensional performance into a single score. Raters must weigh
    content, organization, style, and mechanics, while also dealing with
    time pressure.",
    # S12
    "Evaluating writing is challenging because no rubric can fully capture what
    makes a text effective for a particular audience. Two essays might receive
    the same score for completely different reasons, obscuring the feedback
    loop.",
    # S13
    "Writing assessment is difficult because it is context-dependent. A style
    that works for a narrative is inappropriate for a report. Raters must
    constantly adjust their internal standard based on the specific purpose of
    the prompt.",
    # S14
    "The challenge of writing assessment is distinguishing between
    surface-level errors and deep structural flaws. Raters often over-penalize
    mechanical mistakes while missing more significant issues in logic or
    argumentation due to cognitive load.",
    # S15
    "Writing assessment is difficult because it sits at the intersection of
    measurement and interpretation. Raters must translate complex judgments
    about ideas, voice, and language into discrete rubric categories, often
    losing nuance in the process.",
    # S16
    "Assessing writing is inherently difficult because it requires balancing
    consistency with sensitivity. A rubric describes general qualities, but
    individual texts vary in genre and voice. Raters must decide if an
    unconventional choice is a mistake or a stylistic innovation.",
    # S17
    "Writing assessment is challenging because of the trade-off between
    validity and reliability. Highly standardized scoring protocols often strip
    away the subjective appreciation of voice and creativity, while holistic
    scoring captures the 'whole' but risks being unreliable.",
    # S18
    "The fundamental difficulty in writing assessment is cognitive complexity.
    The rater must construct a mental model of the writer's argument while
    simultaneously evaluating against specific criteria. This dual processing
    makes the task prone to bias and halo effects.",
    # S19
    "Writing assessment is difficult because it asks us to quantify something
    fundamentally qualitative. To evaluate a piece of writing, raters integrate
    judgments about content, organization, and style, while also considering
    task demands. Scores often reflect both the text and the rater's implicit
    theory of writing.",
    # S20: Excellent
    "Writing assessment is inherently problematic because it attempts to
    standardize a socially situated act. The assessment process often
    decontextualizes the writing, stripping it of its communicative purpose.
    Consequently, the score represents a construct of 'school writing' rather
    than authentic communication, creating a validity gap that simple
    psychometrics cannot resolve."
  ),
  # Simple true quality score to drive simulated pair outcomes (1 to 20)
  quality_score = 1:20
)

# 2) Complete paired comparison table implied by quality_score
# With 20 samples, this generates 190 pairs (20 * 19 / 2)
pairs_matrix <- utils::combn(example_writing_samples$ID, 2)

set.seed(1)

example_writing_pairs <- tibble::tibble(
  ID1 = pairs_matrix[1, ],
  ID2 = pairs_matrix[2, ]
) |>
  dplyr::left_join(
    example_writing_samples |>
      dplyr::select(ID, q1 = quality_score),
    by = c("ID1" = "ID")
  ) |>
  dplyr::left_join(
    example_writing_samples |>
      dplyr::select(ID, q2 = quality_score),
    by = c("ID2" = "ID")
  ) |>
  dplyr::mutate(
    # Reproducible stochastic outcomes: higher quality tends to win,
    # but not deterministically (avoids separation / convergence warnings).
    #
    # Tune `scale`:
    #   larger scale -> closer to 50/50 (more noise)
    #   smaller scale -> more deterministic (less noise)
    #
    # With scale = 2, a gap of 2 points gives p ~ 0.73, gap of 5 gives p ~ 0.92.
    .gap = q2 - q1,
    .scale = 2,
    .eta = .gap / .scale,
    .p_id2_wins = stats::plogis(.eta),
    better_id = dplyr::if_else(stats::runif(dplyr::n()) < .p_id2_wins, ID2, ID1)
  ) |>
  dplyr::select(ID1, ID2, better_id)

# 2b) Canonical results_tbl representation for Bayesian BTL workflows
example_writing_results <- example_writing_pairs |>
  dplyr::mutate(
    A_id = as.character(.data$ID1),
    B_id = as.character(.data$ID2),
    unordered_key = paste(pmin(.data$A_id, .data$B_id), pmax(.data$A_id, .data$B_id), sep = ":"),
    ordered_key = paste(.data$A_id, .data$B_id, sep = ":")
  ) |>
  dplyr::group_by(.data$unordered_key) |>
  dplyr::mutate(unordered_occurrence_index = dplyr::row_number()) |>
  dplyr::ungroup() |>
  dplyr::transmute(
    pair_uid = paste0(.data$unordered_key, "#", .data$unordered_occurrence_index),
    unordered_key = .data$unordered_key,
    ordered_key = .data$ordered_key,
    A_id = .data$A_id,
    B_id = .data$B_id,
    better_id = as.character(.data$better_id),
    winner_pos = as.integer(ifelse(.data$better_id == .data$A_id, 1L, 2L)),
    phase = "phase2",
    iter = as.integer(dplyr::row_number()),
    received_at = as.POSIXct("1970-01-01 00:00:00", tz = "UTC") +
      as.difftime(dplyr::row_number() - 1L, units = "secs"),
    backend = "non_adaptive_import",
    model = "unknown"
  )


# 3) Example OpenAI Batch output lines (JSONL format)
# Keeping a small representative sample (3 lines) to demonstrate structure.
# Generating 190 fake JSON lines would bloat the package data unnecessarily.
example_openai_batch_output <- c(
  # Line 1: SAMPLE_1 is better (S01 vs S02 - technically S02 is better in our
  # new logic, but this dataset is for structural testing of parsers,
  # not strict truth alignment).
  '{"id": "batch_req_aaa111", "custom_id": "EXP_S01_vs_S02", "response":
  {"status_code": 200, "request_id": "req_111aaa", "body":
  {"id": "chatcmpl-111aaa", "object": "chat.completion", "created":
  1753322001, "model": "o3-2025-04-16", "choices": [{"index": 0, "message":
  {"role": "assistant", "content": "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>",
  "refusal": null, "annotations": []}, "finish_reason": "stop"}], "usage":
  {"prompt_tokens": 440, "completion_tokens": 95, "total_tokens": 535,
  "prompt_tokens_details": {"cached_tokens": 0, "audio_tokens": 0},
  "completion_tokens_details": {"reasoning_tokens": 64, "audio_tokens": 0,
  "accepted_prediction_tokens": 0, "rejected_prediction_tokens": 0}},
  "system_fingerprint": null}}, "error": null}',
  # Line 2: SAMPLE_2 is better
  '{"id": "batch_req_bbb222", "custom_id": "EXP_S01_vs_S03", "response":
  {"status_code": 200, "request_id": "req_222bbb", "body":
  {"id": "chatcmpl-222bbb", "object": "chat.completion", "created": 1753322002,
  "model": "o3-2025-04-16", "choices": [{"index": 0, "message": {"role":
  "assistant", "content": "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>", "refusal":
  null, "annotations": []}, "finish_reason": "stop"}], "usage": {"prompt_tokens"
  : 452, "completion_tokens": 90, "total_tokens": 542, "prompt_tokens_details":
  {"cached_tokens": 0, "audio_tokens": 0}, "completion_tokens_details":
  {"reasoning_tokens": 60, "audio_tokens": 0, "accepted_prediction_tokens": 0,
  "rejected_prediction_tokens": 0}}, "system_fingerprint": null}}, "error":
  null}',
  # Line 3: an error case (status_code null, error populated)
  '{"id": "batch_req_ccc333", "custom_id": "EXP_S02_vs_S03", "response":
  {"status_code": null, "request_id": "", "body": null}, "error": {"code":
  "rate_limit_exceeded", "message": "Request was not processed in batch due
  to rate limiting."}}'
)

# Save datasets into the package
usethis::use_data(example_writing_samples, overwrite = TRUE)
usethis::use_data(example_writing_pairs, overwrite = TRUE)
usethis::use_data(example_writing_results, overwrite = TRUE)
usethis::use_data(example_openai_batch_output, overwrite = TRUE)
