# dev/dev-together-template-positional-bias.R
#
# Positional bias across five prompt templates ("test1"–"test5")
# using Together.ai models.

library(pairwiseLLM)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(purrr)

# ------------------------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------------------------

out_dir <- "dev-output/together-template-positional-bias"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

set.seed(123)

data("example_writing_samples", package = "pairwiseLLM")

td <- trait_description("overall_quality")

template_ids <- paste0("test", 1:5)

templates_tbl <- tibble::tibble(
  template_id     = template_ids,
  prompt_template = lapply(template_ids, get_prompt_template)
)

print(templates_tbl)

# ------------------------------------------------------------------------------
# 1. Build forward + reverse pairs (ALL pairs)
# ------------------------------------------------------------------------------

pairs_all <- example_writing_samples |>
  make_pairs()

pairs_forward <- pairs_all |>
  alternate_pair_order()

pairs_reverse <- sample_reverse_pairs(
  pairs_forward,
  reverse_pct = 1.0,
  seed        = 2002
)

get_pairs_for_direction <- function(direction) {
  if (identical(direction, "forward")) {
    pairs_forward
  } else if (identical(direction, "reverse")) {
    pairs_reverse
  } else {
    stop("Unknown direction: ", direction, call. = FALSE)
  }
}

directions <- c("forward", "reverse")

# ------------------------------------------------------------------------------
# 2. Together model grid
# ------------------------------------------------------------------------------

together_models <- c(
  "deepseek-ai/DeepSeek-R1",
  "deepseek-ai/DeepSeek-V3",
  "moonshotai/Kimi-K2-Instruct-0905",
  "Qwen/Qwen3-235B-A22B-Instruct-2507-tput"
)

model_matrix <- tidyr::expand_grid(
  model = together_models
)

print(model_matrix)

# Helper: safe filename
safe_name <- function(x) gsub("[^A-Za-z0-9_.-]", "-", x)

# Helper: CSV path for a combo
csv_path_for <- function(template_id, model, direction) {
  prefix <- paste("together", template_id, model, direction, sep = "_")
  prefix <- safe_name(prefix)
  file.path(out_dir, paste0(prefix, ".csv"))
}

# ------------------------------------------------------------------------------
# 3. Run / resume all combinations
# ------------------------------------------------------------------------------

summary_rows <- list()

for (t_row in seq_len(nrow(templates_tbl))) {
  template_id <- templates_tbl$template_id[t_row]
  tmpl_string <- templates_tbl$prompt_template[[t_row]]

  for (i in seq_len(nrow(model_matrix))) {
    model <- model_matrix$model[i]

    cat(
      "\n====================================================\n",
      "Template=", template_id,
      " | Model=", model,
      "\n====================================================\n"
    )

    res_dir <- list()

    for (direction in directions) {
      message("Direction: ", direction)

      csv_path <- csv_path_for(template_id, model, direction)

      if (file.exists(csv_path)) {
        message("  -> CSV exists, loading: ", csv_path)
        res <- readr::read_csv(csv_path, show_col_types = FALSE)
      } else {
        pairs_use <- get_pairs_for_direction(direction)

        # Safe wrapper: don't let a backend error kill the whole script
        res <- tryCatch(
          submit_together_pairs_live(
            pairs             = pairs_use,
            model             = model,
            trait_name        = td$name,
            trait_description = td$description,
            prompt_template   = tmpl_string,
            verbose           = TRUE,
            status_every      = 10,
            progress          = TRUE,
            include_raw       = FALSE
          ),
          error = function(e) {
            message(
              "  !! submit_together_pairs_live error for ",
              "template=", template_id,
              " / model=", model,
              " / direction=", direction, ": ",
              conditionMessage(e)
            )
            NULL
          }
        )

        if (!is.null(res)) {
          readr::write_csv(res, csv_path)
          message("  -> Results written to: ", csv_path)
        } else {
          message("  -> No results for this combo (skipping).")
        }
      }

      res_dir[[direction]] <- res
    }

    # ------------------------------------------------------------------------
    # 4. Compute reverse consistency + positional bias
    # ------------------------------------------------------------------------

    res_forward <- res_dir[["forward"]]
    res_reverse <- res_dir[["reverse"]]

    prop_consistent <- NA_real_
    prop_pos1 <- NA_real_
    p_sample1_overall <- NA_real_

    if (!is.null(res_forward) && !is.null(res_reverse) &&
      nrow(res_forward) > 0L && nrow(res_reverse) > 0L) {
      cons <- tryCatch(
        compute_reverse_consistency(res_forward, res_reverse),
        error = function(e) {
          message(
            "compute_reverse_consistency error for ",
            "template=", template_id,
            " / model=", model, ": ",
            conditionMessage(e)
          )
          NULL
        }
      )

      if (!is.null(cons)) {
        if (is.list(cons) && "summary" %in% names(cons)) {
          s <- cons$summary
          if (is.data.frame(s) && "prop_consistent" %in% names(s)) {
            prop_consistent <- s$prop_consistent[1]
          }
        }

        bias <- tryCatch(
          check_positional_bias(cons),
          error = function(e) {
            message(
              "check_positional_bias error for ",
              "template=", template_id,
              " / model=", model, ": ",
              conditionMessage(e)
            )
            NULL
          }
        )

        if (!is.null(bias) && is.list(bias) && "summary" %in% names(bias)) {
          bs <- bias$summary
          if (is.data.frame(bs)) {
            if ("prop_pos1" %in% names(bs)) {
              prop_pos1 <- bs$prop_pos1[1]
            } else if (all(c("total_pos1_wins", "total_comparisons")
            %in% names(bs))) {
              total_pos1_wins <- bs$total_pos1_wins[1]
              total_comparisons <- bs$total_comparisons[1]
              if (is.finite(total_pos1_wins) &&
                is.finite(total_comparisons) &&
                total_comparisons > 0) {
                prop_pos1 <- total_pos1_wins / total_comparisons
              }
            }

            if ("p_sample1_overall" %in% names(bs)) {
              p_sample1_overall <- bs$p_sample1_overall[1]
            }
          }
        }
      }
    }

    summary_rows[[length(summary_rows) + 1L]] <- tibble::tibble(
      template_id       = template_id,
      model             = model,
      prop_consistent   = prop_consistent,
      prop_pos1         = prop_pos1,
      p_sample1_overall = p_sample1_overall
    )
  }
}

# ------------------------------------------------------------------------------
# 5. Save summary
# ------------------------------------------------------------------------------

if (length(summary_rows) > 0L) {
  summary_tbl <- dplyr::bind_rows(summary_rows)

  summary_path <- file.path(
    out_dir,
    "together_template_positional_bias_summary.csv"
  )

  readr::write_csv(summary_tbl, summary_path)

  print(summary_tbl)
  message("Together template positional bias summary written to: ", summary_path)
} else {
  message("No summary rows were generated; check for earlier errors.")
}
