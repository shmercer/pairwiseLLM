# Create only the fabricated-text example used by the warm-start guide.
# Run from the source root with an already provisioned interpreter:
# Rscript --vanilla data-raw/warm-start/create-vignette-example.R PYTHON OUTPUT.rds
local({
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 2L || !file.exists(args[[1L]]) || file.exists(args[[2L]])) {
    stop("Supply an existing Python interpreter and a new output RDS path.")
  }
  pkgload::load_all(quiet = TRUE)
  openings <- c(
    "Our class could grow vegetables beside the school.",
    "A library gives neighbors a quiet place to read.",
    "Walking to school can make a morning more enjoyable.",
    "Students should have time to draw and make music.",
    "A local park needs paths that everyone can use.",
    "Learning to cook helps people take care of themselves.",
    "Our town could run a small repair workshop.",
    "Sharing tools can help neighbors finish their projects.",
    "A school newspaper lets students explain events in their own words."
  )
  details <- c(
    "For example, a group of friends could work together on Saturday.",
    "Some people would need extra help, so a volunteer could explain each step.",
    "The first attempt might be difficult, but practice would make the work easier.",
    "I would ask the people nearby what they need before making a final decision.",
    "Although there would be a cost, we could begin with a small experiment.",
    "A clear schedule would give everyone an opportunity to join in.",
    "We should listen carefully when someone suggests a different approach.",
    "Keeping a record would help us understand what worked and what to change."
  )
  index <- seq_len(48L)
  texts <- vapply(index, function(i) {
    n <- 2L + (i - 1L) %% 6L
    selected <- ((seq_len(n) + i * 3L - 1L) %% length(details)) + 1L
    paste(c(openings[1L + (i - 1L) %% length(openings)], details[selected]), collapse = " ")
  }, character(1))
  stopifnot(!anyDuplicated(texts))
  ids <- sprintf("example-%02d", index)
  schema <- "writing_features_v2"
  status <- warm_start_python_status(python = args[[1L]], schema = schema)
  if (!isTRUE(status$available)) stop("The pinned extraction environment is unavailable.")
  features <- extract_warm_start_features(ids, texts, schema = schema, python = args[[1L]])
  # Invented demonstration outcomes, not BTL estimates or a scoring rule.
  theta <- 0.4 * ((index - 1L) %% 6L) + sin(index * 0.9) / 3
  example <- list(
    training = data.frame(item_id = ids[1:40], text = texts[1:40], theta = theta[1:40]),
    new_items = data.frame(item_id = ids[41:48], text = texts[41:48]),
    training_features = features[1:40, ], new_features = features[41:48, ],
    provenance = list(
      purpose = "Fabricated texts and outcomes for an offline documentation example; no student data.",
      schema = schema,
      schema_sha256 = "d9f271abae50eeac6d06a9ab7304309932174d54193ba8fb26348e9e886b2492",
      python = "3.12.3", spacy = "3.7.5", textdescriptives = "2.8.4", textstat = "0.7.13"
    )
  )
  saveRDS(example, args[[2L]], version = 2L, compress = "xz")
})
