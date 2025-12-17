# Internal environment for user-registered templates (session-local)
.pwllm_prompt_templates <- new.env(parent = emptyenv())

# Internal helper: read a built-in template from inst/templates
.pwllm_read_builtin_template <- function(name) {
  stopifnot(is.character(name), length(name) == 1L, nzchar(name))

  path <- system.file(
    "templates",
    paste0(name, ".txt"),
    package = "pairwiseLLM"
  )

  if (identical(path, "") || !file.exists(path)) {
    return(NULL)
  }

  text <- paste(
    readLines(path, warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )

  text
}

# Internal helper: get and validate a built-in template by name
.pwllm_get_builtin_template <- function(name) {
  tmpl <- .pwllm_read_builtin_template(name)
  if (is.null(tmpl)) {
    return(NULL)
  }
  .validate_template(tmpl)
}


#' Get or set a prompt template for pairwise comparisons
#'
#' This function returns a default prompt template that includes
#' placeholders for the trait name, trait description, and two
#' writing samples. Any custom template must contain the
#' placeholders \code{{TRAIT_NAME}}, \code{{TRAIT_DESCRIPTION}},
#' \code{{SAMPLE_1}}, and \code{{SAMPLE_2}}.
#'
#' The default template is stored as a plain-text file in
#' \code{inst/templates/default.txt} and loaded at run time. This
#' makes it easy to inspect and modify the prompt text without
#' changing the R code.
#'
#' @param template Optional character string containing a custom template.
#'   If \code{NULL}, a default template is returned.
#' @param file Optional path to a text file containing a template.
#'   Ignored if \code{template} is not \code{NULL}.
#'
#' @return A character string containing the prompt template.
#'
#' @examples
#' # Get the default template shipped with the package
#' tmpl <- set_prompt_template()
#' cat(substr(tmpl, 1, 200), "...\n")
#'
#' # Use a custom template defined in-line
#' custom <- "
#' You are an expert writing assessor for {TRAIT_NAME}.
#'
#' {TRAIT_NAME} is defined as {TRAIT_DESCRIPTION}.
#'
#' Which of the samples below is better on {TRAIT_NAME}?
#'
#' SAMPLE 1:
#' {SAMPLE_1}
#'
#' SAMPLE 2:
#' {SAMPLE_2}
#'
#' <BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE> or
#' <BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>
#' "
#'
#' tmpl2 <- set_prompt_template(template = custom)
#' cat(substr(tmpl2, 1, 120), "...\n")
#'
#' @export
set_prompt_template <- function(template = NULL,
                                file = NULL) {
  if (!is.null(template)) {
    return(.validate_template(template))
  }

  if (!is.null(file)) {
    template_text <- paste(readLines(file, warn = FALSE), collapse = "\n")
    return(.validate_template(template_text))
  }

  # Default: use the built-in template from inst/templates/default.txt
  default <- .pwllm_get_builtin_template("default")

  if (is.null(default)) {
    stop(
      "Built-in default template not found. ",
      "Expected a file 'inst/templates/default.txt' in the package.",
      call. = FALSE
    )
  }

  default
}

# Internal helper: check that required placeholders are present
.validate_template <- function(template) {
  required <- c("{TRAIT_NAME}", "{TRAIT_DESCRIPTION}", "{SAMPLE_1}", "{SAMPLE_2}")
  missing <- required[!vapply(required, grepl, logical(1), x = template, fixed = TRUE)]

  if (length(missing) > 0L) {
    stop(
      "Template is missing required placeholder(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  template
}

#' Build a concrete LLM prompt from a template
#'
#' This function takes a prompt template (typically from
#' \code{\link{set_prompt_template}}), a trait name and description,
#' and two writing samples, and fills in the required placeholders.
#'
#' The template must contain the placeholders:
#' \code{{TRAIT_NAME}}, \code{{TRAIT_DESCRIPTION}},
#' \code{{SAMPLE_1}}, and \code{{SAMPLE_2}}.
#'
#' @param template Character string containing the prompt template.
#' @param trait_name Character scalar giving a short label for the trait
#'   (e.g., "Overall Quality").
#' @param trait_desc Character scalar giving the full definition of the trait.
#' @param text1 Character scalar containing the text for SAMPLE_1.
#' @param text2 Character scalar containing the text for SAMPLE_2.
#'
#' @return A single character string containing the completed prompt.
#'
#' @examples
#' tmpl <- set_prompt_template()
#' td <- trait_description("overall_quality")
#' prompt <- build_prompt(
#'   template   = tmpl,
#'   trait_name = td$name,
#'   trait_desc = td$description,
#'   text1      = "This is sample 1.",
#'   text2      = "This is sample 2."
#' )
#' cat(substr(prompt, 1, 200), "...\n")
#'
#' @export
build_prompt <- function(template,
                         trait_name,
                         trait_desc,
                         text1,
                         text2) {
  if (!is.character(template) || length(template) != 1L) {
    stop("`template` must be a single character string.", call. = FALSE)
  }
  if (!is.character(trait_name) || length(trait_name) != 1L) {
    stop("`trait_name` must be a single character string.", call. = FALSE)
  }
  if (!is.character(trait_desc) || length(trait_desc) != 1L) {
    stop("`trait_desc` must be a single character string.", call. = FALSE)
  }
  if (!is.character(text1) || length(text1) != 1L) {
    stop("`text1` must be a single character string.", call. = FALSE)
  }
  if (!is.character(text2) || length(text2) != 1L) {
    stop("`text2` must be a single character string.", call. = FALSE)
  }

  out <- template
  out <- gsub("{TRAIT_NAME}", trait_name, out, fixed = TRUE)
  out <- gsub("{TRAIT_DESCRIPTION}", trait_desc, out, fixed = TRUE)
  out <- gsub("{SAMPLE_1}", text1, out, fixed = TRUE)
  out <- gsub("{SAMPLE_2}", text2, out, fixed = TRUE)

  out
}

#' Register a named prompt template
#'
#' This function validates a template (or reads it from a file) and
#' stores it under a user-provided name for reuse in the current R
#' session. Registered templates live in a package-internal registry.
#'
#' To make templates persistent across sessions, call this function
#' in your \code{.Rprofile} or in a project startup script.
#'
#' Any template must contain the placeholders
#' \code{{TRAIT_NAME}}, \code{{TRAIT_DESCRIPTION}},
#' \code{{SAMPLE_1}}, and \code{{SAMPLE_2}}.
#'
#' @param name Character scalar; name under which to store the template.
#' @param template Optional character string containing a custom template.
#'   If \code{NULL}, the template is read from \code{file}, or the
#'   package default is used when both \code{template} and \code{file}
#'   are \code{NULL}.
#' @param file Optional path to a text file containing a template.
#'   Ignored if \code{template} is not \code{NULL}.
#' @param overwrite Logical; if \code{FALSE} (default), an error is
#'   thrown when \code{name} already exists in the registry.
#'
#' @return Invisibly, the validated template string.
#'
#' @examples
#' # Register a custom template for this session
#' custom <- "
#' You are an expert writing assessor for {TRAIT_NAME}.
#'
#' {TRAIT_NAME} is defined as {TRAIT_DESCRIPTION}.
#'
#' Which of the samples below is better on {TRAIT_NAME}?
#'
#' SAMPLE 1:
#' {SAMPLE_1}
#'
#' SAMPLE 2:
#' {SAMPLE_2}
#'
#' <BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE> or
#' <BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>
#' "
#'
#' register_prompt_template("my_custom", template = custom)
#'
#' # Retrieve and inspect it
#' tmpl <- get_prompt_template("my_custom")
#' cat(substr(tmpl, 1, 160), "...\n")
#'
#' @export
register_prompt_template <- function(name,
                                     template = NULL,
                                     file = NULL,
                                     overwrite = FALSE) {
  if (!is.character(name) || length(name) != 1L || !nzchar(name)) {
    stop("`name` must be a non-empty character scalar.", call. = FALSE)
  }

  # Use the existing helper to build/validate the template
  tmpl <- set_prompt_template(template = template, file = file)

  if (!overwrite &&
    exists(name, envir = .pwllm_prompt_templates, inherits = FALSE)) {
    stop(
      "A prompt template named '", name, "' is already registered. ",
      "Use `overwrite = TRUE` to replace it.",
      call. = FALSE
    )
  }

  assign(name, tmpl, envir = .pwllm_prompt_templates)
  invisible(tmpl)
}

#' Retrieve a named prompt template
#'
#' This function retrieves a prompt template from either:
#' \itemize{
#'   \item the user registry (see \code{\link{register_prompt_template}}), or
#'   \item a built-in template stored under \code{inst/templates}.
#' }
#'
#' The function first checks user-registered templates, then looks for
#' a built-in text file \code{inst/templates/<name>.txt}. The special
#' name \code{"default"} falls back to \code{\link{set_prompt_template}()}
#' when no user-registered or built-in template is found.
#'
#' @param name Character scalar giving the template name.
#'
#' @return A single character string containing the prompt template.
#'
#' @examples
#' # Get the built-in default template
#' tmpl_default <- get_prompt_template("default")
#'
#' # List available template names
#' list_prompt_templates()
#'
#' @seealso \code{\link{register_prompt_template}},
#'   \code{\link{list_prompt_templates}},
#'   \code{\link{remove_prompt_template}}
#'
#' @export
get_prompt_template <- function(name = "default") {
  if (!is.character(name) || length(name) != 1L || !nzchar(name)) {
    stop("`name` must be a non-empty character scalar.", call. = FALSE)
  }

  # 1) User-registered template wins
  if (exists(name, envir = .pwllm_prompt_templates, inherits = FALSE)) {
    return(get(name, envir = .pwllm_prompt_templates, inherits = FALSE))
  }

  # 2) Built-in template from inst/templates
  builtin <- .pwllm_get_builtin_template(name)
  if (!is.null(builtin)) {
    return(builtin)
  }

  # 3) Special case: "default" falls back to set_prompt_template()
  if (identical(name, "default")) {
    return(set_prompt_template())
  }

  stop(
    "No prompt template found with name '", name, "'. ",
    "Use `list_prompt_templates()` to see available names.",
    call. = FALSE
  )
}

#' List available prompt templates
#'
#' This function lists template names that are available either as
#' built-in text files under \code{inst/templates} or as
#' user-registered templates in the current R session.
#'
#' Built-in templates are identified by files named
#' \code{<name>.txt} within \code{inst/templates}. For example, a
#' file \code{inst/templates/minimal.txt} will be listed as
#' \code{"minimal"}.
#'
#' @param include_builtin Logical; include built-in template names
#'   (the default is \code{TRUE}).
#' @param include_registered Logical; include user-registered names
#'   (the default is \code{TRUE}).
#'
#' @return A sorted character vector of unique template names.
#'
#' @examples
#' list_prompt_templates()
#'
#' @export
list_prompt_templates <- function(include_builtin = TRUE,
                                  include_registered = TRUE) {
  out <- character()

  if (isTRUE(include_builtin)) {
    # List files under inst/templates at run time
    dir_path <- system.file("templates", package = "pairwiseLLM")
    if (!identical(dir_path, "") && dir.exists(dir_path)) {
      files <- list.files(dir_path, pattern = "\\.txt$", full.names = FALSE)
      builtin_names <- sub("\\.txt$", "", files)
      out <- c(out, builtin_names)
    }
  }

  if (isTRUE(include_registered)) {
    reg <- ls(envir = .pwllm_prompt_templates, all.names = FALSE)
    out <- c(out, reg)
  }

  sort(unique(out))
}

#' Remove a registered prompt template
#'
#' This function removes a template from the user registry created by
#' \code{\link{register_prompt_template}}. It does not affect built-in
#' templates stored under \code{inst/templates}.
#'
#' @param name Character scalar; name of the template to remove.
#' @param quiet Logical; if \code{FALSE} (default), an error is thrown
#'   when \code{name} is not found in the user registry. When
#'   \code{TRUE}, the function simply returns \code{FALSE} in that
#'   case.
#'
#' @return Invisibly, \code{TRUE} if a template was removed,
#'   \code{FALSE} otherwise.
#'
#' @examples
#' # Register and then remove a template
#' register_prompt_template("to_delete", template = set_prompt_template())
#' remove_prompt_template("to_delete")
#'
#' @seealso \code{\link{register_prompt_template}},
#'   \code{\link{get_prompt_template}},
#'   \code{\link{list_prompt_templates}}
#'
#' @export
remove_prompt_template <- function(name, quiet = FALSE) {
  if (!is.character(name) || length(name) != 1L || !nzchar(name)) {
    stop("`name` must be a non-empty character scalar.", call. = FALSE)
  }

  if (!exists(name, envir = .pwllm_prompt_templates, inherits = FALSE)) {
    if (!quiet) {
      stop(
        "No registered prompt template named '", name, "'. ",
        "Built-in templates stored under inst/templates cannot be removed.",
        call. = FALSE
      )
    }
    return(invisible(FALSE))
  }

  rm(list = name, envir = .pwllm_prompt_templates)
  invisible(TRUE)
}
