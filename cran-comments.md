## Test environments

I tested the package on the following environments:

- **Local Windows 11**
  - R 4.5.2
  - `R CMD check --as-cran` runs cleanly with no errors, warnings, or notes.

- **GitHub Actions CI**, using `.github/workflows/R-CMD-check.yaml`
  - **ubuntu-latest**, R release
  - **ubuntu-latest**, R devel
  - **windows-latest**, R release
  - **macos-latest**, R release
  - All CI jobs complete successfully with: 0 errors or warnings.

- `devtools::check_win_devel()`
  - O errors or warnings, 1 note for possibly misspelled words ("Anthropic", "Elo", "LLMs", "Ollama", and "OpenAI"), which are spelled correctly.

## R CMD check results

0 errors | 0 warnings | 1 note

No errors or warnings.  
The single NOTE is expected and harmless.

## CRAN policy compliance

### Internet access

The package includes helper functions for calling external Large Language Model APIs (OpenAI, Anthropic, Gemini, Together.ai) and optional local inference via Ollama. **No network access ever occurs during R CMD check**:

- All examples involving API calls are wrapped in `\dontrun{}`.
- All vignette chunks that would perform API requests use `eval = FALSE`.
- Unit tests do **not** perform HTTP requests; external calls are mocked using `testthat::with_mocked_bindings`.

Thus, no internet access or API keys are needed for CRAN checks.

### External software

Ollama is optionally supported for local model inference but:

- Is never required by the package,
- Is never invoked automatically, and
- Is never executed during examples, tests, or vignettes.

This complies with CRAN rules regarding external system requirements.

### Examples

All exported functions contain examples.  
Examples that would access external services are correctly placed in `\dontrun{}` to comply with CRAN guidelines.  
Examples that do not require network calls run quickly and successfully.

### Vignettes

- Vignettes build cleanly with **no network access** and rely only on small built-in datasets.
- All code requiring API usage is set to `eval = FALSE`, matching CRAN policies for long-running or external-service code.

### Tests

Tests run deterministically and without side effects:

- All API interactions are mocked.
- Tests run successfully without requiring API keys or external binaries.
- No test performs file I/O outside temporary directories.

This ensures fast, clean CRAN checks.

### Reverse dependencies

This is the first CRAN submission, so there are no reverse dependencies.

## Additional notes

- The package does not modify user files or global options.
- All non-CRAN dependencies are placed in `Suggests` with proper conditional usage.
- The package follows MIT licensing and includes the required `LICENSE` and `LICENSE.md` files.

