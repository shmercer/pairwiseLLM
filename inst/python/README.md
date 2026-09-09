# Optional writing feature environment

`extract_warm_start_features()` uses optional reticulate and an existing Python
installation. It never installs software, creates an environment, or downloads
resources. Schema inspection and prediction from precomputed features do not need
Python. The model is large (approximately 588 MB download). A status check loads
it to verify capabilities, so the first check can take several seconds.

The frozen baseline is Python 3.12.3, TextDescriptives 2.8.4, textstat 0.7.13,
spaCy 3.7.5, Thinc 8.2.5, NumPy 1.26.4, en_core_web_lg 3.7.1,
spacy-lookups-data 1.0.5, Pyphen 0.18.1 and NLTK 3.10.3. Runtime checks require
these versions and the audited resource hashes. The complete resolution is in
`requirements-warm-start.lock`; tooling/transitive versions in that lock are
reproducibility evidence, not additional runtime version gates.

## Explicit setup

These steps are user-invoked and require network access. They are never executed
by examples, tests, package loading or extraction. Use a new destination, not an
existing environment you use for another project. Install Python 3.12.3 yourself
if needed. The commands assume this Python has `venv` and `pip` support.

Locate the installed lock and provenance in R:

```r
system.file("python", "requirements-warm-start.lock", package = "pairwiseLLM")
system.file("python", "audit-environment.json", package = "pairwiseLLM")
```

In a terminal, replace the paths with your own. On Linux/macOS:

```sh
/path/to/python3.12 -m venv /path/to/new/venv
/path/to/new/venv/bin/python -m pip install -r /path/to/requirements-warm-start.lock
```

On Windows, in PowerShell:

```powershell
& 'C:\path\to\Python312\python.exe' -m venv 'C:\path\to\new\venv'
& 'C:\path\to\new\venv\Scripts\python.exe' -m pip install -r 'C:\path\to\requirements-warm-start.lock'
```

CMUdict must be installed under this environment's `nltk_data/corpora` directory.
Start the new environment's Python interpreter and explicitly run the following
Python code to download and verify the audited archive before extracting it:

```python
import hashlib
import io
from pathlib import Path
import sys
import urllib.request
import zipfile

url = "https://raw.githubusercontent.com/nltk/nltk_data/gh-pages/packages/corpora/cmudict.zip"
with urllib.request.urlopen(url) as response:
    content = response.read()
expected = "d07cca47fd72ad32ea9d8ad1219f85301eeaf4568f8b6b73747506a71fb5afd6"
if hashlib.sha256(content).hexdigest() != expected:
    raise RuntimeError("CMUdict changed; do not accept it as writing_features_v1")
corpora = Path(sys.prefix) / "nltk_data/corpora"
corpora.mkdir(parents=True, exist_ok=True)
with zipfile.ZipFile(io.BytesIO(content)) as archive:
    for name in archive.namelist():
        path = Path(name)
        if path.is_absolute() or ".." in path.parts or path.parts[0] != "cmudict":
            raise RuntimeError("Unexpected archive entry")
    archive.extractall(corpora)
(corpora / "cmudict.zip").write_bytes(content)
```

Retain both the archive and extracted corpus: runtime checks verify both. No
third-party resources or binaries are distributed inside the R package.

## Select and check the environment

Install the optional R package explicitly if needed with
`install.packages("reticulate")`. In a fresh R session:

```r
library(pairwiseLLM)
python <- "/path/to/new/venv/bin/python"
# Windows: python <- "C:/path/to/new/venv/Scripts/python.exe"
status <- warm_start_python_status(python = python)
status[c("available", "problems")]
features <- extract_warm_start_features(
  ids = c("a", "b"), texts = c("The cat sat down.", "It was sunny outside."),
  python = python
)
```

Alternatively select an existing environment with
`reticulate::use_virtualenv(..., required = TRUE)` or
`reticulate::use_condaenv(..., required = TRUE)`, then omit `python`.
`RETICULATE_PYTHON` must agree with an explicit `python` argument. Automatic
managed environments are disabled for these calls. If another interpreter is
already initialized, restart R before selecting this environment.

Reticulate's environment controls are documented at
<https://pkgs.rstudio.com/reticulate/articles/versions.html>.

The environment has been tested on Linux x86_64 only. The path conventions above
accommodate Windows and macOS; they are not evidence that this pinned stack has
been validated there. An installation or compatibility failure should be reported
with `warm_start_python_status()` output. Do not relax version/resource checks
or substitute another spaCy model to work around it.

## Contract and provenance

The output is `item_id` plus the 20 ordered canonical numeric columns, with a
`warm_start_schema` attribute. IDs are character strings. Text is unchanged.
Document-level undefined values stay missing; no features are imputed. In
particular, entropy divides by all spaCy tokens rather than filtered `n_tokens`.
Zero-vector coherence retains upstream values and warnings.

`audit-environment.json` and `requirements-warm-start.lock` are exact copies of
Task 01's `audit-environment.json` and `requirements-audit.lock` from
`data-raw/warm-start/`. Packaged runtime code does not read that development
folder. The frozen schema remains in `warm-start/feature-schema-writing-v1.csv`.
Ten synthetic golden cases were promoted from Task 01's `audit-values.json` to
`tests/testthat/fixtures/warm-start-features/golden.json`, with named columns,
relative tolerance 1e-7 and absolute tolerance 1e-10. These resources establish
feature reproducibility, not predictive validity.

For optional package integration tests, explicitly set `PAIRWISELLM_TEST_PYTHON`
to the provisioned interpreter before running the focused warm-start tests. Tests
never install dependencies. The fixture R tests run without Python; the standalone
`test_backend.py` in the fixture directory accepts the installed Python module
directory as its sole argument and blocks network calls.
