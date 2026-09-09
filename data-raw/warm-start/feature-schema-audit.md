# writing_features_v1: feature and environment audit

Audit date: 2026-09-07. Package: pairwiseLLM 1.3.1, branch `warm_start`.
This is Task 01 development evidence, excluded from the R source package by
the existing `^data-raw$` build rule. No training outcomes, outcome correlations,
or model performance were used to choose features.

## Contract and curation

The installed contract is `inst/warm-start/feature-schema-writing-v1.csv`.
`warm_start_feature_schema()` returns that metadata without Python. The audit
inventory has 116 candidate rows: 19 unchanged upstream outputs, one explicitly
derived output, and 96 exclusions. It includes all public textstat measurement
methods, relevant intermediate methods, and non-English methods labeled out of
scope. The TextDescriptives inventory covers all seven components, every default
coarse POS category, and the alternative fine-grained POS configuration. Quality
thresholds and configurable strings are configuration, not additional predictors.

The 20 retained predictors cover nine families:

| Family | Count |
|---|---:|
| Length/productivity | 1 |
| Lexical diversity | 1 |
| Lexical/surface complexity | 2 |
| Sentence/syntactic complexity | 2 |
| POS composition | 8 |
| Dependency characteristics | 3 |
| Information theory | 1 |
| Semantic coherence | 1 |
| Readability | 1 |

`feature-inventory.csv` records each decision and its retained representative,
or explicitly states that a construct has no retained substitute. Curation favors
distinct constructs and location/spread summaries, not a collection of formula
variants. Excluding a feature does not claim it has no predictive value.

Major exclusions and distinctions:

- Unique-token count is determined by retained token count and type-token ratio
  for positive counts. Sentence count is recoverable from token count and mean
  sentence length where the denominator is positive.
- TextDescriptives ARI is exactly a linear combination of retained mean token
  length and mean sentence length. Other readability formulas reuse surface
  length, syllable, or long-word ingredients. Flesch reading ease and Flesch–Kincaid
  share ingredients but are not affine transforms of each other alone.
- textstat `avg_sentence_length` and `words_per_sentence` are exact aliases under
  the same instance settings; `reading_time` rescales character count. No pair
  of cross-package same-named metrics was established as universally identical:
  tokenization, syllabification, sentence rules, and edge cases differ.
- Retain one familiar-word readability measure, textstat's original
  `dale_chall_readability_score`. Its `_v2` alternative uses a different difficult-word
  threshold and correction condition. Do not substitute it by name similarity.
- Medians, syllable summaries, remaining POS categories, second-order coherence,
  and corpus-cleaning quality heuristics are excluded with reasons in the inventory.
  The eight retained POS shares do not form a complete closed composition.

## Approved derived information-theory feature

The user approved replacing total entropy with `upstream_entropy_per_token`:

```text
H = -sum(exp(token.prob) * token.prob for token in doc)
upstream_entropy_per_token = H / len(doc), or NA when len(doc) == 0
```

All spaCy tokens contribute to numerator and denominator, including punctuation
and whitespace tokens. Do not divide by the filtered `n_tokens` feature. There
is no extra log-base conversion or document probability renormalization.

This quantity is an average of upstream probability-weighted contributions, not
normalized document Shannon entropy, mean surprisal, or language-model cross-entropy.
Duplicating a fixed token sequence leaves the average unchanged. This removes
direct additive scaling with length; it does not establish predictive superiority.
Both perplexity outputs are excluded. In particular, upstream
`per_word_perplexity = exp(H) / len(doc)` divides after exponentiation.

The audited model initially has `lexeme_norm`, and the information-theory component
adds the English `lexeme_prob` table from spacy-lookups-data. spaCy 3.7.5 reads that
table through `token.prob`. The default model has no `lexeme_settings` table, so
the source fallback for unknown forms is -20.0. Keep that configuration fixed:
do not add a custom OOV setting or probability corpus under v1.

The real audit verified different probabilities for `the`, `cat`, and
`sophistication`, plus the -20 fallback for `qzxvzzq`. Missing probability tables
must become an actionable environment failure in Task 02, not an all-missing or
constant feature silently accepted for prediction.

## Upstream details frozen into metadata

- TextDescriptives filters tokens using `not token.is_punct` and absence of an
  ASCII apostrophe anywhere in `token.text`. The source docstring describes a
  narrower apostrophe rule; the implementation governs. Whitespace tokens remain.
  Curly apostrophes are not interchangeable with ASCII apostrophes in this rule.
- POS proportions divide by every spaCy token, including SPACE tokens even though
  SPACE is not in the upstream coarse-tag output list. Missing a category gives
  zero for an annotated nonempty document; missing annotations are a different error.
- Standard deviations use NumPy population SD (`ddof=0`). Dependency summaries
  average sentence-level values equally and include ROOT distance zero and punctuation.
- Coherence uses static sentence vectors from the pinned large English model.
  Fewer than two sentences gives missing first-order coherence. Zero-vector pairs
  may yield zero with W008 warnings; identical token sequences can yield one.
  Preserve these upstream values and document the warning, rather than redefining
  coherence. Absent model vectors are an environment incompatibility.
- Empty text has token count zero, missing normalized entropy, and textstat
  Dale–Chall zero. Punctuation-only and whitespace-only texts have different
  upstream outputs, captured in `audit-values.json`. Do not silently trim or clean
  inputs before calculating features.
- textstat uses its own word/sentence rules, not spaCy's. Its sentence counter
  discounts segments of two words or fewer and returns at least one for nonempty
  input. Dale–Chall counts difficult occurrences at syllable threshold zero,
  unlike the public `difficult_words()` defaults (unique words, threshold two).
  Inflections and proper names get no special familiar-word exemption.
- Although Dale–Chall's threshold is zero, its helper still calls the syllable
  counter. textstat can attempt an implicit NLTK CMUdict download. Task 02 must
  check resources before invoking textstat. Provisioning belongs only in explicit
  setup, and audit/test execution blocks downloads and network connections.

## Local environment and reproduction

The tested environment is `/home/sterett/.virtualenvs/pairwisellm-writing-v1`.
It is outside the Dropbox repository and must never be committed or bundled.
The same lock was installed independently at `/tmp/pairwisellm-writing-v1-recreated`
to verify reproducibility on this machine. This is a Linux x86_64 validation,
not a claim of tested Windows/macOS compatibility.

| Dependency | Locally exercised version |
|---|---|
| Python | 3.12.3 |
| TextDescriptives | 2.8.4 |
| textstat | 0.7.13 |
| spaCy / Thinc | 3.7.5 / 8.2.5 |
| NumPy | 1.26.4 |
| spaCy model | en_core_web_lg 3.7.1 |
| spacy-lookups-data | 1.0.5 |
| Pyphen | 0.18.1 |
| NLTK | 3.10.3 |

`requirements-audit.in` records direct inputs; `requirements-audit.lock` records
the complete installed resolution, including pip and the model wheel hash.
`audit-environment.json` records interpreter/platform, dependency versions,
audited Python source hashes, model/resource hashes, and the frozen schema hash.
Only hashes of third-party resource contents are retained in the repository.

To create a **new** environment, run from the repository root:

```sh
python3 data-raw/warm-start/setup_audit_venv.py "$HOME/.virtualenvs/pairwisellm-writing-v1-new"
```

This explicit command prints what will be installed, downloads dependencies and
the approximately 588 MB model wheel, and provisions CMUdict inside the venv. It
refuses existing destinations and requires Python 3.12.3. The system Python on
the audit machine lacked `ensurepip`; the script creates the venv without pip
and uses the official PyPA bootstrap with a verified SHA-256 checksum. Resource
hash changes fail rather than silently accepting new content. No system packages
are installed or modified. If setup is interrupted, use a new destination or
inspect and explicitly remove the incomplete environment yourself.

Run offline audit checks using the selected interpreter directly; activation is
unnecessary:

```sh
"$HOME/.virtualenvs/pairwisellm-writing-v1/bin/python" -m pip check
"$HOME/.virtualenvs/pairwisellm-writing-v1/bin/python" data-raw/warm-start/audit_feature_schema.py
```

The audit restricts NLTK lookups to the venv's `nltk_data` directory, blocks
`nltk.download` and socket connections, verifies capabilities and formulas, and
compares output to fixed evidence (relative tolerance 1e-7, absolute 1e-10).
Explicit `--record` writes development evidence; it is never run by package tests
and must not be used to approve an unexplained change. For future R integration
tests, pass this interpreter explicitly using Task 02's eventual environment
controls; no public Python bridge or extraction API is introduced in Task 01.

Core R schema tests remain offline and independent of this environment. The
standalone maintainer audit requires the pinned environment and reports missing
dependencies; it is not automatically invoked by the R test suite.

## Sources audited versus versions tested

The versions in the table were both source-audited and locally exercised.
Planning also inspected spaCy 3.8 model metadata and current package metadata;
those versions were not exercised and are not part of the v1 tested baseline.
The 3.7 spaCy/Thinc baseline accommodates TextDescriptives' NumPy <2 constraint.
Source-file hashes bind the audit to installed release implementations, rather
than relying only on documentation pages that may change.

- [TextDescriptives entropy source](https://hlasse.github.io/TextDescriptives/_modules/textdescriptives/components/information_theory.html)
- [textstat 0.7.13 release](https://pypi.org/project/textstat/0.7.13/)
- [English model 3.7.1 metadata](https://github.com/explosion/spacy-models/blob/master/meta/en_core_web_lg-3.7.1.json)
- [spaCy 3.7.5 release](https://pypi.org/project/spacy/3.7.5/)
- [spaCy lookup resources](https://pypi.org/project/spacy-lookups-data/1.0.5/)
- [Python venv documentation](https://docs.python.org/3/library/venv.html)

## Introducing writing_features_v2

Never regenerate v1 from whichever upstream packages happen to be installed.
Changes in canonical names, order, mapping, preprocessing of text, formulas,
model/resources, or undefined-value policy require explicit compatibility review.
Any change that alters the feature contract must get a new schema identifier and
artifact, while leaving v1 available for existing models. Carry forward an audit,
new pinned environment, deterministic fixtures, and migration notes. Do not
rename old models' schema metadata to make them appear compatible.
