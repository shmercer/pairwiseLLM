# writing_features_v2 source audit

Phase 3 of issue #259, package 1.5.1. This schema was materialized before
extraction consumers. Selection uses the approved source-only 46-feature list
in issue-259-design.md; no study files, outcomes, correlations or fits were read.

`inst/warm-start/feature-schema-writing-v2.csv` contains all 20 v1 rows first
(only the schema identifier changes), followed by 26 additions in original
inventory order. `feature-inventory-v2.csv` re-audits all 116 original candidates,
with explicit retained positions, exclusions, formulas and conditional domains.
The schema includes definitions, defaults, units, requirements and missingness
for every retained field. Byte identities are in the installed
`inst/python/schema-writing-v2.json`; CSVs use LF on every checkout platform.

The environment, 73 upstream source hashes and resource hashes remain those in
the immutable v1 `audit-environment.json`. All 73 local source hashes were
verified before this schema commit. The v2 manifest additionally pins the
textstat public wrapper, whose defaults distinguish unique difficult words from
readability occurrence counts. The pinned sources are TextDescriptives 2.8.4,
textstat 0.7.13, spaCy 3.7.5, en_core_web_lg 3.7.1, and Pyphen 0.18.1.
No dependency or original audit/fixture file changes are required.

Representative policy: preserve v1; retain eligible primitive scalar summaries;
exclude algebraic/conditional restatements against the final retained set;
prefer wider defined domains, then original inventory order. Conceptual overlap
is intentional. Quality filters, utilities, lists, predicates, non-English
measures, fine-grained POS and prompt/topic/format/annotation diagnostics stay
excluded. Closed-class AUX, DET and PART are included.

Important source distinctions:

- TD character count removes literal spaces, while textstat removes regex
  whitespace. Neither can replace the other.
- TD syllables count hyphens in Pyphen's inserted **lowercased** form, including
  pre-existing hyphens. This is not simply the number of insertion positions.
- TD Fog/LIX are undefined when filtered token count is zero. Sentence median
  depends on sentence existence, and second-order coherence needs three sentences.
  Empty character counts remain zero. Preserve valid zeros, negatives and NA.
- textstat difficult_words defaults to unique words and threshold two. Spache
  and Dale–Chall v2 use threshold-two occurrences; Fog uses threshold-three
  occurrences plus the easy-word list, so raw polysyllabcount cannot replace it.
- Linsear–Write uses strict_lower=False and strict_upper=True (first 100 words).
  Short/wordless inputs can yield valid negative scores; never clamp them.
- Spache precedes Dale–Chall v2 and, with word/sentence counts, recovers its
  thresholded formula. TD Fog covers a wider domain than SMOG; TD LIX precedes RIX.
- textstat characters-per-word and ARI use a punctuation-preserving denominator.
  Their exclusions are conditional on that word count equaling retained lexicon
  count; punctuation-only segments break equivalence. The inventory explicitly
  records this condition, including its effect on text_standard.
- The inherited perplexity exclusions transform total entropy, requiring the
  unretained all-token denominator. They are not claimed to be exact functions
  of retained normalized entropy alone. The approved membership is preserved.

This is a definition audit, not evidence of predictive validity. Existing v1
schema, inventory, locks, environment/value manifests and golden/legacy fixtures
remain frozen. Phase 3 extraction checks will be recorded in the phase handoff.
