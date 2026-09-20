# Synthetic v2 extraction evidence

`golden.json` was captured directly from pinned upstream getters by
`data-raw/warm-start/audit_feature_schema_v2.py --record` on Linux x86_64,
Python 3.12.3, using the unchanged audited v1 environment. The script refuses
overwrite. It does not use package extraction to generate expected values.

The 16 synthetic cases include the ten immutable v1 texts plus three sentences,
mixed whitespace, hyphens, repeated case variants, punctuation-only segments
and a text longer than 100 words. No study data or outcomes were used.
CSV identity is stored in the fixture; compare numeric values with relative
tolerance 1e-7 and absolute 1e-10, while preserving missingness exactly.
The audit additionally compares explicit formulas/defaults and repeated
extraction, and verifies that the first 20 extracted features equal v1.

Do not refresh evidence to accommodate unexplained source or resource changes.
The original v1 golden values and legacy model fixtures remain untouched.
