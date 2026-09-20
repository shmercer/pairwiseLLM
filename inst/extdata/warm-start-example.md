# Offline warm-start example

`warm-start-example.rds` contains 40 fabricated training texts, eight new fabricated
texts, their extracted `writing_features_v2` tables, invented training outcomes,
and compact extraction provenance. No student data or fitted predictor is included.
The outcomes are not BTL estimates and the example does not establish predictive validity.

The source script `data-raw/warm-start/create-vignette-example.R` documents every
text and the invented outcome rule. Run it from the package source root with an
existing pinned interpreter and a new output path:

```sh
Rscript --vanilla data-raw/warm-start/create-vignette-example.R /path/to/venv/bin/python /tmp/warm-start-example.rds
```

The script refuses overwrite and installs nothing. Original v1/v2 audit fixtures
are separate and immutable. Extraction used Python 3.12.3, spaCy 3.7.5,
TextDescriptives 2.8.4, textstat 0.7.13, and the existing audited English resources
on Linux x86_64. V2 schema SHA-256:
`d9f271abae50eeac6d06a9ab7304309932174d54193ba8fb26348e9e886b2492`.

Both feature tables retain their schema attribute and original column/ID order.
The guide can use these tables without Python; repeating extraction is optional.
