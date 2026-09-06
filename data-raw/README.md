# Example-data provenance

`example_writing_samples.R` creates the four small synthetic fixtures shipped
in `data/`. It sets a fixed seed before simulating pair outcomes.

`generate_example_writing_samples1000.R` records the prompt, model metadata,
filtering, labeling, and save path used for the 1,000-row synthetic writing
dataset. Running it is optional and makes live, billable OpenAI API requests;
provider nondeterminism means it is a reproducible source path, not a promise of
byte-identical regeneration.

These scripts are package source and are not run during installation, build,
examples, tests, or vignette rendering.
