"""Frozen writing_features_v1 extraction. Importing this module does not load the stack.

Only request_json is called by R. Feature payloads contain IDs and canonical fields;
version diagnostics and warnings live in the enclosing protocol, not feature columns.
"""

from contextlib import contextmanager
import hashlib
import importlib.metadata as metadata
import json
import math
from pathlib import Path
import platform
import socket
import sys
from unittest.mock import patch
import warnings


class FeatureError(Exception):
    """An actionable compatibility or input error."""


def artifact(name):
    return Path(__file__).resolve().parent / name


def require(condition, message):
    if not condition:
        raise FeatureError(message)


def check_versions(expected):
    observed = {"python": platform.python_version()}
    require(observed["python"] == expected["python"],
            f"Use Python {expected['python']}; found {observed['python']}.")
    for name in ("textdescriptives", "textstat", "spacy", "thinc", "numpy",
                 "en-core-web-lg", "spacy-lookups-data", "pyphen", "nltk"):
        try:
            version = metadata.version(name)
        except metadata.PackageNotFoundError:
            raise FeatureError(f"Missing {name}; explicitly install the packaged environment lock.") from None
        observed[name] = version
        require(version == expected["packages"][name],
                f"Use {name}=={expected['packages'][name]}; found {version}.")
    return observed


def check_resources(expected):
    # Check contents before loading any corpus or invoking textstat. No downloads.
    for relative, checksum in expected["resource_sha256"].items():
        if relative.startswith("nltk_data/"):
            path = Path(sys.prefix) / relative
        else:
            package = relative.split("/")[0].replace("_", "-")
            path = Path(metadata.distribution(package).locate_file(relative))
        require(path.is_file(), f"Missing resource {relative}; follow the installed Python setup instructions.")
        digest = hashlib.sha256()
        with path.open("rb") as stream:
            for chunk in iter(lambda: stream.read(1024 * 1024), b""):
                digest.update(chunk)
        require(digest.hexdigest() == checksum,
                f"Incompatible resource {relative}; restore the audited resource from the environment lock.")


def no_download(*args, **kwargs):
    raise FeatureError("An implicit download was blocked; explicitly provision the audited local resources.")


@contextmanager
def offline_resources():
    import nltk
    from nltk.corpus.reader import CMUDictCorpusReader

    location = Path(sys.prefix) / "nltk_data"
    # Restore shared NLTK configuration on success and failure. An explicit reader
    # avoids reusing an already-loaded corpus from another directory in this session.
    with patch.object(nltk.data, "path", [str(location)]), \
            patch.object(nltk, "download", no_download), \
            patch.object(socket.socket, "connect", no_download), \
            patch.object(socket, "create_connection", no_download):
        nltk.data.find("corpora/cmudict")
        reader = CMUDictCorpusReader(str(location / "corpora/cmudict"), ["cmudict"])
        with patch.object(nltk.corpus, "cmudict", reader):
            yield


def check_annotations(doc):
    if len(doc):
        for annotation in ("SENT_START", "POS", "DEP"):
            require(doc.has_annotation(annotation, require_complete=True),
                    f"Missing complete {annotation} annotation; restore en_core_web_lg 3.7.1.")


def check_pipeline(nlp):
    require(nlp.meta.get("version") == "3.7.1" and nlp.meta.get("lang") == "en",
            "Use the audited English en_core_web_lg 3.7.1 model.")
    require(all(name in nlp.pipe_names for name in ("parser", "tagger", "attribute_ruler")),
            "Missing parser/tagger/attribute_ruler; restore the default model pipeline.")
    require(nlp.vocab.vectors_length == 300 and nlp.vocab.vectors.n_keys > 0,
            "Missing audited 300-dimensional static vectors; restore en_core_web_lg 3.7.1.")
    require(nlp.vocab.lookups.has_table("lexeme_prob"),
            "Missing English lexeme_prob; install spacy-lookups-data==1.0.5.")
    require(not nlp.vocab.lookups.has_table("lexeme_settings"),
            "Custom lexeme_settings are incompatible with writing_features_v1.")
    probabilities = [t.prob for t in nlp.make_doc("the cat sophistication qzxvzzq")]
    require(all(math.isfinite(p) for p in probabilities) and len(set(probabilities)) == 4
            and probabilities[-1] == -20.0 and probabilities[0] > probabilities[2],
            "Incompatible English lexeme probabilities or OOV fallback; restore the audited resources.")
    check_annotations(nlp("The cat sat on the mat. It was warm there."))


def build_pipeline():
    import spacy
    import textdescriptives  # noqa: F401 -- registers spaCy factories

    nlp = spacy.load("en_core_web_lg")
    for component in ("descriptive_stats", "pos_proportions", "dependency_distance",
                      "information_theory", "coherence"):
        nlp.add_pipe("textdescriptives/" + component)
    check_pipeline(nlp)
    return nlp


def document_values(doc, schema, scorer):
    check_annotations(doc)
    token_count = len(doc)
    sentence_count = len(list(doc.sents))
    filtered_count = doc._.descriptive_stats["n_tokens"]
    values = []
    for row in schema:
        name = row["feature"]
        if row["source_package"] == "textstat":
            value = scorer.dale_chall_readability_score(doc.text)
            undefined = False
        else:
            mapping = getattr(doc._, row["component"])
            require(row["upstream_field"] in mapping, f"Missing required upstream feature {name}.")
            value = mapping[row["upstream_field"]]
            if name == "upstream_entropy_per_token":
                value = value / token_count if token_count else float("nan")
            if name == "n_tokens":
                undefined = False
            elif name in ("proportion_unique_tokens", "token_length_mean", "token_length_std"):
                undefined = filtered_count == 0
            elif name in ("sentence_length_mean", "sentence_length_std"):
                undefined = sentence_count == 0
            elif name == "first_order_coherence":
                undefined = sentence_count < 2
            else:
                undefined = token_count == 0
        require(not math.isinf(value), f"Infinite value for {name}.")
        require(math.isnan(value) == undefined, f"Unexpected missingness for {name}.")
        values.append(None if undefined else float(value))
    return values


def run_request(request, expected, schema):
    observed = check_versions(expected)
    check_resources(expected)
    with offline_resources():
        nlp = build_pipeline()
        if request["operation"] == "status":
            return {"observed": observed, "python": sys.executable}
        from textstat.textstat import textstatistics
        scorer = textstatistics()
        scorer.set_lang("en_US")
        rows = []
        for item_id, text in zip(request["ids"], request["texts"]):
            try:
                rows.append([item_id] + document_values(nlp(text), schema, scorer))
            except FeatureError as exc:
                raise FeatureError(f"Item {item_id}: {exc}") from None
        return {"schema": request["schema"], "columns": ["item_id"] + [r["feature"] for r in schema],
                "rows": rows}


def request_json(payload):
    """Return strict JSON, including actionable errors instead of Python tracebacks."""
    import csv

    caught = []
    try:
        request = json.loads(payload)
        require(request.get("schema") == "writing_features_v1", "Unknown feature schema.")
        require(request.get("operation") in ("status", "extract"), "Unknown extraction operation.")
        if request["operation"] == "extract":
            ids, texts = request.get("ids"), request.get("texts")
            require(isinstance(ids, list) and isinstance(texts, list) and len(ids) > 0
                    and len(ids) == len(texts), "IDs and texts must be nonempty, length-matched lists.")
            require(all(isinstance(x, str) and x.strip() for x in ids)
                    and len(set(ids)) == len(ids), "IDs must be unique nonblank strings.")
            require(all(isinstance(x, str) for x in texts), "Texts must be strings without missing values.")
        expected = json.loads(artifact("audit-environment.json").read_text(encoding="utf-8"))
        schema_path = artifact("../warm-start/feature-schema-writing-v1.csv")
        require(hashlib.sha256(schema_path.read_bytes()).hexdigest() == expected["schema_sha256"],
                "Installed feature schema differs from the frozen audit; reinstall pairwiseLLM.")
        with schema_path.open(encoding="utf-8") as stream:
            schema = list(csv.DictReader(stream))
        with warnings.catch_warnings(record=True) as caught:
            warnings.simplefilter("always")
            result = run_request(request, expected, schema)
        return json.dumps({"ok": True, "result": result,
                           "warnings": list(dict.fromkeys(str(w.message) for w in caught))}, allow_nan=False)
    except Exception as exc:
        message = str(exc) if isinstance(exc, FeatureError) else (
            f"Feature environment failed ({type(exc).__name__}); check the pinned packages and local resources."
        )
        return json.dumps({"ok": False, "error": message}, allow_nan=False)
