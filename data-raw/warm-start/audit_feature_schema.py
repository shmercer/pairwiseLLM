"""Offline upstream audit, not a public extractor. See feature-schema-audit.md.

Run without arguments to check committed evidence. --record explicitly refreshes
development evidence after review; never use it to bless a dependency upgrade.
"""

import argparse
import csv
import hashlib
import importlib.metadata as metadata
import json
import math
from pathlib import Path
import platform
import socket
import sys
import unittest
import warnings
from unittest.mock import patch

import nltk
import numpy as np
import spacy
from spacy.tokens import Doc
import textdescriptives
from textdescriptives.components.information_theory import entropy_getter
from textdescriptives.components.utils import filter_tokens
from textstat.textstat import textstatistics


HERE = Path(__file__).resolve().parent
SCHEMA_PATH = HERE.parents[1] / "inst/warm-start/feature-schema-writing-v1.csv"
COMPONENTS = [
    "descriptive_stats", "pos_proportions", "dependency_distance",
    "information_theory", "coherence",
]
TEXTS = {
    "empty": "",
    "space": " ",
    "punctuation": "!!!",
    "single": "Hello.",
    "short": "The cat sat on the mat. It was warm there.",
    "contractions": "I can't go.\n\nThey won't leave!",
    "curly_apostrophe": "I can’t go, but she can.",
    "unfamiliar": "qzxvzzq flarblezz",
    "zero_vectors": "qzxvzzq\nflarblezz",
    "long": (
        "Students planted trees beside the school. They measured each tree every week. "
        "Although the weather was dry, the students watered the soil carefully. "
        "Their observations helped explain why some trees grew faster than others. "
    ) * 6,
}


def no_network(*args, **kwargs):
    raise AssertionError("Network access or an implicit download was attempted during the audit")


def sha256(path):
    digest = hashlib.sha256()
    with Path(path).open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def schema_values(doc, schema, scorer):
    """Read upstream values directly for development evidence only."""
    values = []
    for row in schema:
        if row["source_package"] == "textstat":
            value = scorer.dale_chall_readability_score(doc.text)
        else:
            value = getattr(doc._, row["component"])[row["upstream_field"]]
            if row["feature"] == "upstream_entropy_per_token":
                value = value / len(doc) if len(doc) else np.nan
        values.append(float(value) if math.isfinite(value) else None)
    return values


def provenance():
    packages = sorted(
        (d.metadata["Name"].lower().replace("_", "-"), d.version)
        for d in metadata.distributions()
    )
    sources = {}
    for package, relative in [("textdescriptives", "components"), ("textstat", "backend")]:
        root = Path(metadata.distribution(package).locate_file(package))
        for path in sorted((root / relative).rglob("*.py")):
            sources[package + "/" + str(path.relative_to(root))] = sha256(path)
    spacy_root = Path(spacy.__file__).parent
    sources["spacy/lexeme.pyx"] = sha256(spacy_root / "lexeme.pyx")
    sources["spacy/tokens/span.pyx"] = sha256(spacy_root / "tokens/span.pyx")
    resources = {}
    for package, pattern in [
        ("spacy-lookups-data", "spacy_lookups_data/data/en_lexeme_prob.json.gz"),
        ("textstat", "textstat/resources/en/easy_words.txt"),
        ("pyphen", "pyphen/dictionaries/hyph_en_*.dic"),
        ("en-core-web-lg", "en_core_web_lg/en_core_web_lg-3.7.1/**/*"),
    ]:
        root = Path(metadata.distribution(package).locate_file(""))
        paths = [p for p in sorted(root.glob(pattern)) if p.is_file()]
        assert paths, (package, pattern)
        for path in paths:
            resources[str(path.relative_to(root))] = sha256(path)
    cmu = Path(sys.prefix) / "nltk_data/corpora/cmudict.zip"
    resources["nltk_data/corpora/cmudict.zip"] = sha256(cmu)
    for path in sorted(cmu.with_suffix("").rglob("*")):
        if path.is_file():
            resources[str(path.relative_to(sys.prefix))] = sha256(path)
    return {
        "python": platform.python_version(), "platform": platform.platform(),
        "packages": dict(packages), "source_sha256": sources,
        "resource_sha256": resources, "schema_sha256": sha256(SCHEMA_PATH),
        "inventory_sha256": sha256(HERE / "feature-inventory.csv"),
    }


class FeatureAudit(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        nltk.data.path[:] = [str(Path(sys.prefix) / "nltk_data")]
        nltk.data.find("corpora/cmudict")
        cls.nlp = spacy.load("en_core_web_lg")
        for component in COMPONENTS:
            cls.nlp.add_pipe("textdescriptives/" + component)
        cls.scorer = textstatistics()
        cls.scorer.set_lang("en_US")
        with SCHEMA_PATH.open() as stream:
            cls.schema = list(csv.DictReader(stream))
        cls.docs = {name: cls.nlp(text) for name, text in TEXTS.items()}

    def test_model_and_probability_capabilities(self):
        self.assertEqual(self.nlp.meta["version"], "3.7.1")
        self.assertEqual(self.nlp.vocab.vectors_length, 300)
        self.assertGreater(self.nlp.vocab.vectors.n_keys, 0)
        doc = self.docs["short"]
        for annotation in ["SENT_START", "POS", "DEP"]:
            self.assertTrue(doc.has_annotation(annotation, require_complete=True))
        self.assertTrue(self.nlp.vocab.lookups.has_table("lexeme_prob"))
        self.assertFalse(self.nlp.vocab.lookups.has_table("lexeme_settings"))
        words = self.nlp.make_doc("the cat sophistication qzxvzzq")
        probabilities = [t.prob for t in words]
        self.assertEqual(len(set(probabilities)), 4)
        self.assertTrue(all(np.isfinite(probabilities)))
        self.assertEqual(probabilities[-1], -20.0)
        self.assertGreater(probabilities[0], probabilities[2])

    def test_inventory_completeness_and_traceability(self):
        with (HERE / "feature-inventory.csv").open() as stream:
            inventory = list(csv.DictReader(stream))
        self.assertEqual(len(inventory), 116)
        self.assertTrue(all(all(value.strip() for value in row.values()) for row in inventory))
        keys = [(r["source_package"], r["component"], r["upstream_field"]) for r in inventory]
        self.assertEqual(len(keys), len(set(keys)))
        retained = [r for r in inventory if r["decision"].startswith("retained")]
        self.assertEqual([r["representative"] for r in retained], [r["feature"] for r in self.schema])
        for component in COMPONENTS:
            fields = set(getattr(self.docs["short"]._, component))
            inventoried = {r["upstream_field"] for r in inventory
                           if r["source_package"] == "textdescriptives" and r["component"] == component}
            self.assertTrue(fields <= inventoried, component)
        public_methods = {name for name in dir(textstatistics)
                          if not name.startswith("_") and not name.startswith("set_")
                          and callable(getattr(textstatistics, name))}
        inventoried = {r["upstream_field"] for r in inventory if r["source_package"] == "textstat"}
        self.assertEqual(public_methods, inventoried)

    def test_entropy_formula_denominator_and_duplication(self):
        for doc in self.docs.values():
            if len(doc):
                expected = np.mean([-np.exp(t.prob) * t.prob for t in doc])
                self.assertAlmostEqual(doc._.entropy / len(doc), expected, places=14)
                doubled = Doc(self.nlp.vocab, words=[t.text for t in doc] * 2)
                self.assertAlmostEqual(entropy_getter(doubled) / len(doubled), expected, places=14)
                self.assertAlmostEqual(doc._.per_word_perplexity, np.exp(doc._.entropy) / len(doc))
        self.assertIsNone(schema_values(self.docs["empty"], self.schema, self.scorer)[17])
        self.assertEqual(self.docs["punctuation"]._.descriptive_stats["n_tokens"], 0)
        self.assertIsNotNone(schema_values(self.docs["punctuation"], self.schema, self.scorer)[17])

    def test_filtered_counts_and_population_deviations(self):
        for doc in self.docs.values():
            selected = [t for t in doc if not t.is_punct and "'" not in t.text]
            self.assertEqual(selected, filter_tokens(doc))
            stats = doc._.descriptive_stats
            self.assertEqual(stats["n_tokens"], len(selected))
            if selected:
                self.assertAlmostEqual(stats["proportion_unique_tokens"],
                                       len({t.lower_ for t in selected}) / len(selected))
                self.assertAlmostEqual(stats["token_length_std"], np.std([len(t) for t in selected]))
            if len(doc):
                lengths = [len(filter_tokens(s)) for s in doc.sents]
                self.assertAlmostEqual(stats["sentence_length_mean"], np.mean(lengths))
                self.assertAlmostEqual(stats["sentence_length_std"], np.std(lengths))
        self.assertEqual(self.docs["space"]._.descriptive_stats["n_tokens"], 1)

    def test_pos_denominator_absence_and_empty(self):
        for doc in self.docs.values():
            for row in self.schema[6:14]:
                tag = row["upstream_field"].removeprefix("pos_prop_")
                actual = doc._.pos_proportions[row["upstream_field"]]
                if len(doc):
                    self.assertEqual(actual, sum(t.pos_ == tag for t in doc) / len(doc))
                else:
                    self.assertTrue(math.isnan(actual))
        self.assertEqual(self.docs["punctuation"]._.pos_proportions["pos_prop_NOUN"], 0)

    def test_dependency_sentence_weighting(self):
        for doc in self.docs.values():
            if not len(doc):
                self.assertTrue(all(math.isnan(v) for v in doc._.dependency_distance.values()))
                continue
            distances = [[0 if t.dep_ == "ROOT" else abs(t.i - t.head.i) for t in s]
                         for s in doc.sents]
            actual = doc._.dependency_distance
            self.assertAlmostEqual(actual["dependency_distance_mean"], np.mean([np.mean(s) for s in distances]))
            self.assertAlmostEqual(actual["dependency_distance_std"], np.std([np.mean(s) for s in distances]))
            self.assertAlmostEqual(actual["prop_adjacent_dependency_relation_mean"],
                                   np.mean([np.mean(np.array(s) == 1) for s in distances]))

    def test_coherence_undefined_and_vectors(self):
        self.assertTrue(math.isnan(self.docs["single"]._.coherence["first_order_coherence"]))
        doc = self.docs["short"]
        sentences = list(doc.sents)
        self.assertAlmostEqual(doc._.coherence["first_order_coherence"],
                               sentences[0].similarity(sentences[1]))
        # Explicit sentence boundaries allow zero-vector behavior to be audited
        # without assuming that the parser splits unfamiliar strings as desired.
        unknown = Doc(self.nlp.vocab, words=["qzxvzzq", "flarblezz"], sent_starts=[True, True])
        self.assertTrue(all(s.vector_norm == 0 for s in unknown.sents))
        with warnings.catch_warnings(record=True) as caught:
            warnings.simplefilter("always")
            self.nlp.get_pipe("textdescriptives/coherence")(unknown)
        self.assertEqual(unknown._.coherence["first_order_coherence"], 0)
        self.assertTrue(any("W008" in str(w.message) for w in caught))

    def test_textstat_defaults_and_short_inputs(self):
        self.assertEqual(self.scorer.dale_chall_readability_score(""), 0)
        self.assertEqual(self.scorer.dale_chall_readability_score("!!!"), 0)
        self.assertAlmostEqual(self.scorer.dale_chall_readability_score("Hello."), 0.0496)
        self.assertTrue(math.isfinite(self.scorer.dale_chall_readability_score(TEXTS["long"])))

    def test_repeated_values_and_committed_evidence(self):
        actual = {}
        for name, doc in self.docs.items():
            values = schema_values(doc, self.schema, self.scorer)
            repeated = schema_values(self.nlp(doc.text), self.schema, self.scorer)
            self.assertEqual(values, repeated)
            self.assertEqual(len(values), 20)
            actual[name] = {"text": doc.text, "values": values}
        self.__class__.values = actual
        if not RECORD:
            expected = json.loads((HERE / "audit-values.json").read_text())
            self.assertEqual(list(expected), list(actual))
            for name in actual:
                self.assertEqual(expected[name]["text"], actual[name]["text"])
                for old, new in zip(expected[name]["values"], actual[name]["values"]):
                    if old is None or new is None:
                        self.assertEqual(old, new)
                    else:
                        self.assertTrue(math.isclose(old, new, rel_tol=1e-7, abs_tol=1e-10))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--record", action="store_true")
    RECORD = parser.parse_args().record
    with patch("socket.create_connection", no_network), patch.object(socket.socket, "connect", no_network), \
            patch("nltk.download", no_network):
        suite = unittest.defaultTestLoader.loadTestsFromTestCase(FeatureAudit)
        result = unittest.TextTestRunner(verbosity=2).run(suite)
        if not result.wasSuccessful():
            sys.exit(1)
        evidence = provenance()
        if RECORD:
            (HERE / "audit-values.json").write_text(json.dumps(FeatureAudit.values, indent=2) + "\n")
            (HERE / "audit-environment.json").write_text(json.dumps(evidence, indent=2) + "\n")
        else:
            expected = json.loads((HERE / "audit-environment.json").read_text())
            for key in ["python", "packages", "source_sha256", "resource_sha256", "schema_sha256",
                        "inventory_sha256"]:
                if evidence[key] != expected[key]:
                    raise RuntimeError(f"Audit environment or frozen schema mismatch: {key}")
        print(f"PASS: {result.testsRun} offline upstream audit tests; "
              "schema, inventory, versions, sources, and resources verified")
