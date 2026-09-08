"""Offline tests for the installed Python module. Pass the installed module directory.

Usage: python test_backend.py /path/to/pairwiseLLM/python
No dependency installation or network access is performed.
"""
import csv
import hashlib
import importlib.metadata as metadata
import json
from pathlib import Path
import socket
import sys
import tempfile
import unittest
from unittest.mock import patch, MagicMock
import warnings

sys.path.insert(0, sys.argv.pop(1))
import pairwisellm_warm_start as backend

HERE = Path(__file__).resolve().parent
GOLDEN = json.loads((HERE / "golden.json").read_text(encoding="utf-8"))


class BackendTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.expected = json.loads(backend.artifact("audit-environment.json").read_text())
        backend.check_versions(cls.expected)
        backend.check_resources(cls.expected)
        with backend.offline_resources():
            cls.nlp = backend.build_pipeline()
        with backend.artifact("../warm-start/feature-schema-writing-v1.csv").open() as stream:
            cls.schema = list(csv.DictReader(stream))

    def request(self, **changes):
        request = dict(operation="extract", schema=GOLDEN["schema"],
                       ids=[row[0] for row in GOLDEN["rows"]], texts=GOLDEN["texts"])
        request.update(changes)
        return json.loads(backend.request_json(json.dumps(request)))

    def test_golden_and_repeat(self):
        first = self.request()
        self.assertTrue(first["ok"], first)
        result = first["result"]
        self.assertEqual(result["columns"], GOLDEN["columns"])
        for actual, expected in zip(result["rows"], GOLDEN["rows"]):
            self.assertEqual(actual[0], expected[0])
            for a, e in zip(actual[1:], expected[1:]):
                if e is None:
                    self.assertIsNone(a)
                else:
                    self.assertLessEqual(abs(a - e), 1e-10 + 1e-7 * abs(e))
        self.assertEqual(self.request()["result"], result)

    def test_status_and_input_errors(self):
        status = self.request(operation="status")
        self.assertTrue(status["ok"], status)
        self.assertEqual(status["result"]["observed"]["python"], "3.12.3")
        for changes in [dict(schema="v2"), dict(operation="wrong"), dict(ids=[]),
                        dict(ids=["a", "a"], texts=["", ""]), dict(ids=[" "], texts=[""]),
                        dict(ids=[1], texts=[""]), dict(ids=["a"], texts=[None])]:
            self.assertFalse(self.request(**changes)["ok"])
        self.assertFalse(json.loads(backend.request_json("bad json"))["ok"])

    def test_version_errors(self):
        with patch.object(backend.platform, "python_version", return_value="0.0.0"):
            self.assertIn("Use Python", self.request()["error"])
        with patch.object(backend.metadata, "version", return_value="0.0.0"):
            self.assertIn("Use textdescriptives", self.request()["error"])
        with patch.object(backend.metadata, "version", side_effect=metadata.PackageNotFoundError):
            self.assertIn("Missing textdescriptives", self.request()["error"])

    def test_missing_and_changed_resources(self):
        for relative in self.expected["resource_sha256"]:
            with self.subTest(resource=relative), tempfile.TemporaryDirectory() as directory:
                missing = Path(directory) / "missing"
                distribution = MagicMock()
                distribution.locate_file.return_value = missing
                expected = {"resource_sha256": {relative: self.expected["resource_sha256"][relative]}}
                with patch.object(backend.sys, "prefix", directory), \
                        patch.object(backend.metadata, "distribution", return_value=distribution):
                    with self.assertRaisesRegex(backend.FeatureError, "Missing resource"):
                        backend.check_resources(expected)
                    path = Path(directory) / relative if relative.startswith("nltk_data/") else missing
                    path.parent.mkdir(parents=True, exist_ok=True)
                    path.write_bytes(b"incompatible")
                    with self.assertRaisesRegex(backend.FeatureError, "Incompatible resource"):
                        backend.check_resources(expected)

    def test_pipeline_failures(self):
        for modify, message in [
            (lambda n: setattr(n, "meta", {}), "audited English"),
            (lambda n: setattr(n, "pipe_names", []), "parser/tagger"),
            (lambda n: setattr(n.vocab, "vectors_length", 0), "static vectors"),
            (lambda n: setattr(n.vocab.lookups, "has_table", lambda _: False), "lexeme_prob"),
            (lambda n: setattr(n.vocab.lookups, "has_table", lambda _: True), "lexeme_settings"),
            (lambda n: setattr(n, "make_doc", lambda _: []), "probabilities")
        ]:
            fake = MagicMock(wraps=self.nlp)
            # Explicit attributes avoid MagicMock wrappers around scalar comparisons.
            fake.meta = self.nlp.meta
            fake.pipe_names = self.nlp.pipe_names
            fake.vocab.vectors_length = 300
            fake.vocab.vectors.n_keys = 1
            fake.vocab.lookups.has_table = lambda name: name == "lexeme_prob"
            modify(fake)
            with self.assertRaisesRegex(backend.FeatureError, message):
                backend.check_pipeline(fake)
        for annotation in ("SENT_START", "POS", "DEP"):
            doc = MagicMock()
            doc.__len__.return_value = 1
            doc.has_annotation.side_effect = lambda name, **kw: name != annotation
            with self.assertRaisesRegex(backend.FeatureError, annotation):
                backend.check_annotations(doc)

    def test_download_guard_and_restoration(self):
        import nltk
        previous_path = nltk.data.path
        previous_download = nltk.download
        previous_corpus = nltk.corpus.cmudict
        with self.assertRaisesRegex(backend.FeatureError, "implicit download"):
            with backend.offline_resources():
                nltk.download("cmudict")
        self.assertIs(nltk.data.path, previous_path)
        self.assertIs(nltk.download, previous_download)
        self.assertIs(nltk.corpus.cmudict, previous_corpus)
        with backend.offline_resources():
            with self.assertRaisesRegex(backend.FeatureError, "implicit download"):
                socket.create_connection(("example.invalid", 443))

    def test_preflight_precedes_pipeline_and_scorer(self):
        with patch.object(backend, "check_resources", side_effect=backend.FeatureError("Missing CMUdict")), \
                patch.object(backend, "build_pipeline") as build:
            self.assertIn("Missing CMUdict", self.request()["error"])
            build.assert_not_called()
        with patch.object(backend, "build_pipeline", side_effect=RuntimeError("private traceback")):
            result = self.request()
            self.assertFalse(result["ok"])
            self.assertNotIn("private traceback", result["error"])

    def test_document_missingness_and_missing_fields(self):
        from textstat.textstat import textstatistics
        scorer = textstatistics()
        scorer.set_lang("en_US")
        doc = self.nlp("!!!")
        with backend.offline_resources():
            values = backend.document_values(doc, self.schema, scorer)
            self.assertEqual(values[0], 0)
            self.assertIsNotNone(values[17])
            bad_schema = [dict(self.schema[0], upstream_field="not_a_field")]
            with self.assertRaisesRegex(backend.FeatureError, "Missing required upstream"):
                backend.document_values(doc, bad_schema, scorer)
            with patch.object(scorer, "dale_chall_readability_score", return_value=float("inf")):
                with self.assertRaisesRegex(backend.FeatureError, "Infinite"):
                    backend.document_values(doc, self.schema, scorer)
            with patch.object(scorer, "dale_chall_readability_score", return_value=float("nan")):
                with self.assertRaisesRegex(backend.FeatureError, "Unexpected missingness"):
                    backend.document_values(doc, self.schema, scorer)
        with patch.object(backend, "document_values", side_effect=backend.FeatureError("bad feature")):
            self.assertIn("Item empty: bad feature", self.request()["error"])

    def test_corrupt_schema(self):
        with patch.object(backend.hashlib, "sha256") as digest:
            digest.return_value.hexdigest.return_value = "bad"
            self.assertIn("frozen audit", self.request()["error"])


if __name__ == "__main__":
    with patch.object(socket.socket, "connect", side_effect=AssertionError("Network attempted")), \
            patch.object(socket, "create_connection", side_effect=AssertionError("Network attempted")), \
            warnings.catch_warnings():
        warnings.simplefilter("ignore")
        unittest.main(verbosity=2)
