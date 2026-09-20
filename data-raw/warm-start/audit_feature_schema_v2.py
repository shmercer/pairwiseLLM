"""Offline v2 source/value audit on synthetic texts only; no installation.

Run with the pinned interpreter. --record creates (never overwrites) new v2
fixture evidence directly from upstream getters, independently of our extractor.
Ordinary execution checks hashes, formulas, missingness, protocol and repeats.
"""
import csv
import hashlib
import importlib.metadata as metadata
import json
import math
from pathlib import Path
import socket
import sys
import unittest
from unittest.mock import patch
import warnings

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "inst/python"))
import pairwisellm_warm_start as backend

HERE = Path(__file__).resolve().parent
FIXTURE = ROOT / "tests/testthat/fixtures/warm-start-features-v2/golden.json"
MANIFEST = json.loads(backend.artifact("schema-writing-v2.json").read_text())
LEGACY = json.loads((ROOT / "tests/testthat/fixtures/warm-start-features/golden.json").read_text())
TEXTS = dict(zip([r[0] for r in LEGACY["rows"]], LEGACY["texts"]))
TEXTS.update({
    "three_sentences": "The little cat slept on the mat. A dog ran around outside. The children played together.",
    "whitespace": "A\tbright\nstar shines.\n\nIt is far away.",
    "hyphens": "Well-known well-known ideas involve re-evaluation and co-operation.",
    "difficult_case": "Sophistication sophistication sophistication sophistication.",
    "punct_segments": "Words !!! are ??? here.",
    "first_100": ("A cat sat on the mat. " * 20) + ("Sophistication materializes unexpectedly. " * 10),
})


def sha(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def schema_rows(version):
    with (ROOT / f"inst/warm-start/feature-schema-writing-v{version}.csv").open() as stream:
        return list(csv.DictReader(stream))


def upstream_pipeline():
    # Independent of build_pipeline and document_values in the installed bridge.
    import spacy
    import textdescriptives  # noqa: F401
    nlp = spacy.load("en_core_web_lg")
    for component in ("descriptive_stats", "pos_proportions", "dependency_distance",
                      "information_theory", "coherence", "readability"):
        nlp.add_pipe("textdescriptives/" + component)
    return nlp


def upstream_values(doc, schema, scorer):
    values = []
    for row in schema:
        if row["source_package"] == "textstat":
            value = getattr(scorer, row["upstream_field"])(doc.text)
        else:
            value = getattr(doc._, row["component"])[row["upstream_field"]]
            if row["feature"] == "upstream_entropy_per_token":
                value = value / len(doc) if len(doc) else math.nan
        assert not math.isinf(value)
        values.append(None if math.isnan(value) else float(value))
    return values


class V2Audit(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        from textstat.textstat import textstatistics
        cls.expected = json.loads(backend.artifact("audit-environment.json").read_text())
        backend.check_versions(cls.expected)
        backend.check_resources(cls.expected)
        cls.schema = schema_rows(2)
        cls.nlp = upstream_pipeline()
        cls.docs = {key: cls.nlp(text) for key, text in TEXTS.items()}
        cls.scorer = textstatistics()
        cls.scorer.set_lang("en_US")
        cls.golden = json.loads(FIXTURE.read_text())

    def test_sources_inventory_and_schema(self):
        self.assertEqual(sha(ROOT / "inst/warm-start/feature-schema-writing-v2.csv"), MANIFEST["schema_sha256"])
        self.assertEqual(sha(HERE / "feature-inventory-v2.csv"), MANIFEST["inventory_sha256"])
        for relative, digest in {**self.expected["source_sha256"], **MANIFEST["source_sha256"]}.items():
            self.assertEqual(sha(metadata.distribution(relative.split('/')[0]).locate_file(relative)), digest, relative)
        self.assertEqual(self.schema[:20], [dict(r, schema="writing_features_v2") for r in schema_rows(1)])
        with (HERE / "feature-inventory-v2.csv").open() as stream:
            inventory = list(csv.DictReader(stream))
        self.assertEqual(len(inventory), 116)
        retained = [r for r in inventory if r["decision"].startswith("retained")]
        self.assertEqual([r["representative"] for r in retained], [r["feature"] for r in self.schema])
        self.assertEqual(len(retained), 46)
        self.assertEqual([r["schema_position"] for r in retained], [str(i) for i in range(1, 47)])

    def test_upstream_golden_and_missingness(self):
        with backend.offline_resources():
            for expected in self.golden["rows"]:
                doc = self.docs[expected[0]]
                values = upstream_values(doc, self.schema, self.scorer)
                actual = backend.document_values(doc, self.schema, self.scorer)
                self.assertEqual(actual, values)
                for a, e in zip(actual, expected[1:]):
                    if e is None:
                        self.assertIsNone(a)
                    else:
                        self.assertLessEqual(abs(a-e), 1e-10 + 1e-7*abs(e))
                x = dict(zip([r["feature"] for r in self.schema], actual))
                self.assertIsNotNone(x["n_characters"])
                for name in ("token_length_median", "syllables_per_token_mean",
                             "syllables_per_token_median", "syllables_per_token_std", "gunning_fog", "lix"):
                    self.assertEqual(x[name] is None, x["n_tokens"] == 0, name)
                self.assertEqual(x["sentence_length_median"] is None, len(list(doc.sents)) == 0)
                self.assertEqual(x["second_order_coherence"] is None, len(list(doc.sents)) < 3)

    def test_primitive_and_formula_oracles(self):
        import numpy as np
        from pyphen import Pyphen
        from textdescriptives.components.utils import filter_tokens
        from textstat.backend.counts import count_difficult_words
        with backend.offline_resources():
            for doc in self.docs.values():
                x = dict(zip([r["feature"] for r in self.schema], backend.document_values(doc, self.schema, self.scorer)))
                filtered = filter_tokens(doc)
                self.assertEqual(x['n_characters'], len(doc.text.replace(' ', '')))
                if filtered:
                    syllables = [max(1, Pyphen(lang='en').inserted(t.lower_).count('-')+1) for t in filtered]
                    self.assertEqual(x['syllables_per_token_mean'], np.mean(syllables))
                    self.assertEqual(x['syllables_per_token_median'], np.median(syllables))
                    self.assertEqual(x['syllables_per_token_std'], np.std(syllables))
                    a = x['sentence_length_mean']
                    self.assertAlmostEqual(x['gunning_fog'], .4*(a+100*sum(s>=3 for s in syllables)/len(filtered)), places=12)
                    self.assertAlmostEqual(x['lix'], a+100*sum(len(t)>6 for t in filtered)/len(filtered), places=12)
                w, n = x['textstat_lexicon_count'], x['textstat_sentence_count']
                if w:
                    d = count_difficult_words(doc.text, 'en_US', 2, unique=False)
                    self.assertAlmostEqual(x['textstat_spache_readability'], .141*(w/n)+.086*(100*d/w)+.839, places=12)
                    d3 = count_difficult_words(doc.text, 'en_US', 3, unique=False)
                    self.assertAlmostEqual(x['textstat_gunning_fog'], .4*(w/n+100*d3/w), places=12)
            self.assertEqual(self.scorer.difficult_words(TEXTS['difficult_case']), 2)
            self.assertEqual(self.scorer.lexicon_count('well-known'), 1)
            self.assertEqual(self.scorer.letter_count('well-known'), 9)
            self.assertNotEqual(self.scorer.linsear_write_formula(TEXTS['first_100']),
                                self.scorer.linsear_write_formula(TEXTS['first_100'], strict_upper=False))
            self.assertLess(self.scorer.linsear_write_formula('!!!'), 0)

    def test_protocol_repeat_and_v1_identity(self):
        request = dict(operation='extract', schema='writing_features_v2', ids=list(TEXTS), texts=list(TEXTS.values()))
        first = json.loads(backend.request_json(json.dumps(request)))
        self.assertTrue(first['ok'], first)
        self.assertEqual(first['result']['columns'], self.golden['columns'])
        second = json.loads(backend.request_json(json.dumps(request)))
        self.assertEqual(first, second)
        request['schema'] = 'writing_features_v1'
        v1 = json.loads(backend.request_json(json.dumps(request)))
        self.assertTrue(v1['ok'], v1)
        self.assertEqual([r[:21] for r in first['result']['rows']], v1['result']['rows'])
        status = json.loads(backend.request_json(json.dumps(dict(operation='status', schema='writing_features_v2'))))
        self.assertTrue(status['ok'], status)

    def test_dispatch_and_corruption_guards(self):
        from types import SimpleNamespace
        doc = self.docs['three_sentences']
        textstat_rows = [r for r in self.schema if r['source_package']=='textstat']
        scorer = SimpleNamespace(**{r['upstream_field']: (lambda text, i=i: float(i)) for i, r in enumerate(textstat_rows)})
        self.assertEqual(backend.document_values(doc, textstat_rows, scorer), list(map(float, range(14))))
        bad = [dict(textstat_rows[0], upstream_field='set_lang')]
        with self.assertRaisesRegex(backend.FeatureError, 'Unknown textstat'):
            backend.document_values(doc, bad, scorer)
        for value, message in [(math.nan, 'Unexpected missingness'), (math.inf, 'Infinite')]:
            with patch.object(scorer, 'gunning_fog', return_value=value):
                with self.assertRaisesRegex(backend.FeatureError, message):
                    backend.document_values(doc, textstat_rows, scorer)
        request = json.dumps(dict(operation='status', schema='writing_features_v2'))
        with patch.object(backend.hashlib, 'sha256') as digest:
            digest.return_value.hexdigest.return_value = 'bad'
            self.assertIn('frozen audit', json.loads(backend.request_json(request))['error'])
        for schema in ['writing_features_v3', '../writing_features_v2', [], None]:
            result = json.loads(backend.request_json(json.dumps(dict(operation='status', schema=schema))))
            self.assertFalse(result['ok'])


def record():
    if FIXTURE.exists():
        raise SystemExit('Refusing to overwrite frozen v2 evidence')
    from textstat.textstat import textstatistics
    expected = json.loads(backend.artifact('audit-environment.json').read_text())
    backend.check_versions(expected)
    backend.check_resources(expected)
    with backend.offline_resources():
        nlp = upstream_pipeline()
        scorer = textstatistics()
        scorer.set_lang('en_US')
        schema = schema_rows(2)
        result = dict(schema='writing_features_v2', schema_sha256=MANIFEST['schema_sha256'],
                      columns=['item_id']+[r['feature'] for r in schema], texts=list(TEXTS.values()),
                      rows=[[key]+upstream_values(nlp(text), schema, scorer) for key,text in TEXTS.items()])
    FIXTURE.parent.mkdir(parents=True, exist_ok=True)
    FIXTURE.write_text(json.dumps(result, indent=2, allow_nan=False)+'\n')


if __name__ == '__main__':
    with patch.object(socket.socket, 'connect', backend.no_download), \
            patch.object(socket, 'create_connection', backend.no_download), warnings.catch_warnings():
        warnings.simplefilter('ignore')
        if sys.argv[1:] == ['--record']:
            record()
        else:
            unittest.main(verbosity=2)
