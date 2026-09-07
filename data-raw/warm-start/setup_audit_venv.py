"""Explicit maintainer setup; downloads software. Never called by tests or R.

Usage: python3 setup_audit_venv.py /absolute/path/to/new/venv
Requires Python 3.12.3 for the recorded Linux audit baseline.
"""

import argparse
import hashlib
import json
from pathlib import Path
import platform
import subprocess
import sys
import urllib.request
import venv
import zipfile


HERE = Path(__file__).resolve().parent
PIP_BOOTSTRAP = "https://bootstrap.pypa.io/get-pip.py"
PIP_SHA256 = "fb24e693bab954209a063d90953621412ccad4a500905a726286e038f508ddf6"
CMUDICT = "https://raw.githubusercontent.com/nltk/nltk_data/gh-pages/packages/corpora/cmudict.zip"


def download_checked(url, destination, checksum):
    with urllib.request.urlopen(url) as response:
        content = response.read()
    if hashlib.sha256(content).hexdigest() != checksum:
        raise RuntimeError(f"Downloaded resource changed: {url}. Review before updating its pin.")
    destination.write_bytes(content)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("venv", type=Path)
    args = parser.parse_args()
    destination = args.venv.expanduser().absolute()
    if platform.python_version() != "3.12.3":
        parser.error("Use Python 3.12.3 to reproduce the recorded audit environment.")
    if destination.exists():
        parser.error("Destination already exists; choose a new path. Existing environments are never overwritten.")
    evidence = json.loads((HERE / "audit-environment.json").read_text())
    print(f"Creating {destination}; installing requirements-audit.lock, the 588 MB English model, "
          "and CMUdict. Network access is required for this explicit setup step.", flush=True)
    # --without-pip also works on Debian installations missing ensurepip.
    venv.EnvBuilder(with_pip=False).create(destination)
    executable = destination / ("Scripts/python.exe" if sys.platform == "win32" else "bin/python")
    bootstrap = destination / "get-pip.py"
    download_checked(PIP_BOOTSTRAP, bootstrap, PIP_SHA256)
    subprocess.run([str(executable), str(bootstrap), "pip==26.2.1"], check=True)
    subprocess.run([str(executable), "-m", "pip", "install", "-r",
                    str(HERE / "requirements-audit.lock")], check=True)
    corpora = destination / "nltk_data/corpora"
    corpora.mkdir(parents=True)
    archive = corpora / "cmudict.zip"
    download_checked(CMUDICT, archive, evidence["resource_sha256"]["nltk_data/corpora/cmudict.zip"])
    with zipfile.ZipFile(archive) as resource:
        for name in resource.namelist():
            path = Path(name)
            if path.is_absolute() or ".." in path.parts or not path.parts or path.parts[0] != "cmudict":
                raise RuntimeError("Unexpected archive entry in CMUdict")
        resource.extractall(corpora)
    subprocess.run([str(executable), "-m", "pip", "check"], check=True)
    print(f"Setup complete. Run: {executable} {HERE / 'audit_feature_schema.py'}", flush=True)


if __name__ == "__main__":
    main()
