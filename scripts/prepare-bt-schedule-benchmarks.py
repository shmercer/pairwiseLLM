#!/usr/bin/env python3
"""Install three exact revisions into new, isolated benchmark libraries.

Run from the source checkout. Existing libraries and output directories are
never overwritten. Dependencies must already be available; this script does
not install dependencies, invoke providers, or access any study repository.
"""
import argparse
import hashlib
import io
import json
from pathlib import Path
import subprocess
import tarfile


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output", type=Path)
    parser.add_argument("--candidate", default="HEAD", help="Committed candidate revision")
    args = parser.parse_args()
    if args.output.exists():
        parser.error("output must be a new directory")
    args.output.mkdir(parents=True)
    versions = dict(post328="7999c15c6c50b0d8de670ff49702648630d8235b",
                    study="4c4c01453077f90e987040c6a3c3d3cd711c6f21",
                    optimized=subprocess.check_output(["git", "rev-parse", args.candidate], text=True).strip())
    manifests = {}
    for label, revision in versions.items():
        source = args.output / (label + "-source")
        library = args.output / (label + "-library")
        source.mkdir()
        library.mkdir()
        archive = subprocess.check_output(["git", "archive", revision])
        with tarfile.open(fileobj=io.BytesIO(archive)) as package:
            package.extractall(source, filter="data")
        files = list((source / "R").glob("*.R")) + list((source / "inst/stan").glob("*.stan"))
        manifests[label] = dict(git_sha=revision, library=str(library.resolve()),
            source_sha256={str(p.relative_to(source)): hashlib.sha256(p.read_bytes()).hexdigest()
                           for p in sorted(files)})
        with (args.output / (label + "-install.log")).open("w") as log:
            subprocess.run(["R", "CMD", "INSTALL", "--library=" + str(library.resolve()), str(source.resolve())],
                           stdout=log, stderr=subprocess.STDOUT, check=True)
        (args.output / "installations.json").write_text(json.dumps(manifests, indent=2) + "\n")
        print(label, revision, library, flush=True)


if __name__ == "__main__":
    main()
