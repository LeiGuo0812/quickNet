#!/usr/bin/env python3
"""Run the unmodified local SymPerturb source on the near-singular audit input.

First run validate-intervention-reliability.R reference, then:
  python tools/generate-intervention-reference.py --source /path/to/SymPerturb/src
No upstream implementation is copied into quickNet.
"""
import argparse
import hashlib
import json
import pathlib
import platform
import sys

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--source", type=pathlib.Path, required=True)
parser.add_argument("--output", type=pathlib.Path,
                    default=pathlib.Path(__file__).resolve().parents[2] / "output/audit/intervention-reliability")
args = parser.parse_args()
sys.path.insert(0, str(args.source.resolve()))
import numpy as np
from symperturb.network import fit_gaussian_network
from symperturb.state import post_intervention_moments

data = np.loadtxt(args.output / "near-singular-input.csv", delimiter=",", skiprows=1)
result = {}
for ridge in (0, .02):
    network = fit_gaussian_network(data, ridge=ridge)
    post = post_intervention_moments(network.mu, network.covariance, [0, 1], .4)
    result[str(ridge)] = {
        "pseudoinverse": network.used_pseudoinverse,
        "precision": network.precision.tolist(),
        "partial_correlations": network.partial_correlations.tolist(),
        "mu": post.mean.tolist(),
        "covariance": post.covariance.tolist(),
        "condition": float(np.linalg.cond(network.covariance)),
    }
result["versions"] = {"Python": platform.python_version(), "NumPy": np.__version__}
result["source"] = {"expected_commit": "76dd4178b285b80beb69f14a642e84ed1cabc7a0",
                    "directory": str(args.source.resolve()),
                    "sha256": {name: hashlib.sha256((args.source / "symperturb" / name).read_bytes()).hexdigest()
                               for name in ("network.py", "state.py")}}
with (args.output / "near-singular-python.json").open("w") as handle:
    json.dump(result, handle, indent=2)
