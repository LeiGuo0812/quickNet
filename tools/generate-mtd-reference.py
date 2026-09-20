"""Generate MTD/TTS fixtures by executing the authors' unmodified TTS source.

Download Yuan and Shou's S1 Code (doi:10.1371/journal.pbio.3002758.s009)
and extract aeyuan-tts_repo-551b5a612383/core/test_independence.py, then run:

    python tools/generate-mtd-reference.py --tts-source /path/to/test_independence.py

The source is not bundled here. Only NumPy and the Python standard library are
needed to generate these fixtures. R regression tests read the stored JSON and
do not need Python. The MTD statistic supplied to the authors' generic test is
the absolute mean derivative product, with full-series sample-SD scaling.
"""

import argparse
import hashlib
import importlib.util
import json
from pathlib import Path
import platform

import numpy as np


AUTHOR_SHA256 = "5dd63754c02bf6cc1d9e89a799f3765624ea322543745540d6ecf21467481fc2"
ARCHIVE_SHA256 = "9323e192e4c586794cd7f0c795c8cb601e295f616062c237f0761ed94832e7cd"
SEED = 392708


def author_test(path):
    digest = hashlib.sha256(path.read_bytes()).hexdigest()
    if digest != AUTHOR_SHA256:
        raise ValueError(
            "TTS source SHA256 does not match the inspected S1 Code release: "
            f"expected {AUTHOR_SHA256}, got {digest}"
        )
    spec = importlib.util.spec_from_file_location("author_tts_reference", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module.test_independence


def signed_product(x, shifted_y):
    """Callback for the authors' vectorized generic statistic interface."""
    return np.mean(x[:, None] * shifted_y, axis=0)


def absolute_product(x, shifted_y):
    return np.abs(signed_product(x, shifted_y))


def ar1(rng, n, coefficient):
    noise = rng.normal(size=n + 256)
    series = np.empty_like(noise)
    series[0] = noise[0] / np.sqrt(1 - coefficient**2)
    for index in range(1, series.size):
        series[index] = coefficient * series[index - 1] + noise[index]
    return series[-n:]


def fixtures(test):
    rng = np.random.default_rng(SEED)
    independent = np.column_stack((ar1(rng, 96, .75), ar1(rng, 96, -.4)))
    x = ar1(rng, 96, .6)
    negative = np.column_stack((x, -1.4 * x + .3 * ar1(rng, 96, .3)))
    alternating = np.concatenate(([0.0], np.cumsum(np.tile([1.0, -1.0], 5))))
    one_retained = np.column_stack((
        np.concatenate(([0.0], np.cumsum([1, -2, 3, 1, -1, 4, -2, 2, -3]))),
        np.concatenate(([0.0], np.cumsum([2, 1, -1, 3, -2, -3, 1, 4, -1]))),
    ))
    designs = {
        "independent_ar": (independent, 19),
        "negative_coupling": (negative, 19),
        "shifted_scaled": (independent * [-2.5, .125] + [10.0, -7.0], 19),
        "exact_ties": (np.column_stack((alternating, -alternating)), 3),
        "radius_zero": (independent[:12], 0),
        "maximal_radius": (one_retained, 4),
        "minimum_length": (np.array([[0., 2.], [1., 4.], [-1., 3.]]), 0),
    }
    cases = {}
    for name, (data, radius) in designs.items():
        derivatives = np.diff(data, axis=0)
        sd = np.std(derivatives, axis=0, ddof=1)
        normalized = derivatives / sd
        x, y = normalized.T
        b, bound, shifts, distribution = test(x, y, radius, absolute_product)
        _, _, signed_shifts, signed_distribution = test(x, y, radius, signed_product)
        raw_b, raw_bound, raw_shifts, raw_distribution = test(
            derivatives[:, 0], derivatives[:, 1], radius, absolute_product
        )
        np.testing.assert_array_equal(shifts, signed_shifts)
        np.testing.assert_array_equal(shifts, raw_shifts)
        np.testing.assert_allclose(distribution, np.abs(signed_distribution), rtol=0, atol=0)
        np.testing.assert_allclose(raw_distribution / np.prod(sd), distribution, rtol=2e-14, atol=2e-14)
        if b != raw_b or bound != raw_bound:
            raise AssertionError(f"Full-SD scaling changed the reference ranking for {name}")
        coupling = normalized[:, :, None] * normalized[:, None, :]
        m = derivatives.shape[0]
        cases[name] = {
            "name": name,
            "data": data.tolist(),
            "radius": radius,
            "derivative_sd": sd.tolist(),
            "standardized_derivatives": normalized.tolist(),
            "coupling": coupling.tolist(),
            "coupling_mean": float(np.mean(coupling[:, 0, 1])),
            "test_coupling_mean": float(signed_distribution[0]),
            "statistic": float(distribution[0]),
            "shifts": shifts.tolist(),
            "distribution": distribution.tolist(),
            "null_coupling": signed_distribution.tolist(),
            "extreme_count": int(b),
            "tts_bound": float(bound),
            "p_value": min(1.0, float(bound)),
            "minimum_p": 1.0 / (radius + 1),
            "derivative_indices": list(range(radius + 1, m - radius + 1)),
            "raw_extreme_count": int(raw_b),
            "raw_tts_bound": float(raw_bound),
            "raw_distribution": raw_distribution.tolist(),
            "python_population_sd_magnitude_factor": m / (m - 1),
        }
    if cases["exact_ties"]["extreme_count"] != 7:
        raise AssertionError("Tie fixture must count every shift")
    if len(cases["maximal_radius"]["derivative_indices"]) != 1:
        raise AssertionError("Maximal-radius fixture must retain one derivative")
    np.testing.assert_allclose(
        cases["shifted_scaled"]["null_coupling"],
        -np.asarray(cases["independent_ar"]["null_coupling"]),
        rtol=2e-13, atol=2e-13,
    )
    return cases


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--tts-source", required=True, type=Path,
                        help="Unmodified core/test_independence.py from the authors' S1 Code")
    parser.add_argument("--output", type=Path,
                        default=Path(__file__).resolve().parents[1] / "tests/testthat/fixtures/mtd-reference.json")
    args = parser.parse_args()
    test = author_test(args.tts_source)
    result = {
        "reference": {
            "authors": "Alex E. Yuan and Wenying Shou",
            "year": 2024,
            "paper_url": "https://doi.org/10.1371/journal.pbio.3002758",
            "source_url": "https://doi.org/10.1371/journal.pbio.3002758.s009",
            "archive_directory": "aeyuan-tts_repo-551b5a612383",
            "archive_sha256": ARCHIVE_SHA256,
            "source_member": "core/test_independence.py",
            "source_sha256": AUTHOR_SHA256,
            "function": "test_independence(x, y, w, statistic, x_lag=0)",
            "source_modified": False,
            "radius_default": None,
            "statistic": "abs(mean(x * y)); full-series sample-SD-scaled first differences",
            "normalization": "Sample SD (ddof=1), consistent with the authors' MATLAB std(td)",
            "matlab_source_url": "https://github.com/macshine/coupling/blob/da1f92b7337fe8ed74006f86f04ce39f7c3b41db/coupling.m",
            "matlab_executed": False,
            "normalization_note": "Shine's Python np.std uses ddof=0; its unsmoothed magnitudes are m/(m-1) times the sample-SD definition. That code is not executed by this generator.",
        },
        "generation": {
            "python_version": platform.python_version(),
            "numpy_version": np.__version__,
            "seed": SEED,
            "rng": "numpy.random.default_rng (PCG64)",
            "array_order": "coupling is time by node by node; data is row by column",
            "index_convention": "derivative_indices are one-based; shifts are signed integer offsets",
        },
        "cases": fixtures(test),
    }
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(result, indent=2, allow_nan=False) + "\n", encoding="utf-8")
    print(f"Saved {len(result['cases'])} MTD/TTS author-reference cases to {args.output}")


if __name__ == "__main__":
    main()
