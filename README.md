[README.md](https://github.com/user-attachments/files/28616209/README.md)
QSC Coder

A deterministic, peer-reviewable content-analysis instrument for measuring the
**Governance of Expectations** in the quantum-safe cryptographic (QSC)
transition. It codes policy and governance documents against a versioned
codebook of twelve sub-codes spanning two analytic families — Expectation
Construction (**EXP**) and Institutionalisation (**INST**) — and emits
per-document GoE measurements together with a cryptographic provenance manifest
for independent re-execution.

Built for a master thesis in Science and Technology Studies. Runs **headless**
from the command line; there is no machine-learning component in the coding
loop, so identical inputs yield identical outputs on every run.

> **License:** MIT · **DOI:** [10.5281/zenodo.20547281](https://doi.org/10.5281/zenodo.20547281) · **Cite:** [`CITATION.cff`](CITATION.cff) · **Methods:** see [Methodological note](#methodological-note)

---

## What it measures

For each document, the Coder computes:

- **B(d) = (n_EXP − n_INST) / (n_EXP + n_INST)** — the expectation–institutionalisation imbalance score, bounded on [−1, +1].
- **Specificity index** — a 0–3 score per expectation excerpt (dated deadline, named actor, named standard).
- **Named-standard mention index** — count and presence of FIPS / ETSI / ISO / BSI / ANSSI references.
- **EXP × INST co-occurrence** — document-set count and Jaccard coefficient.
- **Pipeline completeness** — per-document `has_EXP` / `has_INST` flags.

The engine is `quanteda::dictionary()` + `kwic()` with a per-sub-code
`threshold_regex` gate on the keyword-in-context window. The imbalance score
adapts the normalised-difference (polarity-score) form from dictionary-based
text measurement (Rauh 2018; Boumans & Trilling 2016); the specificity index
operationalises the sociology-of-expectations claim that the governing force of
an articulated future rises with its concreteness (Konrad 2006).

## Requirements

- R ≥ 4.1 (developed against 4.6).
- The packages installed by `install_deps.R`: quanteda, quanteda.textstats,
  readtext, pdftools, yaml, jsonlite, dplyr, tidyr, stringr, tibble, readr,
  digest, testthat.

## Quick start

```bash
# 1. install R packages (one-off)
Rscript install_deps.R

# 2. verify the codebook is well-formed (idempotent; safe to re-run)
Rscript scripts/lint_codebook.R

# 3. run the test suite
Rscript -e 'testthat::test_dir("tests")'

# 4. code a corpus
Rscript qsc_code.R --csv examples/sample_corpus.csv --docs examples
```

`qsc_code.R` options:

| flag         | meaning                                                         |
|--------------|----------------------------------------------------------------|
| `--csv`      | corpus manifest CSV (required)                                  |
| `--docs`     | directory holding the referenced documents (required)          |
| `--tau`      | `expect_heavy_soft` threshold (default `0.50`)                  |
| `--out`      | output run directory (default `output/runs/<UTC-stamp>`)        |
| `--codebook` | codebook YAML (default `backend/codebook/qsc_codebook.yml`)     |
| `--version`  | print tool version and exit                                    |
| `--help`     | print usage and exit                                           |

The command prints a run summary (run id, document and hit counts, families
present, median B, the `expect_heavy` tallies, and the codebook SHA-256) and
lists the files written.

## Inputs

The corpus manifest CSV needs these columns:

| column   | type    | values                            |
|----------|---------|-----------------------------------|
| doc_id   | string  | unique per row                    |
| helix    | string  | one of U, G, I, hybrid, T         |
| tier     | string  | one of T1, T2, T3, T4             |
| year     | integer | 1995 – 2030                       |
| path     | string  | optional — filename to match      |

Each document is resolved inside `--docs` by `path` (preferred) or by `doc_id`
treated as a filename. Supported document types: `.txt`, `.md`, `.docx`, `.pdf`.

## Outputs

Each run lives under `output/runs/<UTC-stamp>/` (or `--out`):

| file                          | content                                                |
|-------------------------------|--------------------------------------------------------|
| `manifest.json`               | run metadata: codebook hash, tool version, configs     |
| `codebook_used.yml`           | byte-identical snapshot of the loaded codebook         |
| `hits.csv`                    | one row per qualified KWIC hit                          |
| `documents.csv`               | wide table — one row per document with all GoE metrics  |
| `documents_long.csv`          | long table — one row per (doc, sub-code)                |
| `cooc_matrix.csv`             | family × family co-occurrence (EXP × INST in v2.0)      |
| `pipeline_completeness.csv`   | per-doc `has_EXP` / `has_INST` flags                    |
| `qsc_run_<run_id>.json`       | JSON twin of the above for interpretive sessions        |

## Codebook

`backend/codebook/qsc_codebook.yml` defines 12 sub-codes across 2 families:

- **EXP** (Expectation Construction): THREAT, TIMELINE, URGENCY, COLLECTIVE, RISK, CONTESTATION
- **INST** (Institutionalisation): MANDATE, STANDARD, GOVERNANCE, CERTIFICATION, SCOPE, TRANSITION

Each sub-code carries a definitional rule, a plain-language qualifying threshold,
a list of keyword phrases, and a Perl `threshold_regex` that must fire in the
KWIC window for a hit to qualify. To edit:

1. Update the YAML.
2. Add positive / negative test sentences to `tests/fixtures/{positive,negative}.yml`.
3. Run `Rscript scripts/lint_codebook.R` and ensure it exits 0.

## Reproducing a published run

Every figure reported from this instrument can be reconstructed:

1. Clone the repository at the commit recorded in the run's `manifest.json`
   (`git_commit` field).
2. Install the package versions recorded in the manifest (`package_versions`).
3. Supply the same corpus and the same `config` (the τ value is in the manifest).
4. Re-run. The new `manifest.json` will carry the same `codebook_sha256`,
   confirming the codebook is byte-identical to the one that produced the
   published result.

Determinism is verified in CI by running the sample corpus twice and asserting
that every coded CSV is byte-identical across runs.

## Methodological note

Constructing a computational instrument for an interpretive question invites the
critiques that critical algorithm studies direct at algorithmic systems —
opacity, the silent encoding of values, and exclusion by classification (Burrell
2016; Crawford 2021). The instrument is designed to answer each:

- **Opacity.** The Coder is a deterministic rule system, not a machine-learning
  model; every coding decision traces to a named keyword and an inspectable
  regular expression.
- **Encoded values.** The analytic choices a coding scheme embeds are made the
  instrument's first-class, contestable object — the codebook is a versioned
  text in which each sub-code's rule and theoretical anchor are explicit.
- **Exclusion.** The instrument's limits are reported, not concealed: v2.0
  implements 12 of a projected 23 sub-codes; dictionary recall is bounded by
  keyword coverage; out-of-vocabulary expressions are recorded rather than
  discarded.

The tool was implemented with AI-assisted programming under the researcher's
specification; the analytical instrument — the codebook, the measurements, and
the output schema — is human-authored.

## Citation

If you use this software, please cite it via [`CITATION.cff`](CITATION.cff) or
the DOI [10.5281/zenodo.20547281](https://doi.org/10.5281/zenodo.20547281). Cite
the version DOI for the release you used.

## What is not in this repository

The assembled corpus and any stakeholder transcripts are **not** included. The
corpus is a separate research artefact released under its own terms; stakeholder
material is confidential. See [`.gitignore`](.gitignore).

## License

[MIT](LICENSE) © 2026 Swann Ashworth.
