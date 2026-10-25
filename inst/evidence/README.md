# readfiscalau stage-3 evidence

Checked: 27 July 2026 (Australia/Sydney)

## Stage-3 exit status: PASS_WITH_MEMORY_WARNING

The functional and source-evidence gate passes: the package source check
passed, the opted-in live replay
matched the pinned official raw-file hashes and checked values, and both
committed evidence charts passed visual/source-note review. The corrected
quiet-slot run also passed the latency and 1 GiB deployment-memory envelopes.
The disposition retains a memory warning because the largest single measured
increment was 147.984375 MiB.

`value-crosscheck.csv` records seven values hand-checked against two distinct
official machine-readable releases:

- Australian Treasury Budget 2026–27 chart-data ZIP, Table 1, raw SHA-256
  `2069eb7986f890423323732f12be0ef172ece996bffbd1bbaf0e814d324a1681`;
- Department of Finance May 2026 actual aggregate workbook, sheet `2025-26`,
  raw SHA-256
  `8b74f69be62ff34a04a18ac4455cc0c706034368d22dbcf3126200d3a0c6ca96`.

The package test suite replays committed parser fixtures and requires their
values, units, and actual/forecast status to match this file. The opted-in live
validator downloads the pinned official bytes, requires the same values and
hashes, exercises both discovery pages, replays the second historical Finance
schema from the same workbook, and proves that the two Finance releases retain
different cache namespace and raw snapshot paths.

`budget-history-release-ledger.csv` records the Treasury Budget vintage
evidence. The supported annual panel spans 2010–11 through 2026–27 and includes
both 2022–23 publications. Its 15 older releases use 264 committed curated
observations from official Budget Paper No. 1 PDFs; each runtime read verifies
the selected PDF against the ledger/panel URL and SHA-256. The panel is limited
to gross income tax withholding, company tax, and total receipts. The newer
2024–25 vintage uses direct Table 1 CSV, while 2025–26 and 2026–27 use native
chart-data ZIPs.

Run the live check after installing the package:

```sh
READFISCALAU_RUN_LIVE=true \
Rscript tools/validate-live.R
```

Render the two committed charts:

```sh
Rscript tools/render-stage3-evidence.R
```

The chart script reads only `value-crosscheck.csv`; it does not silently
download or revise evidence. Both PNGs were visually checked for complete
titles, axes, source notes, status labels, negative-value treatment, and
clipping.

## Quiet-slot performance disposition

`quiet-slot-performance-raw.csv` contains 18 guarded rows: three cases, cold
and warm disk-snapshot states, and three repetitions per case/mode.
`quiet-slot-performance-summary.csv` records min/median/max latency and RSS.
Every row retains the expected parsed row count and exact official raw
SHA-256. Warm means that the verified hash-addressed raw snapshot was
preseeded; it does not claim a parsed-object cache hit.

The maximum observed latency was 0.176 seconds. The maximum process RSS was
423.796875 MiB. The largest measured increment was 147.984375 MiB for the May
2026 Finance workbook; adding that increment to the 416 MiB deployment
baseline gives 563.984375 MiB. These observations pass the 1.5-second and
1 GiB envelopes, with the material one-run increment retained as an explicit
monitoring warning.

An independent checker recomputed the summary exactly and verified the 18-row
matrix, row counts, hashes, finite measurements, cache-state labels, fresh
non-aliased output files, and absence of private paths. Artifact SHA-256:

- raw: `5b3e08b8f303cb29dd8c32bce19ebda752d48b043e02c04dd8c512f67a4215a6`;
- summary: `45d8cc814bde59e4e2a3d48c3b2e92a3bad4109ce7ca51aac018f8516fe3214e`.

`tools/benchmark-quiet.R` continues to refuse execution without two explicit
quiet-slot opt-ins, 3–20 repetitions, fresh isolated output targets, and local
pinned official source files. It rejects pre-existing outputs and targets that
alias each other or either official input. No private input path is committed
in this evidence.
