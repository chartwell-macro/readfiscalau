# readfiscalau 0.1.1

This is a pre-submission check record for the documentation and CRAN-readiness
branch. The package has not been submitted to CRAN, and this branch does not
publish a release.

This record applies to the documentation candidate ending at commit `10c731e`
and predates the Budget/MYEFO table-history development work. It is retained as
historical evidence only; run and record a fresh built-tarball check before any
submission or release from the current development branch.

## Test environments

The complete built-tarball check recorded below was run locally on:

* macOS Sequoia 15.6, Apple Silicon (`aarch64-apple-darwin24.4.0`)
* R 4.5.1 (2025-06-13), UTF-8 session charset
* `R CMD check --as-cran` with the declared dependencies and locally available
  documentation/build tools

No Linux, Windows, R-devel, R-oldrel, Win-builder, or R-hub result is claimed
here. The repository configures GitHub Actions for Linux, macOS, and Windows,
but a configured workflow is not represented as a completed external check.
The feature-branch jobs were created, but GitHub started no steps and reported
an account billing/payment or spending-limit block; those runs therefore do
not provide platform check results.
No coverage percentage or spelling result is claimed.

## R CMD check results

The exact source tarball was built with R 4.5.1, installed into a clean
isolated library, and checked with:

```text
R CMD check --as-cran readfiscalau_0.1.1.tar.gz
```

The 1,243,042-byte tarball was `readfiscalau_0.1.1.tar.gz` (SHA-256
`40dafddbe7798dc9a47163b2dc3ca09b106c37c284ad626381d1007c27ecd641`).
The check completed with **0 ERRORs, 0 WARNINGs, and 1 NOTE**. Installation,
loading, namespace and dependency checks, syntax and R-code checks, Rd checks,
examples, tests, all five vignette rebuilds, the PDF and HTML manuals, and
detritus checks completed successfully.

The sole NOTE was the incoming-feasibility URL check. Nine canonical GitHub,
GitHub Issues, GitHub Pages, article, and reference URLs returned HTTP 404
because the repository is private and its Pages site is not deployed. This is
an external publication-state issue; no invalid relative link was retained in
the disposable pkgdown site.

The check log is retained at
`work/evidence/final-check-v23/check-run/readfiscalau.Rcheck/00check.log` in the
integration evidence directory, which is outside the package source bundle.
The future-file-timestamp check completed successfully.

## Additional validation

The installed-tarball tests reported:

```text
[ FAIL 0 | WARN 0 | SKIP 1 | PASS 682 ]
```

The one skip is the intentional source-only documentation check: installed
packages do not contain the source `man/` blocks inspected by that test. The
source documentation test passed separately. All exported functions and S3
methods have documented, executable examples; the complete examples run
passed without a timing NOTE.

The README and five vignettes rendered from source. A disposable installed-mode
pkgdown build generated the home page, 25 reference topics in four task groups,
and five articles. Its configuration, built-site, rendered-error, stale-path,
and local-link checks passed (2,033 links and assets inspected; zero missing
local targets). The home page and Getting Started article include the real
Treasury release-discovery and `read_au_budget()` workflow, filtering of the
returned `period_end` and `value` fields, conversion from AUD million to AUD
billions, and a deterministic rendered `ggplot2` line chart of Total receipts.
The surrounding provenance text and alt text identify the Table 1 CSV and
Budget Paper 1 Table 5.7 sources; the figure preserves Commonwealth
attribution.

The final functional comparison used immutable baseline commit
`pre-publication baseline` and the exact final tarball. All 43
offline probes matched both raw and semantically: 38 successful and 5 expected
failure probes. Returned values, names, order, schemas, classes, attributes,
dates, warnings/messages, error classes/messages, side effects, and cache
behavior were identical. The cache probe made one fetch, reported
`cache_hit = FALSE` then `TRUE`, and returned identical data.

All 16 executable R files in the final tarball are parsed-expression-identical
to the accepted performance candidate. Its namespace fingerprint, 28 exports,
3 S3 registrations, all exported formals, and all 86 bundled `inst/extdata`
files are also identical. The accepted single-worker, counterbalanced offline
benchmarks used three warmups and 100 paired observations per panel/core mode,
with a targeted 200-pair extension for the one initially noisy Finance-cold
tail. Every median and p95 met the predeclared no-regression budgets of at most
5% and 10%, respectively. Relative final-versus-baseline results were:

| Workflow | Median | p95 |
|---|---:|---:|
| Budget panel first load | -0.93% | +7.79% |
| MYEFO panel first load | -0.32% | -13.14% |
| Budget core, cold cache | -2.41% | -5.17% |
| Budget core, warm cache | -0.13% | -0.09% |
| Finance core, cold cache (pooled 300 pairs) | -0.71% | +2.65% |
| Finance core, warm cache | 0.00% | +4.47% |
| FBO core, cold cache | +0.23% | +7.30% |
| FBO core, warm cache | +0.28% | -3.16% |

The read-only downstream Chartwell provider contract passed 1,514 expectations
across 36 test groups with no failures, errors, or warnings against the final
pre-ownership installed candidate; its two existing `callr`-dependent worker
tests were skipped because `callr` was unavailable in that isolated run. The
final tarball's 16 executable R files, namespace/API surface, and 86 bundled
`inst/extdata` files are byte-identical to that candidate.

`Authors@R`, the effective built `Author`/`Maintainer`, `inst/CITATION`, and
public documentation consistently identify the package author, maintainer,
and copyright holder as Chartwell Macro. The non-personal noreply address required for
the maintainer record appears only in `DESCRIPTION` and generated R package
metadata; optional public citation and documentation surfaces omit email.
The repository-level `CITATION.cff` is tracked for GitHub and excluded from the
source tarball; `inst/CITATION` remains in the tarball. No manual duplicate
`Author` or `Maintainer` fields are present in the source `DESCRIPTION`.

## Reverse dependencies and publication status

No reverse-dependency check was run: the 2026-08-24 registry audit found no
current or archived CRAN package named `readfiscalau`, and no current or
reachable archived Bioconductor software entry with that name. This is an
availability observation, not a reservation or acceptance decision.

## Items requiring resolution before a CRAN submission

The package must not be submitted to CRAN until the following external facts
are confirmed by the maintainer or the affected material is removed/reworked.
`LICENSE` and `Authors@R` identify Chartwell Macro as the copyright holder for
package-authored material; that package licence remains separate from the
rights and terms of bundled upstream data, captured official pages, logos, and
other third-party material.

* The package ships 20,552 normalized PBO historical rows, 4,928 PBO-derived
  Budget rows, and 4,494 PBO-derived MYEFO rows. The recorded PBO terms are CC
  BY-NC-ND 4.0 International; permission or a documented rights basis covering
  these selected/normalized representations and their intended distribution
  has not been established.
* Seventy captured Budget archive HTML index pages require a documented
  redistribution basis or replacement with synthetic/metadata-only fixtures.
  Every capture contains inline Google tag loader code, and ten also contain
  inline Hotjar/Contentsquare loader code; Treasury's Creative Commons notice
  excludes third-party content, so it is not a whole-file licence for these
  composite captures.
* The technical maintainer address recorded in `Authors@R` must be confirmed as
  active and monitored for CRAN correspondence.
* The canonical GitHub Pages URL and repository/issue URLs must be publicly
  reachable before they are presented to CRAN as verifiable publication links.
