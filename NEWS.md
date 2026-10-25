# readfiscalau 0.1.4

* Correct confirmed split-word aliases in Treasury Budget/MYEFO extraction and
  recover the parent labels for current-account-balance unit rows, fiscal
  balance (net lending/borrowing), and the wrapped mining/function label.
* Preserve the published financial-statement sector and use cash/accrual basis
  from the statement type. Preferred-source selection now includes sector, so
  public corporations cannot displace general-government observations.
* Actual-history catalogues distinguish single observations, sparse coverage,
  and continuous annual coverage without filling missing years.
* Keep original table text, cell values, dates, source hashes and all PBO data.
  Numerical equality alone is not a duplicate-removal rule. Corrected identities
  can retire malformed or incorrectly classified identities; callers must select
  a corrected identity rather than silently replay an ambiguous old definition.

# readfiscalau (development version)

* Constructs one package-owned actual history for every compatible
  Budget/MYEFO series, unit, accounting basis, and consolidation. The
  versioned chain selects the latest official publication for each historical
  date while retaining row-level release, table, URL, PDF hash, and container
  provenance. Exact forecast profiles now also have a direct package reader,
  so applications never need to assemble either path from raw table cells.
* Normalizes Unicode dash glyphs before parsing Treasury fiscal-year headers.
  Headings such as `2023–24` now remain fiscal years ending 30 June instead of
  being truncated to calendar year 2023 and incorrectly dated 31 December.
* Adds a static, hash-bound historical Budget/MYEFO table snapshot covering
  22 selected economic, fiscal, receipts, expenses, debt, interest, and formal
  statement families across 28 Budgets and 25 MYEFOs from 2000-01 onward.
  Nine local catalogue/read functions expose canonical source cells,
  package-constructed actual histories, exact forecast profiles, or complete
  tables without
  runtime PDF parsing. All durable rows in the
  latest detailed domestic forecast and detailed cash-receipts tables are
  queryable; volatile reconciliation detail and non-time totals stay at table
  level, and the expense-estimate reconciliation is explicitly table-only.
* Adds a deterministic cross-vintage chart example comparing underlying cash
  balance paths published in selected Budgets from 2004-05 through 2026-27
  with the corresponding annual actuals from PBO Historical Fiscal Data.
* Adds a generated README, task-oriented vignettes, complete offline examples,
  a package topic, expanded function reference, grouped pkgdown configuration,
  contributor/support guidance, and multi-platform check, coverage, and site
  workflows.
* Standardizes the package author and maintainer display as Chartwell Macro across
  DESCRIPTION, citations, the package topic, README, and vignettes while
  identifying Chartwell Macro as the package-authored material's copyright holder and
  retaining all upstream source-attribution statements.
* Adds a prominent end-to-end Budget time-series workflow to the README and
  Getting Started article. The visible code discovers a pinned Treasury
  release, reads and filters the `Total receipts` series, and charts published
  annual values with `ggplot2`; documentation builds use a checked-in,
  source-attributed render so they remain deterministic and offline.
* Makes the Australian Treasury, Department of Finance, and Parliamentary
  Budget Office source roles and their admitted time coverage prominent on the
  homepage and in Getting Started, including the row-level PBO provenance
  boundary within some Treasury Budget and MYEFO panels.
* Distinguishes Finance's January 2000 archived statements from the July 2000
  start of the machine-readable aggregates surfaced by the package, and
  documents the 2008–09 accounting/format boundary without implying a recast.
* Explains how the curated Budget and MYEFO publication-vintage panels combine
  named Treasury tables with selected PBO Tables 1, 2, and 5, including their
  supported families, actual/forecast semantics, and family-wide date
  envelopes.
* Reframes the README and task articles around supported workflows and
  reproducibility features, with source boundaries, licensing caveats, and
  operational limits consolidated in explicit scope and troubleshooting
  sections.
* Documents the exact release, output, provenance, cache, archive, source-term,
  and failure contracts, including the distinction between discovery candidates
  and admitted parser schemas.
* Makes Finance month-header matching independent of the process locale, lets
  libcurl negotiate the HTTP protocol, rejects credential-bearing source URLs
  before network or cache metadata handling, avoids redundantly validating the
  already-validated built-in release manifest, and makes unavailable free-space
  probing advisory on platforms without the Unix `df` utility. Explicit
  `available_bytes` archive limits remain enforced.
* Adds deterministic regression coverage for documented public entry points,
  Budget-index crawling, portability, and transport credential redaction.

# readfiscalau 0.1.1

* Add `list_au_fiscal_search_controls()` for fast, local application controls
  without materializing series-level coverage.

# readfiscalau 0.1.0

* Memoises immutable Budget, MYEFO, and PBO coverage catalogues, avoids
  materialising full cached source documents for path-based parsers, and keeps
  complete metadata records visible during concurrent cache publication.

- Expands all 28 Budget vintages from 2000–01 to 14,352 observations across
  major economic
  parameters, Budget aggregates, detailed domestic forecasts, full cash
  receipts, and cash interest. Structured PBO forecast tables are combined
  with exact Budget Paper No. 1 tables while retaining release and
  observation-source hashes. Current Budget cash receipts preserve both the
  complete detailed BP1 table and the longer native chart-data history.
- Adds every completed-year series from the PBO Historical Fiscal Data
  2026–27 Budget workbook: 11 tables, 616 series, and 20,552 observations
  through 2024–25. New table/series catalogue functions and a table-selecting
  reader preserve the exact workbook URL, SHA-256, licence and attribution.
- Expands the 25-release MYEFO vintage panel from 2000–01 to six table
  families and 10,224 observations:
  major economic parameters, fiscal aggregates, Budget comparators, detailed
  domestic forecasts, cash receipts, and cash interest. The PBO structured
  forecasts workbook is primary where available; exact Treasury release tables
  provide the detailed fallback.
- Restores the complete four-year cash-receipts forecast horizon published in
  the 2006–07 through 2014–15 MYEFO appendices. Lettered appendix tables are
  now recognised during full archive rebuilds, with regression checks on every
  affected vintage.
- Adds a hash-bound MYEFO cash-receipts panel where published across all 25
  official vintages from 2000–01 through 2025–26, plus exact local coverage
  metadata via
  `list_au_myefo_series()`.
- Reuses previously downloaded pinned release snapshots after re-validating
  their SHA-256 and release-bound metadata, avoiding duplicate network
  transfers while preserving exact provenance and fail-closed behavior.
- Memoizes immutable installed release metadata and compact historical Budget
  observations by file fingerprint to avoid repeated CSV parsing.
- Adds official Budget-document archive tooling covering the 1901–02 through
  2026–27 catalogue scope: index discovery, explicit historical/link-gap
  records, SHA-256 content-addressed local preservation, revision linkage,
  storage-capacity preflight, one-file streaming admission with per-document
  inventory/gap checkpoints and resume, listing, and integrity/completeness
  validation.
  A bundled machine-readable snapshot inventories 1,269 official document
  candidates and 70 captured index pages (about 1.7 MiB total). Durable
  materialisation stores 1,260 candidates (about 2.33 GiB); the other nine
  candidate documents are explicit gaps (seven HTTP 404, one 2007–08 HTML
  redirect/media mismatch, and one 2005–06 web-archive HTML/PDF-signature
  failure). Together with nine structural/insecure gaps, accounted-completeness
  passes with no unaccounted URLs. The corpus remains operator-managed in an
  `archive_dir` outside git and is never imported by the app.
- Adds `read_au_monthly_financial_history()`, which reads every Finance
  aggregate workbook sheet from 2000–01, derives monthly flows from YTD
  observations, handles explicitly monthly early columns, and keeps stock
  measures as levels.
- Adds a pinned parser for the 2024–25 Final Budget Outcome Appendix B
  historical aggregate DOCX. Comparable annual actuals complete June as the
  annual outcome less May YTD, with possible revision residuals flagged.
- Adds an explicit canonical series crosswalk across legacy label changes and
  the 2008–09 accounting boundary. Pre-2007 AAS concepts are not conflated with
  the GFS annual series.
- Adds per-series/year completeness reporting and preserves irreducible
  published gaps as `NA`.
- Adds `read_au_budget_history()` for provenance-preserving chains across
  2010–11 to 2026–27 Treasury Budget vintages, including both March and
  October 2022–23 publications, with deterministic latest-actual overlap
  resolution and an explicit all-vintage revision panel.
- Parses the transposed Treasury percentage-of-GDP CSV schema without changing
  its unit or combining it with dollar-denominated observations.
- Adds versioned Australian Treasury and Department of Finance release
  manifests and fixture-replayable discovery.
- Adds strict chart CSV/ZIP and Finance monthly workbook parsers with explicit
  pre/post-2008 schema families.
- Adds bounded, atomic, SHA-256-addressed source caching and full provenance.
- Enforces the Finance July–May monthly boundary; June remains FBO or CFS.
- Pins four verified machine-readable built-ins and removes unsupported
  decorative release rows.
- Separates historical coverage from the update date of Finance's
  overwrite-in-place workbook; the current route now reads actual aggregates,
  not the monthly-profile forecast resource.
- Adds expected-hash enforcement, cache age bounds, collision-safe cache
  repair, stricter manifest semantics, and published thousands-separator
  support.
- Isolates byte-identical snapshots by release contract and records exact
  contract/parser revisions, response metadata, dependencies, and source terms
  in provenance.
- Rejects partial/paginated responses, response-length disagreement, ZIP member
  truncation, period/unit/scope drift, unfinished periods labelled actual, and
  Finance schema-unit or non-YTD drift.
- Adds deterministic committed ZIP/XLSX fixtures, official-value cross-check
  evidence, two chart artifacts, and guarded quiet-slot benchmark tooling.
- The single-release reader returns the complete cumulative-YTD history through
  its Finance release period, instead of discarding every workbook column except
  the latest. It still excludes non-YTD columns; the new monthly-history API
  handles those under its explicit reconstruction contract.
