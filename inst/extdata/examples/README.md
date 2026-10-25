# `readfiscalau` documentation fixtures

These small files are packaged only to make the README and vignettes
deterministic and offline-capable. They are copied from the regression fixtures
used by the package tests and are deliberately labelled as schema/replay
fixtures, not as a second release manifest or a claim that their values are a
current official publication.

- `budget-current.csv` is a small Treasury chart-data-shaped CSV. The
  documentation passes it to `read_au_budget()` with an explicit fixture
  manifest row and `raw =` so no network call is made.
- `treasury-downloads.html` is a small official-downloads-shaped HTML snapshot
  for `discover_au_fiscal_releases(raw = ...)`.
- `finance-ckan.json` is a small CKAN `package_show`-shaped payload for
  `discover_finance_ckan_resources(raw = ...)`.
- `budget-chart-archive.zip` is a small multi-table chart-data ZIP for the
  `table_id` selection path.
- `finance-current.xlsx` and `finance-history.xlsx` are small workbook-shaped
  fixtures for Finance release/history replay examples.
- `fbo-history.docx` is a small DOCX-shaped fixture for the admitted FBO
  history parser path.

The files are not a substitute for the pinned release manifest in
`releases.csv`. A live read uses the selected manifest resource and validates
its HTTPS response and expected SHA-256; a fixture read uses exact bytes and
an explicit fixture manifest solely for reproducible examples. These files are
not represented as current official publication snapshots, and their terms do
not supersede the source-level terms recorded in `inst/TERMS.md`.
