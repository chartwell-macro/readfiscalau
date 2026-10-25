# Contributing to `readfiscalau`

`readfiscalau` reads selected Australian Commonwealth fiscal releases and
preserves release, source, schema, and cache provenance. This file records the
reproducibility and validation information for people who choose to propose or
implement changes while preserving the package's output and source-attribution
contracts.

## Before changing the package

- Read the [package site](https://chartwell-macro.github.io/readfiscalau/), the
  relevant reference page or vignette, and the installed `inst/TERMS.md`
  source-terms file.
- For a new source, a new schema family, or a behavior change, record the
  proposal in [chartwellmacro@gmail.com](mailto:chartwellmacro@gmail.com)
  with a `[Proposal]` subject and the relevant evidence. Use the same email address with a `[Question]` subject for usage or workflow questions.
- Never commit credentials, cookies, authorization headers, signed URL query
  strings, private data, or unredacted downloaded source bytes.

## Adding or updating a source

Source additions need evidence that can be replayed offline and checked later:

1. Use the official HTTPS resource and record the release identity, source URL,
   schema/media expectations, and source hash in the relevant manifest or
   evidence file.
2. Record the source `terms_url`, licence, and attribution. Read the exact
   publisher terms before adding a fixture or preserving source bytes; a
   parser or cache does not expand those rights.
3. Add the smallest deterministic fixture that exercises the real parser. A
   synthetic or reduced replay fixture is preferred to a full official
   publication, subject to the source terms.
4. Add parser and contract tests covering columns, order, types, classes,
   attributes, dates, warnings, errors, and provenance. Tests must not depend
   on a live endpoint being available.
5. Document the supported source, limitations, and output contract in the
   roxygen reference and the user-facing README or vignette.

## Code and documentation changes

- Keep public signatures, defaults, identities, output schemas, date semantics,
  cache behavior, and failure conditions stable unless the change is explicitly
  justified and tested.
- Treat roxygen comments as the source for generated reference pages and
  `NAMESPACE`; regenerate them after changing exported documentation.
- Edit `README.Rmd` and vignette source files, then render their generated
  outputs. Keep examples deterministic and make the offline path exercise real
  package code.
- Keep changes focused. Do not add a runtime dependency for convenience when a
  base-R or existing dependency implementation is sufficient.

## Local checks

Before opening a pull request, run the checks relevant to the change. A full
package change should include, where the tools are available:

```r
devtools::test()
```

```sh
R CMD build .
R CMD check --as-cran readfiscalau_*.tar.gz
R -q -e 'pkgdown::build_site()'
git diff --check
```

Inspect the built tarball, rendered README/vignettes, reference pages, and
pkgdown links. Do not include local check directories, downloaded data,
credentials, benchmark output, or other generated artifacts in a pull request.

## Pull requests

Describe the user-facing purpose, affected source/schema or documentation
area, fixture and test coverage, generated files, and commands run. For a
behavior-sensitive change, include before/after output-contract evidence and
explain any intentional difference. Reference the prior email discussion.

## Questions

Use [chartwellmacro@gmail.com](mailto:chartwellmacro@gmail.com)
with a `[Question]` subject for usage and workflow questions, `[Proposal]` for
source/schema or behavior proposals, and `[Bug]` for reproducible bugs, source
drift, or documentation defects.
