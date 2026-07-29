# Changelog

## rippo 1.0.6

### Minor changes

- Add MPL license

## rippo 1.0.5

### Minor changes

- When creating Table 3, clarify FOSS licence restrictions rather than
  referring to the terms of the licence.
- When creating a list of tables using `list_ippo_tables`, the previous
  vague text about the terms of licensing are replaced with more
  descriptive text automatically for the IPPO register. This means that
  the original spreadsheets don’t need to be changed, but going forward
  new versions will use the new functionality upon creation.

## rippo 1.0.4

### Minor changes

- [`list_ippo_tables()`](https://aagi-aus.github.io/rippo/reference/list_ippo_tables.md)
  now includes a table, `Validation`, that clearly describes issues with
  IPPO registers so that they can be corrected at the source.
- [`list_ippo_tables()`](https://aagi-aus.github.io/rippo/reference/list_ippo_tables.md)
  now provides a clear error message when stopping on a malformed IPPO
  file with the filename, sheet and cell that needs to be fixed.
- [`list_ippo_tables()`](https://aagi-aus.github.io/rippo/reference/list_ippo_tables.md)
  now captures and returns more meaningful warnings.
- Update the internal Word document template SP logo block.

## rippo 1.0.3

- Internal change to devtool documentation, no user-facing changes.

## rippo 1.0.2

### Bug fixes

- Removed duplicate code blocks which caused the IPPO app to crash.
- Amended some fringe issues when importing an existing register.

### Minor changes

- Adjusted the formatting of the final template to align with expected
  format.
- Added additional R packages to the database file.

## rippo 1.0.1

### Bug fixes

- Fixes bug when importing AAGI R&D projects

### Minor changes

- Removes unnecessary code from script in `inst` for generating an IPPO
  document

## rippo 1.0.0

### Major changes

- Removes infile argument for
  [`create_ippo_report()`](https://aagi-aus.github.io/rippo/reference/create_ippo_report.md)

- Includes AAGI R&D projects in IPPO register

## rippo 0.0.1

- Initial release
