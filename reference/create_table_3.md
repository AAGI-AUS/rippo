# Create a Table of Third Party IP (Table 3) of the R packages used for an AAGI IPPO document

Automatically recognises R packages that are used in a project and
generates Table 3 of the IPPO register. Has allowances for non-CRAN
packages, ASReml-R and DiGGer and also the Quarto software.

## Usage

``` r
create_table_3(
  project_path = getwd(),
  outfile = "1 documentation/table_3.csv",
  aagi_project_code = "AAGI-ALL-SP-003",
  quarto = FALSE,
  digger = FALSE,
  asreml = FALSE
)
```

## Arguments

- project_path:

  The path to the directory of the project to generate the tables for,
  defaults to the current working directory and recurses into
  subfolders.

- outfile:

  Character string that provides the file name and full path to the
  directory where the table will be saved. Defaults to the current
  project's “1 Documentation” directory with a filename of
  “table_3.csv”.

- aagi_project_code:

  The AAGI project code to be inserted into the table. Defaults to the
  current Service and Support project code within the AAGI project. Note
  that this is NOT the project being serviced but the code within the
  AAGI project that is allocated for Service and Support, *e.g.*
  "AAGI-ALL-SP-003".

- quarto:

  `Boolean` cite the Quarto software? Defaults to `FALSE`.

- digger:

  `Boolean` cite the DiGGer package? Defaults to `FALSE`.

- asreml:

  `Boolean` cite the ASReml-R package? Defaults to `FALSE`.

## Value

An invisible `NULL`, called for its side effects of writing a sheet into
an Excel workbook file (the project's IPPO register) on the local disk.

## See also

Other create IPPO:
[`run_IPPO_app()`](https://aagi-aus.github.io/rippo/reference/run_IPPO_app.md)

## Author

Zhanglong Cao, <zhanglong.cao@curtin.edu.au>, and Adam H. Sparks,
<adam.sparks@curtin.edu.au>

## Examples

``` r
if (FALSE) { # interactive()
# after opening an RStudio project for a Service and Support Analysis

create_table_3()
}
```
