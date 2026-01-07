# Create IPPO tables list

Traverses directories and imports IPPO register Excel(TM) workbook files
and creates a list of tables corresponding to Tables 1-5 in the AAGI
IPPO register for all AAGI-CU Service and Support projects.

## Usage

``` r
list_ippo_tables(dir_path_in, sp)
```

## Arguments

- dir_path_in:

  A string value that provides the path to the top-level directory of
  the R: drive holding the AAGI-CU Service and Support project files.

- sp:

  A character string providing the AAGI strategic partner that is
  generating the report, can be full name or two letter string used by
  AAGI.

## Value

A `list` object that contains tables of IPPO registers organised by AAGI
Service and Support project or AAGI R&D Activity.

## See also

Other reporting:
[`create_ippo_report()`](https://aagi-aus.github.io/rippo/reference/create_ippo_report.md)

## Examples

``` r
if (FALSE) { # interactive()
 # for macOS
 library(fs)
 R_drive <- "/Volumes/dmp/A-J/AAGI_CCDM_CBADA-GIBBEM-SE21982/"
 list_ippo_tables(
   dir_path_in = path(R_drive, "Projects"),
   sp = "CU"
 )
}
```
