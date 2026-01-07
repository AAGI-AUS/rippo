# Create an IPPO register report for GRDC

Create an IPPO register report for GRDC

## Usage

``` r
create_ippo_report(tables_list, sp, outfile)
```

## Arguments

- tables_list:

  A nested list of IPPO register data tables for AAGI service and
  support and research and development projects for reporting generated
  with
  [`list_ippo_tables()`](https://aagi-aus.github.io/rippo/reference/list_ippo_tables.md).

- sp:

  A character string providing the AAGI strategic partner that is
  generating the report, can be full name or two letter string used by
  AAGI.

- outfile:

  A filename including the file path where the file is to be written.
  The output file is an MS Word(TM) document that follows AAGI style
  guidelines.

## Value

An invisible `NULL`, called for its side-effects of generating an MS
Word(TM) document with tables that report each project's IPPO.

## See also

Other reporting:
[`list_ippo_tables()`](https://aagi-aus.github.io/rippo/reference/list_ippo_tables.md)

## Examples

``` r
if (FALSE) { # interactive()

 # for macOS
 library(fs)
 sp <- "CU" # strategic partner for report
 R_drive <- "/Volumes/dmp/A-J/AAGI_CCDM_CBADA-GIBBEM-SE21982/"
 tl <- list_ippo_tables(
   dir_path_in = path(R_drive, "Projects"),
   sp = sp
 )
create_ippo_report(tables_list = tl,
                   sp = sp,
                   outfile = "~/AAGI-CU-IPPO-register.docx"
                   )
}
```
