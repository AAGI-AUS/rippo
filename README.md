---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# rippo

<!-- badges: start -->
<!-- badges: end -->

The goal of {rippo} is to ease data entry and tracking of AAGI-CU Intellectual Property and Project Outputs (IPPO) for reporting purposes.

## Installation

You can install the development version of {rippo} like so:

```r
o <- options() # store default options

options(pkg.build_vignettes = TRUE)

if (!require("pak"))
  install.packages("pak")
pak::pak("AAGI-AUS/rippo")

options(o) # reset options
```

## Running IPPO Shiny app

{rippo} includes a Shiny web app to make interfacing with the IPPO tables simpler.
Once {rippo} is installed you can run the web app like so:

```r
library(rippo)
run_IPPO_app()
```

The app currently has the following features:
- Add background IP by specifying the relevant details.
- Add background IP from a catalog of frequently-used IP
- Add outputs and specify links between outputs and background IP
- Add correspondence related to outputs
- Load an existing IPPO registry
- Edit and delete existing entries
- Export resulting IPPO in .xlsx format

### The background IP catalogue

The current catalog contains a selection of IP from a small number of AAGI projects.
We do not expect this is a complete list of the frequently used IP.
If you believe there is IP that is used frequently by yourself and others and should be added to the catalog, submit an issue with either:
- the package name and relevant language, or
- the relevant information for the IPPO table.

### Limitations on IPPO upload

While the uploader works well on IPPO files generated using the app, older files that do not conform to the same structure can run into issues.
When uploading your existing IPPO register, be aware of the following restrictions (this list may change as further development is undertaken).


- If the description of background IP in the imported document does not match *exactly* any of the descriptions in the catalog, it may be identified as a novel piece of IP.
This can be confusing for packages where the name may be the same but they are identified as different due to discrepancies in the description.
If you specify the name of the background IP in curly braces followed by a space at the start of the description (eg. `{BIPName} Description of BIP.`), the uploader will recognise this as a name for the background IP and will check that against the existing records as well.
We recommend avoiding the use of curly braces for other purposes in the background IP descriptions.

- Dates are expected to be formatted as date objects in Excel.
If stored as text (or other) data types, this may cause issues upon loading.

- The links between outputs and background IP are expected to come as `Table X, Lines Y` where `X` is the relevant table and `Y` is a list corresponding to the rows in that table.
By default the app will save this as a list of comma separated integers (eg. `Table 1, Lines 1,2,3,4,8`), however for the purposes of reading in files it will recognise collapsed lists using colons and hyphens (eg. `Table 1 Lines 1:4, 8` and `Table 1 Lines 1-4, 8`).
Other ways of specifying this list will not be identified.

## Contributions 

All contributions are appreciated, but please make sure to follow the [Contribution Guidelines](.github/CONTRIBUTING.md).

### Code of Conduct

Please note that the {rippo} project is released with a [Contributor Code of Conduct](https://AAGI-AUS.github.io/rippo/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.
