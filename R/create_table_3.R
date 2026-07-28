#' Create a Table of Third Party IP (Table 3) of the R packages used for an AAGI IPPO document
#'
#' Automatically recognises \R packages that are used in a project and generates
#'  Table 3 of the \acronym{IPPO} register.  Has allowances for
#'  non-\acronym{CRAN} packages, \pkg{ASReml-R} and \pkg{DiGGer} and also the
#'  Quarto software.
#'
#' @param project_path The path to the directory of the project to generate the
#'  tables for, defaults to the current working directory and recurses into
#'  subfolders.
#' @param outfile Character string that provides the file name and full path to
#'  the directory where the table will be saved. Defaults to the current
#'  project's \dQuote{1 Documentation} directory with a filename of
#'  \dQuote{table_3.csv}.
#' @param aagi_project_code The \acronym{AAGI} project code to be inserted into
#'  the table.  Defaults to the current Service and Support project code within
#'  the \acronym{AAGI} project.  Note that this is NOT the project being
#'  serviced but the code within the \acronym{AAGI} project that is allocated
#'  for Service and Support, _e.g._ "AAGI-ALL-SP-003".
#' @param quarto `Boolean` cite the Quarto software? Defaults to `FALSE`.
#' @param digger `Boolean` cite the \pkg{DiGGer} package? Defaults to `FALSE`.
#' @param asreml `Boolean` cite the \pkg{ASReml-R} package? Defaults to
#'  `FALSE`.
#' @examplesIf interactive()
#' # after opening an RStudio project for a Service and Support Analysis
#'
#' create_table_3()
#'
#' @return An invisible `NULL`, called for its side effects of writing a sheet
#'  into an Excel workbook file (the project's IPPO register) on the local disk.
#'
#' @family create IPPO
#'
#' @author Zhanglong Cao, \email{zhanglong.cao@@curtin.edu.au}, and Adam H.
#' Sparks, \email{adam.sparks@@curtin.edu.au}
#' @export

create_table_3 <- function(
    project_path = getwd(),
    outfile = "1 documentation/table_3.csv",
    aagi_project_code = "AAGI-ALL-SP-003",
    quarto = FALSE,
    digger = FALSE,
    asreml = FALSE
) {
    # check if the path is a valid directory
    if (!dir.exists(project_path)) {
        cli::cli_abort("the specified path does not exist.")
    }

    # set the working directory to the specified path
    withr::with_dir(project_path, setwd(project_path))

    ### extract package information from cran database
    db <- tools::CRAN_package_db()[, c(
        "Package",
        "Author",
        "Description",
        "License"
    )]
    pkgs <- unique(renv::dependencies()$Package)

    out <- vector(mode = "list", length = length(pkgs))

    for (pkg in pkgs) {
        pkg_info <- db[db$Package == pkg, ]
        if (nrow(pkg_info) > 0L) {
            author_clean <- gsub("\\s*<[^>]+>", "", pkg_info$Author)
            author_clean <- gsub("()", "", author_clean, fixed = TRUE)
            author_clean <- gsub(
                "\\s+",
                " ",
                gsub("\n", " ", author_clean, fixed = TRUE)
            )
            description_clean <- gsub(
                "\\s*<[^>]+>",
                "",
                pkg_info$Description
            )
            description_clean <- gsub(
                "()",
                "",
                description_clean,
                fixed = TRUE
            )
            description_clean <- gsub(
                "\\s+",
                " ",
                gsub("\n", " ", description_clean, fixed = TRUE)
            )

            out[[pkg]] <- data.frame(
                V1 = aagi_project_code,
                V2 = author_clean,
                V3 = paste0(
                    "{",
                    pkg_info$Package,
                    "} ",
                    description_clean
                ),
                V4 = as.Date("2023-07-18"),
                V5 = "",
                V6 = paste0(
                    "{",
                    pkg_info$Package,
                    "} is made freely available for use and redistribution under the ",
                    pkg_info$License,
                    " licence"
                ),
                V7 = paste0(
                    "Subject to the terms of the ",
                    pkg_info$License,
                    " licence."
                ),
                stringsAsFactors = FALSE
            )
        }
    }

    out <- dplyr::bind_rows(out)

    if (quarto) {
        quarto <- tibble::new_tibble(
            list(
                V1 = aagi_project_code,
                V2 = "Posit Software, PBC",
                V3 = "Quarto, open-source tools for scientific and technical publishing",
                V4 = as.Date("2023-07-18"),
                V5 = "",
                V6 = "Version 1.3 (and earlier) is licensed under the GNU GPL v2. Quarto version 1.4 is licensed under the MIT licence.",
                V7 = "Subject to the terms of the licence under which the version was released."
            )
        )
        out <- rbind(out, quarto)
    }

    if (digger) {
        digger <- tibble::new_tibble(
            list(
                V1 = aagi_project_code,
                V2 = "Neil Coombes [aut, ctb]",
                V3 = "{DiGGer}, searches for efficient experimental designs under specified blocking and correlation.",
                V4 = as.Date("2023-07-18"),
                V5 = "",
                V6 = "NSW DPI freeware licence",
                V7 = "Subject to the terms of the NSW DPI freeware licence."
            )
        )
        out <- rbind(out, digger)
    }

    if (asreml) {
        asremlr <- tibble::new_tibble(
            list(
                V1 = aagi_project_code,
                V2 = "VSN International Ltd",
                V3 = "{ASReml-R} Version 4 package is for conducting mixed model analysis in R.",
                V4 = as.Date("2023-07-18"),
                V5 = "",
                V6 = "Commercial",
                V7 = "Subject to the terms of the commercial licence."
            )
        )
        out <- rbind(out, asremlr)
    }

    names(out) <-
        c(
            "AAGI Project Code",
            "`Owner/s\nProvide details of owner(s) including legal entity name and ABN",
            "Description",
            "Date made available to Project",
            "Name of party making Third Party IP available (if not the owner(s))",
            "Arrangements applicable to the provision of Third Party IP for the Project",
            "Restrictions / limitations on use for dissemination or Commercialisation of Project Outputs"
        )
    return(out)
}
