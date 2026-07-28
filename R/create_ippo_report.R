#' Create an IPPO register report for GRDC
#'
#' @param tables_list A nested list of IPPO register data tables for
#'  \acronym{AAGI} service and support and research and development projects for
#'  reporting generated with `list_ippo_tables()`.
#' @param sp A character string providing the AAGI strategic partner that is
#'  generating the report, can be full name or two letter string used by
#'  \acronym{AAGI}.
#' @param outfile A filename including the file path where the file is to be
#'  written. The output file is an MS Word(TM) document that follows
#'  \acronym{AAGI} style guidelines.
#'
#' @examplesIf interactive()
#'
#'  # for macOS
#'  library(fs)
#'  sp <- "CU" # strategic partner for report
#'  R_drive <- "/Volumes/dmp/A-J/AAGI_CCDM_CBADA-GIBBEM-SE21982/"
#'  tl <- list_ippo_tables(
#'    dir_path_in = path(R_drive, "Projects"),
#'    sp = sp
#'  )
#' create_ippo_report(tables_list = tl,
#'                    sp = sp,
#'                    outfile = "~/AAGI-CU-IPPO-register.docx"
#'                    )
#'
#' @returns An invisible `NULL`, called for its side-effects of generating an
#'  MS Word(TM) document with tables that report each project's IPPO.
#' @family reporting
#' @export

create_ippo_report <- function(tables_list, sp, outfile) {
    rlang::arg_match(
        arg = sp,
        values = c(
            "Adelaide University",
            "AU",
            "Curtin University",
            "CU",
            "University of Queensland",
            "UQ"
        )
    )

    ippo_list <- tables_list$ippo_tables

    # import base AAGI IPPO Word template with logos, etc.
    doc <- officer::read_docx(
        path = system.file(
            "assets/ippo_report.docx",
            package = "rippo",
            mustWork = TRUE
        )
    )

    # define global settings for flextable
    flextable::set_flextable_defaults(
        split = TRUE,
        table_align = "center",
        table.layout = "autofit",
        fmt_date = "%Y-%m-%d",
        fmt_datetime = "%Y-%m-%d",
        font.size = 10L
    )

    # Add title of register with strategic partner named
    doc <- officer::body_add_par(
        doc,
        value = sprintf(
            "Intellectual Property and Project Output (IPPO) Register (%s)",
            sp
        ),
        style = "Title"
    )

    # Loop through the nested list
    for (group_name in names(ippo_list)) {
        doc <- officer::body_add_par(
            doc,
            value = group_name,
            style = "heading 1"
        )

        for (table_name in names(ippo_list[[group_name]])) {
            doc <- officer::body_add_par(
                doc,
                value = table_name,
                style = "heading 2"
            )

            ft <- ippo_list[[group_name]][[table_name]] |>
                flextable::flextable() |>
                AAGIThemes::theme_ft_aagi()

            doc <- flextable::body_add_flextable(x = doc, value = ft)
        }
    }

    # Save the document
    print(doc, target = outfile)
    return(invisible(NULL))
}
