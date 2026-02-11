#' Create IPPO tables list
#'
#' Traverses directories and imports IPPO register Excel(TM) workbook files and
#'  creates a list of tables corresponding to Tables 1-5 in the \acronym{AAGI}
#'  \acronym{IPPO} register for all AAGI-CU Service and Support projects.
#'
#' @param dir_path_in A string value that provides the path to the top-level
#'  directory of the R: drive holding the AAGI-CU Service and Support project
#'  files.
#' @inheritParams create_ippo_report
#'
#' @examplesIf interactive()
#'  # for macOS
#'  library(fs)
#'  R_drive <- "/Volumes/dmp/A-J/AAGI_CCDM_CBADA-GIBBEM-SE21982/"
#'  list_ippo_tables(
#'    dir_path_in = path(R_drive, "Projects"),
#'    sp = "CU"
#'  )
#'
#' @returns A `list` object that contains tables of IPPO registers organised by
#'  AAGI Service and Support project or AAGI R&D Activity.
#' @family reporting
#' @export

list_ippo_tables <- function(dir_path_in, sp) {
    if (isFALSE(fs::dir_exists(dir_path_in))) {
        cli::cli_abort("{.var dir_path_in} does not exist; cannot proceed")
    }

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

    # create a list of completed projects that are no longer actively supported
    completed <- fs::dir_ls(fs::path(dir_path_in, "02 Archived Completed"))

    # create a list of active projects and filter out non-proj dirs
    active <- fs::dir_ls(fs::path(dir_path_in))
    active <- active[!grepl("Projects/\\d{2} ", active)]
    active <- active[
        !grepl("Projects/AAGI student files", active, fixed = TRUE)
    ]
    active <- active[!grepl("Projects/AAGI_informatics", active, fixed = TRUE)]
    active <- active[
        !grepl("Projects/AAGI_student_files", active, fixed = TRUE)
    ]
    active <- active[!grepl("Projects/RiskWise Program", active, fixed = TRUE)]

    # point to the IPPO registers
    ippo_paths <- fs::path(
        c(completed, active),
        "1 Documentation"
    )
    ippo_registers <- lapply(
        X = ippo_paths,
        FUN = fs::dir_ls,
        regexp = "AAGI-CU-.*IPPO.*\\.xlsx$"
    )

    merged <- Map(
        function(x, y) if (length(x) == 0L) y else x,
        ippo_registers,
        ippo_paths
    )

    # Regular expression patterns
    naming_pattern <- "(?<=/Projects/).*?(?=/1 Documentation/)"
    merged <- merged[grep(
        sub("(.)$", "~\\1", naming_pattern),
        merged,
        invert = TRUE,
        perl = TRUE
    )]
    # Apply regex using vapply
    project_names <- vapply(
        merged,
        function(x) {
            regmatches(x, regexpr(naming_pattern, x, perl = TRUE))
        },
        character(1L)
    )
    project_names <- gsub(
        pattern = "02 Archived Completed/",
        replacement = "",
        x = project_names
    )
    project_names <- gsub(
        pattern = "/$",
        replacement = "",
        x = project_names
    )
    names(merged) <- project_names

    has_ippo <- merged[grep(".xlsx", merged)]
    no_ippo <- names(merged[names(merged) %notin% names(has_ippo)])

    table_1 <- lapply(
        X = has_ippo,
        FUN = readxl::read_excel,
        sheet = 2L,
        col_types = c("numeric", "text", "text", "date", "text")
    )
    names(table_1) <- paste(
        names(table_1),
        sprintf(
            "1. Background IP - Strategic Partner %s",
            sp
        ),
        sep = " - "
    )
    table_2 <- lapply(
        X = has_ippo,
        FUN = readxl::read_excel,
        sheet = 3L,
        col_types = c("numeric", "text", "text", "date", "text")
    )
    names(table_2) <- paste(
        names(table_2),
        "2. Background IP - GRDC",
        sep = " - "
    )
    table_3 <- lapply(
        X = has_ippo,
        FUN = readxl::read_excel,
        sheet = 4L,
        col_types = c(
            "numeric",
            "text",
            "text",
            "text",
            "date",
            "text",
            "text",
            "text"
        )
    )
    names(table_3) <- paste(
        names(table_3),
        "3. Background IP - Additional Party",
        sep = " - "
    )
    table_4 <- lapply(
        X = has_ippo,
        FUN = readxl::read_excel,
        sheet = 5L,
        col_types = c("numeric", "text", "text", "date", "text", "text")
    )
    names(table_4) <- paste(names(table_4), "4. Project Outputs", sep = " - ")
    table_5 <- lapply(
        X = has_ippo,
        FUN = readxl::read_excel,
        sheet = 6L,
        col_types = c("numeric", "text", "text", "date", "text", "text")
    )
    names(table_5) <- paste(
        names(table_5),
        "5. Project Outputs Provided to a Third Party",
        sep = " - "
    )
    tables <- c(table_1, table_2, table_3, table_4, table_5)
    tables <- tables[order(names(tables))]
    tables <- purrr::keep(tables, ~ nrow(.) > 0L) |>
        purrr::map(.f = as.data.frame)

    base_name <- sub(" - .*", "", names(tables))
    ippo_tables <- split(x = tables, f = base_name)
    ippo_tables_names <- names(ippo_tables)

    ippo_tables <- lapply(names(ippo_tables), function(group) {
        sublist <- ippo_tables[[group]]
        names(sublist) <- sub(paste0("^", group, " - "), "", names(sublist))
        return(sublist)
    })

    names(ippo_tables) <- ippo_tables_names

    return(list("ippo_tables" = ippo_tables, "No_IPPO" = no_ippo))
}
