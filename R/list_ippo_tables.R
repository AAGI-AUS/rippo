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
#'  R_drive <- "/Volumes/dmp/A-J/AAGI_CCDM_CBADA-GIBBEM-SE21982/"
#'  list_ippo_tables(
#'    dir_path_in = fs::path(R_drive, "Projects"),
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

    validation_issues <- list()

    empty_validation <- function() {
        data.frame(
            severity = character(),
            project = character(),
            workbook = character(),
            sheet = integer(),
            table = character(),
            cell = character(),
            column = character(),
            row = integer(),
            value = character(),
            issue = character(),
            stringsAsFactors = FALSE
        )
    }

    add_validation_issue <- function(
        severity,
        project,
        workbook,
        sheet,
        table_name,
        cell = NA_character_,
        column = NA_character_,
        row = NA_integer_,
        value = NA_character_,
        issue
    ) {
        validation_issues[[length(validation_issues) + 1L]] <<- data.frame(
            severity = as.character(severity),
            project = as.character(project),
            workbook = as.character(workbook),
            sheet = as.integer(sheet),
            table = as.character(table_name),
            cell = as.character(cell),
            column = as.character(column),
            row = as.integer(row),
            value = as.character(value),
            issue = as.character(issue),
            stringsAsFactors = FALSE
        )
    }

    excel_col <- function(index) {
        out <- character(length(index))

        for (i in seq_along(index)) {
            n <- index[[i]]
            letters_out <- character()

            while (n > 0L) {
                r <- (n - 1L) %% 26L
                letters_out <- c(LETTERS[[r + 1L]], letters_out)
                n <- (n - 1L) %/% 26L
            }

            out[[i]] <- paste(letters_out, collapse = "")
        }

        out
    }

    project_name_from_dir <- function(project_dir) {
        project <- fs::path_rel(
            path = project_dir,
            start = dir_path_in
        )

        project <- as.character(project)

        project <- sub(
            pattern = "^02 Archived Completed/",
            replacement = "",
            x = project
        )

        sub(
            pattern = "/$",
            replacement = "",
            x = project
        )
    }

    parse_date_value <- function(value) {
        if (inherits(value, "Date")) {
            return(value)
        }

        if (inherits(value, c("POSIXct", "POSIXt"))) {
            return(as.Date(value))
        }

        if (is.na(value)) {
            return(as.Date(NA))
        }

        value_chr <- trimws(as.character(value))

        if (!nzchar(value_chr)) {
            return(as.Date(NA))
        }

        if (grepl("^\\d+(\\.\\d+)?$", value_chr)) {
            parsed <- suppressWarnings(
                as.Date(
                    as.numeric(value_chr),
                    origin = "1899-12-30"
                )
            )

            return(parsed)
        }

        date_formats <- c(
            "%d/%m/%Y",
            "%d/%m/%y",
            "%Y-%m-%d",
            "%d-%m-%Y",
            "%d-%m-%y",
            "%d.%m.%Y",
            "%d.%m.%y"
        )

        for (fmt in date_formats) {
            parsed <- suppressWarnings(
                as.Date(
                    value_chr,
                    format = fmt
                )
            )

            if (!is.na(parsed)) {
                return(parsed)
            }
        }

        as.Date(NA)
    }

    validate_column_count <- function(
        data,
        expected_ncol,
        project,
        workbook,
        sheet,
        table_name
    ) {
        actual_ncol <- ncol(data)

        if (!identical(actual_ncol, expected_ncol)) {
            add_validation_issue(
                severity = "error",
                project = project,
                workbook = workbook,
                sheet = sheet,
                table_name = table_name,
                value = actual_ncol,
                issue = sprintf(
                    "Expected %s columns but found %s",
                    expected_ncol,
                    actual_ncol
                )
            )
        }

        data
    }

    normalise_numeric_column <- function(
        data,
        column_index,
        project,
        workbook,
        sheet,
        table_name
    ) {
        if (!is.data.frame(data)) {
            cli::cli_abort(c(
                "normalise_numeric_column() expected a data frame.",
                "x" = "Received object class: {paste(class(data), collapse = ', ')}",
                "x" = "Received object type: {typeof(data)}"
            ))
        }

        if (ncol(data) < column_index) {
            return(data)
        }

        values <- data[[column_index]]
        column_name <- names(data)[[column_index]]
        cell_prefix <- excel_col(column_index)

        if (is.numeric(values)) {
            return(data)
        }

        out <- rep(NA_real_, length(values))

        for (i in seq_along(values)) {
            value <- values[[i]]

            if (is.na(value) || !nzchar(trimws(as.character(value)))) {
                next
            }

            value_chr <- trimws(as.character(value))

            parsed <- suppressWarnings(
                as.numeric(value_chr)
            )

            if (is.na(parsed)) {
                add_validation_issue(
                    severity = "error",
                    project = project,
                    workbook = workbook,
                    sheet = sheet,
                    table_name = table_name,
                    cell = paste0(cell_prefix, i + 1L),
                    column = column_name,
                    row = i + 1L,
                    value = value_chr,
                    issue = "Expected numeric value"
                )
            }

            out[[i]] <- parsed
        }

        data[[column_index]] <- out

        data
    }

    normalise_date_column <- function(
        data,
        column_index,
        project,
        workbook,
        sheet,
        table_name
    ) {
        if (!is.data.frame(data)) {
            cli::cli_abort(c(
                "normalise_date_column() expected a data frame.",
                "x" = "Received object class: {paste(class(data), collapse = ', ')}",
                "x" = "Received object type: {typeof(data)}"
            ))
        }

        if (ncol(data) < column_index) {
            return(data)
        }

        values <- data[[column_index]]
        column_name <- names(data)[[column_index]]
        cell_prefix <- excel_col(column_index)

        if (inherits(values, "Date")) {
            return(data)
        }

        if (inherits(values, c("POSIXct", "POSIXt"))) {
            data[[column_index]] <- as.Date(values)
            return(data)
        }

        out <- rep(as.Date(NA), length(values))

        for (i in seq_along(values)) {
            value <- values[[i]]

            if (is.na(value) || !nzchar(trimws(as.character(value)))) {
                next
            }

            value_chr <- trimws(as.character(value))

            parsed <- parse_date_value(value)

            if (is.na(parsed)) {
                add_validation_issue(
                    severity = "error",
                    project = project,
                    workbook = workbook,
                    sheet = sheet,
                    table_name = table_name,
                    cell = paste0(cell_prefix, i + 1L),
                    column = column_name,
                    row = i + 1L,
                    value = value_chr,
                    issue = "Expected date value"
                )
            }

            out[[i]] <- parsed
        }

        data[[column_index]] <- out

        data
    }

    read_ippo_sheet <- function(
        file,
        sheet,
        project,
        table_name,
        expected_ncol,
        numeric_columns = integer(),
        date_columns = integer()
    ) {
        workbook <- fs::path_file(file)

        data <- tryCatch(
            {
                readxl::read_excel(
                    path = file,
                    sheet = sheet,
                    col_types = "guess",
                    .name_repair = "minimal"
                )
            },
            error = function(e) {
                cli::cli_abort(
                    c(
                        "Failed to read IPPO register.",
                        "x" = "Project: {.val {project}}",
                        "x" = "Workbook: {.file {workbook}}",
                        "x" = "Sheet: {.val {sheet}}",
                        "x" = "Table: {.val {table_name}}",
                        "i" = "The workbook may not match the expected IPPO register template.",
                        "i" = "Original error: {conditionMessage(e)}"
                    ),
                    parent = e
                )
            }
        )

        data <- validate_column_count(
            data = data,
            expected_ncol = expected_ncol,
            project = project,
            workbook = workbook,
            sheet = sheet,
            table_name = table_name
        )

        for (column_index in numeric_columns) {
            data <- normalise_numeric_column(
                data = data,
                column_index = column_index,
                project = project,
                workbook = workbook,
                sheet = sheet,
                table_name = table_name
            )
        }

        for (column_index in date_columns) {
            data <- normalise_date_column(
                data = data,
                column_index = column_index,
                project = project,
                workbook = workbook,
                sheet = sheet,
                table_name = table_name
            )
        }

        data
    }

    read_ippo_table <- function(
        files,
        sheet,
        table_name,
        expected_ncol,
        numeric_columns = integer(),
        date_columns = integer()
    ) {
        out <- Map(
            f = function(file, project) {
                read_ippo_sheet(
                    file = file,
                    sheet = sheet,
                    project = project,
                    table_name = table_name,
                    expected_ncol = expected_ncol,
                    numeric_columns = numeric_columns,
                    date_columns = date_columns
                )
            },
            file = files,
            project = names(files)
        )

        names(out) <- paste(
            names(files),
            table_name,
            sep = " - "
        )

        out
    }

    completed_dir <- fs::path(
        dir_path_in,
        "02 Archived Completed"
    )

    completed <- if (isTRUE(fs::dir_exists(completed_dir))) {
        fs::dir_ls(
            completed_dir,
            type = "directory"
        )
    } else {
        character()
    }

    active <- fs::dir_ls(
        dir_path_in,
        type = "directory"
    )

    active_names <- fs::path_file(active)

    keep_active <- !grepl(
        pattern = "^\\d{2} ",
        x = active_names
    ) &
        !active_names %in%
            c(
                "02 Archived Completed",
                "AAGI student files",
                "AAGI_student_files",
                "AAGI_informatics",
                "RiskWise Program"
            )

    active <- active[keep_active]

    project_dirs_all <- c(
        completed,
        active
    )

    project_names_all <- project_name_from_dir(project_dirs_all)

    ippo_paths_all <- fs::path(
        project_dirs_all,
        "1 Documentation"
    )

    documentation_exists <- fs::dir_exists(ippo_paths_all)

    missing_documentation <- !documentation_exists

    if (any(missing_documentation)) {
        missing_documentation_projects <-
            project_names_all[missing_documentation]

        missing_documentation_paths <-
            ippo_paths_all[missing_documentation]

        for (i in seq_along(missing_documentation_projects)) {
            add_validation_issue(
                severity = "warning",
                project = missing_documentation_projects[[i]],
                workbook = NA_character_,
                sheet = NA_integer_,
                table_name = NA_character_,
                value = missing_documentation_paths[[i]],
                issue = "Missing 1 Documentation folder"
            )
        }
    }

    project_names <- project_names_all[documentation_exists]
    ippo_paths <- ippo_paths_all[documentation_exists]

    find_ippo_register <- function(documentation_path, project) {
        files <- fs::dir_ls(
            path = documentation_path,
            regexp = "AAGI-CU-.*IPPO.*\\.xlsx$",
            type = "file"
        )

        files <- files[
            !grepl(
                pattern = "^~\\$",
                x = fs::path_file(files)
            )
        ]

        if (length(files) > 1L) {
            cli::cli_abort(
                c(
                    "More than one IPPO register was found in a documentation folder.",
                    "x" = "Project: {.val {project}}",
                    "x" = "Folder: {.path {documentation_path}}",
                    "x" = "Files: {paste(fs::path_file(files), collapse = ', ')}",
                    "i" = "Remove, rename, or archive duplicate IPPO workbooks before proceeding."
                )
            )
        }

        files
    }

    ippo_registers <- Map(
        f = find_ippo_register,
        documentation_path = ippo_paths,
        project = project_names
    )

    merged <- Map(
        f = function(register, documentation_path) {
            if (length(register) == 0L) {
                return(as.character(documentation_path))
            }

            as.character(register)
        },
        register = ippo_registers,
        documentation_path = ippo_paths
    )

    merged <- unlist(
        merged,
        use.names = FALSE
    )

    names(merged) <- project_names

    has_ippo <- merged[
        grepl(
            pattern = "\\.xlsx$",
            x = merged,
            ignore.case = TRUE
        )
    ]

    no_ippo <- names(merged)[
        !grepl(
            pattern = "\\.xlsx$",
            x = merged,
            ignore.case = TRUE
        )
    ]

    if (length(no_ippo) > 0L) {
        for (project in no_ippo) {
            add_validation_issue(
                severity = "warning",
                project = project,
                workbook = NA_character_,
                sheet = NA_integer_,
                table_name = NA_character_,
                value = merged[[project]],
                issue = "No IPPO register workbook found"
            )
        }
    }

    table_1 <- read_ippo_table(
        files = has_ippo,
        sheet = 2L,
        table_name = sprintf(
            "1. Background IP - Strategic Partner %s",
            sp
        ),
        expected_ncol = 5L,
        numeric_columns = 1L,
        date_columns = 4L
    )

    table_2 <- read_ippo_table(
        files = has_ippo,
        sheet = 3L,
        table_name = "2. Background IP - GRDC",
        expected_ncol = 5L,
        numeric_columns = 1L,
        date_columns = 4L
    )

    table_3 <- read_ippo_table(
        files = has_ippo,
        sheet = 4L,
        table_name = "3. Background IP - Additional Party",
        expected_ncol = 8L,
        numeric_columns = 1L,
        date_columns = 5L
    )

    table_4 <- read_ippo_table(
        files = has_ippo,
        sheet = 5L,
        table_name = "4. Project Outputs",
        expected_ncol = 6L,
        numeric_columns = 1L,
        date_columns = 4L
    )

    table_5 <- read_ippo_table(
        files = has_ippo,
        sheet = 6L,
        table_name = "5. Project Outputs Provided to a Third Party",
        expected_ncol = 6L,
        numeric_columns = 1L,
        date_columns = 4L
    )

    tables <- c(
        table_1,
        table_2,
        table_3,
        table_4,
        table_5
    )

    tables <- tables[
        order(names(tables))
    ]

    tables <- Filter(
        f = function(x) {
            is.data.frame(x) && nrow(x) > 0L
        },
        x = tables
    )

    tables <- lapply(
        X = tables,
        FUN = as.data.frame
    )

    if (length(tables) > 0L) {
        base_name <- sub(
            pattern = " - .*",
            replacement = "",
            x = names(tables)
        )

        ippo_tables <- split(
            x = tables,
            f = base_name
        )

        ippo_tables_names <- names(ippo_tables)

        ippo_tables <- lapply(
            X = names(ippo_tables),
            FUN = function(group) {
                sublist <- ippo_tables[[group]]

                names(sublist) <- sub(
                    pattern = paste0("^", group, " - "),
                    replacement = "",
                    x = names(sublist)
                )

                sublist
            }
        )

        names(ippo_tables) <- ippo_tables_names
    } else {
        ippo_tables <- list()
    }

    validation <- if (length(validation_issues) > 0L) {
        do.call(
            what = rbind,
            args = validation_issues
        )
    } else {
        empty_validation()
    }

    row.names(validation) <- NULL

    return(list(
        ippo_tables = ippo_tables,
        No_IPPO = no_ippo,
        Validation = validation
    ))
}
