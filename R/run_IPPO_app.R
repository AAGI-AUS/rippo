#' Run the Shiny IPPO app
#'
#' Runs a Shiny app where a user can build, edit and download an IPPO registry.
#'
#' @returns An invisible `NULL`, called for its side-effects of launching a
#'  Shiny application.
#' @examples
#' \dontrun{
#' run_IPPO_app()
#' }
#' @family create IPPO
#' @export
run_IPPO_app <- function() {
    appDir <- system.file("IPPO_app", package = "rippo", mustWork = TRUE)

    if (!nzchar(appDir)) {
        cli::cli_abort(
            "Could not find {.code IPPO_app}. Try re-installing {.pkg rippo}."
        )
    } else if (interactive()) {
        ### Can add a resource path here if required using shiny::addResourcePath('path', system.file('path', package = 'rippo'))
        shiny::runApp(appDir)
    }
    (return(NULL))
}
