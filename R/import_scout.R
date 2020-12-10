
#' @importFrom jsonlite read_json
#' @importFrom janitor clean_names
#' @importFrom dplyr %>% mutate
#' @importFrom lubridate as_datetime
#' @importFrom sf st_as_sf
import_scout_json <- function(jsonfile) {
    read_json(
        path           = jsonfile,
        simplifyVector = TRUE
    )                 %>%
        clean_names() %>%
        mutate(
            horodatage = as_datetime(horodatage)
        )            %>%
        st_as_sf(
            coords = c("longitude", "latitude"),
            remove = FALSE,
            crs = 4326
        )
}

#' @importFrom dplyr %>% mutate across
#' @importFrom purrr set_names
#' @importFrom lubridate as_datetime
#'
import_scout_metadata <- function(textfile) {
    suppressWarnings(
        read.delim(
            file         = textfile,
            header       = FALSE,
            sep          = ";",
            fileEncoding = "UTF-8"
        )     %>%
            set_names(
                nm = c("operator", "visit", "start", "end")
            ) %>%
            mutate(
                across(
                    c(start, end),
                    as_datetime,
                    format = "%d/%m/%Y %H:%M:%S"
                )
            )
    )

}

#' Import Scout visit exports
#'
#'
#'
#' @param zipfile
#'
#' @return
#' @export
#'
#' @examples
import_scout <- function(zipfile) {
    # uncompress the zip archive
    TempDir <- tempfile()

    unzip(
        zipfile = zipfile,
        files = c("Itineraires.json", "Releves.json", "Recap.txt"),
        exdir = TempDir
    )

    # import visit metadata
    info <- import_scout_metadata(
        textfile = file.path(TempDir, "Recap.txt")
    )

    # import trace
    trace <- import_scout_json(
        jsonfile = file.path(TempDir, "Itineraires.json")
    )

    # import records
    records <- import_scout_json(
        jsonfile = file.path(TempDir, "Releves.json")
    )

    visit <- list(
        info = info,
        trace = trace,
        records = records
    ) %>%
        add_class(
            class_name = "ScoutList"
        )

    unlink(TempDir, recursive = TRUE, force = TRUE)

    return(visit)
}
