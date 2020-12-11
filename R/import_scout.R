#' Import json files from Scout exports
#'
#' @param jsonfile
#'
#' @export
#'
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

#' Import Scout exports
#'
#'
#'
#' @param zipfile
#'
#' @return
#' @export
#'
#' @examples
#'
#' @importFrom dplyr group_by summarise
#' @importFrom sf st_cast
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
    waypoints <- import_scout_json(
        jsonfile = file.path(TempDir, "Itineraires.json")
    )

    # import records
    records <- import_scout_json(
        jsonfile = file.path(TempDir, "Releves.json")
    )

    # convert waypoints to trace
    traces <- waypoints %>%
        group_by(identifiant_itineraire) %>%
        summarise(.groups = "drop") %>%
        st_cast(to = "MULTILINESTRING")

    visit <- list(
        info = info,
        waypoints = waypoints,
        traces = traces,
        records = records
    ) %>%
        add_class(
            class_name = "ScoutList"
        )

    unlink(TempDir, recursive = TRUE, force = TRUE)

    return(visit)
}
