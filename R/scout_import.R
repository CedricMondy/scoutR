#' Import json files from Scout exports
#'
#' Import json files containing geographic data and convert them to {sf} objects
#'
#' Geographic data (waypoints and records) are saved in json files in SCOUT
#' exports. This function use the {jsonlite} package to import them as data
#' frames and then convert them in simple features objects using the {sf}
#' package.
#'
#' @param jsonfile the path to the json file to be imported
#'
#' @return a {sf} object using WGS 84 (CRS 4326) geographic coordinate system
#'
#' @export
#'
#' @importFrom jsonlite read_json
#' @importFrom janitor clean_names
#' @importFrom dplyr %>% mutate
#' @importFrom lubridate as_datetime
#' @importFrom sf st_as_sf
scout_import_json <- function(jsonfile) {
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
scout_import_metadata <- function(textfile) {
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
#' Import Scout's export as a zip file and convert it into R objects
#'
#' The SCOUT's exports are zip archives containing json files for geographic
#' data (waypoints and records), metadata, photos... The `import_scout` function
#' imports the metadata as a data frame,  imports the json files and convert
#' them to {sf} objects.
#' Waypoints are also transformed in linear traces and returned.
#' If any pictures are present in the export, they are moved to a 'Pictures' folder in the working directory.
#'
#' @param zipfile the path to the SCOUT's zip export
#'
#' @return a named list, ofof class ScoutList, with:
#'  * info: a data.frame. The visit metadata, name of the operator, name of the visit, start ad end of the visit
#'  * waypoints: a {sf} object. The individual points periodically recorded along the visit
#'  * traces: a {sf} object. Waypoints converted to lines
#'  * records: a {sf} object. Records with the corresponding text comment and, if relevant, the name of the corresponding picture and audio files.
#'
#' @export
#'
#' @seealso [scout_import_json()], used internally in `scout_import_zip`, to import the json files from the zip export
#'
#' @importFrom dplyr group_by filter pull
#' @importFrom sf st_cast st_combine
#' @importFrom purrr walk
scout_import_zip <- function(zipfile) {
    # uncompress the zip archive
    TempDir <- tempfile()

    unzip(
        zipfile = zipfile,
        # files = c("Itineraires.json", "Releves.json", "Recap.txt"),
        exdir = TempDir
    )

    # import visit metadata
    info <- scout_import_metadata(
        textfile = file.path(TempDir, "Recap.txt")
    )

    # import trace
    waypoints <- scout_import_json(
        jsonfile = file.path(TempDir, "Itineraires.json")
    )

    # import records
    records <- scout_import_json(
        jsonfile = file.path(TempDir, "Releves.json")
    )

    # if any pictures, move them to the working directory
    if (nrow(filter(records, !is.na(nom_fichier_photo))) > 0) {
        if (!dir.exists("Pictures"))
            dir.create("Pictures")

        records %>%
            filter(!is.na(nom_fichier_photo)) %>%
            pull(nom_fichier_photo) %>%
            walk(function(path) {
                file.copy(from = file.path(TempDir, "Photos", path),
                          to = file.path("Pictures", path))
            })
    }

    # convert waypoints to trace
    traces <- waypoints %>%
        group_by(identifiant_itineraire) %>%
        st_combine() %>%
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
