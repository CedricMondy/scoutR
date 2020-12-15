#'Visualize SCOUT visits
#'
#'Visualize on an interactive {leaflet} map the different components of a SCOUT
#'visit: records, waypoints and traces.
#'
#'If both waypoints and traces are present in the `ScoutObject`, only traces are
#'visible by default (but waypoints can be made visible).
#'
#' If any pictures are associated with records, the marker is replaced by a camera and the picture can be view in a popup by clicking on the marker.
#'
#'Orthophotos, IGN maps and OpenStreetMap maps are available as base maps.
#'
#'Export of the leaflet map as a picture is possible thanks to the
#'{leaflet.extras2} `addEasyprint` function.
#'
#'@param ScoutObject a SCOUT visit imported using `scout_import_zip`
#'
#'@return a {leaflet} map
#'@export
#'
#'@references Louveaux M. (2020, Oct. 24). "Visualizing GPX hiking data and photos with leaflet". Retrieved from https://marionlouveaux.fr/blog/gpx-trancks-and-leaflet-interactive-map/.
#'
#'@importFrom purrr map_lgl
#'@importFrom leaflet leaflet addProviderTiles addPolylines addCircleMarkers
#'  addMarkers addLayersControl hideGroup iconList makeIcon
#'@importFrom leaflet.extras2 addEasyprint easyprintOptions
#'@importFrom leafpop addPopupImages
#'@importFrom glue glue
#'@importFrom dplyr filter if_else mutate
#'@importFrom stringr str_wrap
scout_view <- function(ScoutObject) {

    ScoutLayers <- names(ScoutObject)[map_lgl(ScoutObject, is_sf.c)]

    ScoutMap <- leaflet() %>%
        addProviderTiles(provider = "GeoportailFrance.orthos",
                         group = "orthophotos") %>%
        addProviderTiles(provider = "GeoportailFrance.ignMaps",
                         group = "IGN") %>%
        addProviderTiles(provider = "OpenStreetMap.France",
                         group = "OSM") %>%
        addEasyprint(options = easyprintOptions(
            sizeModes = list("A4Landscape", "A4Portrait"),
            exportOnly = TRUE,
            filename = "CarteScout"
        ))

    if ("traces" %in% ScoutLayers)
        ScoutMap <- ScoutMap %>%
        addPolylines(data = ScoutObject$traces,
                     group = "itinéraire")

    if ("waypoints" %in% ScoutLayers)
        ScoutMap <- ScoutMap %>%
        addCircleMarkers(data = ScoutObject$waypoints,
                         group = "waypoints", radius = 2, stroke = FALSE, fillOpacity = 1)

    if ("records" %in% ScoutLayers) {

        ScoutRecordsWithPictures <- ScoutObject$records %>%
            filter(!is.na(nom_fichier_photo))

        ScoutRecordsWithoutPictures <- ScoutObject$records %>%
            filter(is.na(nom_fichier_photo))

        if (nrow(ScoutRecordsWithoutPictures) > 0)
            ScoutMap <- ScoutMap %>%
            addMarkers(data = ScoutRecordsWithoutPictures,
                       group = "relevés",
                       label = ~glue("relevé n°{numero_releve}{if_else(is.na(commentaire_texte), '', paste0(': ', str_wrap(commentaire_texte, 40)))}"))

        if (nrow(ScoutRecordsWithPictures) > 0) {
            CameraIcon <- system.file("www", "Camera-Moto-icon.png",
                                      package = "scoutR") %>%
                makeIcon(iconWidth = 30) %>%
                iconList()

            ScoutMap <- ScoutMap %>%
                addMarkers(data = ScoutRecordsWithPictures,
                           group = "relevés",
                           label = ~glue("relevé n°{numero_releve}{if_else(is.na(commentaire_texte), '', paste0(': ', str_wrap(commentaire_texte, 40)))}"),
                           icon = ~CameraIcon)

            if (dir.exists("Pictures")) {
                ScoutRecordsWithPictures <- ScoutRecordsWithPictures %>%
                    mutate(picture_path = file.path("Pictures", nom_fichier_photo))

                ScoutMap <- ScoutMap %>%
                    addPopupImages(ScoutRecordsWithPictures$picture_path,
                                   width = 200,
                                   group = "relevés")
            }

        }
    }

    groups <- c(traces = "itinéraire",
                waypoints = "waypoints",
                records = "relevés") %>%
        '['(ScoutLayers)

    ScoutMap <- ScoutMap %>%
        addLayersControl(baseGroups = c("orthophotos", "IGN", "OSM"),
                         overlayGroups = unname(groups))

    if (all(c("traces", "waypoints") %in% ScoutLayers))
        ScoutMap <- ScoutMap %>%
        hideGroup("waypoints")

    ScoutMap
}
