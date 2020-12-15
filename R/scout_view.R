#'Visualize SCOUT visits
#'
#'Visualize on an interactive {leaflet} map the different components of a SCOUT
#'visit: records, waypoints and traces.
#'
#'If both waypoints and traces are present in the `ScoutObject`, only traces are
#'visible by default (but waypoints can be made visible).
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
#'@importFrom purrr map_lgl
#'@importFrom leaflet leaflet addProviderTiles addPolylines addCircleMarkers
#'  addMarkers addLayersControl hideGroup iconList makeIcon
#'@importFrom leaflet.extras2 addEasyprint easyprintOptions
#'@importFrom glue glue
#'@importFrom dplyr filter if_else
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
        CameraIcon <- system.file("www", "Camera-Moto-icon.png",
                                  package = "scoutR") %>%
            makeIcon(iconWidth = 30) %>%
            iconList()

        ScoutRecordsWithPhotos <- ScoutObject$records %>%
            filter(!is.na(nom_fichier_photo))

        ScoutRecordsWithoutPhotos <- ScoutObject$records %>%
            filter(is.na(nom_fichier_photo))

        if (nrow(ScoutRecordsWithoutPhotos) > 0)
            ScoutMap <- ScoutMap %>%
            addMarkers(data = ScoutRecordsWithoutPhotos,
                       group = "relevés",
                       label = ~glue("relevé n°{numero_releve}{if_else(is.na(commentaire_texte), '', paste0(': ', str_wrap(commentaire_texte, 40)))}"))

        if (nrow(ScoutRecordsWithPhotos) > 0)
            ScoutMap <- ScoutMap %>%
            addMarkers(data = ScoutRecordsWithPhotos,
                       group = "relevés",
                       label = ~glue("relevé n°{numero_releve}{if_else(is.na(commentaire_texte), '', paste0(': ', str_wrap(commentaire_texte, 40)))}"),
                       icon = ~CameraIcon)
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
