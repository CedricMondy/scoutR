#' Title
#'
#' @param ScoutObject
#' @param interactive
#'
#' @return
#' @export
#'
#' @examples
#'
#' @importFrom purrr map_lgl
#' @importFrom leaflet leaflet addProviderTiles addPolylines addCircleMarkers addMarkers addLayersControl hideGroup iconList makeIcon
#' @importFrom leaflet.extras2 addEasyPrint easyprintOptions
#' @importFrom glue glue
#' @importFrom dplyr filter
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

    if ("records" %in% ScoutLayers)
            ScoutMap <- ScoutMap %>%
            addMarkers(data = ScoutObject$records,
                       group = "relevés",
                       label = ~glue("relevé n°{numero_releve}{if_else(is.na(commentaire_texte), '', paste0(': ', str_wrap(commentaire_texte, 40)))}"))


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
