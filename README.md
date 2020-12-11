# scoutR

## SCOUT

SCOUT (Système de COllecte Universel de Terrain: universal field data collection system) is an android app developed by the French Cerema. It is designed to be an easy way to collect and organize geolocalized field data by allowing to save geographic traces and records (including text comments, photos and vocal files). Its general purpose (any type of records can be saved) and simplicity make it a good swiss knife for field data collection.

The application allows to export the saved 'visits' (waypoints, records, photos...) as a zip archive that can be shared directly from the app (that do not allow the vizualisation, only the input of the data). A Windows deskop application is also provided to import these exports and work on them.

See the [Cerema website](Système de COllecte Universel de Terrain) for more information.

## scoutR

{scoutR} aimed to provide functions to import and manipulate SCOUT visits directly in R. The package was developed using exports from SCOUT version 2.0.1.

## Installation

{scoutR} is only available from GitHub and it can be installed using {remotes}

```{r}
remotes::install_github("CedricMondy/scoutR")
```
