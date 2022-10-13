#https://api.mapbox.com/styles/v1/shannoncarter/cl8n9axcw000614o9yt428xh5/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1Ijoic2hhbm5vbmNhcnRlciIsImEiOiJjbDhuNHQwdXYwMXdrM3BuenZ5Zmx1ODBpIn0.TnzMahnMGYR8f7WrfS10fA
library(tidyverse)
library(leaflet)
library(jaspatial)

no_labels = "https://api.mapbox.com/styles/v1/shannoncarter/cl8n9axcw000614o9yt428xh5/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1Ijoic2hhbm5vbmNhcnRlciIsImEiOiJjbDhuNHQwdXYwMXdrM3BuenZ5Zmx1ODBpIn0.TnzMahnMGYR8f7WrfS10fA"
only_labels = "https://api.mapbox.com/styles/v1/shannoncarter/cl976nzjv002s14tm2f0rzwir/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1Ijoic2hhbm5vbmNhcnRlciIsImEiOiJjbDhuNHQwdXYwMXdrM3BuenZ5Zmx1ODBpIn0.TnzMahnMGYR8f7WrfS10fA"
places_and_labels = "https://api.mapbox.com/styles/v1/emikjackson/cl976nzjv002s14tm2f0rzwir/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1Ijoic2hhbm5vbmNhcnRlciIsImEiOiJjbDhuNHQwdXYwMXdrM3BuenZ5Zmx1ODBpIn0.TnzMahnMGYR8f7WrfS10fA"
"https://api.mapbox.com/styles/v1/emikjackson/cl84mqg32001314pzakci3vf5.html?title=copy&access_token=pk.eyJ1IjoiZW1pa2phY2tzb24iLCJhIjoiY2toZjhmOW9iMG52YTJ4bHkyN21laHVpbCJ9.ac3r8JmRxMsCOc2n4HL_rg&zoomwheel=true&fresh=true#12.01/29.75195/-95.37066"
leaflet() %>%
  setView(lng = , lat = 22.3, zoom = 12) %>%
  #addProviderTiles("Stamen.TonerLite") %>%
  addTiles(
    # Add tile from mapbox style
    # https://docs.mapbox.com/studio-manual/guides/publish-your-style/
    urlTemplate = COLLISION_PTS_TILE_URL
  )


ja_base_map = function (.lon = -95.3103,
                        .lat = 29.7752,
                        .zoom_level = 9,
                        .no_labels_tiles = "https://api.mapbox.com/styles/v1/shannoncarter/cl8n9axcw000614o9yt428xh5/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1Ijoic2hhbm5vbmNhcnRlciIsImEiOiJjbDhuNHQwdXYwMXdrM3BuenZ5Zmx1ODBpIn0.TnzMahnMGYR8f7WrfS10fA",
                        .only_labels_tiles = "https://api.mapbox.com/styles/v1/shannoncarter/cl976nzjv002s14tm2f0rzwir/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1Ijoic2hhbm5vbmNhcnRlciIsImEiOiJjbDhuNHQwdXYwMXdrM3BuenZ5Zmx1ODBpIn0.TnzMahnMGYR8f7WrfS10fA")
{
  leaflet::leaflet() %>% leaflet::addTiles(
    attribution = htmltools::HTML(
      "Built by <a href='https://www.januaryadvisors.com/' target='_blank'>January Advisors</a>"
    )
  ) %>%
    leaflet::addMapPane(name = "polygons", zIndex = 410) %>%
    leaflet::addMapPane(name = "maplabels", zIndex = 420) %>%
    leaflet::addTiles(urlTemplate = .no_labels_tiles) %>%
    leaflet::addTiles(
      urlTemplate = .only_labels_tiles,
      options = leaflet::leafletOptions(pane = "maplabels"),
      group = "maplabels") %>%
    leaflet::setView(.lon,
                           .lat, zoom = .zoom_level) %>% leaflet.extras2::addEasyprint(
                             options = leaflet.extras2::easyprintOptions(
                               title = "Print map",
                               position = "topleft",
                               exportOnly = TRUE
                             )
                           ) %>% leaflet.extras::addSearchOSM(
                             options = leaflet.extras::searchFeaturesOptions(
                               zoom = 10,
                               openPopup = TRUE,
                               position = "topright",
                               hideMarkerOnCollapse = TRUE
                             )
                           )
}
tx_counties
ja_base_map() %>% 
  addPolygons(tx_counties)
