
library(tidygeocoder)
library(leaflet)
library(leaflet.tzh)
library(sf)


expect_no_error(object = {
  tidygeocoder::louisville |>
    geocode(
      street = street, city = city, state = state,
      postalcode = zip, method = 'census'
    ) |>
    st_as_sf(coords = c('longitude', 'latitude'), crs = 4326) |>
    leaflet_minimal() |>
    addMarkers(
      popup = ~ street,
      clusterOptions = markerClusterOptions()
    ) |>
    print()
})




expect_no_error(
  object = {
    data.frame(
      latitude = c(38.895865, 43.6534817),
      longitude = c(-77.0307713, -79.3839347)
    ) |>
      reverse_geocode(
        lat = latitude, long = longitude, method = 'osm'
      ) |>
      st_as_sf(coords = c('longitude', 'latitude'), crs = 4326) |>
      leaflet_minimal() |>
      addMarkers(
        popup = ~ address,
        clusterOptions = markerClusterOptions()
      )
  }
)
