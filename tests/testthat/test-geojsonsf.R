

library(geojsonsf)
library(leaflet)
library(leaflet.tzh)



expect_no_error(object = {
  geojsonsf::geo_melbourne |>
    geojson_sf() |>
    st_transform_all() |>
    leaflet_minimal() |>
    addPolygons(
      popup = ~ paste(SA2_NAME, '\u2605', SA3_NAME),
      fillColor = 'royalblue', fillOpacity = .1, weight = 2, color = 'white', opacity = 1
    ) |>
    print()
})
