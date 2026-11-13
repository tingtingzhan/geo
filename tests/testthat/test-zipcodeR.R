

library(sf)
library(zipcodeR)
library(leaflet)
library(leaflet.tzh)
library(tigris)
options(tigris_use_cache = TRUE)




expect_no_error(object = {
  search_county(
    county_name = 'Philadelphia',
    state_abb = 'PA'
  ) |>
    subset(
      subset = zipcode_type %in% 'Standard', 
      select = 'zipcode',
      drop = TRUE
    ) |>
    zctas(starts_with = _) |>
    st_transform(crs = 4326) |>
    leaflet_minimal() |>
    addPolygons(
      popup = ~ ZCTA5CE20,
      fillColor = 'royalblue', fillOpacity = .1, 
      weight = 2, color = 'white', opacity = 1
    ) |>
    print()
})












