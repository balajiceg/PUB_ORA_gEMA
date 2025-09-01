library(sf)
library(dplyr)
library(mapview)
library(tigris)

# subset and curate ESRI data
# data source: https://hub.arcgis.com/datasets/esri::usa-parks/about

us_esri_parks <- st_read("Esri_parks_2024.shp")

# subset data to Ohio alone
library(tigris)
oh_state <- counties(state = "OH") %>%
  st_union() %>%
  st_sf() %>%
  st_transform(crs = st_crs(us_esri_parks))

esri_parks <- us_esri_parks %>%
  st_join(oh_state %>% mutate(oh = 1)) %>%
  filter(oh == 1) %>%
  select(-oh)

# change multipolygon to polygon
esri_parks <- esri_parks %>% st_cast("POLYGON")

# recompute area
esri_parks <- esri_parks %>%
  mutate(Shape_Area = round(as.numeric(st_area(.)))) %>%
  mutate(SQMI = round(Shape_Area / 2.59e+6, 2))


# remove polygons greater than 40 sq.miles as most of these are big
# national forest boundries that also contain settlements
# many the rec areas inside them are available in PAD US data
esri_parks <- esri_parks %>% filter(SQMI < 40)

# make a unique ID
esri_parks <- esri_parks %>%
  mutate(FID = row_number()) %>%
  select(FID, everything())
# write to output
esri_parks %>% st_write("ESRI_ohio_subset.shp", append = F)
