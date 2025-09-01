library(sf)
library(dplyr)
library(mapview)

# read processed and oh subsetted esri data
esri_parks <- st_read("ESRI_ohio_subset.shp") %>%
  st_transform(st_crs("EPSG:32123"))

# read curated PADUSAR data
padusar <- st_read("PADUS4_AR.shp") %>%
  st_transform(st_crs("EPSG:32123"))

# read osm cleaned polygons and line
osm_track_trails <- st_read("sel_cols_tracks_trails.gpkg") %>%
  st_transform(st_crs("EPSG:32123"))
osm_polys <- st_read("sel_cols_req_polys_final.gpkg") %>%
  st_transform(st_crs("EPSG:32123"))

st_geometry(osm_polys) <- "geometry" # rename
st_geometry(osm_track_trails) <- "geometry" # rename

# single lane roads are approx 12 ft max. https://www.transportation.ohio.gov/working/engineering/roadway/manuals-standards/location-design-vol-1/0300#301RoadwayCriteria
# so put a buffer after 12ft/2 as well. 1m = 0.3048ft
osm_track_trails <- osm_track_trails %>% st_buffer(0.3048 * 6)


# merge all 4 datasets
all_sf <- esri_parks %>%
  select(FID) %>%
  mutate(source = "ESRI") %>%
  rbind((padusar %>% mutate("FID" = fid, source = "PADUSAR") %>% select(FID, source))) %>%
  mutate(FID = as.character(FID)) %>%
  st_cast("MULTIPOLYGON") %>%
  rbind((osm_polys %>% mutate("FID" = osm_id, source = "OSM") %>% select(FID, source))) %>%
  rbind((osm_track_trails %>% mutate("FID" = osm_id, source = "OSM_track_trails") %>% select(FID, source)))

temp_sf <- all_sf %>%
  st_union() %>%
  st_as_sf() %>%
  st_cast("POLYGON") %>%
  mutate(poly_id = row_number())

combined_sf <- all_sf %>%
  mutate(source_fid = paste0(source, "_", FID)) %>%
  st_join(temp_sf) %>%
  group_by(poly_id) %>%
  summarise(
    sources = paste0(unique(source_fid), collapse = ","),
    n = n(),
    geometry = st_union(geometry)
  )

combined_sf <- combined_sf %>% st_cast("MULTIPOLYGON")

# write combined df

st_write(combined_sf, "ORA_ESRI_OSM_PADUS_Merged.shp", append = F)
#### end
