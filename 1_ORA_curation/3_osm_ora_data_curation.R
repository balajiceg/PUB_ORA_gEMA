library(dplyr)
library(readr)
library(tigris)
library(sf)
library(mapview)
library(osmdata)
library(readxl)
library(stringr)


# get osm data ----
## get franklin columbus shp----
oh_state <- counties(state = "OH") %>%
  st_union() %>%
  st_sf()
bbox_poly <- st_as_sfc(oh_state) %>% st_transform(crs = 4326)
bbox_poly <- st_transform(bbox_poly, crs = st_crs("EPSG:32123")) %>%
  st_buffer(1000) %>%
  st_transform(crs = 4326) # add 1 km buffer around ohio
franklin_bbox <- bbox_poly %>% st_bbox()

# download osm dataset for green spaces----
# get key and values
# provided with the codes
osm_query_keys <- read_csv("osm_greespaces_tags.csv") %>%
  filter(final_filtered == 1) %>%
  select(tag, value, feature_type, final_filtered, cleaning_comments) %>%
  arrange(cleaning_comments)
osm_query_keys$key_value <- paste0('\"', osm_query_keys$tag, '\"=\"', osm_query_keys$value, '\"')

osm_q <- opq(bbox = unname(franklin_bbox), timeout = 7200) %>%
  add_osm_features(features = osm_query_keys$key_value)

# might take upto 2hrs ; takes time to process the downloaded osm and write it
osmdata_xml(osm_q,
  quiet = F,
  filename = "OSM_GreenSpaces.osm"
)

# load the osm file
osm_data_oh <- osmdata_sf(osm_q, doc = "OSM_GreenSpaces.osm")

# remove columns in multipoly that are not in poly
col_not_in_poly <- colnames(osm_data_oh$osm_multipolygons)[!(colnames(osm_data_oh$osm_multipolygons) %in% colnames(osm_data_oh$osm_polygons))]

osm_data_oh$osm_multipolygons <- osm_data_oh$osm_multipolygons %>% select(-all_of(col_not_in_poly), leisure_3)

# create NA columns for multipolygons
osm_data_oh$osm_multipolygons[, c(colnames(osm_data_oh$osm_polygons)[!(colnames(osm_data_oh$osm_polygons) %in% colnames(osm_data_oh$osm_multipolygons))])] <- NA

# create leisure_3 in polygon
osm_data_oh$osm_polygons$leisure_3 <- NA

# merge polygon and multipolygon
all_osm_poly <- rbind(osm_data_oh$osm_polygons %>% st_cast("MULTIPOLYGON"), osm_data_oh$osm_multipolygons)

# remove columns with > 95% nulls
na_cols_prop <- all_osm_poly %>% apply(2, function(x) sum(is.na(x)) * 100 / length(x))

all_osm_poly <- all_osm_poly[, na_cols_prop != 100]

st_write(all_osm_poly, "all_columns_multi_poly.kml")

cnames_poly <- colnames(all_osm_poly)

# repeat same for multilines ----
col_not_in_lines <- colnames(osm_data_oh$osm_multilines)[!(colnames(osm_data_oh$osm_multilines) %in% colnames(osm_data_oh$osm_lines))]

osm_data_oh$osm_multilines <- osm_data_oh$osm_multilines %>% select(-all_of(col_not_in_lines))

# create NA columns for multilines
osm_data_oh$osm_multilines[, c(colnames(osm_data_oh$osm_lines)[!(colnames(osm_data_oh$osm_lines) %in% colnames(osm_data_oh$osm_multilines))])] <- NA

# merge lines and multilines
all_osm_lines <- rbind(osm_data_oh$osm_lines %>% st_cast("MULTILINESTRING"), osm_data_oh$osm_multilines)

# remove columns with > 95% nulls
na_cols_prop <- all_osm_lines %>% apply(2, function(x) sum(is.na(x)) * 100 / length(x))

all_osm_lines <- all_osm_lines[, na_cols_prop != 100]


cnames_lines <- colnames(all_osm_lines)

st_write(all_osm_lines, "all_columns_multi_lines.kml")

cnames <- c(cnames_lines, cnames_poly)

cnames[str_detect(cnames, str_c(unique(osm_query_keys$tag), collapse = "|"))]
cnames[str_detect(cnames, str_c(unique(osm_query_keys$value), collapse = "|"))]

# filter only these columns
needed_cnames <- c(
  "osm_id", "abbr_name", "agricultural", "alt_name", "alt_name_2",
  "alt_name_3", "amenity", "athletics", "baseball", "basketball",
  "bicycle", "building", "building_1", "cemetery", "community_centre",
  "construction_year", "courts", "crop", "cycleway", "cycleway_1",
  "expressway", "foot", "footway", "footway.surface", "golf", "grass",
  "grassland", "highway", "hiking", "indoor", "landcover", "landuse",
  "landuse_1", "landuse_2", "leisure", "leisure_1", "leisure_2", "leisure_3",
  "loc_name", "meadow", "name", "natural", "nature", "nature_reserve",
  "old_name", "park", "park.type", "path", "pitch", "place", "playground",
  "recreation_ground", "residential", "route", "sport", "sport_1", "sport_2",
  "sport_3", "tennis", "tracks", "type", "unsigned_ref", "url", "usage",
  "vehicle", "wetland", "zoo"
)

all_osm_poly <- all_osm_poly %>% select(any_of(needed_cnames))
all_osm_lines <- all_osm_lines %>% select(any_of(needed_cnames))

st_write(all_osm_poly, "sel_cols_polys.gpkg")
st_write(all_osm_lines, "sel_cols_lines.gpkg")

## load data from here and continue ----
# subset only trails from lines ----

all_osm_poly <- st_read("sel_cols_polys.gpkg")
all_osm_lines <- st_read("sel_cols_lines.gpkg")


match_str <- "(?i)\\btrail\\b"
trails <- all_osm_lines %>% filter(str_detect(name, match_str) |
  str_detect(alt_name, match_str) |
  str_detect(alt_name_2, match_str) |
  str_detect(loc_name, match_str) |
  str_detect(old_name, match_str))

# subset only those intersect with ohio
oh_state <- oh_state %>% st_transform(crs = st_crs(trails))

# subset trails
trails <- trails %>%
  st_join(oh_state %>% mutate(oh = 1)) %>%
  filter(oh == 1) %>%
  select(-oh)
apply(trails %>% st_drop_geometry() %>% select(-osm_id, -contains("name")), 2, function(x) {
  table(x)
})
trails <- trails %>%
  filter(!(highway %in% c(
    "construction", "pedestrian",
    "primary", "proposed", "residential",
    "secondary", "service", "steps", "tertiary"
  ))) %>%
  filter(!footway %in% c("crossing"))

# extract race tracks ----
tracks <- all_osm_lines %>% filter(leisure == "track")
tracks_trails <- rbind(tracks, trails)
# processing polygons ----
## filter only gardens from allotments
gardens <- all_osm_poly %>% filter(landuse == "allotments", str_detect(str_to_lower(name), "garden"))

# remove sport centers that are building or swimming pools
sport_centers <- all_osm_poly %>% filter(
  leisure == "sports_centre",
  sport != "swimming" | is.na(sport),
  building %in% c("", "no") | is.na(building)
)

# all other features take it as it is
all_key_values <- osm_query_keys$value
all_key_values <- all_key_values[!all_key_values %in% c("allotments", "sports_centre")]
osm_query_keys$tag %>% unique()
other_plys <- all_osm_poly %>%
  filter(landuse %in% all_key_values | landuse_1 %in% all_key_values |
    landuse_2 %in% all_key_values |
    leisure %in% all_key_values | leisure_1 %in% all_key_values |
    leisure_2 %in% all_key_values | leisure_3 %in% all_key_values |
    natural %in% all_key_values |
    sport %in% all_key_values | sport_1 %in% all_key_values |
    sport_2 %in% all_key_values | sport_3 %in% all_key_values) %>%
  filter(
    landuse != "allotments" | is.na(landuse),
    leisure != "sports_centre" | is.na(leisure)
  ) %>%
  filter(highway == "" | is.na(highway)) %>%
  filter(route == "" | is.na(route))

# merge all the required polygons
req_polys <- rbind(other_plys, gardens, sport_centers)
req_polys %>% nrow()
req_polys %>% nrow()
req_polys %>%
  select(
    starts_with("leisure"),
    starts_with("natural"),
    starts_with("landuse"),
    starts_with("sport")
  ) %>%
  st_drop_geometry() %>%
  apply(2, table)


# remove null columns
tracks_trails <- tracks_trails %>% select_if(~ !all(is.na(.)))
req_polys <- req_polys %>% select_if(~ !all(is.na(.)))


# chedk validity and filte
tracks_trails %>%
  st_is_valid() %>%
  table()
req_polys %>%
  st_is_valid() %>%
  table()
req_polys <- st_make_valid(req_polys)
req_polys <- req_polys %>% filter(st_is_valid(.))

# remove polygons that are within each other
poly_within_poly <- req_polys %>% st_within()
# -1 to remove self within
req_polys$within_another <- (poly_within_poly %>% lapply(length) %>% unlist()) - 1

# remove tracks within tracks polygons
tracks <- tracks %>% st_join(union_req_polys %>% mutate(join = 1), join = st_within)
tracks <- tracks %>% filter(is.na(join))
tracks_trails <- rbind(tracks %>% select(-join), trails)

# write out
st_write(tracks_trails, "sel_cols_tracks_trails.gpkg", append = F)
st_write(req_polys, "sel_cols_req_polys_with_covering.gpkg", append = F)

# remove polygons that are inside other polygons
st_write(req_polys %>% filter(within_another == 0), "sel_cols_req_polys_final.gpkg", append = F)

# end ----------
