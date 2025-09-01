# Overlay the stay point clusters on geocoded home and work locations to identify
#   staypoint clusters that are related to home and work.
# load library
library(dplyr)
library(readr)
library(lubridate)
library(stringr)
library(ggplot2)
library(sf)
library(mapview)
library(magrittr)
library(tidyr)
library(lwgeom)
library(igraph)

# eculidian_distance
euclidean_dist <- function(x1, y1, x2, y2) sqrt((x2 - x1)^2 + (y2 - y1)^2)

setwd("staypoint_clusters/")

# read files
# get sample clusters for visalization ----
df <- data.frame()
thetaT <- 5 * 60

gpkg <- paste0("clustered_thetaD35_thetaT", thetaT, "_stillPtsOnly/multipoint_clusters_all_parts.gpkg")
layers <- st_layers(dsn = gpkg)
for (layer in layers$name) {
  print(layer)
  sf_lyr <- st_read(gpkg, layer = layer, quiet = T)

  # check mutual intersection
  # find groups that are overlapping by geometry
  # sf_lyr_buffers <- sf_lyr %>% st_minimum_bounding_circle
  sf_lyr_buffers <- sf_lyr %>%
    st_centroid() %>%
    st_buffer(sf_lyr$rog)

  # plot(tsf$geometry)
  g <- st_intersects(sf_lyr_buffers)
  g <- graph_from_adj_list(g) %>% components()
  sf_lyr_buffers$membership <- g$membership

  grp_res <- sf_lyr_buffers %>%
    st_drop_geometry() %>%
    group_by(membership) %>%
    summarise(tot_time_hr = sum(cluster_time_diff_sec) / 3600, n_members = n()) %>%
    arrange(-tot_time_hr)


  nclass <- 3
  if (nrow(grp_res) < 3) nclass <- nrow(grp_res)
  grp_res$hclust_3class <- 1
  if (nclass > 1) {
    grp_res$hclust_3class <- grp_res %>%
      with(dist(tot_time_hr, "euclidean")) %>%
      hclust(method = "ward.D") %>%
      cutree(k = nclass)
  }


  merged_sf_lyr <- sf_lyr_buffers %>%
    left_join(grp_res, by = "membership") %>% # filter(hclust_3class<3) %>%
    group_by(membership) %>%
    summarise(
      n_members = n(), hclust_3class = first(hclust_3class),
      medX = median(medX), medY = median(medY),
      wmeanX = weighted.mean(meanX, 1 / rog), wmeanY = weighted.mean(meanY, 1 / rog),
      total_merged_time_sec = sum(cluster_time_diff_sec),
      thetaD = first(thetaD), thetaT = first(thetaT),
      unique_dates = length(unique(date(timestamp_start))),
      unique_days = lubridate::wday(timestamp_start, label = T) %>% unique() %>% sort() %>% paste0(collapse = ", "),
      geom = st_union(geom)
    ) %>%
    mutate(participant_id = layer)


  df <- rbind(df, merged_sf_lyr)
}
df <- df %>%
  mutate(rowid = row_number()) %>%
  relocate(rowid)

# match home locations and check ----
home_df <- df %>% filter(hclust_3class == 1)
# read home locations
home_locs <- st_read("HomeAdressGeocoded.shp") %>%
  filter(USER_parti %in% df$participant_id) %>%
  select(Match_addr, City, Region, Subregion, USER_parti) %>%
  st_transform(crs = st_crs("EPSG:32123"))

home_locs$X <- st_coordinates(home_locs)[, 1]
home_locs$Y <- st_coordinates(home_locs)[, 2]

# merge to cluster merged
home_df <- home_df %>% left_join(home_locs %>% st_drop_geometry() %>% mutate(USER_parti = as.character(USER_parti)), by = c("participant_id" = "USER_parti"))

home_df <- home_df %>% mutate(dist = euclidean_dist(X, Y, medX, medY))


# remove unwanted columns
df$home_clusters <- as.numeric(df$rowid %in% home_df$rowid)
df <- st_as_sf(df)

# write to merged clusters to file
st_write(df, dsn = paste0("clustered_thetaD35_thetaT", thetaT, "_stillPtsOnly/polygon_merged_clusters_all.gpkg"), append = F)
# st_write(df , dsn = paste0( 'clustered_thetaD35_thetaT',thetaT,'_stillPtsOnly/polygon_merged_clusters_all_rog_based.gpkg'), append = F)


# extract work locations ----
# reread
df <- st_read(dsn = paste0("clustered_thetaD35_thetaT", thetaT, "_stillPtsOnly/polygon_merged_clusters_all.gpkg"))
# read work locations
work_locs <- st_read("WorkAddresGeocoded.shp") %>%
  filter(USER_type == "work") %>%
  select(USER_name, Match_addr, City, Region, Subregion, USER_parti, ) %>%
  st_transform(crs = st_crs("EPSG:32123"))

work_locs$X <- st_coordinates(work_locs)[, 1]
work_locs$Y <- st_coordinates(work_locs)[, 2]

other_df <- df %>% filter(home_clusters != 1)
# merge to cluster merged
other_df <- other_df %>% left_join(work_locs %>% st_drop_geometry() %>% mutate(USER_parti = as.character(USER_parti)),
  by = c("participant_id" = "USER_parti")
)

other_df <- other_df %>%
  mutate(dist = euclidean_dist(X, Y, medX, medY)) %>%
  filter(!is.na(Match_addr))

# check for intersection with r emerged clusters
other_df <- other_df %>%
  st_join(work_locs %>% mutate(intersects_workloc = 1) %>% select(intersects_workloc, USER_parti)) %>%
  mutate(intersects_workloc = if_else(USER_parti == participant_id & intersects_workloc == 1, 1, 0, missing = 0))

# after manual checking most office locations were within 300m
other_df <- other_df %>% filter(dist < 300 & total_merged_time_sec > 300)

# mark these cluster on original df
df$work_clusters <- as.numeric(df$rowid %in% other_df$rowid)
df <- st_as_sf(df)
st_write(df, dsn = paste0("clustered_thetaD35_thetaT", thetaT, "_stillPtsOnly/polygon_merged_clusters_all.gpkg"), append = F)
