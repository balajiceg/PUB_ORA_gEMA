# Find the time spent in ORA
library(dplyr)
library(readr)
library(tigris)
library(sf)
library(mapview)
library(osmdata)
library(readxl)
library(stringr)
library(dbscan)
library(lubridate)
library(lwgeom)
library(magrittr)



mRadiusOfGyration <- function(X, Y) {
  meanX <- mean(X)
  meanY <- mean(Y)
  dist2 <- (X - meanX)**2 + (Y - meanY)**2
  rog <- sqrt(sum(dist2) / length(dist2))
  return(rog)
}

# eculidian_distance
euclidean_dist <- function(x1, y1, x2, y2) sqrt((x2 - x1)^2 + (y2 - y1)^2)
# output file
output_folder <- "ORA_gEMA_clusters/" # wave change

# ema filled full combs
full_combs <- read_csv("tracking_time_oh_wave1.csv") %>%
  select(-tracking_time_mins, -survey_time_diff_mins)

# read clusters
input_location_data <- "Deduplicated_with_grped_time_interval/" # wave change

# read home staypoint clusters
home_clusters <- st_read("clustered_thetaD35_thetaT300_stillPtsOnly/polygon_merged_clusters_all_rog_based.gpkg") %>%
  filter(home_clusters == 1)

# work locations
work_clusters <- st_read("clustered_thetaD35_thetaT300_stillPtsOnly/polygon_merged_clusters_all_rog_based.gpkg") %>%
  filter(work_clusters == 1)


# osm track and trils
osm_track_trails <- st_read("sel_cols_tracks_trails.gpkg") %>%
  st_transform(st_crs("EPSG:32123"))
# single lane roads are approx 12 ft max. https://www.transportation.ohio.gov/working/engineering/roadway/manuals-standards/location-design-vol-1/0300#301RoadwayCriteria
# so put a buffer after 12ft/2 as well. 1m = 0.3048ft
osm_track_trails <- osm_track_trails %>% st_buffer(0.3048 * 6)
# 10 m buffer as that is the same applied to other greenspaces
osm_track_trails <- osm_track_trails %>% st_buffer(10)
# make union
osm_track_trails_union <- osm_track_trails %>%
  st_union() %>%
  st_as_sf()


# read green spacs
green_spaces <- st_read("ORA_ESRI_OSM_PADUS_Merged.shp")

# 10 meters buffer to capture sidewalks in park (number decided based on mean accuracy of the GPS in our sample)
green_spaces <- green_spaces %>% st_buffer(10)

# crete union
green_spaces_union <- green_spaces$geom %>%
  st_as_sf() %>%
  st_union() %>% # st_cast('POLYGON') %>%
  st_as_sf() %>%
  st_make_valid()


csv_files <- list.files(input_location_data, pattern = ".csv")
epsD <- 10 # was 15
epsT <- 10 # was 150
minPts <- 4


merge_sf <- data.frame()
points_sf <- data.frame()
for (csv_file in csv_files) {
  print(paste0("processing file: ", which(csv_file == csv_files), " / ", length(csv_files), " ========"))
  print(csv_file)
  part_id <- csv_file %>% str_remove(".csv")


  track_df <- read_csv(paste0(input_location_data, csv_file), col_types = cols())

  # based on prvious gps accuracy publications only filter points with accuracy less than or equal to 30m
  # justivication https://doi.org/10.1017/S0373463311000051   https://doi.org/10.1109/CIT.2016.94
  track_df <- track_df %>% filter(mean_accuracy <= 30)

  # only those less than 20mph ~ 8.94m/s
  track_df <- track_df %>% filter(intial_speed <= 9)

  # xx participants have data at this point
  track_df <- track_df %>% mutate(row_id = row_number())

  # duplicate the last point
  dup_track_pts <- track_df %>%
    filter(nDupPoint > 1) %>%
    mutate(timestamp_start = timestamp_stop, row_id = row_id + .1) %>%
    select(-timestamp_stop)

  track_df <- track_df %>%
    select(-timestamp_stop) %>%
    rbind(dup_track_pts) %>%
    arrange(row_id) %>%
    rename(timestamp = timestamp_start) %>%
    mutate(end_point_dup = (row_id %% 1 > 0)) %>%
    mutate(nDupPoint = if_else(end_point_dup, nDupPoint, 1))

  remove(dup_track_pts)

  # remove duplicated timestamps
  track_df <- track_df %>% filter(!duplicated(timestamp))

  # end of deduplication ----
  track_df <- track_df %>%
    arrange(timestamp) %>%
    select(lat, lon, timestamp, nDupPoint, intial_speed, mean_accuracy)

  track_sf <- track_df %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = F) %>%
    st_transform(st_crs("EPSG:32123"))

  # keep only non home intersection points
  track_sf <- track_sf %>%
    st_join(home_clusters %>% filter(participant_id == part_id) %>% select(home_clusters)) %>%
    filter(is.na(home_clusters)) %>%
    select(-home_clusters)

  # find intersection
  track_sf <- track_sf %>% st_join(green_spaces_union %>% mutate(geom_id = 1))


  # print intersection
  # print(table(track_sf$geom_id))

  # keep only intersection points
  track_sf <- track_sf %>% filter(!is.na(geom_id))

  # if no points intersect then skip ; 4 because 4point is min requreid for dbscan cluster
  if (nrow(track_sf) < 4) {
    print(paste0("no ORA intersection point left"))
    next
  }

  # insert metric corordinates
  track_sf$X <- st_coordinates(track_sf$geometry)[, "X"]
  track_sf$Y <- st_coordinates(track_sf$geometry)[, "Y"]

  # compute speed manually and subset speed points agin using computed speed
  track_sf <- track_sf %>%
    arrange(timestamp) %>%
    mutate(comp_speed = euclidean_dist(X, Y, lag(X), lag(Y)) / (as.integer(timestamp) - as.integer(lag(timestamp))))
  track_sf <- track_sf %>% filter(comp_speed <= 9) ## less than 20mph ~ 8.94m/s

  # remove points that are faster than 4m/s if they are not in tracks or trails
  # justification https://doi.org/10.1098/rsif.2012.0980  https://www.nike.com/a/running-vs-jogging-benefits
  track_sf <- track_sf %>% st_join(osm_track_trails_union %>% mutate(track_trails = 1))
  track_sf <- track_sf %>% filter((!is.na(track_trails)) | intial_speed <= 4)
  track_sf <- track_sf %>% filter((!is.na(track_trails)) | comp_speed <= 4)

  # if no points intersect then skip ; 4 because 4point is min requreid for dbscan cluster
  if (nrow(track_sf) < 4) {
    print(paste0("no ORA intersection point left"))
    next
  }

  # assign ema time slot
  full_combs_part <- full_combs[full_combs$part_id == part_id, ] %>%
    select(SStartD, SStartDT, TimeOf42) %>%
    mutate(SStartDT = as.character(SStartDT)) %>%
    arrange(TimeOf42)
  begin_day00 <- as.POSIXct(paste0(full_combs_part$SStartD, "00:00:00"), tz = "UTC")[1]
  breaks <- c(begin_day00, full_combs_part$SStartDT)
  track_sf$ema_SStartDT <- cut(track_sf$timestamp, breaks = breaks, labels = breaks[-1]) %>% as.character()
  # merge timeof42
  track_sf <- track_sf %>% left_join(full_combs_part %>% select(-SStartD), by = c("ema_SStartDT" = "SStartDT"))
  track_sf <- track_sf %>% filter(!is.na(TimeOf42))


  dbscan_res <- dbscan(track_sf %>% select(X, Y) %>% st_drop_geometry(),
    eps = epsD, minPts = minPts
  )

  track_sf$dbscan_cluster <- dbscan_res$cluster
  # remove noise point
  track_sf <- track_sf %>% filter(dbscan_cluster != 0)
  track_sf$dbscan_cluster <- paste0(track_sf$TimeOf42, "_", track_sf$dbscan_cluster)

  ## check for time continutity ----
  track_sf <- track_sf %>%
    arrange(dbscan_cluster, timestamp) %>%
    group_by(dbscan_cluster) %>%
    mutate(gtdiff = as.integer(timestamp) - as.integer(lag(timestamp))) %>%
    ungroup() %>%
    mutate(tsplit = (gtdiff > epsT) | (is.na(gtdiff)))

  indxs <- c(which(track_sf$tsplit), nrow(track_sf) + 1)

  if (length(indxs) > 1) {
    for (spliti in 1:(length(indxs) - 1)) {
      starti <- indxs[spliti]
      endi <- indxs[spliti + 1] - 1
      track_sf$dbscan_cluster[starti:endi] <- spliti
    }
  }

  # set initial speed to NA if not moving
  track_sf <- track_sf %>%
    mutate(intial_speed = if_else(intial_speed == -1, NA_real_, intial_speed)) %>%
    mutate(across(c(intial_speed, comp_speed), ~ if_else(.x == 0, NA_real_, .x)))

  grp_sf <- track_sf %>%
    group_by(dbscan_cluster) %>%
    arrange(timestamp) %>%
    mutate(tdiff = as.integer(timestamp) - lag(as.integer(timestamp))) %>%
    summarise(
      green_intrsct_tdiff_mins = sum(tdiff, na.rm = T) / 60,
      nPoints_incl_dup = sum(nDupPoint, na.rm = T),
      nPoints = n(),
      timestamp_start = min(timestamp),
      timestamp_end = max(timestamp),
      part_id = part_id,
      ema_SStartDT = first(ema_SStartDT),
      nuniqueTime42 = length(unique(TimeOf42)),
      TimeOf42 = first(TimeOf42),
      medX = median(X),
      medY = median(Y),
      sd_accuracy = sd(mean_accuracy, na.rm = T),
      mean_accuracy = mean(mean_accuracy, na.rm = T),
      mean_speed = mean(intial_speed, na.rm = T),
      mean_comp_speed = mean(comp_speed, na.rm = T),
      sd_speed = sd(intial_speed, na.rm = T),
      rog = mRadiusOfGyration(X, Y),
      firstX = first(X),
      lastX = last(X),
      firstY = first(Y),
      lastY = last(Y),
      pct_intersect_trials = sum(track_trails, na.rm = T) / nPoints
    ) %>%
    ungroup()

  # calculate displacement, and velocity
  grp_sf <- grp_sf %>% mutate(
    displacement = euclidean_dist(lastX, lastY, firstX, firstY),
    velocity = displacement / (as.integer(timestamp_end) - as.integer(timestamp_start))
  )

  merge_sf <- rbind(merge_sf, grp_sf)
  points_sf <- rbind(points_sf, track_sf %>% mutate(part_id = part_id))
}

points_sf <- points_sf %>%
  group_by(part_id, dbscan_cluster) %>%
  mutate(nPointsClust = n()) %>%
  ungroup()

# remove clusters with less than 4 points
# the clusters excluded here had a max green_intrsct_tdiff_mins of 2.6mins, median 0.02, 3rd Qu. 0.03mins
merge_sf <- merge_sf %>% filter(nPoints >= 4)
points_sf <- points_sf %>% filter(nPointsClust >= 4)


# remove clusters with velocity greaterthan xx based on intersection with trials
merge_sf <- merge_sf %>%
  filter(velocity <= 9) %>%
  filter(pct_intersect_trials > 0 | velocity <= 4)

# nclust = 10848
# form convex hull to find the density of points
# using rog
merge_sf$hullArea_m2 <- merge_sf %>%
  st_drop_geometry() %>%
  st_as_sf(coords = c("medX", "medY"), crs = st_crs("EPSG:32123")) %>%
  st_buffer(merge_sf$rog) %>%
  st_area() %>%
  as.integer()
merge_sf <- merge_sf %>% mutate(rog_density_ptsPerM2 = if_else(hullArea_m2 != 0, nPoints / hullArea_m2, -1))

# using convex hull
merge_sf$hullArea_m2 <- merge_sf %>%
  st_convex_hull() %>%
  st_area() %>%
  as.integer()
merge_sf <- merge_sf %>% mutate(hull_density_ptsPerM2 = if_else(hullArea_m2 != 0, nPoints / hullArea_m2, -1))

# remove clusters with no hull area -- most of these points are slow travel points - checked manually by taking sample
merge_sf <- merge_sf %>%
  filter(hull_density_ptsPerM2 != -1) %>%
  select(-medX, -medY, -hullArea_m2)


# compute distance to cluster from home
temp_home_sf <- merge_sf %>%
  st_drop_geometry() %>%
  select(part_id) %>%
  left_join(home_clusters %>% group_by(participant_id) %>% summarise(geom = st_union(geom)),
    by = c("part_id" = "participant_id")
  ) %>%
  st_as_sf() %>%
  select(part_id)

merge_sf$dist_to_home <- st_distance(merge_sf, temp_home_sf, by_element = T)

# remove clusters that are 15m close to home buffers and has rog <30
merge_sf <- merge_sf %>% filter((!(as.numeric(dist_to_home) < 15 & rog < 30)) | is.na(dist_to_home))
remove(temp_home_sf)


# remove clsters close to work place 15m close and has rog < 30
# remove work locations that intersect home clusters
temp_work_sf <- merge_sf %>%
  st_drop_geometry() %>%
  select(part_id) %>%
  left_join(work_clusters %>% group_by(participant_id) %>% summarise(geom = st_union(geom)),
    by = c("part_id" = "participant_id")
  ) %>%
  st_as_sf() %>%
  select(part_id)

merge_sf$dist_to_work <- st_distance(merge_sf, temp_work_sf, by_element = T)

# remove points close to work locations as we did for home locations
merge_sf <- merge_sf %>% filter((!(as.numeric(dist_to_work) < 15 & rog < 30)) | is.na(dist_to_work))


# further combine based on time and distance
combined_temp <- merge_sf %>% # st_drop_geometry %>%
  group_by(part_id) %>%
  arrange(part_id, timestamp_start) %>%
  mutate(
    dis = round(euclidean_dist(firstX, firstY, lag(lastX), lag(lastY)), 1),
    tdiff = as.integer(timestamp_start) - as.integer(lag(timestamp_end)),
    will_merge = (dis < 30 & tdiff < 900)
  ) %>%
  ungroup() %>%
  mutate(
    dbscan_cluster_new = dbscan_cluster,
    will_merge = if_else(is.na(will_merge), F, will_merge)
  ) %>%
  arrange(part_id, timestamp_start)

for (i in 2:nrow(combined_temp)) {
  combined_temp$dbscan_cluster_new[i] <- ifelse(combined_temp$will_merge[i],
    combined_temp$dbscan_cluster_new[i - 1],
    combined_temp$dbscan_cluster_new[i]
  )
}

combined_temp <- combined_temp %>%
  group_by(part_id, dbscan_cluster_new) %>%
  summarise(
    green_intrsct_tdiff_mins = sum(green_intrsct_tdiff_mins),
    timestamp_start_min = min(timestamp_start),
    n = n(),
    old_clus_ids = paste0(dbscan_cluster, collapse = ","),
    geom = st_union(geometry)
  )
combined_temp %>% nrow()

# Write cluster to file
st_write(combined_temp, paste0(output_folder, "dbscan_ora_clust_Mrgd_D10T10_w_compSpeed.gpkg"))

###################################################### --
## assign green spaces time to EM time slots ----
##################################################### --
combined_sf <- st_read(paste0(output_folder, "dbscan_ora_clust_Mrgd_D10T10_w_compSpeed.gpkg"))

# manually exclude based on manual review of clusters
merge_df <- merge_df %>% filter(man_check == 0)

# read EMA data
library(haven)
ema_df_1_raw <- read_sav("EMA.sav")

ema_df_1 <- ema_df_1_raw %>%
  mutate(part_id = as.character(PartID)) %>%
  select(part_id, SSubD, SSubT, SStartT, SStartD, Stress, WithKid, TimeOf42)

# categorize the green space time within combinations
merge_df <- merge_df %>%
  filter(part_id %in% ema_df_1$part_id) %>%
  mutate(
    timestamp_start = with_tz(timestamp_start, "UTC"),
    timestamp_end = with_tz(timestamp_end, "UTC")
  )

# check time difference between EMA completion and green space use
merge_df %>%
  mutate(tdiff = difftime(as.POSIXct(ema_SStartDT, tz = "UTC"), timestamp_start, units = "hours")) %>%
  arrange(-tdiff) %>%
  summary()

group_df <- merge_df %>%
  group_by(part_id, TimeOf42) %>%
  summarise(
    green_intrsct_tdiff_mins = sum(green_intrsct_tdiff_mins),
    timestamp_start_min = min(timestamp_start),
    timestamp_start_max = max(timestamp_start),
    n = n(),
    ema_SStartDT = first(ema_SStartDT),
    geom = st_union(geom)
  )

group_df %>%
  mutate(tdiff = difftime(as.POSIXct(ema_SStartDT, tz = "UTC"), timestamp_start_min, units = "hours")) %>%
  arrange(-tdiff) %>%
  print()

# write this to file
group_df %>% st_write(paste0(output_folder, "EMA_ora_grouped_clusters.gpkg"))

full_combs <- full_combs %>%
  mutate(part_id = as.character(part_id)) %>%
  left_join(group_df %>% select(-geom), by = c("part_id", "TimeOf42"))

# FINAL df
full_combs <- full_combs %>% select(-SStartT, -SStartD)
write_csv(full_combs, paste0(output_folder, "EMA_wise_summary.csv"), append = F)
