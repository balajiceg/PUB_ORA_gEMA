# @author: Balaji Ramesh
# Stay point clusters for identifying group of locations around home location and work locations
# load library
library(dplyr)
library(readr)
library(lubridate)
library(haven)
library(stringr)
require(stopdetection)
library(data.table)
library(ggplot2)
library(sf)
library(mapview)
library(plotly)
library(magrittr)
# library(spatstat.geom)
library(igraph)

# eculidian_distance
euclidean_dist <- function(x1, y1, x2, y2) sqrt((x2 - x1)^2 + (y2 - y1)^2)

# radius of gyration
mRadiusOfGyration <- function(X, Y) {
  meanX <- mean(X)
  meanY <- mean(Y)
  dist2 <- (X - meanX)**2 + (Y - meanY)**2
  rog <- sqrt(sum(dist2) / length(dist2))
  return(rog)
}

# set working directory
setwd("Deduplicated_with_grped_time_interval")

# save dir
staypoint_output_dir <- ("staypoint_clusters")

csv_files <- list.files(pattern = ".csv")

thetaD <- 35
thetaT <- 5 * 60 # in seconds

# change working directory name
staypoint_output_dir <- paste0(staypoint_output_dir, "clustered_thetaD", thetaD, "_thetaT", thetaT, "_stillPtsOnly/")
dir.create(file.path(staypoint_output_dir))


for (csv_file in csv_files) {
  print(paste0("processing file: ", which(csv_file == csv_files), " / ", length(csv_files), " ========"))
  print(csv_file)
  # read participant track file ----
  part_id <- csv_file %>%
    str_replace(".csv", "") %>%
    as.numeric()

  track_df <- read_csv(csv_file, col_types = cols()) %>% arrange(timestamp_start)

  # skip participants with less than 1000 track points
  if (nrow(track_df) <= 1000) {
    next
  }

  track_sf <- track_df %>% rename("latitude" = lat, "longitude" = lon)

  # change to spatial points
  track_sf <- st_as_sf(track_sf, coords = c("longitude", "latitude"), crs = 4326, remove = F)

  # change projection
  track_sf <- track_sf %>% st_transform(st_crs("EPSG:32123"))
  # insert metric corordinates
  track_df$X <- st_coordinates(track_sf$geometry)[, "X"]
  track_df$Y <- st_coordinates(track_sf$geometry)[, "Y"]


  # histogram of speed
  # hist(track_df$intial_speed)

  # filter points with accuracy less than 2* thetaD ----
  track_df <- track_df %>% filter(mean_accuracy <= thetaD * 2)


  track_df$date <- as_date(track_df$timestamp_start) %>% as.character()


  # location replicate to duplicate the end location ----
  track_df <- track_df %>% mutate(row_id = row_number())

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
  remove(track_sf)


  track_df_emslt <- track_df %>% select(X, Y, timestamp, lon, lat)

  # stop point detection
  track_df_emslt <- track_df_emslt %>%
    rename(timestamp_clock = timestamp, longitude = lon, latitude = lat) %>%
    mutate(timestamp = as.integer(timestamp_clock)) %>%
    data.table()

  # time = 5*60 = 300 secs
  stopdetection::stopFinder(track_df_emslt, thetaD = thetaD, thetaT = thetaT)

  cluster_id <- track_df_emslt$stop_initiation_idx

  track_df["staypoint_cluster"] <- cluster_id

  # write clustered data
  # write_csv(track_df ,file = paste0(staypoint_output_dir,part_id,'_clustered.csv'))

  track_df$intial_speed[track_df$intial_speed < 0] <- 0

  # consecutive distance
  track_df %<>% mutate(consecu_dist = euclidean_dist(lag(X), lag(Y), X, Y))


  # summarize conscutive distances in clusters after removing first and last point in clusters
  summarize_cons_dist <- track_df %>%
    filter(!is.na(staypoint_cluster)) %>%
    group_by(staypoint_cluster) %>%
    slice(2:(n() - 1)) %>%
    summarise(n = n(), mean = mean(consecu_dist), sd = sd(consecu_dist), max = max(consecu_dist)) %>%
    filter(n > 10)

  # assuming poisson get the 95% upper confidence interval
  thresh_last_point <- (summarize_cons_dist$mean + 1.96 * sqrt(summarize_cons_dist$mean)) %>% max()

  # remove last points that are greater than the threshold ----
  rn_rowids <- track_df %>%
    filter(!is.na(staypoint_cluster)) %>%
    group_by(staypoint_cluster) %>%
    slice(n()) %>%
    ungroup() %>%
    filter(consecu_dist > thresh_last_point) %>%
    select(row_id)
  track_df$staypoint_cluster[track_df$row_id %in% rn_rowids$row_id] <- NA

  ## set groups with only 2 points to null ----
  cluster_pt_count <- track_df %>%
    filter(!is.na(staypoint_cluster)) %>%
    group_by(staypoint_cluster) %>%
    summarise(n = n()) %>%
    filter(n < 3)
  track_df$staypoint_cluster[track_df$staypoint_cluster %in% cluster_pt_count$staypoint_cluster] <- NA


  # remerge clusters based overlap and time ----
  # create buffer for cluster of points by using the clusters radius of gyration
  buffer_sf <- track_df %>%
    filter(!is.na(staypoint_cluster)) %>%
    group_by(staypoint_cluster) %>%
    summarise(
      rog = mRadiusOfGyration(X, Y),
      X = median(X), Y = median(Y),
      # X = mean(X), Y=mean(Y),
      timestamp_start = min(timestamp),
      timestamp_end = max(timestamp)
    ) %>%
    arrange(timestamp_start) %>%
    st_as_sf(coords = c("X", "Y"), crs = st_crs("EPSG:32123"), remove = F)

  buffer_sf <- st_buffer(buffer_sf, buffer_sf$rog)

  # mark clusters to be further merged based on intersection and time

  g <- st_intersects(buffer_sf)
  g <- graph_from_adj_list(g) %>% components()
  buffer_sf$membership <- g$membership


  # for each overlapping group of clusters
  for (group_counter in 1:max(buffer_sf$membership)) {
    group <- which(buffer_sf$membership == group_counter)
    if (length(group) < 2) next

    # get the clusters in the group
    reclustering_recs <- buffer_sf[group, ] %>%
      select(staypoint_cluster, timestamp_start, timestamp_end) %>%
      st_drop_geometry() %>%
      mutate(timestamp_start = as.integer(timestamp_start), timestamp_end = as.integer(timestamp_end))

    group_counter2 <- 0
    reclustering_recs$regrp_id <- NA
    # for each consecutive cluster check if they are seperated by a time of < thetaT/2
    for (i in 1:(nrow(reclustering_recs) - 1)) {
      if (reclustering_recs$timestamp_start[i + 1] - reclustering_recs$timestamp_end[i] <= (thetaT / 2)) {
        reclustering_recs$regrp_id[i:(i + 1)] <- group_counter2
      } else {
        group_counter2 <- group_counter2 + 1
      }
      reclustering_recs <- reclustering_recs %>%
        select(staypoint_cluster, regrp_id) %>%
        na.omit() %>%
        mutate(regrp_id = paste0(group_counter, "_", regrp_id))
    }
  }

  # remerge the clustes based on the new group ids
  if (nrow(reclustering_recs) > 0) {
    track_df <- track_df %>%
      mutate(staypoint_cluster_old = staypoint_cluster) %>%
      left_join(reclustering_recs, by = "staypoint_cluster")
    # merge to track df
    regroup_track_df <- track_df %>% filter(!is.na(regrp_id)) # %>% select(lat, lon, staypoint_cluster, regrp_id)

    # change group ids
    regroup_track_df <- regroup_track_df %>%
      group_by(regrp_id) %>%
      arrange(staypoint_cluster) %>%
      mutate(staypoint_cluster = first(staypoint_cluster)) %>%
      ungroup()

    # remerge into orginal df
    track_df <- track_df %>%
      filter(is.na(regrp_id)) %>%
      rbind(regroup_track_df) %>%
      select(-regrp_id) %>%
      arrange(timestamp) %>%
      select(-staypoint_cluster_old)
  }

  track_df %<>% select(-row_id, -consecu_dist, -end_point_dup) %>% filter(!is.na(staypoint_cluster))

  track_df <- track_df %>%
    group_by(staypoint_cluster) %>%
    mutate(cluster_time_diff_sec = as.numeric(interval(min(timestamp), max(timestamp)))) %>%
    ungroup()

  # mark error groups where the tdiff is < theataD/2----
  track_df <- track_df %>% mutate(error_grp = if_else(cluster_time_diff_sec < thetaT / 2, 1, 0))



  # write clustered data
  # write_csv(track_df ,file = paste0(staypoint_output_dir,part_id,'.csv'))

  # write grouped data as shape file
  grouped_track_sf <- track_df %>%
    st_as_sf(coords = c("X", "Y"), crs = st_crs("EPSG:32123"), remove = F) %>%
    group_by(staypoint_cluster) %>%
    summarise(
      medX = median(X), medY = median(Y),
      meanX = mean(X), meanY = mean(Y),
      mean_accuracy = mean(mean_accuracy),
      mean_initial_speed = mean(intial_speed),
      timestamp_start = min(timestamp),
      timestamp_end = max(timestamp),
      nPoints_incl_dup = sum(nDupPoint, na.rm = T),
      nPoints = n(),
      rog = mRadiusOfGyration(X, Y),
      error_grp = dplyr::first(error_grp),
      cluster_time_diff_sec = dplyr::first(cluster_time_diff_sec),
      geometry = st_combine(geometry)
    ) %>%
    arrange(timestamp_start) # %>% st_drop_geometry %>% st_as_sf(coords=c('medX','medY'), crs=st_crs('EPSG:32123'), remove=F)


  # grouped_sf$Participant_id <- part_id
  grouped_track_sf$thetaD <- thetaD
  grouped_track_sf$thetaT <- thetaT
  st_write(grouped_track_sf, dsn = paste0(staypoint_output_dir, "multipoint_clusters_all_parts.gpkg"), layer = paste0(part_id), append = F)
}
