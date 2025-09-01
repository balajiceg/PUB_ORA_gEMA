# @author: Balaji Ramesh
# usually gps traces contain huge amount of duplictae locations when the device is stationary leading to huge file size
# This code deduplicates the locations but still preserve the start and end time for those locations.
# load library
library(dplyr)
library(readr)
library(lubridate)
library(openxlsx2)
library(sf)


# output dir
output_dir <- "Deduplicated_with_grped_time_interval"

dir.create(output_dir)

# each csv file contains lat, lon, speed, timestamp, accuracy for one participant
csv_files <- list.files(pattern = ".csv") %>% sort()

## for wave2 link participant ID----
file_names <- data.frame(mw_id = gsub(".csv", "", csv_files %>% sort(), ignore.case = T))

file_names <- file_names %>%
  left_join(conv_file) %>%
  na.omit()


for (csv_file in csv_files) {
  # for(csv_file in file_names$mw_id){

  print(csv_file)

  track_df <- read_csv(paste0(csv_file, ".csv"), col_types = cols())

  # for wave AIF  renaming required columns ----
  track_df <- track_df %>% select(lat, lon, speed, timestamp, accuracy) # %>% mutate(timestamp=time)


  # arrange by time
  track_df <- track_df %>% arrange(timestamp)

  # compress
  track_df$lat_lon_chr <- paste0(as.character(track_df$lat), as.character(track_df$lon))
  # track_df$lat_lon_chr <- paste0(as.character(round(track_df$Y,2)), as.character(round(track_df$X,2)))
  rle_lengths <- rle(track_df$lat_lon_chr)$lengths

  # uncompress into dataframe
  track_df$lat_lon_seq_grp <- rep(seq(rle_lengths %>% length()), rle_lengths)


  # group by lat long seq grp and take time from and to
  grp_df <- track_df %>%
    group_by(lat_lon_seq_grp) %>%
    summarise(
      lat = first(lat), lon = first(lon), mean_accuracy = mean(accuracy),
      intial_speed = first(speed), timestamp_start = min(timestamp),
      timestamp_stop = max(timestamp),
      nDupPoint = n()
    ) %>%
    select(-lat_lon_seq_grp)
  # remove stop point when there is only one point
  grp_df$timestamp_stop[grp_df$nDupPoint == 1] <- NA


  print(dim(grp_df))
  print("--------------------")
  # write to csv_file
  csv_file <- file_names$part_id[file_names$mw_id == csv_file]
  write_csv(grp_df, paste0(output_dir, csv_file, ".csv"), append = F)
}
