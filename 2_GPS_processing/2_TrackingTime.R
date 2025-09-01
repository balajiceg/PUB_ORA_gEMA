# @author: Balaji Ramesh
# code to identify missigness and to calculate total tracking time between EMA
# load library
library(dplyr)
library(sf)
library(readr)
library(mapview)
library(lubridate)
library(haven)
library(stringr)
library(data.table)
library(tigris)

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


oh_bound <- states(year = 2020) %>%
  filter(STUSPS == "OH") %>%
  select(GEOID) %>%
  st_transform(crs = 4326)

# set working directory
setwd("Deduplicated_with_grped_time_interval")

# output summary file
output_file <- "tracking_time_oh_wave1.csv"
output_file1 <- "tracking_time_every15mins_oh_wave1.csv"
output_file1 <- "missingWithinEMA_marked.csv"


# read EMA survey data ----
ema_df <- read_sav("EMA.sav")

summary(ema_df)
ema_df <- ema_df %>%
  select(PartID, SStartD, SStartT, TimeOf42, RespID) %>%
  rename(part_id = PartID)

# create a full combs with all slots ----
start_dates <- ema_df %>%
  group_by(part_id) %>%
  summarise(min_date = min(SStartD))

full_combs <- data.frame(
  part_id = rep(start_dates$part_id, each = 42),
  TimeOf42 = rep(seq(1, 42), length(start_dates$part_id)),
  Time_substitue = rep(c("12:00:00", "17:00:00", "23:00:00"), 14 * length(start_dates$part_id)),
  Date_substitue = rep(start_dates$min_date, each = 42),
  DayNo = rep(rep(seq(0, 13), each = 3), length(start_dates$part_id))
) %>%
  mutate(Date_substitue = Date_substitue + DayNo) %>%
  tibble()

# join orginal em
full_combs <- full_combs %>% left_join(ema_df, c("part_id", "TimeOf42"))

# fill time
full_combs <- full_combs %>%
  mutate(
    filled = is.na(SStartD),
    SStartT = if_else(is.na(SStartT), Time_substitue, as.character(SStartT)),
    SStartD = if_else(is.na(SStartD), Date_substitue, SStartD)
  ) %>%
  select(-Time_substitue, -Date_substitue)

full_combs <- full_combs %>% mutate(SStartDT = as.POSIXct(paste0(SStartD, SStartT), tz = "UTC"))

# create summary distance file
csv_files <- list.files(pattern = ".csv")
merged_df <- data.frame()
merged_df1 <- data.frame()
for (csv_file in csv_files) {
  print(paste0("processing file: ", which(csv_file == csv_files), " / ", length(csv_files), " ========"))
  print(csv_file)
  part_id_i <- csv_file %>%
    str_replace(".csv", "") %>%
    as.numeric()

  track_df <- read_csv(csv_file, col_types = cols()) %>% arrange(timestamp_start)

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
  ## end of location replication

  # skip participants with less than 1000 track points
  if (nrow(track_df) <= 1000) {
    next
  }

  # filter points within ohio only
  track_df <- track_df %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    st_join(oh_bound) %>%
    filter(!is.na(GEOID))

  if (nrow(track_df) == 0) {
    next
  }
  # get x y coordinates after projecting
  track_df <- track_df %>% st_transform(st_crs("EPSG:32123"))
  # insert metric corordinates
  track_df$X <- st_coordinates(track_df$geometry)[, "X"]
  track_df$Y <- st_coordinates(track_df$geometry)[, "Y"]

  track_df <- track_df %>%
    st_drop_geometry() %>%
    select(-GEOID)

  # Filter participant record from ema
  part_em_df <- full_combs %>%
    filter(part_id == part_id_i) %>%
    arrange(SStartDT)

  if (nrow(part_em_df) == 0) next
  # start time
  begin_day00 <- as.POSIXct(paste0(part_em_df$SStartD[1], "00:00:00"), tz = "UTC")
  # group locations by their participant em survey submission times
  track_df$em_surv_timeSlot <- cut(track_df$timestamp, breaks = c(begin_day00, part_em_df$SStartDT), labels = part_em_df$SStartDT, include.lowest = T)

  # add date as unix time integer
  track_df$unix_t <- as.integer(track_df$timestamp)

  # compute diffence in time
  track_df <- track_df %>%
    filter(!is.na(em_surv_timeSlot)) %>%
    arrange(timestamp) %>%
    mutate(
      tdiff_sec = unix_t - lag(unix_t),
      tdiff_min_adj_dup = tdiff_sec / (nDupPoint * 60), # tdist_m = st_distance(geometry,lag(geometry),by_element = T),
      rownumber = row_number()
    )

  # summarise time difference within EMA where the time difference is < 30mins
  summ_df <- track_df %>%
    group_by(em_surv_timeSlot) %>%
    mutate(
      tdiff_sec = unix_t - lag(unix_t),
      tdiff_min_adj_dup = tdiff_sec / (nDupPoint * 60)
    ) %>%
    filter(tdiff_min_adj_dup < 30) %>%
    summarize(tracking_time_mins = sum(tdiff_sec, na.rm = T) / 60) %>%
    mutate(part_id = part_id_i) %>%
    ungroup() %>%
    mutate(temp = as.integer(as_datetime(as.character(em_surv_timeSlot)))) %>%
    select(-temp)


  # summarise every 15 minutes
  # categorize each time stamp within their respective 15 minutes
  track_df <- track_df %>% mutate(hour = hour(timestamp) +
    cut(minute(timestamp),
      breaks = c(0, 15, 30, 45, 61),
      include.lowest = T, labels = c(0, .25, .50, .75)
    ) %>%
    as.character() %>% as.numeric())

  # categorice the emas as well
  part_em_df <- part_em_df %>% mutate(
    date = SStartD,
    hour = hour(SStartDT) +
      cut(minute(SStartDT),
        breaks = c(0, 15, 30, 45, 61),
        include.lowest = T, labels = c(0, .25, .50, .75)
      ) %>%
      as.character() %>% as.numeric()
  )

  start_date <- unique(date(track_df$timestamp)) %>% min()
  end_date <- unique(date(track_df$timestamp)) %>% max()

  full_track_df <- expand.grid(
    date = seq(start_date, end_date, by = 1),
    hour = seq(0, 23.75, by = 0.25)
  ) %>%
    tibble() %>%
    arrange(date, hour)

  # summarise traking time within each 15 mintues
  summ_df1 <- track_df %>%
    arrange(timestamp) %>%
    mutate(date = date(timestamp)) %>%
    group_by(date, hour) %>%
    mutate(tdiff_sec = unix_t - lag(unix_t)) %>%
    summarize(
      tracking_time_mins = sum(tdiff_sec, na.rm = T) / 60,
      rad_gy = mRadiusOfGyration(X, Y),
      mean_accuracy = mean(mean_accuracy, na.rm = T),
      n = n(),
      # em_surv_timeSlot=first(em_surv_timeSlot),
      nDupPoint = sum(nDupPoint, na.rm = T),
      firstX = first(X), lastX = last(X),
      firstY = first(Y), lastY = last(Y),
      firstT = first(timestamp), lastT = last(timestamp)
    ) %>%
    ungroup()


  summ_df1 <- full_track_df %>% left_join(summ_df1)
  summ_df1 <- summ_df1 %>%
    left_join(part_em_df %>% select(-part_id), by = join_by(date, hour)) %>%
    mutate(part_id = part_id_i)

  merged_df <- rbind(merged_df, summ_df)
  merged_df1 <- rbind(merged_df1, summ_df1)
}

write_csv(merged_df1, file = output_file1, append = F)

merged_df <- merged_df %>% mutate(SStartDT = as.POSIXct(em_surv_timeSlot, tz = "UTC"))

merged_df <- full_combs %>%
  left_join(merged_df, by = c("part_id", "SStartDT")) %>%
  select(-em_surv_timeSlot)

# compute survey time difference
merged_df <- merged_df %>%
  arrange(part_id, TimeOf42) %>%
  group_by(part_id) %>%
  mutate(
    survey_time_diff_mins =
      if_else(TimeOf42 == 1,
        as.numeric(difftime(SStartDT, as.POSIXct(paste0(SStartD, "23:00:00"), tz = "UTC") - hours(24), units = "mins")),
        as.numeric(difftime(SStartDT, lag(SStartDT), units = "mins"))
      )
  ) %>%
  ungroup()

write_csv(merged_df, file = output_file, append = F)


# Analysing missingness ----
library(ggplot2)
library(plotly)
library(magrittr)
library(tidyr)


track_hours <- read_csv(output_file1)
ema_df <- read_sav("EMA.sav")


part_devices <- ema_df %>%
  select(PartID, DevOS) %>%
  rename(part_id = PartID) %>%
  filter(!duplicated(.)) %>%
  filter(part_id %in% part_ids)

track_hours <- track_hours %>%
  mutate(rad_gy = round(rad_gy, 1)) %>%
  left_join(part_devices, join_by("part_id"))

track_hours <- track_hours %>% mutate(pts_freq_permin = nDupPoint / (15))

# assign missing
track_hours <- track_hours %>%
  mutate(missing = is.na(tracking_time_mins)) %>%
  arrange(part_id, date, hour) %>%
  select(part_id, everything()) %>%
  mutate(slno = row_number())

# compute distances using subset
dist_df <- track_hours %>%
  filter(!missing) %>%
  group_by(part_id) %>%
  arrange(slno) %>%
  mutate(distance = euclidean_dist(firstX, firstY, lag(lastX), lag(lastY))) %>%
  ungroup() %>%
  select(slno, distance)

track_hours <- track_hours %>% left_join(dist_df, by = "slno")

# check the histogram for distance after missing
track_hours %>%
  filter(lag(missing)) %>%
  mutate(distance = round(distance)) %>%
  filter(distance < 21) %$% table(distance)

# fill up distances
track_hours <- track_hours %>%
  group_by(part_id) %>%
  arrange(slno) %>%
  fill(distance, .direction = "up") %>%
  ungroup() %>%
  arrange(slno)

track_hours$missing %>% summary()


mean_track_outdf <- track_hours %>%
  select(part_id, RespID, TimeOf42) %>%
  filter(!duplicated(.)) %>%
  na.omit()

# reassign missing based on filled up distances and missing
# mark time slots seperated by less than a metere as not missing
track_hours <- track_hours %>%
  mutate(missing_filled = if_else(missing == T & distance <= 1, F, missing)) %>%
  replace_na(list(missing_filled = F))

# track_hours$missing_filled %>% summary

# reassing traking time based on filled missing
track_hours <- track_hours %>%
  mutate(
    filled_tracking_time_mins = if_else(!missing, tracking_time_mins,
      if_else(missing_filled, 0, 15)
    ),
    filled_15mins = if_else(missing_filled, 0, 15)
  )

# fill EMA up
track_hours <- track_hours %>%
  group_by(part_id) %>%
  arrange(slno) %>%
  fill(c(RespID, TimeOf42), .direction = "up") %>%
  ungroup() %>%
  arrange(slno)

track_hours %>%
  filter(is.na(RespID)) %>%
  nrow()


# filter only hours that ORA use is valid
track_hours_ema <- track_hours %>%
  filter(hour < 22 & hour > 5) %>%
  mutate(survey_of_day = TimeOf42 %% 3) %>%
  filter(!is.na(RespID)) %>%
  mutate(
    survey_of_day = factor(survey_of_day,
      levels = c(0, 1, 2), labels = c("evening", "morning", "afternoon")
    ),
    survey_of_day = factor(survey_of_day, levels = c("morning", "afternoon", "evening"))
  )

track_hours_ema <- track_hours_ema %>%
  group_by(part_id, survey_of_day, RespID, TimeOf42) %>%
  summarise(mean_track = sum(filled_15mins, na.rm = T) / (n() * 15)) %>%
  ungroup()

mean_track_outdf <- mean_track_outdf %>% left_join(track_hours_ema %>% select(-survey_of_day),
  by = join_by(part_id, RespID, TimeOf42)
)

# save this data frame
write_csv(mean_track_outdf, output_file2)
