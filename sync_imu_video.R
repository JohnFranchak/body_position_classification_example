library(timetk)
library(here)
library(janitor)
library(slider)
library(tidyverse)
library(lubridate)
source("motion_features.R")

id <- 111
session <- 2

start_time <- "2022-06-14 09:29:00"
end_time <- "2022-06-14 10:41:00"

# READ DATA -----
# Reads infant IMU data 
read_infant_imu <- function(name) {
  name_long <- name
  name <- str_split_fixed(name, "_", n = 3) %>% as.list(.) %>%  set_names(c("side","part","signal"))
  file <- here("imu", str_glue("{name$side}_{name$part}_{name$signal}.csv"))
  col_names <- c("time", 
                 str_glue("{str_sub(name$side,1,1)}{str_sub(name$part,1,1)}{str_sub(name$signal,1,3)}_x"), 
                 str_glue("{str_sub(name$side,1,1)}{str_sub(name$part,1,1)}{str_sub(name$signal,1,3)}_y"), 
                 str_glue("{str_sub(name$side,1,1)}{str_sub(name$part,1,1)}{str_sub(name$signal,1,3)}_z"))
  assign(name_long, read_csv(file, skip = 1, col_names = col_names), envir = .GlobalEnv)
}

sensor_data <- c("left_ankle_accel", "right_ankle_accel", "left_hip_accel", "right_hip_accel", "left_ankle_gyro", "right_ankle_gyro", "left_hip_gyro","right_hip_gyro")
walk(sensor_data, ~read_infant_imu(.x))

# FIX TIMES ----
# Get timestamps into participant local datetimes
filter_and_fix_time <- function(data_string, start_time, end_time) {
  temp_ds <- get(data_string)
  fix_biostamp_time <- function(x) as_datetime((round(x/1000000, 2)), tz = "America/Los_Angeles")
  temp_ds <- temp_ds %>% mutate(time = fix_biostamp_time(time))
  temp_ds <- temp_ds %>% filter_by_time(time, start_time, end_time) 
  assign(paste0(data_string,"_filt"), temp_ds, envir = .GlobalEnv)
}

walk(sensor_data, ~ filter_and_fix_time(.x, start_time, end_time))
sdfilt <- map_chr(sensor_data, ~paste0(.x,"_filt"))

# JOIN SENSORS INTO SINGLE FRAME ----
ds <-  full_join(get(sdfilt[1]), get(sdfilt[2])) 
for (i in 3:length(sdfilt)){
  ds <- full_join(ds, get(sdfilt[i]))
}

ds <- ds %>% arrange(time)
ds  <- ds %>% mutate(across(-time, ~ts_impute_vec(.x)))

#ds %>% plot_time_series(time, laacc_x, .smooth = F, .interactive = F)

# USE INFANT SYNC POINT FROM IMU TO CORRECT ACTIVITY TIMES -----
# This annotation file contains the detected sync point time from the time series
anno <- read_csv(here("imu","study_events.csv")) %>% 
  rename_with(~ make_clean_names(.x)) %>% 
  mutate(across(start_timestamp_ms:stop_timestamp_ms, ~as_datetime((round(.x/1000, 2)), tz = "America/Los_Angeles")))
anno <- anno %>% filter_by_time(start_timestamp_ms, as_datetime(start_time) - hours(4), end_time)
sync_point <- anno %>% filter(value == "sync") %>% pull(start_timestamp_ms)

# IMPORT CODED ACTIVITY FROM DATAVYU -----
activity <- read_csv(here("video","activity.csv"),col_names = c("onset", "offset", "code"))
activity <- activity %>% mutate(across(onset:offset, ~ sync_point + seconds(.x/1000)))
valid_codes <- c("d","u","s","sr","ss","sc","w","c","p","hs","hw","l")
activity <- activity %>% filter(code %in%valid_codes)

start_time_coded <- activity %>% slice_head %>% pull(onset)
end_time_coded <- activity %>% slice_tail %>% pull(offset)

# Match activity codes to imu data based on time
ds$code <- as.character(NA)
for (i in 1:nrow(activity)) {
  ds[between_time(ds$time, activity$onset[i], activity$offset[i]),]$code <- activity$code[i]
} 

start_filt <- force_tz(as_datetime(start_time), "America/Los_Angeles")
end_filt <- force_tz(as_datetime(end_time), "America/Los_Angeles")

ds_coded <-ds %>% filter_by_time(time, .start_date = start_filt, .end_date = end_filt)

# MOTION FEATURES ------
# sliding 4 second windows every 1 second
slide <- slide_period_dfr(ds, 
                          .i = ds$time, 
                          .period = "second", 
                          .every = 2, 
                          .after = 1, 
                          .origin = start_time_coded, 
                          .complete = TRUE,
                          ~ motion_features(.x))

save(slide, session_param, file = here("synced_data", "mot_features.RData"))

