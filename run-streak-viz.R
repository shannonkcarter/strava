{
  library(tidyverse)
  library(highcharter)
  library(lubridate)
  library(here)
  library(janitor)
}

df = read_rds(here::here("clean-data", "shannon_strava_raw.rds"))

strava = df %>% 
  mutate(date = as.Date(start_date_local)) %>% 
  mutate(distance = round(distance/1609, 1),
         moving_time = round(moving_time / 60, 1),
         elapsed_time = round(elapsed_time / 60, 1)) %>% 
  filter(type == "Run") %>% 
  mutate(streaking = case_when(date > "2022-09-11" ~ "yes",
                               T ~ "no")) %>% 
  select(date, name, distance, streaking, 
         moving_time, elapsed_time, total_elevation_gain, 
         achievement_count, kudos_count, comment_count, athlete_count, total_photo_count, pr_count,
         average_speed, max_speed, average_heartrate, max_heartrate, suffer_score)

streak_mileage = strava %>% 
  filter(streaking == "yes") %>% 
  summarize(total_streak_mileage = sum(distance))
