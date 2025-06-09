{
  library(tidyverse)
  library(highcharter)
  library(lubridate)
  library(here)
  library(janitor)
  library(chron)
  source("https://raw.githubusercontent.com/iascchen/VisHealth/master/R/calendarHeat.R")
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


# Step 1: Prepare data
tiles <- strava %>%
  mutate(
    date = as.Date(date),
    year = year(date),
    month = month(date, label = TRUE, abbr = TRUE),
    month_num = month(date),
    weekday = wday(date, label = TRUE, week_start = 1),  # Monday = 1
    weekday_num = wday(date, week_start = 1),
    week = isoweek(date)
  ) %>%
  filter(year %in% 2022:2025)

# Step 2: Create month bounding boxes
month_boxes <- tiles %>%
  group_by(year, month, month_num) %>%
  summarise(
    xmin = min(week),
    xmax = max(week) + 1,
    ymin = min(8 - weekday_num),
    ymax = max(8 - weekday_num) + 1,
    .groups = "drop"
  )

# Step 3: Plot with tile + month outlines
ggplot() +
  # Month boxes first
  geom_rect(
    data = month_boxes,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = NA,
    color = "grey30",
    size = 0.5
  ) +
  # Heatmap tiles
  geom_tile(
    data = tiles,
    aes(x = week, y = 8 - weekday_num, fill = distance),
    color = "white"
  ) +
  scale_fill_gradientn(colors = c("#D61818", "#FFAE63", "#FFFFBD", "#B5E384"), na.value = "grey90") +
  facet_wrap(~ year, ncol = 1) +
  labs(
    title = "Running Miles Calendar Heatmap",
    x = "Week of Year",
    y = NULL,
    fill = "Distance"
  ) +
  scale_y_continuous(
    breaks = 1:7,
    labels = rev(levels(tiles$weekday))
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid = element_blank()
  )


