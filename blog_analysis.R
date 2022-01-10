library(tidyverse)
library(highcharter)
library(lubridate)
library(janitor)
library(here)
library(tidymodels)

options(scipen = 100)

shannon_raw = read_rds(here::here("clean-data", "shannon_strava_raw.rds"))
gisele_raw = read_rds(here::here("clean-data", "gisele_strava_raw.rds"))

clean_data <- function(raw_data) {
  raw_data %>% 
    filter(type == "Run") %>%
    # format date and add other date indicators
    mutate(date = as.Date(start_date),
           week = floor_date(date, "week"),
           month = floor_date(date, "month"),
           year = as.numeric(year(date)),
           doy = yday(date),
           dow = wday(date, label = T, abbr = T)) %>% 
    # unit conversion on distance and time
    mutate(distance = round(distance / 1609.34, 1),
           moving_time = moving_time / 60) %>% 
    # bespoke variables
    mutate(emojis = str_detect(name, "[^[:ascii:]]") == T,
           race = case_when(workout_type == 1 ~ TRUE,
                            T ~ F),
           min_per_mile = round(moving_time / distance, 1),
           running_buddy = case_when(athlete_count > 1 ~ T,
                                     T ~ F)) %>% 
    select(name, kudos_count, 
           date, doy, dow, week, month, year, 
           distance, min_per_mile, race, achievement_count, suffer_score,
           running_buddy, total_photo_count, emojis
           )
}

shannon <- clean_data(shannon_raw)
gisele <- clean_data(gisele_raw)

lm_fit <- linear_reg() %>% 
  set_engine("lm") %>% 
  fit(kudos_count ~ distance + total_photo_count + year + min_per_mile + achievement_count + emojis + race + running_buddy, 
      data = shannon)
lm_fit
View(tidy(lm_fit))
summary(lm_fit$fit)

lm_factor <- linear_reg() %>% 
  set_engine("lm") %>% 
  fit(kudos_count ~ total_photo_count, data = shannon)
lm_factor
summary(lm_factor$fit)

shannon_pred_mean <- predict(lm_fit, new_data = shannon) %>% 
  rename(kudos_predict = `.pred`)
shannon_pred <- predict(lm_fit, new_data = shannon, type = "pred_int") %>% 
  rename(kudos_predict_lwr = `.pred_lower`,
         kudos_predict_upr = `.pred_upper`) %>% 
  cbind(shannon_pred_mean) %>% 
  cbind(shannon) %>% 
  mutate(kudos_predict = round(kudos_predict),
         kudos_predict_lwr = floor(kudos_predict_lwr),
         kudos_predict_upr = ceiling(kudos_predict_upr),
         correct_prediction = case_when(kudos_count >= kudos_predict_lwr & kudos_count <= kudos_predict_upr ~ T,
                                        T ~ F)) %>% 
  select(name, kudos_count, kudos_predict, kudos_predict_lwr, kudos_predict_upr, 
         correct_prediction,
         everything())
one_to_one <- tibble(x = c(-1000, 1000),
                     y = c(-1000, 1000))
shannon_pred %>% 
  hchart("scatter", hcaes(x = kudos_count, y = kudos_predict)) %>% 
  hc_add_series(one_to_one, "line", hcaes(x = x, y = y)) %>% 
  hc_plotOptions(scatter = list(jitter = list(x = 0.2, y = 0.2),
                                opacity = 0.8)) %>% 
  hc_tooltip(formatter = JS("function(){
                                return (this.point.name + 
                            ' <br> actual kudos: ' + this.point.kudos_count +
                            ' <br> predicted kudos: ' + this.point.kudos_predict + 
                            ' <br>' + this.point.distance + 'mi, ' + this.point.date +
                            ' <br>' + this.point.total_photo_count + ' photos, ' +
                            this.point.achievement_count + ' achievements, ' +
                            this.point.min_per_mile + ' min/mi' 
                            )}")) %>%
  hc_xAxis(title = list(text = "actual number of kudos"), min = 0, max = 40) %>% 
  hc_yAxis(title = list(text = "predicted number of kudos"), min = 0, max = 40) %>%
  hc_add_theme(strava_hc) %>% 
  hc_title(text = "The model explains 53% of kudos for Shannon's runs") %>% 
  hc_subtitle(text = "For a perfectly predictive model, all points would fall on the 1:1 line")

### test the model on gisele's data
gisele_pred_mean <- predict(lm_fit, new_data = gisele) %>% 
  rename(kudos_predict = `.pred`)
gisele_pred <- predict(lm_fit, new_data = gisele, type = "pred_int") %>% 
  rename(kudos_predict_lwr = `.pred_lower`,
         kudos_predict_upr = `.pred_upper`) %>% 
  cbind(gisele_pred_mean) %>% 
  cbind(gisele) %>% 
  mutate(kudos_predict = round(kudos_predict),
         kudos_predict_lwr = floor(kudos_predict_lwr),
         kudos_predict_upr = ceiling(kudos_predict_upr),
         correct_prediction = case_when(kudos_count >= kudos_predict_lwr & kudos_count <= kudos_predict_upr ~ T,
                                        T ~ F)) %>% 
  select(name, kudos_count, kudos_predict, kudos_predict_lwr, kudos_predict_upr, 
         correct_prediction,
         everything())

gisele_pred %>% 
  hchart("scatter", hcaes(x = kudos_count, y = kudos_predict)) %>% 
  hc_add_series(one_to_one, "line", hcaes(x = x, y = y)) %>% 
  hc_plotOptions(scatter = list(jitter = list(x = 0.2, y = 0.2),
                                opacity = 0.8)) %>% 
  hc_tooltip(formatter = JS("function(){
                                return (this.point.name + 
                            ' <br> actual kudos: ' + this.point.kudos_count +
                            ' <br> predicted kudos: ' + this.point.kudos_predict + 
                            ' <br>' + this.point.distance + 'mi, ' + this.point.date +
                            ' <br>' + this.point.total_photo_count + ' photos, ' +
                            this.point.achievement_count + ' achievements, ' +
                            this.point.min_per_mile + ' min/mi' 
                            )}")) %>%
  hc_xAxis(title = list(text = "actual number of kudos"), min = 0, max = 45) %>% 
  hc_yAxis(title = list(text = "predicted number of kudos"), min = 0, max = 45) %>%
  hc_add_theme(strava_hc) %>% 
  hc_title(text = "Gisele tends to get more kudos than predicted") 

lm_test <- linear_reg() %>% 
  set_engine("lm") %>% 
  fit(kudos_count ~ distance + total_photo_count + year + min_per_mile + achievement_count + emojis + running_buddy, 
      data = gisele)
lm_test
View(tidy(lm_test))
summary(lm_test$fit)


## race prediction
race_shannon <- tibble(distance = 26.2,
                       total_photo_count = 10,
                       year = 2021,
                       min_per_mile = 9,
                       achievement_count = 3,
                       emojis = T,
                       running_buddy = T,
                       race = T)
shannon_race_pred <- predict(lm_fit, new_data = race_shannon, type = "conf_int")

race_gisele <- tibble(distance = 26.2,
                       total_photo_count = 10,
                       year = 2021,
                       min_per_mile = 7.75,
                       achievement_count = 3,
                       emojis = T,
                       running_buddy = T,
                       race = T)
gisele_race_pred <- predict(lm_test, new_data = race_gisele)

### plots 
ggplot(shannon_pred, aes(x = distance, y = kudos_count)) + 
  geom_point(color = "white", alpha = 0.5) + 
  geom_smooth(method = "lm", color = "#d3d3d3", fill = "#d3d3d3") +
  strava_gg() +
  labs(x = "distance (mi)", y = "number of kudos",
       subtitle = "Each mile is worth 0.96 kudos")

ggplot(shannon_pred, aes(x = as.factor(year), y = kudos_count)) + 
  geom_boxplot(color = "white", fill = "#d3d3d3", alpha = 0.5) + 
  strava_gg() +
  labs(x = "year", y = "number of kudos",
       subtitle = "Each year I get more followers, which brings more kudos")

shannon_pred %>% 
  mutate(race = case_when(race == T ~ "race",
                          race == F ~ "training run"),
         race = factor(race, levels = c("training run", "race"), ordered = T)) %>% 
  ggplot(aes(x = race, y = kudos_count)) + 
  geom_boxplot(color = "white", fill = "#d3d3d3", alpha = 0.5) + 
  strava_gg() +
  labs(x = "", y = "number of kudos",
       subtitle = "Races get on average 12 more kudos than normal runs")

ggplot(shannon_pred, aes(x = suffer_score, y = kudos_count)) + 
  geom_point(color = "white", alpha = 0.5) + 
  geom_smooth(method = "lm", color = "#d3d3d3", fill = "#d3d3d3") +
  strava_gg() +
  labs(x = "suffer score", y = "number of kudos",
       subtitle = "No need to suffer - negligible effect on kudos, except in extreme cases")

ggplot(shannon_pred, aes(x = total_photo_count, y = kudos_count)) + 
  geom_jitter(color = "white", alpha = 0.5) + 
  geom_smooth(method = "lm", color = "#d3d3d3", fill = "#d3d3d3") +
  strava_gg() +
  labs(x = "number of photos uploaded", y = "number of kudos",
       subtitle = "A picture is worth 1000 words and 1.4 kudos")

shannon_pred %>% 
  mutate(emojis = case_when(emojis == T ~ "emojis",
                          emojis == F ~ "no emojis"),
         emojis = factor(emojis, levels = c("no emojis", "emojis"), ordered = T)) %>% 
  ggplot(aes(x = emojis, y = kudos_count)) + 
  geom_boxplot(color = "white", fill = "#d3d3d3", alpha = 0.5) + 
  strava_gg() +
  labs(x = "", y = "number of kudos",
       subtitle = "Emojis are far and away the easiest way to earn kudos")


