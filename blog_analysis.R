library(tidyverse)
library(highcharter)
library(lubridate)
library(janitor)
library(here)
library(tidymodels)
library(htmlwidgets)

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
upload_to_ja_site <- function(.chart, .fn) {
  fn <- .fn
  htmlwidgets::saveWidget(.chart, here::here("figures",fn), selfcontained = TRUE)
  user <- "maps@januaryadvisors.com"
  pwd <- "RFubsvfhb4NjYpGs"
  RCurl::ftpUpload(what = here::here("figures",fn), 
                   to = paste("ftp://januaryadvisors.com:21/",fn,sep=""), userpwd = paste(user, pwd, sep = ":"))
}

distancelm <- tibble(x = c(0, 26.2),
                     y = c(5, 30))

blog1 <- shannon_pred %>% 
  hchart("scatter", hcaes(x = distance, y = kudos_count)) %>% 
  hc_plotOptions(scatter = list(jitter = list(x = 0.2, y = 0.2),
                                opacity = 0.8)) %>% 
  hc_add_series(distancelm, "line", hcaes(x = x, y = y)) %>% 
  hc_tooltip(formatter = JS("function(){
                                return (this.point.name + 
                            ' <br> actual kudos: ' + this.point.kudos_count +
                            ' <br> predicted kudos: ' + this.point.kudos_predict + 
                            ' <br>' + this.point.distance + 'mi, ' + this.point.date +
                            ' <br>' + this.point.total_photo_count + ' photos, ' +
                            this.point.achievement_count + ' achievements, ' +
                            this.point.min_per_mile + ' min/mi' 
                            )}")) %>%
  hc_xAxis(title = list(text = "distance (mi)")) %>% 
  hc_yAxis(title = list(text = "kudo count")) %>%
  hc_add_theme(strava_hc) %>% 
  hc_title(text = "Each mile earns on average 0.96 kudos") %>% 
  hc_subtitle(text = "Each point represents one run - hover over a point for data about that run")
upload_to_ja_site(blog1, "ja-strava-chart1.html")

photoslm <- tibble(x = c(0, 9),
                   y = c(8, 20))
blog2 <- shannon_pred %>% 
  hchart("scatter", hcaes(x = total_photo_count, y = kudos_count)) %>% 
  hc_plotOptions(scatter = list(jitter = list(x = 0.45, y = 0.2),
                                opacity = 0.8)) %>% 
  hc_add_series(photoslm, "line", hcaes(x = x, y = y)) %>% 
  hc_tooltip(formatter = JS("function(){
                                return (this.point.name + 
                            ' <br> actual kudos: ' + this.point.kudos_count +
                            ' <br> predicted kudos: ' + this.point.kudos_predict + 
                            ' <br>' + this.point.distance + 'mi, ' + this.point.date +
                            ' <br>' + this.point.total_photo_count + ' photos, ' +
                            this.point.achievement_count + ' achievements, ' +
                            this.point.min_per_mile + ' min/mi' 
                            )}")) %>%
  hc_xAxis(title = list(text = "photo count")) %>% 
  hc_yAxis(title = list(text = "kudo count")) %>%
  hc_add_theme(strava_hc) %>% 
  hc_title(text = "A picture is worth 1,000 words and 1.4 kudos") %>% 
  hc_subtitle(text = "Each point represents one run - hover over a point for data about that run")
blog2
upload_to_ja_site(blog2, "ja-strava-chart2.html")

blog3 <- shannon_pred %>% 
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
  hc_xAxis(title = list(text = "actual kudo count"), min = 0, max = 40) %>% 
  hc_yAxis(title = list(text = "predicted kudo count"), min = 0, max = 40) %>%
  hc_add_theme(strava_hc) %>% 
  hc_title(text = "The model explains 53% of kudo variation for my runs") %>% 
  hc_subtitle(text = "For a perfectly predictive model, all points would fall on the 1:1 line") %>% 
  hc_annotations(
    list(
      labels = list(
        list(point = list(x = 13, y = 20, xAxis = 0, yAxis = 0), text = "Points above the line represent runs<br>where I got fewer kudos than expected 😔"),
        list(point = list(x = 16, y = 7, xAxis = 0, yAxis = 0), text = "Points below the line represent runs<br>where I got more kudos than expected 😄")
        )))
blog3
upload_to_ja_site(blog3, "ja-strava-chart3.html")

blog4 <- gisele_pred %>% 
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
  hc_xAxis(title = list(text = "actual kudo count"), min = 0, max = 45) %>% 
  hc_yAxis(title = list(text = "predicted kudo count"), min = 0, max = 45) %>%
  hc_add_theme(strava_hc) %>% 
  hc_title(text = "Gisele tends to get more kudos than predicted") %>% 
  hc_subtitle(text = "The model is less predictive of Gisele's kudos, but still accurately predicts within a margin of error 79% of the time")
blog4
upload_to_ja_site(blog4, "ja-strava-chart4.html")
