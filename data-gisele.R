
# https://bldavies.com/blog/accessing-strava-api/
credentials = read_yaml("credentials.yml")
app = oauth_app("strava", credentials$client_id_gisele, credentials$secret_gisele)
endpoint = oauth_endpoint(
  request = NULL,
  authorize = "https://www.strava.com/oauth/authorize",
  access = "https://www.strava.com/oauth/token"
)

token = oauth2.0_token(endpoint, app, as_header = FALSE,
                       scope = "activity:read_all")

df_list = list()
i = 1
done = FALSE
while (!done) {
  req = GET(
    url = "https://www.strava.com/api/v3/athlete/activities",
    config = token,
    query = list(per_page = 200, page = i)
  )
  df_list[[i]] = fromJSON(content(req, as = "text"), flatten = TRUE)
  if (length(content(req)) < 200) {
    done = TRUE
  } else {
    i = i + 1
  }
}

df = rbind_pages(df_list)
write_rds(df, here::here("clean-data", "gisele_strava_raw.rds"))

