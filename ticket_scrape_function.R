library(tidyverse)
library(httr)
library(rvest)
library(data.table)
library(vroom)
library(tidycensus)
library(usethis)

SEATGEEK_CLIENT_ID <- Sys.getenv("SEATGEEK_CLIENT_ID")
SEATGEEK_CLIENT_SECRET <- Sys.getenv("SEATGEEK_CLIENT_SECRET")
##usethis::edit_r_environ()

daily_run_mlb <- data.frame()

get_seatgeek_data <- function() {
  
  ### scrape mlb standings
  url <- "https://www.baseball-reference.com/leagues/MLB-standings.shtml"
  
  ### rvest for scraping
  mlb_standings <- url |> 
    read_html() |> 
    html_nodes("#all_expanded_standings_overall") |> 
    html_nodes(xpath = 'comment()') |> 
    html_text() |> 
    read_html() |> 
    html_node('table') |> 
    html_table() |> 
    filter(row_number() != n()) |> 
    janitor::clean_names() |> 
    select(-rk)
  
  ### cleaning retrieve standing data
  mlb_standings <- mlb_standings |> 
    mutate(date = Sys.Date()) |> 
    rename(
      team = tm,
      wins = w,
      loss = l,
      win_pct = w_l_percent,
      streak = strk,
      runs_for_avg = r,
      runs_against_avg = ra,
      run_diff = rdiff,
      schedule_strength = sos,
      simple_rating_system = srs,
      pythagorean_wl = pyth_wl,
      pythagorean_luck = luck,
      vs_east = v_east,
      vs_central = v_cent,
      vs_west = v_west,
      interleague = inter,
      home_record = home,
      road_record = road,
      ex_innings = ex_inn,
      one_run_games = x1run,
      vs_rhp = v_rhp,
      vs_lhp = v_lhp,
      vs_winning_teams = x500,
      vs_losing_teams = x500_2,
      last_10 = last10,
      last_20 = last20,
      last_30 = last30) |> 
    select(date, everything())

  ### function for scraping seat geek data
  for(mlb_pages in 1:10) {
  
  mlb_taxonomy_url <- paste0("https://api.seatgeek.com/2/events?taxonomies.id=1010100&client_id=",SEATGEEK_CLIENT_ID,
                           "&client_secret=",SEATGEEK_CLIENT_SECRET,"&per_page=5000&page=", mlb_pages)
  
  ### gathering the data from the API
  mlb_output <- GET(url = mlb_taxonomy_url)
  
  ### parsing it with json
  mlb_parsed <- jsonlite::fromJSON(content(mlb_output, "text", encoding = "UTF-8"))
  
  ### unnesting
  mlb_dat <- mlb_parsed$events |> 
  unnest(cols = c(performers, stats), names_sep = "new") |> 
  group_by(id) |> 
  slice(1:2) |> 
  ungroup() |> 
  select(id, datetime_local, teams = performersnewname,
         performersnewid, performersnewslug, performersnewshort_name, performersnewhome_team,
         performersnewaway_team, is_open, short_title, new_listings = statsnewlisting_count,
         avg_price = statsnewaverage_price, lowest_price = statsnewlowest_price,
         highest_price = statsnewhighest_price, ticket_count = statsnewticket_count)
  
  ### data prep
  mlb_dat_out <- mlb_dat |> 
  mutate(datetime_local = lubridate::as_datetime(datetime_local),
         retrieve_date = Sys.Date(),
         retrieve_date = str_remove(retrieve_date, " UTC"),
         date = as.Date(datetime_local),
         time = format(datetime_local, format = "%I:%M %p"),
         time = gsub("^0", "", time),
         day_of_week = lubridate::wday(date, label = TRUE),
         home_team = case_when(performersnewhome_team == TRUE ~ performersnewslug),
         away_team = case_when(performersnewaway_team == TRUE ~ performersnewslug),
         season_type = case_when(short_title %like% "Spring Training" ~ "Spring Training")) |>
  filter(!short_title %like% "(Exhibition)") |> 
  mutate(season_type = case_when(!short_title %like% "Spring Training" ~ "Regular Season",
                                 TRUE ~ season_type)) |> 
  group_by(id) |> 
  mutate(home_team = first(na.omit(home_team)),
         away_team = first(na.omit(away_team))) |> 
  ungroup() |> 
  distinct(id, .keep_all = TRUE) |> 
  select(event_id = id, season_type, retrieve_date, date, time, day_of_week,
         home_team, away_team, new_listings,
         avg_price, lowest_price, highest_price, ticket_count) |> 
    mutate(retrieve_date = lubridate::ymd(retrieve_date)) |> 
    filter(home_team != "toronto-blue-jays")

  df <- data.frame(mlb_dat_out)
  
  ### mlb team name mapping
  team_name_mapping <- vroom::vroom("https://raw.githubusercontent.com/bcongelio/usc_ticket_research/main/team_name_mapping.csv")

  ### merging team mapping
  df <- df |> 
    inner_join(team_name_mapping, by = c("home_team" = "team_hyphen"))
  
  ### merging standings on day of retrieve
  df <- df |> 
    inner_join(mlb_standings, by = c("team_full_name" = "team",
                                     "retrieve_date" = "date"))
  
  ### retrieving pre-compiled census data information
  census_data <- vroom::vroom("https://raw.githubusercontent.com/bcongelio/usc_ticket_research/main/census_data_wide.csv")
  
  census_data <- census_data |> 
    mutate(team_name = case_when(
      team_name == "Diamondbacks" ~ "D-backs",
      TRUE ~ team_name))
  
  df <- df |> 
    inner_join(census_data, by = c("team_name" = "team_name"))

  daily_run_mlb <- rbind(daily_run_mlb, df)
  
   }
  
  return(daily_run_mlb)
}

daily_run <- get_seatgeek_data()

############################
## weather data
############################

### read in stadium coordinates
stadiums <- vroom::vroom("https://raw.githubusercontent.com/bcongelio/usc_ticket_research/main/stadium_locations.csv")
OWM_API_KEY <- Sys.getenv("OWM_API_KEY")

### looping through coordinates for weather data
get_daily_weather <- function() {
  weather_base_url <- "https://api.openweathermap.org/data/3.0/onecall?"
  weather_daily_run <- list() # use a list to store temporary results
  
  for(i in 1:nrow(stadiums)) {
    # Construct URL
    weather_url <- modify_url(weather_base_url, query = list(
      lat = stadiums$lat[i],
      lon = stadiums$lng[i],
      exclude = "current,minutely,hourly,alerts",
      units = "imperial",
      appid = OWM_API_KEY))
    
    # Make GET request
    response <- httr::GET(weather_url)
    
    if (status_code(response) == 200) {
      # converting unicode to character vector
      weather_raw <- rawToChar(response$content)
      weather_data_city <- jsonlite::fromJSON(weather_raw)
      df <- as.data.frame(weather_data_city)
      
      df$team <- stadiums$team[i]
      
      df <- tidyr::unpack(df, cols = 12)
      df <- tidyr::unpack(df, cols = 18, names_sep = "_")
      
      df$daily.dt <- lubridate::as_date(as_datetime(df$daily.dt))
      
      df <- df |> 
        select(date = daily.dt, team, temp = day, feels_like = daily.feels_like_day, pressure = daily.pressure,
               humidity = daily.humidity, dew_points = daily.dew_point, wind_speed = daily.wind_speed,
               clouds = daily.clouds, precip_chance = daily.pop, uv_index = daily.uvi)
      
      # Append data frame to list
      weather_daily_run[[i]] <- df
    } else {
      message("Failed to fetch data for row ", i)
    }
  }
  
  # Combine all data frames
  do.call(rbind, weather_daily_run)
}

weather_daily_run <- get_daily_weather()


############################
## air pollution
############################

get_daily_pollution <- function() {
  pollution_base_url <- "http://api.openweathermap.org/data/2.5/air_pollution/forecast?"
  pollution_daily_run <- list()
  
  for(i in 1:nrow(stadiums)) {
    pollution_url <- modify_url(pollution_base_url, query = list(
      lat = stadiums$lat[i],
      lon = stadiums$lng[i],
      appid = OWM_API_KEY))
    
    response <- httr::GET(pollution_url)
    
    if (status_code(response) == 200) {
      pollution_raw <- rawToChar(response$content)
      pollution_data_city <- jsonlite::fromJSON(pollution_raw)
      df <- as.data.frame(pollution_data_city)
      
      df$team <- stadiums$team[i]
      
      df <- tidyr::unpack(df, cols = 3:5)
      
      df$list.dt <- lubridate::as_date(as_datetime(df$list.dt))
      
      df <- df |> 
        select(aqi, everything()) |> 
        rename(carbon_monoxide = co, nitrogen_monoxide = no, nitrogen_dioxide = no2, ozone = 03, sulphur_dioxide = so2,
               fine_particles = pm2_5, coarse_particles = pm10, ammonia = nh3, date = list.dt) |> 
        group_by(team, date) |> 
        summarize(aqi = mean(aqi),
                  carbon_monoxide = mean(carbon_monoxide),
                  nitrogen_monoxide = mean(nitrogen_monoxide),
                  nitrogen_dioxide = mean(nitrogen_dioxide),
                  ozone = mean(ozone),
                  sulphur_dioxide = mean(sulphur_dioxide),
                  fine_particles = mean(fine_particles),
                  coarse_particles = mean(coarse_particles),
                  ammonia = mean(ammonia))
      
      pollution_daily_run[[i]] <- df
    } else {
      message("Failed to fetch data for row ", i)
    }
  }
  
  do.call(rbind, pollution_daily_run)
}

pollution_daily_run <- get_daily_pollution()


############################
## merging
############################

combined_weather_data <- left_join(weather_daily_run, pollution_daily_run, by = c("team", "date"))

combined_weather_data <- combined_weather_data |> 
  na.omit() |> 
  mutate(retrieve_date = Sys.Date()) |> 
  select(retrieve_date, everything()) |> 
  filter(team != "Toronto Blue Jays")

############################
## merging everything together
############################

daily_run<- daily_run |> 
  left_join(combined_weather_data, by = c("team_full_name" = "team",
                                          "date" = "date",
                                          "retrieve_date" = "retrieve_date"))

daily_run <- daily_run |> 
  na.omit()

retrieve_date <- Sys.Date() |> 
  str_remove(" UTC")

write.csv(daily_run, paste0("./daily_data/mlb_",retrieve_date,".csv"), row.names = FALSE)

