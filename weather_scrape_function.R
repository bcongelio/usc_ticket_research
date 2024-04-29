library(tidyverse)
library(httr)
library(jsonlite)
library(tidyr)

### read in stadium coordinates
stadiums <- vroom::vroom("https://raw.githubusercontent.com/bcongelio/usc_ticket_research/main/stadium_locations.csv")
OWM_API_KEY <- Sys.getenv("OWM_API_KEY")

############################
## weather data
############################

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

