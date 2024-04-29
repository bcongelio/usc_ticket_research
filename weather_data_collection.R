library(httr)
library(jsonlite)
library(tidyverse)
library(tidyr)

### writing openweather API key into environment
##usethis::edit_r_environ()
##Sys.getenv("OWM_API_KEY")


url <- httr::GET("https://api.openweathermap.org/data/3.0/onecall?lat=33.79&lon=-117.88&exclude=current,minutely,hourly,alerts&units=imperial&appid=19576d339d4ae6c84273069c24979b01")

### converting unicode to character vector
weather_raw <- rawToChar(url$content)
weather_data <- jsonlite::fromJSON(weather_raw)
weather_data <- as.data.frame(weather_data)

weather_data <- tidyr::unpack(weather_data, cols = 12)
weather_data <- tidyr::unpack(weather_data, cols = 18, names_sep = "_")

weather_data$daily.dt <- lubridate::as_date(as_datetime(weather_data$daily.dt))

weather_data <- weather_data |> 
  select(date = daily.dt, temp = day, feels_like = daily.feels_like_day, pressure = daily.pressure,
         humidity = daily.humidity, dew_points = daily.dew_point, wind_speed = daily.wind_speed,
         clouds = daily.clouds, precip_chance = daily.pop, uv_index = daily.uvi, mm_rain = daily.rain)

weather_data$mm_rain <- tidyr::replace_na(weather_data$mm_rain, 0)


#######
### air pollution
######

air_pollution <- httr::GET("http://api.openweathermap.org/data/2.5/air_pollution/forecast?lat=33.79&lon=-117.88&appid=###")


air_pollution_data <- fromJSON(rawToChar(air_pollution$content))
air_pollution_data <- as.data.frame(air_pollution_data)

air_pollution_data <- tidyr::unpack(air_pollution_data, cols = 3:5)

air_pollution_data$list.dt <- lubridate::as_date(as_datetime(air_pollution_data$list.dt))

colnames(air_pollution_data)

air_pollution_data <- air_pollution_data |> 
  select(aqi, everything()) |> 
  rename(carbon_monoxide = co, nitrogen_monoxide = no, nitrogen_dioxide = no2, ozone = 03, sulphur_dioxide = so2,
         fine_particles = pm2_5, coarse_particles = pm10, ammonia = nh3, date = list.dt) |> 
  group_by(date) |> 
  summarize(aqi = mean(aqi),
            carbon_monoxide = mean(carbon_monoxide),
            nitrogen_monoxide = mean(nitrogen_monoxide),
            nitrogen_dioxide = mean(nitrogen_dioxide),
            ozone = mean(ozone),
            sulphur_dioxide = mean(sulphur_dioxide),
            fine_particles = mean(fine_particles),
            coarse_particles = mean(coarse_particles),
            ammonia = mean(ammonia))

air_pollution_data$retrieve_date <- Sys.Date()


#### merging weather with air pollution
weather_data <- merge(weather_data, air_pollution_data, by = "date", all.x = TRUE)