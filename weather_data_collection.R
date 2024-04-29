library(httr)
library(jsonlite)
library(tidyverse)


try this: https://github.com/jcblsn/dissertation/blob/71bb8f3f6dcc82ae28be0ac077a8c450c9cb186d/scripts/00-get_weather.R#L39



request <- httr::GET("https://api.openweathermap.org/data/3.0/onecall?lat=33.79&lon=-117.88&exclude=current,minutely,hourly,alerts&units=imperial")

### check if reuqest has 200 status
request # it does

### converting unicode to character vector
weather_raw <- rawToChar(request$content)
weather_data <- jsonlite::fromJSON(weather_raw)
weather_data <- as.data.frame(weather_data)

weather_data$daily.dt <- lubridate::as_date(as_datetime(weather_data$daily.dt))

weather_data <- weather_data |> 
  select(date = daily.dt, temp = daily.temp, feels_like = daily.feels_like, pressure = daily.pressure,
         humidity = daily.humidity, dew_points = daily.dew_point, wind_speed = daily.wind_speed,
         clouds = daily.clouds, precip_chance = daily.pop, uv_index = daily.uvi, mm_rain = daily.rain)

weather_data <- janitor::clean_names(weather_data)

weather_data <- weather_data |> 
  rename(temperature = 2)

colnames(weather_data)


### air pollution
air_pollution <- httr::GET("http://api.openweathermap.org/data/2.5/air_pollution/forecast?lat=33.799572&lon=-117.889031&appid=7ba6a76c98d5c0129167c936c48527d8")

air_pollution

air_pollution_data <- fromJSON(rawToChar(air_pollution$content))
air_pollution_data <- as.data.frame(air_pollution_data)

air_pollution_data$list.dt <- lubridate::as_date(as_datetime(air_pollution_data$list.dt))
