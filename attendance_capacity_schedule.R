library(tidyverse)
library(rvest)

team_abbv <- c("NYY", "BAL", "BOS", "TBR", "TOR", "CLE", "KCR", "DET", "MIN", "CHW",
               "HOU", "SEA", "TEX", "OAK", "LAA", "PHI", "ATL", "NYM", "WSN", "MIA",
               "MIL", "CHC", "STL", "CIN", "PIT", "LAD", "SDP", "ARI", "SFG", "COL")

##########################
## testing for just one team
##########################

##########################
## do not use
## use purrr method below
##########################

### setting the URL
url <- "https://www.baseball-reference.com/teams/ATL/2024-schedule-scores.shtml"

### scrapping the data
nyy_schedule <- url |> 
  read_html() |> 
  html_node("#team_schedule") |> 
  html_table() |> 
  janitor::clean_names() |> 
  filter(x_2 != "@") |> 
  select(date, attendance) |> 
  filter(!date %in% c("April", "May", "June", "July", "August", "September", "Date"),
         attendance != "Attendance",
         attendance != "Game Preview, and Matchups",
         !grepl("\\(1\\)", date)) |> 
  mutate(date = gsub("\\s*\\(2\\)", "", date),
         date = as.Date(date, format = "%A, %b %d")) |> 
  mutate(schedule_team = "atlanta-braves")

################
## looping over each team with map_dfr
#############

### take the above code and loop over each team_abbv
schedule <- map_dfr(team_abbv, function(x) {
  url <- paste0("https://www.baseball-reference.com/teams/", x, "/2024-schedule-scores.shtml")
  schedule <- url |> 
    read_html() |> 
    html_node("#team_schedule") |> 
    html_table() |> 
    janitor::clean_names() |> 
    filter(x_2 != "@") |> 
    select(date, attendance) |> 
    filter(!date %in% c("April", "May", "June", "July", "August", "September", "Date"),
           attendance != "Attendance",
           attendance != "Game Preview, and Matchups",
           !grepl("\\(1\\)", date)) |> 
    mutate(date = gsub("\\s*\\(2\\)", "", date),
           date = as.Date(date, format = "%A, %b %d"),
           attendance = readr::parse_number(attendance)) |> 
    mutate(schedule_team = x)
  return(schedule)
})

### adding matching team names for merging
schedule <- schedule |> 
  mutate(schedule_team = case_when(
    schedule_team == "NYY" ~ "new-york-yankees",
    schedule_team == "BAL" ~ "baltimore-orioles",
    schedule_team == "BOS" ~ "boston-red-sox",
    schedule_team == "TBR" ~ "tampa-bay-rays",
    schedule_team == "TOR" ~ "toronto-blue-jays",
    schedule_team == "CLE" ~ "cleveland-guardians",
    schedule_team == "KCR" ~ "kansas-city-royals",
    schedule_team == "DET" ~ "detroit-tigers",
    schedule_team == "MIN" ~ "minnesota-twins",
    schedule_team == "CHW" ~ "chicago-white-sox",
    schedule_team == "HOU" ~ "houston-astros",
    schedule_team == "SEA" ~ "seattle-mariners",
    schedule_team == "TEX" ~ "texas-rangers",
    schedule_team == "OAK" ~ "oakland-athletics",
    schedule_team == "LAA" ~ "los-angeles-angels",
    schedule_team == "PHI" ~ "philadelphia-phillies",
    schedule_team == "ATL" ~ "atlanta-braves",
    schedule_team == "NYM" ~ "new-york-mets",
    schedule_team == "WSN" ~ "washington-nationals",
    schedule_team == "MIA" ~ "miami-marlins",
    schedule_team == "MIL" ~ "milwaukee-brewers",
    schedule_team == "CHC" ~ "chicago-cubs",
    schedule_team == "STL" ~ "st-louis-cardinals",
    schedule_team == "CIN" ~ "cincinnati-reds",
    schedule_team == "PIT" ~ "pittsburgh-pirates",
    schedule_team == "LAD" ~ "los-angeles-dodgers",
    schedule_team == "SDP" ~ "san-diego-padres",
    schedule_team == "ARI" ~ "arizona-diamondbacks",
    schedule_team == "SFG" ~ "san-francisco-giants",
    schedule_team == "COL" ~ "colorado-rockies"))

### manually adding stadium capacity information
schedule <- schedule |> 
  mutate(stadium_capacity = case_when(
    schedule_team == "new-york-yankees" ~ 46537,
    schedule_team == "baltimore-orioles" ~ 44970,
    schedule_team == "boston-red-sox" ~ 37755,
    schedule_team == "tampa-bay-rays" ~ 42735,
    schedule_team == "toronto-blue-jays" ~ 49282,
    schedule_team == "cleveland-guardians" ~ 34830,
    schedule_team == "kansas-city-royals" ~ 37903,
    schedule_team == "detroit-tigers" ~ 41083,
    schedule_team == "minnesota-twins" ~ 38544,
    schedule_team == "chicago-white-sox" ~ 40615,
    schedule_team == "houston-astros" ~ 41168,
    schedule_team == "seattle-mariners" ~ 47929,
    schedule_team == "texas-rangers" ~ 40300,
    schedule_team == "oakland-athletics" ~ 56782,
    schedule_team == "los-angeles-angels" ~ 45517,
    schedule_team == "philadelphia-phillies" ~ 42901,
    schedule_team == "atlanta-braves" ~ 41084,
    schedule_team == "new-york-mets" ~ 41922,
    schedule_team == "washington-nationals" ~ 41339,
    schedule_team == "miami-marlins" ~ 37442,
    schedule_team == "milwaukee-brewers" ~ 41900,
    schedule_team == "chicago-cubs" ~ 41649,
    schedule_team == "st-louis-cardinals" ~ 44383,
    schedule_team == "cincinnati-reds" ~ 43500,
    schedule_team == "pittsburgh-pirates" ~ 42445,
    schedule_team == "los-angeles-dodgers" ~ 56000,
    schedule_team == "san-diego-padres" ~ 39860,
    schedule_team == "arizona-diamondbacks" ~ 48405,
    schedule_team == "san-francisco-giants" ~ 41265,
    schedule_team == "colorado-rockies" ~ 46897))

### calculating percent of capacity for each game
schedule <- schedule |> 
  mutate(percent_capacity = attendance / stadium_capacity)

### writing off and saving
write.csv(schedule, "attendance_capacity_schedule.csv", row.names = FALSE)

### merging publication data with capacity information
publication_data <- publication_data |> 
  left_join(attendance_capacity_schedule, by = c("date" = "date", "home_team" = "schedule_team"))

### arranging columns
publication_data <- publication_data |> 
  select(event_id, retrieve_date, date, days_before, home_team, away_team, attendance, stadium_capacity,
         percent_capacity, everything())

### adding information for braves' end-of-season game with mets
publication_data <- publication_data |> 
  mutate(
    attendance = case_when(
      date == "2024-09-25" | date == "2024-09-26" & home_team == "atlanta-braves" ~ 41561,
      TRUE ~ attendance),
    stadium_capacity = case_when(
      date == "2024-09-25" | date == "2024-09-26" & home_team == "atlanta-braves" ~ 41084,
      TRUE ~ stadium_capacity),
    percent_capacity = case_when(
      date == "2024-09-25" | date == "2024-09-26" & home_team == "atlanta-braves" ~ attendance / stadium_capacity,
      TRUE ~ percent_capacity))

### removing a few NA values that won't match up because of strange double-headers and/or rain dates
publication_data <- publication_data |> 
  filter(!is.na(attendance))

### writing off final version of publication data
write.csv(publication_data, "publication_data.csv", row.names = FALSE)
