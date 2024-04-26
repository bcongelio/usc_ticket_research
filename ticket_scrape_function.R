library(tidyverse)
library(httr)
library(data.table)
library(rvest)

daily_run_mlb <- data.frame()

get_seatgeek_data <- function() {
  
  ### scrape mlb standings
  url <- "https://www.baseball-reference.com/leagues/MLB-standings.shtml"
  
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
    mutate(retrieve_date = lubridate::ymd(retrieve_date))

  df <- data.frame(mlb_dat_out)
  
  ### mlb team name mapping

  df <- df |> 
    inner_join(team_name_mapping, by = c("home_team" = "team_hyphen"))
  
  df <- df |> 
    inner_join(mlb_standings, by = c("team_full_name" = "team",
                                     "retrieve_date" = "date"))

  
  daily_run_mlb <- rbind(daily_run_mlb, df)
  
  }
  
  return(daily_run_mlb)
}

daily_run <- get_seatgeek_data()

retrieve_date <- Sys.Date() |> 
  str_remove(" UTC")

write.csv(daily_run, paste0("./daily_data/mlb_",retrieve_date,".csv"), row.names = FALSE)

