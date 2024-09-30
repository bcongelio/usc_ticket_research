library(tidyverse)
library(readr)

options(digits = 3)

### bind every .csv file in daily_data folder
setwd("daily_data")
all_data <- do.call(rbind, lapply(list.files(pattern = "*.csv"), read.csv))

### creating clean and edited version of data
publication_data <- all_data

### selecting only regular season games
publication_data <- publication_data |> 
  filter(date <= "2024-09-29")

### calculate number of days between retrieve date and gamedate
publication_data <- publication_data |> 
  mutate(days_before = as.Date(date) - as.Date(retrieve_date)) |> 
  mutate(days_before = str_remove(days_before, " days"))

### putting columsn in better order
publication_data <- publication_data |> 
  select(event_id, retrieve_date, date, days_before, everything())

### create a standardized popularity column
publication_data <- publication_data |> 
  mutate(popularity_scaled = as.numeric(scale(popularity))) |> 
  select(event_id, retrieve_date, date, days_before, popularity, popularity_scaled, everything())

### making streak negative or positive
publication_data <- publication_data |> 
  mutate(streak = case_when(
    str_detect(streak, "W") ~ as.numeric(str_remove(streak, "W")),
    str_detect(streak, "L") ~ -as.numeric(str_remove(streak, "L")),
    TRUE ~ 0)) ### this throws an NA error, but can confirm that no NAs are created

### separating all various win loss records in order to get a pct
publication_data <- publication_data |> 
  separate(pythagorean_wl, c("pythagorean_wins", "pythagorean_losses"), sep = "-") |> 
  mutate(pythagorean_pct = as.numeric(pythagorean_wins) / (as.numeric(pythagorean_wins) + as.numeric(pythagorean_losses)))

publication_data <- publication_data |> 
  separate(vs_east, c("vs_east_wins", "vs_east_losses"), sep = "-") |>
  mutate(vs_east_pct = as.numeric(vs_east_wins) / (as.numeric(vs_east_wins) + as.numeric(vs_east_losses))) |> 
  separate(vs_central, c("vs_central_wins", "vs_central_losses"), sep = "-") |>
  mutate(vs_central_pct = as.numeric(vs_central_wins) / (as.numeric(vs_central_wins) + as.numeric(vs_central_losses))) |>
  separate(vs_west, c("vs_west_wins", "vs_west_losses"), sep = "-") |>
  mutate(vs_west_pct = as.numeric(vs_west_wins) / (as.numeric(vs_west_wins) + as.numeric(vs_west_losses))) |>
  separate(interleague, c("interleague_wins", "interleague_losses"), sep = "-") |>
  mutate(interleague_pct = as.numeric(interleague_wins) / (as.numeric(interleague_wins) + as.numeric(interleague_losses))) |> 
  separate(home_record, c("home_wins", "home_losses"), sep = "-") |>
  mutate(home_pct = as.numeric(home_wins) / (as.numeric(home_wins) + as.numeric(home_losses))) |>
  separate(road_record, c("away_wins", "away_losses"), sep = "-") |>
  mutate(away_pct = as.numeric(away_wins) / (as.numeric(away_wins) + as.numeric(away_losses))) |>
  separate(ex_innings, c("extra_inning_wins", "extra_inning_losses"), sep = "-") |>
  mutate(extra_inning_pct = as.numeric(extra_inning_wins) / (as.numeric(extra_inning_wins) + as.numeric(extra_inning_losses))) |> 
  separate(one_run_games, c("one_run_wins", "one_run_losses"), sep = "-") |>
  mutate(one_run_pct = as.numeric(one_run_wins) / (as.numeric(one_run_wins) + as.numeric(one_run_losses))) |>
  separate(vs_rhp, c("vs_rhp_wins", "vs_rhp_losses"), sep = "-") |>
  mutate(vs_rhp_pct = as.numeric(vs_rhp_wins) / (as.numeric(vs_rhp_wins) + as.numeric(vs_rhp_losses))) |>
  separate(vs_lhp, c("vs_lhp_wins", "vs_lhp_losses"), sep = "-") |>
  mutate(vs_lhp_pct = as.numeric(vs_lhp_wins) / (as.numeric(vs_lhp_wins) + as.numeric(vs_lhp_losses))) |>
  separate(vs_winning_teams, c("vs_winning_teams_wins", "vs_winning_teams_losses"), sep = "-") |>
  mutate(vs_winning_teams_pct = as.numeric(vs_winning_teams_wins) / (as.numeric(vs_winning_teams_wins) + as.numeric(vs_winning_teams_losses))) |>
  separate(vs_losing_teams, c("vs_losing_teams_wins", "vs_losing_teams_losses"), sep = "-") |>
  mutate(vs_losing_teams_pct = as.numeric(vs_losing_teams_wins) / (as.numeric(vs_losing_teams_wins) + as.numeric(vs_losing_teams_losses))) |>
  separate(last_10, c("last_10_wins", "last_10_losses"), sep = "-") |>
  mutate(last_10_pct = as.numeric(last_10_wins) / (as.numeric(last_10_wins) + as.numeric(last_10_losses))) |>
  separate(last_20, c("last_20_wins", "last_20_losses"), sep = "-") |>
  mutate(last_20_pct = as.numeric(last_20_wins) / (as.numeric(last_20_wins) + as.numeric(last_20_losses))) |>
  separate(last_30, c("last_30_wins", "last_30_losses"), sep = "-") |>
  mutate(last_30_pct = as.numeric(last_30_wins) / (as.numeric(last_30_wins) + as.numeric(last_30_losses)))
  
  
### putting columns back in beter order
publication_data <- publication_data |> 
  relocate(event_id, retrieve_date, date, days_before, popularity, popularity_scaled, streak, time:pythagorean_luck,
           pythagorean_wins, pythagorean_losses, pythagorean_pct, vs_east_wins, vs_east_losses, vs_east_pct, vs_central_wins,
           vs_central_losses, vs_central_pct, vs_west_wins, vs_west_losses, vs_west_pct, interleague_wins, interleague_losses,
           interleague_pct, home_wins, home_losses, home_pct, away_wins, away_losses, away_pct, extra_inning_wins,
           extra_inning_losses, extra_inning_pct, one_run_wins, one_run_losses, one_run_pct, vs_rhp_wins, vs_rhp_losses,
           vs_rhp_pct, vs_lhp_wins, vs_lhp_losses, vs_lhp_pct, vs_winning_teams_wins, vs_winning_teams_losses,
           vs_winning_teams_pct, vs_losing_teams_wins, vs_losing_teams_losses, vs_losing_teams_pct, last_10_wins,
           last_10_losses, last_10_pct, last_20_wins, last_20_losses, last_20_pct, last_30_wins, last_30_losses, last_30_pct, everything())
