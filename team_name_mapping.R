######
## name mapping for future merges
######

### collect team name information from baeballr
baseball_names <- baseballr::mlb_teams(season = 2024, sport_ids = c(1)) |> 
  select(team_full_name, team_code, team_abbreviation, team_name, team_id) |> 
  arrange(team_full_name)

### collect team names from ticketing daily run
team_hyphen_names <- unique(daily_run$home_team) |> 
  as.data.frame() |> 
  rename(team_hyphen = 'unique(daily_run$home_team)') |> 
  arrange(team_hyphen)

### merge for complete team name mapping
team_name_mapping <- cbind(team_hyphen_names, baseball_names)

write.csv(team_name_mapping, "./team_name_mapping.csv", row.names = FALSE)
