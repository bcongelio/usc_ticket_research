library(tidyverse)
library(rvest)

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

colnames(mlb_standings)  

