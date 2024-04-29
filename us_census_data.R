library(tidyverse)
library(tidycensus)
library(tigris)

## loading variables
census_variables <- tidycensus::load_variables(2020, "acs5")

## gathering data from the census on population/race and overall median income
population <- tidycensus::get_acs(
  geography = "zip code tabulation area",
  variables = c(total_population = "B01003_001",
                white_population = "B02001_002",
                black_population = "B02001_003",
                hispanic_population = "B03002_018",
                median_12_month_income = "B19013_001")) |> 
  mutate(NAME = str_remove(NAME, "ZCTA5 "))

## read in zip code data
mlb_zip_codes <- vroom::vroom("./mlb_zip_codes.csv")

population_zips <- population |> 
  filter(NAME %in% c(mlb_zip_codes$zip_code)) |> 
  left_join(mlb_zip_codes, by = c("NAME" = "zip_code"),
            relationship = "many-to-many")

## making census data wide
population_zips <- population_zips |> 
  select(variable, estimate, moe, team_name) |> 
  tidyr::pivot_wider(names_from = variable,
                     values_from = c(estimate, moe))

## write off to csv for inclusion in daily run data
write.csv(population_zips, "census_data_wide.csv", row.names = FALSE)


