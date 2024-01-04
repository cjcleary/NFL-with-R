### advanced passing scraper -----------------------------
library(tidyverse)
library(rvest)

# for PFR's advanced passing, there are 4 tables per year after 2019
# 2018 only had three tables (air_yards, accuracy, pressure)
# 2019- present had four (air_yards, accuracy, pressure, play_type)

# define column names for air yards table
air_yards_tbl_names <- c("rank", "player_name", "team", "age", "position", "games_played", 
                         "games_started", "passes_completed", "passes_attempted", "passing_yards", 
                         "intended_air_yards", "intended_air_yards_per_attempt", "completed_air_yards", 
                         "completed_air_yards_per_completion", "completed_air_yards_per_attempt", 
                         "pass_yards_after_catch", "pass_yards_after_catch_per_completion")

# define column names for accuracy table
accuracy_tbl_names <- c("rank", "player_name", "team", "age", "position", "games_played", 
                        "games_started", "passes_completed", "passes_attempted", "passing_yards", 
                        "batted_balls", "throw_aways", "spikes", "drops", "drops_percentage", 
                        "bad_throws", "bad_throws_percentage", "on_target_throws", "on_target_throws_percentage")

# define column names for pressure table
pressure_tbl_names <- c("rank", "player_name", "team", "age", "position", "games_played", 
                        "games_started", "passes_completed", "passes_attempted", "passing_yards", 
                        "sacks_taken", "pocket_time", "blitzed_count", "hurried_count", 
                        "hit_count", "pressured_count", "pressured_per_dropback", "scrambled_count", 
                        "yards_per_scramble")

# define column names for play type table
play_type_tbl_names <- c("rank", "player_name", "team", "age", "position", "games_played", 
                         "games_started", "passes_completed", "passes_attempted", "passing_yards", 
                         "rpo_plays", "rpo_total_yards", "rpo_pass_attempts", "rpo_passing_yards", 
                         "rpo_rush_attempts", "rop_rushing_yards", "playaction_pass_attempts", 
                         "playaction_passing_yards")

# for cleaner code, we are going to define a custom function to clean up the dataframes
clean_pfr_tables <- function(x) {
  dat <- x %>% filter(!grepl("Rk", rank)) |>
    # clean up data and player names
    mutate(team = nflreadr::clean_team_abbrs(team),
           player_name = str_remove(player_name, "\\*"),
           player_name = str_remove(player_name, "\\+"),
           team = fct(team),
           player_name = nflreadr::clean_player_names(player_name))
}

# define custom function to scrape 
scrape_advanced_passing <- function(year) {
 
   # print what year is being scraped
  print(paste(year, "now being scrapped..."))
  
  # define url
  url <- paste0("https://www.pro-football-reference.com/years/", year, "/passing_advanced.htm")
  
  # scrape page
  x <- read_html(file.path(url)) |>
    html_table() 
  
  # first table, air yards
  air_yards_tbl <- x[[1]] |>
    as.data.frame() |>
    purrr::set_names(air_yards_tbl_names) |>
    clean_pfr_tables() |> 
    mutate(season = year,
           .after = rank) |>
    select(-rank)
  
  # second table, accuracy
  if(year == 2018) {
    accuracy_tbl <- x[[2]] |>
      as.data.frame() |>
      purrr::set_names(accuracy_tbl_names[-c(11, 18:19)]) |>
      clean_pfr_tables() |> 
      mutate(season = year,
             .after = rank) |>
      select(-rank)
  }
  else{
    accuracy_tbl <- x[[2]] |>
      as.data.frame() |>
      purrr::set_names(accuracy_tbl_names) |>
      clean_pfr_tables() |> 
      mutate(season = year,
             .after = rank) |>
      select(-rank)
  }
  
  # third table, pressure
  pressure_tbl <- x[[3]] |>
    as.data.frame() |>
    purrr::set_names(pressure_tbl_names) |>
    clean_pfr_tables() |> 
    mutate(season = year,
           .after = rank) |>
    select(-rank)
  
  # fourth table after 2019 only, play_type
  # ad in logic
  if(year >= 2019) {
    play_type_tbl <- x[[4]] |>
      as.data.frame() |>
      purrr::set_names(play_type_tbl_names) |>
      clean_pfr_tables() |> 
      mutate(season = year,
             .after = rank) |>
      select(-rank)
  }
  
  # since 2018 data only has 3 tables, add in logic to deal with this
  if(year == 2018) {
    dat <- purrr::reduce(list(air_yards_tbl,
                              accuracy_tbl,
                              pressure_tbl),
                         left_join,
                         by = c("player_name", "team", "age", "position", "games_played", "games_started", 
                                "passes_completed", "passes_attempted", "passing_yards", "season"))
  }
  
  # return for if year >= 2019
  else{
    dat <- purrr::reduce(list(air_yards_tbl,
                              accuracy_tbl,
                              pressure_tbl,
                              play_type_tbl),
                         left_join,
                         by = c("player_name", "team", "age", "position", "games_played", "games_started", 
                                "passes_completed", "passes_attempted", "passing_yards", "season"))
  }
  
 return(dat) 
  
}

# testing looked OK so...let's map it
advanced_passing <- 2018:2023 %>% 
  map(possibly(slowly(quiet = F,
                      scrape_advanced_passing,
                      rate_delay(10))),
      .progress = T)

# change dataframe into 
advanced_passing_dat <- advanced_passing %>% 
  bind_rows() %>% 
  as_tibble() %>% 
  # change values into numeric
  mutate(across(.cols = 6:43,
                as.numeric)) %>% 
  # fix multiple teams positions into qbs after manually checking
  # taysom hill is also listed as "WR/QB". We'll make him into a QB
  mutate(position = 
           case_when(position == "" | position == "WR/QB" ~ "QB",
                     TRUE ~ position)) 
unique(advanced_passing_dat$team)
