### receiving over time using pfr -------------------------
library(tidyverse)
library(rvest)

# first, function to return colnames across years
scrape_receiving_colnames <- function(year) {
  
  # print what year is being scraped
  print(paste(year, "now being scrapped..."))
  
  # define url
  url <- paste0("https://www.pro-football-reference.com/years/", year, "/receiving.htm")
  
  # data
  x <- read_html(file.path(url)) |>
    html_table() |>
    as.data.frame() 
  
  # return
  dat <- tibble(season = year,
                column_names = colnames(x))
}

# define rate delay for purrr::slowly()
rate_del <- rate_delay(10)

# get and store column names for data
receiving_colnames_dat_2 <- 1970:2023 %>% 
  map(possibly(slowly(quiet = F,
                      scrape_receiving_colnames, 
                      rate_del)), 
      .progress = T)

receiving_colnames_dat_2

# visualize passing column names 
receiving_colnames_dat %>% 
  bind_rows() %>% 
  mutate(column_names = tolower(column_names)) %>% 
  group_by(season) %>% 
  mutate(row_num = row_number()) %>% 
  filter(season >= 1993) %>% 
  ggplot(aes(x = row_num,
             y = season)) +
  geom_text(aes(label = column_names),
            size = 2.5) +
  scale_y_continuous(n.breaks = 2023-1970,
                     expand = expansion(mult = 0.02)) +
  scale_x_continuous(n.breaks = 20)

# based on pulling column names, the first 7 are similar 
base_passing_col_names <- c("rank",
                          "player_name",
                          "team",
                          "age",
                          "position",
                          "games_played",
                          "games_started")

# looks like 1993 was the last year before a change in column names
col_names_1991_or_less <- c(base_passing_col_names,
                            "receptions",
                            "receiving_yards",
                            "receiving_yards_per_reception",
                            "receiving_touchdowns",
                            "longest_reception",
                            "receiving_yards_per_target",
                            "receptions_per_game",
                            "receiving_yards_per_game",
                            "fumbles")

# 1992-1993 names were similar but added in a few new ones
col_names_1992_to_1993 <- c(base_passing_col_names,
                            "targets",
                            "receptions",
                            "catch_percentage",
                            "receiving_yards",
                            "receiving_yards_per_reception",
                            "receiving_touchdowns",
                            "longest_reception",
                            "receiving_yards_per_target",
                            "receptions_per_game",
                            "receiving_yards_per_game",
                            "fumbles")

# change in 1994 as well, append is a base R function
col_names_1994_to_present <- append(col_names_1992_to_1993, 
                                    c("receiving_first_downs",
                                      "receiving_success_rate"), 
                                    after = 13)

# now we can define a receiving scraper function
# generally, i use base R functions for these...with package development
# in mind and not requiring to import too many of the {tidyverse} packages
scrape_passing_stats <- function(year) {
  
  # print what year is being scraped
  print(paste(year, "now being scrapped..."))
  
  # define url
  url <- paste0("https://www.pro-football-reference.com/years/", year, "/receiving.htm")
  
  # scrape
  x <- read_html(file.path(url)) |>
    html_table() |> 
    as.data.frame()
  
  # define logic for column names
  if(year <= 1991) {
    colnames(x) <- col_names_1991_or_less
  }
  
  if(year == 1992 || year == 1993) {
    colnames(x) <- col_names_1992_to_1993
  }
  
  if(year >= 1994) {
    colnames(x) <- col_names_1994_to_present
  }

  # clean dataframe return
  dat <- x |>
    # filter out column headers since they repeat every 30 rows
    filter(!grepl("Rk", rank)) |>
    # clean up data and player names
    mutate(team = nflreadr::clean_team_abbrs(team),
           player_name = str_remove(player_name, "\\*"),
           player_name = str_remove(player_name, "\\+"),
           season = year, .before = player_name,
           player_name = nflreadr::clean_player_names(player_name)) |>
    select(-rank)
}

# testing showed everything looked good so we can move forward
# scrape from 1970 to 2023
# put the custom function in purrr::possibly() and purrr::slowly() to
# 1- (possibly()) maintain scrapped data if the function fails for any reason
# 2- (slowly()) slow down rvest::read_html() to avoid http server 429 issues
receiving_stats_total <- 1970:2023 %>% 
  map(possibly(slowly(scrape_passing_stats,
                      rate_del,
                      quiet = F)),
      .progress = T)

# turn the list into a data frame
receiving_dat <- receiving_stats_total %>% 
  bind_rows() %>% 
  as_tibble() %>% 
  # Cardinals moved from phoenix (PHO) to glendale (ARI) in 1994
  # Boston (BOS) Patriots became New England (NE) Patriots in 1971
  mutate(team = case_when(team == "PHO" ~ "ARI",
                          team == "BOS" ~ "NE",
                          TRUE ~ team)) %>% 
  # lastly, if a player is on multiple teams, change to "MULTI" instead
  # of "2TM", or "3TM" etc.
  mutate(team = case_when(grepl("TM", team) ~ "MULTI",
                          TRUE ~ team)) %>% 
  mutate(across(contains("percentage"), ~str_remove_all(., "%")),
         across(.cols = 6:20, as.numeric),
         age = as.numeric(age)) 

# some sanity checks....
# first, make sure there are 33 teams (32 NFL teams + 1 for "MULTI")
length(unique(receiving_dat$team))

# check the team names in alphabetical order
sort(unique(receiving_dat$team))

# check positions..
sort(unique(receiving_dat$position))

# there are positions as "" for those on multiple teams
receiving_dat %>% 
  filter(position == "")

# first, change the "" to NA
receiving_dat <- receiving_dat %>% 
  mutate(position = case_when(position == "" ~ NA,
                              TRUE ~ position)) 

# let's get the names of those with "" for position
names_missing_pos <- receiving_dat %>% 
  filter(is.na(position)) %>% 
  pull(player_name)

# for those 28 players, what is the most frequent position
# they are listed at?
missing_pos_dat <- receiving_dat %>% 
  filter(player_name %in% names_missing_pos) %>% 
  group_by(player_name) %>% 
  summarize(position = max(position, na.rm = T)) %>% 
  # manually fix one case...
  # https://www.pro-football-reference.com/players/H/HoskHa00.htm 
  mutate(position = case_when(player_name == "Gator Hoskins" ~ "TE",
                              TRUE ~ position)) %>% 
  ungroup()
missing_pos_dat

# we are going to kind of cheap out for fixing this by
# using dplyr::rows_update()
# theoretically, some players may have converted from QB to WR
receiving_dat %>% 
  filter(grepl("Pryor", player_name))

# but we are most interested in WR stats,
# so just force that to be the one they played the most at 
receiving_dat <- receiving_dat %>% 
  rows_update(y = missing_pos_dat,
              by = "player_name")

# let's check this
receiving_dat %>% 
  mutate(position = fct(position)) %>% 
  group_by(position) %>% 
  tally(sort = T) %>% 
  ggplot(aes(y = fct_reorder(position, n),
             x = n)) +
  geom_col()

# no more "" or NA values for position

# save the files
receiving_dat %>% 
  write.csv("Data Files/nfl-receiving-statistics/NFL_receiving_statistics_1970to2023.csv",
            row.names = F,
            na = "")

receiving_dat %>% 
  write_rds("Data Files/nfl-receiving-statistics/NFL_receiving_statistics_1970to2023.rds")


 