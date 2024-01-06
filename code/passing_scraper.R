### passing over time using pfr -------------------------
library(tidyverse)
library(rvest)

# first, function to return colnames across years
scrape_passing_colnames <- function(year) {
  
  # print what year is being scraped
  print(paste(year, "now being scrapped..."))
  
  # define url
  url <- paste0("https://www.pro-football-reference.com/years/", year, "/passing.htm")
  
  # data
  x <- read_html(file.path(url)) %>% 
    html_table() %>% 
    as.data.frame() 
  
  # return
  dat <- tibble(season = year,
                column_names = colnames(x))
}

# define rate delay for purrr::slowly()
rate_del <- rate_delay(10)

# get and store column names for data
passing_colnames_dat <- 1970:2023 %>% 
  map(possibly(slowly(quiet = F,
                      scrape_passing_colnames, 
                      rate_del)), 
      .progress = T)

# visualize passing column names 
passing_colnames_dat %>% 
  bind_rows() %>% 
  mutate(column_names = tolower(column_names)) %>% 
  group_by(season) %>% 
  mutate(row_num = row_number()) %>% 
  filter(season > 1992) %>% 
  ggplot(aes(x = row_num,
             y = season)) +
  geom_text(aes(label = column_names),
            size = 2.5) +
  scale_y_continuous(limits = c(1992, 2023),
                     n.breaks = 2023-1992,
                     expand = expansion(mult = 0))

# based on pulling column names, the first 16 are similar 
base_passing_col_names <- c("rank",
                          "player_name",
                          "team",
                          "age",
                          "position",
                          "games_played",
                          "games_started",
                          "quarterback_record",
                          "passes_completed",
                          "passing_attempts",
                          "completion_percentage",
                          "passing_yards",
                          "passing_tds",
                          "passing_tds_percentage",
                          "passing_interceptions",
                          "passing_interceptions_percentage")

# looks like 1993 was the last year before a change in column names
col_names_1993_or_less <- c(base_passing_col_names,
                            "longest_pass",
                            "passing_yards_per_attempt",
                            "adjusted_passing_yards_per_attempt",
                            "passing_yards_per_completion",
                            "passing_yards_per_game",
                            "passer_rating",
                            "times_sacked",
                            "yards_lost_to_sacks",
                            "sacked_percentage",
                            "net_yards_per_attempt",
                            "adjusted_net_yards_per_attempt",
                            "fourth_quarter_comebacks",
                            "game_winning_drives")

# 1994-2005 names were similar but added in a few new ones
col_names_1994_to_2005 <- append(col_names_1993_or_less,
                                 c("passing_first_downs",
                                   "passing_success_rate"),
                                 after = 16)

# change in 2006 as well, append is a base R function
col_names_2006_to_present <- append(col_names_1994_to_2005, 
                                    "espn_qbr", 
                                    after = 24)

# now we can define a passing scraper function
# generally, i use base R functions for these...with package development
# in mind and not requiring to import too many of the {tidyverse} packages
scrape_passing_stats <- function(year) {
  
  # print what year is being scraped
  print(paste(year, "now being scrapped..."))
  
  # define url
  url <- paste0("https://www.pro-football-reference.com/years/", year, "/passing.htm")
  
  # scrape
  x <- read_html(file.path(url)) |>
    html_table() |> 
    as.data.frame()
  
  # define logic for column names
  if(year <= 1993) {
    colnames(x) <- col_names_1993_or_less
  }
  
  if(year >= 1994) {
    colnames(x) <- col_names_1994_to_2005
  }
  
  if(year >= 2006) {
    colnames(x) <- col_names_2006_to_present
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
           team = fct(team),
           position = fct(position),
           player_name = nflreadr::clean_player_names(player_name)) |>
    select(-rank)
}

# testing showed everything looked good so we can move forward
# scrape from 1970 to 2023
# put the custom function in purrr::possibly() and purrr::slowly() to
# 1- (possibly()) maintain scrapped data if the function fails for any reason
# 2- (slowly()) slow down rvest::read_html() to avoid http server 429 issues
passing_stats_total <- 1970:2023 %>% 
  map(possibly(slowly(quiet = F,
                      scrape_passing_stats,
                      rate_del)),
      .progress = T)

# turn the list into a data frame
passing_dat <- passing_stats_total %>% 
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
  mutate(across(.cols = 9:32, as.numeric)) %>%  
  separate(quarterback_record,
           into = c("quarterback_wins",
                    "quarterback_losses",
                    "quarterback_ties"),
           sep = "-") %>% 
  mutate(quarterback_wins = as.numeric(quarterback_wins),
         quaterback_losses = as.numeric(quarterback_losses),
         quaterback_ties = as.numeric(quarterback_ties))


# some sanity checks....
# first, make sure there are 33 teams (32 NFL teams + 1 for "MULTI")
length(unique(passing_dat$team))

# check the team names in alphabetical order
sort(unique(passing_dat$team))

# check positions....
passing_dat %>% 
  group_by(position) %>% 
  tally(sort = T) %>% 
  print(n = nrow(.))

# for some reason, there are positions as ""
passing_dat %>% 
  filter(position == "")

# one of these was baker mayfield in 2022
passing_dat %>% 
  filter(grepl("Mayfield", player_name),
         year == "2022") %>% 
  select(1:8)

# double checking, these are quarterbacks...
# so going to manually fix
# check our code before we assign it to anything
passing_dat %>% 
  mutate(position = case_when(position == "" ~ "QB",
                              TRUE ~ position)) %>% 
  group_by(position) %>% 
  tally(sort = T) %>% 
  print(n = nrow(.))

# looks good let's assign it! 
passing_dat <- passing_dat %>% 
  mutate(position = case_when(position == "" ~ "QB",
                              TRUE ~ position)) 

# check that baker's 2022 season position is fixed
passing_dat %>% 
  filter(grepl("Mayfield", player_name),
         season == "2022") %>% 
  select(1:8)

# yay!
# now let's write these to files
passing_dat %>% 
  write.csv("Data Files/nfl-passing-statistics/NFL_passing_statistics_1970to2023.csv",
            row.names = F,
            na = "")

passing_dat %>% 
  write_rds("Data Files/nfl-passing-statistics/NFL_passing_statistics_1970to2023.rds")


