### rushing over time using prf -------------------------
library(tidyverse)
library(rvest)

# define column names
nfl_rush_colnames_old <- c("rank", "player_name", "team", "age", 
                           "position", "games_played", "games_started", "rush_attempts",
                           "rush_yards", "rush_touchdowns", "rush_long", "rush_yards_per_attempt",
                           "rush_yds_per_game", "rush_fumbles")

nfl_rush_colnames_current <- c("rank", "player_name", "team", "age", "position", "games_played", 
                               "games_started", "rush_attempts", "rush_yards", "rush_touchdowns",
                               "rush_first_downs", "rush_success_rate", "rush_long", "rush_yards_per_attempt",
                               "rush_yds_per_game", "rush_fumbles")

# function for scrapping data
scrape_rush_data <- function(year) {
  
  # print what year is being scraped
  print(paste(year, "now being scrapped..."))
  
  # define url
  url <- paste0("https://www.pro-football-reference.com/years/", year, "/rushing.htm")
  
  x <- read_html(file.path(url)) %>% 
    html_table() %>% 
    as.data.frame()
  
  # logic because there was a change in the column counts after 1993
  if(year <= 1993) {
    colnames(x) <- nfl_rush_colnames_old
    
    dat <- x %>% 
      # filter out column headers since they repeat every 30 rows
      filter(!grepl("Rk", rank)) %>% 
      # do not need rank
      select(-rank) %>% 
      relocate(position, .after = team) %>% 
      # clean up data and player names
      mutate(across(.cols = 5:13, as.numeric),
             team = nflreadr::clean_team_abbrs(team),
             player_name = str_remove(player_name, "\\*"),
             player_name = str_remove(player_name, "\\+")) 
  }
  
  # if year > 1994, change column names
  else {
    colnames(x) <- nfl_rush_colnames_current
    
    dat <- x %>% 
      filter(!grepl("Rk", rank)) %>% 
      select(-rank) %>% 
      relocate(position, .after = team) %>% 
      mutate(across(.cols = 5:15, as.numeric),
             team = nflreadr::clean_team_abbrs(team),
             player_name = str_remove(player_name, "\\*"),
             player_name = str_remove(player_name, "\\+")) 
  }
  
  # lastly, clean up add in year, make team a factor
  dat <- dat %>% 
    mutate(year = year, .before = player_name,
           team = fct(team),
           position = fct(position),
           player_name = nflreadr::clean_player_names(player_name))
}


# use purrr::map() to map the years you want to scrape
# to note, scraping more than 30+ years is not recommended as
# sportsref will lock you out of their server (error 429)
# not a big deal, do it in pieces or use the provided data set
rushing_2010 <- 2010 %>% 
  map(scrape_rush_data, 
      .progress = T)

# write rds file of all years

write_rds(dat1, "Data Files/NFL_rushing_statistics1950-2023.rds")

# write csv file of all years
write.csv(dat1, "Data Files/NFL_rushing_statistics1950-2023.csv",
          row.names = F,
          na = "")

# define dictionary for data
nfl_rushing_dictionary <- tibble(column_names = colnames(dat1),
                                 dictionary = c("NFL season",
                                                "name of player",
                                                "team adjusted for 2023 team names",
                                                "player position",
                                                "player age",
                                                "player games played in season",
                                                "player games started in season",
                                                "player rush attempts (total) in season",
                                                "player rush yards (total) in season",
                                                "player rush touchdowns (total) in season",
                                                "player longest rush of season",
                                                "player rush yards per rushing attempt in season",
                                                "player rush yards per game in season",
                                                "player fumbles (total) in season",
                                                "player first downs (total) in season",
                                                "player rushing succcess rate per pfr in season"))

# write rds file of dictionary
nfl_rushing_dictionary %>% 
  write_rds(file = "Data Files/NFL_rushing_statistics_dictionary.rds")
       