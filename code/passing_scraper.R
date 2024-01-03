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
rate_del <- rate_delay(20)

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
col_names_1994_to_2005 <- c(base_passing_col_names,
                            "passing_first_downs",
                            "passing_success_rate",
                            col_names_1993_or_less)

# change in 2006 as well
col_names_2006_to_present <- append(col_names_1994_to_2005, 
                                    "espn_qbr", 
                                    after = 24)

# now we can define a passing scraper function
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
           year = year, .before = player_name,
           team = fct(team),
           position = fct(position),
           player_name = nflreadr::clean_player_names(player_name))
}

# testing showed everything looked good so we can move forward
passing_stats_total <- 1970:2023 %>% 
  map(possibly(slowly(quiet = F,
                      scrape_passing_stats,
                      rate_del)),
      .progress = T)

passing_dat <- passing_stats_total %>% 
  bind_rows() %>% 
  as_tibble() %>% 
  mutate(across(.cols = 10:33, as.numeric)) 

unique(passing_dat$team)


