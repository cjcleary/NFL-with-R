### visualizing rushing statistics across time with {ggplot2} -----------------------
# load in libraries
library(tidyverse)
library(scales)
library(gt)
library(gtExtras)

# read in datafile,  i prefer csv
rush_1 <- read_csv("Data Files/nfl-rushing-statistics/NFL_rushing_statistics1950-2023.csv")
str(rush_1)

# clean data file
rush_2 <- rush_1 %>% 
  # we are only interested in when the nfl merged in 1970
  filter(!year < 1970) %>% 
  # create decade variable
  mutate(decade = case_when(
    between(year, 1970, 1979) ~ "1970s",
    between(year, 1980, 1989) ~ "1980s",
    between(year, 1990, 1999) ~ "1990s",
    between(year, 2000, 2009) ~ "2000s",
    between(year, 2010, 2019) ~ "2010s",
    between(year, 2020, 2023) ~ "2020s"),
    .after = year) %>% 
  # of note, cardinals moved from phoenix (PHO) to glendale (ARI) in 1994
  # and patriots became NE in 1971
  # will fix that manually
  mutate(team = case_when(team == "PHO" ~ "ARI",
                          team == "BOS" ~ "NE",
                          TRUE ~ team)) %>% 
  # lastly, if a player is on multiple teams, will change to "MULTI" instead
  # of "2TM", or "3TM" etc.
  mutate(team = case_when(grepl("TM", team) ~ "MULTI",
                          TRUE ~ team)) %>% 
  # and make team a factor for plotting, use base::factor to sort alphabetically
  mutate(team = factor(team))

str(rush_2)

# explore the data, let's see top 5 leading rushers per decade
rush_2 %>% 
  group_by(decade, player_name) %>% 
  filter(rush_attempts > 100) %>% 
  summarize(max = sum(rush_yards),
            team_1 = first(team),
            team_2 = last(team)) %>% 
  slice_max(n = 5, order_by = max) %>% 
  ungroup() %>% 
  sele
  gt::gt(groupname_col = "decade") %>% 
  gtExtras::gt_theme_538()


