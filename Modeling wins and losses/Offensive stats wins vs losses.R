#### Offensive stats deterministic of wins and losses ---------------
library(tidyverse)
library(nflreadr)
library(nflfastR)

wins_off <- read.csv("Modeling wins and losses/NFL_wins_offense_12272023.csv") %>% 
  mutate(winner_ind = fct(winner_ind,
                          levels = c("winner",
                                     "loser")),
         winner_loc = fct(winner_loc,
                          levels = c("home",
                                     "away"))) %>% 
  mutate(across(where(is.integer),
                as.numeric)) %>% 
  select(-qb_kneels)

wins_off_clean <- wins_off %>% 
  select(season, winner_ind, winner_loc,
         where(is.numeric),
         -score)
wins_off_clean

wins_model_dat <- wins_off_clean %>% 
  select(-season) %>% 
  drop_na()

# see where these numbers differ based on winners and losers
wins_model_dat %>% 
  group_by(winner_ind) %>% 
  summarize(across(where(is.numeric), ~mean(.))) %>% 
  pivot_longer(cols = 2:33) %>% 
  gt::gt(groupname_col = "name")
