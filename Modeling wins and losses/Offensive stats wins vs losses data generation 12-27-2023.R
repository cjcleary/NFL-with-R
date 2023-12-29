#### Offensive stats deterministic of wins and losses ---------------
library(tidyverse)
library(nflreadr)
library(nflfastR)

# load in pbp data
pbp_dat <- load_pbp(2000:2023)
colnames(pbp_dat)
head(pbp_dat)

# clean pbp data
pbp_dat2 <- pbp_dat %>% 
  filter(season_type == "REG") %>% 
  mutate(posteam = clean_team_abbrs(posteam),
         away_team = clean_team_abbrs(away_team),
         home_team = clean_team_abbrs(home_team))
head(pbp_dat2)

# qb data
qb_dat <- pbp_dat2 %>% 
  filter(pass == 1,
         !is.na(epa)) %>% 
  group_by(game_id, season, team = posteam) %>% 
  drop_na(team) %>% 
  summarize(
    plays = n(),
    qb_dropbacks = sum(qb_dropback, na.rm = T),
    qb_kneels = sum(qb_kneel, na.rm = T),
    qb_spikes = sum(qb_spike, na.rm = T),
    qb_shotgun = sum(shotgun, na.rm = T),
    qb_scrambles = sum(qb_scramble, na.rm = T),
    pass_incompletions = sum(incomplete_pass, na.rm = T),
    pass_completions = sum(complete_pass, na.rm = T),
    sacks = sum(sack, na.rm = T),
    qb_hits = sum(qb_hit, na.rm = T),
    epa_pass_play = mean(epa),
    pass_attempts = sum(incomplete_pass + complete_pass, na.rm = T),
    pass_tds = sum(pass_touchdown, na.rm = T),
    pass_yds = sum(passing_yards, na.rm = T),
    pass_ints = sum(interception, na.rm = T),
    qb_fumbles = sum(fumble, na.rm = T),
    qb_lost_fumbles = sum(fumble_lost, na.rm = T)) %>% 
  mutate(completion_percent = (pass_completions / pass_attempts) * 100,
         qb_yds_attempt = pass_yds / pass_attempts) %>% 
  ungroup()
head(qb_dat)

# load in wins-losses
wl_dat <- load_schedules(2000:2023) %>% 
  filter(game_type == "REG") %>% 
  select(season, game_id, home_team, home_score, away_team, away_score) %>% 
  mutate(winner = case_when(home_score > away_score ~ home_team,
                            away_score > home_score ~ away_team,
                            home_score == away_score ~ "TIE"),
         home_team = clean_team_abbrs(home_team),
         away_team = clean_team_abbrs(away_team),
         winner = clean_team_abbrs(winner)) %>%
  drop_na(winner) %>% 
  relocate(winner, 
           .after = game_id)

# clean wins losses data
wl_dat2 <- wl_dat %>% 
  filter(!winner == "TIE") %>% 
  pivot_longer(contains("score"),
               names_to = c("team_loc", "dum"),
               values_to = "score",
               names_sep = "_") %>% 
  select(-dum) %>% 
  mutate(team = case_when(team_loc == "home" ~ home_team,
                          team_loc == "away" ~ away_team),
         team = clean_team_abbrs(team),
         winner_ind = ifelse(winner == team, "winner", "loser"),
         winner_ind = fct(winner_ind, levels = c("winner",
                                                   "loser")),
         winner_loc = case_when(winner == home_team ~ "home",
                                winner == away_team ~ "away"),
         winner_loc = fct(winner_loc, levels = c("home",
                                                 "away"))) 
wl_dat2

# join dataframes
wins_qbs <- left_join(wl_dat2, qb_dat)

# running back data
rb_dat <- pbp_dat2 %>% 
  filter(rush == 1,
         !is.na(epa)) %>% 
  group_by(game_id, season, team = posteam) %>% 
  summarize(
    rush_attempts = sum(rush, na.rm = T),
    shotgun_runs = sum(shotgun, na.rm = T),
    other_formation_runs = rush_attempts - shotgun_runs,
    rush_tds = sum(rush_touchdown, na.rm = T),
    rush_epa_play = mean(epa), 
    rush_yds = sum(rushing_yards, na.rm = T),
    longest_rush = max(rushing_yards, na.rm = T),
    rushing_fumbles = sum(fumble, na.rm = T),
    rushing_fumbles_lost = sum(fumble_lost, na.rm = T),
    runs_for_loss = sum(tackled_for_loss, na.rm = T)) %>% 
  mutate(avg_rush_yds = rush_yds/rush_attempts) %>% 
  ungroup()
head(rb_dat)

# receiver dat
rec_dat <- pbp_dat2 %>% 
  filter(pass == 1,
         !is.na(epa)) %>% 
  group_by(game_id, season, team = posteam) %>% 
  drop_na(team) %>% 
  summarize(
    pass_plays = sum(pass, na.rm = T),
    rec_yds = sum(receiving_yards, na.rm = T),
    rec_yac = sum(yards_after_catch, na.rm = T),
    longest_rec = max(receiving_yards, na.rm = T))
rec_dat

# join rec and rushing
rec_rb <- left_join(rec_dat, rb_dat)

# join wins qb and rec_reb
wins_offense <- left_join(wins_qbs, rec_rb)


