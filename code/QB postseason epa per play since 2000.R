### relationship between QB postseason play and SB Winning---------------------
library(tidyverse)
library(nflfastR)
library(ggimage)
library(ggrepel)
library(ggthemes)
library(gt)
library(gtExtras)

# load in pbp data
pbp <- load_pbp(2000:2022)

# clean team colors logo
teams_colors_logos <- nflfastR::teams_colors_logos %>% 
  select(team_abbr, team_color, team_color2, team_logo_espn)
teams_colors_logos
colnames(pbp)
# get qb epa per play
qb_epa_play <- pbp %>% 
  filter(pass == 1 | rush == 1, !is.na(epa),
         season_type == "REG") %>% 
  group_by(id, season) %>% 
  summarize(name = first(name),
            team = last(posteam),
            plays = n(),
            epa_play = mean(epa),
            pass_attempts = sum(incomplete_pass + complete_pass, na.rm = T),
            pass_tds = sum(pass_touchdown, na.rm = T),
            pass_yds = sum(passing_yards, na.rm = T),
            ints = sum(interception, na.rm = T)) %>% 
  mutate(pass_rate = pass_attempts / plays) %>% 
  left_join(teams_colors_logos, by = c("team" = "team_abbr")) %>% 
  ungroup()
qb_epa_play %>% arrange(-ints) %>% view()

sb_winners <- nflreadr::load_schedules(seasons = 2000:2022) %>% 
  filter(game_type == "SB") %>% 
  mutate(sb_winner = case_when(result > 0 ~ home_team,
                               result < 0 ~ away_team),
         sb_loser = case_when(sb_winner == home_team ~ away_team,
                              sb_winner == away_team ~ home_team)) %>% 
  select(season, game_type, sb_winner, sb_loser) %>% 
  mutate(sb_winner = nflreadr::clean_team_abbrs(sb_winners$sb_winner),
         sb_loser = nflreadr::clean_team_abbrs(sb_winners$sb_loser)) %>% 
  select(season, sb_winner, sb_loser)
sb_winners

qb_epa_sb <- left_join(qb_epa_play, sb_winners) %>%
  group_by(season) %>% 
  filter(team == sb_winner | team == sb_loser,
         pass_attempts > 25) %>%
  relocate(season, id, team, sb_winner, sb_loser) %>% 
  ungroup() %>% 
  mutate(winning_qb = ifelse(team == sb_winner, "winner", "loser"),
         .before = name,
         winning_qb = fct(winning_qb,
                         levels = c("winner",
                                    "loser"))) %>% 
  rename(winning_team = winning_qb) %>% 
  relocate(season, winning_team, qb_name = name) %>% 
  arrange(season)

qb_epa_sb %>% 
  ggplot(aes(x = season,
             y = ints,
             color = winning_team)) +
  geom_point() +
  geom_line() +
  geom_text_repel(aes(label = qb_name))

qb_epa_sb %>% 
  select(-season) %>% 
  group_by(winning_team) %>% 
  summarize(across(where(is.numeric), ~mean(.))) 

sb_model <- glm(winning_team ~ 
                  plays + 
                  epa_play + 
                  pass_attempts + 
                  pass_tds + 
                  pass_yds + 
                  ints + 
                  pass_rate,
                data = qb_epa_sb,
                family = "binomial")

### other focusing on reg season performance ###
qb_reg_play <- pbp %>% 
  filter(pass == 1 | rush == 1, !is.na(epa),
         season_type == "REG") %>% 
  group_by(game_id, season, posteam) %>% 
  summarize(
            plays = n(),
            incompletions = sum(incomplete_pass, na.rm = T),
            completions = sum(complete_pass, na.rm = T),
            sacks = sum(sack, na.rm = T),
            qb_hits = sum(qb_hit, na.rm = T),
            epa_play = mean(epa),
            pass_attempts = sum(incomplete_pass + complete_pass, na.rm = T),
            pass_tds = sum(pass_touchdown, na.rm = T),
            pass_yds = sum(passing_yards, na.rm = T),
            ints = sum(interception, na.rm = T)) %>% 
  mutate(pass_rate = pass_attempts / plays,
         team = nflreadr::clean_team_abbrs(posteam)) %>% 
  ungroup() %>% 
  relocate(season, game_id, team) %>% 
  filter(pass_attempts > 5) %>% 
  select(-posteam)

# sanity check
qb_reg_play %>% 
  group_by(season) %>% 
  tally() %>% 
  print(n = nrow(.))

records_2000_2023 <- nflreadr::load_schedules(seasons = 2000:2023) %>% 
  filter(game_type == "REG") %>% 
  select(season, game_id, home_team, home_score, away_team, away_score) %>% 
  mutate(winner = case_when(home_score > away_score ~ home_team,
                            away_score > home_score ~ away_team,
                            home_score == away_score ~ "TIE")) %>% 
  drop_na(winner) %>% 
  filter(!winner == "TIE") %>% 
  select(season, game_id, home_score, away_score, winner, home_team, away_team) %>% 
  pivot_longer(contains("score"),
               names_to = c("team_loc", "dum"),
               names_sep = "_",
               values_to = "score") %>% 
  select(-dum) %>% 
  mutate(team = case_when(team_loc == "home" ~ home_team,
                          team_loc == "away" ~ away_team),
         winning_ind = case_when(winner == team ~ "winner",
                                 TRUE ~ "loser")) %>% 
  select(season, game_id, home_team, away_team, score, team, winning_ind, winner)

records_2000_2023

# now join the dataframes together
winning_qb_play <- left_join(records_2000_2023,
                             qb_reg_play) %>% 
  mutate(winning_ind = fct(winning_ind,
                           levels = c("winner",
                                      "loser")),
         winner = nflreadr::clean_team_abbrs(winner),
         team = nflreadr::clean_team_abbrs(team))

colnames(winning_qb_play)

# sanity check should be equal
table(winning_qb_play$winning_ind)

winning_qb_play %>% 
  group_by(winning_ind) %>% 
  summarize(pass_tds = sum(pass_tds, na.rm = T))

winning_qb_play %>% 
  group_by(winner) %>% 
  tally(sort = T)

# add in colors
team_colors2 <- nflfastR::teams_colors_logos %>% 
  mutate(team_abbr = nflreadr::clean_team_abbrs(team_abbr)) %>% 
  group_by(team_abbr) %>% 
  slice(1) %>% 
  ungroup()

winning_qb_colors <- left_join(winning_qb_play,
                               team_colors2,
                               by = c("team" = "team_abbr"))

wins <- winning_qb_colors %>% 
  group_by(winner) %>% 
  tally(sort = T) %>% 
  left_join(team_colors2,
            by = c("winner" = "team_abbr")) %>% 
  rename(wins = n)

wins_plt <- wins %>% 
  ggplot(aes(x = wins,
             y = fct_reorder(winner, wins))) +
  geom_col(aes(fill = team_color,
               color = team_color2)) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  geom_vline(xintercept = mean(wins$wins),
             col = "darkgrey",
             linetype = "dashed") +
  scale_x_continuous(breaks = scales::breaks_pretty(10),
                     expand = expansion(add = c(0, 50))) +
  geom_image(aes(image = team_logo_espn), 
             nudge_x = +10,
             asp = 0.8) +
  theme(text = element_text(face = "bold",
                            color = "black",
                            family = "Times"),
        axis.text = element_text(face = "bold",
                                 color = "black")) +
  labs(y = NULL,
       x = NULL,
       title = "NFL Team Wins since 2000-Current",
       subtitle = "Regular Season",
       caption = "Data courtesy of {nflfastR}")
wins_plt

wins_plt %>% 
  ggsave(filename = "Figures/nfl_wins.tiff",
         compression = "lzw+p",
         height = 11,
         width = 8)

confint(m1) %>% exp()
exp(coef(m1))

m2 <- (glm(winning_ind ~ sacks, data = winning_qb_colors, family = 'binomial'))
coef(m2)        
exp(coef(m2))
contrasts(winning_qb_colors$winning_ind)

fct(winning_qb_colors$winning_ind)
