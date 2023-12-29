### Tej Smith https://github.com/tejseth/nfl-tutorials-2022/blob/master/1-intro-to-nflfastr.R
library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)

# load pbp data
pbp <- load_pbp(2020:2022)

str(pbp)

# number of rows in pbp
nrow(pbp)

# first few rows of data
pbp %>% head(5)

# run and pass data
pbp_rp <- pbp %>% 
  filter(season == 2022,
         pass == 1 | rush == 1) %>% 
  # get plays that have epa
  filter(!is.na(epa))

# who was the patriots best rusher last year
pbp_rp %>% 
  filter(
    posteam == "NE", 
    season == 2022,
    rush == 1,
    !is.na(rusher_player_name)) %>% 
  group_by(rusher = rusher_player_name, 
           posteam, 
           season) %>% 
  summarize(rushes = n(),
            epa_rush = mean(epa)) %>%  
  filter(rushes > 200) %>% 
  arrange(-epa_rush) 

unique(pbp_rp$season_type)
unique(pbp_rp$season)

# who was the patriots best passer last season
pbp_rp %>% 
  filter(
    season == 2022,
    posteam == "NE",
    !is.na(id),
    season_type == "REG") %>% 
  group_by(id, season) %>% 
  summarize(name = first(name),
            plays = n(),
            epa_per_play = mean(epa),
            pass_attempts = 
              sum(complete_pass + incomplete_pass, na.rm = T)) %>% 
  filter(plays > 50, pass_attempts > 1) %>% 
  arrange(-epa_per_play)

# compare pass efficiency to rush efficiency 
# first get pass eff
pass_eff22 <- pbp_rp %>% 
  filter(season == 2022, pass == 1) %>% 
  group_by(posteam) %>% 
  summarize(passes = n(),
            pass_epa = mean(epa))

# it appears as pass attempts go up, so does pass epa
pass_eff22 %>% 
  ggplot(aes(x = passes,
             y = pass_epa)) +
  geom_point()

summary(lm(pass_epa ~ passes, data = pass_eff22))

# now get rush eff
rush_eff22 <- pbp_rp %>% 
  filter(season == 2022, rush == 1) %>% 
  group_by(posteam) %>% 
  summarize(rushes = n(),
            rush_epa = mean(epa))

# does that trend hold true for rushes
rush_eff22 %>% 
  ggplot(aes(x = rushes,
             y = rush_epa)) +
  geom_point()

summary(lm(rush_epa ~ rushes, data = rush_eff22))

# combine eff dataframes
total_eff <- left_join(pass_eff22, rush_eff22)
total_eff
names(pbp_rp)

# now lets add in team colors
teams_colors_logos

total_eff <- left_join(total_eff,
                       teams_colors_logos[c("team_name", "team_color", 
                                            "team_abbr", "team_logo_espn")],
                       by = c("posteam" = "team_abbr"))
total_eff

# get the relationship between the two
eff_mod <- lm(rush_epa ~ pass_epa, data = total_eff)
summary(eff_mod)
eff_rsquared <- round(summary(eff_mod)$r.squared, 3)

# make a plot showing the relationship between the two
theme_set(theme_classic())
theme_update(axis.text = element_text(face = "bold",
                                      color = "black"),
             plot.caption = element_text(face = "bold",
                                         color = "black"))

total_eff %>% 
  ggplot(aes(x = pass_epa,
             y = rush_epa)) +
  geom_hline(yintercept = mean(total_eff$rush_epa), 
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = mean(total_eff$pass_epa),
             linetype = "dashed",
             color = "red") +
  geom_smooth(method = "lm",
              se = F,
              linetype = "dashed",
              color = "black") +
  geom_image(aes(image = team_logo_espn),
             size = 0.05, 
             asp = 16/9) +
  labs(caption = paste0(eff_rsquared*100, "%", " of the variance in Rushing EPA is explained by Passing EPA"))

# make a table showing qb aggressiveness
qb_agg <- pbp %>% 
  mutate(yards_past_sticks = air_yards - ydstogo) %>% 
  filter(season_type == "REG",
         !is.na(passer_player_id)) %>% 
  group_by(passer_player_id, season) %>% 
  summarize(name = first(name),
            team = last(posteam),
            passes = n(),
            agg = mean(yards_past_sticks, na.rm = T)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = season,
              values_from = passes:agg,
              names_glue = "{.value}_{season}") %>% 
  select(-ends_with("2021"))
 
head(qb_agg)
qb_agg

# now get down to only qbs that played all 3 years
qb_agg2 <- qb_agg %>% drop_na() %>% 
  mutate(pass_diff = passes_2022 - passes_2020,
         agg_diff = agg_2022 - agg_2020) %>% 
  left_join(teams_colors_logos[c("team_abbr", "team_wordmark")],
            by = c("team" = "team_abbr")) %>% 
  filter(passes_2022 >= 100 & passes_2020 >= 100)
qb_agg2 %>% View()

qb_agg2 %>% 
  mutate(across(where(is.numeric), ~round(., 2))) %>% 
  select(Quaterback = name, 
         Team = team_wordmark, 
         `2020 Aggressiveness` = agg_2020, 
         `2022 Aggressiveness` = agg_2022, 
         `Aggressiveness Difference` = agg_diff) %>% 
  arrange(-`Aggressiveness Difference`) %>% 
  gt() %>% 
  cols_align(align = "center") %>% 
  gtExtras::gt_img_rows(Team) %>% 
  gtExtras::gt_theme_espn() %>% 
  data_color(columns = `Aggressiveness Difference`,
             method = "numeric",
             reverse = T,
             palette = "tvthemes::Stark")

