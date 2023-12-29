library(rvest)
library(tidyverse)

# define column names
air_yards_names <- c("rank", "player_name", "team", "age", "position", "games_played",
                     "games_started", "completions", "pass_attempts", "pass_yds",
                     "intended_air_yards", "intended_air_yards_per_attempt", "completed_air_yards",
                     "completed_air_yards_per_completion", "completed_air_yards_per_attempt",
                     "yards_after_catch", "yards_after_catch_per_completion")

accuracy_names <- c("rank", "player_name", "team", "age", "position", "games_played", "games_started",
                    "completions", "pass_attempts", "pass_yds", "batted_balls", "throw_aways",
                    "spikes", "dropped_passes", "dropped_per_attempt", "bad_throws",
                    "bad_throw_per_attempt", "on_target_throws", "one_target_per_attempt")

scrape_pfr_passing_advanced <- function(year) {
  x <- read_html(file.path("https://www.pro-football-reference.com/years/",year,"/passing_advanced.htm")) %>% 
    html_table() 
  
  # air yards table
  air_yards <- x[[1]] %>% 
    as.data.frame() %>% 
    set_names(air_yards_names) %>% 
    slice(-1) %>% 
    mutate(team = nflreadr::clean_team_abbrs(team),
           player_name = str_remove(player_name, "\\*"))
  
  air_yards$season <- year
  
  air_yards <- air_yards %>% 
    relocate(season, .before = 1) %>% 
    select(-rank) %>% 
    mutate(across(where(is.character),
                  ~str_remove(., pattern = "%"))) %>% 
    mutate(team = nflreadr::clean_team_abbrs(team),
           player_name = str_remove(player_name, "\\*"),
           player_name = str_remove(player_name, "\\+"),
           player_name = nflreadr::clean_player_names(player_name))
  
  # accuracy table
  accuracy <- x[[2]] %>% 
    as.data.frame() %>% 
    set_names(accuracy_names) %>% 
    slice(-1)
  
  accuracy$season <- year
  
  accuracy <- accuracy %>% 
    relocate(season, .before = 1) %>% 
    select(-rank) %>% 
    mutate(across(where(is.character),
                  ~str_remove(., pattern = "%"))) %>% 
    mutate(team = nflreadr::clean_team_abbrs(team),
           player_name = str_remove(player_name, "\\*"),
           player_name = str_remove(player_name, "\\+"),
           player_name = nflreadr::clean_player_names(player_name))
  
  dat <- full_join(air_yards,
                   accuracy) %>% 
    filter(position == "QB") %>% 
    mutate(across(.cols = 6:26,
                  as.numeric)) %>% 
    filter(!pass_attempts < 50)
    
  return(dat)
}

few_years <- 2019:2023 %>% 
  map(scrape_pfr_passing_advanced)

few_years_dat <- few_years %>% 
  bind_rows() %>% 
  as.data.frame()

# team colors
team_primary_colors <- nflfastR::teams_colors_logos %>% 
  mutate(team_abbr = nflreadr::clean_team_abbrs(team_abbr)) %>% 
  select(team_abbr, team_color, team_color2) %>% 
  distinct(team_abbr, .keep_all = T)

# set themes
theme_set(theme_classic())
theme_update(axis.text = element_text(face = "bold",
                                      color = "black",
                                      family = "Times"),
             text = element_text(face = "bold",
                                 color = "black",
                                 family = "Times"))

mean_dat <- few_years_dat %>% 
  group_by(team) %>% 
  filter(!team == "2TM") %>% 
  select(-season) %>% 
  summarize(across(where(is.numeric), ~mean(.))) %>% 
  left_join(team_primary_colors,
            by = c("team" = "team_abbr")) %>% 
  mutate(team = fct(team)) %>% 
  ungroup()

colnames(mean_dat)
  
mean_dat %>% 
  ggplot(aes(x = pass_yds,
             y = fct_reorder(team, pass_yds))) +
  geom_col(aes(color = team_color2,
               fill = team_color)) +
  scale_color_identity(aesthetics = c('fill', 'color')) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(y = "Team")

library(ggrepel)
mean_dat %>% 
  ggplot(aes(x = bad_throws,
             y = dropped_passes)) +
  geom_point(aes(size = pass_attempts,
                 color = team_color2,
                 fill = team_color),
             shape = 21) +
  geom_text_repel(aes(label = team,
                      color = team_color), 
                  force = 1,
                  size = 3.5, 
                  nudge_x = 2) +
  scale_color_identity(aesthetics = c('fill', 'color')) +
  geom_vline(xintercept = mean(mean_dat$bad_throws),
             color = "red",
             linetype = "dashed",
             alpha = 0.5) +
  geom_hline(yintercept = mean(mean_dat$dropped_passes),
             color = "red",
             linetype = "dashed",
             alpha = 0.5) +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 8),
                     limits = c(0, 30)) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 8),
                     limits = c(0, 120)) +
  labs(y = "Dropped Passes (mean)",
       x = "Bad Throws (mean)",
       size = "Pass Attempts") +
  theme(legend.position = "bottom")
  

sum_dat <- few_years_dat %>% 
  group_by(team) %>% 
  filter(!team == "2TM") %>% 
  select(-season) %>% 
  summarize(across(where(is.numeric), ~sum(.))) %>% 
  left_join(team_primary_colors,
            by = c("team" = "team_abbr")) %>% 
  mutate(team = fct(team)) %>% 
  ungroup()

passing_2023 %>% 
  ggplot(aes(x = pass_attempts,
             y = throw_aways)) +
  geom_point(aes(#size = pass_attempts,
                 color = team_color2,
                 fill = team_color),
             shape = 21) +
  geom_text_repel(aes(label = team,
                      color = team_color), 
                  force = 1,
                  size = 3.5, 
                  nudge_x = 2) +
  scale_color_identity(aesthetics = c('fill', 'color')) +
  geom_vline(xintercept = mean(passing_2023$pass_attempts),
             color = "red",
             linetype = "dashed",
             alpha = 0.5) +
  geom_hline(yintercept = mean(passing_2023$throw_aways),
             color = "red",
             linetype = "dashed",
             alpha = 0.5) +
  geom_smooth(method = "lm",
              se = F,
              color = "darkblue",
              linetype = "dashed") +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 8)) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 8)) +
  labs(x = "Pass Attempts",
       y = "Throw Aways",
       size = "Pass Attempts") +
  theme(legend.position = "bottom")

colnames(sum_dat)

summary(lm(throw_aways ~ pass_attempts,
           data = passing_2023)) 
passing_2023 <- few_years_dat %>% 
  filter(season == "2023") %>% 
  group_by(team) %>% 
  summarize(across(where(is.numeric), ~mean(.))) %>% 
  left_join(team_primary_colors,
            by = c("team" = "team_abbr")) %>% 
  mutate(team = fct(team)) %>% 
  ungroup()
passing_2023

passing_per_year <- few_years_dat %>% 
  filter(!team == "2TM") %>% 
  group_by(team, season) %>% 
  summarize(across(where(is.numeric), ~mean(.))) %>% 
  left_join(team_primary_colors,
            by = c("team" = "team_abbr")) %>% 
  mutate(team = fct(team)) %>% 
  ungroup()

theme_update(panel.background = element_blank())
passing_per_year %>% 
  ggplot(aes(x = pass_attempts,
             y = throw_aways)) +
  geom_point() +
  geom_point(aes(
    color = team_color2,
    fill = team_color),
    shape = 21) +
  geom_text_repel(aes(label = team,
                      color = team_color), 
                  force = 1,
                  size = 3.5) +
  scale_color_identity(aesthetics = c('fill', 'color')) +
  geom_smooth(aes(group = season),
              method = "lm",
              se = F,
              color = "darkblue",
              linetype = "dashed") +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 8)) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 8)) +
  labs(x = "Pass Attempts",
       y = "Throw Aways",
       size = "Pass Attempts") +
  theme(strip.background = element_blank()) +
  facet_wrap(~season, nrow = 1)

library(lme4)
m1 <- lmerTest::lmer(throw_aways ~ pass_attempts * as.factor(season) + (1 | team),
           data = passing_per_year)
summary(m1)
anova(m1)

library(emmeans)
emtrends(m1, ~season)
emmeans()
