### Data Viz with nflfastR -- Tej Smith --------------------------------
# link https://github.com/tejseth/nfl-tutorials-2022/blob/master/2-dataviz-with-nfl-data.R
library(tidyverse)
library(nflfastR)
library(ggimage)
library(ggrepel)
library(ggthemes)
library(gt)
library(gtExtras)

# load in pbp data
pbp <- load_pbp(2018:2023)

# clean team colors logo
teams_colors_logos <- nflfastR::teams_colors_logos %>% 
  select(team_abbr, team_color, team_color2, team_logo_espn)
teams_colors_logos

# get qb epa per play
qb_epa_post_play <- pbp %>% 
  filter(pass == 1 | rush == 1, !is.na(epa),
         season_type == "POST") %>% 
  group_by(id, season) %>% 
  summarize(name = first(name),
            team = last(posteam),
            plays = n(),
            epa_play = mean(epa),
            pass_attempts = sum(incomplete_pass + complete_pass, na.rm = T)) %>% 
  mutate(pass_rate = pass_attempts / plays) %>% 
  left_join(teams_colors_logos, by = c("team" = "team_abbr")) 
qb_epa_post_play

# make plot of qb epa per play
theme_set(theme_classic())
qb_epa_play %>% 
  ggplot(aes(x = pass_rate,
             y = epa_play)) +
  geom_point(aes(size = plays,
                 fill = team_color,
                 color = team_color2),
             shape = 21,
             alpha = 0.9) +
  geom_text_repel(aes(label = name)) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  theme(legend.position = "none") + 
  facet_wrap(~season)

# make a bar graph with the same data
qb_epa_play %>% 
  filter(season == 2019) %>% 
  filter(season_type == "POST") %>% 
  filter(pass_attempts > 5) %>% 
  arrange(-epa_play) %>% 
  ggplot(aes(x = epa_play,
             y = fct_reorder(name, epa_play))) +
  geom_col(aes(fill = team_color,
               color = team_color2),
           alpha = 0.9) +
  scale_color_identity(aesthetics = c('fill', "color")) +
  geom_vline(xintercept = mean(qb_epa_play[c(qb_epa_play$season == "2019" & 
                                               qb_epa_play$season_type == "POST" &
                                               qb_epa_play$pass_attempts > 5),]$epa_play),
             lwd = 1.5,
             linetype = "dashed",
             color = "black") +
  scale_x_continuous(breaks = scales::pretty_breaks(8)) +
  theme(axis.title.y = element_blank())

mean(qb_epa_play[qb_epa_play$season == "2022",]$epa_play)

mean(qb_epa_play[c(qb_epa_play$season == "2019" & 
                     qb_epa_play$season_type == "POST" &
                     qb_epa_play$pass_attempts > 5),]$epa_play)
