# linear regression modeling by Tej Smith
# url: https://github.com/tejseth/nfl-tutorials-2022/blob/master/3-linear-regression-modeling.R
library(tidyverse)
library(nflfastR)
library(vip)
library(ggimage)

# load pbp data
pbp <- load_pbp(2018:2022)

# look at just 4th downs
fourth_downs <- pbp %>% 
  filter(down == 4, !is.na(play_type),
         season_type == "REG")

# see what happens usually on fourth downs
fourth_downs %>% 
  group_by(play_type) %>% 
  tally(sort = T)

# create an indicator variable
fourth_downs <- fourth_downs %>% 
  mutate(went_for_it = ifelse(play_type %in% c("pass", "run"), 1, 0))

# seeing which variables correlate with going for it
fourth_downs %>% 
  group_by(ydstogo) %>% 
  summarize(count = n(),
            balls_rate = mean(went_for_it)) %>% 
  filter(count >= 5) %>% 
  ggplot(aes(x = ydstogo,
             y = balls_rate)) +
  geom_bar(aes(fill = balls_rate), stat = "identity") +
  theme_minimal() +
  geom_smooth(method = "loess")

# make a logistic regression model
options(scipen = 9999)
fourth_glm_mod <- glm(went_for_it ~ (yardline_100 + ydstogo + wp)^2,
                      data = fourth_downs)
summary(fourth_glm_mod)
round(exp(coef(fourth_glm_mod)), 4)
vip(fourth_glm_mod)

# visualize this
fourth_downs %>% 
  mutate(pred_prob = fourth_glm_mod$fitted.values) %>% 
  ggplot(aes(x = ydstogo)) +
  geom_line(aes(y = pred_prob)) +
  geom_point(aes(y = went_for_it, 
                 color = ifelse(went_for_it == 1, "darkgreen", "darkred")), 
             alpha = 0.3) +
  scale_color_identity() +
  facet_wrap(~season)

fourth_downs %>% 
  filter(went_for_it == 1) %>% 
  group_by(season, posteam) %>% 
  summarize(mean(ydstogo),
            mean(wp),
            mean(yardline_100),
            n = n(),
            rate = sum(fourth_down_converted)/n) %>% 
  arrange(-rate) %>% 
  ggplot(aes(x = season,
             y = rate,
             group = posteam,
             color = posteam)) +
  geom_line() +
  geom_point()

