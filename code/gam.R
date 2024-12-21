### GAM with R ----------------------
## resource: https://bradcongelio.com/nfl-analytics-with-r-book/05-nfl-analytics-advanced-methods.html
## resource: https://www.amazon.com/Football-Analytics-Python-Learning-Science/dp/1492099627/ref=sr_1_1?crid=1Z58XKARAPHUH&dib=eyJ2IjoiMSJ9.0x9-HS_bHYpe_ojInr9GCpK7Lr_ZVk6fjrLpeIF2SBLubOi8LytVvQIaZxupAic2UB8eg-conje1ob17ykP3WxWW6h72SC2zYag1uTiBlW59mofq0wQpOBT0FsbWoT99y9WINTG-k7DuwqCom6PxbYzywNnbeD1YfUBjdRwXErmYy13ppFMXKAdxfHhPLXbPBGzS5HuoFBFn7Af3W9KQrULdvWySqiSecTHALgG1tS0.cto7wX7o46dhZ7rns_eVQ7s8xiXgO236u-WsE-I6EAA&dib_tag=se&keywords=data+science+football&qid=1717375743&s=books&sprefix=data+science+football%2Cstripbooks%2C177&sr=1-1
## data and cleaning are based on chapter 6 of the eager and erickson text
## first, we will walk through the book and use their poisson regression
## examples, and then build to GAM
library(tidyverse)
library(nflfastR)

### bring in data --------------------------
# set theme aes
theme_set(theme_classic())
theme_update(strip.background = element_blank())

# data
pbp <- load_pbp(seasons = 2016:2023)

pbp_pass <- pbp %>% filter(!is.na(passer_id))

pbp_pass_td_y <- pbp_pass %>% 
  mutate(
    pass_touchdown = ifelse(is.na(pass_touchdown), 0, pass_touchdown)) %>% 
  group_by(season, week, passer_id, passer) %>% 
  summarize(n_passes = (sum(complete_pass, na.rm = T) + (sum(incomplete_pass, na.rm = T))),
            pass_td_y = sum(pass_touchdown),
            total_line = mean(total_line)) %>% 
  filter(n_passes >= 10) %>% 
  ungroup()


# how many times has X number of passing TDs been thrown in a game?
pbp_pass_td_y %>% 
  count(pass_td_y)

### plot of histogram for poisson distribution -------------
pbp_pass_td_y_mean <- pbp_pass_td_y %>% 
  pull(pass_td_y) %>% 
  mean()

plot_pos_r <- tibble(x = seq(0, 7)) %>% 
  mutate(expected = dpois(x = x,
                          lambda = pbp_pass_td_y_mean))
pbp_pass_td_y %>% 
  ggplot(aes(x = pass_td_y,
             y = after_stat(count/sum(count)))) +
  geom_histogram(binwidth = 0.5) +
  geom_line(data = plot_pos_r,
            aes(x = x,
            y = expected),
            color = 'red', 
            lwd = 1)

### Training Data (authors) --------------
# the authors are doing this to model the odds of a player throwing a certain # of 
# touchdowns per week, particularly mahomes in 2022 super bowl
pbp_pass_td_y_geq10 <- pbp_pass_td_y %>% 
  filter(n_passes >= 10)

# preallocate tibble
x_r <- tibble()

# take the average touchdown passes for each QB for the previous
# and current season up to the current game of interest
for(season_idx in seq(2017, 2022)) {
  for(week_idx in seq(1, 22)) {
    week_calc <- pbp_pass_td_y_geq10 %>% 
      filter((season == (season_idx-1)) |
               (season == season_idx & week < week_idx)) %>%
      group_by(passer_id, passer) %>% 
      summarize(n_games = n(),
                pass_td_rate = mean(pass_td_y),
                .groups = 'keep') %>% 
      mutate(season = season_idx, week = week_idx)
    
    x_r <- bind_rows(x_r, week_calc)
               
  }
}

## now join the dataframes we have been working with
pbp_pass_td_y_geq10 <- pbp_pass_td_y_geq10 %>% 
  inner_join(x_r, by = c("season", "week", "passer_id", "passer"))

### Plot training data ---------------
weekly_tds_plot <- pbp_pass_td_y_geq10 %>% 
  ggplot(aes(x = week,
             y = pass_td_y,
             group = passer_id)) +
  geom_line(alpha = 0.25) +
  facet_wrap(~season, nrow = 3)
weekly_tds_plot

weekly_tds_plot +
  geom_smooth(method = 'glm',
              method.args = list("family" = "poisson"),
              se = F,
              lwd = 0.5,
              alpha = 0.20,
              color = 'blue')

#### poisson model fit -------------------------
# bring in {broom:tidy()}
library(broom)
mod_poisson <- glm(pass_td_y ~ pass_td_rate + total_line,
                   data = pbp_pass_td_y_geq10,
                   family = "poisson")

# view model
summary(mod_poisson)
tidy(mod_poisson, exponentiate = T, conf.int = T)

# save preds to dataframe
pbp_pass_td_y_geq10 <- pbp_pass_td_y_geq10 %>% 
  mutate(exp_pass_td = predict(mod_poisson, type = "response"))

head(pbp_pass_td_y_geq10$exp_pass_td)

# the authors are using mahomes as an example to illustrate how 
# many TDs the model we constructed will predict he passes for
# in the super bowl vs the eagles
# but we will use brady against the chiefs
pbp_pass_td_y_geq10 %>% 
  filter(passer == "T.Brady",
         season == 2018)

pbp_pass %>% filter(old_game_id == "201802040nwe")


mod2 <- gam::gam(pass_td_y ~ pass_td_rate + total_line,
          data = pbp_pass_td_y_geq10)
summary(mod2)
mod2$coefficients
