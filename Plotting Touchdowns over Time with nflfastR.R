#### plotting TDs over time --------------------
# describes changes in touchdowns over time
# shows adding annotation to figures using annotate
# basic script

library(tidyverse)
library(nflfastR)

# load in data
# seasons = TRUE for all seasons (1999-present)
d1 <- nflfastR::load_player_stats(seasons = TRUE)
d1 %>% colnames()

# clean data
off_tds <- d1 %>% 
  # only regular season td's
  filter(season_type == "REG") %>% 
  # select the variables we're interested in
  select(season, passing_tds, rushing_tds) %>% 
  # group_by season to get per-season mesaures
  group_by(season) %>% 
  # calculate summary variables
  summarize(pass_tds = sum(passing_tds),
            rush_tds = sum(rushing_tds),
            total_tds = pass_tds + rush_tds) %>%
  # always ungroup
  ungroup()
  
# view data
off_tds
head(off_tds)

# check data structure
str(off_tds)

# basic plot
# again, i like to set the main theme settings first
theme_set(theme_classic())

off_tds %>% 
  ggplot(aes(x = season)) +
  # since we have three different lines, we need
  # to define a geom_line for each one
  geom_line(aes(y = pass_tds),
            color = "darkred") +
  # this is simply done by changing the y aesthetic for each
  # call of geom_line()
  geom_line(aes(y = rush_tds),
            color = "darkblue") +
  geom_line(aes(y = total_tds),
            color = "darkgreen") +
  # now adding in annotations
  # annotate() needs a "geom" call to it
  # to tell it how to add the label
  annotate(label = "Total Touchdowns", 
           geom = "text",
           x = 2005,
           y = 1200,
           color = "darkgreen") +
  annotate(label = "Passing Touchdowns",
           geom = "text",
           x = 2005,
           y = 800,
           color = "darkred") +
  annotate(label = "Rushing Touchdowns",
           geom = "text",
           x = 2005,
           y = 500,
           color = "darkblue") +
  # change the x-axis limits and breaks for clarity
  # {scales} package comes in handy for breaks
  scale_x_continuous(limits = c(min(off_tds$season),
                                max(off_tds$season)),
                     breaks = scales::pretty_breaks(n = 25),
                     # fix default expansion for cleaniness
                     expand = expansion(add = 0.5)) +
  scale_y_continuous(limits = c(min(off_tds$rush_tds),
                                max(off_tds$total_tds)),
                     breaks = scales::pretty_breaks(n = 5),
                     expand = expansion(mult = 0.1)) +
  labs(y = "Touchdowns (count)",
       # x axis is self-explanatory so going to trash it
       x = NULL,
       title = "NFL Touchdowns since 1999-2023",
       caption = "Data: {nflfastR}") +
  # change x-axis text to angled for ease
  theme(
    text = element_text(face = "bold",
                        color = "black"),
    axis.text = element_text(face = "bold",
                             color = "black"),
    axis.text.x = element_text(angle = 90),
    # center plot title
    plot.title = element_text(hjust = 0.5))
off_tds



