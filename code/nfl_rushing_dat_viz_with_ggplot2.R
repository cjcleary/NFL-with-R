### visualizing rushing statistics across time with {ggplot2} -----------------------
# load in libraries
library(tidyverse)
library(scales)

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

# let's get top 20 rushers of all time
# first, let's make sure positions are correct since we want to 
# calculate years played and names will repeat
# this is a workaround not having a unique ID for each player in these data
# like nflfastR does
unique(rush_2$position)

positions_of_interest <- c("RB", "FB", "RB/FB", "FB/RB", "HB")

top20_rushers <- rush_2 %>% 
  filter(position %in% positions_of_interest) %>% 
  group_by(player_name) %>%
  mutate(player_season = (year - first(year)) + 1) %>% 
  summarize(max_rush = sum(rush_yards),
            seasons_played = max(player_season),
            # get team most frequently played for
            team = names(which.max(table(team)))) %>% 
  slice_max(n = 20, 
            order_by = max_rush) %>% 
  # clean team names for plotitng colors
  mutate(team = nflreadr::clean_team_abbrs(team),
         player_name = fct(player_name))  %>% 
  # now add in colors
  left_join(nflfastR::teams_colors_logos[,c("team_abbr",
                                            "team_color",
                                            "team_color2")],
            by = c("team" = "team_abbr"))

# view the dataframe
top20_rushers

# now set theme settings
theme_set(theme_classic())
theme_update(text = element_text(face = "bold",
                                 color = "black",
                                 size = 18, 
                                 family = "Tahoma"),
             axis.text = element_text(face = "bold",
                                      color = "black",
                                      size = 10,
                                      family = "Tahoma"),
             plot.title = element_text(face = "bold",
                                       size = 24,
                                       color = "black",
                                       hjust = 0.5),
             axis.title.y = element_blank(),
             plot.caption = element_text(face = "italic",
                                         size = 8))

# and now plot
top20_rushers %>% 
  ggplot(aes(x = max_rush,
             y = fct_reorder(player_name, max_rush))) +
  geom_col(aes(fill = team_color,
               color = team_color2)) +
  scale_color_identity(aesthetics = c("fill",
                                      "color")) +
  scale_x_continuous(limits = c(0, 20000),
                     breaks = scales::breaks_pretty(n = 6),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(x = "Rushing Yards",
       title = "NFL Top 20 Rushers",
       caption = "Data: profootballreference.com | Fig: @cleary_cj")

