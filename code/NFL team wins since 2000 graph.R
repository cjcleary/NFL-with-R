### NFL team wins since 2000---------------------
# install packages if not installed
# install.packages(c("tidyverse",
#                    "nflreadr",
#                    "nflfastR",
#                    "ggimage",
#                    "gt",
#                    "gtExtras",
#                    "scales"))

# load packages
library(tidyverse) # data cleaning and manipulation
library(nflreadr) # records
library(nflfastR) # for this script, adding colors of each team
library(ggimage) # adding images to plots
library(gt) # makes great tables (gt)
library(gtExtras) # helper functions for {gt}

# bring in schedules, limit to regular season only
records_dat1 <- nflreadr::load_schedules(seasons = 2000:2023) %>% 
  filter(game_type == "REG") 

# see colnames of records_dat1
colnames(records_dat1)

# and structure of it
str(records_dat1)

# clean schedules dataframe up for ease
records_dat2 <- records_dat1 %>% 
  
  # select what we are interested in
  select(season, game_id, home_team, home_score, away_team, away_score) %>% 
  
  # create variable that indicates which team won
  # we are not interested in ties but create it as an outcome
  mutate(winner = case_when(home_score > away_score ~ home_team,
                            away_score > home_score ~ away_team,
                            home_score == away_score ~ "TIE")) %>%
  
  # drop the games that have yet to happen
  drop_na(winner) %>% 
  
  # relocate winning team to somewhere cleaner
  relocate(winner, 
           .after = game_id)

# view the records_dat2 dataframe 
records_dat2

# first, sanity checks
# see the winning teams 
unique(records_dat2$winner)

# how many winning teams
length(unique(records_dat2$winner))

# before we move on, there are 36 teams, this is because some teams relocated
# like OAK for the Raiders who are now LV
# SD for Chargers, STL for Rams
# let's clean that up using an {nflreadr} function
records_dat2 <- records_dat2 %>% 
  mutate(winner = clean_team_abbrs(winner))

# check again
# see the winning teams 
unique(records_dat2$winner)

# how many winning teams
# there are 33 because we still have ties in there
length(unique(records_dat2$winner))

# how many games were played each season
# nfl introducted 17-game schedule in 2021
# 2022- game was canceled because of Damar Hamlin's injury
records_dat2 %>% 
  group_by(season) %>% 
  tally()  %>% 
  # print all the rows
  print(n = nrow(.))

# individual teams
# how many wins have the patriots had per season since 2000
# check with profootballreference
# https://www.pro-football-reference.com/teams/nwe/index.htm
records_dat2 %>% 
  # filter just to patriots
  filter(winner == "NE") %>% 
  group_by(season) %>% 
  tally() %>% 
  print(n = nrow(.)) 

# we can also see all individual teams
records_dat2 %>% 
  group_by(winner, season) %>% 
  tally() %>% 
  print(n = nrow(.))

# average wins for each team...
records_dat2 %>% 
  group_by(winner, season) %>% 
  tally() %>% 
  # always ungroup after group_by() and function after it
  ungroup() %>% 
  group_by(winner) %>% 
  summarize(mean_wins = mean(n, na.rm = T)) %>%
  ungroup() %>% 
  #sort it for wins
  arrange(-mean_wins) %>% 
  print(n = nrow(.)) 

# now basic plot of total wins per team
# first create separate dataframe showing total wins for each team
# like we have win
wins_2000_2023 <- records_dat2 %>% 
  # for ease, I will change winner to be "team"
  group_by(team = winner) %>% 
  # and instead of "n" as the second column, let's make it wins
  tally(name = "wins") %>% 
  ungroup() %>% 
  # let's get rid of ties
  filter(!team == "TIE")

# head() and tail() each view first and last 6 of a dataframe
head(wins_2000_2023)
tail(wins_2000_2023)

# view structure of tibble, two columns, 32 rows (32 teams)
str(wins_2000_2023)

# to note, R automatically oraganizes dataframes in alphabetical order
# we can change that if we want
wins_2000_2023 %>%
  # arranges defaults to lowest values first
  arrange(wins) 

# we can change that with -variable_we_want_to_sort_by
wins_2000_2023 %>% 
  arrange(-wins)

# ok enough fucking around
# basic plot of team wins since 2000
wins_2000_2023 %>% 
  ggplot(aes(x = wins,
             y = team)) +
  geom_col()

# doesn't look great, let's order it by most wins
# change winner to be a factor variable for ease
wins_2000_2023 <- wins_2000_2023 %>% 
  mutate(team = fct(team))

# check to make sure it took effect
str(wins_2000_2023)

# now replot with a change in the function
# ? function name shows what it does
?forcats::fct_reorder

wins_2000_2023 %>% 
  ggplot(aes(x = wins,
             # fct_reorder is from
             y = fct_reorder(team, wins))) +
  # geom col = column chart
  geom_col() +
  # add in y and x axis labels
  labs(y = "Team",
       x = "Wins since 2000")

# looks OK but we can add colors
# nflfastR has a dataframe with teams colors built in
nflfastR::teams_colors_logos

# let's perform a left join to join our wins dataframe and the 
# teams colors dataframe together
?dplyr::left_join

wins_with_colors <- left_join(
  wins_2000_2023,
  teams_colors_logos,
  # specify by since the colnames are slightly different
  # by = c(column_name_in_x_dataframe = column_name_in_y_dataframe)
  by = c("team" = "team_abbr"))

head(wins_with_colors)

# it has multiple color values
# the {scales} package lets you see colors easily
scales::show_col(wins_with_colors$team_color)
scales::show_col(wins_with_colors$team_color2)

# now add those colors in and name the plot
wins_plt1 <- wins_with_colors %>% 
  ggplot(aes(x = wins,
             y = fct_reorder(team, wins))) +
  # now with geom_col we'll add in a fill and
  # color aesthetic
  # fill = fill of bar/column
  # color = outline
  geom_col(aes(fill = team_color,
               color = team_color2)) +
  # specify how ggplot will use the oclors
  scale_color_identity(aesthetics = c("fill",
                                      "color")) +
  labs(y = "Team",
       x = "Wins since 2000")

# view plot
wins_plt1

# looks better but we can improve
# ggplot has lots of theme options
# once you name a plot you can add elements to it with +
wins_plt1 + theme_bw()
wins_plt1 + theme_minimal()
wins_plt1 + theme_linedraw()

# i prefer theme_classic()
# and instead of adding the theme elements to a plot,
# i specify them first...its for ease
theme_set(theme_classic())

# theme has changed
wins_plt1

# and also update the theme
# text = all text on a plot
# you can also use axis.text.x or .y to specify
# and axis.title.x for plot titles
theme_update(text = element_text(face = "bold",
                                 color = "black",
                                 family = "Times",
                                 size = 20),
             axis.text = element_text(face = "bold",
                                      family = "Times",
                                      color = "black"))
wins_plt1

# looks better, but note how far the "0" is from the y-axis
# r does this automatically for some reason but we can fix it
# i'll just make a new plot
wins_plt2 <- wins_with_colors %>% 
  ggplot(aes(x = wins,
             y = fct_reorder(team, wins))) +
  geom_col(aes(fill = team_color,
               color = team_color2)) +
  scale_color_identity(aesthetics = c("fill",
                                      "color")) +
  # scale_x_continuous can let you adjust scale characteristics
  scale_x_continuous(
    # breaks specify how many breaks there are, {scales} helps with it
    breaks = scales::pretty_breaks(n = 10),
    # set x axis limts
    limits = c(0, 300),
    # now expand helps bring that 0 closer to where it should be
    expand = expansion(mult = c(0, 0.1))) +
  # and I think the axes are explanatory so, getting rid of titles
  # and adding a main plot title and caption
  labs(y = NULL,
       x = NULL,
       title = "NFL Regular Team Wins from 2000-Current",
       caption = "Data courtesy of {nflreadr}")
wins_plt2

# and save the plot
# i prefer ggsave() and .tiff file formats
wins_plt2 %>% 
  ggsave(filename = "Figures/nfl_wins.tiff",
         compression = "lzw+p",
         height = 13,
         width = 8)

# other information
# adding in team logos to plot
wins_with_colors
wins_plt2 +
  geom_image(aes(image = team_logo_espn))

records_dat3 <- records_dat2 %>% 
  filter(!winner == "TIE",
         !season == "2023") %>% 
  pivot_longer(contains("score"),
               names_to = c("team_loc", "dum"),
               values_to = "score",
               names_sep = "_") %>% 
  select(-dum) %>% 
  mutate(team = case_when(team_loc == "home" ~ home_team,
                          team_loc == "away" ~ away_team),
         team = nflreadr::clean_team_abbrs(team),
         home_team = nflreadr::clean_team_abbrs(home_team),
         away_team = nflreadr::clean_team_abbrs(away_team),
         winner_ind = ifelse(winner == team, 0, 1),
         winner_ind2 = ifelse(winner == team, "winner", "loser"),
         winner_ind2 = fct(winner_ind2, levels = c("winner",
                                                   "loser")),
         winner_loc = case_when(winner == home_team ~ "home",
                                winner == away_team ~ "away"),
         winner_loc = fct(winner_loc, levels = c("home",
                                                 "away"))) 
length(unique(records_dat2$winner))

records_dat3 %>% 
  ggplot(aes(y = winner_ind2,
             x = score)) +
  geom_violin(fill = "lightgreen")

m1 <- glm(winner_ind2 ~ winner_loc, family = "binomial",
          data = records_dat3)
summary(m1)
exp(coef(m1))

m2 <- glm(winner_ind2 ~ score, family = "binomial",
          data = records_dat3)

m3 <- glm(winner_ind2 ~ winner_loc + score, family = "binomial",
          data = records_dat3)
library(performance)
performance::compare_performance(m1, m2, m3) %>% 
  flextable::flextable()

anova(m2, m3, test = "Chisq")
exp(coef(m3))
coef(m3)
library(lme4)
m2 <- glmer(winner_ind2 ~  winner_loc + (1 | game_id),
            family = "binomial",
            data = records_dat3)
summary(m2)
exp(fixef(m2))
exp(coef(m1))

records_dat3 %>% 
  group_by(winner_loc) %>% 
  tally()
records_dat3  %>% 
  filter(is.na(winner_loc))

