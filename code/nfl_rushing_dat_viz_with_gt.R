### visualizing rushing statistics across time with {ggplot2} -----------------------
# load in libraries
library(tidyverse)
library(scales)
library(gt)
library(gtExtras)

# read in datafile,  i prefer csv
rush_1 <- read_csv("Data Files/nfl-rushing-statistics/NFL_rushing_statistics1950-2023.csv")
str(rush_1)

unique(rush_1$position)
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
rush_2 %>% 
  group_by(player_name) %>% 
  summarize(names(which.max(table(team))))



# explore the data, let's see top 5 leading rushers per decade
rb_by_decade_tbl <- rush_2 %>% 
  group_by(decade, player_name) %>% 
  filter(rush_attempts > 100) %>% 
  summarize(max = sum(rush_yards),
            team = names(which.max(table(team)))) %>% 
  slice_max(n = 5, order_by = max) %>% 
  ungroup() %>% 
  select(
    decade,
    player = player_name,
    team,
    max) %>%  
  gt(groupname_col =  "decade") %>%
  tab_header(title = "NFL Top 5 Rushing Leaders since 1970") %>% 
  tab_style(locations = cells_title(groups = c("title")),
            style = list(cell_text(weight = "bolder",
                                   size = px(25),
                                   align = "left"),
                         cell_borders(sides = c("t", "b"),
                                      style = NULL))) %>% 
  tab_style(locations = cells_title(groups = "title"),
            style = cell_text(color = "#013369")) %>%
  tab_style(locations = cells_title(groups = "subtitle"),
            style = list(cell_text(weight = "bolder",
                                   size = px(15),
                                   align = "left",
                                   color = "#D50A0A"),
                         cell_borders(sides = "all",
                                      style = "hidden"))) %>% 
  tab_footnote(footnote = "Data from PFR | Table: @cleary_cj") %>% 
  tab_style(locations = cells_footnotes(),
            style = list(cell_text(weight = "lighter",
                                   align = "right",
                                   font = "Times"),
                         cell_borders(sides = c("t", "b"),
                                      style = "hidden"))) %>% 
  tab_style(locations = cells_row_groups(),
            style = list(cell_text(weight = "bold",
                                   align = "center", 
                                   decorate = "underline",  
                                   size = px(25), 
                                   font = "Times"),
                         cell_borders(sides = c("t", 
                                                "b"),
                                      weight = 3))) %>% 
  tab_style(locations = cells_body(columns = player),
            style = cell_text(weight = "bolder")) %>% 
  cols_hide(team) %>% 
  fmt_image(columns = team_wordmark) %>%
  tab_style(locations = cells_body(columns = c(player, 
                                               team_wordmark,
                                               max)),
            style = list(cell_text(align = "center",
                                   font = "Times"),
                         cell_borders(sides = "all",
                                      color = NULL))) %>% 
  tab_options(column_labels.hidden = T) 

# save table
rb_by_decade_tbl %>% 
  gtsave(filename = "Figures/nfl-rb-by-decade.pdf", 
         expand = 2)