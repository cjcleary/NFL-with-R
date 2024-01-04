#### Passing Touchdowns by Team ------------------------------------
passing_dat_2023 <-  passing_dat %>% 
  filter(year == "2016",
         position == "QB") %>% 
  group_by(team) %>% 
  summarize(pass_tds = sum(passing_tds)) %>% 
  left_join(nflfastR::teams_colors_logos[,c("team_abbr",
                                            "team_color",
                                            "team_color2")],
            by = c("team" = "team_abbr")) 

passing_dat_2023 %>% 
  ggplot(aes(y = fct_reorder(team, pass_tds),
             x = pass_tds)) +
  geom_col(aes(color = team_color2, 
               fill = team_color)) +
  scale_fill_identity(aesthetics = c("color", "fill")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05)),
                     breaks = scales::pretty_breaks(n = 6)) +
  labs(y = NULL,
       x = "Passing Touchdowns") +
  theme_classic() +
  theme(axis.text.y = element_nfl_logo(),
        legend.position = NULL) 