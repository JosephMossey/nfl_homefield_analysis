install.packages("tidyverse")
install.packages("nflfastR")
install.packages("ggimage")
install.packages("gt")
install.packages("gtExtras")
install.packages("ggrepel")

library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(ggrepel)

pbp <- load_pbp(2000:2024)


stadium_data <- pbp %>%
  filter(season_type == "REG") %>%
  filter(!is.na(game_stadium)) %>%
  filter(!is.na(result))

nrow(stadium_data)

stadium_summary <- stadium_data %>%
  group_by(game_stadium, home_team) %>%
 
  summarize(
    total_games = n_distinct(game_id),
    avg_pd = mean(result),
    wins = n_distinct(game_id[result > 0]),
    win_pct = wins / total_games,) %>%
  filter(total_games >= 50) %>%
  arrange(desc(avg_pd))

stadium_summary <- stadium_summary %>%
  left_join(teams_colors_logos, by = c("home_team" = "team_abbr"))
 
  stadium_summary %>%
  ggplot(aes(x = win_pct, y = avg_pd)) +
    geom_point(aes(size = total_games, color = team_color),
               alpha = 0.9) +
    scale_color_identity() +
    guides(color= "none") +
    geom_text_repel(aes(label = paste(game_stadium,"\n", home_team)),
                   size = 2.5,
                  fontface = "bold",
              hjust = 0.5,
              vjust = 0.5) +
    scale_x_continuous(
      breaks = seq(0.3, 0.9, by = 0.1),
      labels = scales::percent_format()
    ) +
    scale_y_continuous(
      breaks = seq(-4, 9, by = 1),
    ) +
    theme_bw() +
    labs(x = "Win Percentage",
         y = "Avg Points Differential",
         title = "Home Field Advantage Analysis by NFL Stadium (2000-2024)",
         subtitle = "Minimum 50 Games Played",
         caption = "By Joseph Mossey | @jmanalytics | nflfastr")
