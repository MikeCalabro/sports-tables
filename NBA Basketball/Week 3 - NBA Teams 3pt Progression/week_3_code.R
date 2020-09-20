# Week 3 NBA 3pt rise over time by team

library(tidyverse)
library(ballr)
library(gt)
library(DT)

seasons <- (2013:2020)

nba_data <- purrr::map_df(seasons, function(x){
  NBAPerGameStatistics(season = x) %>%
    mutate(season = x)
})

plot_data <- nba_data %>%
  mutate(total_3pa = ceiling(x3pa * g)) %>%
  group_by(tm, season) %>%
  summarise(team_3pa = sum(total_3pa), g = ifelse(season == 2020, max(g), 82)) %>%
  distinct() %>%
  mutate(per_game_3pa = round(team_3pa / g, 1)) %>%
  select(tm, season, per_game_3pa) %>%
  filter(!tm == "TOT")

table_data <- plot_data %>%
  spread(season, per_game_3pa) %>%
  filter(!is.na(`2013`)) %>%
  filter(!is.na(`2019`)) %>%
  mutate(growth = (`2020`/`2013` - 1)) %>%
  arrange(desc(growth)) %>%
  filter(!tm == "TOT") %>%
  select(tm, `2013`, `2020`, growth) %>%
  mutate(PLOT = NA)

test_plot <- plot_data %>%
  filter(tm == "CHI") %>%
  ggplot() +
  geom_line(aes(x = season, y = per_game_3pa), color = "red", size = 14) +
  theme_void() 



table_data %>%
  gt(groupname_col = "none") %>%
  cols_width(
    vars("tm")   ~ px(50),
    vars("2020") ~ px(80),
    everything() ~ px(90)
  ) %>%
  tab_header(
    title = md("**RISE OF THE 3-POINT SHOT**"),
    subtitle = md("The growth of 3-point shot attempts for NBA team between 2013 and 2020")
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("top"),
      color = "black",
      weight = px(2.6),
      style = "solid"
    ),
    locations = list(
      cells_body(
        columns = everything(),
        rows = 1
      )
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("top"),
      color = "#F5F5F5",
      weight = px(2.5),
      style = "solid"
    ),
    locations = list(
      cells_title(
        groups = "title"
      )
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "white",
      weight = px(2.5),
      style = "solid"
    ),
    locations = list(
      cells_title(
        groups = "subtitle"
      )
    )
  ) %>%
  opt_align_table_header(
    align = "left"
  ) %>%
  tab_options(
    table.font.size = 15,
    data_row.padding = 4
  ) %>%
  fmt_percent(
    columns = "growth",
    rows = everything(),
    decimals = 1
  ) %>%
  fmt_number(
    columns = "growth",
    rows = 2:nrow(table_data),
    scale_by = 100,
    decimals = 1
  ) %>%
  data_color(
    columns = vars("growth"),
    colors = scales::col_numeric(
      palette = c("blue", "white", "red"),
      domain = c(-.9, 1.3)
    )
  ) %>%
  cols_label(
    tm = "TEAM",
    growth = "GROWTH"
  )


