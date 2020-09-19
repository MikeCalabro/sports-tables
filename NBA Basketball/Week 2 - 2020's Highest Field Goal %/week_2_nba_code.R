# Highest Field Goal % 2020

library(tidyverse)
library(ballr)
library(reactable)
library(gt)

nba_data <- NBAPerGameStatistics(season = 2020)

table_data <- nba_data %>%
  filter(g >= 50) %>%
  mutate(fga = ceiling(fga*g)) %>%
  filter(fga > 250) %>%
  select(player, tm, fga, fgpercent) %>%
  arrange(desc(fgpercent)) %>%
  head(15)

table_data %>%
  gt() %>%
  tab_header(
    title = md("**Feed These Men The ROCK**"),
    subtitle = "The NBA Players in the 2020 season with the highest field goal 
                percentage, among players who played at least 50 games and attempted
                at least 250 shots"
  ) %>%
  data_color(
    columns = vars("fgpercent"),
    colors = scales::col_numeric(
      palette = c("white", "tomato"),
      domain = c(.58, .742)
    )
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
  cols_label(
    player = "PLAYER",
    tm = "TEAM",
    fga = "FG ATTEMPTS",
    fgpercent = "FG PERCENT"
  ) %>%
  cols_width(
    vars("player")    ~ px(170),
    vars("tm")        ~ px(80),
    vars("fga")       ~ px(140),
    vars("fgpercent") ~ px(150)
  ) %>%
  opt_align_table_header(
    align = "left"
    ) %>%
  tab_options(
    table.font.size = 15,
    data_row.padding = 4
  )
  
