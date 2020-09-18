# Overconfident Shooters

library(tidyverse)
library(ballr)
library(reactable)
library(gt)

nba_data <- NBAPerGameStatistics(season = 2020)

table_data <- nba_data %>%
  filter(g > 50) %>%
  arrange(x3ppercent) %>%
  filter(x3pa > 2) %>%
  head(10) %>%
  mutate(x3ppercent = 100*x3ppercent,
         x3p = ceiling(x3p*g),
         x3pa = ceiling(x3pa*g)) %>%
  select(player, tm, x3pa, x3p, x3ppercent)

overconfident_shooters <- 
  table_data %>%
  gt() %>%
  tab_header(
    title = md('**"Just Go To The Rack!**"'),
    subtitle = md("**A Loserboard of the NBA's Most Overconfident 3-Point Shooters in 2020**")
  ) %>%
  tab_source_note(
    source_note = md("Data Source: [Basketball Reference](basketball-reference.com)")
  ) %>%
  cols_label(
    player = md("**Player**"),
    tm = md("**Team**"),
    x3pa = md("**3pt Attempts**"),
    x3p = md("**3pt Makes**"),
    x3ppercent = md("**3pt Make %**")
  ) %>%
  cols_width(
    vars("x3pa") ~ px(115),
    vars("x3p") ~ px(100),
    vars("tm") ~ px(60),
    vars("player") ~ px(150)
  ) %>%
  cols_align(
    align = "center",
    columns = c("tm", "x3pa", "x3p", "x3ppercent")
  ) %>%
  data_color(
    columns = c("x3ppercent"),
    colors = scales::col_numeric(
      palette = c("blue", "white"),
      domain = c(23, 32)
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("left"),
      color = "#gray",
      weight = px(2.5),
      style = "solid"
    ),
    locations = list(
      cells_body(
        columns = vars("tm"),
        rows = everything()
      )
    )
  )

overconfident_shooters %>%
  gtsave("week_1_3pt_loserboard.png")

