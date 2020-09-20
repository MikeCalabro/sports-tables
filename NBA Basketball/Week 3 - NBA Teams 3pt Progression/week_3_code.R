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
  filter(!tm == "TOT") %>%
  filter(!tm == "NOP") %>%
  filter(!tm == "NOH") %>%
  filter(!tm == "CHO") %>%
  filter(!tm == "CHA")

table_data <- plot_data %>%
  spread(season, per_game_3pa) %>%
  filter(!is.na(`2013`)) %>%
  filter(!is.na(`2019`)) %>%
  mutate(growth = (`2020`/`2013` - 1)) %>%
  arrange(desc(growth)) %>%
  select(tm, `2013`, `2020`, growth) %>%
  mutate(PLOT = NA) %>%
  filter(growth > .8635 | growth < .60)

plot_maker <- function(data){
  data %>%
    ggplot() +
    geom_line(aes(x = season, y = per_game_3pa), color = "red", size = 14) +
    theme_void() +
    theme(legend.position = "none")
}

plots <- plot_data %>%
  nest(threes = c(season, per_game_3pa)) %>%
  mutate(plot = map(threes, plot_maker)) %>%
  left_join(table_data %>% select(tm, growth)) %>%
  arrange(desc(growth)) %>%
  filter(growth > .8635 | growth < .60)


table_data %>%
  gt(groupname_col = "none") %>%
  cols_width(
    vars("tm")   ~ px(70),
    vars("2020") ~ px(80),
    everything() ~ px(90)
  ) %>%
  tab_header(
    title = md("**RISE OF THE 3-POINT SHOT**"),
    subtitle = md("3-Point Shot Attempt growth for every NBA team between 2013 and 2020... Somebody tell the Knicks, please!")
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
    tm = " ",
    growth = "PERCENT",
    PLOT = "PLOT"
  ) %>%
  tab_spanner(
    label = "3-PT ATTEMPTS",
    columns = vars("2013", "2020")
  ) %>%
  tab_spanner(
    label = "GROWTH",
    columns = vars("growth", "PLOT")
  ) %>%
  tab_row_group(
    group = "BOT 8",
    rows = (9:16)
  ) %>%
  tab_row_group(
    group = "TOP 8",
    rows = (1:8)
  ) %>%
  tab_style(
    style = cell_text(
      weight = "bold"
    ),
    locations = cells_row_groups()
  ) %>%
  tab_options(
    row_group.padding = 1
  ) %>%
  cols_align(
    align = "center",
    columns = vars("2013", "2020")
  ) %>%
  text_transform(
    locations = cells_body(
      columns = vars("PLOT")
    ),
    fn = function(x){
      map(plots$plot, ggplot_image, height = px(15), aspect_ratio = 4)
    }
  )

