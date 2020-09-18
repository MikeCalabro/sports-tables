# 2019 QBs

library(tidyverse)
library(nflfastR)
library(gt)

# Reads in nfl play-by-play data from github
nfl <- readRDS(url("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2019.rds"))

glimpse(nfl)

# Creates a list of QBs who played every game for their team for weeks 1-16 of 2019
starting_qbs <- nfl %>%
  filter(season_type == "REG") %>%
  filter(!week == 17) %>%
  group_by(passer, week) %>%
  summarise(plays_run = n()) %>%
  filter(plays_run > 10) %>%
  count(passer) %>%
  filter(n > 14) %>%
  filter(!is.na(passer)) %>%
  select(passer) %>%
  deframe()
