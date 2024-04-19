library(tidyverse)
library(XML)
library(stringr)
source("functions.R")

seasons <- 2023

players <- scrape_yearly_tables("https://www.basketball-reference.com/leagues/NBA_", seasons, 1, "_totals.html")

test <- players %>%
  filter(Rk != "Rk",
         Tm != "TOT") %>%
  select(-Rk)

