library(tidyverse)

csv_files <- list.files("data/", pattern = "\\.csv$")

coaches = read.csv(paste0("data/", csv_files[grepl("coaches", csv_files)])) %>%
  replace(is.na(.), 0)
fran = read.csv(paste0("data/", csv_files[grepl("franchise_encyclopedia", csv_files)]))
odds = read.csv(paste0("data/", csv_files[grepl("preseason_odds", csv_files)]))
ratings = read.csv(paste0("data/", csv_files[grepl("ratings", csv_files)]))
stats = read.csv(paste0("data/", csv_files[grepl("team_stats_rs", csv_files)]))
pstats = read.csv(paste0("data/", csv_files[grepl("team_stats_playoffs", csv_files)]))
breakdown = read.csv("data/team_breakdown.csv")

df = fran %>%
  merge(stats, by = c("team", "year"), all = TRUE) %>%
  merge(pstats, by = c("team", "year"), all = TRUE) %>%
  merge(odds, by = c("team", "year"), all = TRUE) %>%
  merge(ratings, by = c("team", "year"), all = TRUE) %>%
  merge(coaches, by.x = c("team", "year", "start_coach"), by.y = c("team", "year", "coach_id"), all.x = TRUE) %>%
  merge(coaches, by.x = c("team", "year", "end_coach"), by.y = c("team", "year", "coach_id"), all.x = TRUE, suffixes = c("_start", "_end")) %>%
  select(team, year, playoffs, everything()) %>%
  select(-c(coach_start, coach_end))

write.csv(df, "data/all_team_stats_1980-2023.csv", row.names = FALSE)


