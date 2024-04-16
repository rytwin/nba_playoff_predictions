library(tidyverse)

coaches = read.csv("data/coaches_1980-2023.csv") %>%
  mutate(coach_id = gsub("st. jean", "stjean", coach_id)) %>%
  replace(is.na(.), 0)
fran = read.csv("data/franchise_encyclopedia_1980-2023.csv")
odds = read.csv("data/preseason_odds_1980-2023.csv")
ratings = read.csv("data/ratings_1980-2023.csv")
stats = read.csv("data/team_stats_1980-2023.csv")
pstats = read.csv("data/team_stats_playoffs_1980-2023.csv")
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


