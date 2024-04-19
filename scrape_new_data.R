library(tidyverse)
library(XML)
library(stringr)
source("functions.R")

csv_files <- list.files("data/", pattern = "\\.csv$")
csv_files <- csv_files[grepl("\\d{4}-\\d{4}.csv$", csv_files)]
year_min = 3000
year_max = 0
for(f in csv_files) {
  
  if(as.integer(substr(f, nchar(f) - 12, nchar(f) - 9)) < year_min) {
    year_min = as.integer(substr(f, nchar(f) - 12, nchar(f) - 9))
    print(paste0("Start year changed to ", year_min))
  }
  if(as.integer(substr(f, nchar(f) - 7, nchar(f) - 4)) > year_max) {
    year_max = as.integer(substr(f, nchar(f) - 7, nchar(f) - 4))
    print(paste0("End year changed to ", year_max))
  }
}

file_suffix = paste0(year_min, "-", year_max, ".csv")
existing_data <- read.csv(paste0("data/", csv_files[grepl("all_team_stats_", csv_files)]))

new_seasons = 2024

# scrape team stat tables: total, opp total, per 100 poss, opp per 100 poss, advanced, shooting, opp shooting 
team_stats_rs <- scrape_team_stats(new_seasons, regular_season = TRUE, playoff_series_completed = 0) %>%
         mutate(across(c(team, arena), as.character),
                across(c(year:fga, fg3:fga3, fg2:fga2, ft:fta, oreb:opp_fga, opp_fg3:opp_fga3,
                         opp_fg2:opp_fga2, opp_ft:opp_fta, opp_oreb:opp_pts, age:pl, attend:attend_pg, dunks_made,
                         layups_made, heaves_att:heaves_made, opp_dunks_made, opp_layups_made), as.integer),
                across(c(fgp, fgp3, fgp2, ftp, opp_fgp, opp_fgp3, opp_fgp2, opp_ftp, fg_per100:opp_pts_per100,
                         mov:opp_ft_per_fga, avg_fg_dist:pct_of_fg_dunk, pct_of_fg_layup, pct_of_3_corner:fgp3_corner,
                         opp_avg_fg_dist:opp_pct_of_fg_dunk, opp_pct_of_fg_layup, opp_pct_of_3_corner:opp_fgp3_corner), as.numeric))
ts_rs_orig <- read.csv(paste0("data/team_stats_rs_", file_suffix))
team_stats_rs <- bind_rows(ts_rs_orig, team_stats_rs)
write.csv(team_stats_rs, paste0("data/team_stats_rs_", substr(file_suffix, 1, 4), "-", max(new_seasons), ".csv"), row.names = FALSE) 


# scrape information from franchise encyclopedia
franchise_tables <- list()
for(t in franchises) {
  team_url <- paste0("https://www.basketball-reference.com/teams/", t)
  table <- data.frame(scrape_bballref(team_url, 1))
  franchise_tables <- c(franchise_tables, list(table))
  print(t)
  Sys.sleep(10)
}

team_df <- bind_rows(franchise_tables)
names(team_df) <- colnames_franchise_encyc
team_df <- team_df %>%
  rename_teams() %>%
  mutate(year = as.integer(paste0(substr(year, 1, 2), substr(year, 6, 7))),
         year = ifelse(year == 1900, 2000, year),
         div_finish = as.integer(substr(finish, 1, 1)),
         div_teams = as.integer(substr(finish, nchar(finish), nchar(finish))),
         playoff = tolower(playoff),
         champion = case_when(playoff == "won finals" ~ 1,
                              TRUE ~ 0),
         finals = case_when(champion == 1 ~ 1,
                            playoff == "lost finals" ~ 1,
                            TRUE ~ 0),
         conf_finals = case_when(champion == 1 | finals == 1 ~ 1,
                                 grepl("conf. finals", playoff) ~ 1,
                                 TRUE ~ 0),
         second_rd = case_when(champion == 1 | finals == 1 | conf_finals == 1 ~ 1,
                               grepl("conf. semis", playoff) ~ 1,
                               TRUE ~ 0),
         playoffs = case_when(champion == 1 | finals == 1 | conf_finals == 1 | second_rd == 1 ~ 1,
                              grepl("1st rnd.", playoff) ~ 1,
                              TRUE ~ 0),
         coach = tolower(coach),
         num_coaches = str_count(coach, ",") + 1,
         start_coach = sub("\\s*\\(.*", "", coach),
         end_coach = sub(".*,(.*?)(?=\\()", "\\1", coach, perl=TRUE),
         end_coach = sub("\\s*\\(.*", "", end_coach),
         across(c(start_coach, end_coach), trimws),
         top_ws = as.numeric(gsub(".*\\((.*?)\\).*", "\\1", top_ws))) %>%
  filter(year %in% new_seasons) %>%
  select(-c(league, w, l, srs, pace, ortg, drtg, del1, del2, finish, coach, playoff)) %>%
  mutate(across(c(start_coach, end_coach), ~ gsub("\\.\\s", "", .))) %>%
  rename_duplicate_coaches("start_coach") %>%
  select(-start_coach) %>%
  rename(start_coach = coach) %>%
  rename_duplicate_coaches("end_coach") %>%
  select(-end_coach) %>%
  rename(end_coach = coach) %>%
  mutate(across(c(year, div_finish:num_coaches), as.integer),
         across(c(team, start_coach:end_coach), as.character),
         across(c(win_pct:top_ws), as.numeric))

fran_orig <- read.csv(paste0("data/franchise_encyclopedia_", file_suffix))
fran_encyc <- bind_rows(fran_orig, team_df)
write.csv(fran_encyc, paste0("data/franchise_encyclopedia_", substr(file_suffix, 1, 4), "-", max(new_seasons), ".csv"), row.names = FALSE) 


# scrape team ratings pages
ratings <- scrape_yearly_tables("https://www.basketball-reference.com/leagues/NBA_", new_seasons, 1, "_ratings.html")
names(ratings) <- colnames_team_ratings
ratings_df <- ratings %>%
  mutate(across(c(conf, div), tolower)) %>%
  select(team, year, conf, div, ortg2, drtg2, net_rtg2, mov_adj, ortg2_adj, drtg2_adj, net_rtg2_adj) %>%
  rename_teams() %>%
  mutate(year = as.integer(year),
         across(c(team, conf:div), as.character),
         across(c(ortg2:net_rtg2_adj), as.numeric))

ratings_orig <- read.csv(paste0("data/ratings_", file_suffix))
ratings_df <- bind_rows(ratings_orig, ratings_df)
write.csv(ratings_df, paste0("data/ratings_", substr(file_suffix, 1, 4), "-", max(new_seasons), ".csv"), row.names = FALSE)


# scrape coach information
coaches <- scrape_yearly_tables("https://www.basketball-reference.com/leagues/NBA_", new_seasons, 1, "_coaches.html")
names(coaches) <- colnames_coaches
coaches_df <- coaches %>%
  rename_teams_abbrev() %>%
  mutate(coach = tolower(coach)) %>%
  select(-starts_with("del"))

check_coach_names <- coaches_df %>%
  group_by(coach, coach_exp) %>%
  summarize(count = n()) %>%
  filter(count > 1) %>%
  pull(coach)

coaches_df <- coaches_df %>%
  rename_duplicate_coaches("coach") %>%
  mutate(coach_id = paste0(substr(coach, 1, 1), sub("^[^ ]+ ", "", coach)),
         coach_id = gsub("st. jean", "stjean", coach_id),
         across(c(coach:team, coach_id), as.character),
         across(c(coach_team_exp:coach_career_l, coach_season_p_g:year), as.integer),
         coach_win_pct = as.numeric(coach_win_pct))
  
coaches_orig <- read.csv(paste0("data/coaches_", file_suffix))
coaches_df <- bind_rows(coaches_orig, coaches_df)
write.csv(coaches_df, paste0("data/coaches_", substr(file_suffix, 1, 4), "-", max(new_seasons), ".csv"), row.names = FALSE)


# scrape playoff team stat tables: total, opp total, per 100 poss, opp per 100 poss, advanced, shooting, opp shooting 
team_stats_playoffs <- scrape_team_stats(new_seasons, regular_season = FALSE)
keep_name_cols <- c("team", "year")
cols_to_rename <- setdiff(names(team_stats_playoffs), keep_name_cols)
new_colnames <- paste0("p_", cols_to_rename)
names(team_stats_playoffs) <- c(keep_name_cols, new_colnames)

ts_po_orig <- read.csv(paste0("data/team_stats_playoffs_", file_suffix))
team_stats_playoffs <- bind_rows(ts_po_orig, team_stats_playoffs)
write.csv(team_stats_playoffs, paste0("data/team_stats_playoffs_", substr(file_suffix, 1, 4), "-", max(new_seasons), ".csv"), row.names = FALSE) 


# scrape preseason odds
seasons_odds <- new_seasons[new_seasons >= 1985]
odds <- scrape_yearly_tables("https://www.basketball-reference.com/leagues/NBA_", seasons_odds, 1, "_preseason_odds.html")
if(min(seasons_odds) > 1995) {
  names(odds) <- colnames_preseason_odds_c
} else if(max(seasons_odds) > 1995 & min(seasons_odds) <= 1995) {
  names(odds) <- colnames_preseason_odds_b
} else {
  names(odds) <- colnames_preseason_odds_a
}
odds_df <- odds %>%
  mutate(champ_odds = as.integer(champ_odds)) %>%
  select(team, year, champ_odds, o_u) %>%
  rename_teams() %>%
  mutate(team = as.character(team),
         across(year:champ_odds, as.integer),
         o_u = as.numeric(o_u))

odds_orig <- read.csv(paste0("data/preseason_odds_", file_suffix))
odds_df <- bind_rows(odds_orig, odds_df)
write.csv(odds_df, paste0("data/preseason_odds_", substr(file_suffix, 1, 4), "-", max(new_seasons), ".csv"), row.names = FALSE)

