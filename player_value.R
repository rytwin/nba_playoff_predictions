library(tidyverse)
source("functions.R")

# read in data
data <- read.csv("data/all_player_stats_1980-2024_single_rows.csv")


# add a few per game stats, percent of team games played, and adjusted win shares metric (WS with a prior)
df <- data %>%
  mutate(min_pg = mp / g,
         pts_pg = pts / g,
         trb_pg = trb / g,
         ast_pg = ast / g,
         stl_pg = stl / g,
         blk_pg = blk / g,
         tov_pg = tov / g,
         p_pts_pg = p_pts / p_g,
         allnba_val = case_when(allnba_tm == 1 ~ 10,
                                allnba_tm == 2 ~ 5,
                                allnba_tm == 3 ~ 1),
         team_gm = case_when(year == 1999 ~ 50,
                             year == 2012 ~ 66,
                             year == 2021 ~ 72,
                             year == 2020 & (team == "CHI" | team == "NYK" | team == "GSW" | team == "CLE" | team == "MIN" |
                                               team == "CHO" | team == "ATL" | team == "DET") ~ 66,
                             year == 2020 ~ 72,
                             TRUE ~ 82),
         g_pct = g / team_gm,
         ws_48_adj = (ws / (mp + 48)) * 48,
         p_ws_48_adj = (p_ws / (p_mp + 48)) * 48)

# select columns to use and create variables with data from prior seasons
cols_prev <- c("g", "mp", "pts", "trb", "ast", "stl", "blk", "tov", "g_pct", "min_pg", "pts_pg", "trb_pg", "ast_pg",
               "stl_pg", "blk_pg", "tov_pg", "ortg", "drtg", "per", "ws", "ws_48", "ws_48_adj", "bpm", "vorp",
               "p_g", "p_mp", "p_pts", "p_pts_pg", "p_ws", "p_ws_48", "p_ws_48_adj", "p_bpm", "p_vorp", "mvp", "mvp_share",
               "roy", "roy_share", "dpoy", "dpoy_share", "smoy", "smoy_share", "mip", "mip_share", "allnba", "allnba1",
               "allnba2", "allnba3", "allnba_share", "alldef", "alldef1", "alldef2", "alldef_share", "allnba_val")
for(c in cols_prev){
  df <- prior_years_stats_simple(df, c, c(1:3), "player_id")
}

# beta decay formula, used for weighted average in some variables below
beta <- 0.2
# weights to use when a player has at least 3 previous seasons
weights3 <- exp(-beta * c(1, 2, 3))
weights3 <- weights3 / sum(weights3)
# weights to use when a player has only 2 previous seasons
weights2 <- exp(-beta * c(1, 2))
weights2 <- weights2 / sum(weights2)

# create new features and select relevant features
features_df <- df %>%
  arrange(player, year) %>%
  mutate(across(c(allnba, allnba_share, g_1yr:allnba_val_3yr), ~ replace(., is.na(.), 0)),
         pick = ifelse(is.na(pick) | pick > 65, 65, pick)) %>%
  group_by(player_id) %>%
  mutate(cum_pts_pg = ifelse(is.na(cumsum(pts_1yr) / cumsum(g_1yr)), 0, cumsum(pts_1yr) / cumsum(g_1yr)),
         cum_ws = cumsum(ws_1yr),
         cum_vorp = cumsum(vorp_1yr),
         cum_p_pts_pg = ifelse(is.na(cumsum(p_pts_1yr) / cumsum(p_g_1yr)), 0, cumsum(p_pts_1yr) / cumsum(p_g_1yr)),
         cum_p_ws = cumsum(p_ws_1yr),
         cum_p_vorp = cumsum(p_vorp_1yr),
         cum_mvp_share = cumsum(mvp_share_1yr),
         cum_dpoy_share = cumsum(dpoy_share_1yr),
         cum_allnba = cumsum(allnba_1yr),
         cum_allnba_share = cumsum(allnba_share_1yr),
         cum_alldef = cumsum(alldef_1yr),
         cum_alldef_share = cumsum(alldef_share_1yr),
         cum_allnba_val = cumsum(allnba_val_1yr),
         cum_allnba1 = cumsum(allnba1_1yr),
         cum_allnba2 = cumsum(allnba2_1yr),
         cum_allnba3 = cumsum(allnba3_1yr),
         ws_48_adj_avg_3yr = case_when(exp > 3 ~ ws_48_adj_1yr * weights3[1] + ws_48_adj_2yr * weights3[2] + ws_48_adj_3yr * weights3[3],
                                       exp == 3 ~ ws_48_adj_1yr * weights2[1] + ws_48_adj_2yr * weights2[2],
                                       exp <= 2 ~ ws_48_adj_1yr),
         allnba_share_avg_3yr = case_when(exp > 3 ~ allnba_share_1yr * weights3[1] + allnba_share_2yr * weights3[2] + allnba_share_3yr * weights3[3],
                                          exp == 3 ~ allnba_share_1yr * weights2[1] + allnba_share_2yr * weights2[2],
                                          exp <= 2 ~ allnba_share_1yr),
         mvp_share_avg_3yr = case_when(exp > 3 ~ mvp_share_1yr * weights3[1] + mvp_share_2yr * weights3[2] + mvp_share_3yr * weights3[3],
                                       exp == 3 ~ mvp_share_1yr * weights2[1] + mvp_share_2yr * weights2[2],
                                       exp <= 2 ~ mvp_share_1yr),
         allnba_val_avg_3yr = case_when(exp > 3 ~ allnba_val_1yr * weights3[1] + allnba_val_2yr * weights3[2] + allnba_val_3yr * weights3[3],
                                        exp == 3 ~ allnba_val_1yr * weights2[1] + allnba_val_2yr * weights2[2],
                                        exp <= 2 ~ allnba_val_1yr),) %>%
  ungroup() %>%
  select(player_id, pl_yr_id, year, pick, age, exp, yrs_off, allnba, allnba_share, g_1yr:allnba_val_avg_3yr)


# NOTE: players who were on an opening night roster for their final season but did not play that season will have NA values for most stats.
# You will need to manually add them like allnba_share to get an accurate value for them:
# -When creating features_df, need to replace NAs with 0 and select the column at the end
# -Add variables in features_plus: the 1 year prior version with case_whens and the current version replacing NAs with 0 again
# -Unselect the original column at the end of features_plus


# read in opening day rosters
od <- read.csv("data/opening_day_rosters.csv") %>%
  mutate(od_player_id = paste0(gsub("\\s", "", tolower(player)), "_", gsub("\\s", "", tolower(college)), "_", draft_year, "_", pick),
         pl_yr_id = paste0(gsub("\\s", "", tolower(player)), "_", gsub("\\s", "", tolower(college)), "_", draft_year, "_", pick, "_", year)) %>%
  select(pl_yr_id, od_player_id, od_team, year, age, pick, out_for_season) %>%
  rename(od_year = year,
         od_age = age,
         od_pick = pick)

# merge player stats with opening day rosters
# for players who were on an opening night roster for a season but didn't play (so they aren't in the stats dataframe), fill in the variables
features_plus <- features_df %>%
  merge(od, by = "pl_yr_id", all.y = TRUE) %>%
  mutate(year = ifelse(is.na(year), od_year, year),
         player_id = ifelse(is.na(player_id), od_player_id, player_id),
         age = ifelse(is.na(age), od_age, age),
         pick = ifelse(is.na(pick), od_pick, pick),
         pick = ifelse(is.na(pick), 65, pick)) %>%
  arrange(player_id, year) %>%
  mutate(across(c(pick, g_1yr:allnba_val_avg_3yr), ~ case_when(!is.na(.) ~ .,
                                                               lead(player_id) != player_id & lag(player_id) != player_id ~ 0,
                                                               !is.na(lead(.)) & lead(player_id) == player_id ~ lead(.),
                                                               !is.na(lead(., 2)) & lead(player_id, 2) == player_id ~ lead(., 2),
                                                               !is.na(lead(., 3)) & lead(player_id, 3) == player_id ~ lead(., 3),
                                                               TRUE ~ .)),
         allnba_share_1yr = case_when(!is.na(allnba_share_1yr) ~ allnba_share_1yr,
                                      lag(player_id) != player_id ~ 0,
                                      !is.na(lag(allnba_share)) ~ lag(allnba_share),
                                      lag(player_id, 2) != player_id ~ 0,
                                      !is.na(lag(allnba_share, 2)) ~ lag(allnba_share, 2),
                                      lag(player_id, 3) != player_id ~ 0,
                                      !is.na(lag(allnba_share, 2)) ~ lag(allnba_share, 2),
                                      TRUE ~ allnba_share_1yr),
         allnba_share = ifelse(is.na(allnba_share), 0, allnba_share),
         allnba_1yr = case_when(!is.na(allnba_1yr) ~ allnba_1yr,
                                      lag(player_id) != player_id ~ 0,
                                      !is.na(lag(allnba)) ~ lag(allnba),
                                      lag(player_id, 2) != player_id ~ 0,
                                      !is.na(lag(allnba, 2)) ~ lag(allnba, 2),
                                      lag(player_id, 3) != player_id ~ 0,
                                      !is.na(lag(allnba, 2)) ~ lag(allnba, 2),
                                      TRUE ~ allnba_1yr),
         allnba = ifelse(is.na(allnba), 0, allnba),
         exp = case_when(!is.na(exp) ~ exp,
                         lead(player_id) != player_id & lag(player_id) != player_id ~ 1,
                         !is.na(lag(exp)) & lag(player_id) == player_id ~ lag(exp) + 1,
                         !is.na(lag(exp, 2)) & lag(player_id, 2) == player_id ~ lag(exp, 2) + 1,
                         !is.na(lag(exp, 3)) & lag(player_id, 3) == player_id ~ lag(exp, 3) + 1,
                         !is.na(lead(exp)) & lead(player_id) == player_id ~ lead(exp),
                         !is.na(lead(exp, 2)) & lead(player_id, 2) == player_id ~ lead(exp, 2),
                         !is.na(lead(exp, 3)) & lead(player_id, 3) == player_id ~ lead(exp, 3),
                         TRUE ~ 1),
         yrs_off = case_when(!is.na(yrs_off) ~ yrs_off,
                             lag(player_id) != player_id ~ 0,
                             lag(exp) == 1 ~ 0,
                             !is.na(lag(yrs_off)) ~ year - lag(year) - 1,
                             !is.na(lag(yrs_off, 2)) ~ year - lag(year, 2) - 1,
                             !is.na(lag(yrs_off, 3)) ~ year - lag(year, 3) - 1,
                             TRUE ~ yrs_off),
         old_first = ifelse(age >= 28 & cum_allnba == 1 & allnba_1yr == 1 & allnba_share_1yr < 0.6, 1, 0),
         young_star = ifelse((allnba_share_1yr >= 0.3 | cum_allnba_share >= 0.8) & age < 25, 1, 0),
         notrotation_1yr = ifelse(min_pg_1yr < 15, 1, 0),
         notstarter_1yr = ifelse(min_pg_1yr < 28, 1, 0),
         high_pick = ifelse(pick <= 10 & roy_share_1yr >= 0.5 & exp <= 2, 1, 0),
         star = ifelse(allnba_share_avg_3yr > 0.4 & age <= 30, 1, 0),
         rookie = ifelse(exp == 1, 1, 0),
         injured = ifelse(g_pct_1yr < 0.65 & pts_pg_1yr > 15 & cum_allnba_share > 1 & age < 35, 1, 0),
         years_from_prime = ifelse(age > 31, age - 31, pmax(0, 25 - age)),
         young_pick = ifelse(exp <= 3, 66 - pick, 0)) %>%
  select(player_id, year, od_team, allnba, pick, age, exp, yrs_off, g_pct_1yr, g_pct_2yr, min_pg_1yr, min_pg_2yr, pts_pg_1yr, pts_pg_2yr, ortg_1yr,
         drtg_1yr,per_1yr, ws_48_adj_1yr, ws_48_adj_2yr, bpm_1yr, vorp_1yr, p_ws_1yr, p_ws_2yr, mvp_share_1yr, mvp_share_2yr, roy_share_1yr, roy_share_2yr,
         dpoy_share_1yr, dpoy_share_2yr, mip_share_1yr, mip_share_2yr, allnba_1yr, allnba1_1yr, allnba2_1yr, allnba3_1yr, allnba_share_1yr,
         allnba_share_2yr, allnba_share_3yr, alldef_share_1yr, alldef_share_2yr, allnba_val_1yr, allnba_val_2yr, allnba_val_3yr, cum_pts_pg, cum_ws,
         cum_vorp, cum_p_pts_pg, cum_p_ws, cum_p_vorp, cum_mvp_share, cum_dpoy_share, cum_allnba, cum_allnba_share, cum_alldef, cum_alldef_share,
         cum_allnba_val, ws_48_adj_avg_3yr, allnba_share_avg_3yr, mvp_share_avg_3yr, allnba_val_avg_3yr, old_first:young_pick, out_for_season)

# save file
write.csv(features_plus, "data/player_features.csv", row.names = FALSE)


# summarize data by opening night team
grouped_by_team <- features_plus %>%
  filter(out_for_season == 0) %>%
  select(-out_for_season) %>%
  group_by(od_team, year) %>%
  summarize(allnba_team_pred_sum = sum(allnba_share_1yr),
            allnba_team_pred_max = max(allnba_share_1yr),
            allnba_team_pred_count = sum(allnba_share_1yr >= 0.1),
            allnba_team_pred_count2 = sum(allnba_share_1yr >= 0.05)) %>%
  filter(od_team != "XXX")

# compare opening night roster with roster that played the previous season
# create variables based on player predictions
data2 <- read.csv("data/all_player_stats_1980-2024.csv")
team_stats_from_players <- data2 %>%
  mutate(across(c(allnba_share, mp), ~ replace(., is.na(.), 0))) %>%
  filter(team != "TOT",
         mp != 0) %>%
  group_by(pl_yr_id) %>%
  mutate(allnba_share = (mp / sum(mp)) * allnba_share) %>%
  select(pl_yr_id, player, year, team, allnba_share) %>%
  ungroup() %>%
  group_by(team, year) %>%
  summarize(allnba_team_tot = sum(allnba_share)) %>%
  ungroup() %>%
  arrange(team, year) %>%
  prior_years_stats_simple("allnba_team_tot", 1, "team") %>%
  merge(grouped_by_team, by.x = c("team", "year"), by.y = c("od_team", "year"), all = TRUE) %>%
  select(-allnba_team_tot) %>%
  mutate(allnba_team_diff = allnba_team_pred_sum - allnba_team_tot_1yr)

# save file
write.csv(team_stats_from_players, "data/team_stats_from_players.csv", row.names = FALSE)



  

