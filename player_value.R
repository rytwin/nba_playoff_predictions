library(tidyverse)
source("functions.R")

# read in data
data <- read.csv("data/all_player_stats_1980-2024_single_rows.csv")
od <- read.csv("data/opening_day_rosters.csv") %>%
  mutate(player_id = paste0(gsub("\\s", "", tolower(player)), "_", gsub("\\s", "", tolower(college)), "_", draft_year, "_", pick),
         pl_yr_id = paste0(gsub("\\s", "", tolower(player)), "_", gsub("\\s", "", tolower(college)), "_", draft_year, "_", pick, "_", year)) %>%
  select(pl_yr_id, od_team, out_for_season)


comb <- data %>%
  merge(od, by = "pl_yr_id")


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
  mutate(across(c(g_1yr:allnba_val_3yr), ~ replace(., is.na(.), 0)),
         pick = ifelse(is.na(pick) | pick > 65, 65, pick)) %>%
  group_by(player_id) %>%
  mutate(cum_pts_pg = cumsum(pts_1yr) / cumsum(g_1yr),
         cum_ws = cumsum(ws_1yr),
         cum_vorp = cumsum(vorp_1yr),
         cum_p_g = cumsum(p_g_1yr),
         cum_p_pts_pg = cumsum(p_pts_1yr) / cum_p_g,
         cum_p_ws = cumsum(p_ws_1yr),
         cum_p_vorp = cumsum(p_vorp_1yr),
         cum_mvp_share = cumsum(mvp_share_1yr),
         cum_dpoy_share = cumsum(dpoy_share_1yr),
         cum_allnba = cumsum(allnba_1yr),
         cum_allnba_share = cumsum(allnba_share_1yr),
         cum_alldef = cumsum(alldef_1yr),
         cum_alldef_share = cumsum(alldef_share_1yr),
         cum_allnba_val = cumsum(allnba_val_1yr),
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
  select(pl_yr_id, year, pick, age, exp, g_1yr:allnba_val_avg_3yr)

# save file
write.csv(features_df, "data/player_features.csv", row.names = FALSE)

