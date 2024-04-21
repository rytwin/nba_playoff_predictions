library(tidyverse)
source("functions.R")

data <- read.csv("data/all_player_stats_1980-2024_single_rows.csv")

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
         g_pct = g / team_gm)

cols_prev <- c("g", "mp", "pts", "trb", "ast", "stl", "blk", "tov", "g_pct", "min_pg", "pts_pg", "trb_pg", "ast_pg",
               "stl_pg", "blk_pg", "tov_pg", "ortg", "drtg", "per", "ws", "ws_48", "bpm", "vorp",
               "p_g", "p_mp", "p_pts", "p_pts_pg", "p_ws", "p_ws_48", "p_bpm", "p_vorp", "mvp", "mvp_share",
               "roy", "roy_share", "dpoy", "dpoy_share", "smoy", "smoy_share", "mip", "mip_share", "allnba", "allnba1",
               "allnba2", "allnba3", "allnba_share", "alldef", "alldef1", "alldef2", "alldef_share", "allnba_val")
for(c in cols_prev){
  df <- prior_years_stats_simple(df, c, c(1:3), "player_id")
}
df <- df %>%
  group_by(player_id) %>%
  mutate(cum_g = cumsum(g_1yr))
  
  
  select(pl_yr_id, pick:yrs_off, g, mp, min_pg:tov_pg, efg, ortg, drtg, per, tsp, ft_rate, usg, ows, dws, ws, ws_48,
         obpm, dpbm, bpm, vorp, p_g, p_mp, p_pts_pg, p_usg, p_ows, p_dws, p_ws, p_ws_48, p_obpm, p_dbpm, p_bpm,
         p_vorp, mvp, mvp_share, roy, roy_share, dpoy, dpoy_share, smoy, smoy_share, mip, mip_share, allnba:alldef_share)


