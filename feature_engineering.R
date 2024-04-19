library(tidyverse)
source("functions.R")

data <- read.csv("data/all_team_stats_1980-2024.csv")
breakdown <- read.csv("data/team_breakdown.csv")

conf_win_pct <- data %>%
  group_by(year, conf) %>%
  summarize(w = sum(w),
            l = sum(l)) %>%
  mutate(conf_win_pct = w / (w + l)) %>%
  select(-c(w, l)) %>%
  pivot_wider(names_from = "conf", values_from = "conf_win_pct") %>%
  rename(e_win_pct = e,
         w_win_pct = w)

conf_win_pct_playoffs <- data %>%
  group_by(year, conf, playoffs) %>%
  summarize(min_conf_p_win_pct = min(win_pct),
            w = sum(w),
            l = sum(l)) %>%
  mutate(conf_p_win_pct = w / (w + l)) %>%
  filter(playoffs == 1) %>%
  select(-c(playoffs, w, l)) %>%
  pivot_wider(names_from = "conf", values_from = c("min_conf_p_win_pct", "conf_p_win_pct"))

df <- data %>%
  merge(breakdown, by = "year", all.x = TRUE) %>%
  mutate(proj_win_pct = pw / (pw + pl),
         p_win_pct = p_w / (p_w + p_l),
         p_proj_win_pct = p_pw / (p_pw + p_pl),
         coach_preseason_g = coach_career_g_start - coach_season_g_start,
         coach_preseason_g_tm = coach_team_g_start - coach_season_g_start,
         coach_preseason_w = coach_career_w_start - coach_season_w_start,
         coach_preseason_w_tm = coach_team_w_start - coach_season_w_start,
         coach_preseason_win_pct = coach_preseason_w / coach_preseason_g,
         coach_preseason_win_pct_tm = coach_preseason_w_tm / coach_preseason_g_tm,
         coach_preseason_win_pct_adj = (coach_preseason_w + 5) / (coach_preseason_g + 10),
         coach_preseason_win_pct_tm_adj = (coach_preseason_w_tm + 5) / (coach_preseason_g_tm + 10)) %>%
  merge(conf_win_pct, by = "year", all.x = TRUE) %>%
  merge(conf_win_pct_playoffs, by = "year", all.x = TRUE) %>%
  arrange(team, year) %>%
  select(year, team, everything())

col_names_prior <- colnames(df)[!colnames(df) %in% c("team", "year")]

for(c in col_names_prior){
  df <- prior_years_stats_simple(df, c, c(1:3), "team")
}

df <- df %>%
  mutate(coach_exp_diff = coach_exp_start - coach_exp_start_1yr,
         coach_win_pct_diff = coach_preseason_win_pct_adj - coach_preseason_win_pct_adj_1yr,
         new_coach_from_start = ifelse(start_coach == start_coach_1yr, 0, 1),
         new_coach_from_end = ifelse(start_coach == end_coach_1yr, 0, 1),
         num_conf_teams = ifelse(conf == "e", num_east_teams, num_west_teams),
         conf_win_pct_1yr = ifelse(conf == "e", e_win_pct_1yr, w_win_pct_1yr),
         conf_p_win_pct_1yr = ifelse(conf == "e", conf_p_win_pct_e_1yr, conf_p_win_pct_w_1yr),
         min_conf_p_win_pct_1yr = ifelse(conf == "e", min_conf_p_win_pct_e_1yr, min_conf_p_win_pct_w_1yr),
         playoffs_last2yr = ifelse(playoffs_1yr == 1 & playoffs_2yr == 1, 1, 0),
         playoffs_last3yr = ifelse(playoffs_1yr == 1 & playoffs_2yr == 1 & playoffs_3yr == 1, 1, 0),
         second_rd_last2yr = ifelse(second_rd_1yr == 1 & second_rd_2yr == 1, 1, 0),
         second_rd_last3yr = ifelse(second_rd_1yr == 1 & second_rd_2yr == 1 & second_rd_3yr == 1, 1, 0),
         win_pct_last2yr = (w_1yr + w_2yr) / (g_1yr + g_2yr),
         win_pct_last3yr = (w_1yr + w_2yr + w_3yr) / (g_1yr + g_2yr + g_3yr),
         proj_win_pct_last2yr = (pw_1yr + pw_2yr) / (g_1yr + g_2yr),
         proj_win_pct_last3yr = (pw_1yr + pw_2yr + pw_3yr) / (g_1yr + g_2yr + g_3yr),
         win_pct_last2yr_over60 = ifelse(win_pct_1yr >= 0.6 & win_pct_2yr >= 0.6, 1, 0),
         win_pct_last3yr_over60 = ifelse(win_pct_1yr >= 0.6 & win_pct_2yr >= 0.6 & win_pct_3yr >= 0.6, 1, 0),) %>%
  select(team, year, playoffs, champ_odds, o_u, play_in, playoffs_1yr, win_pct_1yr, new_coach_from_start, new_coach_from_end,
         coach_exp_start, coach_preseason_g, coach_preseason_win_pct_adj, coach_team_exp_start, coach_preseason_g_tm,
         coach_preseason_win_pct_tm_adj, coach_exp_diff, coach_win_pct_diff, rel_ortg_1yr, rel_drtg_1yr, top_ws_1yr, champion_1yr,
         finals_1yr, conf_finals_1yr, second_rd_1yr, proj_win_pct_1yr, mov_1yr, sos_1yr, srs_1yr,
         off_rtg_1yr, def_rtg_1yr, net_rtg_1yr, p_win_pct_1yr, p_proj_win_pct_1yr, p_srs_1yr, p_off_rtg_1yr,
         p_def_rtg_1yr, p_net_rtg_1yr, ortg2_1yr, drtg2_1yr, net_rtg2_1yr, mov_adj_1yr, ortg2_adj_1yr,
         drtg2_adj_1yr, net_rtg2_adj_1yr, conf_win_pct_1yr, conf_p_win_pct_1yr, min_conf_p_win_pct_1yr,
         num_conf_teams, playoffs_last2yr, playoffs_last3yr, second_rd_last2yr, second_rd_last3yr, win_pct_last2yr,
         win_pct_last3yr, proj_win_pct_last2yr, proj_win_pct_last3yr, win_pct_last2yr_over60, win_pct_last3yr_over60)


