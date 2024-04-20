library(tidyverse)
library(XML)
library(stringr)
source("functions.R")


seasons <- 1980:2024

scrape1 <- scrape_yearly_tables("https://www.basketball-reference.com/leagues/NBA_", seasons, 1, "_totals.html")
player_total_rs <- scrape1 %>%
  filter(Rk != "Rk") %>%
  select(-Rk) %>%
  mutate(across(c(Age, G:year), as.numeric),
         Player = gsub("\\*", "", Player))
names(player_total_rs) <- colnames_player_total


scrape2 <- scrape_yearly_tables("https://www.basketball-reference.com/leagues/NBA_", seasons, 1, "_per_poss.html")
player_per100_rs <- scrape2 %>%
  filter(Rk != "Rk") %>%
  select(-c(Rk, Var.30)) %>%
  mutate(across(c(Age, G:year), as.numeric),
         Player = gsub("\\*", "", Player))
names(player_per100_rs) <- colnames_player_per100
player_per100_rs <- player_per100_rs %>%
  select(-c(fgp, fgp3, fgp2, ftp))


scrape3 <- scrape_yearly_tables("https://www.basketball-reference.com/leagues/NBA_", seasons, 1, "_advanced.html")
player_adv_rs <- scrape3 %>%
  filter(Rk != "Rk") %>%
  select(-c(Rk, X..1, X.)) %>%
  mutate(across(c(Age, G:year), as.numeric),
         Player = gsub("\\*", "", Player))
names(player_adv_rs) <- colnames_player_adv


scrape4 <- scrape_yearly_tables("https://www.basketball-reference.com/leagues/NBA_", seasons, 1, "_adj_shooting.html")
player_adjshoot_rs <- scrape4 %>%
  filter(Rk != "Rk",
         Rk != "") %>%
  select(-c(Rk, X.:X..1, X..2)) %>%
  mutate(across(c(Age, G:year), as.numeric),
         Player = gsub("\\*", "", Player))
names(player_adjshoot_rs) <- colnames_player_adjshoot


seasons_shoot <- seasons[seasons > 1996]
scrape5 <- scrape_yearly_tables("https://www.basketball-reference.com/leagues/NBA_", seasons_shoot, 1, "_shooting.html")
player_shoot_rs <- scrape5 %>%
  filter(Rk != "Rk") %>%
  select(-c(Rk, FG., X., X..1, X..2, X..3, X..5, X..6, X2P, X3P)) %>%
  mutate(across(c(Age, G:year), as.numeric),
         Player = gsub("\\*", "", Player))
names(player_shoot_rs) <- colnames_player_shoot


scrape6 <- scrape_yearly_tables("https://www.basketball-reference.com/leagues/NBA_", seasons_shoot, 1, "_play-by-play.html")
player_pbp_rs <- scrape6 %>%
  filter(Rk != "Rk") %>%
  select(-c(Rk)) %>%
  mutate(across(c(PG.:C.), ~ gsub("\\%", "", .)),
         across(c(Age, G:year), as.numeric),
         Player = gsub("\\*", "", Player))
names(player_pbp_rs) <- colnames_player_pbp
player_pbp_rs <- player_pbp_rs %>%
  mutate(across(c(pg_pct:c_pct), ~ ifelse(is.na(.), 0, .)))


all_player_stats_rs <- player_total_rs %>%
  merge(player_per100_rs, by = c("year", "team", "player", "age", "pos", "g", "gs", "mp"), all = TRUE) %>%
  merge(player_adv_rs, by = c("year", "team", "player", "age", "pos", "g", "mp"), all = TRUE) %>%
  merge(player_adjshoot_rs, by = c("year", "team", "player", "age", "g", "mp"), all = TRUE) %>%
  select(-pos.y) %>%
  rename(pos = pos.x) %>%
  merge(player_shoot_rs, by = c("year", "team", "player", "age", "pos", "g", "mp"), all = TRUE) %>%
  merge(player_pbp_rs, by = c("year", "team", "player", "age", "pos", "g", "mp"), all = TRUE)


seasons = 1980:2023

scrape7 <- scrape_yearly_tables("https://www.basketball-reference.com/playoffs/NBA_", seasons, 1, "_totals.html")
player_total_po <- scrape7 %>%
  filter(Rk != "Rk") %>%
  select(-Rk) %>%
  mutate(across(c(Age, G:year), as.numeric),
         Player = gsub("\\*", "", Player))
names(player_total_po) <- colnames_player_total


scrape8 <- scrape_yearly_tables("https://www.basketball-reference.com/playoffs/NBA_", seasons, 1, "_per_poss.html")
player_per100_po <- scrape8 %>%
  filter(Rk != "Rk") %>%
  select(-c(Rk, Var.30)) %>%
  mutate(across(c(Age, G:year), as.numeric),
         Player = gsub("\\*", "", Player))
names(player_per100_po) <- colnames_player_per100
player_per100_po <- player_per100_po %>%
  select(-c(fgp, fgp3, fgp2, ftp))


scrape9 <- scrape_yearly_tables("https://www.basketball-reference.com/playoffs/NBA_", seasons, 1, "_advanced.html")
player_adv_po <- scrape9 %>%
  filter(Rk != "Rk") %>%
  select(-c(Rk, X..1, X.)) %>%
  mutate(across(c(Age, G:year), as.numeric),
         Player = gsub("\\*", "", Player))
names(player_adv_po) <- colnames_player_adv


seasons_shoot <- seasons[seasons > 1996]
scrape10 <- scrape_yearly_tables("https://www.basketball-reference.com/playoffs/NBA_", seasons_shoot, 1, "_shooting.html")
player_shoot_po <- scrape10 %>%
  filter(Rk != "Rk") %>%
  select(-c(Rk, FG., X., X..1, X..2, X..3, X..5, X..6, X2P, X3P)) %>%
  mutate(across(c(Age, G:year), as.numeric),
         Player = gsub("\\*", "", Player))
names(player_shoot_po) <- colnames_player_shoot


scrape11 <- scrape_yearly_tables("https://www.basketball-reference.com/playoffs/NBA_", seasons_shoot, 1, "_play-by-play.html")
player_pbp_po <- scrape11 %>%
  filter(Rk != "Rk") %>%
  select(-c(Rk)) %>%
  mutate(across(c(PG.:C.), ~ gsub("\\%", "", .)),
         across(c(Age, G:year), as.numeric),
         Player = gsub("\\*", "", Player))
names(player_pbp_po) <- colnames_player_pbp
player_pbp_po <- player_pbp_po %>%
  mutate(across(c(pg_pct:c_pct), ~ ifelse(is.na(.), 0, .)))


all_player_stats_po <- player_total_po %>%
  merge(player_per100_po, by = c("year", "team", "player", "age", "pos", "g", "gs", "mp"), all = TRUE) %>%
  merge(player_adv_po, by = c("year", "team", "player", "age", "pos", "g", "mp"), all = TRUE) %>%
  merge(player_shoot_po, by = c("year", "team", "player", "age", "pos", "g", "mp"), all = TRUE) %>%
  merge(player_pbp_po, by = c("year", "team", "player", "age", "pos", "g", "mp"), all = TRUE) %>%
  select()

keep_name_cols <- c("year", "team", "player", "age", "pos")
cols_to_rename <- setdiff(names(all_player_stats_po), keep_name_cols)
new_colnames <- paste0("p_", cols_to_rename)
names(all_player_stats_po) <- c(keep_name_cols, new_colnames)

all_player_stats <- all_player_stats_rs %>%
  merge(all_player_stats_po, by = c("year", "team", "player", "age", "pos"), all = TRUE)


mvp = list()
roy = list()
dpoy = list()
smoy = list()
mip = list()
clutch = list()
allnba = list()
alldef = list()
for(y in seasons) {
  url <- paste0("https://www.basketball-reference.com/awards/awards_", y, ".html")
  if(y >= 2023) {
    table_id <- 1:8
  } else if(y >= 1986) {
    table_id <- 1:7
  } else if(y >= 1984) {
    table_id <- 1:6
  } else if(y == 1983) {
    table_id <- 1:5
  } else{
    table_id <- 1:4
  }
  tables <- scrape_bballref(url, table_id)
  awards_list <- tables
  for(i in 1:length(awards_list)){
    awards_list[[i]] <- tables[[i]] %>%
      mutate(year = y)
    if("Rank" %in% colnames(tables[[i]])) {
      awards_list[[i]] <- awards_list[[i]] %>%
        mutate(Rank = gsub("T", "", Rank),
               Rank = as.numeric(Rank),
               win = ifelse(Rank == 1, 1, 0))
    }
  }
  mvp <- c(mvp, list(awards_list[[1]]))
  roy <- c(roy, list(awards_list[[2]]))
  if(y >= 2023) {
    dpoy <- c(dpoy, list(awards_list[[3]]))
    smoy <- c(smoy, list(awards_list[[4]]))
    mip <- c(mip, list(awards_list[[5]]))
    clutch <- c(clutch, list(awards_list[[6]]))
    allnba <- c(allnba, list(awards_list[[7]]))
    alldef <- c(alldef, list(awards_list[[8]]))
  } else if(y >= 1986) {
    dpoy <- c(dpoy, list(awards_list[[3]]))
    smoy <- c(smoy, list(awards_list[[4]]))
    mip <- c(mip, list(awards_list[[5]]))
    allnba <- c(allnba, list(awards_list[[6]]))
    alldef <- c(alldef, list(awards_list[[7]]))
  } else if(y >= 1984) {
    dpoy <- c(dpoy, list(awards_list[[3]]))
    smoy <- c(smoy, list(awards_list[[4]]))
    allnba <- c(allnba, list(awards_list[[5]]))
    alldef <- c(alldef, list(awards_list[[6]]))
  } else if(y == 1983) {
    dpoy <- c(dpoy, list(awards_list[[3]]))
    allnba <- c(allnba, list(awards_list[[4]]))
    alldef <- c(alldef, list(awards_list[[5]]))
  } else{
    allnba <- c(allnba, list(awards_list[[3]]))
    alldef <- c(alldef, list(awards_list[[4]]))
  }
  print(y)
  Sys.sleep(10)
}

mvp_df <- bind_rows(mvp) %>%
  select(year, Player:Tm, Rank, win, Share) %>%
  rename(player = Player,
         age = Age,
         team = Tm,
         mvp_rank = Rank,
         mvp = win,
         mvp_share = Share)
roy_df <- bind_rows(roy) %>%
  select(year, Player:Tm, Rank, win, Share) %>%
  rename(player = Player,
         age = Age,
         team = Tm,
         roy_rank = Rank,
         roy = win,
         roy_share = Share)
dpoy_df <- bind_rows(dpoy) %>%
  select(year, Player:Tm, Rank, win, Share) %>%
  rename(player = Player,
         age = Age,
         team = Tm,
         dpoy_rank = Rank,
         dpoy = win,
         dpoy_share = Share)
smoy_df <- bind_rows(smoy) %>%
  select(year, Player:Tm, Rank, win, Share) %>%
  rename(player = Player,
         age = Age,
         team = Tm,
         smoy_rank = Rank,
         smoy = win,
         smoy_share = Share)
mip_df <- bind_rows(mip) %>%
  select(year, Player:Tm, Rank, win, Share) %>%
  rename(player = Player,
         age = Age,
         team = Tm,
         mip_rank = Rank,
         mip = win,
         mip_share = Share)
clutch_df <- bind_rows(clutch) %>%
  select(year, Player:Tm, Rank, win, Share) %>%
  rename(player = Player,
         age = Age,
         team = Tm,
         clutch_rank = Rank,
         clutch = win,
         clutch_share = Share)
allnba_df <- bind_rows(allnba) %>%
  filter(Player != "") %>%
  rename(player = Player,
         age = Age,
         team = Tm,
         allnba_tm = `# Tm`,
         allnba_share = Share) %>%
  mutate(allnba_tm = case_when(allnba_tm == "1st" | allnba_tm == "1T" ~ 1,
                               allnba_tm == "2nd" | allnba_tm == "2T" ~ 2,
                               allnba_tm == "3rd" | allnba_tm == "3T" ~ 3,
                               allnba_tm == "ORV" ~ 0,
                               TRUE ~ NA),
         allnba = ifelse(allnba_tm > 0, 1, 0),
         allnba1 = ifelse(allnba_tm == 1, 1, 0),
         allnba2 = ifelse(allnba_tm == 2, 1, 0),
         allnba3 = ifelse(allnba_tm == 3, 1, 0),
         player = gsub("\\*", "", player)) %>%
  select(year, player, age, team, allnba, allnba_tm, allnba1:allnba3, allnba_share)
alldef_df <- bind_rows(alldef) %>%
  filter(Player != "") %>%
  rename(player = Player,
         age = Age,
         team = Tm,
         alldef_tm = `# Tm`,
         alldef_share = Share) %>%
  mutate(alldef_tm = case_when(alldef_tm == "1st" | alldef_tm == "1T" ~ 1,
                               alldef_tm == "2nd" | alldef_tm == "2T" ~ 2,
                               alldef_tm == "ORV" ~ 0,
                               TRUE ~ NA),
         alldef = ifelse(alldef_tm > 0, 1, 0),
         alldef1 = ifelse(alldef_tm == 1, 1, 0),
         alldef2 = ifelse(alldef_tm == 2, 1, 0),
         player = gsub("\\*", "", player)) %>%
  select(year, player, age, team, alldef, alldef_tm, alldef1:alldef2, alldef_share)

awards_df <- mvp_df %>%
  merge(roy_df, by = c("year", "player", "age", "team"), all = TRUE) %>%
  merge(dpoy_df, by = c("year", "player", "age", "team"), all = TRUE) %>%
  merge(smoy_df, by = c("year", "player", "age", "team"), all = TRUE) %>%
  merge(mip_df, by = c("year", "player", "age", "team"), all = TRUE) %>%
  merge(clutch_df, by = c("year", "player", "age", "team"), all = TRUE) %>%
  merge(allnba_df, by = c("year", "player", "age", "team"), all = TRUE) %>%
  merge(alldef_df, by = c("year", "player", "age", "team"), all = TRUE)
  

all_player_stats <- all_player_stats %>%
  merge(awards_df, by = c("year", "player", "age"), all = TRUE)



