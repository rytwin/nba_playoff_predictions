library(tidyverse)
library(XML)
library(stringr)
source("functions.R")


# select seasons
seasons <- 1980:2024

# gets total player statistics for all selected seasons (regular season)
scrape1 <- scrape_yearly_tables("https://www.basketball-reference.com/leagues/NBA_", seasons, 1, "_totals.html")
player_total_rs <- scrape1 %>%
  filter(Rk != "Rk") %>%
  select(-Rk) %>%
  mutate(across(c(Age, G:year), as.numeric),
         Player = gsub("\\*", "", Player),
         tm_order = row_number())
names(player_total_rs) <- c(colnames_player_total, "tm_order")


# gets per possession player statistics for all selected seasons (regular season)
scrape2 <- scrape_yearly_tables("https://www.basketball-reference.com/leagues/NBA_", seasons, 1, "_per_poss.html")
player_per100_rs <- scrape2 %>%
  filter(Rk != "Rk") %>%
  select(-c(Rk, Var.30)) %>%
  mutate(across(c(Age, G:year), as.numeric),
         Player = gsub("\\*", "", Player))
names(player_per100_rs) <- colnames_player_per100
player_per100_rs <- player_per100_rs %>%
  select(-c(fgp, fgp3, fgp2, ftp))


# gets advanced player statistics for all selected seasons (regular season)
scrape3 <- scrape_yearly_tables("https://www.basketball-reference.com/leagues/NBA_", seasons, 1, "_advanced.html")
player_adv_rs <- scrape3 %>%
  filter(Rk != "Rk") %>%
  select(-c(Rk, X..1, X.)) %>%
  mutate(across(c(Age, G:year), as.numeric),
         Player = gsub("\\*", "", Player))
names(player_adv_rs) <- colnames_player_adv


# gets adjusted shooting player statistics for all selected seasons (regular season)
scrape4 <- scrape_yearly_tables("https://www.basketball-reference.com/leagues/NBA_", seasons, 1, "_adj_shooting.html")
player_adjshoot_rs <- scrape4 %>%
  filter(Rk != "Rk",
         Rk != "") %>%
  select(-c(Rk, X.:X..1, X..2)) %>%
  mutate(across(c(Age, G:year), as.numeric),
         Player = gsub("\\*", "", Player))
names(player_adjshoot_rs) <- colnames_player_adjshoot


# gets player shooting statistics for all selected seasons (regular season)
seasons_shoot <- seasons[seasons > 1996]
scrape5 <- scrape_yearly_tables("https://www.basketball-reference.com/leagues/NBA_", seasons_shoot, 1, "_shooting.html")
player_shoot_rs <- scrape5 %>%
  filter(Rk != "Rk") %>%
  select(-c(Rk, FG., X., X..1, X..2, X..3, X..5, X..6, X2P, X3P)) %>%
  mutate(across(c(Age, G:year), as.numeric),
         Player = gsub("\\*", "", Player))
names(player_shoot_rs) <- colnames_player_shoot


# gets player play-by-play statistics for all selected seasons (regular season)
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


# combines all regular season statistics into one dataframe
all_player_stats_rs <- player_total_rs %>%
  merge(player_per100_rs, by = c("year", "team", "player", "age", "pos", "g", "gs", "mp"), all = TRUE) %>%
  merge(player_adv_rs, by = c("year", "team", "player", "age", "pos", "g", "mp"), all = TRUE) %>%
  merge(player_adjshoot_rs, by = c("year", "team", "player", "age", "g", "mp"), all = TRUE) %>%
  select(-pos.y) %>%
  rename(pos = pos.x) %>%
  merge(player_shoot_rs, by = c("year", "team", "player", "age", "pos", "g", "mp"), all = TRUE) %>%
  merge(player_pbp_rs, by = c("year", "team", "player", "age", "pos", "g", "mp"), all = TRUE)


seasons = 1980:2023

# gets player total statistics for all selected seasons (playoffs)
scrape7 <- scrape_yearly_tables("https://www.basketball-reference.com/playoffs/NBA_", seasons, 1, "_totals.html")
player_total_po <- scrape7 %>%
  filter(Rk != "Rk") %>%
  select(-Rk) %>%
  mutate(across(c(Age, G:year), as.numeric),
         Player = gsub("\\*", "", Player))
names(player_total_po) <- colnames_player_total


# gets player per possession statistics for all selected seasons (playoffs)
scrape8 <- scrape_yearly_tables("https://www.basketball-reference.com/playoffs/NBA_", seasons, 1, "_per_poss.html")
player_per100_po <- scrape8 %>%
  filter(Rk != "Rk") %>%
  select(-c(Rk, Var.30)) %>%
  mutate(across(c(Age, G:year), as.numeric),
         Player = gsub("\\*", "", Player))
names(player_per100_po) <- colnames_player_per100
player_per100_po <- player_per100_po %>%
  select(-c(fgp, fgp3, fgp2, ftp))


# gets player advanced statistics for all selected seasons (playoffs)
scrape9 <- scrape_yearly_tables("https://www.basketball-reference.com/playoffs/NBA_", seasons, 1, "_advanced.html")
player_adv_po <- scrape9 %>%
  filter(Rk != "Rk") %>%
  select(-c(Rk, X..1, X.)) %>%
  mutate(across(c(Age, G:year), as.numeric),
         Player = gsub("\\*", "", Player))
names(player_adv_po) <- colnames_player_adv


# gets player shooting statistics for all selected seasons (playoffs)
seasons_shoot <- seasons[seasons > 1996]
scrape10 <- scrape_yearly_tables("https://www.basketball-reference.com/playoffs/NBA_", seasons_shoot, 1, "_shooting.html")
player_shoot_po <- scrape10 %>%
  filter(Rk != "Rk") %>%
  select(-c(Rk, FG., X., X..1, X..2, X..3, X..5, X..6, X2P, X3P)) %>%
  mutate(across(c(Age, G:year), as.numeric),
         Player = gsub("\\*", "", Player))
names(player_shoot_po) <- colnames_player_shoot


# gets player play-by-play statistics for all selected seasons (playoffs)
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


# combines all playoff statistics into one dataframe
all_player_stats_po <- player_total_po %>%
  merge(player_per100_po, by = c("year", "team", "player", "age", "pos", "g", "gs", "mp"), all = TRUE) %>%
  merge(player_adv_po, by = c("year", "team", "player", "age", "pos", "g", "mp"), all = TRUE) %>%
  merge(player_shoot_po, by = c("year", "team", "player", "age", "pos", "g", "mp"), all = TRUE) %>%
  merge(player_pbp_po, by = c("year", "team", "player", "age", "pos", "g", "mp"), all = TRUE) %>%
  select()

# renames all playoff statistic columns with prefix "p_"
keep_name_cols <- c("year", "team", "player", "age", "pos")
cols_to_rename <- setdiff(names(all_player_stats_po), keep_name_cols)
new_colnames <- paste0("p_", cols_to_rename)
names(all_player_stats_po) <- c(keep_name_cols, new_colnames)

# merges playoff and regular season statistics
all_player_stats <- all_player_stats_rs %>%
  merge(all_player_stats_po, by = c("year", "team", "player", "age", "pos"), all = TRUE)


# gets player awards data
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

# merges all awards data into one dataframe
awards_df <- mvp_df %>%
  merge(roy_df, by = c("year", "player", "age", "team"), all = TRUE) %>%
  merge(dpoy_df, by = c("year", "player", "age", "team"), all = TRUE) %>%
  merge(smoy_df, by = c("year", "player", "age", "team"), all = TRUE) %>%
  merge(mip_df, by = c("year", "player", "age", "team"), all = TRUE) %>%
  merge(clutch_df, by = c("year", "player", "age", "team"), all = TRUE) %>%
  merge(allnba_df, by = c("year", "player", "age", "team"), all = TRUE) %>%
  merge(alldef_df, by = c("year", "player", "age", "team"), all = TRUE) %>%
  mutate(across(c(year, age, mvp_rank:alldef_share), as.numeric))
  

# merges player statistics with player awards data
all_player_stats <- all_player_stats %>%
  merge(awards_df, by = c("year", "player", "age"), all = TRUE) %>%
  mutate(tm_order = ifelse(is.na(tm_order), 0, tm_order)) %>%
  rename(team = team.x) %>%
  select(-team.y) %>%
  select(year:g, gs, mp, fg:alldef_share)

# save file
write.csv(all_player_stats, "data/all_player_stats_1980-2023.csv", row.names = FALSE)


# gets draft data (year, round, pick, college) for players
draft_seasons <- 1960:2023
draft <- scrape_yearly_tables("https://www.basketball-reference.com/draft/NBA_", draft_seasons, 1, ".html")
draft_df <- draft %>%
  filter(Rk != "",
         Rk != "Rk") %>%
  select(Player, year, Pk, Tm, College, Yrs) %>%
  rename(pick = Pk,
         player = Player,
         team = Tm,
         college = College,
         experience = Yrs) %>%
  mutate(across(c(year, pick, experience), as.numeric))

# save draft file
write.csv(draft_df, "data/draft_1960-2023.csv", row.names = FALSE)


# filter for players who have actually played in the NBA
draft_df <- draft_df %>%
  filter(experience != "") %>%
  rename(draft_year = year) %>%
  select(-team)

# for players who were drafted more than once, find the last year they were drafted
final_pick <- draft_df %>%
  group_by(player, college, experience) %>%
  summarize(draft_year = max(draft_year))

draft_single <- draft_df %>%
  merge(final_pick, by = c("player", "draft_year", "college", "experience"), all.y = TRUE) %>%
  filter(!(player == "Manute Bol" & college == ""))


# create a "TOT" (total) row for players who played on a team in the playoffs that they did not play for at all in the regular season,
# and they played on at least one other team in the regular season
split_rs_po <- all_player_stats %>%
  filter((player == "Scott Machado" & year == 2013) | (player == "Shaquille Harrison" & year == 2023)) %>%
  group_by(year, player, age, pos) %>%
  summarize(across(c(g:alldef_share), ~ sum(., na.rm = TRUE))) %>%
  mutate(team = "TOT",
         tm_order = 0) %>%
  select(year:age, team, everything())

# combine draft data with all player data. use the checks below to find cases where different players have the same name
final_output <- all_player_stats %>%
  rbind(split_rs_po) %>%
  rename_teams_abbrev() %>%
  merge(draft_single, by = "player", all.x = TRUE) %>%
  mutate(draft_year = ifelse(is.na(draft_year), 0, draft_year),
         tm_order = case_when(player == "Scott Machado" & year == 2013 & team == "GSW" ~ 16247,
                              player == "Shaquille Harrison" & year == 2023 & team == "LAL" ~ 22722,
                              TRUE ~ tm_order)) %>%
  filter(draft_year < year,
         !(player == "Bobby Jones" & year >= 2007 & draft_year < 2006),
         !(player == "Cedric Henderson" & year >= 1998 & draft_year < 1997),
         !(player == "Dee Brown" & year >= 2007 & draft_year < 2006),
         !(player == "Gerald Henderson" & year >= 2010 & draft_year < 2009),
         !(player == "Jeff Taylor" & year >= 2013 & draft_year < 2012),
         !(player == "Johnny Davis" & year >= 2023 & draft_year < 2022),
         !(player == "Ken Johnson" & year >= 2003 & draft_year < 2001),
         !(player == "Larry Johnson" & year >= 1992 & draft_year < 1991),
         !(player == "Luke Jackson" & year >= 2005 & draft_year < 2004),
         !(player == "Mark Davis" & year >= 1996 & draft_year < 1995),
         !(player == "Mike Dunleavy" & year >= 2003 & draft_year < 2002),
         !(player == "Patrick Ewing" & year >= 2011 & draft_year < 2008),
         !(player == "Sam Williams" & year >= 1982 & draft_year < 1981),
         !(player == "George Johnson" & age > 30 & draft_year >= 1978),
         !(player == "George Johnson" & age < 30 & draft_year < 1978),
         !(player == "George Johnson" & college == "Stephen F. Austin"),
         !(player == "Marcus Williams" & college == "UConn" & pos != "PG"),
         !(player == "Marcus Williams" & college == "Arizona" & pos == "PG"),
         !(player == "Charles Jones" & year >= 1999 & college == "Louisville"),
         !(player == "Charles Jones" & year < 1999 & college == "Albany State University" & team == "POR"),
         !(player == "Charles Jones" & year < 1999 & college == "Albany State University" & team == "PHO"),
         !(player == "Charles Jones" & year < 1999 & college == "Louisville" & team == "PHI"),
         !(player == "Charles Jones" & year < 1999 & college == "Louisville" & team == "TOT"),
         !(player == "Charles Jones" & year < 1999 & college == "Louisville" & team == "CHI"),
         !(player == "Charles Jones" & year < 1999 & college == "Louisville" & team == "DET"),
         !(player == "Charles Jones" & year < 1999 & college == "Louisville" & team == "HOU"),
         !(player == "Charles Jones" & year < 1985 & college == "Louisville"),
         !(player == "Charles Jones" & year > 1989 & college == "Louisville"),
         !(player == "Charles Jones" & year == 1989 & college == "Albany State University" & g == 43),
         !(player == "Charles Jones" & year == 1989 & college == "Louisville" & g == 53),
         !(player == "Charles Jones" & year < 1989 & team == "WAS" & college == "Louisville"),
         !(player == "Michael Smith" & year > 1995 & college == "BYU"),
         !(player == "Michael Smith" & year == 1995 & college == "BYU" & g == 82),
         !(player == "Michael Smith" & year == 1995 & college == "Providence" & g == 29),
         !(player == "Eddie Johnson" & college == "Auburn" & pos == "SF"),
         !(player == "Eddie Johnson" & college == "Auburn" & year > 1987),
         !(player == "Eddie Johnson" & college == "Illinois" & pos == "SG" & year != 1991),
         !(player == "Charles Smith" & college != "New Mexico" & year >= 1998)) %>%
  mutate(college = case_when(player == "Charles Jones" & year >= 1999 & year <= 2000 ~ "Long Island University",
                             player == "Charles Smith" & year < 1998 & pos == "PG" ~ "Georgetown",
                             player == "Walker Russell" & year > 1988 ~ "Jacksonville State University",
                             player == "Mark Jones" & year > 2000 ~ "Central Florida",
                             player == "Reggie Williams" & year > 2000 ~ "Virginia Military Institute",
                             player == "Chris Smith" & year > 2000 ~ "Louisville",
                             player == "Brandon Williams" & year < 2010 ~ "Davidson",
                             player == "Brandon Williams" & year >= 2022 ~ "Arizona",
                             player == "Chris Johnson" & year < 2013 ~ "LSU",
                             player == "Chris Johnson" & year > 2013 ~ "Dayton",
                             player == "Chris Johnson" & year == 2013 & age == 27 ~ "LSU",
                             player == "Chris Johnson" & year == 2013 & age == 22 ~ "Dayton",
                             player == "Chris Wright" & year == 2013 ~ "Georgetown",
                             player == "Chris Wright" & year < 2015 ~ "Dayton",
                             player == "Mike James" & year < 2015 ~ "Duquesne",
                             player == "Mike James" & year < 2024 ~ "Lamar",
                             player == "Tony Mitchell" & year == 2014 & team == "MIL" ~ "Alabama",
                             TRUE ~ college),
         draft_year = case_when(player == "Charles Jones" & year >= 1999 & year <= 2000 ~ NA,
                                player == "Charles Smith" & year < 1998 & pos == "PG" ~ NA,
                                player == "Walker Russell" & year > 1988 ~ NA,
                                player == "Mark Jones" & year > 2000 ~ NA,
                                player == "Reggie Williams" & year > 2000 ~ NA,
                                player == "Chris Smith" & year > 2000 ~ NA,
                                player == "Tony Mitchell" & year == 2014 & team == "MIL" ~ NA,
                                TRUE ~ draft_year),
         pick = case_when(player == "Charles Jones" & year >= 1999 & year <= 2000 ~ NA,
                          player == "Charles Smith" & year < 1998 & pos == "PG" ~ NA,
                          player == "Walker Russell" & year > 1988 ~ NA,
                          player == "Mark Jones" & year > 2000 ~ NA,
                          player == "Reggie Williams" & year > 2000 ~ NA,
                          player == "Chris Smith" & year > 2000 ~ NA,
                          player == "Tony Mitchell" & year == 2014 & team == "MIL" ~ NA,
                          TRUE ~ pick),
         experience = case_when(player == "Charles Jones" & year >= 1999 & year <= 2000 ~ 2,
                                player == "Charles Smith" & year < 1998 & pos == "PG" ~ 3,
                                player == "Walker Russell" & year > 1988 ~ 1,
                                player == "Mark Jones" & year > 2000 ~ 1,
                                player == "Reggie Williams" & year > 2000 ~ 7,
                                player == "Chris Smith" & year > 2000 ~ 1,
                                player == "Tony Mitchell" & year == 2014 & team == "MIL" ~ 1,
                                TRUE ~ experience),
         draft_year = ifelse(draft_year == 0, NA, draft_year)) %>%
  select(player, college, draft_year, pick, year:alldef_share) %>%
  arrange(player, college, draft_year, pick, year, tm_order)


# check for duplicate/missing/incorrect entries
check <- final_output %>%
  group_by(player, year, team) %>%
  summarize(count = n()) %>%
  filter(count > 1)
duplicates <- unique(check$player)
dup_check <- draft_single %>%
  filter(player %in% duplicates)

check2 <- final_output %>%
  count(player, college, draft_year, pick, year) %>%
  count(player, college, draft_year, pick) %>%
  merge(draft_single, by = c("player", "college", "draft_year", "pick"), all.x = TRUE) %>%
  filter(experience != n)

check3 <- final_output %>%
  filter(is.na(pick))

check4 <- final_output %>%
  filter(is.na(g))

# save file
write.csv(final_output, "data/all_player_stats_1980-2024.csv", row.names = FALSE)


# create another copy of player data with only one row for each player season
# if players played for multiple teams in a season, only the row with total statistics is kept,
# and the team column will show all teams that were played for in order (unless the player played with the same team in 2 different stints)
single_rows <- final_output %>%
  mutate(player_id = paste0(gsub("\\s", "", tolower(player)), "_", gsub("\\s", "", tolower(college)), "_", draft_year, "_", pick),
         pl_yr_id = paste0(gsub("\\s", "", tolower(player)), "_", gsub("\\s", "", tolower(college)), "_", draft_year, "_", pick, "_", year)) %>%
  group_by(pl_yr_id) %>%
  mutate(team = paste(team, collapse = "-"),
         team = gsub("TOT-", "", team)) %>%
  distinct(pl_yr_id, .keep_all = TRUE) %>%
  group_by(player_id) %>%
  mutate(exp = row_number(),
         exp = case_when(player_id == "adriandantley_notredame_1976_6" ~ exp + 3,
                         player_id == "alexenglish_southcarolina_1976_23" ~ exp + 3,
                         player_id == "bernardking_tennessee_1977_7" ~ exp + 2,
                         player_id == "braddavis_maryland_1977_15" ~ exp + 2,
                         player_id == "caldwelljones_albanystateuniversity_1973_32" ~ exp + 6,
                         player_id == "darryldawkins__1975_5" ~ exp + 4,
                         player_id == "davecorzine_depaul_1978_18" ~ exp + 1,
                         player_id == "dennisjohnson_pepperdine_1976_29" ~ exp + 3,
                         player_id == "gregballard_oregon_1977_4" ~ exp + 2,
                         player_id == "jacksikma_illinoiswesleyanuniversity_1977_8" ~ exp + 2,
                         player_id == "jamesedwards_washington_1977_46" ~ exp + 2,
                         player_id == "jeromewhitehead_marquette_1978_41" ~ exp + 1,
                         player_id == "johnlong_detroitmercy_1978_29" ~ exp + 1,
                         player_id == "johnlucas_maryland_1976_1" ~ exp + 3,
                         player_id == "kareemabdul-jabbar_ucla_1969_1" ~ exp + 10,
                         player_id == "marquesjohnson_ucla_1977_3" ~ exp + 2,
                         player_id == "mauricecheeks_westtexasa&muniversity_1978_36" ~ exp + 1,
                         player_id == "michaelcooper_newmexico_1978_60" ~ exp + 1,
                         player_id == "mikedunleavy_southcarolina_1976_99" ~ exp + 3,
                         player_id == "mikemitchell_auburn_1978_15" ~ exp + 1,
                         player_id == "mosesmalone_NA_NA_NA" ~ exp + 5,
                         player_id == "mychalthompson_minnesota_1978_1" ~ exp + 1,
                         player_id == "normnixon_duquesne_1977_22" ~ exp + 2,
                         player_id == "otisbirdsong_houston_1977_2" ~ exp + 2,
                         player_id == "purvisshort_jacksonstateuniversity_1978_5" ~ exp + 1,
                         player_id == "reggietheus_unlv_1978_9" ~ exp + 1,
                         player_id == "rickeygreen_michigan_1977_16" ~ exp + 2,
                         player_id == "robertparish_centenary(la)_1976_8" ~ exp + 3,
                         player_id == "robertreid_st.mary'suniversity_1977_40" ~ exp + 2,
                         player_id == "t.r.dunn_alabama_1977_41" ~ exp + 2,
                         player_id == "terrytyler_detroitmercy_1978_23" ~ exp + 1,
                         player_id == "treerollins_clemson_1977_14" ~ exp + 2,
                         player_id == "walterdavis_unc_1977_5" ~ exp + 2,
                         player_id == "waynecooper_neworleans_1978_40" ~ exp + 1,
                         TRUE ~ exp),
         yrs_off = ifelse(is.na(lag(year)), 0, year - lag(year) - 1)) %>%
  select(player:age, exp, yrs_off, team:alldef_share, player_id, pl_yr_id)

# use this to check for players where experience value is not correct
# only for players who started playing before 1980, because that is where our dataset starts
need_exp <- single_rows %>%
  group_by(player, player_id) %>%
  summarize(start_year = min(year),
            end_year = max(year),
            draft_year = max(draft_year)) %>%
  filter(start_year <= 1985,
         end_year >= 1989,
         draft_year <= 1979 | is.na(draft_year))

# save file
write.csv(single_rows, "data/all_player_stats_1980-2024_single_rows.csv", row.names = FALSE)
         
         