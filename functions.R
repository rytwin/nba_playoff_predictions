teams <- c("ATL", "BOS", "BRK", "CHO", "CHI", "CLE", "DAL", "DEN", "DET", "GSW", 
           "HOU", "IND", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN", "NOP", "NYK",
           "OKC", "ORL", "PHI", "PHO", "POR", "SAC", "SAS", "TOR", "UTA", "WAS", 
           "NJN", "SEA", "CHA", "CHH", "WSB", "VAN", "NOH", "NOK", "KCK", "SDC")
franchises <- c("ATL", "BOS", "NJN", "CHA", "CHI", "CLE", "DAL", "DEN", "DET", "GSW", 
                "HOU", "IND", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN", "NOH", "NYK",
                "OKC", "ORL", "PHI", "PHO", "POR", "SAC", "SAS", "TOR", "UTA", "WAS")
current_tms <- c("ATL", "BOS", "BRK", "CHO", "CHI", "CLE", "DAL", "DEN", "DET", "GSW", 
                 "HOU", "IND", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN", "NOP", "NYK", 
                 "OKC", "ORL", "PHI", "PHO", "POR", "SAC", "SAS", "TOR", "UTA", "WAS")
current_teams <- c("Atlanta Hawks", "Boston Celtics", "Brooklyn Nets", 
                   "Charlotte Hornets", "Chicago Bulls", "Cleveland Cavaliers", 
                   "Dallas Mavericks", "Denver Nuggets", "Detroit Pistons", 
                   "Golden State Warriors", "Houston Rockets", "Indiana Pacers", 
                   "Los Angeles Clippers", "Los Angeles Lakers", "Memphis Grizzlies", 
                   "Miami Heat", "Milwaukee Bucks", "Minnesota Timberwolves", 
                   "New Orleans Pelicans", "New York Knicks", "Oklahoma City Thunder", 
                   "Orlando Magic", "Philadelphia 76ers", "Phoenix Suns", 
                   "Portland Trail Blazers", "Sacramento Kings", "San Antonio Spurs", 
                   "Toronto Raptors", "Utah Jazz", "Washington Wizards")
team_colors <- c(ATL = "#E03A3E", BOS = "#007A33", BRK = "#000000", CHI = "#CE1141", CHO = "#00788C", CLE = "#860038", 
                 DAL = "#00538C", DEN = "#FEC524", DET = "#C8102E", GSW = "#FFC72C", HOU = "#CE1141", IND = "#FDBB30", 
                 LAC = "#1D428A", LAL = "#552583", MEM = "#5D76A9", MIA = "#98002E", MIL = "#00471B", MIN = "#78BE20", 
                 NOP = "#85714D", NYK = "#F58426", OKC = "#007AC1", ORL = "#0077C0", PHI = "#006BB6", PHO = "#E56020", 
                 POR = "#E03A3E", SAC = "#5A2D81", SAS = "#C4CED4", TOR = "#753BBD", UTA = "#002B5C", WAS = "#E31837")

colnames_team_stats_total <- c("rk", "team", "g", "mp", "fg", "fga", "fgp", "fg3", "fga3", "fgp3", "fg2", "fga2", "fgp2", "ft",
                               "fta", "ftp", "oreb", "dreb", "totreb", "ast", "stl", "blk", "tov", "pf", "pts", "year")
colnames_team_stats_total_opp <- c("opp_rk", "team", "opp_g", "opp_mp", "opp_fg", "opp_fga", "opp_fgp", "opp_fg3", "opp_fga3",
                                   "opp_fgp3", "opp_fg2", "opp_fga2", "opp_fgp2", "opp_ft", "opp_fta", "opp_ftp", "opp_oreb",
                                   "opp_dreb", "opp_totreb", "opp_ast", "opp_stl", "opp_blk", "opp_tov", "opp_pf", "opp_pts", "year")
colnames_team_stats_per100 <- c("rk", "team", "g", "mp", "fg_per100", "fga_per100", "fgp", "fg3_per100", "fga3_per100", "fgp3",
                                "fg2_per100", "fga2_per100", "fgp2", "ft_per100", "fta_per100", "ftp", "oreb_per100", "dreb_per100",
                                "totreb_per100", "ast_per100", "stl_per100", "blk_per100", "tov_per100", "pf_per100", "pts_per100",
                                "year")
colnames_team_stats_per100_opp <- c("rk", "team", "g", "mp", "opp_fg_per100", "opp_fga_per100", "fgp", "opp_fg3_per100",
                                    "opp_fga3_per100", "fgp3", "opp_fg2_per100", "opp_fga2_per100", "fgp2", "opp_ft_per100",
                                    "opp_fta_per100", "ftp", "opp_oreb_per100", "opp_dreb_per100", "opp_totreb_per100",
                                    "opp_ast_per100", "opp_stl_per100", "opp_blk_per100", "opp_tov_per100", "opp_pf_per100",
                                    "opp_pts_per100", "year")
colnames_team_stats_adv <- c("rk", "team", "age", "w", "l", "pw", "pl", "mov", "sos", "srs", "off_rtg", "def_rtg", "net_rtg",
                             "pace", "ft_rate", "rate_3pa", "ts_pct", "del1", "efg", "tov_pct", "orb_pct", "ft_per_fga", "del2",
                             "opp_efg", "opp_tov_pct", "opp_drb_pct", "opp_ft_per_fga", "del3", "arena", "attend", "attend_pg", "year")
colnames_team_stats_adv_playoffs <- c("rk", "team", "age", "w", "l", "pw", "pl", "srs", "off_rtg", "def_rtg", "net_rtg",
                                      "pace", "ft_rate", "rate_3pa", "ts_pct", "del1", "efg", "tov_pct", "orb_pct", "ft_per_fga", "del2",
                                      "opp_efg", "opp_tov_pct", "opp_drb_pct", "opp_ft_per_fga", "year")
colnames_team_stats_shoot <- c("rk", "team", "g", "mp", "fgp", "avg_fg_dist", "del0", "pct_of_fg_2p", "pct_of_fg_0_3",
                               "pct_of_fg_3_10", "pct_of_fg_10_16", "pct_of_fg_16_3p", "pct_of_fg_3p", "del1", "fgp2",
                               "fgp_0_3", "fgp_3_10", "fgp_10_16", "fgp_16_3p", "fgp3", "del2", "pct_of_2p_ast", "pct_of_3p_ast",
                               "del3", "pct_of_fg_dunk", "dunks_made", "del4")
colnames_team_stats_shoot_opp <- c("opp_rk", "team", "opp_g", "opp_mp", "opp_fgp", "opp_avg_fg_dist", "del0", "opp_pct_of_fg_2p",
                                   "opp_pct_of_fg_0_3", "opp_pct_of_fg_3_10", "opp_pct_of_fg_10_16", "opp_pct_of_fg_16_3p",
                                   "opp_pct_of_fg_3p", "del1", "opp_fgp2", "opp_fgp_0_3", "opp_fgp_3_10", "opp_fgp_10_16",
                                   "opp_fgp_16_3p", "opp_fgp3", "del2", "opp_pct_of_2p_ast", "opp_pct_of_3p_ast", "del3",
                                   "opp_pct_of_fg_dunk", "opp_dunks_made", "del4")
colnames_franchise_encyc <- c("year", "league", "team", "w", "l", "win_pct", "finish", "srs", "del1", "pace", "rel_pace",
                              "ortg", "rel_ortg", "drtg", "rel_drtg", "del2", "playoff", "coach", "top_ws")
colnames_team_ratings <- c("rk", "team", "conf", "div", "w", "l", "win_pct", "mov", "ortg2", "drtg2", "net_rtg2",
                           "mov_adj", "ortg2_adj", "drtg2_adj", "net_rtg2_adj", "year")
colnames_coaches <- c("coach", "team", "del0", "coach_team_exp", "coach_exp", "del1", "coach_season_g", "coach_season_w",
                      "coach_season_l", "coach_team_g", "coach_team_w", "coach_team_l", "coach_career_g", "coach_career_w",
                      "coach_career_l", "coach_win_pct", "del3", "coach_season_p_g", "coach_season_p_w", "coach_season_p_l", 
                      "coach_team_p_g", "coach_team_p_w", "coach_team_p_l", "coach_career_p_g", "coach_career_p_w",
                      "coach_career_p_l", "year")
colnames_preseason_odds_a <- c("team", "champ_odds", "year")
colnames_preseason_odds_b <- c("team", "champ_odds", "year", "del", "o_u", "result")
colnames_preseason_odds_c <- c("team", "champ_odds", "del", "o_u", "result", "year")

scrape_bballref <- function(url, table_id) {
  # function that takes a url and table number from that url, and returns the table as a dataframe.
  # specifically for use with basketball reference because it removes the comments
  # in the html code that causes the tables on the website to not be recognized as tables.
  
  ## read in data from the webpage
  urltxt <- readLines(url)
  ## REMOVE COMMENT TAGS
  urltxt <- gsub("-->", "", gsub("<!--", "", urltxt))
  ## PARSE UNCOMMENTED TEXT
  doc <- htmlParse(urltxt)
  ## RETRIEVE ALL <table> TAGS
  tables <- xpathApply(doc, "//table")
  ## LIST OF DATAFRAMES
  teamPageTables <- tryCatch(lapply(tables[table_id], function(i) readHTMLTable(i)), 
                             error = function(cond) {
                               message("ERROR (but continued running): ", url)
                               return(NULL)
                             })
  return(teamPageTables)
}

scrape_team_stats <- function(years, regular_season = TRUE, playoff_series_completed = 15) {
  # function that takes a vector of years and returns the team stats as one merged dataframe with all stats
  # (total, opp total, per 100, opp per 100, advanced, shooting, opp shooting)
  # set regular_season = TRUE for regular season stats or FALSE for playoff stats
  # playoff_series_completed = number of playoff series completed in the current season (only relevant if you are scraping a season that is not yet completed)
  
  url <- ifelse(regular_season, "https://www.basketball-reference.com/leagues/NBA_", "https://www.basketball-reference.com/playoffs/NBA_")
  table_id <- ifelse(regular_season, 23, 19)
  list_of_dfs <- list(tot = list(), tot_opp = list(), per100 = list(), per100_opp = list(),
                      adv = list(), shoot = list(), shoot_opp = list())
  for(val in years) {
    season_url <- paste0(url, val, ".html")
    idx_num <- case_when(val < 1984 & regular_season ~ table_id - 6,
                         val < 1984 ~ table_id - 4,
                         val < 2016 & regular_season ~ table_id - 2,
                         TRUE ~ table_id)
    idx_num <- ifelse(playoff_series_completed != 15, idx_num - 15 + playoff_series_completed, idx_num)
    idx_num <- ifelse(playoff_series_completed == 0, idx_num - 1, idx_num)
    idx_add <- ifelse(val < 1997, 4, 6)
    scraped_tables <- scrape_bballref(season_url, seq.int(idx_num, idx_num+idx_add))
    for(i in 1:length(scraped_tables)){
      df <- data.frame(scraped_tables[i]) %>%
        mutate(year = val)
      if("Md..2" %in% names(df)) {
        df <- df %>%
          rename(made = Md..1,
                 Md..1 = Md..2)
      }
      # if("X..6" %in% names(df) & !regular_season) {
      #   df <- df %>%
      #     rename(Md..1 = X..6)
      # }
      if("Tm" %in% names(df)) {
        df <- df %>%
          rename(Team = Tm)
      }
      list_of_dfs[[i]] = c(list_of_dfs[[i]], list(df))
    }
    print(val)
    Sys.sleep(10)
  }
  
  # combine each list of dataframes into one dataframe each
  tot <- bind_rows(list_of_dfs["tot"])
  tot_opp <- bind_rows(list_of_dfs["tot_opp"])
  per100 <- bind_rows(list_of_dfs["per100"])
  per100_opp <- bind_rows(list_of_dfs["per100_opp"])
  adv <- bind_rows(list_of_dfs["adv"])
  shoot <- bind_rows(list_of_dfs["shoot"])
  shoot_opp <- bind_rows(list_of_dfs["shoot_opp"])
  
  names(tot) <- colnames_team_stats_total
  tot <- tot %>%
    select(-rk) %>%
    rename_teams()
  
  names(tot_opp) <- colnames_team_stats_total_opp
  tot_opp <- tot_opp %>%
    select(-c(opp_rk, opp_g, opp_mp)) %>%
    rename_teams()
  
  names(per100) <- colnames_team_stats_per100
  per100 <- per100 %>%
    select(-c(rk, g, mp, fgp, fgp3, fgp2, ftp)) %>%
    rename_teams()
  
  names(per100_opp) <- colnames_team_stats_per100_opp
  per100_opp <- per100_opp %>%
    select(-c(rk, g, mp, fgp, fgp3, fgp2, ftp)) %>%
    rename_teams()
  
  if(regular_season) {
    names(adv) <- colnames_team_stats_adv
    adv <- adv %>%
      mutate(attend_pg = gsub(",", "", attend_pg),
             attend = gsub(",", "", attend))
  } else {
    names(adv) <- colnames_team_stats_adv_playoffs
  }
  adv <- adv %>%
    select(-c(starts_with("del"), rk)) %>%
    rename_teams() %>%
    mutate(net_rtg = as.numeric(net_rtg))
  
  if(max(years) > 2001 & min(years) > 2001) {
    colnames_team_stats_shoot <- c(colnames_team_stats_shoot, "pct_of_fg_layup", "layups_made", "del5", "pct_of_3_corner",
                                   "fgp3_corner", "del6", "heaves_att", "heaves_made", "year")
    colnames_team_stats_shoot_opp <- c(colnames_team_stats_shoot_opp, "opp_pct_of_fg_layup", "opp_layups_made", "del5",
                                       "opp_pct_of_3_corner", "opp_fgp3_corner", "year")
  } else if(max(years) > 2001 & min(years) <= 2001) {
    colnames_team_stats_shoot <- c(colnames_team_stats_shoot, "pct_of_3_corner", "fgp3_corner", "del6", "heaves_att",
                                   "heaves_made", "year", "pct_of_fg_layup", "layups_made", "del5")
    colnames_team_stats_shoot_opp <- c(colnames_team_stats_shoot_opp, "opp_pct_of_3_corner", "opp_fgp3_corner", "year",
                                       "opp_pct_of_fg_layup", "opp_layups_made", "del5")
  } else if(max(years) <= 2001 & max(years) >= 1997) {
    colnames_team_stats_shoot <- c(colnames_team_stats_shoot, "pct_of_3_corner", "fgp3_corner", "del6", "heaves_att", "heaves_made", "year")
    colnames_team_stats_shoot_opp <- c(colnames_team_stats_shoot_opp, "opp_pct_of_3_corner", "opp_fgp3_corner", "year")
  }
  
  if(max(years) >= 1997) {
    names(shoot) <- colnames_team_stats_shoot
    shoot <- shoot %>%
      select(-c(starts_with("del"), rk, g, mp, fgp, fgp2, fgp3)) %>%
      rename_teams()
    
    names(shoot_opp) <- colnames_team_stats_shoot_opp
    shoot_opp <- shoot_opp %>%
      select(-c(starts_with("del"), opp_rk, opp_g, opp_mp, opp_fgp, opp_fgp2, opp_fgp3)) %>%
      rename_teams()
  }
  
  team_stats <- tot %>%
    merge(tot_opp, by = c("team", "year"), all = TRUE) %>%
    merge(per100, by = c("team", "year"), all = TRUE) %>%
    merge(per100_opp, by = c("team", "year"), all = TRUE) %>%
    merge(adv, by = c("team", "year"), all = TRUE)
  
  if(max(years) >= 1997) {
    team_stats <- team_stats %>%
      merge(shoot, by = c("team", "year"), all = TRUE) %>%
      merge(shoot_opp, by = c("team", "year"), all = TRUE)
  }
  
  team_stats <- team_stats %>%
    filter(g != 0)
  
  return(team_stats)
}

scrape_yearly_tables <- function(url1, years, table_id, url2 = "") {
  # takes a url, years, and table_id. adds the year and binds the rows into one dataframe
  # uses scrape_bballref function to get data
  
  all_tables <- list()
  for(y in years) {
    url <- paste0(url1, y, url2)
    table <- data.frame(scrape_bballref(url, table_id))
    table <- table %>%
      mutate(year = y)
    all_tables <- c(all_tables, list(table))
    print(y)
    Sys.sleep(10)
  }
  
  return(bind_rows(all_tables))
}

valid_url <- function(url_in, t=2) {
  # function that takes a url and returns a boolean depending on whether the website exists or not
  con <- url(url_in)
  check <- suppressWarnings(try(open.connection(con, open = "rt", timeout = t),
                                silent = T)[1])
  suppressWarnings(try(close.connection(con), silent = T))
  return(is.null(check))
}

rename_teams <- function(df) {
  # function that takes a dataframe as an argument and replaces team names with team abbreviations
  # dataframe must have column named "team"
  
  df <- df %>%
    mutate(team = gsub("\\*", "", team),
           team = case_when(team == "Atlanta Hawks" ~ "ATL", 
                            team == "Boston Celtics" ~ "BOS", 
                            team == "Brooklyn Nets" ~ "BRK", 
                            team == "Charlotte Hornets" & year > 2013 ~ "CHO",
                            team == "Charlotte Hornets" & year < 2004 ~ "NOP",
                            team == "Chicago Bulls" ~ "CHI", 
                            team == "Cleveland Cavaliers" ~ "CLE", 
                            team == "Dallas Mavericks" ~ "DAL", 
                            team == "Denver Nuggets" ~ "DEN", 
                            team == "Detroit Pistons" ~ "DET", 
                            team == "Golden State Warriors" ~ "GSW", 
                            team == "Houston Rockets" ~ "HOU", 
                            team == "Indiana Pacers" ~ "IND", 
                            team == "Los Angeles Clippers" ~ "LAC", 
                            team == "Los Angeles Lakers" ~ "LAL", 
                            team == "Memphis Grizzlies" ~ "MEM", 
                            team == "Miami Heat" ~ "MIA", 
                            team == "Milwaukee Bucks" ~ "MIL", 
                            team == "Minnesota Timberwolves" ~ "MIN", 
                            team == "New Orleans Pelicans" ~ "NOP", 
                            team == "New York Knicks" ~ "NYK", 
                            team == "Oklahoma City Thunder" ~ "OKC", 
                            team == "Orlando Magic" ~ "ORL", 
                            team == "Philadelphia 76ers" ~ "PHI", 
                            team == "Phoenix Suns" ~ "PHO", 
                            team == "Portland Trail Blazers" ~ "POR", 
                            team == "Sacramento Kings" ~ "SAC", 
                            team == "San Antonio Spurs" ~ "SAS", 
                            team == "Toronto Raptors" ~ "TOR", 
                            team == "Utah Jazz" ~ "UTA", 
                            team == "Washington Wizards" ~ "WAS",
                            team == "Charlotte Bobcats" ~ "CHO",
                            team == "Kansas City Kings" ~ "SAC",
                            team == "New Jersey Nets" ~ "BRK",
                            team == "New Orleans Hornets" ~ "NOP",
                            team == "New Orleans/Oklahoma City Hornets" ~ "NOP",
                            team == "San Diego Clippers" ~ "LAC",
                            team == "Seattle SuperSonics" | team == "Seattle Supersonics" ~ "OKC",
                            team == "Vancouver Grizzlies" ~ "MEM",
                            team == "Washington Bullets" ~ "WAS",
                            TRUE ~ team))
  return(df)
}

rename_teams_abbrev <- function(df) {
  # function that takes a dataframe as an argument and replaces team abbreviations with current ones
  # dataframe must have column named "team"
  
  df <- df %>%
    mutate(team = case_when(team == "NJN" ~ "BRK",
                            team == "SEA" ~ "OKC",
                            team == "WSB" ~ "WAS",
                            team == "VAN" ~ "MEM",
                            team == "NOH" | team == "NOK" | team == "CHH" ~ "NOP",
                            team == "KCK" ~ "SAC",
                            team == "SDC" ~ "LAC",
                            team == "CHA" ~ "CHO",
                            TRUE ~ team))
  return(df)
}

rename_duplicate_coaches <- function(df, coach_col) {
  # function takes a dataframe and the name of the coach column as a string
  # replaces duplicate coach names with a unique name (dataframe must also have a year column)
  
  df <- df %>%
    mutate(coach = case_when(!!sym(coach_col) == "wes unseld" & year < 2020 ~ "wes unseld sr",
                             !!sym(coach_col) == "wunseld" & year < 2020 ~ "wunseld sr",
                             TRUE ~ !!sym(coach_col)))
  return(df)
}

prior_years_stats_simple <- function(df, stat, years = 1, match) {
  # function that takes a data frame and adds 'x' number of variables that equal 
  # the value of that statistic for the previous 'x' number of years, given that
  # 1 other variable matches in those years
  
  stat <- ifelse(is.numeric(stat), colnames(df[stat]), stat)
  z <- df
  
  for(y in years) {
    new_name <- paste0(stat, "_", y, "yr")
    z <- mutate(z, new_stat = ifelse(lag(z[[match]], y) == z[[match]],
                                     lag(z[[stat]], y), NA))
    names(z)[names(z) == "new_stat"] <- new_name
  }
  return(z)
} 



