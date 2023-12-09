viewable_opps = read.csv("data/viewable_opps.csv")
opponentSRurl_db = read.csv("data/opp_url.csv", fileEncoding = "ISO-8859-1") %>% 
  mutate(ESPN_name = ifelse(ESPN_name == "", opponent, ESPN_name),
         KP_name = ifelse(KP_name == "", gsub(" State", " St.", opponent), KP_name))

OO_TrendStat_List = c("Winning Margin" = "Winning_Margin", 
                      #"ATS Margin" = "ATS_Margin",
                      "Pace" = "Pace",
                      "Offensive Efficency" = "Offensive_Efficency", 
                      "Defensive Efficency" = "Defensive_Efficency", 
                      "Points Scored" = "Points_Scored", 
                      "Points Allowed" = "Points_Allowed", 
                      "Offensive 3PAr" = "Offensive_3PAr", 
                      "Offensive 3P%" = "Offensive_3Ppct", 
                      "Offensive TO%" = "Offensive_TOpct", 
                      "Offensive OREB%" = "Offensive_OREBpct", 
                      "Offensive FTr" = "Offensive_FTr", 
                      "Defensive 3PAr" = "Defensive_3PAr", 
                      "Defensive 3P%" = "Defensive_3Ppct", 
                      "Defensive TO%" = "Defensive_TOpct", 
                      "Defensive OREB%" = "Defensive_OREBpct", 
                      "Defensive FTr" = "Defensive_FTr")

#empty df to populate
OO_splits_data = data.frame()
Opp_Trends_df = data.frame()


for(opp in viewable_opps$opp){
  #SR data
  opponentSRurl = opponentSRurl_db %>% filter(opponent == opp) %>% pull(SRurl)
  SRopponentTables = read_html(opponentSRurl) %>% html_table()
  
  #creates a variable for opponent name when referring to kp functions
  opponent_kp = opponentSRurl_db %>% filter(opponent == opp) %>% pull(KP_name)
  #creates a variable for opponent name when using espn functions
  opponent_espn = opponentSRurl_db %>% filter(opponent == opp) %>% pull(ESPN_name)
  
  #OO Graph
  #find the conference of our opp
  #right now, conf tournament games are not being included as conf games
  opp_conf = kp_program_ratings() %>%
    #manually change the abbreviation of certain conferences
    mutate(conf = ifelse(conf == "Amer", "AAC", conf)) %>%
    filter(team == opponent_kp) %>%
    pull(conf)

  # #opponent espn id of opp
  # opp_id = espn_mbb_teams() %>%
  #   filter(team == opponent_espn) %>%
  #   pull(team_id)
  # 
  # find the spread in every game our opp has played this season
  # opp_spreads = {
  #   df <- load_mbb_schedule(seasons = this_year) %>%
  #     filter(home_id == opp_id | away_id == opp_id,
  #            status_type_completed == TRUE) %>%
  #     select(game_id = id, date, home_short_display_name, away_short_display_name, home_id, away_id) %>%
  #     arrange(date) %>%
  #     mutate(spread = NA,
  #            #turn date from a character to a date column and adjust the time zone so the correct date is reflected
  #            date = gsub("T", " ", date),
  #            date = gsub("Z", ":00", date),
  #            date = strptime(date, "%Y-%m-%d %H:%M:%S") - 6*60*60,
  #            game_date = gsub("-", "", substr(date, 1, 10)))
  # 
  #   for(r in 1:nrow(df)) {
  #     df[r,"spread"] = espn_mbb_betting(df$game_id[r])[[1]] %>%
  #       filter(provider_name == "consensus") %>% .[[1,"spread"]] %>% as.integer()
  #     Sys.sleep(3)}
  # 
  #   df <- df %>%
  #     mutate(spread = ifelse(home_id == opp_id, spread, spread*-1),
  #            game_date = as.double(game_date)) %>%
  #     select(game_date, spread)
  # 
  # }

  #find other stats used in OO Trend table and format column names to match select input
  opp_gp_stats = kp_gameplan(opponent_kp, this_year)[[1]] %>%
    select(game_date, Pace=pace, Offensive_Efficency=off_eff, Defensive_Efficency=def_eff,
           Points_Scored=team_score, Points_Allowed=opponent_score,
           Offensive_3PAr=off_fg_3a_pct, 'Offensive_3Ppct'=off_fg_3_pct, 'Offensive_TOpct'=off_to_pct,
           'Offensive_OREBpct'=off_or_pct, Offensive_FTr=off_ftr,
           Defensive_3PAr=def_fg_3a_pct, 'Defensive_3Ppct'=def_fg_3_pct, 'Defensive_TOpct'=def_to_pct,
           'Defensive_OREBpct'=def_or_pct, Defensive_FTr=def_ftr)


  #pull trending stats and other stats used for formatting
  opp_game_stats = kp_team_schedule(opponent_kp, this_year) %>%
    select(opponent, game_date, location, conference_game, result) %>%
    full_join(kp_opptracker(opponent_kp, this_year), by = c("opponent", "game_date")) %>%
    mutate(opponent = clean_school_names(opponent)) %>%
    select(date, game_date, location, opponent, wl, result=result.x, conference_game) %>%
    mutate(wl = ifelse(is.na(date) & nchar(result)>1, substr(result, 1, 1), wl),
           result = ifelse(is.na(wl), NA, result),
           date = paste0(substr(game_date, 1, 4), "-", substr(game_date, 5, 6), "-", substr(game_date, 7, 8)),
           result = gsub("W, ", "", result),
           result = gsub("L, ", "", result)) %>%
    separate_wider_delim(result, delim = "-", names = c("w_score", "l_score")) %>%
    mutate(team_score = ifelse(wl == "W", as.numeric(w_score), as.numeric(l_score)),
           opponent_score = ifelse(wl == "W", as.numeric(l_score), as.numeric(w_score))) %>%
    select(date, game_date, location, opponent, wl, team_score, opponent_score, conference_game) %>%
    mutate(Winning_Margin = ifelse(is.na(team_score), NA, team_score - opponent_score),
           #create a unique identifier for each game since teams can be played multiple times
           game_code = paste(opponent, game_date),
           #if not a true home game, consider it a road game
           location = ifelse(location=="Home", "Home", "Away")) %>%
    #find NET rankings for teams our opp has played
    left_join(resume_database(min_year = this_year, max_year = this_year, min_net = 1) %>%
                filter(year == this_year) %>%
                mutate(team = clean_school_names(team),
                       net = as.numeric(net)) %>%
                select(opponent=team, net)) %>%
    mutate(net_rk = ifelse(net <= 100,
                           ifelse(net <= 50, "NET Top 50", "NET Top 100"),
                           "Other"),
           net_rk = ifelse(is.na(net), "Other", net_rk),
           conference_game = ifelse(conference_game==TRUE, opp_conf, "OOC")) %>%
    # #create ATS Margin
    # left_join(opp_spreads, by="game_date") %>%
    # mutate(ATS_Margin = Winning_Margin + spread) %>%
    #join in other used stats
    left_join(opp_gp_stats, by="game_date")
  
  #create a variable for games played so far this season
  gp = opp_game_stats %>%
    filter(!is.na(wl)) %>%
    nrow()

  #create separate data frames to split the data based on the input so that we can calculate mean in each split
  for(stat in OO_TrendStat_List){
    opp_game_stats_recency5 = opp_game_stats %>% filter(!is.na(team_score)) %>%
      .[ifelse(gp-4<1, 1, gp-4):gp,] %>%
      mutate(recency5 = "Last 5 Games",
             Team = opp,
             split = 'recency5',
             variable = stat) %>%
      select(Team, split, variable, game_code, stat) %>%
      rename(stats = 5)

    opp_game_stats_recency10 = opp_game_stats %>% filter(!is.na(team_score)) %>%
      .[ifelse(gp-9<1, 1, gp-9):gp,] %>%
      mutate(recency10 = "Last 10 Games",
             Team = opp,
             split = 'recency10',
             variable = stat) %>%
      select(Team, split, variable, game_code, stat) %>%
      rename(stats = 5)

    opp_game_stats_home = opp_game_stats %>% filter(!is.na(team_score)) %>%
      filter(location == "Home") %>%
      mutate(Team = opp,
             split = 'home',
             variable = stat) %>%
      select(Team, split, variable, game_code, stat) %>%
      rename(stats = 5)

    opp_game_stats_away = opp_game_stats %>% filter(!is.na(team_score)) %>%
      filter(location == "Away") %>%
      mutate(Team = opp,
             split = 'away',
             variable = stat) %>%
      select(Team, split, variable, game_code, stat) %>%
      rename(stats = 5)

    opp_game_stats_net50 = opp_game_stats %>% filter(!is.na(team_score)) %>%
      filter(net <= 50) %>%
      mutate(Team = opp,
             split = 'net50',
             variable = stat) %>%
      select(Team, split, variable, game_code, stat) %>%
      rename(stats = 5)

    opp_game_stats_net100 = opp_game_stats %>% filter(!is.na(team_score)) %>%
      filter(net <= 100) %>%
      mutate(Team = opp,
             split = 'net100',
             variable = stat) %>%
      select(Team, split, variable, game_code, stat) %>%
      rename(stats = 5)

    opp_game_stats_wins = opp_game_stats %>%
      filter(wl == "W") %>%
      mutate(Team = opp,
             split = 'wins',
             variable = stat) %>%
      select(Team, split, variable, game_code, stat) %>%
      rename(stats = 5)

    opp_game_stats_losses = opp_game_stats %>%
      filter(wl == "L") %>%
      mutate(Team = opp,
             split = 'losses',
             variable = stat) %>%
      select(Team, split, variable, game_code, stat) %>%
      rename(stats = 5)

    opp_game_stats_conf = opp_game_stats %>% filter(!is.na(team_score)) %>%
      filter(conference_game == opp_conf) %>%
      mutate(Team = opp,
             split = 'conf',
             variable = stat) %>%
      select(Team, split, variable, game_code, stat) %>%
      rename(stats = 5)

    OO_trend_per_stat = rbind(opp_game_stats_recency5, opp_game_stats_recency10,
                              opp_game_stats_home, opp_game_stats_away,
                              opp_game_stats_net50, opp_game_stats_net100,
                              opp_game_stats_wins, opp_game_stats_losses, opp_game_stats_conf)

    OO_splits_data = rbind(OO_splits_data, OO_trend_per_stat)
  }


  #limit teams on the trends graph to games that have been played already and the next three games
  Opp_Trends_df_team = rbind(opp_game_stats %>% filter(!is.na(team_score)),
                        opp_game_stats %>% filter(is.na(team_score)) %>% .[1:3,]) %>%
    filter(!is.na(opponent)) %>%
    mutate(Team = opp)

  Opp_Trends_df = rbind(Opp_Trends_df, Opp_Trends_df_team)

  Sys.sleep(30)
}

write.csv(Opp_Trends_df, "Oregon/data/Opp_Trends_df.csv", row.names = FALSE)
write.csv(Opp_Trends_df, "Clemson/data/Opp_Trends_df.csv", row.names = FALSE)
write.csv(Opp_Trends_df, "MississippiState/data/Opp_Trends_df.csv", row.names = FALSE)
write.csv(Opp_Trends_df, "NewMexico/data/Opp_Trends_df.csv", row.names = FALSE)

write.csv(OO_splits_data, "Oregon/data/OO_splits_data.csv", row.names = FALSE)
write.csv(OO_splits_data, "Clemson/data/OO_splits_data.csv", row.names = FALSE)
write.csv(OO_splits_data, "MississippiState/data/OO_splits_data.csv", row.names = FALSE)
write.csv(OO_splits_data, "NewMexico/data/OO_splits_data.csv", row.names = FALSE)

rm(OO_trend_per_stat, opp_game_stats, opp_game_stats_away, opp_game_stats_conf, opp_game_stats_home, opp_game_stats_losses,
   opp_game_stats_net100, opp_game_stats_net50, opp_game_stats_recency10, opp_game_stats_recency5, opp_game_stats_wins, 
   opp_gp_stats, Opp_Trends_df, Opp_Trends_df_team, opponentSRurl_db, SRopponentTables, viewable_opps, gp, OO_TrendStat_List,
   opp, opp_conf, opponent_espn, opponent_kp, opponentSRurl, stat)
