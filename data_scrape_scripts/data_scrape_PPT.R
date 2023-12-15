viewable_opps = read.csv("data/viewable_opps.csv")
opponentSRurl_db = read.csv("data/opp_url.csv", fileEncoding = "ISO-8859-1") %>% 
  mutate(ESPN_name = ifelse(ESPN_name == "", opponent, ESPN_name),
         KP_name = ifelse(KP_name == "", gsub(" State", " St.", opponent), KP_name))

headshot_urls_db = read.csv("data/headshot_url.csv") %>%
  mutate(athlete_headshot_href = ifelse(athlete_headshot_href == "", "https://a.espncdn.com/combiner/i?img=/i/headshots/nophoto.png", athlete_headshot_href))

#empty df to populate
PPT_data = data.frame()

for(opp in viewable_opps$opp){
  #SR data
  opponentSRurl = opponentSRurl_db %>% filter(opponent == opp) %>% pull(SRurl)
  if(opponentSRurl == ""){next}
  SRopponentTables = read_html(opponentSRurl) %>% html_table()
  
  SR2_tab_num = ifelse(length(SRopponentTables) == 8, 4, 6)
  SR3_tab_num = ifelse(length(SRopponentTables) == 8, 8, 14)
  
  #creates a variable for opponent name when referring to kp functions
  opponent_kp = opponentSRurl_db %>% filter(opponent == opp) %>% pull(KP_name)
  #creates a variable for opponent name when using espn functions
  opponent_espn = opponentSRurl_db %>% filter(opponent == opp) %>% pull(ESPN_name)
  
  #PPT Data coming from different sources
  SR_PPT = SRopponentTables[[1]] %>%
    select('#', Player, Pos) %>%
    mutate(player_join = standardize_name(Player)) %>%
    distinct() #there is one team that I found that has a duplicate player in this SR table
  SR2_PPT = SRopponentTables[[SR2_tab_num]] %>%
    select(Player, G, GS, MPG=MP, twoPperc="2P%", threePperc="3P%", ftperc="FT%", ApG=AST, TOV, PPG=PTS) %>%
    mutate(player_join = standardize_name(Player))
  SR3_PPT = SRopponentTables[[SR3_tab_num]] %>%
    select(Player, "USG%", eFGperc="eFG%", tsperc="TS%", threePAr="3PAr", "FTr", "AST%", "TOV%", 
           "PER", "OBPM", "DBPM", "BPM", "ORB%", "DRB%", "TRB%", "STL%", "BLK%") %>%
    mutate(player_join = standardize_name(Player))
  KP_PPT = kp_team_players(opponent_kp, this_year) %>%
    select(number, ht, wt, yr, poss_pct, f_dper40, f_cper40)
  headshots_PPT = headshot_urls_db %>% filter(team_location == opp)
  
  #find player position
  kp_pos = kp_team_depth_chart(opponent_kp, this_year)
  pos_pg = kp_pos %>% select(first = pg_player_first_name, last = pg_player_last_name, min_pct = pg_min_pct, '#' = pg_number) %>% mutate(pos="PG") %>% filter(!is.na(first))
  pos_sg = kp_pos %>% select(first = sg_player_first_name, last = sg_player_last_name, min_pct = sg_min_pct, '#' = sg_number) %>% mutate(pos="SG") %>% filter(!is.na(first))
  pos_sf = kp_pos %>% select(first = sf_player_first_name, last = sf_player_last_name, min_pct = sf_min_pct, '#' = sf_number) %>% mutate(pos="SF") %>% filter(!is.na(first))
  pos_pf = kp_pos %>% select(first = pf_player_first_name, last = pf_player_last_name, min_pct = pf_min_pct, '#' = pf_number) %>% mutate(pos="PF") %>% filter(!is.na(first))
  pos_c = kp_pos %>% select(first = c_player_first_name, last = c_player_last_name, min_pct = c_min_pct, '#' = c_number) %>% mutate(pos="C") %>% filter(!is.na(first))
  KP2_PPT = rbind(pos_pg, pos_sg, pos_sf, pos_pf, pos_c) %>% 
    pivot_wider(names_from = pos, values_from = min_pct, values_fill = 0) %>% 
    mutate(total=PG+SG+SF+PF+C) %>%
    mutate(PG = PG/total, SG=SG/total, SF=SF/total, PF=PF/total, C=C/total) %>% .[,-9] %>%
    select('#', PG, SG, SF, PF, C)
  
  #find starters for last game the team played
  lastGdate = kp_team_schedule(opponent_kp, year=this_year) %>% filter(is.na(pre_wp)) %>% arrange(desc(date)) %>% .[[1,18]]
  opponentGID = espn_mbb_scoreboard(lastGdate) %>%
    filter(home_team_location == opponent_espn | away_team_location == opponent_espn) %>% .[[1,6]]
  lastGstarters = espn_mbb_player_box(opponentGID) %>% 
    mutate(athlete_jersey = as.double(athlete_jersey)) %>%
    filter(starter==TRUE) %>%
    filter(team_short_display_name == gsub(" St.", " St", opponent_kp) | team_short_display_name == opp) %>%
    select("#" = athlete_jersey, starter)
  
  #join together all sources of info for PPT
  PPT_data_temp = SR_PPT %>%
    full_join(SR2_PPT, by = 'player_join') %>%
    full_join(SR3_PPT, by = 'player_join') %>%
    full_join(KP_PPT, by = c('#' = 'number')) %>%
    full_join(headshots_PPT, by = c('#' = 'athlete_jersey')) %>%
    full_join(KP2_PPT, by = '#') %>%
    left_join(lastGstarters, by = '#') %>%
    mutate('All Players' = 1,
           "Guards" = ifelse(PG+SG>.7 | (is.na(PG+SG+SF+PF+C) & Pos=="G"), 1, 0),
           "Bigs" = ifelse(C+PF>.8 & C>0 | (is.na(PG+SG+SF+PF+C) & Pos=="C") , 1, 0),
           "Wings" = ifelse(Guards+Bigs==0 | (is.na(Guards) & is.na(Bigs)), 1, 0),
           "Starters" = ifelse(is.na(starter), 0, 1),
           "Lefties" = 0 ) %>% #don't know where I can find data on this yet
    #make 0s a NA so they don't show up in the table and multiply the other values so they don't show up as decimals
    mutate(PG = as.numeric(ifelse(PG==0, NA, PG*100)),
           SG = as.numeric(ifelse(SG==0, NA, SG*100)),
           SF = as.numeric(ifelse(SF==0, NA, SF*100)),
           PF = as.numeric(ifelse(PF==0, NA, PF*100)),
           C = as.numeric(ifelse(C==0, NA, C*100))) %>%
    #round percentages
    mutate(twoPperc = round(twoPperc*100, 1),
           threePperc = round(threePperc*100, 1),
           threePAr = round(threePAr*100, 1),
           eFGperc = round(eFGperc*100, 1),
           tsperc = round(tsperc*100, 1),
           ftperc = round(ftperc*100, 1),
           FTr = round(FTr*100, 1)) %>%
    mutate(G = ifelse(!is.na(G), G, 0),
           MPG = ifelse(!is.na(MPG), MPG, 0),
           "AST:TO" = round(ApG / TOV, 2)) %>%
    select(-starter) %>%
    separate(Player.x, into = c('first', 'last'), sep = "[^\\w'.-]", extra = 'merge') %>%
    #order columns into the order I want them to appear in the PPT
    select("#", player_join, URL=athlete_headshot_href, first, last, Team=team_location, 
           Class=yr, Pos, Height=ht, Weight=wt, 
           GP=G, GS, MPG, "Poss%"=poss_pct, "USG%",
           PG, SG, SF, PF, C,
           PER, OBPM, DBPM, BPM,
           PPG, "FD/40"=f_dper40,
           "2P%"=twoPperc, "3P%"=threePperc, "3PAr"=threePAr, "eFG%"=eFGperc, "TS%"=tsperc, "FT%"=ftperc, FTr,
           ApG, "AST:TO", "AST%", "TOV%",
           "ORB%", "DRB%", "TRB%",
           "STL%", "BLK%", "FC/40"=f_cper40,
           "All Players", Guards, Wings, Bigs, Starters, Lefties) %>%
    filter(!is.na(player_join))
  
  PPT_data = rbind(PPT_data, PPT_data_temp)
  
  Sys.sleep(30)
}

write.csv(PPT_data, file = "data/PPT_data.csv", row.names = FALSE)
pb_upload("data/PPT_data.csv")

rm(headshot_urls_db, headshots_PPT, kp_pos, KP_PPT, KP2_PPT, lastGstarters, opponentSRurl_db, pos_c, pos_pf, pos_pg,
   pos_sf, pos_sg, PPT_data_temp, SR_PPT, SR2_PPT, SR3_PPT, SRopponentTables, viewable_opps, lastGdate, opp, opponent_espn,
   opponent_kp, opponentGID, opponentSRurl, SR2_tab_num, SR3_tab_num)
