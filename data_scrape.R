library(pacman)
p_load(tidyverse, janitor, cfbplotR, stringr, readxl, hoopR, toRvik, rvest)

our_team = "Oregon"
opponentList = c("Houston", "UCONN", "Alabama", "Michigan State", "Washington State", "UCLA")
opponentSRurl_db = read.csv("data/opp_url.csv")
year=2023

OO_TrendStat_List = c("Winning Margin" = "Winning_Margin", 
                      "ATS Margin" = "ATS_Margin",
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

#####basic offense table webscrape
basic_offense_url = "https://www.sports-reference.com/cbb/seasons/2023-school-stats.html"

basic_offense = read_html(basic_offense_url) %>%
  html_table(fill = TRUE) %>%
  .[[1]] %>% as.data.frame() %>%
  .[,-c(9,12,15,18,21)] %>% #remove empty columns
  row_to_names(row_number = 1, remove_row = T) %>% clean_names() %>% #use row 1 as col names
  filter(!g=='G') %>% filter(!g=="Overall") %>% #filter out 'header' rows scattered in the data frame
  mutate(school = ifelse(endsWith(school, "NCAA"), substr(school, 1, nchar(school)-5), school)) %>% #removes tag given on SR for making the NCAAT
  .[,-c(1, 9:16)] # removes columns for conference, home, and away team record
#add offense label to col names
colnames(basic_offense)= c(colnames(basic_offense[,1:7]), paste("offense_", colnames(basic_offense[,8:24]), sep = ""))

Sys.sleep(15)

#####basic defense table webscrape
basic_defense_url = "https://www.sports-reference.com/cbb/seasons/2023-opponent-stats.html"

basic_defense = read_html(basic_defense_url) %>%
  html_table(fill = TRUE) %>%
  .[[1]] %>% as.data.frame() %>%
  .[,-c(9,12,15,18,21)] %>% #remove empty columns
  row_to_names(row_number = 1, remove_row = T) %>% clean_names() %>% #use row 1 as col names
  filter(!g=='G') %>% filter(!g=="Overall") %>% #filter out 'header' rows scattered in the data frame
  mutate(school = ifelse(endsWith(school, "NCAA"), substr(school, 1, nchar(school)-5), school)) %>% #removes tag given on SR for making the NCAAT
  .[,-c(1, 9:16)] #removes columns for conference, home, and away team records
#add defense label to col names
colnames(basic_defense)= c(colnames(basic_defense[,1:7]), paste("defense_", colnames(basic_defense[,8:24]), sep = ""))

Sys.sleep(15)

#####advanced offense table webscrape
advanced_offense_url = "https://www.sports-reference.com/cbb/seasons/2023-advanced-school-stats.html"

advanced_offense = read_html(advanced_offense_url) %>%
  html_table(fill = TRUE) %>%
  .[[1]] %>% as.data.frame() %>%
  .[,-c(9,12,15,18,21)] %>% #remove empty columns
  row_to_names(row_number = 1, remove_row = T) %>% clean_names() %>% #use row 1 as col names
  filter(!g=='G') %>% filter(!g=="Overall") %>% #filter out 'header' rows scattered in the data frame
  mutate(school = ifelse(endsWith(school, "NCAA"), substr(school, 1, nchar(school)-5), school)) %>% #removes tag given on SR for making the NCAAT
  .[,-c(1, 9:16)] #removes columns for conference, home, and away team records
colnames(advanced_offense)= c(colnames(advanced_offense[,1:7]), paste("offense_", colnames(advanced_offense[,8:20]), sep = ""))

Sys.sleep(15)

#####advanced defense table webscrape
advanced_defense_url = "https://www.sports-reference.com/cbb/seasons/2023-advanced-opponent-stats.html"

advanced_defense = read_html(advanced_defense_url) %>%
  html_table(fill = TRUE) %>%
  .[[1]] %>% as.data.frame() %>%
  .[,-c(9,12,15,18,21)] %>% #remove empty columns
  row_to_names(row_number = 1, remove_row = T) %>% clean_names() %>% #use row 1 as col names
  filter(!g=='G') %>% filter(!g=="Overall") %>% #filter out 'header' rows scattered in the data frame
  mutate(school = ifelse(endsWith(school, "NCAA"), substr(school, 1, nchar(school)-5), school)) %>%
  .[,-c(1, 9:16)] #removes columns for conference, home, and away team records
colnames(advanced_defense)= c(colnames(advanced_defense[,1:7]), paste("defense_", colnames(advanced_defense[,8:20]), sep = ""))

Sys.sleep(15)

#put all tables into one 
SR_team_stats = left_join(basic_offense, basic_defense, by=c("school", "g", "w", "l", "w_l_percent", "srs", "sos")) %>%
  left_join(advanced_offense, by=c("school", "g", "w", "l", "w_l_percent", "srs", "sos")) %>%
  left_join(advanced_defense, by=c("school", "g", "w", "l", "w_l_percent", "srs", "sos"))

rm(advanced_offense, advanced_defense, basic_offense, basic_defense, advanced_offense_url, advanced_defense_url, basic_offense_url, basic_defense_url)


SR_team_stats = SR_team_stats %>%
  #columns into numeric
  mutate(across(2:67, as.numeric)) %>%
  #make all stats we want using other stats
  mutate(offense_x2p_percent = (offense_fg - offense_x3p)/(offense_fga - offense_x3pa),
                                         offense_drb_percent = 100 - defense_orb_percent) %>%
  #standardize school names
  mutate(school = clean_school_names(school))

#pull team groups that will be in GMC
our_schedule = kp_team_schedule(our_team, year=year) %>% select(opponent) %>% 
  mutate(opponent = clean_school_names(opponent)) %>% as.vector() %>% unlist() %>% unique()
#currently using coaches poll due to function not having ap poll at the moment
AP_top25 = espn_mbb_rankings() %>% filter(type=="usa", current>0) %>% select(team_location) %>% 
  mutate(team_location = clean_school_names(team_location)) %>% as.vector() %>% unlist() %>% unique()
NET_top50 = bart_tourney_sheets() %>% filter(net<=50) %>%
  mutate(team = gsub(" N4O", "", team),
         team = gsub(" F4O", "", team), 
         team = gsub(" St.", " State", team)) %>% 
  select(team) %>% mutate(team = clean_school_names(team)) %>% as.vector() %>% unlist() %>% unique()

#get graphic info for each group of teams
graphic_info_OS = cfbplotR::logo_ref %>%
  filter(school %in% our_schedule | school %in% opponentList | school == our_team) %>%
  mutate(school = clean_school_names(school))
graphic_info_AP = cfbplotR::logo_ref %>%
  filter(school %in% AP_top25 | school == our_team) %>%
  mutate(school = clean_school_names(school),
         school = ifelse(school=="UConn", "Connecticut", school))
graphic_info_NET = cfbplotR::logo_ref %>%
  filter(school %in% NET_top50 | school == our_team) %>%
  mutate(school = clean_school_names(school))

#change SR_team_stats school names to how they appear in graphic_info
SR_team_stats = SR_team_stats %>% 
  mutate(school= ifelse(school=="Albany (NY)", "Albany", school),
         school= ifelse(school=="Bowling Green State", "Bowling Green", school),
         school= ifelse(school=="Brigham Young", "BYU", school),
         school= ifelse(school=="Cal State Bakersfield", "CSU Bakersfield", school),
         school= ifelse(school=="Cal State Fullerton", "CSU Fullerton", school),
         school= ifelse(school=="Cal State Northridge", "CSU Northridge", school),
         school= ifelse(school=="Central Connecticut State", "Central Connecticut", school),
         school= ifelse(school=="Central Florida", "UCF", school),
         school= ifelse(school=="College of Charleston", "Charleston", school),
         school= ifelse(school=="Houston Christian", "Houston Baptist", school),
         school= ifelse(school=="Illinois-Chicago", "UIC", school),
         school= ifelse(school=="Louisiana State", "LSU", school),
         school= ifelse(school=="Loyola (IL)", "Loyola Chicago", school),
         school= ifelse(school=="Maryland-Baltimore County", "UMBC", school),
         school= ifelse(school=="Massachusetts-Lowell", "UMass Lowell", school),
         school= ifelse(school=="McNeese State", "McNeese", school),
         school= ifelse(school=="Nevada-Las Vegas", "UNLV", school),
         school= ifelse(school=="Nicholls State", "Nicholls", school),
         school= ifelse(school=="Purdue-Fort Wayne", "Purdue Fort Wayne", school),
         school= ifelse(school=="Saint Francis (PA)", "St. Francis (PA)", school),
         school= ifelse(school=="Saint Mary's (CA)", "Saint Mary's", school),
         school= ifelse(school=="Southern California", "USC", school),
         school= ifelse(school=="Southern Methodist", "SMU", school),
         school= ifelse(school=="St. Francis (NY)", "St. Francis (BKN)", school),
         school= ifelse(school=="Tennessee-Martin", "UT Martin", school),
         school= ifelse(school=="Texas A&M-Corpus Christi", "Texas A&M-CC", school),
         school= ifelse(school=="Texas-Rio Grande Valley", "UT Rio Grande Valley", school),
         school= ifelse(school=="Utah Tech", "Dixie State", school),
         school= ifelse(school=="Virginia Commonwealth", "VCU", school))

all_graphic_info = cfbplotR::logo_ref %>%
  mutate(school = clean_school_names(school))

#list of teams that need to be renamed
#currently there are 3 teams that are not in graphic_info, so they cant be renamed
#not_naming_consistent = left_join(SR_team_stats, all_graphic_info, by='school') %>% select(school, g, w, l, logo, type, color, alt_color, wordmark) %>% filter(is.na(type)) %>% select(school) %>% as.vector() %>% unlist() %>% unique()

#create data frame that will be used on the Graphical Metric Comparison page
GMC_OS = left_join(graphic_info_OS, SR_team_stats, by= 'school')
GMC_AP = left_join(graphic_info_AP, SR_team_stats, by= 'school')
GMC_NET = left_join(graphic_info_NET, SR_team_stats, by= 'school')

#find medians of all vars in GMC
GMC_medians = data.frame()
for(c in 2:ncol(SR_team_stats)){
  a = pull(SR_team_stats, var = c)
  GMC_medians[1,c-1] = median(a)
}
colnames(GMC_medians) <- colnames(SR_team_stats)[2:69]

#read headshot url table
headshot_urls_db = read.csv("data/headshot_url.csv") %>%
  mutate(URL = ifelse(URL == "", "https://a.espncdn.com/combiner/i?img=/i/headshots/nophoto.png&w=110&h=80&scale=crop", URL))

#write the data to file
write.csv(GMC_AP, file = "data/GMC_AP.csv", row.names = FALSE)
write.csv(GMC_OS, file = "data/GMC_OS.csv", row.names = FALSE)
write.csv(GMC_NET, file = "data/GMC_NET.csv", row.names = FALSE)
write.csv(GMC_medians, file = "data/GMC_medians.csv", row.names = FALSE)
write.csv(graphic_info_OS, file = "data/graphic_info_OS.csv", row.names = FALSE)

rm(SR_team_stats, AP_top25, NET_top50, our_schedule, a, c, all_graphic_info, graphic_info_AP, graphic_info_NET)

#write a function that will standardize a player's name
standardize_name = function(player_name){
  #remove periods
  name = gsub("\\.", "", player_name)
  #remove apostrophe
  name = gsub("'", "", name)
  #remove dashes
  name = gsub("-", "", name)
  #remove spaces and any third or more name element
  name = paste0(strsplit(name, split = " ")[[1]][1], strsplit(name, split = " ")[[1]][2])
  #upcase all letters
  name = toupper(name)
  return(name)
}
standardize_name = Vectorize(standardize_name)



###### data for PPT, OO splits, and Opp trends
###### looped by team

#empty df to populate
PPT_data = data.frame()
OO_splits_data = data.frame()
Opp_Trends_df = data.frame()

for(opp in opponentList){
  #SR data
  opponentSRurl = opponentSRurl_db %>% filter(opponent == opp) %>% .[[1,2]]
  SRopponentTables = read_html(opponentSRurl) %>% html_table()
  
  #creates a variable for opponent name when referring to kp functions
  opponent_kp = if(opp == "UCONN"){"Connecticut"} else{gsub(" State", " St.", opp)}
  
  #PPT Data coming from different sources
  SR_PPT = SRopponentTables[[1]] %>%
    select('#', Player, Pos) %>%
    mutate(player_join = standardize_name(Player)) %>%
    distinct() #there is one team that I found that has a duplicate player in this SR table
  SR2_PPT = SRopponentTables[[6]] %>%
    select(Player, G, GS, MPG=MP, twoPperc="2P%", threePperc="3P%", ftperc="FT%", ApG=AST, TOV, PPG=PTS) %>%
    mutate(player_join = standardize_name(Player))
  SR3_PPT = SRopponentTables[[14]] %>%
    select(Player, "USG%", eFGperc="eFG%", tsperc="TS%", threePAr="3PAr", "FTr", "AST%", "TOV%", 
           "PER", "OBPM", "DBPM", "BPM", "ORB%", "DRB%", "TRB%", "STL%", "BLK%") %>%
    mutate(player_join = standardize_name(Player))
  KP_PPT = kp_team_players(opponent_kp, year) %>%
    select(number, ht, wt, yr, poss_pct, f_dper40, f_cper40)
  headshots_PPT = headshot_urls_db %>% filter(Team == opp) %>%
    mutate(player_join = standardize_name(Player))
  
  #find player position
  kp_pos = kp_team_depth_chart(opponent_kp, year)
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
  lastGdate = kp_team_schedule(opponent_kp, year=year) %>% filter(is.na(pre_wp)) %>% arrange(desc(date)) %>% .[[1,18]]
  opponentGID = espn_mbb_scoreboard(lastGdate) %>%
    filter(toupper(home_team_location) == toupper(opp) | toupper(away_team_location) == toupper(opp)) %>% .[[1,6]]
  lastGstarters = espn_mbb_player_box(opponentGID) %>% 
                             mutate(athlete_jersey = as.double(athlete_jersey)) %>%
                             filter(starter==TRUE) %>%
                             filter(toupper(team_short_display_name) == toupper(gsub(" St.", " St", opponent_kp)) |
                                      toupper(team_short_display_name) == toupper(opp)) %>%
                             select("#" = athlete_jersey, starter)
  
  #join together all sources of info for PPT
  PPT_data_temp = SR_PPT %>%
    full_join(SR2_PPT, by = 'player_join') %>%
    full_join(SR3_PPT, by = 'player_join') %>%
    full_join(KP_PPT, by = c('#' = 'number')) %>%
    full_join(headshots_PPT, by = 'player_join') %>%
    full_join(KP2_PPT, by = '#') %>%
    left_join(lastGstarters, by = '#') %>%
    mutate('All Players' = 1,
           "Guards" = ifelse(PG+SG>.7, 1, 0),
           "Bigs" = ifelse(C+PF>.8 & C>0 , 1, 0),
           "Wings" = ifelse(Guards+Bigs==0, 1, 0),
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
    select("#", player_join, URL, first, last, Team, 
           Class=yr, Pos, Height=ht, Weight=wt, 
           GP=G, GS, MPG, "Poss%"=poss_pct, "USG%",
           PG, SG, SF, PF, C,
           PER, OBPM, DBPM, BPM,
           PPG, "FD/40"=f_dper40,
           "2P%"=twoPperc, "3P%"=threePperc, "3PAr"=threePAr, "eFG%"=eFGperc, "TS%"=tsperc, "FT%"=ftperc, FTr,
           ApG, "AST:TO", "AST%", "TOV%",
           "ORB%", "DRB%", "TRB%",
           "STL%", "BLK%", "FC/40"=f_cper40,
           "All Players", Guards, Wings, Bigs, Starters, Lefties)
  
  PPT_data = rbind(PPT_data, PPT_data_temp)

  
  
  #OO Graph
  #find the conference of our opp
  #right now, conf tournament games are not being included as conf games
  opp_conf = kp_program_ratings() %>%
    #manually change the abbreviation of certain conferences
    mutate(conf = ifelse(conf == "Amer", "AAC", conf)) %>%
    filter(team == opponent_kp) %>%
    .[[1,3]]
  
  #opponent espn id of opp
  opp_id = espn_mbb_teams() %>%
    filter(toupper(team) == toupper(opp)) %>%
    select(team_id) %>%
    .[[1,1]]
  
  #find the spread in every game our opp has played this season
  opp_spreads = {
    df <- load_mbb_schedule() %>% 
      filter(home_id == opp_id | away_id == opp_id,
             status_type_completed == TRUE) %>%
      select(game_id = id, date, home_short_display_name, away_short_display_name, home_id, away_id) %>%
      arrange(date) %>%
      mutate(spread = NA,
             #turn date from a character to a date column and adjust the time zone so the correct date is reflected
             date = gsub("T", " ", date),
             date = gsub("Z", ":00", date),
             date = strptime(date, "%Y-%m-%d %H:%M:%S") - 6*60*60,
             game_date = gsub("-", "", substr(date, 1, 10)))
    
    for(r in 1:nrow(df)) {
      df[r,"spread"] = espn_mbb_betting(df$game_id[r])[[1]] %>% 
        filter(provider_name == "consensus") %>% .[[1,"spread"]] %>% as.integer()
      Sys.sleep(3)}
    
    df <- df %>% 
      mutate(spread = ifelse(home_id == opp_id, spread, spread*-1),
             game_date = as.double(game_date)) %>%
      select(game_date, spread)
    
  }
  
  #find other stats used in OO Trend table and format column names to match select input
  opp_gp_stats = kp_gameplan(opponent_kp, year)[[1]] %>%
    select(game_date, Pace=pace, Offensive_Efficency=off_eff, Defensive_Efficency=def_eff, 
           Points_Scored=team_score, Points_Allowed=opponent_score, 
           Offensive_3PAr=off_fg_3a_pct, 'Offensive_3Ppct'=off_fg_3_pct, 'Offensive_TOpct'=off_to_pct, 
           'Offensive_OREBpct'=off_or_pct, Offensive_FTr=off_ftr, 
           Defensive_3PAr=def_fg_3a_pct, 'Defensive_3Ppct'=def_fg_3_pct, 'Defensive_TOpct'=def_to_pct, 
           'Defensive_OREBpct'=def_or_pct, Defensive_FTr=def_ftr)
  
  
  #pull trending stats and other stats used for formatting
  opp_game_stats = kp_team_schedule(opponent_kp, year) %>%
    select(opponent, game_date, location, conference_game) %>%
    full_join(kp_opptracker(opponent_kp, year), by = c("opponent", "game_date")) %>%
    mutate(opponent = clean_school_names(opponent)) %>%
    select(date, game_date, location, opponent, wl, team_score, opponent_score, conference_game) %>%
    mutate(Winning_Margin = ifelse(is.na(team_score), NA, team_score - opponent_score),
           #create a unique identifier for each game since teams can be played multiple times
           game_code = paste(opponent, game_date),
           #if not a true home game, consider it a road game
           location = ifelse(location=="Home", "Home", "Away")) %>%
    #find NET rankings for teams our opp has played
    left_join(bart_tourney_sheets() %>%
                mutate(team = gsub(" N4O", "", team),
                       team = gsub(" F4O", "", team), 
                       team = gsub(" St.", " State", team),
                       team = ifelse(team=="McNeese State", "McNeese St.", team)) %>%
                mutate(team = clean_school_names(team)) %>%
                select(opponent=team, net)) %>%
    mutate(net_rk = ifelse(net <= 100, 
                           ifelse(net <= 50, "NET Top 50", "NET Top 100"), 
                           "Other"),
           conference_game = ifelse(conference_game==TRUE, opp_conf, "OOC")) %>%
    #create ATS Margin
    left_join(opp_spreads, by="game_date") %>%
    mutate(ATS_Margin = Winning_Margin + spread) %>%
    #join in other used stats
    left_join(opp_gp_stats, by="game_date")
  
  #create separate data frames to split the data based on the input so that we can calculate mean in each split
  for(stat in OO_TrendStat_List){
    opp_game_stats_recency5 = opp_game_stats %>% filter(!is.na(team_score)) %>%
      .[(nrow(opp_game_stats %>% filter(!is.na(team_score)))-4):nrow(opp_game_stats %>% filter(!is.na(team_score))),] %>%
      mutate(recency5 = "Last 5 Games",
             Team = opp,
             split = 'recency5',
             variable = stat) %>%
      select(Team, split, variable, game_code, stat) %>%
      rename(stats = 5)
    
    opp_game_stats_recency10 = opp_game_stats %>% filter(!is.na(team_score)) %>%
      .[(nrow(opp_game_stats %>% filter(!is.na(team_score)))-9):nrow(opp_game_stats %>% filter(!is.na(team_score))),] %>%
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


write.csv(PPT_data %>% select(-player_join), file = "data/PPT_data.csv", row.names = FALSE)
write.csv(OO_splits_data, file = "data/OO_splits_data.csv", row.names = FALSE)
write.csv(Opp_Trends_df, file = "data/Opp_Trends_df.csv", row.names = FALSE)

rm(df, headshots_PPT, kp_pos, KP_PPT, KP2_PPT, lastGstarters, OO_trend_per_stat, opp_game_stats, opp_game_stats_away,
   opp_game_stats_conf, opp_game_stats_home, opp_game_stats_losses, opp_game_stats_net100, opp_game_stats_net50, 
   opp_game_stats_recency10, opp_game_stats_recency5, opp_game_stats_wins, opp_gp_stats, opp_spreads, Opp_Trends_df_team, pos_c,
   pos_pf, pos_pg, pos_sf, pos_sg, PPT_data_temp, SR_PPT, SR2_PPT, SR3_PPT, lastGdate, opp, opp_conf, opp_id,
   opponentGID, opponentSRurl, r, stat, opponent_kp)

###### player comp data
player_totals_url = 'https://www.sports-reference.com/cbb/play-index/psl_finder.cgi?request=1&match=single&year_min=2023&year_max=2023&conf_id=&school_id=&class_is_fr=Y&class_is_so=Y&class_is_jr=Y&class_is_sr=Y&pos_is_g=Y&pos_is_f=Y&pos_is_c=Y&games_type=A&qual=&c1stat=&c1comp=gt&c1val=&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=pts&order_by_asc=&offset='
offset = seq(0, 6000, by = 100)

player_totals = data.frame()
for(num in offset){
  pt_url = paste0(player_totals_url, num)
  raw_pt = read_html(pt_url) %>% html_table()
  if(length(raw_pt) == 0){break}
  player_totals_partial = raw_pt %>% .[[1]] %>% as.data.frame() %>%
    row_to_names(row_number = 1, remove_row = T) %>% clean_names() %>%
    filter(rk != "") %>% filter(rk != "Rk")
  player_totals = rbind(player_totals, player_totals_partial)
  Sys.sleep(30)
}
player_totals = player_totals %>% unique() %>% select(-rk)

#scrape ncaa box score advanced
player_advanced_url = 'https://www.sports-reference.com/cbb/play-index/psl_finder.cgi?request=1&match=single&year_min=2023&year_max=2023&conf_id=&school_id=&class_is_fr=Y&class_is_so=Y&class_is_jr=Y&class_is_sr=Y&pos_is_g=Y&pos_is_f=Y&pos_is_c=Y&games_type=A&qual=&c1stat=&c1comp=gt&c1val=&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=per&order_by_asc=&offset='

player_advanced = data.frame()
for(num in offset){
  pa_url = paste0(player_advanced_url, num)
  raw_pa = read_html(pa_url) %>% html_table()
  if(length(raw_pa) == 0){break}
  player_advanced_partial = raw_pa %>% .[[1]] %>% as.data.frame() %>%
    row_to_names(row_number = 1, remove_row = T) %>% clean_names() %>%
    filter(rk != "") %>% filter(rk != "Rk")
  player_advanced = rbind(player_advanced, player_advanced_partial)
  Sys.sleep(30)
}
player_advanced = player_advanced %>% unique() %>% select(-rk)

ncaa_player_stats = player_totals %>% full_join(player_advanced) %>%
  mutate_at(vars('g':'bpm'), as.numeric) %>%
  select(-c('season', 'conf', 'p_prod')) %>%
  rename(team = school)

#load in a saved dataset of nba total and advanced stats
nba_player_stats = read.csv("data/nba_stats.csv") %>%
  clean_names() %>% 
  mutate(class = 'NBA') %>%
  #match names to ncaa table
  select(player, class, pos, team, g, mp, fg, fga, fg_percent=fg_2, x2p, x2pa, x3p, x3pa, x3p_percent=x3p_2, ft, fta, ft_percent=ft_2,
         orb, drb, trb, ast, stl, blk, tov, pf, pts, per, ts_percent=ts, e_fg_percent=e_fg, orb_percent=orb_2, drb_percent=drb_2, 
         trb_percent=trb_2, ast_percent=ast_2, stl_percent=stl_2, blk_percent=blk_2, tov_percent=tov_2, usg_percent=usg, o_rtg,
         d_rtg, ows, dws, ws, obpm, dbpm, bpm)

#join nba and ncaa tables together
all_player_stats = rbind(ncaa_player_stats, nba_player_stats) %>%
  filter((class=="NBA" & g>=10 & mp>=400) | (class!="NBA" & g>=7 & mp>=150)) %>%
  mutate(x3p_percent = ifelse(is.na(x3p_percent), 0, x3p_percent)) %>%
  arrange(desc(mp)) %>%
  mutate(player_join = standardize_name(player))

#in the future, this is where I will add a part to change some listed player positions

#separate positions into three groups
guard_stats = all_player_stats %>% filter(pos=='G' | pos=='G-F')
wing_stats = all_player_stats %>% filter(pos=='F' | pos=='F-G')
big_stats = all_player_stats %>% filter(pos=='C' | pos=='C-F' | pos=='F-C')

#calculate play styles and percentiles for each league and each position
nba_guard_styles = guard_stats %>% 
  filter(class=="NBA") %>%
  mutate(x3_p_s = (x3p * 3)/pts + x3p_percent + (x3pa/mp),
         slasher = (x2pa/fga) + (fta/fga),
         ball_dominant = usg_percent,
         playmaker = ast_percent - .5*(usg_percent),
         limit_to = tov/mp - .5*(ast/mp),
         dbpm_pct = percent_rank(dbpm),
         perim_defender = dbpm_pct/2 + stl_percent/10,
         oreb = orb_percent,
         dreb = drb_percent,
         sim_1=NA, sim_1_url=NA, sim_2=NA, sim_3=NA, sim_4=NA, sim_5=NA) %>%
  select(player, player_join, class, pos, team, x3_p_s, slasher, ball_dominant, playmaker, limit_to, perim_defender, oreb, dreb, 
         sim_1, sim_1_url, sim_2, sim_3, sim_4, sim_5) %>%
  mutate_if(is.numeric, percent_rank) %>%
  mutate(limit_to = (limit_to - 1)* -1)

ncaa_guard_styles = guard_stats %>% 
  filter(class!="NBA") %>%
  mutate(x3_p_s = (x3p * 3)/pts + x3p_percent + (x3pa/mp),
         slasher = (x2pa/fga) + (fta/fga),
         ball_dominant = usg_percent,
         playmaker = ast_percent - .5*(usg_percent),
         limit_to = tov/mp - .5*(ast/mp),
         dbpm_pct = percent_rank(dbpm),
         perim_defender = dbpm_pct/2 + stl_percent/10,
         oreb = orb_percent,
         dreb = drb_percent,
         sim_1=NA, sim_1_url = NA, sim_2=NA, sim_3=NA, sim_4=NA, sim_5=NA) %>%
  select(player, player_join, class, pos, team, x3_p_s, slasher, ball_dominant, playmaker, limit_to, perim_defender, oreb, dreb,
         sim_1, sim_1_url, sim_2, sim_3, sim_4, sim_5) %>%
  mutate_if(is.numeric, percent_rank) %>%
  mutate(limit_to = (limit_to - 1)* -1) %>%
  filter(team %in% opponentList | team == "Connecticut" )

nba_wing_styles = wing_stats %>% 
  filter(class=="NBA") %>%
  mutate(x3_p_s = (x3p * 3)/pts + x3p_percent + (x3pa/mp),
         slasher = (x2pa/fga) + (fta/fga),
         ball_dominant = usg_percent,
         playmaker = ast_percent - .5*(usg_percent),
         limit_to = tov/mp - .5*(ast/mp),
         dbpm_pct = percent_rank(dbpm),
         perim_defender = dbpm_pct/2 + stl_percent/10,
         rim_protector = dbpm_pct/2 + blk_percent/10,
         oreb = orb_percent,
         dreb = drb_percent,
         sim_1=NA, sim_1_url = NA, sim_2=NA, sim_3=NA, sim_4=NA, sim_5=NA) %>%
  select(player, player_join, class, pos, team, x3_p_s, slasher, ball_dominant, playmaker, limit_to, perim_defender, rim_protector, oreb, dreb,
         sim_1, sim_1_url, sim_2, sim_3, sim_4, sim_5) %>%
  mutate_if(is.numeric, percent_rank) %>%
  mutate(limit_to = (limit_to - 1)* -1)

ncaa_wing_styles = wing_stats %>% 
  filter(class!="NBA") %>%
  mutate(x3_p_s = (x3p * 3)/pts + x3p_percent + (x3pa/mp),
         slasher = (x2pa/fga) + (fta/fga),
         ball_dominant = usg_percent,
         playmaker = ast_percent - .5*(usg_percent),
         limit_to = tov/mp - .5*(ast/mp),
         dbpm_pct = percent_rank(dbpm),
         perim_defender = dbpm_pct/2 + stl_percent/10,
         rim_protector = dbpm_pct/2 + blk_percent/10,
         oreb = orb_percent,
         dreb = drb_percent,
         sim_1=NA, sim_1_url = NA, sim_2=NA, sim_3=NA, sim_4=NA, sim_5=NA) %>%
  select(player, player_join, class, pos, team, x3_p_s, slasher, ball_dominant, playmaker, limit_to, perim_defender, rim_protector, oreb, dreb,
         sim_1, sim_1_url, sim_2, sim_3, sim_4, sim_5) %>%
  mutate_if(is.numeric, percent_rank) %>%
  mutate(limit_to = (limit_to - 1)* -1) %>%
  filter(team %in% opponentList | team == "Connecticut" )

nba_big_styles = big_stats %>% 
  filter(class=="NBA") %>%
  mutate(stretch_big = (x3p * 3)/pts + x3p_percent + (x3pa/mp),
         draws_fouls = (fta/fga),
         passing_big = ast_percent,
         limit_to = tov/mp - .5*(ast/mp),
         dbpm_pct = percent_rank(dbpm),
         rim_protector = dbpm_pct/2 + blk_percent/10,
         oreb = orb_percent,
         dreb = drb_percent,
         sim_1=NA, sim_1_url = NA, sim_2=NA, sim_3=NA, sim_4=NA, sim_5=NA) %>%
  select(player, player_join, class, pos, team, stretch_big, draws_fouls, passing_big, limit_to, rim_protector, oreb, dreb,
         sim_1, sim_1_url, sim_2, sim_3, sim_4, sim_5) %>%
  mutate_if(is.numeric, percent_rank) %>%
  mutate(limit_to = (limit_to - 1)* -1)

ncaa_big_styles = big_stats %>% 
  filter(class!="NBA") %>%
  mutate(stretch_big = (x3p * 3)/pts + x3p_percent + (x3pa/mp),
         draws_fouls = (fta/fga),
         passing_big = ast_percent,
         limit_to = tov/mp - .5*(ast/mp),
         dbpm_pct = percent_rank(dbpm),
         rim_protector = dbpm_pct/2 + blk_percent/10,
         oreb = orb_percent,
         dreb = drb_percent,
         sim_1=NA, sim_1_url = NA, sim_2=NA, sim_3=NA, sim_4=NA, sim_5=NA) %>%
  select(player, player_join, class, pos, team, stretch_big, draws_fouls, passing_big, limit_to, rim_protector, oreb, dreb,
         sim_1, sim_1_url, sim_2, sim_3, sim_4, sim_5) %>%
  mutate_if(is.numeric, percent_rank) %>%
  mutate(limit_to = (limit_to - 1)* -1) %>%
  filter(team %in% opponentList | team == "Connecticut")

styles_list = list(list(ncaa_guard_styles, nba_guard_styles), 
                   list(ncaa_wing_styles, nba_wing_styles), 
                   list(ncaa_big_styles, nba_big_styles))

#maybe do PCA first

#for the desired ncaa athletes, find their most alike nba comparison
sim_list = list(); z=1
for(a in styles_list){
  n_ncaa_players = nrow(a[[1]])
  for(player in 1:n_ncaa_players){
    similarity_vec = dist(rbind(a[[1]][player,] %>% select(-c(player, player_join, class, pos, team, sim_1, sim_1_url, sim_2, sim_3, sim_4, sim_5)) , 
                                a[[2]] %>% select(-c(player, player_join, class, pos, team, sim_1, sim_1_url, sim_2, sim_3, sim_4, sim_5))))[1:nrow(a[[2]])]
    top_5 = head(order(similarity_vec), 5)
    top_5 = a[[2]][top_5, 'player']
    a[[1]][player, 'sim_1'] = top_5[1]
    sim_1_url = hoopR::nba_playerheadshot(player_id = 
                                            (hoopR::nba_commonallplayers()$CommonAllPlayers %>% as.data.frame() %>% 
                                               filter(DISPLAY_FIRST_LAST == top_5[1] %>% unlist()) %>% 
                                               select(PERSON_ID) %>% unlist() %>% as.character()))
    Sys.sleep(10)
    a[[1]][player, 'sim_1_url'] = sim_1_url
    a[[1]][player, 'sim_2'] = top_5[2]
    a[[1]][player, 'sim_3'] = top_5[3]
    a[[1]][player, 'sim_4'] = top_5[4]
    a[[1]][player, 'sim_5'] = top_5[5]
  }
  sim_list[[z]] = a
  z = z+1
}

ncaa_guard_sim = sim_list[[1]][[1]] %>% left_join(PPT_data %>% select(player_join, URL, '#'), by = "player_join") %>% select(-player_join)
ncaa_wing_sim = sim_list[[2]][[1]] %>% left_join(PPT_data %>% select(player_join, URL, '#'), by = "player_join") %>% select(-player_join)
ncaa_big_sim = sim_list[[3]][[1]] %>% left_join(PPT_data %>% select(player_join, URL, '#'), by = "player_join") %>% select(-player_join)


write.csv(ncaa_guard_sim, file = "data/ncaa_guard_sim.csv", row.names = FALSE)
write.csv(ncaa_wing_sim, file = "data/ncaa_wing_sim.csv", row.names = FALSE)
write.csv(ncaa_big_sim, file = "data/ncaa_big_sim.csv", row.names = FALSE)

write.csv(styles_list[[1]][[2]] %>% select(-c(player_join, sim_1, sim_1_url, sim_2, sim_3, sim_4, sim_5)), file = "data/nba_guard_style.csv", row.names = FALSE)
write.csv(styles_list[[2]][[2]] %>% select(-c(player_join, sim_1, sim_1_url, sim_2, sim_3, sim_4, sim_5)), file = "data/nba_wing_style.csv", row.names = FALSE)
write.csv(styles_list[[3]][[2]] %>% select(-c(player_join, sim_1, sim_1_url, sim_2, sim_3, sim_4, sim_5)), file = "data/nba_big_style.csv", row.names = FALSE)

rm(a, all_player_stats, big_stats, guard_stats, nba_big_styles, nba_guard_styles, nba_player_stats, nba_wing_styles, ncaa_big_styles,
   ncaa_guard_styles, ncaa_player_stats, ncaa_wing_styles, raw_pa, raw_pt, player_totals_partial, player_advanced_partial, 
   num, offset, pa_url, pt_url, player_advanced_url, player_totals_url, player_advanced, player_totals, wing_stats, n_ncaa_players,
   player, similarity_vec, top_5, z, styles_list)


###### pbp data
scrape_pbp <- function(game_id = 6048478) {
  json <-
    paste0("https://data.ncaa.com/casablanca/game/",
           game_id,
           "/pbp.json") |>
    jsonlite::fromJSON(flatten = T)
  teams <- json |>
    purrr::pluck("meta") |>
    purrr::pluck("teams") |>
    dplyr::select(-c(color)) |>
    tidyr::pivot_wider(
      names_from = homeTeam,
      values_from = c(id, shortName, seoName, sixCharAbbr, shortName, nickName)
    ) |>
    dplyr::rename_with(\(x) gsub("_true", "_home", x), dplyr::ends_with("_true")) |>
    dplyr::rename_with(\(x) gsub("_false", "_visitor", x), dplyr::ends_with("_false"))
  pbp <- json |>
    purrr::pluck("periods") |>
    tidyr::unnest(playStats) |>
    dplyr::mutate(game_id = game_id) |>
    dplyr::bind_cols(teams)
  return(pbp)
}
extract_all_players <- function(text) {
  # extract pos player (first player on play)
  pos_1 <-
    stringr::str_extract(text, "^(.*) (misses|defensive|offensive|makes|turnover)")
  # extract def player (first player on play)
  def_1 <- stringr::str_extract(text, "^(.*) blocks")
  # extract pos player (second player on play)
  pos_2 <- stringr::str_extract(text, "\\(.* assists|blocks .*'s")
  # extract def player (second player on team)
  def_2 <- stringr::str_extract(text, "\\(.* draws|\\(.* steals")
  pos_1 <-
    trimws(gsub('(misses|defensive|offensive|makes|turnover)', '', pos_1))
  pos_2 <- trimws(gsub("\\(| assists|blocks|'s", '', pos_2))
  def_1 <- trimws(gsub("blocks", '', def_1))
  def_2 <-
    trimws(gsub("\\(|draws|steals|bad pass\\)|lost ball\\)", '', def_2))
  return(data.frame(pos_1,
                    pos_2,
                    def_1,
                    def_2))
}
parse_pbp <- function(pbp_text, type = "mens") {
  stopifnot(type %in% c("mens","womens"))
  pbp <- pbp_text |>
    # fill in score column (is left blank when score is unchanged)
    dplyr::mutate(score = dplyr::case_when(score == "" ~ NA_character_,
                                           T ~ score)) |>
    tidyr::fill(score, .direction = "down") |>
    dplyr::mutate(score = dplyr::case_when(is.na(score) ~ "0-0",
                                           T ~ score)) |>
    # generate time columns
    tidyr::separate(time,
                    c("minute", "second"),
                    fill = "right") |>
    dplyr::ungroup() |>
    dplyr::mutate(
      dplyr::across(c(minute, second),
                    \(x) as.numeric(x)),
      time = lubridate::period(minute = minute, second = second),
      play_id = paste0(game_id, "_", dplyr::row_number()),
      events = dplyr::case_when(homeText == '' ~ visitorText,
                                visitorText == '' ~ homeText,
                                T ~ '')
    )
  if((type == "mens" & max(pbp_text$periodNumber) < 2) | (type == "womens" & max(pbp_text$periodNumber) < 4)) {
    cli::cli_alert_warning("Incomplete game provided! Unable to provide lineups for {pbp$game_id[1]}, returning empty df...")
    pbp_parsed <- data.frame()
  } else
    # identify if this is v1 or v2 or v3
    if (any(grepl('Subbing in for ', pbp$events)) & any(grepl(",", pbp$events))) {
      # v3
      pbp_parsed <- pbp |>
        # check which plays get subbed in, which get subbed out
        dplyr::mutate(
          subbed_in_home = grepl('Subbing in for ', homeText),
          subbed_out_home = grepl('Subbing out for ', homeText),
          player_on_play_home = gsub(
            "((.*)'s |(.*)- |(.*)by )",
            "",
            homeText),
          player_on_play_home = gsub("Subbing in for ", "", player_on_play_home),
          player_on_play_home = gsub("Subbing out for ", "", player_on_play_home),
          player_on_play_home = dplyr::case_when(grepl("\\[", player_on_play_home) ~ NA_character_,
                                                 grepl("Foul on ", player_on_play_home) ~ NA_character_,
                                                 grepl("End of ", player_on_play_home) ~ NA_character_,
                                                 T ~ player_on_play_home),
          player_on_play_home = gsub("\\(.*\\)", "", player_on_play_home),
          player_on_play_home = dplyr::case_when(
            player_on_play_home == "second time out." ~ "",
            T ~ player_on_play_home
          ),
          player_on_play_home = trimws(player_on_play_home),
          player_on_play_home = gsub(dplyr::first(shortName_home), "", player_on_play_home),
          player_on_play_home = dplyr::case_when(
            !grepl(",", player_on_play_home) ~ NA_character_,
            player_on_play_home == '' ~ NA_character_,
            player_on_play_home == "'s" ~ NA_character_,
            player_on_play_home == "Turnover" ~ NA_character_,
            T ~ player_on_play_home
          ),
          on_court_flag_home = dplyr::case_when(subbed_in_home ~ 1,
                                                subbed_out_home ~ 0,
                                                T ~ NA_real_),
          subbed_in_visitor = grepl('Subbing in for ', visitorText),
          subbed_out_visitor = grepl('Subbing out for ', visitorText),
          player_on_play_visitor = gsub(
            "((.*)'s |(.*)- |(.*)by )",
            "",
            visitorText),
          player_on_play_visitor = gsub("Subbing in for ", "", player_on_play_visitor),
          player_on_play_visitor = gsub("Subbing out for ", "", player_on_play_visitor),
          player_on_play_visitor = dplyr::case_when(grepl("\\[", player_on_play_visitor) ~ NA_character_,
                                                 grepl("Foul on ", player_on_play_visitor) ~ NA_character_,
                                                 grepl("End of ", player_on_play_visitor) ~ NA_character_,
                                                 T ~ player_on_play_visitor),
          player_on_play_visitor = gsub("\\(.*\\)", "", player_on_play_visitor),
          player_on_play_visitor = dplyr::case_when(
            player_on_play_visitor == "second time out." ~ "",
            T ~ player_on_play_visitor
          ),
          player_on_play_visitor = trimws(player_on_play_visitor),
          player_on_play_visitor = gsub(
            dplyr::first(shortName_visitor),
            "",
            player_on_play_visitor
          ),
          player_on_play_visitor = dplyr::case_when(
            !grepl(",", player_on_play_visitor) ~ NA_character_,
            player_on_play_visitor == '' ~ NA_character_,
            player_on_play_visitor == "'s" ~ NA_character_,
            player_on_play_visitor == "Turnover" ~ NA_character_,
            T ~ player_on_play_visitor
          ),
          on_court_flag_visitor = dplyr::case_when(subbed_in_visitor ~ 1,
                                                   subbed_out_visitor ~ 0,
                                                   T ~ NA_real_),
        ) |>
        tidyr::separate_wider_delim(cols = player_on_play_home, delim = ", ", names = c("home_last", "home_first")) |>
        tidyr::separate_wider_delim(cols = player_on_play_visitor, delim = ", ", names = c("vis_last", "vis_first")) |>
        dplyr::mutate(player_on_play_home = paste0(home_first, " ", home_last),
                      player_on_play_home = ifelse(player_on_play_home == "NA NA", NA, player_on_play_home),
                      player_on_play_visitor = paste0(vis_first, " ", vis_last),
                      player_on_play_visitor = ifelse(player_on_play_visitor == "NA NA", NA, player_on_play_visitor)) |>
        dplyr::select(-home_first, -home_last, -vis_first, -vis_last) |>
        dplyr::filter(!is.na(player_on_play_visitor) |
                        !is.na(player_on_play_home)) |>
        dplyr::group_by(time,player_on_play_home) |>
        dplyr::filter(!(sum(subbed_in_home) == sum(subbed_out_home) & sum(subbed_in_home) > 0)) |>
        dplyr::ungroup() |>
        dplyr::group_by(time,player_on_play_visitor) |>
        dplyr::filter(!(sum(subbed_in_visitor) == sum(subbed_out_home) & sum(subbed_in_home) > 0)) |>
        dplyr::ungroup() |>
        dplyr::group_by(periodNumber) |>
        dplyr::group_modify( ~ (\(a) {
          cbind(
            a,
            a |>
              # create a matrix of all players mentioned in the pbp at any point
              tidyr::pivot_wider(
                names_from = player_on_play_home,
                values_from = on_court_flag_home,
                names_prefix = 'x_',
                names_repair = "check_unique"
              ) |>
              #' whenever a player is subbed in, they have a 1 in that row and a 0
              #' in the row before. whenever a player is subbed out, they have a 0
              #' in that row and a 1 in the row before
              dplyr::mutate(
                dplyr::across(
                  dplyr::starts_with('x_'),
                  ~ dplyr::case_when(dplyr::lead(.) == 0 ~ 1,
                                     dplyr::lead(.) == 1 ~ 0,
                                     T ~
                                       .)
                )
              ) |>
              #' we then cascade the 1s and 0s up and down to create map of who is
              #' in the game at any given time
              tidyr::fill(c(dplyr::starts_with('x_')), .direction = "down") |>
              tidyr::fill(c(dplyr::starts_with('x_')), .direction = "up") |>
              dplyr::mutate(
                dplyr::across(
                  dplyr::starts_with('x_'),
                  ~ dplyr::case_when(is.na(.x) ~ 1,
                                     T ~ .x)
                )
              ) |>
              # finally we collapse the values down into a string of who is on court
              (\(d) {
                d |>
                  dplyr::select(dplyr::starts_with('x_')) |>
                  dplyr::select(-dplyr::any_of('x_NA')) |>
                  dplyr::mutate(row = dplyr::row_number()) |>
                  tidyr::pivot_longer(dplyr::starts_with("x_")) |>
                  dplyr::group_by(row) |>
                  dplyr::summarise(on_court_home = paste0(name[value == 1],collapse=';'))
              })()
          )
        })(.x)) |>
        dplyr::group_modify( ~ (\(a) {
          cbind(
            a,
            a |>
              # create a matrix of all players mentioned in the pbp at any point
              tidyr::pivot_wider(
                names_from = player_on_play_visitor,
                values_from = on_court_flag_visitor,
                names_prefix = 'x_',
                names_repair = "check_unique"
              ) |>
              #' whenever a player is subbed in, they have a 1 in that row and a 0
              #' in the row before. whenever a player is subbed out, they have a 0
              #' in that row and a 1 in the row before
              dplyr::mutate(
                dplyr::across(
                  dplyr::starts_with('x_'),
                  ~ dplyr::case_when(dplyr::lead(.) == 0 ~ 1,
                                     dplyr::lead(.) == 1 ~ 0,
                                     T ~
                                       .)
                )
              ) |>
              #' we then cascade the 1s and 0s up and down to create map of who is
              #' in the game at any given time
              tidyr::fill(c(dplyr::starts_with('x_')), .direction = "down") |>
              tidyr::fill(c(dplyr::starts_with('x_')), .direction = "up") |>
              dplyr::mutate(
                dplyr::across(
                  dplyr::starts_with('x_'),
                  ~ dplyr::case_when(is.na(.x) ~ 1,
                                     T ~ .x)
                )
              ) |>
              # finally we collapse the values down into a string of who is on court
              (\(d) {
                d |>
                  dplyr::select(dplyr::starts_with('x_')) |>
                  dplyr::select(-dplyr::any_of('x_NA')) |>
                  dplyr::mutate(row_1 = dplyr::row_number()) |>
                  tidyr::pivot_longer(dplyr::starts_with("x_")) |>
                  dplyr::group_by(row_1) |>
                  dplyr::summarise(on_court_visitor = paste0(name[value == 1],collapse=';'))
              })()
          )
        })(.x)) |>
        dplyr::ungroup() |>
        # find out when lineups change
        dplyr::mutate(stint = cumsum(
          subbed_in_home + subbed_in_visitor
        )) |>
        dplyr::group_by(stint, on_court_home, on_court_visitor, periodNumber) |>
        dplyr::summarise(
          stint_start = dplyr::first(time),
          stint_end = dplyr::last(time),
          .groups = "drop"
        ) |>
        dplyr::filter(stint_start != stint_end) |>
        dplyr::ungroup() |>
        dplyr::arrange(periodNumber,-stint_start) |>
        dplyr::group_by(periodNumber) |>
        dplyr::mutate(
          stint_start = dplyr::case_when(
            dplyr::row_number() == 1 &
              periodNumber %in% c(1, 2) &
              type == "mens" ~ lubridate::as.period("20M 0S"),
            dplyr::row_number() == 1 &
              periodNumber %in% c(1, 2, 3, 4) &
              type == "womens" ~ lubridate::as.period("10M 0S"),
            (
              dplyr::row_number() == 1 &
                periodNumber >= 3 &
                type == "mens"
            ) |
              (
                dplyr::row_number() == 1 &
                  periodNumber >= 5 &
                  type == "womens"
              ) ~ lubridate::as.period("5M 0S"),
            T ~ stint_start
          ),
          stint_end = dplyr::case_when(
            dplyr::row_number() == dplyr::n() ~ lubridate::as.period("0M 0S"),
            T ~ stint_end
          )
        ) |>
        dplyr::select(-c(stint)) |>
        #remove x_team from listed players in on_court
        dplyr::mutate(
          on_court_home = gsub(";x_team|x_team;", "", on_court_home),
          on_court_visitor = gsub(";x_team|x_team;", "", on_court_visitor),
          on_court_home = gsub(';x_ \\[.*\\]', "", on_court_home),
          on_court_home = gsub(';x_ \\[.*\\]', "", on_court_home),
          on_court_visitor = gsub(';x_ \\[.*\\]', "", on_court_visitor),
          on_court_visitor = gsub(';x_ \\[.*\\]', "", on_court_visitor),
        ) |>
        # omit bad sub data
        dplyr::mutate(dplyr::across(
          dplyr::starts_with("on_court"),
          \(x) dplyr::case_when(stringr::str_count(x, ';') == 4 ~ x,
                                T ~ NA_character_)
        )) |>
        data.frame()
    } else if (any(grepl('Subbing in for ', pbp$events))) {
      # v2
      pbp_parsed <- pbp |>
        # check which plays get subbed in, which get subbed out
        dplyr::mutate(
          subbed_in_home = grepl('Subbing in for ', homeText),
          subbed_out_home = grepl('Subbing out for ', homeText),
          player_on_play_home = gsub(
            "((.*)'s |-| by )",
            "",
            homeText),
          player_on_play_home = gsub("Subbing in for ", "", player_on_play_home),
          player_on_play_home = gsub("Subbing out for ", "", player_on_play_home),
          player_on_play_home = dplyr::case_when(grepl("\\[", player_on_play_home) ~ NA_character_,
                                                 grepl("Foul on ", player_on_play_home) ~ NA_character_,
                                                 grepl("End of ", player_on_play_home) ~ NA_character_,
                                                 T ~ player_on_play_home),
          player_on_play_home = gsub("\\(.*\\)", "", player_on_play_home),
          player_on_play_home = dplyr::case_when(
            player_on_play_home == "second time out." ~ "",
            T ~ player_on_play_home
          ),
          player_on_play_home = trimws(player_on_play_home),
          player_on_play_home = gsub(dplyr::first(shortName_home), "", player_on_play_home),
          player_on_play_home = dplyr::case_when(
            player_on_play_home == '' ~ NA_character_,
            player_on_play_home == "'s" ~ NA_character_,
            player_on_play_home == "Turnover" ~ NA_character_,
            T ~ player_on_play_home
          ),
          on_court_flag_home = dplyr::case_when(subbed_in_home ~ 1,
                                                subbed_out_home ~ 0,
                                                T ~ NA_real_),
          subbed_in_visitor = grepl('Subbing in for ', visitorText),
          subbed_out_visitor = grepl('Subbing out for ', visitorText),
          player_on_play_visitor = gsub(
            "((.*)'s |-| by )",
            "",
            visitorText),
          player_on_play_visitor = gsub("Subbing in for ", "", player_on_play_visitor),
          player_on_play_visitor = gsub("Subbing out for ", "", player_on_play_visitor),
          player_on_play_visitor = dplyr::case_when(grepl("\\[", player_on_play_visitor) ~ NA_character_,
                                                    grepl("Foul on ", player_on_play_visitor) ~ NA_character_,
                                                    grepl("End of ", player_on_play_visitor) ~ NA_character_,
                                                    T ~ player_on_play_visitor),
          player_on_play_visitor = gsub("\\(.*\\)", "", player_on_play_visitor),
          player_on_play_visitor = dplyr::case_when(
            player_on_play_visitor == "second time out." ~ "",
            T ~ player_on_play_visitor
          ),
          player_on_play_visitor = trimws(player_on_play_visitor),
          player_on_play_visitor = gsub(
            dplyr::first(shortName_visitor),
            "",
            player_on_play_visitor
          ),
          player_on_play_visitor = dplyr::case_when(
            player_on_play_visitor == '' ~ NA_character_,
            player_on_play_visitor == "'s" ~ NA_character_,
            player_on_play_visitor == "Turnover" ~ NA_character_,
            T ~ player_on_play_visitor
          ),
          on_court_flag_visitor = dplyr::case_when(subbed_in_visitor ~ 1,
                                                   subbed_out_visitor ~ 0,
                                                   T ~ NA_real_),
        ) |>
        dplyr::filter(!is.na(player_on_play_visitor) |
                        !is.na(player_on_play_home)) |>
        dplyr::group_by(time,player_on_play_home) |>
        dplyr::filter(!(sum(subbed_in_home) == sum(subbed_out_home) & sum(subbed_in_home) > 0)) |>
        dplyr::ungroup() |>
        dplyr::group_by(time,player_on_play_visitor) |>
        dplyr::filter(!(sum(subbed_in_visitor) == sum(subbed_out_home) & sum(subbed_in_home) > 0)) |>
        dplyr::ungroup() |>
        dplyr::group_by(periodNumber) |>
        dplyr::group_modify( ~ (\(a) {
          cbind(
            a,
            a |>
              # create a matrix of all players mentioned in the pbp at any point
              tidyr::pivot_wider(
                names_from = player_on_play_home,
                values_from = on_court_flag_home,
                names_prefix = 'x_',
                names_repair = "check_unique"
              ) |>
              #' whenever a player is subbed in, they have a 1 in that row and a 0
              #' in the row before. whenever a player is subbed out, they have a 0
              #' in that row and a 1 in the row before
              dplyr::mutate(
                dplyr::across(
                  dplyr::starts_with('x_'),
                  ~ dplyr::case_when(dplyr::lead(.) == 0 ~ 1,
                                     dplyr::lead(.) == 1 ~ 0,
                                     T ~
                                       .)
                )
              ) |>
              #' we then cascade the 1s and 0s up and down to create map of who is
              #' in the game at any given time
              tidyr::fill(c(dplyr::starts_with('x_')), .direction = "down") |>
              tidyr::fill(c(dplyr::starts_with('x_')), .direction = "up") |>
              dplyr::mutate(
                dplyr::across(
                  dplyr::starts_with('x_'),
                  ~ dplyr::case_when(is.na(.x) ~ 1,
                                     T ~ .x)
                )
              ) |>
              # finally we collapse the values down into a string of who is on court
              (\(d) {
                d |>
                  dplyr::select(dplyr::starts_with('x_')) |>
                  dplyr::select(-dplyr::any_of('x_NA')) |>
                  dplyr::mutate(row = dplyr::row_number()) |>
                  tidyr::pivot_longer(dplyr::starts_with("x_")) |>
                  dplyr::group_by(row) |>
                  dplyr::summarise(on_court_home = paste0(name[value == 1],collapse=';'))
              })()
          )
        })(.x)) |>
        dplyr::group_modify( ~ (\(a) {
          cbind(
            a,
            a |>
              # create a matrix of all players mentioned in the pbp at any point
              tidyr::pivot_wider(
                names_from = player_on_play_visitor,
                values_from = on_court_flag_visitor,
                names_prefix = 'x_',
                names_repair = "check_unique"
              ) |>
              #' whenever a player is subbed in, they have a 1 in that row and a 0
              #' in the row before. whenever a player is subbed out, they have a 0
              #' in that row and a 1 in the row before
              dplyr::mutate(
                dplyr::across(
                  dplyr::starts_with('x_'),
                  ~ dplyr::case_when(dplyr::lead(.) == 0 ~ 1,
                                     dplyr::lead(.) == 1 ~ 0,
                                     T ~
                                       .)
                )
              ) |>
              #' we then cascade the 1s and 0s up and down to create map of who is
              #' in the game at any given time
              tidyr::fill(c(dplyr::starts_with('x_')), .direction = "down") |>
              tidyr::fill(c(dplyr::starts_with('x_')), .direction = "up") |>
              dplyr::mutate(
                dplyr::across(
                  dplyr::starts_with('x_'),
                  ~ dplyr::case_when(is.na(.x) ~ 1,
                                     T ~ .x)
                )
              ) |>
              # finally we collapse the values down into a string of who is on court
              (\(d) {
                d |>
                  dplyr::select(dplyr::starts_with('x_')) |>
                  dplyr::select(-dplyr::any_of('x_NA')) |>
                  dplyr::mutate(row_1 = dplyr::row_number()) |>
                  tidyr::pivot_longer(dplyr::starts_with("x_")) |>
                  dplyr::group_by(row_1) |>
                  dplyr::summarise(on_court_visitor = paste0(name[value == 1],collapse=';'))
              })()
          )
        })(.x)) |>
        dplyr::ungroup() |>
        # find out when lineups change
        dplyr::mutate(stint = cumsum(
          subbed_in_home + subbed_in_visitor
        )) |>
        dplyr::group_by(stint, on_court_home, on_court_visitor, periodNumber) |>
        dplyr::summarise(
          stint_start = dplyr::first(time),
          stint_end = dplyr::last(time),
          .groups = "drop"
        ) |>
        dplyr::filter(stint_start != stint_end) |>
        dplyr::ungroup() |>
        dplyr::arrange(periodNumber,-stint_start) |>
        dplyr::group_by(periodNumber) |>
        dplyr::mutate(
          stint_start = dplyr::case_when(
            dplyr::row_number() == 1 &
              periodNumber %in% c(1, 2) &
              type == "mens" ~ lubridate::as.period("20M 0S"),
            dplyr::row_number() == 1 &
              periodNumber %in% c(1, 2, 3, 4) &
              type == "womens" ~ lubridate::as.period("10M 0S"),
            (
              dplyr::row_number() == 1 &
                periodNumber >= 3 &
                type == "mens"
            ) |
              (
                dplyr::row_number() == 1 &
                  periodNumber >= 5 &
                  type == "womens"
              ) ~ lubridate::as.period("5M 0S"),
            T ~ stint_start
          ),
          stint_end = dplyr::case_when(
            dplyr::row_number() == dplyr::n() ~ lubridate::as.period("0M 0S"),
            T ~ stint_end
          )
        ) |>
        dplyr::select(-c(stint)) |>
        #remove x_team from listed players in on_court
        dplyr::mutate(
          on_court_home = gsub(";x_team|x_team;", "", on_court_home),
          on_court_visitor = gsub(";x_team|x_team;", "", on_court_visitor),
          on_court_home = gsub(';x_ \\[.*\\]', "", on_court_home),
          on_court_home = gsub(';x_ \\[.*\\]', "", on_court_home),
          on_court_visitor = gsub(';x_ \\[.*\\]', "", on_court_visitor),
          on_court_visitor = gsub(';x_ \\[.*\\]', "", on_court_visitor),
        ) |>
        # omit bad sub data
        dplyr::mutate(dplyr::across(
          dplyr::starts_with("on_court"),
          \(x) dplyr::case_when(stringr::str_count(x, ';') == 4 ~ x,
                                T ~ NA_character_)
        )) |>
        data.frame()
    } 
  else if (any(grepl(' lineup change ', pbp$events))) {
      # v1
      pbp_parsed <- pbp |>
        # v1 pbp just tells us full new lineup whenever there's a change, except to start the game
        dplyr::mutate(
          on_court = dplyr::case_when(
            grepl(' lineup change ', events) ~ stringr::str_extract(events, '\\((.*)\\)'),
            T ~ NA_character_
          ),
          on_court = gsub('\\(', 'x_', on_court),
          on_court = gsub(', ', ';x_', on_court),
          on_court = gsub(')', '', on_court),
          home_lineup_change = grepl(' lineup change ', homeText),
          away_lineup_change = grepl(' lineup change ', visitorText),
          home_on_court = dplyr::case_when(home_lineup_change ~ on_court,
                                           T ~ NA_character_),
          away_on_court = dplyr::case_when(away_lineup_change ~ on_court,
                                           T ~ NA_character_),
        ) |>
        tidyr::fill(c(home_on_court, away_on_court), .direction = "down") |>
        (\(x) {
          # for the start of the game, we're missing lineups, so we have to estimate these
          missing_both <- x |>
            dplyr::filter(is.na(home_on_court) &
                            is.na(away_on_court))
          # sometimes only one team makes a change initially, this handles that
          missing_home <- x |>
            dplyr::filter(is.na(home_on_court) &
                            !is.na(away_on_court))
          missing_away <- x |>
            dplyr::filter(is.na(away_on_court) &
                            !is.na(home_on_court))
          complete <- x |>
            dplyr::filter(!is.na(home_on_court) &
                            !is.na(away_on_court))
          # extract players/teams from missing data
          extracted_both_home <-
            extract_all_players(missing_both$homeText)
          extracted_both_away <-
            extract_all_players(missing_both$visitorText)
          # if the away team made a substitution first and the home team did not make a sub at the same time
          if (nrow(missing_home)) {
            # grab the players from the text
            extracted_one_home <-
              extract_all_players(missing_home$homeText)
            extracted_one_away <-
              extract_all_players(missing_home$visitorText)
            # pull in all the players who are recorded as being on court for the home team while this is missing
            on_court_home <- c(
              extracted_both_home$pos_1,
              extracted_both_home$pos_2,
              extracted_both_away$def_1,
              extracted_both_away$def_2,
              extracted_one_home$pos_1,
              extracted_one_home$pos_2,
              extracted_one_away$def_1,
              extracted_one_away$def_2
            ) |>
              unique() |>
              (\(y) y[!(y %in% c(x$nickName_home[1], x$nickName_visitor[1], NA_character_))])()
            #  check that there's five players on the court for the home team
            if (length(on_court_home) == 5) {
              on_court_home <- on_court_home |>
                (\(y) paste0('x_', y))() |>
                paste0(collapse = ';')
            } else {
              # if not, NA the lineup
              on_court_home <- NA_character_
            }
            # pull in the away participation data, combine with the known lineups
            on_court_away <- c(
              extracted_both_away$pos_1,
              extracted_both_away$pos_2,
              extracted_both_home$def_1,
              extracted_both_home$def_2
            ) |>
              unique() |>
              (\(y) y[!(y %in% c(x$nickName_home[1], x$nickName_visitor[1], NA_character_))])()
            if (length(on_court_away) == 5) {
              on_court_away <- on_court_away |>
                (\(y) paste0('x_', y))() |>
                paste0(collapse = ';')
            } else {
              on_court_away <- NA_character_
            }
            # finally set the lineups for the data
            missing_both$away_on_court <- on_court_away
            missing_both$home_on_court <- on_court_home
            missing_home$home_on_court <- on_court_home
            # do the same process for home sub first/away no sub
          } else if (nrow(missing_away)) {
            extracted_one_away <- extract_all_players(missing_away$awayText)
            extracted_one_home <-
              extract_all_players(missing_away$visitorText)
            on_court_away <- c(
              extracted_both_away$pos_1,
              extracted_both_away$pos_2,
              extracted_both_home$def_1,
              extracted_both_home$def_2,
              extracted_one_away$pos_1,
              extracted_one_away$pos_2,
              extracted_one_home$def_1,
              extracted_one_home$def_2
            ) |>
              unique() |>
              (\(y) y[!(y %in% c(x$nickName_away[1], x$nickName_visitor[1], NA_character_))])()
            if (length(on_court_away) == 5) {
              on_court_away <- on_court_away |>
                (\(y) paste0('x_', y))() |>
                paste0(collapse = ';')
            } else {
              on_court_away <- NA_character_
            }
            on_court_home <- c(
              extracted_both_home$pos_1,
              extracted_both_home$pos_2,
              extracted_both_away$def_1,
              extracted_both_away$def_2
            ) |>
              unique() |>
              (\(y) y[!(y %in% c(x$nickName_away[1], x$nickName_visitor[1], NA_character_))])()
            if (length(on_court_home) == 5) {
              on_court_home <- on_court_home |>
                (\(y) paste0('x_', y))() |>
                paste0(collapse = ';')
            } else {
              on_court_home <- NA_character_
            }
            missing_both$home_on_court <- on_court_home
            missing_both$away_on_court <- on_court_away
            missing_away$away_on_court <- on_court_away
            # now if both teams sub at the same time, rely on the usual lineup logic
          } else {
            on_court_home <- c(
              extracted_both_home$pos_1,
              extracted_both_home$pos_2,
              extracted_both_away$def_1,
              extracted_both_away$def_2
            ) |>
              unique() |>
              (\(y) y[!(y %in% c(x$nickName_home[1], x$nickName_visitor[1], NA_character_))])()
            if (length(on_court_home) == 5) {
              on_court_home <- on_court_home |>
                (\(y) paste0('x_', y))() |>
                paste0(collapse = ';')
            } else {
              on_court_home <- NA_character_
            }
            on_court_away <- c(
              extracted_both_away$pos_1,
              extracted_both_away$pos_2,
              extracted_both_home$def_1,
              extracted_both_home$def_2
            ) |>
              unique() |>
              (\(y) y[!(y %in% c(x$nickName_home[1], x$nickName_visitor[1], NA_character_))])()
            if (length(on_court_away) == 5) {
              on_court_away <- on_court_away |>
                (\(y) paste0('x_', y))() |>
                paste0(collapse = ';')
            } else {
              on_court_away <- NA_character_
            }
            missing_both$away_on_court <- on_court_away
            missing_both$home_on_court <- on_court_home
          }
          # combine everything back together
          return(dplyr::bind_rows(missing_both,
                                  missing_away,
                                  missing_home,
                                  complete))
        })() |>
        # clean the data and calculate stints
        dplyr::mutate(
          on_court = paste0(home_on_court, ";", away_on_court),
          substitution = grepl(' lineup change ', events)
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(stint = cumsum(substitution)) |>
        dplyr::group_by(stint, home_on_court, away_on_court, periodNumber) |>
        dplyr::summarise(
          stint_start = dplyr::first(time),
          stint_end = dplyr::last(time),
          .groups = "drop"
        ) |>
        dplyr::filter(stint_start != stint_end) |>
        dplyr::ungroup() |>
        dplyr::arrange(periodNumber,-stint_start) |>
        dplyr::group_by(periodNumber) |>
        dplyr::mutate(
          stint_start = dplyr::case_when(
            dplyr::row_number() == 1 &
              periodNumber %in% c(1, 2) &
              type == "mens" ~ lubridate::as.period("20M 0S"),
            dplyr::row_number() == 1 &
              periodNumber %in% c(1, 2, 3, 4) &
              type == "womens" ~ lubridate::as.period("10M 0S"),
            (
              dplyr::row_number() == 1 &
                periodNumber >= 3 &
                type == "mens"
            ) |
              (
                dplyr::row_number() == 1 &
                  periodNumber >= 5 &
                  type == "womens"
              ) ~ lubridate::as.period("5M 0S"),
            T ~ stint_start
          ),
          stint_end = dplyr::case_when(
            dplyr::row_number() == dplyr::n() ~ lubridate::as.period("0M 0S"),
            T ~ stint_end
          )
        ) |>
        dplyr::select(-c(stint)) |>
        # omit bad sub data
        dplyr::mutate(
          home_on_court = dplyr::case_when(
            stringr::str_count(home_on_court, ';') == 4 ~ home_on_court,
            T ~ NA_character_
          )
        ) |>
        dplyr::mutate(
          away_on_court = dplyr::case_when(
            stringr::str_count(away_on_court, ';') == 4 ~ away_on_court,
            T ~ NA_character_
          )
        ) |>
        #consistent column naming with v2
        dplyr::rename(on_court_home = home_on_court,
                      on_court_visitor = away_on_court) |>
        data.frame()
    } else {
      cli::cli_alert_warning("No substitutions found for {pbp$game_id[1]}! Returning empty DF...")
      pbp_parsed <- data.frame()
    }
  return(pbp_parsed)
}

season_dates = seq(as.Date("2022-11-07"), as.Date("2023-04-03"), by="days") %>% format(format = "%Y/%m/%d")

games = data.frame()
for(d in season_dates){
  exist = tryCatch({paste0("https://data.ncaa.com/casablanca/scoreboard/basketball-men/d1/", d, "/scoreboard.json") %>% jsonlite::fromJSON(flatten = T)}, 
           error = function(e) {TRUE})
  if(exist[[1]] == TRUE){next}
  Sys.sleep(20)
  json <- paste0("https://data.ncaa.com/casablanca/scoreboard/basketball-men/d1/", d, "/scoreboard.json") %>%
    jsonlite::fromJSON(flatten = T)
  if(length(json$games) == 0){next}
  json = json$games %>% filter(game.gameState=="final")
  games = rbind(games, json)
}

#make names from games df match team names for our app
games = games %>% mutate(away = gsub(" St.", " State", away),
                         home = gsub(" St.", " State", home)) %>% 
  mutate(away = case_when(away=="UConn" ~ "UCONN",
                          .default = away),
         home = case_when(home=="UConn" ~ "UCONN",
                          .default = home))

write.csv(games, file = "data/ncaa_games.csv", row.names = F)

opp_games = games %>% filter(away %in% opponentList | home %in% opponentList)

rotation_times = data.frame()
for(t in opponentList){
  t_games = opp_games %>% filter(away==t | home==t)
  if(nrow(t_games)==0){next}
  for(g in 1:nrow(t_games)){
    g_id = t_games[g, 'url'] %>% gsub("/game/", "", .) %>% as.numeric()
    h_v = ifelse(t_games[g, 'home'] == t, "home", "visitor")
    h_a = ifelse(h_v == "home", "home", "away")
    h_a_opp = ifelse(h_v == "home", "away", "home")
    pbp = parse_pbp(scrape_pbp(g_id))
    if(nrow(pbp) == 0){Sys.sleep(20)}
    if(nrow(pbp) == 0){next}
    pbp = pbp %>% filter(periodNumber == 1 | periodNumber == 2) %>%
      #convert character stint times to numeric
      separate_wider_delim(delim = " ", names = c("mins_stint_start", "secs_stint_start"), cols = stint_start, too_few = "align_end") %>%
      mutate(mins_stint_start = gsub("M", "", mins_stint_start) %>% as.numeric(),
             secs_stint_start = gsub("S", "", secs_stint_start) %>% as.numeric(),
             mins_stint_start = ifelse(is.na(mins_stint_start), 0, mins_stint_start)) %>%
      separate_wider_delim(delim = " ", names = c("mins_stint_end", "secs_stint_end"), cols = stint_end, too_few = "align_end") %>%
      mutate(mins_stint_end = gsub("M", "", mins_stint_end) %>% as.numeric(),
             secs_stint_end = gsub("S", "", secs_stint_end) %>% as.numeric(),
             mins_stint_end = ifelse(is.na(mins_stint_end), 0, mins_stint_end)) %>%
      #create columns for stint start and end only using seconds as a unit
      mutate(periodSeconds = ifelse(periodNumber == 1, 1200, 0)) %>%
      mutate(stint_start_s = periodSeconds + (mins_stint_start*60) + secs_stint_start,
             stint_end_s = periodSeconds + (mins_stint_end*60) + secs_stint_end)
    #fix error in pbp_parse where stint end times are not correct
    for(r in 1:nrow(pbp)){
      next_row_start = pbp[r+1, "stint_start_s"] %>% as.numeric()
      pbp[r, "stint_end_s"] = case_when(is.na(next_row_start) ~ 0,
                                          .default = next_row_start)
    }
    pbp = pbp %>% select(on_court = paste0("on_court_", h_v), stint_start_s, stint_end_s) %>%
      separate_longer_delim(cols = on_court, delim = ";") %>%
      mutate(on_court = gsub("x_", "", on_court) %>% standardize_name()) %>%
      mutate(g_date = t_games[g, "date"],
             opp = t_games[g, h_a_opp],
             team = t_games[g, h_a])
    rotation_times = rbind(rotation_times, pbp)
    Sys.sleep(20)
    }
}

write.csv(games, file = "data/rotation_times.csv", row.names = F)
