library(pacman)
p_load(tidyverse, janitor, cfbplotR, stringr, readxl, hoopR, toRvik, rvest)

our_team = "Oregon"
opponentList = c("Houston", "UCONN", "Alabama", "Michigan State", "Washington State", "UCLA")
opponentSRurl_db = read.csv("UOregon_Prescout/data/opp_url.csv")
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
headshot_urls_db = read.csv("UOregon_Prescout/data/headshot_url.csv") %>%
  mutate(URL = ifelse(is.na(URL), "https://a.espncdn.com/combiner/i?img=/i/headshots/nophoto.png&w=110&h=80&scale=crop", URL))

#write the data to file
write.csv(GMC_AP, file = "UOregon_Prescout/data/GMC_AP.csv", row.names = FALSE)
write.csv(GMC_OS, file = "UOregon_Prescout/data/GMC_OS.csv", row.names = FALSE)
write.csv(GMC_NET, file = "UOregon_Prescout/data/GMC_NET.csv", row.names = FALSE)
write.csv(GMC_medians, file = "UOregon_Prescout/data/GMC_medians.csv", row.names = FALSE)
write.csv(graphic_info_OS, file = "UOregon_Prescout/data/graphic_info_OS.csv", row.names = FALSE)

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
    select(-starter, -player_join) %>%
    separate(Player.x, into = c('first', 'last'), sep = "[^\\w'.-]", extra = 'merge') %>%
    #order columns into the order I want them to appear in the PPT
    select("#", URL, first, last, Team, 
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


write.csv(PPT_data, file = "UOregon_Prescout/data/PPT_data.csv", row.names = FALSE)
write.csv(OO_splits_data, file = "UOregon_Prescout/data/OO_splits_data.csv", row.names = FALSE)
write.csv(Opp_Trends_df, file = "UOregon_Prescout/data/Opp_Trends_df.csv", row.names = FALSE)

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
nba_player_stats = read.csv("UOregon_Prescout/data/nba_stats.csv") %>%
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
  mutate(x3p_percent = ifelse(is.na(x3p_percent), 0, x3p_percent))

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
         sim_1=NA, sim_2=NA, sim_3=NA, sim_4=NA, sim_5=NA) %>%
  select(player, class, pos, team, x3_p_s, slasher, ball_dominant, playmaker, limit_to, perim_defender, oreb, dreb, 
         sim_1, sim_2, sim_3, sim_4, sim_5) %>%
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
         sim_1=NA, sim_2=NA, sim_3=NA, sim_4=NA, sim_5=NA) %>%
  select(player, class, pos, team, x3_p_s, slasher, ball_dominant, playmaker, limit_to, perim_defender, oreb, dreb,
         sim_1, sim_2, sim_3, sim_4, sim_5) %>%
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
         sim_1=NA, sim_2=NA, sim_3=NA, sim_4=NA, sim_5=NA) %>%
  select(player, class, pos, team, x3_p_s, slasher, ball_dominant, playmaker, limit_to, perim_defender, rim_protector, oreb, dreb,
         sim_1, sim_2, sim_3, sim_4, sim_5) %>%
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
         sim_1=NA, sim_2=NA, sim_3=NA, sim_4=NA, sim_5=NA) %>%
  select(player, class, pos, team, x3_p_s, slasher, ball_dominant, playmaker, limit_to, perim_defender, rim_protector, oreb, dreb,
         sim_1, sim_2, sim_3, sim_4, sim_5) %>%
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
         sim_1=NA, sim_2=NA, sim_3=NA, sim_4=NA, sim_5=NA) %>%
  select(player, class, pos, team, stretch_big, draws_fouls, passing_big, limit_to, rim_protector, oreb, dreb,
         sim_1, sim_2, sim_3, sim_4, sim_5) %>%
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
         sim_1=NA, sim_2=NA, sim_3=NA, sim_4=NA, sim_5=NA) %>%
  select(player, class, pos, team, stretch_big, draws_fouls, passing_big, limit_to, rim_protector, oreb, dreb,
         sim_1, sim_2, sim_3, sim_4, sim_5) %>%
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
    similarity_vec = dist(rbind(a[[1]][player,], a[[2]]))[1:nrow(a[[2]])]
    top_5 = head(order(similarity_vec), 5)
    top_5 = a[[2]][top_5, 'player']
    a[[1]][player, 'sim_1'] = top_5[1]
    a[[1]][player, 'sim_2'] = top_5[2]
    a[[1]][player, 'sim_3'] = top_5[3]
    a[[1]][player, 'sim_4'] = top_5[4]
    a[[1]][player, 'sim_5'] = top_5[5]
  }
  sim_list[[z]] = a
  z = z+1
}

write.csv(sim_list[[1]][1], file = "UOregon_Prescout/data/ncaa_guard_sim.csv", row.names = FALSE)
write.csv(sim_list[[2]][1], file = "UOregon_Prescout/data/ncaa_wing_sim.csv", row.names = FALSE)
write.csv(sim_list[[3]][1], file = "UOregon_Prescout/data/ncaa_big_sim.csv", row.names = FALSE)


rm(a, all_player_stats, big_stats, guard_stats, nba_big_styles, nba_guard_styles, nba_player_stats, nba_wing_styles, ncaa_big_styles,
   ncaa_guard_styles, ncaa_player_stats, ncaa_wing_styles, raw_pa, raw_pt, player_totals_partial, player_advanced_partial, 
   num, offset, pa_url, pt_url, player_advanced_url, player_totals_url, player_advanced, player_totals, wing_stats, n_ncaa_players,
   player, similarity_vec, top_5, z)




