library(pacman)
p_load(tidyverse, janitor, cfbplotR, stringr, readxl, hoopR, toRvik, rvest)

our_team = "Oregon"
opponentList = c("Houston", "UCONN", "Alabama", "Michigan State", "Washington State", "UCLA")
opponentSRurl_db = read_xlsx("UOregon_Prescout/opp_url.xlsx")
year=2023

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

Sys.sleep(5)

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

Sys.sleep(5)

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

Sys.sleep(5)

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

Sys.sleep(5)

#put all tables into one 
SR_team_stats = left_join(basic_offense, basic_defense, by=c("school", "g", "w", "l", "w_l_percent", "srs", "sos"))
SR_team_stats = left_join(SR_team_stats, advanced_offense, by=c("school", "g", "w", "l", "w_l_percent", "srs", "sos"))
SR_team_stats = left_join(SR_team_stats, advanced_defense, by=c("school", "g", "w", "l", "w_l_percent", "srs", "sos"))

rm(advanced_offense, advanced_defense, basic_offense, basic_defense, advanced_offense_url, advanced_defense_url, basic_offense_url, basic_defense_url)

#convert stats into usable numeric stats
for(c in 2:67){SR_team_stats[,c] = SR_team_stats[,c] %>% as.numeric()}; rm(c)

#make all stats we want using other stats
SR_team_stats = SR_team_stats %>% mutate(offense_x2p_percent = (offense_fg - offense_x3p)/(offense_fga - offense_x3pa),
                                         offense_drb_percent = 100 - defense_orb_percent)

#standardize school names
SR_team_stats = SR_team_stats %>% mutate(school = clean_school_names(school))

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
rm(a, c, all_graphic_info, graphic_info_AP, graphic_info_NET)

#read headshot url table
headshot_urls_db = read_xlsx("UOregon_Prescout/headshot_url.xlsx") %>%
  mutate(URL = ifelse(is.na(URL), "https://a.espncdn.com/combiner/i?img=/i/headshots/nophoto.png&w=110&h=80&scale=crop", URL))




#write the data to file
write.csv(GMC_AP, file = "UOregon_Prescout/data/GMC_AP.csv", row.names = FALSE)
write.csv(GMC_OS, file = "UOregon_Prescout/data/GMC_OS.csv", row.names = FALSE)
write.csv(GMC_NET, file = "UOregon_Prescout/data/GMC_NET.csv", row.names = FALSE)
write.csv(GMC_medians, file = "UOregon_Prescout/data/GMC_medians.csv", row.names = FALSE)
write.csv(graphic_info_OS, file = "UOregon_Prescout/data/graphic_info_OS.csv", row.names = FALSE)



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



#opponent specific data

#list of teams that we will pull data for
data_needed_for = c("Houston", "UCONN", "Alabama", "Michigan State", "Washington State", "UCLA")
#empty df to populate
PPT_data = data.frame()

for(opp in data_needed_for){
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

  
  
  

  Sys.sleep(15)
}

write.csv(PPT_data, file = "UOregon_Prescout/data/PPT_data.csv", row.names = FALSE)
