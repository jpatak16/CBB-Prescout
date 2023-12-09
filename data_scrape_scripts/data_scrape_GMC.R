viewable_opps = read.csv("data/viewable_opps.csv")
opponentSRurl_db = read.csv("data/opp_url.csv", fileEncoding = "ISO-8859-1") %>% 
  mutate(ESPN_name = ifelse(ESPN_name == "", opponent, ESPN_name),
         KP_name = ifelse(KP_name == "", gsub(" State", " St.", opponent), KP_name))

#####basic offense table webscrape
basic_offense_url = "https://www.sports-reference.com/cbb/seasons/2024-school-stats.html"

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
basic_defense_url = "https://www.sports-reference.com/cbb/seasons/2024-opponent-stats.html"

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
advanced_offense_url = "https://www.sports-reference.com/cbb/seasons/2024-advanced-school-stats.html"

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
advanced_defense_url = "https://www.sports-reference.com/cbb/seasons/2024-advanced-opponent-stats.html"

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



SR_team_stats = SR_team_stats %>%
  #columns into numeric
  mutate(across(2:67, as.numeric)) %>%
  #make all stats we want using other stats
  mutate(offense_x2p_percent = (offense_fg - offense_x3p)/(offense_fga - offense_x3pa),
         offense_drb_percent = 100 - defense_orb_percent) %>%
  #standardize school names
  mutate(school = clean_school_names(school))

#pull team groups that will be in GMC
AP_top25 = espn_mbb_rankings() %>% filter(type=="ap", current>0) %>% select(team_location) %>% 
  mutate(team_location = clean_school_names(team_location)) %>% as.vector() %>% unlist() %>% unique()
NET_top50 = resume_database(min_year = this_year, max_year = this_year, min_net = 1, max_net = 50) %>% filter(year == this_year) %>%
  mutate(team = gsub(" St.", " State", team)) %>% 
  select(team) %>% mutate(team = clean_school_names(team)) %>% as.vector() %>% unlist() %>% unique()


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

#all_graphic_info = cfbplotR::logo_ref %>%
  #mutate(school = clean_school_names(school))

#list of teams that need to be renamed
#currently there are 3 teams that are not in graphic_info, so they cant be renamed
#not_naming_consistent = left_join(SR_team_stats, all_graphic_info, by='school') %>% select(school, g, w, l, logo, type, color, alt_color, wordmark) %>% filter(is.na(type)) %>% select(school) %>% as.vector() %>% unlist() %>% unique()

#find medians of all vars in GMC
GMC_medians = data.frame()
for(c in 2:ncol(SR_team_stats)){
  a = pull(SR_team_stats, var = c)
  GMC_medians[1,c-1] = median(a)
}
colnames(GMC_medians) <- colnames(SR_team_stats)[2:69]

write.csv(GMC_medians, file = "Oregon/data/GMC_medians.csv", row.names = FALSE)
write.csv(GMC_medians, file = "Clemson/data/GMC_medians.csv", row.names = FALSE)
write.csv(GMC_medians, file = "MississippiState/data/GMC_medians.csv", row.names = FALSE)
write.csv(GMC_medians, file = "NewMexico/data/GMC_medians.csv", row.names = FALSE)

#get graphic info for each group of teams
graphic_info_AP = cfbplotR::logo_ref %>%
  filter(school %in% AP_top25 | school %in% our_teams | school %in% viewable_opps$opp) %>%
  mutate(school = clean_school_names(school),
         school = ifelse(school=="UConn", "Connecticut", school),
         belong = ifelse(school %in% AP_top25, 1, 0))
graphic_info_NET = cfbplotR::logo_ref %>%
  filter(school %in% NET_top50 | school %in% our_teams | school %in% viewable_opps$opp) %>%
  mutate(school = clean_school_names(school),
         belong = ifelse(school %in% NET_top50, 1, 0))
  
#create data frame that will be used on the Graphical Metric Comparison page
GMC_AP = left_join(graphic_info_AP, SR_team_stats, by= 'school')
GMC_NET = left_join(graphic_info_NET, SR_team_stats, by= 'school')
  
write.csv(GMC_AP, file = "Oregon/data/GMC_AP.csv", row.names = FALSE)
write.csv(GMC_AP, file = "Clemson/data/GMC_AP.csv", row.names = FALSE)
write.csv(GMC_AP, file = "MississippiState/data/GMC_AP.csv", row.names = FALSE)
write.csv(GMC_AP, file = "NewMexico/data/GMC_AP.csv", row.names = FALSE)
  
write.csv(GMC_NET, file = "Oregon/data/GMC_NET.csv", row.names = FALSE)
write.csv(GMC_NET, file = "Clemson/data/GMC_NET.csv", row.names = FALSE)
write.csv(GMC_NET, file = "MississippiState/data/GMC_NET.csv", row.names = FALSE)
write.csv(GMC_NET, file = "NewMexico/data/GMC_NET.csv", row.names = FALSE)

#loop for each of our teams schedule
for(t in our_teams){
  vo_l = viewable_opps %>% filter(our_team == t) %>% pull(opp)
  
  our_schedule = kp_team_schedule(gsub(" State", " St.", t), year = this_year) %>% select(opponent) %>% 
    mutate(opponent = clean_school_names(opponent)) %>% as.vector() %>% unlist() %>% unique()
  Sys.sleep(15)
  
  graphic_info_OS = cfbplotR::logo_ref %>%
    filter(school %in% our_schedule | school %in% vo_l | school == t) %>%
    mutate(school = clean_school_names(school))
  
  GMC_OS = left_join(graphic_info_OS, SR_team_stats, by= 'school')
  
  write.csv(GMC_OS, file = paste0(gsub(" ", "", t), "/data/GMC_OS.csv"), row.names = FALSE)
  write.csv(graphic_info_OS, file = paste0(gsub(" ", "", t), "/data/graphic_info_OS.csv"), row.names = FALSE)
}


rm(advanced_defense, advanced_offense, basic_defense, basic_offense, GMC_AP, GMC_medians, GMC_NET, GMC_OS, graphic_info_AP,
   graphic_info_NET, graphic_info_OS, opponentSRurl_db, SR_team_stats, viewable_opps, a, advanced_defense_url, advanced_offense_url,
   AP_top25, basic_defense_url, basic_offense_url, c, NET_top50, our_schedule, t, vo_l)



