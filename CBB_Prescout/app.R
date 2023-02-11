library(shiny)
library(pacman)
p_load(rvest, tidyverse, janitor, cfbplotR, stringr, gt, gtExtras, readxl, hoopR, paletteer, toRvik)
#remotes::install_github("sportsdataverse/hoopR")

our_team = "Oregon"
opponentList = c("Houston", "UCONN", "Alabama", "Michigan State", "Washington State", "UCLA")
opponentSRurl_db = read_xlsx("opp_url.xlsx")
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
our_schedule = kp_team_schedule(our_team, year=year) %>% mutate(opponent = clean_school_names(opponent))
AP_top25 = espn_mbb_rankings() %>% filter(type=="ap",
                                          current>0) %>% .[,c(7,8,9,11,17,18,19,20)] %>% mutate(team_location = clean_school_names(team_location))
NET_top50 = bart_tourney_sheets() %>% filter(net<=50) %>%
  mutate(team = gsub(" N4O", "", team),
         team = gsub(" F4O", "", team), 
         team = gsub("St.", "State", team)) %>%
  mutate(team = clean_school_names(team))

#get graphic info for each group of teams
graphic_info_OS = cfbplotR::logo_ref %>%
  filter(school %in% our_schedule$opponent | school %in% opponentList | school == our_team) %>%
  mutate(school = clean_school_names(school))
graphic_info_AP = cfbplotR::logo_ref %>%
  filter(school %in% AP_top25$team_location | school == our_team) %>%
  mutate(school = clean_school_names(school),
         school = ifelse(school=="UConn", "Connecticut", school))
graphic_info_NET = cfbplotR::logo_ref %>%
  filter(school %in% NET_top50$team | school == our_team) %>%
  mutate(school = clean_school_names(school))

#change SR_team_stats school names to how they appear in graphic_info
#these are just the ones I've found to be different so far; they must be manually changed
SR_team_stats[30,1] = 'BYU'
SR_team_stats[285,1] = 'SMU'
SR_team_stats[281,1] = 'USC'
SR_team_stats[259,1] = "Saint Mary's"
SR_team_stats[341,1] = "VCU"

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
rm(a, c)

#the list of options for our metric comparison plots
MetricCompList = c("ORTG x DRTG", "2P% x 3P%", "3PAR x 3P%", "AST% x TOV%", "STL% x BLK%", "OREB% x DREB%")

#list of options for filtering and sorting player personnel table
PPT_FilterList = c("All Players", "Guards", "Wings", "Bigs", "Starters", "Lefties")
PPT_SortList = c("MPG", "USG%", "PER", "BPM", "PPG", "3P%", "3PAr", "AST:TO", "TRB%")
PPT_ColumnList = c("Player Info", "Usage", "Positional Breakdown", "Advanced", "Scoring", "Shooting", "Playmaking", "Rebounding", "Defense")
PPT_ColumnListVecs = list(c("Class", "Pos", "Height", "Weight"), 
                          c("GP", "GS", "MPG", "Poss%", "USG%"), 
                          c("PG", "SG", "SF", "PF", "C"), 
                          c("PER", "OBPM", "DBPM", "BPM"),
                          c("PPG", "FD/40"),
                          c("2P%", "3P%", "3PAr", "eFG%", "TS%", "FT%", "FTr"),
                          c("ApG", "AST:TO", "AST%", "TOV%"), 
                          c("ORB%", "DRB%", "TRB%"), 
                          c("STL%", "BLK%", "FC/40"))
PPT_alwaysShow = c("#", "URL", "first", "last", "Team")

#read headshot url table
headshot_urls_db = read_xlsx("headshot_url.xlsx") %>%
  mutate(URL = ifelse(is.na(URL), "https://a.espncdn.com/combiner/i?img=/i/headshots/nophoto.png&w=110&h=80&scale=crop", URL))






ui = navbarPage("Pre-Scout Portal", fluid = TRUE,
                tabPanel("Matchup Selection", 
                         fluidRow(column(3, selectInput("opponent", "Opponent", opponentList)),
                                  column(6, h1(strong("Pre-Scout Portal")), uiOutput("header")),
                                  column(3, img(src="logo_oregon.png", height = 180, width = 240))
                                  ) #end of header fluidRow
                         ), #end of matchup selector tabPanel
                
                tabPanel("Graphical Metric Comparison",
                         fluidRow(column(9, h1(strong("Pre-Scout Portal")), uiOutput("header2")),
                                  column(3, img(src="logo_oregon.png", height = 180, width = 240))
                                  ), #end of header fluidRow
                         sidebarLayout(
                           sidebarPanel(radioButtons("whichGraph", "Metric Comparison", MetricCompList),
                                        radioButtons("displayedTeams", "Teams Displayed", c("Our Schedule", "AP Top 25", "NET Top 50")),
                                        checkboxInput("focus", "Focused View?")),
                           mainPanel(plotOutput("MetricComp", height = "600px"))
                           ) #end of sidebarLayout
                         ), #end of GMC tabPanel
                
                tabPanel("Player Personnel", 
                         fluidRow(column(9, h1(strong("Pre-Scout Portal")), uiOutput("header3")),
                                  column(3, img(src="logo_oregon.png", height = 180, width = 240))
                                  ), #end of header fluidRow
                         fluidRow(column(4, selectInput("filterPPT", "Filter", PPT_FilterList), 
                                         sliderInput("minMinsPPT", "Min/G Minimum", value=5, min=0, max=40), 
                                         style = "background-color:#f5f5f5"),
                                  column(4, selectInput("sortPPT", "Sort", PPT_SortList, selected = "MPG"), 
                                         uiOutput("minGP_PPT_out"), 
                                         style = "background-color:#f5f5f5"),
                                  column(4, checkboxGroupInput("columnsPPT", "Visible Columns", choices = PPT_ColumnList, selected = "Player Info"), 
                                         style = "background-color:#f5f5f5")
                                  ), #end of PPT sort/filter options fluidRow
                         fluidRow(12, gt_output("PlayerPersonnel"))
                         ), #end of PPT tabPanel
                
                tabPanel("Opponent Overview", 
                         fluidRow(column(9, h1(strong("Pre-Scout Portal")), uiOutput("header4")),
                                  column(3, img(src="logo_oregon.png", height = 180, width = 240))
                                  ), #end of header fluidRow
                         h5("Coming Soon")
                         ), #end of OO tabPanel
                
                tabPanel("Shot Charts", 
                         fluidRow(column(9, h1(strong("Pre-Scout Portal")), uiOutput("header5")),
                                  column(3, img(src="logo_oregon.png", height = 180, width = 240))
                                  ), #end of header fluidRow
                         h5("Coming Soon")
                         ), #end of SC tabPanel
                
                tabPanel("Lineups", 
                         fluidRow(column(9, h1(strong("Pre-Scout Portal")), uiOutput("header6")),
                                  column(3, img(src="logo_oregon.png", height = 180, width = 240))
                                  ), #end of header fluidRow
                         h5("Coming Soon")
                         ), #end of LU tabPanel
                
                ) #end of navbarPage


server = function(input, output, session) {
  
  #Oregon vs TEAM text output for header on top of all pages
  output$header6 = output$header5 = output$header4 = output$header3 = output$header2 = output$header = 
    renderUI(HTML(paste('<h1 style="color:green;font-size:50px">', our_team, " vs ", input$opponent, '</h1>', sep = "")))
  
  #decide which GMC table to use
  GMC = reactive(if(input$displayedTeams == "Our Schedule"){GMC_OS}
                 else if (input$displayedTeams == "AP Top 25"){GMC_AP}
                 else if (input$displayedTeams == "NET Top 50"){GMC_NET})
  #add 'focus' aesthetics to our team and opponent
  GMC_df = reactive(GMC() %>% mutate(color2 = if_else(school == our_team | school == input$opponent | school == opponent_kp(), NA_character_ ,"b/w"),
                                   alpha = if_else(school == our_team | school == input$opponent | school == opponent_kp(), 1, .6)))
   
  #Set x var and y var for axis labels
  xvar = reactive(str_split(input$whichGraph, " x ")[[1]][1])
  yvar = reactive(str_split(input$whichGraph, " x ")[[1]][2])
  
  #set x var and y var for values ####maybe change logic to xvar()=="ORTG"
  xvar_vals = reactive(if(input$whichGraph == "ORTG x DRTG"){GMC_df()[,'offense_o_rtg']} 
                       else if(input$whichGraph == "2P% x 3P%"){GMC_df()[,'offense_x2p_percent']}
                       else if(input$whichGraph == "3PAR x 3P%"){GMC_df()[,'offense_x3p_ar']}
                       else if(input$whichGraph == "AST% x TOV%"){GMC_df()[,'offense_ast_percent']}
                       else if(input$whichGraph == "STL% x BLK%"){GMC_df()[,'offense_stl_percent']}
                       else if(input$whichGraph == "OREB% x DREB%"){GMC_df()[,'offense_orb_percent']})
  yvar_vals = reactive(if(input$whichGraph == "ORTG x DRTG"){GMC_df()[,'defense_o_rtg']}
                       else if(input$whichGraph == "2P% x 3P%"){GMC_df()[,'offense_x3p_percent']}
                       else if(input$whichGraph == "3PAR x 3P%"){GMC_df()[,'offense_x3p_percent']}
                       else if(input$whichGraph == "AST% x TOV%"){GMC_df()[,'offense_tov_percent']}
                       else if(input$whichGraph == "STL% x BLK%"){GMC_df()[,'offense_blk_percent']}
                       else if(input$whichGraph == "OREB% x DREB%"){GMC_df()[,'offense_drb_percent']})
  
  #find medians for the x and y vars
  xvar_med = reactive(if(input$whichGraph == "ORTG x DRTG"){GMC_medians$offense_o_rtg} 
                      else if(input$whichGraph == "2P% x 3P%"){GMC_medians$offense_x2p_percent}
                      else if(input$whichGraph == "3PAR x 3P%"){GMC_medians$offense_x3p_ar}
                      else if(input$whichGraph == "AST% x TOV%"){GMC_medians$offense_ast_percent}
                      else if(input$whichGraph == "STL% x BLK%"){GMC_medians$offense_stl_percent}
                      else if(input$whichGraph == "OREB% x DREB%"){GMC_medians$offense_orb_percent})
  yvar_med = reactive(if(input$whichGraph == "ORTG x DRTG"){GMC_medians$defense_o_rtg}
                      else if(input$whichGraph == "2P% x 3P%"){GMC_medians$offense_x3p_percent}
                      else if(input$whichGraph == "3PAR x 3P%"){GMC_medians$offense_x3p_percent}
                      else if(input$whichGraph == "AST% x TOV%"){GMC_medians$offense_tov_percent}
                      else if(input$whichGraph == "STL% x BLK%"){GMC_medians$offense_blk_percent}
                      else if(input$whichGraph == "OREB% x DREB%"){GMC_medians$offense_drb_percent})
  
  #invert y axis if certain metric comps are selected
  flip_yvar = reactive(ifelse((yvar()=="DRTG" | yvar()=="TOV%"), "reverse", "identity"))
  
  #GMC plot output
  output$MetricComp = renderPlot({
    if(input$focus==F){
      ggplot() + scale_y_continuous(trans = flip_yvar()) +
        geom_hline(yintercept = median(yvar_med()), color = "red", linetype = "dashed") +
        geom_vline(xintercept = median(xvar_med()), color = "red", linetype = "dashed") +
        geom_cfb_logos(data = GMC_df(),
                       aes(x = unlist(xvar_vals()), y = unlist(yvar_vals()), team=school), width = .05) +
        xlab(xvar()) + ylab(yvar()) +
        theme_bw()}
    else{
      ggplot() + scale_y_continuous(trans = flip_yvar()) +
        geom_hline(yintercept = median(yvar_med()), color = "red", linetype = "dashed") +
        geom_vline(xintercept = median(xvar_med()), color = "red", linetype = "dashed") +
        geom_cfb_logos(data = GMC_df(),
                       aes(x = unlist(xvar_vals()), y = unlist(yvar_vals()), team=school, alpha=alpha, color=color2), width = .05) +
        scale_alpha_identity() + scale_color_identity() +
        xlab(xvar()) + ylab(yvar()) +
        theme_bw()}
  }) #end of MetricComp output
  
  #reactive expressions for SR data depending on opponent
  opponentSRurl = reactive(opponentSRurl_db %>% filter(opponent == input$opponent) %>% .[[1,2]])
  SRopponentTables = reactive(read_html(opponentSRurl()) %>% html_table())
  
  #creates a variable for opponent name when referring to kp functions
  opponent_kp = reactive(if(input$opponent == "UCONN"){"Connecticut"}
                         else{gsub(" State", " St.", input$opponent)})
  
  #PPT Data coming from different sources
  SR_PPT = reactive(SRopponentTables()[[1]] %>%
                      select('#', Player, Pos) %>%
                      # 5 steps to standardize player's names but not overwrite the original col of names
                      mutate(player_join = gsub("\\.", "", Player)) %>% 
                      mutate(player_join = gsub("'", "", player_join)) %>%
                      separate(player_join, into = c("first_join","last_join"), extra = "drop", sep = "[^\\w']") %>%
                      mutate(player_join = toupper(paste(first_join, last_join, sep = " "))) %>%
                      select(-first_join, -last_join) %>%
                      distinct())
  
  SR2_PPT = reactive(SRopponentTables()[[6]] %>%
                       select(Player, G, GS, MPG=MP, twoPperc="2P%", threePperc="3P%", ftperc="FT%", ApG=AST, TOV, PPG=PTS) %>%
                       # 5 steps to standardize player's names but not overwrite the original col of names
                       mutate(player_join = gsub("\\.", "", Player)) %>% 
                       mutate(player_join = gsub("'", "", player_join)) %>%
                       separate(player_join, into = c("first_join","last_join"), extra = "drop", sep = "[^\\w']") %>%
                       mutate(player_join = toupper(paste(first_join, last_join, sep = " "))) %>%
                       select(-first_join, -last_join, -Player))
  
  SR3_PPT = reactive(SRopponentTables()[[14]] %>%
                       select(Player, "USG%", eFGperc="eFG%", tsperc="TS%", threePAr="3PAr", "FTr", "AST%", "TOV%", 
                              "PER", "OBPM", "DBPM", "BPM", "ORB%", "DRB%", "TRB%", "STL%", "BLK%") %>%
                       # 5 steps to standardize player's names but not overwrite the original col of names
                       mutate(player_join = gsub("\\.", "", Player)) %>% 
                       mutate(player_join = gsub("'", "", player_join)) %>%
                       separate(player_join, into = c("first_join","last_join"), extra = "drop", sep = "[^\\w']") %>%
                       mutate(player_join = toupper(paste(first_join, last_join, sep = " "))) %>%
                       select(-first_join, -last_join, -Player))
  
  KP_PPT = reactive(kp_team_players(opponent_kp(), year) %>%
                      select(number, ht, wt, yr, poss_pct, f_dper40, f_cper40))
  
  headshots_PPT = reactive(headshot_urls_db %>% filter(Team == input$opponent) %>%
                             # 5 steps to standardize player's names but not overwrite the original col of names
                             mutate(player_join = gsub("\\.", "", Player)) %>% 
                             mutate(player_join = gsub("'", "", player_join)) %>%
                             separate(player_join, into = c("first_join","last_join"), extra = "drop", sep = "[^\\w']") %>%
                             mutate(player_join = toupper(paste(first_join, last_join, sep = " "))) %>%
                             select(-first_join, -last_join, -Player))
  
  #find player position
  kp_pos = reactive(kp_team_depth_chart(opponent_kp(), year))
  pos_pg = reactive(kp_pos() %>% select(first = pg_player_first_name, last = pg_player_last_name, min_pct = pg_min_pct, '#' = pg_number) %>% mutate(pos="PG") %>% filter(!is.na(first)))
  pos_sg = reactive(kp_pos() %>% select(first = sg_player_first_name, last = sg_player_last_name, min_pct = sg_min_pct, '#' = sg_number) %>% mutate(pos="SG") %>% filter(!is.na(first)))
  pos_sf = reactive(kp_pos() %>% select(first = sf_player_first_name, last = sf_player_last_name, min_pct = sf_min_pct, '#' = sf_number) %>% mutate(pos="SF") %>% filter(!is.na(first)))
  pos_pf = reactive(kp_pos() %>% select(first = pf_player_first_name, last = pf_player_last_name, min_pct = pf_min_pct, '#' = pf_number) %>% mutate(pos="PF") %>% filter(!is.na(first)))
  pos_c = reactive(kp_pos() %>% select(first = c_player_first_name, last = c_player_last_name, min_pct = c_min_pct, '#' = c_number) %>% mutate(pos="C") %>% filter(!is.na(first)))
  KP2_PPT = reactive(rbind(pos_pg(), pos_sg(), pos_sf(), pos_pf(), pos_c()) %>% 
                       pivot_wider(names_from = pos, values_from = min_pct, values_fill = 0) %>% 
                       mutate(total=PG+SG+SF+PF+C) %>%
                       mutate(PG = PG/total, SG=SG/total, SF=SF/total, PF=PF/total, C=C/total) %>% .[,-9] %>%
                       select('#', PG, SG, SF, PF, C))
  
  #find starters for last game the team played
  lastGdate = reactive(kp_team_schedule(opponent_kp(), year=year) %>% filter(is.na(pre_wp)) %>% arrange(desc(date)) %>% .[[1,18]])
  opponentGID = reactive(espn_mbb_scoreboard(lastGdate()) %>%
                           filter(toupper(home_team_location) == toupper(input$opponent) | toupper(away_team_location) == toupper(input$opponent)) %>% .[[1,6]])
  lastGstarters = reactive(espn_mbb_player_box(opponentGID()) %>% 
                             mutate(athlete_jersey = as.double(athlete_jersey)) %>%
                             filter(starter==TRUE) %>%
                             filter(toupper(team_short_display_name) == toupper(gsub(" St.", " St", opponent_kp())) |
                                      toupper(team_short_display_name) == toupper(input$opponent)) %>%
                             select("#" = athlete_jersey, starter))
  
  #join together all sources of info for PPT
  PPT_data = reactive(SR_PPT() %>%
                        full_join(SR2_PPT(), by = 'player_join') %>%
                        full_join(SR3_PPT(), by = 'player_join') %>%
                        full_join(KP_PPT(), by = c('#' = 'number')) %>%
                        full_join(headshots_PPT(), by = 'player_join') %>%
                        full_join(KP2_PPT(), by = '#') %>%
                        left_join(lastGstarters(), by = '#') %>%
                        mutate('All Players' = 1,
                               "Guards" = ifelse(PG+SG>.7, 1, 0),
                               "Bigs" = ifelse(C+PF>.8 & C>0 , 1, 0),
                               "Wings" = ifelse(Guards+Bigs==0, 1, 0),
                               "Starters" = ifelse(is.na(starter), 0, 1),
                               "Lefties" = 0) %>%
                        mutate(PG = as.numeric(ifelse(PG==0, NA, PG*100)),
                               SG = as.numeric(ifelse(SG==0, NA, SG*100)),
                               SF = as.numeric(ifelse(SF==0, NA, SF*100)),
                               PF = as.numeric(ifelse(PF==0, NA, PF*100)),
                               C = as.numeric(ifelse(C==0, NA, C*100))) %>%
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
                        separate(Player, into = c('first', 'last'), sep = "[^\\w'.-]", extra = 'merge') %>%
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
                               "All Players", Guards, Wings, Bigs, Starters, Lefties))
                        
  
  #filter and sort PPtable_raw before making it a gt object
  filter_PPT = reactive({
    tibble(TCL = PPT_ColumnList, TCLV = PPT_ColumnListVecs) %>%
      filter(TCL %in% input$columnsPPT) %>%
      pull(TCLV) %>%
      unlist()
  })
  sort_PPT = reactive(PPT_data() %>% 
                        filter(.data[[input$filterPPT]] == 1) %>%
                        filter(.data[["GP"]] >= input$minGP_PPT_in) %>%
                        filter(.data[["MPG"]] >= input$minMinsPPT) %>%
                        arrange(desc(.data[[input$sortPPT]])) %>%
                        select(PPT_alwaysShow, all_of(filter_PPT())))
  
  #find how many games the opponent has played this season
  opp_n_games = reactive(kp_team_schedule(opponent_kp(), year) %>% 
    filter(!is.na(w)) %>%
    nrow() %>% as.numeric())
  
  #take this out due to it messing up my R session constantly. I was not able to figure out why
  output$minGP_PPT_out = renderUI({sliderInput("minGP_PPT_in", "Games Played Minimum", value=0, min=0, max=opp_n_games())})

  #color for opponent
  opp_color = reactive(ifelse(nrow(graphic_info %>% filter(school == input$opponent)) == 1,
                              graphic_info %>% filter(school == input$opponent) %>% .[[1,4]], "black"))
  
  PPT_v1 = reactive(
    sort_PPT() %>% gt() %>%
      #formatting for player name
      gt_merge_stack_team_color(first, last, Team) %>%
      #player headshot
      gt_img_rows(columns = URL, img_source = "web", height = 90) %>%
      cols_hide(Team) %>%
      #rounding
      fmt_number(columns = starts_with("PG") & ends_with("PG") |
                   starts_with("SG") & ends_with("SG") |
                   starts_with("SF") & ends_with("SF") |
                   starts_with("PF") & ends_with("PF") |
                   starts_with("C") & ends_with("C"), 
                 decimals=0, drop_trailing_zeros = T) %>%
      #empty cells dont have NA values displayed
      sub_missing(
        columns = everything(), 
        missing_text = " ") %>%
      cols_label(URL = "",
                 first = "Name") %>%
      #left and right borders for each group
      tab_style(
        style = list(
          cell_borders(sides = "left")), locations = cells_body(
            columns = starts_with("Class") & ends_with("Class") |
              starts_with("GP") & ends_with("GP") |
              starts_with("PG") & ends_with("PG") |
              starts_with("PER") & ends_with("PER") |
              starts_with("PPG") & ends_with("PPG") |
              starts_with("2P%") & ends_with("2P%") |
              starts_with("ApG") & ends_with("ApG") |
              starts_with("ORB%") & ends_with("ORB%") |
              starts_with("STL%") & ends_with("STL%"))) %>%
      tab_style(
        style = list(
          cell_borders(sides = "right")), locations = cells_body(
            columns = starts_with("Weight") & ends_with("Weight") |
              starts_with("USG%") & ends_with("USG%") |
              starts_with("C") & ends_with("C") |
              starts_with("BPM") & ends_with("BPM") |
              starts_with("FD/40") & ends_with("FD/40") |
              starts_with("FTr") & ends_with("FTr") |
              starts_with("TOV%") & ends_with("TOV%") |
              starts_with("TRB%") & ends_with("TRB%") |
              starts_with("FC/40") & ends_with("FC/40"))) %>%
      #group title for each group
      tab_spanner(
        label = "Player Info",
        columns = c(starts_with("Class") & ends_with("Class"),
                    starts_with("Pos") & ends_with("Pos"),
                    starts_with("Height") & ends_with("Height"),
                    starts_with("Weight") & ends_with("Weight"))) %>%
      tab_spanner(
        label = "Usage",
        columns = c(starts_with("GP") & ends_with("GP"),
                    starts_with("GS") & ends_with("GS"),
                    starts_with("MPG") & ends_with("MPG"),
                    starts_with("Poss%") & ends_with("Poss%"),
                    starts_with("USG%") & ends_with("USG%"))) %>%
      tab_spanner(
        label = "Pos. Breakdown",
        columns = c(starts_with("PG") & ends_with("PG"),
                    starts_with("SG") & ends_with("SG"),
                    starts_with("SF") & ends_with("SF"),
                    starts_with("PF") & ends_with("PF"),
                    starts_with("C") & ends_with("C"))) %>%
      tab_spanner(
        label = "Advanced",
        columns = c(starts_with("PER") & ends_with("PER"),
                    starts_with("OBPM") & ends_with("OBPM"),
                    starts_with("DBPM") & ends_with("DBPM"),
                    starts_with("BPM") & ends_with("BPM"))) %>%
      tab_spanner(
        label = "Scoring",
        columns = c(starts_with("PPG") & ends_with("PPG"),
                    starts_with("FD/40") & ends_with("FD/40"))) %>%
      tab_spanner(
        label = "Shooting",
        columns = c(starts_with("2P%") & ends_with("2P%"),
                    starts_with("3P%") & ends_with("3P%"),
                    starts_with("3PAr") & ends_with("3PAr"),
                    starts_with("eFG%") & ends_with("eFG%"),
                    starts_with("TS%") & ends_with("TS%"),
                    starts_with("FT%") & ends_with("FT%"),
                    starts_with("FTr") & ends_with("FTr"))) %>%
      tab_spanner(
        label = "Playmaking",
        columns = c(starts_with("ApG") & ends_with("ApG"),
                    starts_with("AST:TO") & ends_with("AST:TO"),
                    starts_with("AST%") & ends_with("AST%"),
                    starts_with("TOV%") & ends_with("TOV%"))) %>%
      tab_spanner(
        label = "Rebounding",
        columns = c(starts_with("ORB%") & ends_with("ORB%"),
                    starts_with("DRB%") & ends_with("DRB%"),
                    starts_with("TRB%") & ends_with("TRB%"))) %>%
      tab_spanner(
        label = "Defense",
        columns = c(starts_with("STL%") & ends_with("STL%"),
                    starts_with("BLK%") & ends_with("BLK%"),
                    starts_with("FC/40") & ends_with("FC/40"))) %>%
      #increase horizontal spacing for each cell
      tab_style(
        style = cell_text(size = pct(115)),
        locations = cells_column_labels()) %>%
      #center all cell values
      cols_align(
        align = "center",
        columns = everything()) %>%
      #formatting for jersey number cell
      tab_style(
        style = list(
          cell_text(color = opp_color(),
                    weight = "bolder",
                    size = "x-large")),
        locations = cells_body(
          columns = "#",
          rows = everything()))
    )
  
  #how many columns are in our mostly finished gt table
  PPT_v1_cols = reactive(ncol(PPT_v1()$'_data'))
  
  #format column shading
  output$PlayerPersonnel = render_gt(PPT_v1() %>%
                                       tab_style(
                                         style = list(
                                           cell_fill(color = "grey95")), locations = cells_body(
                                             columns = if(PPT_v1_cols()!=5){seq(6,PPT_v1_cols(),2)}else{c(5)})) %>%
                                       tab_style(
                                         style = list(
                                           cell_fill(color = "grey90")), locations = cells_body(
                                             columns = starts_with("PG") & ends_with("PG") |
                                               starts_with("SG") & ends_with("SG") |
                                               starts_with("SF") & ends_with("SF") |
                                               starts_with("PF") & ends_with("PF") |
                                               starts_with("C") & ends_with("C"))) %>%
                                       gt_color_rows(columns = starts_with(input$sortPPT) & ends_with(input$sortPPT), 
                                                     palette = c("red", "white", "darkgreen"),
                                                     domain = PPT_data()[[input$sortPPT]]))
  
  output$test = renderText(PPT_v1_cols())
  
  
  } #end of server

shinyApp(ui, server)

