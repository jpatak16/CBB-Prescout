library(shiny)
library(pacman)
p_load(rvest, tidyverse, janitor, cfbplotR, stringr, gt, gtExtras, readxl, hoopR, paletteer, toRvik, ggtext)
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
AP_top25 = espn_mbb_rankings() %>% filter(type=="usa",
                                          current>0) %>% .[,c(7,8,9,11,17,18,19,20)] %>% mutate(team_location = clean_school_names(team_location))

NET_top50 = bart_tourney_sheets() %>% filter(net<=50) %>%
  mutate(team = gsub(" N4O", "", team),
         team = gsub(" F4O", "", team), 
         team = gsub(" St.", " State", team)) %>%
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
graphic_info_test = cfbplotR::logo_ref %>%
  mutate(school = clean_school_names(school))

#change SR_team_stats school names to how they appear in graphic_info
#commented out test code can be used to find which schools are not currently matching. 
#There are 3 schools that dont have a match on both sides right now
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

#create data frame that will be used on the Graphical Metric Comparison page
GMC_OS = left_join(graphic_info_OS, SR_team_stats, by= 'school')
GMC_AP = left_join(graphic_info_AP, SR_team_stats, by= 'school')
GMC_NET = left_join(graphic_info_NET, SR_team_stats, by= 'school')
#GMC_test = right_join(graphic_info_test, SR_team_stats, by = "school")

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

OO_TrendSplit_List = c("OFF", "All", "Recency", "Location", "Result", "NET", "Conference")
OO_TrendIndicator_List = c("OFF", "Location", "NET", "Conference")

color1 = "#50c878"
color2 = "red"
color3 = "#ffce42"
color4 = "#0096ff"
color5 = "#ff7518"
color6 = "#9300ff"

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
                         fluidRow(column(2, selectInput("trendingStat", "Stat", OO_TrendStat_List, selected = "Winning Margin")),
                                  column(6, radioButtons("trendSplits", "Average By Splits", OO_TrendSplit_List, inline = T)),
                                  column(4, radioButtons("trendIndicators", "Indicators", OO_TrendIndicator_List, inline = T)),
                                  column(12, plotOutput("OppTrends", height = "350px"))
                                  ) #end of Opp Trends fluidRow 
                         ), #end of OO tabPanel
                
                tabPanel("Shot Charts", 
                         fluidRow(column(9, h1(strong("Pre-Scout Portal")), uiOutput("header5")),
                                  column(3, img(src="logo_oregon.png", height = 180, width = 240))
                                  ), #end of header fluidRow
                         h5("Coming Soon"),
                         dataTableOutput("test")
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
  opp_color = reactive(ifelse(nrow(graphic_info_OS %>% filter(school == input$opponent)) == 1,
                              graphic_info_OS %>% filter(school == input$opponent) %>% .[[1,4]], "black"))
  
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
  #find the conference of our opp
  opp_conf = reactive(kp_program_ratings() %>%
                        #manually change the abbreviation of certain conferences
                        mutate(conf = ifelse(conf == "Amer", "AAC", conf)) %>%
                        filter(team == opponent_kp()) %>%
                        .[[1,3]])
  
  #opponent espn id of opp
  opp_id = reactive(espn_mbb_teams() %>%
                      filter(toupper(team) == toupper(input$opponent)) %>%
                      select(team_id) %>%
                      .[[1,1]])
  
  #find the spread in every game our opp has played this season
  opp_spreads = reactive({
    df <- load_mbb_schedule() %>% 
      filter(home_id == opp_id() | away_id == opp_id(),
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
        filter(provider_name == "consensus") %>% .[[1,"spread"]] %>% as.integer()}
    
    df <- df %>% 
      mutate(spread = ifelse(home_id == opp_id(), spread, spread*-1),
             game_date = as.double(game_date)) %>%
      select(game_date, spread)
      
    })
  
  #find other stats used in OO Trend table and format column names to match select input
  opp_gp_stats = reactive(kp_gameplan(opponent_kp(), year)[[1]] %>%
                            select(game_date, Pace=pace, Offensive_Efficency=off_eff, Defensive_Efficency=def_eff, Points_Scored=team_score, Points_Allowed=opponent_score, 
                                   Offensive_3PAr=off_fg_3a_pct, 'Offensive_3Ppct'=off_fg_3_pct, 'Offensive_TOpct'=off_to_pct, 'Offensive_OREBpct'=off_or_pct, Offensive_FTr=off_ftr, 
                                   Defensive_3PAr=def_fg_3a_pct, 'Defensive_3Ppct'=def_fg_3_pct, 'Defensive_TOpct'=def_to_pct, 'Defensive_OREBpct'=def_or_pct, Defensive_FTr=def_ftr))
  
  
  #pull trending stats and other stats used for formatting
  opp_game_stats = reactive(kp_team_schedule(opponent_kp(), year) %>%
                              select(opponent, game_date, location, conference_game) %>%
                              full_join(kp_opptracker(opponent_kp(), year), by = c("opponent", "game_date")) %>%
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
                                     conference_game = ifelse(conference_game==TRUE, opp_conf(), "OOC")) %>%
                              #create ATS Margin
                              left_join(opp_spreads(), by="game_date") %>%
                              mutate(ATS_Margin = Winning_Margin + spread) %>%
                              #join in other used stats
                              left_join(opp_gp_stats(), by="game_date"))
  
  output$test = renderDataTable(opp_game_stats())
  
  #create separate data frames to split the data based on the input so that we can calculate mean in each split
  opp_game_stats_recency5 = reactive(opp_game_stats() %>% filter(!is.na(team_score)) %>%
                                       .[(nrow(opp_game_stats() %>% filter(!is.na(team_score)))-4):nrow(opp_game_stats() %>% filter(!is.na(team_score))),] %>%
                                       mutate(recency5 = "Last 5 Games") %>%
                                       select(game_code, recency5, input$trendingStat))
  
  opp_game_stats_recency10 = reactive(opp_game_stats() %>% filter(!is.na(team_score)) %>%
                                        .[(nrow(opp_game_stats() %>% filter(!is.na(team_score)))-9):nrow(opp_game_stats() %>% filter(!is.na(team_score))),] %>%
                                        mutate(recency10 = "Last 10 Games") %>%
                                        select(game_code, recency10, input$trendingStat))
  
  opp_game_stats_home = reactive(opp_game_stats() %>% filter(!is.na(team_score)) %>%
                                   filter(location == "Home") %>%
                                   select(game_code, location, input$trendingStat))
  
  opp_game_stats_away = reactive(opp_game_stats() %>% filter(!is.na(team_score)) %>%
                                   filter(location == "Away") %>%
                                   select(game_code, location, input$trendingStat))
  
  opp_game_stats_net50 = reactive(opp_game_stats() %>% filter(!is.na(team_score)) %>%
                                    filter(net <= 50) %>%
                                    select(game_code, net, input$trendingStat))
  
  opp_game_stats_net100 = reactive(opp_game_stats() %>% filter(!is.na(team_score)) %>%
                                    filter(net <= 100) %>%
                                    select(game_code, net, input$trendingStat))
  
  opp_game_stats_wins = reactive(opp_game_stats() %>%
                                     filter(wl == "W") %>%
                                     select(game_code, wl, input$trendingStat))
  
  opp_game_stats_losses = reactive(opp_game_stats() %>%
                                   filter(wl == "L") %>%
                                   select(game_code, wl, input$trendingStat))
  
  opp_game_stats_conf = reactive(opp_game_stats() %>% filter(!is.na(team_score)) %>%
                                     filter(conference_game == opp_conf()) %>%
                                     select(game_code, conference_game, input$trendingStat))
  
  #limit teams on the trends graph to games that have been played already and the next three games
  Opp_Trends_df = reactive(rbind(opp_game_stats() %>% filter(!is.na(team_score)),
                                  opp_game_stats() %>% filter(is.na(team_score)) %>% .[1:3,]) %>%
                             filter(!is.na(opponent)))
  
  #create columns so we can access date info and order of games for easier shading
  Opp_Trends_df_dates = reactive(Opp_Trends_df() %>%
    mutate(month = substr(game_date, 5, 6),
           day = substr(game_date, 7, 8),
           game_num = seq(1, nrow(Opp_Trends_df()))))
  
  #find max and min games of months that need shading
  dec_min = reactive(Opp_Trends_df_dates() %>% filter(month==12) %>% arrange(day) %>% .[1, "game_num"] %>% as.numeric() %>% sum(-.5))
  dec_max = reactive(Opp_Trends_df_dates() %>% filter(month==12) %>% arrange(desc(day)) %>% .[1,"game_num"] %>% as.numeric() %>% sum(.5))
  feb_min = reactive(Opp_Trends_df_dates() %>% filter(month=="02") %>% arrange(day) %>% .[1,"game_num"] %>% as.numeric() %>% sum(-.5))
  feb_max = reactive(Opp_Trends_df_dates() %>% filter(month=="02") %>% arrange(desc(day)) %>% .[1,"game_num"] %>% as.numeric() %>% sum(.5))
  
  output$OppTrends = renderPlot(
    
    if(input$trendSplits == "OFF"){
      
      if(input$trendIndicators == "OFF"){
        ggplot(data = Opp_Trends_df(), aes(x=reorder(game_code, date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df()$opponent) + 
          xlab("") + ylab(gsub("pct", "%", gsub("_", " ", input$trendingStat))) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2)) +
          guides(fill = guide_legend(title = NULL)) +
          theme_bw() + theme(axis.text.x = element_cfb_logo(size=1.5),
                             legend.position = "bottom",
                             legend.direction = "horizontal")}
      
      else if(input$trendIndicators == "Location"){
        ggplot(data = Opp_Trends_df(), aes(x=reorder(game_code, date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2)) +
          scale_colour_manual(values = c("Home" = color3,
                                         "Away" = color4)) +
          guides(fill = guide_legend(title = NULL, order=1),
                 color = guide_legend(title = NULL, order=2)) +
          theme_bw() + theme(axis.text.x = element_cfb_logo(size=1.5),
                             legend.position = "bottom",
                             legend.direction = 'horizontal') +
          geom_point(aes_string(y=input$trendingStat, colour = "location"), shape = 18, size = 4)}
      
      else if(input$trendIndicators == "NET"){
        ggplot(data = Opp_Trends_df(), aes(x=reorder(game_code, date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2)) +
          scale_colour_manual(values = c("NET Top 50" = color3,
                                         "NET Top 100" = color4)) +
          scale_size_manual(values = c("NET Top 50" = 4,
                                       "NET Top 100" = 4,
                                       "Other" = 0)) +
          guides(fill = guide_legend(title = NULL, order=1),
                 color = guide_legend(title = NULL, order=2),
                 size = FALSE) +
          theme_bw() + theme(axis.text.x = element_cfb_logo(size=1.5),
                             legend.position = "bottom",
                             legend.direction = 'horizontal') +
          geom_point(aes_string(y=input$trendingStat, colour = "net_rk", size = "net_rk"), shape = 18)}
      
      else if(input$trendIndicators == "Conference"){
        ggplot(data = Opp_Trends_df(), aes(x=reorder(game_code, date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2)) +
          scale_colour_manual(values = c(color3, color3),
                              limits = c(opp_conf())) +
          scale_size_manual(values = c(4), limits = c(opp_conf())) +
          guides(fill = guide_legend(title = NULL, order=1),
                 color = guide_legend(title = NULL, order=2),
                 size = FALSE) +
          theme_bw() + theme(axis.text.x = element_cfb_logo(size=1.5),
                             legend.position = "bottom",
                             legend.direction = 'horizontal') +
          geom_point(aes_string(y=input$trendingStat, colour = "conference_game", size = "conference_game"), shape = 18)}
    }
    
    else if(input$trendSplits == "All"){
      
      if(input$trendIndicators == "OFF"){
        ggplot(data = Opp_Trends_df(), aes(x=reorder(game_code, date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2)) +
          scale_linetype_manual(paste0("Average ", gsub("_", " ", input$trendingStat)), 
                                values = c("All Games" = "longdash")) +
          guides(fill = guide_legend(title = NULL, order=2),
                 linetype = guide_legend(keywidth = 5, title.position = "top", title.hjust = .5, order=1)) +
          theme_bw() + theme(axis.text.x = element_cfb_logo(size=1.5),
                             legend.position = "bottom",
                             legend.direction = "horizontal") +
          geom_hline(aes(yintercept = mean(Opp_Trends_df()[,input$trendingStat], na.rm=T),
                         linetype="All Games"), color = "black" , size = 1)}
      
      else if(input$trendIndicators == "Location"){
        ggplot(data = Opp_Trends_df(), aes(x=reorder(game_code, date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2)) +
          scale_colour_manual(values = c("Home" = color3,
                                         "Away" = color4)) +
          scale_linetype_manual(paste0("Average ", gsub("_", " ", input$trendingStat)), 
                                values = c("All Games" = "longdash")) +
          guides(fill = guide_legend(title = NULL, order=2),
                 color = guide_legend(title = NULL, order=3),
                 linetype = guide_legend(keywidth = 5, title.position = "top", title.hjust = .5, order=1)) +
          theme_bw() + theme(axis.text.x = element_cfb_logo(size=1.5),
                             legend.position = "bottom",
                             legend.direction = 'horizontal') +
          geom_hline(aes(yintercept = mean(Opp_Trends_df()[,input$trendingStat], na.rm=T),
                         linetype="All Games"), color = "black" , size = 1) +
          geom_point(aes_string(y=input$trendingStat, colour = "location"), shape = 18, size = 4)}
      
      else if(input$trendIndicators == "NET"){
        ggplot(data = Opp_Trends_df(), aes(x=reorder(game_code, date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2)) +
          scale_colour_manual(values = c("NET Top 50" = color3,
                                         "NET Top 100" = color4)) +
          scale_linetype_manual(name = paste0("Average ", gsub("_", " ", input$trendingStat)), 
                                values = c("All Games" = "longdash")) +
          scale_size_manual(values = c("NET Top 50" = 4,
                                       "NET Top 100" = 4,
                                       "Other" = 0)) +
          guides(fill = guide_legend(title = NULL, order=2),
                 color = guide_legend(title = NULL, order=3),
                 linetype = guide_legend(keywidth = 5, title.position = "top", title.hjust = .5, order=1),
                 size = FALSE) +
          theme_bw() + theme(axis.text.x = element_cfb_logo(size=1.5),
                             legend.position = "bottom",
                             legend.direction = 'horizontal') +
          geom_hline(aes(yintercept = mean(Opp_Trends_df()[,input$trendingStat], na.rm=T),
                         linetype="All Games"), color = "black" , size = 1) +
          geom_point(aes_string(y=input$trendingStat, colour = "net_rk", size = "net_rk"), shape = 18)}
      
      else if(input$trendIndicators == "Conference"){
        ggplot(data = Opp_Trends_df(), aes(x=reorder(game_code, date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2)) +
          scale_colour_manual(values = c(color3, color3),
                              limits = c(opp_conf())) +
          scale_linetype_manual(name = paste0("Average ", gsub("_", " ", input$trendingStat)), 
                                values = c("All Games" = "longdash")) +
          scale_size_manual(values = c(4), limits = c(opp_conf())) +
          guides(fill = guide_legend(title = NULL, order=2),
                 color = guide_legend(title = NULL, order=3),
                 linetype = guide_legend(keywidth = 5, title.position = "top", title.hjust = .5, order=1),
                 size = FALSE) +
          theme_bw() + theme(axis.text.x = element_cfb_logo(size=1.5),
                             legend.position = "bottom",
                             legend.direction = 'horizontal') +
          geom_hline(aes(yintercept = mean(Opp_Trends_df()[,input$trendingStat], na.rm=T),
                         linetype="All Games"), color = "black" , size = 1) +
          geom_point(aes_string(y=input$trendingStat, colour = "conference_game", size = "conference_game"), shape = 18)}
    }
    
    else if(input$trendSplits == "Recency"){
      
      if(input$trendIndicators == "OFF"){
        ggplot(data = Opp_Trends_df(), aes(x=reorder(game_code, date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2)) +
          scale_linetype_manual(name = paste0("Average ", gsub("_", " ", input$trendingStat)), 
                                values = c("All Games" = "longdash", 
                                           "Last 5 Games" = "dashed",
                                           "Last 10 Games" = "dashed")) +
          guides(fill = guide_legend(title = NULL, order=2),
                 linetype = guide_legend(keywidth = 3, title.position = "top", title.hjust = .5, order=1,
                                         override.aes = list(colour=c("black", color5, color6)))) +
          theme_bw() + theme(axis.text.x = element_cfb_logo(size=1.5),
                             legend.position = "bottom",
                             legend.direction = "horizontal") +
          geom_hline(aes(yintercept = mean(Opp_Trends_df()[,input$trendingStat], na.rm=T),
                         linetype="All Games"), color = "black" , size = 1) +
          geom_hline(aes(yintercept = mean(opp_game_stats_recency5()[,input$trendingStat], na.rm=T),
                         linetype="Last 5 Games"), color = color5 , size = 1) +
          geom_hline(aes(yintercept = mean(opp_game_stats_recency10()[,input$trendingStat], na.rm=T),
                         linetype="Last 10 Games"), color = color6 , size = 1)}
      
      else if(input$trendIndicators == "Location"){
        ggplot(data = Opp_Trends_df(), aes(x=reorder(game_code, date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2)) +
          scale_colour_manual(values = c("Home" = color3,
                                         "Away" = color4)) +
          scale_linetype_manual(name = paste0("Average ", gsub("_", " ", input$trendingStat)), 
                                values = c("All Games" = "longdash",
                                           "Last 5 Games" = "dashed",
                                           "Last 10 Games" = "dashed")) +
          guides(fill = guide_legend(title = NULL, order=2),
                 color = guide_legend(title = NULL, order=3),
                 linetype = guide_legend(keywidth = 5, title.position = "top", title.hjust = .5, order=1,
                                         override.aes = list(colour=c("black", color5, color6)))) +
          theme_bw() + theme(axis.text.x = element_cfb_logo(size=1.5),
                             legend.position = "bottom",
                             legend.direction = 'horizontal') +
          geom_hline(aes(yintercept = mean(Opp_Trends_df()[,input$trendingStat], na.rm=T),
                         linetype="All Games"), color = "black" , size = 1) +
          geom_point(aes_string(y=input$trendingStat, colour = "location"), shape = 18, size = 4) +
          geom_hline(aes(yintercept = mean(opp_game_stats_recency5()[,input$trendingStat], na.rm=T),
                         linetype="Last 5 Games"), color = color5 , size = 1) +
          geom_hline(aes(yintercept = mean(opp_game_stats_recency10()[,input$trendingStat], na.rm=T),
                         linetype="Last 10 Games"), color = color6 , size = 1)}
      
      else if(input$trendIndicators == "NET"){
        ggplot(data = Opp_Trends_df(), aes(x=reorder(game_code, date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2)) +
          scale_colour_manual(values = c("NET Top 50" = color3,
                                         "NET Top 100" = color4)) +
          scale_linetype_manual(name = paste0("Average ", gsub("_", " ", input$trendingStat)), 
                                values = c("All Games" = "longdash",
                                           "Last 5 Games" = "dashed",
                                           "Last 10 Games" = "dashed")) +
          scale_size_manual(values = c("NET Top 50" = 4,
                                       "NET Top 100" = 4,
                                       "Other" = 0)) +
          guides(fill = guide_legend(title = NULL, order=2),
                 color = guide_legend(title = NULL, order=3),
                 linetype = guide_legend(keywidth = 5, title.position = "top", title.hjust = .5, order=1,
                                         override.aes = list(colour=c("black", color5, color6))),
                 size = FALSE) +
          theme_bw() + theme(axis.text.x = element_cfb_logo(size=1.5),
                             legend.position = "bottom",
                             legend.direction = 'horizontal') +
          geom_hline(aes(yintercept = mean(Opp_Trends_df()[,input$trendingStat], na.rm=T),
                         linetype="All Games"), color = "black" , size = 1) +
          geom_point(aes_string(y=input$trendingStat, colour = "net_rk", size = "net_rk"), shape = 18) +
          geom_hline(aes(yintercept = mean(opp_game_stats_recency5()[,input$trendingStat], na.rm=T),
                         linetype="Last 5 Games"), color = color5 , size = 1) +
          geom_hline(aes(yintercept = mean(opp_game_stats_recency10()[,input$trendingStat], na.rm=T),
                         linetype="Last 10 Games"), color = color6 , size = 1)}
      
      else if(input$trendIndicators == "Conference"){
        ggplot(data = Opp_Trends_df(), aes(x=reorder(game_code, date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2)) +
          scale_colour_manual(values = c(color3, color3),
                              limits = c(opp_conf())) +
          scale_linetype_manual(name = paste0("Average ", gsub("_", " ", input$trendingStat)), 
                                values = c("All Games" = "longdash",
                                           "Last 5 Games" = "dashed",
                                           "Last 10 Games" = "dashed")) +
          scale_size_manual(values = c(4), limits = c(opp_conf())) +
          guides(fill = guide_legend(title = NULL, order=2),
                 color = guide_legend(title = NULL, order=3),
                 linetype = guide_legend(keywidth = 5, title.position = "top", title.hjust = .5, order=1,
                                         override.aes = list(colour=c("black", color5, color6))),
                 size = FALSE) +
          theme_bw() + theme(axis.text.x = element_cfb_logo(size=1.5),
                             legend.position = "bottom",
                             legend.direction = 'horizontal') +
          geom_hline(aes(yintercept = mean(Opp_Trends_df()[,input$trendingStat], na.rm=T),
                         linetype="All Games"), color = "black" , size = 1) +
          geom_point(aes_string(y=input$trendingStat, colour = "conference_game", size = "conference_game"), shape = 18) +
          geom_hline(aes(yintercept = mean(opp_game_stats_recency5()[,input$trendingStat], na.rm=T),
                         linetype="Last 5 Games"), color = color5 , size = 1) +
          geom_hline(aes(yintercept = mean(opp_game_stats_recency10()[,input$trendingStat], na.rm=T),
                         linetype="Last 10 Games"), color = color6 , size = 1)}
    }
    
    else if(input$trendSplits == "Location"){
      
      if(input$trendIndicators == "OFF"){
        ggplot(data = Opp_Trends_df(), aes(x=reorder(game_code, date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2)) +
          scale_linetype_manual(name = paste0("Average ", gsub("_", " ", input$trendingStat)), 
                                values = c("All Games" = "longdash", 
                                           "Home" = "dashed",
                                           "Away" = "dashed")) +
          guides(fill = guide_legend(title = NULL, order=2),
                 linetype = guide_legend(keywidth = 3, title.position = "top", title.hjust = .5, order=1,
                                         override.aes = list(colour=c("black", color5, color6)))) +
          theme_bw() + theme(axis.text.x = element_cfb_logo(size=1.5),
                             legend.position = "bottom",
                             legend.direction = "horizontal") +
          geom_hline(aes(yintercept = mean(Opp_Trends_df()[,input$trendingStat], na.rm=T),
                         linetype="All Games"), color = "black" , size = 1) +
          geom_hline(aes(yintercept = mean(opp_game_stats_home()[,input$trendingStat], na.rm=T),
                         linetype="Home"), color = color5 , size = 1) +
          geom_hline(aes(yintercept = mean(opp_game_stats_away()[,input$trendingStat], na.rm=T),
                         linetype="Away"), color = color6 , size = 1)}
      
      else if(input$trendIndicators == "Location"){
        ggplot(data = Opp_Trends_df(), aes(x=reorder(game_code, date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2)) +
          scale_colour_manual(values = c("Home" = color3,
                                         "Away" = color4)) +
          scale_linetype_manual(name = paste0("Average ", gsub("_", " ", input$trendingStat)), 
                                values = c("All Games" = "longdash",
                                           "Home" = "dashed",
                                           "Away" = "dashed")) +
          guides(fill = guide_legend(title = NULL, order=2),
                 color = guide_legend(title = NULL, order=3),
                 linetype = guide_legend(keywidth = 5, title.position = "top", title.hjust = .5, order=1,
                                         override.aes = list(colour=c("black", color3, color4)))) +
          theme_bw() + theme(axis.text.x = element_cfb_logo(size=1.5),
                             legend.position = "bottom",
                             legend.direction = 'horizontal') +
          geom_hline(aes(yintercept = mean(Opp_Trends_df()[,input$trendingStat], na.rm=T),
                         linetype="All Games"), color = "black" , size = 1) +
          geom_point(aes_string(y=input$trendingStat, colour = "location"), shape = 18, size = 4) +
          geom_hline(aes(yintercept = mean(opp_game_stats_home()[,input$trendingStat], na.rm=T),
                         linetype="Home"), color = color3 , size = 1) +
          geom_hline(aes(yintercept = mean(opp_game_stats_away()[,input$trendingStat], na.rm=T),
                         linetype="Away"), color = color4 , size = 1)}
      
      else if(input$trendIndicators == "NET"){
        ggplot(data = Opp_Trends_df(), aes(x=reorder(game_code, date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2)) +
          scale_colour_manual(values = c("NET Top 50" = color3,
                                         "NET Top 100" = color4)) +
          scale_linetype_manual(name = paste0("Average ", gsub("_", " ", input$trendingStat)), 
                                values = c("All Games" = "longdash",
                                           "Home" = "dashed",
                                           "Away" = "dashed")) +
          scale_size_manual(values = c("NET Top 50" = 4,
                                       "NET Top 100" = 4,
                                       "Other" = 0)) +
          guides(fill = guide_legend(title = NULL, order=2),
                 color = guide_legend(title = NULL, order=3),
                 linetype = guide_legend(keywidth = 5, title.position = "top", title.hjust = .5, order=1,
                                         override.aes = list(colour=c("black", color5, color6))),
                 size = FALSE) +
          theme_bw() + theme(axis.text.x = element_cfb_logo(size=1.5),
                             legend.position = "bottom",
                             legend.direction = 'horizontal') +
          geom_hline(aes(yintercept = mean(Opp_Trends_df()[,input$trendingStat], na.rm=T),
                         linetype="All Games"), color = "black" , size = 1) +
          geom_point(aes_string(y=input$trendingStat, colour = "net_rk", size = "net_rk"), shape = 18) +
          geom_hline(aes(yintercept = mean(opp_game_stats_home()[,input$trendingStat], na.rm=T),
                         linetype="Home"), color = color5 , size = 1) +
          geom_hline(aes(yintercept = mean(opp_game_stats_away()[,input$trendingStat], na.rm=T),
                         linetype="Away"), color = color6 , size = 1)}
      
      else if(input$trendIndicators == "Conference"){
        ggplot(data = Opp_Trends_df(), aes(x=reorder(game_code, date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2)) +
          scale_colour_manual(values = c(color3, color3),
                              limits = c(opp_conf())) +
          scale_linetype_manual(name = paste0("Average ", gsub("_", " ", input$trendingStat)), 
                                values = c("All Games" = "longdash",
                                           "Home" = "dashed",
                                           "Away" = "dashed")) +
          scale_size_manual(values = c(4), limits = c(opp_conf())) +
          guides(fill = guide_legend(title = NULL, order=2),
                 color = guide_legend(title = NULL, order=3),
                 linetype = guide_legend(keywidth = 5, title.position = "top", title.hjust = .5, order=1,
                                         override.aes = list(colour=c("black", color5, color6))),
                 size = FALSE) +
          theme_bw() + theme(axis.text.x = element_cfb_logo(size=1.5),
                             legend.position = "bottom",
                             legend.direction = 'horizontal') +
          geom_hline(aes(yintercept = mean(Opp_Trends_df()[,input$trendingStat], na.rm=T),
                         linetype="All Games"), color = "black" , size = 1) +
          geom_point(aes_string(y=input$trendingStat, colour = "conference_game", size = "conference_game"), shape = 18) +
          geom_hline(aes(yintercept = mean(opp_game_stats_home()[,input$trendingStat], na.rm=T),
                         linetype="Home"), color = color5 , size = 1) +
          geom_hline(aes(yintercept = mean(opp_game_stats_away()[,input$trendingStat], na.rm=T),
                         linetype="Away"), color = color6 , size = 1)}
    }
    
    else if(input$trendSplits == "Result"){
      
      if(input$trendIndicators == "OFF"){
        ggplot(data = Opp_Trends_df(), aes(x=reorder(game_code, date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2)) +
          scale_linetype_manual(name = paste0("Average ", gsub("_", " ", input$trendingStat)), 
                                values = c("All Games" = "longdash", 
                                           "Wins" = "dashed",
                                           "Losses" = "dashed")) +
          guides(fill = guide_legend(title = NULL, order=2),
                 linetype = guide_legend(keywidth = 3, title.position = "top", title.hjust = .5, order=1,
                                         override.aes = list(colour=c("black", color1, color2)))) +
          theme_bw() + theme(axis.text.x = element_cfb_logo(size=1.5),
                             legend.position = "bottom",
                             legend.direction = "horizontal") +
          geom_hline(aes(yintercept = mean(Opp_Trends_df()[,input$trendingStat]),
                         linetype="All Games"), color = "black" , size = 1) +
          geom_hline(aes(yintercept = mean(opp_game_stats_wins()[,input$trendingStat]),
                         linetype="Wins"), color = color1 , size = 1) +
          geom_hline(aes(yintercept = mean(opp_game_stats_losses()[,input$trendingStat]),
                         linetype="Losses"), color = color2 , size = 1)}
      
      else if(input$trendIndicators == "Location"){
        ggplot(data = Opp_Trends_df(), aes(x=reorder(game_code, date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2)) +
          scale_colour_manual(values = c("Home" = color3,
                                         "Away" = color4)) +
          scale_linetype_manual(name = paste0("Average ", gsub("_", " ", input$trendingStat)), 
                                values = c("All Games" = "longdash",
                                           "Wins" = "dashed",
                                           "Losses" = "dashed")) +
          guides(fill = guide_legend(title = NULL, order=2),
                 color = guide_legend(title = NULL, order=3),
                 linetype = guide_legend(keywidth = 5, title.position = "top", title.hjust = .5, order=1,
                                         override.aes = list(colour=c("black", color1, color2)))) +
          theme_bw() + theme(axis.text.x = element_cfb_logo(size=1.5),
                             legend.position = "bottom",
                             legend.direction = 'horizontal') +
          geom_hline(aes(yintercept = mean(Opp_Trends_df()[,input$trendingStat], na.rm=T),
                         linetype="All Games"), color = "black" , size = 1) +
          geom_point(aes_string(y=input$trendingStat, colour = "location"), shape = 18, size = 4) +
          geom_hline(aes(yintercept = mean(opp_game_stats_wins()[,input$trendingStat], na.rm=T),
                         linetype="Wins"), color = color1 , size = 1) +
          geom_hline(aes(yintercept = mean(opp_game_stats_losses()[,input$trendingStat], na.rm=T),
                         linetype="Losses"), color = color2 , size = 1)}
      
      else if(input$trendIndicators == "NET"){
        ggplot(data = Opp_Trends_df(), aes(x=reorder(game_code, date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2)) +
          scale_colour_manual(values = c("NET Top 50" = color3,
                                         "NET Top 100" = color4)) +
          scale_linetype_manual(name = paste0("Average ", gsub("_", " ", input$trendingStat)), 
                                values = c("All Games" = "longdash",
                                           "Wins" = "dashed",
                                           "Losses" = "dashed")) +
          scale_size_manual(values = c("NET Top 50" = 4,
                                       "NET Top 100" = 4,
                                       "Other" = 0)) +
          guides(fill = guide_legend(title = NULL, order=2),
                 color = guide_legend(title = NULL, order=3),
                 linetype = guide_legend(keywidth = 5, title.position = "top", title.hjust = .5, order=1,
                                         override.aes = list(colour=c("black", color1, color2))),
                 size = FALSE) +
          theme_bw() + theme(axis.text.x = element_cfb_logo(size=1.5),
                             legend.position = "bottom",
                             legend.direction = 'horizontal') +
          geom_hline(aes(yintercept = mean(Opp_Trends_df()[,input$trendingStat], na.rm=T),
                         linetype="All Games"), color = "black" , size = 1) +
          geom_point(aes_string(y=input$trendingStat, colour = "net_rk", size = "net_rk"), shape = 18) +
          geom_hline(aes(yintercept = mean(opp_game_stats_wins()[,input$trendingStat], na.rm=T),
                         linetype="Wins"), color = color1 , size = 1) +
          geom_hline(aes(yintercept = mean(opp_game_stats_losses()[,input$trendingStat], na.rm=T),
                         linetype="Losses"), color = color2 , size = 1)}
      
      else if(input$trendIndicators == "Conference"){
        ggplot(data = Opp_Trends_df(), aes(x=reorder(game_code, date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2)) +
          scale_colour_manual(values = c(color3, color3),
                              limits = c(opp_conf())) +
          scale_linetype_manual(name = paste0("Average ", gsub("_", " ", input$trendingStat)), 
                                values = c("All Games" = "longdash",
                                           "Wins" = "dashed",
                                           "Losses" = "dashed")) +
          scale_size_manual(values = c(4), limits = c(opp_conf())) +
          guides(fill = guide_legend(title = NULL, order=2),
                 color = guide_legend(title = NULL, order=3),
                 linetype = guide_legend(keywidth = 5, title.position = "top", title.hjust = .5, order=1,
                                         override.aes = list(colour=c("black", color1, color2))),
                 size = FALSE) +
          theme_bw() + theme(axis.text.x = element_cfb_logo(size=1.5),
                             legend.position = "bottom",
                             legend.direction = 'horizontal') +
          geom_hline(aes(yintercept = mean(Opp_Trends_df()[,input$trendingStat], na.rm=T),
                         linetype="All Games"), color = "black" , size = 1) +
          geom_point(aes_string(y=input$trendingStat, colour = "conference_game", size = "conference_game"), shape = 18) +
          geom_hline(aes(yintercept = mean(opp_game_stats_wins()[,input$trendingStat], na.rm=T),
                         linetype="Wins"), color = color1 , size = 1) +
          geom_hline(aes(yintercept = mean(opp_game_stats_losses()[,input$trendingStat], na.rm=T),
                         linetype="Losses"), color = color2 , size = 1)}
    }
    
    else if(input$trendSplits == "NET"){
      
      if(input$trendIndicators == "OFF"){
        ggplot(data = Opp_Trends_df(), aes(x=reorder(game_code, date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2)) +
          scale_linetype_manual(name = paste0("Average ", gsub("_", " ", input$trendingStat)), 
                                values = c("All Games" = "longdash", 
                                           "NET Top 50" = "dashed",
                                           "NET Top 100" = "dashed")) +
          guides(fill = guide_legend(title = NULL, order=2),
                 linetype = guide_legend(keywidth = 3, title.position = "top", title.hjust = .5, order=1,
                                         override.aes = list(colour=c("black", color5, color6)))) +
          theme_bw() + theme(axis.text.x = element_cfb_logo(size=1.5),
                             legend.position = "bottom",
                             legend.direction = "horizontal") +
          geom_hline(aes(yintercept = mean(Opp_Trends_df()[,input$trendingStat], na.rm=T),
                         linetype="All Games"), color = "black" , size = 1) +
          geom_hline(aes(yintercept = mean(opp_game_stats_net50()[,input$trendingStat], na.rm=T),
                         linetype="NET Top 50"), color = color5 , size = 1) +
          geom_hline(aes(yintercept = mean(opp_game_stats_net100()[,input$trendingStat], na.rm=T),
                         linetype="NET Top 100"), color = color6 , size = 1)}
      
      else if(input$trendIndicators == "Location"){
        ggplot(data = Opp_Trends_df(), aes(x=reorder(game_code, date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2)) +
          scale_colour_manual(values = c("Home" = color3,
                                         "Away" = color4)) +
          scale_linetype_manual(name = paste0("Average ", gsub("_", " ", input$trendingStat)), 
                                values = c("All Games" = "longdash",
                                           "NET Top 50" = "dashed",
                                           "NET Top 100" = "dashed")) +
          guides(fill = guide_legend(title = NULL, order=2),
                 color = guide_legend(title = NULL, order=3),
                 linetype = guide_legend(keywidth = 5, title.position = "top", title.hjust = .5, order=1,
                                         override.aes = list(colour=c("black", color5, color6)))) +
          theme_bw() + theme(axis.text.x = element_cfb_logo(size=1.5),
                             legend.position = "bottom",
                             legend.direction = 'horizontal') +
          geom_hline(aes(yintercept = mean(Opp_Trends_df()[,input$trendingStat], na.rm=T),
                         linetype="All Games"), color = "black" , size = 1) +
          geom_point(aes_string(y=input$trendingStat, colour = "location"), shape = 18, size = 4) +
          geom_hline(aes(yintercept = mean(opp_game_stats_net50()[,input$trendingStat], na.rm=T),
                         linetype="NET Top 50"), color = color5 , size = 1) +
          geom_hline(aes(yintercept = mean(opp_game_stats_net100()[,input$trendingStat], na.rm=T),
                         linetype="NET Top 100"), color = color6 , size = 1)}
      
      else if(input$trendIndicators == "NET"){
        ggplot(data = Opp_Trends_df(), aes(x=reorder(game_code, date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2)) +
          scale_colour_manual(values = c("NET Top 50" = color3,
                                         "NET Top 100" = color4)) +
          scale_linetype_manual(name = paste0("Average ", gsub("_", " ", input$trendingStat)), 
                                values = c("All Games" = "longdash",
                                           "NET Top 50" = "dashed",
                                           "NET Top 100" = "dashed")) +
          scale_size_manual(values = c("NET Top 50" = 4,
                                       "NET Top 100" = 4,
                                       "Other" = 0)) +
          guides(fill = guide_legend(title = NULL, order=2),
                 color = guide_legend(title = NULL, order=3),
                 linetype = guide_legend(keywidth = 5, title.position = "top", title.hjust = .5, order=1,
                                         override.aes = list(colour=c("black", color3, color4))),
                 size = FALSE) +
          theme_bw() + theme(axis.text.x = element_cfb_logo(size=1.5),
                             legend.position = "bottom",
                             legend.direction = 'horizontal') +
          geom_hline(aes(yintercept = mean(Opp_Trends_df()[,input$trendingStat], na.rm=T),
                         linetype="All Games"), color = "black" , size = 1) +
          geom_point(aes_string(y=input$trendingStat, colour = "net_rk", size = "net_rk"), shape = 18) +
          geom_hline(aes(yintercept = mean(opp_game_stats_net50()[,input$trendingStat], na.rm=T),
                         linetype="NET Top 50"), color = color3 , size = 1) +
          geom_hline(aes(yintercept = mean(opp_game_stats_net100()[,input$trendingStat], na.rm=T),
                         linetype="NET Top 100"), color = color4 , size = 1)}
      
      else if(input$trendIndicators == "Conference"){
        ggplot(data = Opp_Trends_df(), aes(x=reorder(game_code, date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2)) +
          scale_colour_manual(values = c(color3, color3),
                              limits = c(opp_conf())) +
          scale_linetype_manual(name = paste0("Average ", gsub("_", " ", input$trendingStat)), 
                                values = c("All Games" = "longdash",
                                           "NET Top 50" = "dashed",
                                           "NET Top 100" = "dashed")) +
          scale_size_manual(values = c(4), limits = c(opp_conf())) +
          guides(fill = guide_legend(title = NULL, order=2),
                 color = guide_legend(title = NULL, order=3),
                 linetype = guide_legend(keywidth = 5, title.position = "top", title.hjust = .5, order=1,
                                         override.aes = list(colour=c("black", color5, color6))),
                 size = FALSE) +
          theme_bw() + theme(axis.text.x = element_cfb_logo(size=1.5),
                             legend.position = "bottom",
                             legend.direction = 'horizontal') +
          geom_hline(aes(yintercept = mean(Opp_Trends_df()[,input$trendingStat], na.rm=T),
                         linetype="All Games"), color = "black" , size = 1) +
          geom_point(aes_string(y=input$trendingStat, colour = "conference_game", size = "conference_game"), shape = 18) +
          geom_hline(aes(yintercept = mean(opp_game_stats_net50()[,input$trendingStat], na.rm=T),
                         linetype="NET Top 50"), color = color5 , size = 1) +
          geom_hline(aes(yintercept = mean(opp_game_stats_net100()[,input$trendingStat], na.rm=T),
                         linetype="NET Top 100"), color = color6 , size = 1)}
    }
    
    else if(input$trendSplits == "Conference"){
      
      if(input$trendIndicators == "OFF"){
        ggplot(data = Opp_Trends_df(), aes(x=reorder(game_code, date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2)) +
          scale_linetype_manual(name = paste0("Average ", gsub("_", " ", input$trendingStat)), 
                                values = c("All Games" = "longdash", 
                                           "Conference Games" = "dashed"),
                                labels = c("All Games", paste0(opp_conf(), " Games"))) +
          guides(fill = guide_legend(title = NULL, order=2),
                 linetype = guide_legend(keywidth = 3, title.position = "top", title.hjust = .5, order=1,
                                         override.aes = list(colour=c("black", color6)))) +
          theme_bw() + theme(axis.text.x = element_cfb_logo(size=1.5),
                             legend.position = "bottom",
                             legend.direction = "horizontal") +
          geom_hline(aes(yintercept = mean(Opp_Trends_df()[,input$trendingStat], na.rm=T),
                         linetype="All Games"), color = "black" , size = 1) +
          geom_hline(aes(yintercept = mean(opp_game_stats_conf()[,input$trendingStat], na.rm=T),
                         linetype="Conference Games"), color = color6 , size = 1)}
      
      else if(input$trendIndicators == "Location"){
        ggplot(data = Opp_Trends_df(), aes(x=reorder(game_code, date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2)) +
          scale_colour_manual(values = c("Home" = color3,
                                         "Away" = color4)) +
          scale_linetype_manual(name = paste0("Average ", gsub("_", " ", input$trendingStat)), 
                                values = c("All Games" = "longdash",
                                           "Conference Games" = "dashed"),
                                labels = c("All Games", paste0(opp_conf(), " Games"))) +
          guides(fill = guide_legend(title = NULL, order=2),
                 color = guide_legend(title = NULL, order=3),
                 linetype = guide_legend(keywidth = 5, title.position = "top", title.hjust = .5, order=1,
                                         override.aes = list(colour=c("black", color6)))) +
          theme_bw() + theme(axis.text.x = element_cfb_logo(size=1.5),
                             legend.position = "bottom",
                             legend.direction = 'horizontal') +
          geom_hline(aes(yintercept = mean(Opp_Trends_df()[,input$trendingStat], na.rm=T),
                         linetype="All Games"), color = "black" , size = 1) +
          geom_point(aes_string(y=input$trendingStat, colour = "location"), shape = 18, size = 4) +
          geom_hline(aes(yintercept = mean(opp_game_stats_conf()[,input$trendingStat], na.rm=T),
                         linetype="Conference Games"), color = color6 , size = 1)}
      
      else if(input$trendIndicators == "NET"){
        ggplot(data = Opp_Trends_df(), aes(x=reorder(game_code, date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2)) +
          scale_colour_manual(values = c("NET Top 50" = color3,
                                         "NET Top 100" = color4)) +
          scale_linetype_manual(name = paste0("Average ", gsub("_", " ", input$trendingStat)), 
                                values = c("All Games" = "longdash",
                                           "Conference Games" = "dashed"),
                                labels = c("All Games", paste0(opp_conf(), " Games")))+
          scale_size_manual(values = c("NET Top 50" = 4,
                                       "NET Top 100" = 4,
                                       "Other" = 0)) +
          guides(fill = guide_legend(title = NULL, order=2),
                 color = guide_legend(title = NULL, order=3),
                 linetype = guide_legend(keywidth = 5, title.position = "top", title.hjust = .5, order=1,
                                         override.aes = list(colour=c("black", color6))),
                 size = FALSE) +
          theme_bw() + theme(axis.text.x = element_cfb_logo(size=1.5),
                             legend.position = "bottom",
                             legend.direction = 'horizontal') +
          geom_hline(aes(yintercept = mean(Opp_Trends_df()[,input$trendingStat], na.rm=T),
                         linetype="All Games"), color = "black" , size = 1) +
          geom_point(aes_string(y=input$trendingStat, colour = "net_rk", size = "net_rk"), shape = 18) +
          geom_hline(aes(yintercept = mean(opp_game_stats_conf()[,input$trendingStat], na.rm=T),
                         linetype="Conference Games"), color = color6 , size = 1)}
      
      else if(input$trendIndicators == "Conference"){
        ggplot(data = Opp_Trends_df(), aes(x=reorder(game_code, date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2)) +
          scale_colour_manual(values = c(color3, color3),
                              limits = c(opp_conf())) +
          scale_linetype_manual(name = paste0("Average ", gsub("_", " ", input$trendingStat)), 
                                values = c("All Games" = "longdash",
                                           "Conference Games" = "dashed"),
                                labels = c("All Games", paste0(opp_conf(), " Games"))) +
          scale_size_manual(values = c(4), limits = c(opp_conf())) +
          guides(fill = guide_legend(title = NULL, order=2),
                 color = guide_legend(title = NULL, order=3),
                 linetype = guide_legend(keywidth = 5, title.position = "top", title.hjust = .5, order=1,
                                         override.aes = list(colour=c("black", color3))),
                 size = FALSE) +
          theme_bw() + theme(axis.text.x = element_cfb_logo(size=1.5),
                             legend.position = "bottom",
                             legend.direction = 'horizontal') +
          geom_hline(aes(yintercept = mean(Opp_Trends_df()[,input$trendingStat], na.rm=T),
                         linetype="All Games"), color = "black" , size = 1) +
          geom_point(aes_string(y=input$trendingStat, colour = "conference_game", size = "conference_game"), shape = 18) +
          geom_hline(aes(yintercept = mean(opp_game_stats_conf()[,input$trendingStat], na.rm=T),
                         linetype="Conference Games"), color = color3 , size = 1)}
    }
    )

  
  } #end of server

shinyApp(ui, server)

