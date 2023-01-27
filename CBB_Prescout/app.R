library(shiny)
library(pacman)
p_load(rvest, tidyverse, janitor, cfbplotR, stringr, gt, gtExtras, readxl, hoopR)
#remotes::install_github("sportsdataverse/hoopR")

our_team = "Oregon"
opponentList = c("Washington State", "UCLA")
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

#pull our schedule
our_schedule = kp_team_schedule(our_team, year=year) %>% mutate(opponent = clean_school_names(opponent))

#get graphic info for teams on our schedule, opp list, and our team
graphic_info = cfbplotR::logo_ref %>%
  filter(school %in% our_schedule$opponent | school %in% opponentList | school == our_team) %>%
  mutate(school = clean_school_names(school))

#change SR_team_stats school names to how they appear in graphic_info
#these are just the ones I've found to be different so far; they must be manually changed
SR_team_stats[30,1] = 'BYU'
SR_team_stats[285,1] = 'SMU'
SR_team_stats[281,1] = 'USC'
SR_team_stats[259,1] = "Saint Mary's"
SR_team_stats[341,1] = "VCU"

#create data frame that will be used on the Graphical Metric Comparison page
GMC = left_join(graphic_info, SR_team_stats, by= 'school')

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
PPT_SortList = c("MpG")
PPT_ColumnList = c("Player Info", "Positional Breakdown", "Shooting", "Playmaking", "Dribble Drive Direction", "Defense", "Player Comp")
PPT_ColumnListVecs = list(c("#", "Class", "Pos", "Height", "Hand", "MpG"), c("PG", "SG", "SF", "PF", "C"), c(), c(), c(), c(), c())
PPT_alwaysShow = c("URL", "first", "last", "team")

#read headshot url table
headshot_urls_db = read_xlsx("headshot_url.xlsx")




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
                                  column(4, selectInput("sortPPT", "Sort", PPT_SortList), 
                                         sliderInput("minGamesPPT", "Games Played Minimum", value=0, min=0, max=30), 
                                         style = "background-color:#f5f5f5"),
                                  column(4, checkboxGroupInput("columnsPPT", "Visible Columns", choices = PPT_ColumnList, selected = "Player Info"), 
                                         style = "background-color:#f5f5f5")
                                  ), #end of PPT sort/filter options fluidRow
                         fluidRow(12, dataTableOutput("PPT_test"))
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
  
  #add 'focus' aesthetics to our team and opponent
  GMC_df = reactive(GMC %>% mutate(color2 = if_else(school == our_team | school == input$opponent, NA_character_ ,"b/w"),
                                   alpha = if_else(school == our_team | school == input$opponent, 1, .6)))
   
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
  
  #creates a variable for opponent name when reffering to kp functions
  opponent_kp = reactive(gsub(" State", " St.", input$opponent))
  
  #PPT Data coming from different sources
  SR_PPT = reactive(SRopponentTables()[[1]] %>% mutate(team=input$opponent) %>%
                      select('#', Pos))
  
  KP_PPT = reactive(kp_team_players(opponent_kp(), year) %>%
                      select(player, number, ht, wt, yr, g, s) %>% 
                      # 5 steps to standardize player's names but not overwrite the original col of names
                      mutate(player_join = gsub("\\.", "", player)) %>% 
                      mutate(player_join = gsub("'", "", player_join)) %>%
                      separate(player_join, into = c("first_join","last_join"), extra = "drop", sep = "[^\\w']") %>%
                      mutate(player_join = toupper(paste(first_join, last_join, sep = " "))) %>%
                      select(-first_join, -last_join))
  
  headshots_PPT = reactive(headshot_urls_db %>% filter(Team == input$opponent) %>%
                             # 5 steps to standardize player's names but not overwrite the original col of names
                             mutate(player_join = gsub("\\.", "", Player)) %>% 
                             mutate(player_join = gsub("'", "", player_join)) %>%
                             separate(player_join, into = c("first_join","last_join"), extra = "drop", sep = "[^\\w']") %>%
                             mutate(player_join = toupper(paste(first_join, last_join, sep = " "))) %>%
                             select(-first_join, -last_join))
  
  
  output$PPT_test = renderDataTable(KP_PPT())
  
  } #end of server

shinyApp(ui, server)

