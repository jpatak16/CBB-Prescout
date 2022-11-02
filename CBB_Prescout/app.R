library(shiny)
library(pacman)
p_load(rvest, tidyverse, janitor, cfbplotR, stringr, gt, gtExtras, readxl, hoopR)
#p_load_current_gh("sportsdataverse/hoopR", dependencies = TRUE, update = TRUE)

our_team = "Oregon"
opponentList = c("Texas A&M", "VCU", "Wake Forest")
opponentSRurl_db = read_xlsx("opp_url.xlsx")
year=2022

#####basic offense table webscrape
basic_offense_url = "https://www.sports-reference.com/cbb/seasons/2022-school-stats.html"

basic_offense = read_html(basic_offense_url) %>%
  html_table(fill = TRUE) %>%
  .[[1]] %>% as.data.frame() %>%
  .[,-c(9,12,15,18,21)] %>%
  row_to_names(row_number = 1, remove_row = T) %>% clean_names() %>%
  filter(!g=='G') %>% filter(!g=="Overall") %>%
  mutate(school = ifelse(endsWith(school, "NCAA"), substr(school, 1, nchar(school)-5), school)) %>%
  .[,-c(1, 9:16)]
colnames(basic_offense)= c(colnames(basic_offense[,1:7]), paste("offense_", colnames(basic_offense[,8:24]), sep = ""))

#####basic defense table webscrape
basic_defense_url = "https://www.sports-reference.com/cbb/seasons/2022-opponent-stats.html"

basic_defense = read_html(basic_defense_url) %>%
  html_table(fill = TRUE) %>%
  .[[1]] %>% as.data.frame() %>%
  .[,-c(9,12,15,18,21)] %>%
  row_to_names(row_number = 1, remove_row = T) %>% clean_names() %>%
  filter(!g=='G') %>% filter(!g=="Overall") %>%
  mutate(school = ifelse(endsWith(school, "NCAA"), substr(school, 1, nchar(school)-5), school)) %>%
  .[,-c(1, 9:16)]
colnames(basic_defense)= c(colnames(basic_defense[,1:7]), paste("defense_", colnames(basic_defense[,8:24]), sep = ""))

#####advanced offense table webscrape
advanced_offense_url = "https://www.sports-reference.com/cbb/seasons/2022-advanced-school-stats.html"

advanced_offense = read_html(advanced_offense_url) %>%
  html_table(fill = TRUE) %>%
  .[[1]] %>% as.data.frame() %>%
  .[,-c(9,12,15,18,21)] %>%
  row_to_names(row_number = 1, remove_row = T) %>% clean_names() %>%
  filter(!g=='G') %>% filter(!g=="Overall") %>%
  mutate(school = ifelse(endsWith(school, "NCAA"), substr(school, 1, nchar(school)-5), school)) %>%
  .[,-c(1, 9:16)]
colnames(advanced_offense)= c(colnames(advanced_offense[,1:7]), paste("offense_", colnames(advanced_offense[,8:20]), sep = ""))

#####advanced defense table webscrape
advanced_defense_url = "https://www.sports-reference.com/cbb/seasons/2022-advanced-opponent-stats.html"

advanced_defense = read_html(advanced_defense_url) %>%
  html_table(fill = TRUE) %>%
  .[[1]] %>% as.data.frame() %>%
  .[,-c(9,12,15,18,21)] %>%
  row_to_names(row_number = 1, remove_row = T) %>% clean_names() %>%
  filter(!g=='G') %>% filter(!g=="Overall") %>%
  mutate(school = ifelse(endsWith(school, "NCAA"), substr(school, 1, nchar(school)-5), school)) %>%
  .[,-c(1, 9:16)]
colnames(advanced_defense)= c(colnames(advanced_defense[,1:7]), paste("defense_", colnames(advanced_defense[,8:20]), sep = ""))

#put all tables into one 
team_stats = left_join(basic_offense, basic_defense, by=c("school", "g", "w", "l", "w_l_percent", "srs", "sos"))
team_stats = left_join(team_stats, advanced_offense, by=c("school", "g", "w", "l", "w_l_percent", "srs", "sos"))
team_stats = left_join(team_stats, advanced_defense, by=c("school", "g", "w", "l", "w_l_percent", "srs", "sos"))

#convert stats into usable numeric stats
for(c in 2:67){team_stats[,c] = team_stats[,c] %>% as.numeric()}

#make all stats we want using other stats and standardize school names
team_stats = team_stats %>% mutate(offense_x2p_percent = (offense_fg - offense_x3p)/(offense_fga - offense_x3pa),
                                   offense_drb_percent = 100 - defense_orb_percent)

#change team names in df to match with logo table
team_stats[32,1] = 'BYU'
team_stats[292,1] = 'SMU'
team_stats[289,1] = 'USC'
team_stats[268,1] = "Saint Mary's"
team_stats[335,1] = "VCU"


#get graphic info for schools that will be on our plot
our_schedule = kp_team_schedule(our_team, year=year) %>% mutate(opponent = clean_school_names(opponent))
team_info = cfbplotR::logo_ref %>% mutate(school = clean_school_names(school)) %>%
  filter(school %in% our_schedule$opponent | school %in% opponentList | school == our_team) 

combined = left_join(team_info, team_stats, by=c('school' = 'school'))

#select only the stats we are going to use from our df
combined = combined %>%
  select(school, logo, type, color, alt_color, wordmark, offense_o_rtg, defense_o_rtg, offense_x2p_percent, offense_x3p_percent,
         offense_x3p_ar, offense_ast_percent, offense_tov_percent, offense_stl_percent,
         offense_blk_percent, offense_orb_percent, offense_drb_percent)

#find medians of all vars for plot
team_stats_meds = data.frame()
for(c in 2:ncol(team_stats)){
  a = pull(team_stats, var = c)
  median(a)
  team_stats_meds[1,c-1] = median(a)
}
colnames(team_stats_meds) <- colnames(team_stats)[2:69]
team_stats_meds = team_stats_meds %>% select(offense_o_rtg, defense_o_rtg, offense_x2p_percent, offense_x3p_percent,
                                             offense_x3p_ar, offense_ast_percent, offense_tov_percent, offense_stl_percent,
                                             offense_blk_percent, offense_orb_percent, offense_drb_percent)

#read headshot url table
headshot_urls_db = read_xlsx("headshot_url.xlsx")

#list of options for filtering and sorting player personnel table
TableFilterList = c("All Players", "Guards", "Wings", "Bigs", "Starters")
TableSortList = c("MpG")
TableColumnList = c("Player Info", "Positional Info", "Shooting", "Playmaking", "Dribble Drive Direction", "Defense")
TableColumnListVecs = list(c("#", "Class", "Pos", "Height"), c("MpG"), c(), c(), c(), c())
alwaysShow = c("URL", "first", "last", "team")


rm(advanced_defense, advanced_offense, basic_defense, basic_offense, advanced_defense_url, advanced_offense_url, basic_defense_url, basic_offense_url, c, a, team_info)

#the list of options for our metric comparison plots
MetricCompList = c("ORTG x DRTG", "2P% x 3P%", "3PAR x 3P%", "AST% x TOV%", "STL% x BLK%", "OREB% x DREB%")































ui = navbarPage("Pre-Scout Portal", fluid = TRUE,
                tabPanel("Graphical Metric Comparison",
                         fluidRow(column(3, selectInput("opponent", "Opponent", opponentList)),
                                  column(6, h1(strong("Pre-Scout Portal")), uiOutput("header")),
                                  column(3, img(src = "logo_oregon.png", height = 180, width = 240))),
                         sidebarLayout(
                           sidebarPanel(radioButtons("whichGraph", "Metric Comparison", MetricCompList),
                                        checkboxInput("focus", "Focused View?")),
                           mainPanel(plotOutput("MetricComp", height = "600px")))),
                tabPanel("Player Personnel", 
                         fluidRow(column(3, selectInput("opponent", "Opponent", opponentList)),
                                  column(6, h1(strong("Pre-Scout Portal")), uiOutput("header2")),
                                  column(3, img(src = "logo_oregon.png", height = 180, width = 240))),
                         fluidRow(
                           column(4, selectInput("filterPersonnel", "Filter", TableFilterList), sliderInput("minMinsPPT", "Min/G Minimum", value=5, min=0, max=40), style = "background-color:#f5f5f5"),
                           column(4, selectInput("sortPersonnel", "Sort", TableSortList), sliderInput("minGamesPPT", "GP Minimum", value=0, min=0, max=30), style = "background-color:#f5f5f5"),
                           column(4, checkboxGroupInput("columnsPersonnel", "Visible Columns", choices = TableColumnList),style = "background-color:#f5f5f5")),
                         fluidRow(12, gt_output("PlayerPersonnel"))),
                tabPanel("Opponent Trends", h5("Under Construction")),
                tabPanel("Shot Charts", h5("Under Construction")),
                tabPanel("Lineups", h5("Under Construction"))
                
                
)


server = function(input, output, session) {
  #reactive expressions for changing opponent input
  opponentSRurl = reactive(opponentSRurl_db %>% filter(opponent == input$opponent) %>% .[[1,2]])
  SRopponentTables = reactive(read_html(opponentSRurl()) %>% html_table())
  #add aestetics to focus on the teams we want
  combined_df = reactive(combined %>% mutate(color2 = if_else(school == our_team | school == input$opponent, NA_character_ ,"b/w"),
                                             alpha = if_else(school == our_team | school == input$opponent, 1, .6)))
  
  #find player position
  opp_pos = reactive(kp_team_depth_chart(input$opponent, year = year))
  opp_pg = reactive(opp_pos() %>% select(first = pg_player_first_name, last = pg_player_last_name, min_pct = pg_min_pct) %>% mutate(pos="PG") %>% filter(!is.na(first)))
  opp_sg = reactive(opp_pos() %>% select(first = sg_player_first_name, last = sg_player_last_name, min_pct = sg_min_pct) %>% mutate(pos="SG") %>% filter(!is.na(first)))
  opp_sf = reactive(opp_pos() %>% select(first = sf_player_first_name, last = sf_player_last_name, min_pct = sf_min_pct) %>% mutate(pos="SF") %>% filter(!is.na(first)))
  opp_pf = reactive(opp_pos() %>% select(first = pf_player_first_name, last = pf_player_last_name, min_pct = pf_min_pct) %>% mutate(pos="PF") %>% filter(!is.na(first)))
  opp_c = reactive(opp_pos() %>% select(first = c_player_first_name, last = c_player_last_name, min_pct = c_min_pct) %>% mutate(pos="C") %>% filter(!is.na(first)))
  opp_pos2 = reactive(rbind(opp_pg(), opp_sg(), opp_sf(), opp_pf(), opp_c()) %>% pivot_wider(names_from = pos, values_from = min_pct, values_fill = 0) %>% mutate(total=PG+SG+SF+PF+C) %>%
                        mutate(PG = PG/total, SG=SG/total, SF=SF/total, PF=PF/total, C=C/total) %>% .[,-8] %>%
                        #combine first and last name then separate so that any extra suffixes are handled the same way as they are when pulling from SR
                        mutate(Player = paste(first, last, sep=" ")) %>% select(Player, PG, SG, SF, PF, C) %>% separate(Player, into = c("first","last"), extra = "drop") %>% mutate(last = str_to_title(last))) 
  
  #get starters from opponent's last game
  lastGdate = reactive(kp_team_schedule(input$opponent, year=year) %>% arrange(desc(date)) %>% .[[1,18]])
  opponentGID = reactive(espn_mbb_scoreboard(lastGdate()) %>% filter(home_team_location == input$opponent | away_team_location == input$opponent) %>% .[[1,6]])
  lastGstarters = reactive(espn_mbb_player_box(opponentGID()) %>% filter(starter==TRUE) %>% filter(team_short_display_name==input$opponent) %>% select(athlete_display_name, starter) %>% separate(athlete_display_name, into = c("first","last"), extra = "drop") %>% mutate(last = str_to_title(last)))
  
  #base table for personnel output
  PPtable_raw = reactive(SRopponentTables()[[1]] %>% mutate(team=input$opponent) %>%
                           select(team, Player, "#", Class, Pos, Height) %>%
                           separate(Player, into = c("first","last"), extra = "drop") %>%
                           mutate(last = str_to_title(last)) %>% 
                           right_join(opp_pos2(), by=c('last')) %>% select(-first.x) %>%
                           #create dummy variables for the TableFilterList to filter by
                           mutate("All Players" = 1,
                                  "Guards" = ifelse(PG+SG>.6, 1, 0),
                                  "Bigs" = ifelse(C+PF>.75 & C>0 , 1, 0),
                                  "Wings" = ifelse(Guards+Bigs==0, 1, 0)) %>%
                           left_join(lastGstarters(), by=c('last')) %>% select(-first) %>%
                           mutate("Starters" = ifelse(starter==TRUE, 1, 0)) %>% select(-starter) %>%
                           left_join(SRopponentTables()[[6]] %>% select(Player, "MpG" = MP) %>% separate(Player, into = c("first","last"), extra = "drop") %>% mutate(last = str_to_title(last)), by=c('last')) %>% 
                           select(-first.y) %>% relocate(first, .before = last))
  
  headshot_urls = reactive(headshot_urls_db %>%
                             separate(Player, into = c("first","last"), extra = "drop") %>%
                             mutate(last = str_to_title(last)))
  #join headshots to PPtable
  PPtable_raw2 = reactive(left_join(PPtable_raw(), headshot_urls(), by=c("first", "last", "team"="Team")))
  
  #Set x var and y var for axis labels
  xvar = reactive(str_split(input$whichGraph, " x ")[[1]][1])
  yvar = reactive(str_split(input$whichGraph, " x ")[[1]][2])
  
  #set x var and y var for values
  xvardf = reactive(if(input$whichGraph == "ORTG x DRTG"){combined_df()[,'offense_o_rtg']} 
                    else if(input$whichGraph == "2P% x 3P%"){combined_df()[,'offense_x2p_percent']}
                    else if(input$whichGraph == "3PAR x 3P%"){combined_df()[,'offense_x3p_ar']}
                    else if(input$whichGraph == "AST% x TOV%"){combined_df()[,'offense_ast_percent']}
                    else if(input$whichGraph == "STL% x BLK%"){combined_df()[,'offense_stl_percent']}
                    else if(input$whichGraph == "OREB% x DREB%"){combined_df()[,'offense_orb_percent']})
  yvardf = reactive(if(input$whichGraph == "ORTG x DRTG"){combined_df()[,'defense_o_rtg']}
                    else if(input$whichGraph == "2P% x 3P%"){combined_df()[,'offense_x3p_percent']}
                    else if(input$whichGraph == "3PAR x 3P%"){combined_df()[,'offense_x3p_percent']}
                    else if(input$whichGraph == "AST% x TOV%"){combined_df()[,'offense_tov_percent']}
                    else if(input$whichGraph == "STL% x BLK%"){combined_df()[,'offense_blk_percent']}
                    else if(input$whichGraph == "OREB% x DREB%"){combined_df()[,'offense_drb_percent']})
  #set x var and y var for means
  xvar_med = reactive(if(input$whichGraph == "ORTG x DRTG"){team_stats_meds$offense_o_rtg} 
                      else if(input$whichGraph == "2P% x 3P%"){team_stats_meds$offense_x2p_percent}
                      else if(input$whichGraph == "3PAR x 3P%"){team_stats_meds$offense_x3p_ar}
                      else if(input$whichGraph == "AST% x TOV%"){team_stats_meds$offense_ast_percent}
                      else if(input$whichGraph == "STL% x BLK%"){team_stats_meds$offense_stl_percent}
                      else if(input$whichGraph == "OREB% x DREB%"){team_stats_meds$offense_orb_percent})
  yvar_med = reactive(if(input$whichGraph == "ORTG x DRTG"){team_stats_meds$defense_o_rtg}
                      else if(input$whichGraph == "2P% x 3P%"){team_stats_meds$offense_x3p_percent}
                      else if(input$whichGraph == "3PAR x 3P%"){team_stats_meds$offense_x3p_percent}
                      else if(input$whichGraph == "AST% x TOV%"){team_stats_meds$offense_tov_percent}
                      else if(input$whichGraph == "STL% x BLK%"){team_stats_meds$offense_blk_percent}
                      else if(input$whichGraph == "OREB% x DREB%"){team_stats_meds$offense_drb_percent})
  
  #invert y axis depending on which metric comp is selected
  flip_yvar = reactive(ifelse((yvar()=="DRTG" | yvar()=="TOV%"), "reverse", "identity"))
  
  #filter and sort PPtable_raw before making it a gt object
  selected_cols = reactive({
    tibble(TCL = TableColumnList, TCLV = TableColumnListVecs) %>%
      filter(TCL %in% input$columnsPersonnel) %>%
      pull(TCLV) %>%
      unlist()
  })
  
  PPtable = reactive(PPtable_raw2() %>% 
                       filter(.data[[input$filterPersonnel]] == 1) %>%
                       filter(.data[["MpG"]] > input$minMinsPPT) %>%
                       arrange(desc(.data[[input$sortPersonnel]])) %>%
                       select(alwaysShow, all_of(selected_cols())))
  
  output$header = renderUI(HTML(paste('<h1 style="color:green;font-size:50px">', our_team, " vs ", input$opponent, '</h1>', sep = "")))
  output$header2 = renderUI(HTML(paste('<h1 style="color:green;font-size:50px">', our_team, " vs ", input$opponent, '</h1>', sep = "")))
  
  output$MetricComp = renderPlot({
    if(input$focus==F){
      ggplot() + scale_y_continuous(trans = flip_yvar()) +
        geom_hline(yintercept = median(yvar_med()), color = "red", linetype = "dashed") +
        geom_vline(xintercept = median(xvar_med()), color = "red", linetype = "dashed") +
        geom_cfb_logos(data = combined_df(),
                       aes(x = unlist(xvardf()), y = unlist(yvardf()), team=school), width = .05) +
        xlab(xvar()) + ylab(yvar()) +
        theme_bw()}
    else{
      ggplot() + scale_y_continuous(trans = flip_yvar()) +
        geom_hline(yintercept = median(yvar_med()), color = "red", linetype = "dashed") +
        geom_vline(xintercept = median(xvar_med()), color = "red", linetype = "dashed") +
        geom_cfb_logos(data = combined_df(),
                       aes(x = unlist(xvardf()), y = unlist(yvardf()), team=school, alpha=alpha, color=color2), width = .05) +
        scale_alpha_identity() + scale_color_identity() +
        xlab(xvar()) + ylab(yvar()) +
        theme_bw()}
  })
  
  output$PlayerPersonnel = render_gt({
    PPtable() %>% gt() %>%
      gt_merge_stack_team_color(first, last, team) %>%
      gt_img_rows(columns = URL, img_source = "web", height = 90) %>%
      cols_hide(team) %>% cols_label(URL = "",
                                     first = "Name")
  })
  
}

shinyApp(ui, server)