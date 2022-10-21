library(shiny)
library(pacman)
p_load(rvest, tidyverse, janitor, cfbplotR, stringr, gt)
p_load_current_gh("sportsdataverse/hoopR", dependencies = TRUE, update = TRUE)

our_team = "Oregon"
opponent = "Texas A&M"
opponentSRurl = "https://www.sports-reference.com/cbb/schools/texas-am/2022.html"

SRopponentTables = read_html(opponentSRurl) %>% html_table()


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

#change team names in df to match with logo table
team_stats[32,1] = 'BYU'
team_stats[292,1] = 'SMU'
team_stats[289,1] = 'USC'
team_stats[268,1] = "Saint Mary's"

#convert stats into usable numeric stats
for(c in 2:67){team_stats[,c] = team_stats[,c] %>% as.numeric()}

#make all stats we want using other stats
team_stats = team_stats %>% mutate(offense_x2p_percent = (offense_fg - offense_x3p)/(offense_fga - offense_x3pa),
                                   offense_drb_percent = 100 - defense_orb_percent)


rm(advanced_defense, advanced_offense, basic_defense, basic_offense, advanced_defense_url, advanced_offense_url, basic_defense_url, basic_offense_url, c)

#get graphic info for schools that will be on our plot
team_info = cfbplotR::logo_ref %>% 
  filter(school %in% c('Oregon','Texas Southern','SMU', 'BYU', 'Chaminade', "Saint Mary's", 'Houston', 'Montana', 'UC Riverside', 'Arizona State', 'Stanford', 'Portland', 'Baylor', 'Pepperdine', 'Utah', 'Oregon State', 'UCLA', 'USC', 'Washington', 'Colorado', 'California', 'Washington State', 'Arizona', 'Utah State', 'Texas A&M'))

combined = left_join(team_info, team_stats, by=c('school' = 'school'))

#the list of options for our metric comparison plots
MetricCompList = c("ORTG x DRTG", "2P% x 3P%", "3PAR x 3P%", "AST% x TOV%", "STL% x BLK%", "OREB% x DREB%")


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

rm(a,c, team_info)

#add aestetics to focus on the teams we want
combined = combined %>% mutate(color2 = if_else(school == our_team | school == opponent, NA_character_ ,"b/w"),
                               alpha = if_else(school == our_team | school == opponent, 1, .6))

#list of options for filtering and sorting player personnel table
TableFilterList = c("All Players", "Guards", "Wings", "Bigs", "Starters")
TableSortList = c()

#find player position
opp_pos = kp_team_depth_chart(opponent, year = 2022)
opp_pg = opp_pos %>% select(first = pg_player_first_name, last = pg_player_last_name, min_pct = pg_min_pct) %>% mutate(pos="PG") %>% filter(!is.na(first))
opp_sg = opp_pos %>% select(first = sg_player_first_name, last = sg_player_last_name, min_pct = sg_min_pct) %>% mutate(pos="SG") %>% filter(!is.na(first))
opp_sf = opp_pos %>% select(first = sf_player_first_name, last = sf_player_last_name, min_pct = sf_min_pct) %>% mutate(pos="SF") %>% filter(!is.na(first))
opp_pf = opp_pos %>% select(first = pf_player_first_name, last = pf_player_last_name, min_pct = pf_min_pct) %>% mutate(pos="PF") %>% filter(!is.na(first))
opp_c = opp_pos %>% select(first = c_player_first_name, last = c_player_last_name, min_pct = c_min_pct) %>% mutate(pos="C") %>% filter(!is.na(first))
opp_pos = rbind(opp_pg, opp_sg, opp_sf, opp_pf, opp_c) %>% pivot_wider(names_from = pos, values_from = min_pct, values_fill = 0) %>% mutate(total=PG+SG+SF+PF+C) %>%
  mutate(PG = PG/total, SG=SG/total, SF=SF/total, PF=PF/total, C=C/total) %>% .[,-8] %>%
  #combine first and last name then separate so that any extra suffixes are handled the same way as they are when pulling from SR
  mutate(Player = paste(first, last, sep=" ")) %>% select(Player, PG, SG, SF, PF, C) %>% separate(Player, into = c("first","last"), extra = "drop")
rm(opp_pg, opp_sg, opp_sf, opp_pf, opp_c)


#base table for personnel output
PPtable_raw = SRopponentTables[[1]] %>% mutate(team=opponent) %>%
  select(team, Player, "#", Class, Pos, Height) %>%
  separate(Player, into = c("first","last"), extra = "drop") %>%
  right_join(opp_pos, by=c('first', 'last')) %>%
  #create dummy variables for the TableFilterList to filter by
  mutate("All Players" = 1)





ui = navbarPage("Pre-Scout Portal", fluid = TRUE,
                tabPanel("Graphical Metric Comparisons",
                         fluidRow(column(9, h1(strong("Pre-Scout Portal"), align="center", style="color:green"),
                                         h1("Oregon @ Texas A&M - NIT Round 2", align="center", style="color:green"),
                                         h3("March 19, 2022", align="center", style="color:green")),
                                  column(3, img(src = "logo_oregon.png", height = 180, width = 240))),
                         sidebarLayout(
                           sidebarPanel(radioButtons("whichGraph", "Metric Comparison", MetricCompList)),
                           mainPanel(plotOutput("MetricComp", height = "600px")))
                         ),
                
                tabPanel("Player Personnel", 
                         fluidRow(column(9, h1(strong("Pre-Scout Portal"), align="center", style="color:green"),
                                         h1("Oregon @ Texas A&M - NIT Round 2", align="center", style="color:green"),
                                         h3("March 19, 2022", align="center", style="color:green")),
                                  column(3, img(src = "logo_oregon.png", height = 180, width = 240))),
                         fluidRow(
                           column(3, selectInput("filterPersonnel", "Filter", TableFilterList), style = "background-color:#f5f5f5"),
                           column(3, selectInput("sortPersonnel", "Sort", TableFilterList), style = "background-color:#f5f5f5")),
                         fluidRow(12, gt_output("PlayerPersonnel"))
                         ),
                tabPanel("Opponent Trends", h5("Coming Soon")),
                tabPanel("Shot Charts", h5("Coming Soon")),
                tabPanel("Lineups", h5("Coming Soon"))
                )

server = function(input, output, session) {
  #Set x var and y var for axis labels
  xvar = reactive(str_split(input$whichGraph, " x ")[[1]][1])
  yvar = reactive(str_split(input$whichGraph, " x ")[[1]][2])
  #set x var and y var for values
  xvardf = reactive(if(input$whichGraph == "ORTG x DRTG"){combined$offense_o_rtg} 
                    else if(input$whichGraph == "2P% x 3P%"){combined$offense_x2p_percent}
                    else if(input$whichGraph == "3PAR x 3P%"){combined$offense_x3p_ar}
                    else if(input$whichGraph == "AST% x TOV%"){combined$offense_ast_percent}
                    else if(input$whichGraph == "STL% x BLK%"){combined$offense_stl_percent}
                    else if(input$whichGraph == "OREB% x DREB%"){combined$offense_orb_percent})
  yvardf = reactive(if(input$whichGraph == "ORTG x DRTG"){combined$defense_o_rtg}
                    else if(input$whichGraph == "2P% x 3P%"){combined$offense_x3p_percent}
                    else if(input$whichGraph == "3PAR x 3P%"){combined$offense_x3p_percent}
                    else if(input$whichGraph == "AST% x TOV%"){combined$offense_tov_percent}
                    else if(input$whichGraph == "STL% x BLK%"){combined$offense_blk_percent}
                    else if(input$whichGraph == "OREB% x DREB%"){combined$offense_drb_percent})
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
  
  
  
    
  output$MetricComp = renderPlot({
    ggplot() + scale_y_continuous(trans = flip_yvar()) + 
      geom_hline(yintercept = median(yvar_med()), color = "red", linetype = "dashed") + 
      geom_vline(xintercept = median(xvar_med()), color = "red", linetype = "dashed") +
      geom_cfb_logos(aes(x = xvardf(), y = yvardf(), team=combined$school), width = .05) +
      xlab(xvar()) + ylab(yvar()) +
      theme_bw()
  })
  
  output$PlayerPersonnel = render_gt({
    PPtable
  })
}

shinyApp(ui, server)