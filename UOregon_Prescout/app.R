library(shiny)
library(pacman)
p_load(rvest, tidyverse, janitor, cfbplotR, stringr, gt, gtExtras, hoopR, paletteer, toRvik, ggtext, readxl)
#remotes::install_github("sportsdataverse/hoopR")

our_team = "Oregon"
opponentList = c("Houston", "UCONN", "Alabama", "Michigan State", "Washington State", "UCLA")
year=2023

#load in data
GMC_AP = read.csv("data/GMC_AP.csv")
GMC_OS = read.csv("data/GMC_OS.csv")
GMC_NET = read.csv("data/GMC_NET.csv")
GMC_medians = read.csv("data/GMC_medians.csv")
graphic_info_OS = read.csv("data/graphic_info_OS.csv")
PPT_data = read.csv("data/PPT_data.csv", check.names = FALSE)
OO_splits_data = read.csv("data/OO_splits_data.csv")
Opp_Trends_df = read.csv("data/Opp_Trends_df.csv")

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
  
  #creates a variable for opponent name when referring to kp functions
  opponent_kp = reactive(if(input$opponent == "UCONN"){"Connecticut"}
                         else{gsub(" State", " St.", input$opponent)})
  
  
  #filter and sort PPtable_raw before making it a gt object
  filter_PPT = reactive({
    tibble(TCL = PPT_ColumnList, TCLV = PPT_ColumnListVecs) %>%
      filter(TCL %in% input$columnsPPT) %>%
      pull(TCLV) %>%
      unlist()
  })
  
  PPT_data_filtered = reactive(PPT_data %>% filter(Team == input$opponent))
  
  sort_PPT = reactive(PPT_data_filtered() %>% 
                        filter(.data[[input$filterPPT]] == 1) %>%
                        filter(.data[["GP"]] >= input$minGP_PPT_in) %>%
                        filter(.data[["MPG"]] >= input$minMinsPPT) %>%
                        arrange(desc(.data[[input$sortPPT]])) %>%
                        select(PPT_alwaysShow, all_of(filter_PPT())))
  
  #find how many games the opponent has played this season
  opp_n_games = reactive(kp_team_schedule(opponent_kp(), year) %>% 
    filter(!is.na(w)) %>%
    nrow() %>% as.numeric())
  
  #this line is working but causes an error to show every time the app is run
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
  
  #how many columns are in our mostly finished gt table for alternating cell shading purposes
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
                                                     domain = PPT_data[[input$sortPPT]]))
  
  opp_conf = reactive(kp_program_ratings() %>%
                        #manually change the abbreviation of certain conferences
                        mutate(conf = ifelse(conf == "Amer", "AAC", conf)) %>%
                        filter(team == opponent_kp()) %>%
                        .[[1,3]])
  
  #for each split, find the needed mean given the team and stat inputs
  recency5_mean = reactive(OO_splits_data %>% 
                             filter(Team == input$opponent,
                                    split == 'recency5',
                                    variable == input$trendingStat) %>%
                             select(stats) %>% colMeans(na.rm = T) %>% .[[1]])
  
  recency10_mean = reactive(OO_splits_data %>% 
                              filter(Team == input$opponent,
                                    split == 'recency10',
                                    variable == input$trendingStat) %>%
                             select(stats) %>% colMeans(na.rm = T) %>% .[[1]])
  
  home_mean = reactive(OO_splits_data %>% 
                              filter(Team == input$opponent,
                                     split == 'home',
                                     variable == input$trendingStat) %>%
                              select(stats) %>% colMeans(na.rm = T) %>% .[[1]])
  
  away_mean = reactive(OO_splits_data %>% 
                              filter(Team == input$opponent,
                                     split == 'away',
                                     variable == input$trendingStat) %>%
                              select(stats) %>% colMeans(na.rm = T) %>% .[[1]])
  
  net50_mean = reactive(OO_splits_data %>% 
                              filter(Team == input$opponent,
                                     split == 'net50',
                                     variable == input$trendingStat) %>%
                              select(stats) %>% colMeans(na.rm = T) %>% .[[1]])
  
  net100_mean = reactive(OO_splits_data %>% 
                              filter(Team == input$opponent,
                                     split == 'net100',
                                     variable == input$trendingStat) %>%
                              select(stats) %>% colMeans(na.rm = T) %>% .[[1]])
  
  wins_mean = reactive(OO_splits_data %>% 
                              filter(Team == input$opponent,
                                     split == 'wins',
                                     variable == input$trendingStat) %>%
                              select(stats) %>% colMeans(na.rm = T) %>% .[[1]])
  
  losses_mean = reactive(OO_splits_data %>% 
                              filter(Team == input$opponent,
                                     split == 'losses',
                                     variable == input$trendingStat) %>%
                              select(stats) %>% colMeans(na.rm = T) %>% .[[1]])
  
  conf_mean = reactive(OO_splits_data %>% 
                              filter(Team == input$opponent,
                                     split == 'conf',
                                     variable == input$trendingStat) %>%
                              select(stats) %>% colMeans(na.rm = T) %>% .[[1]])
  
  #limit teams on the trends graph to games that have been played already and the next three games
  Opp_Trends_df_filtered = reactive(Opp_Trends_df %>%
                                      filter(Team == input$opponent) %>%
                                      select(-Team))
  
  #create columns so we can access date info and order of games for easier shading
  Opp_Trends_df_dates = reactive(Opp_Trends_df_filtered() %>%
    mutate(month = substr(game_date, 5, 6),
           day = substr(game_date, 7, 8),
           game_num = seq(1, nrow(Opp_Trends_df_filtered()))))
  
  #find max and min games of months that need shading
  dec_min = reactive(Opp_Trends_df_dates() %>% filter(month==12) %>% arrange(day) %>% .[1, "game_num"] %>% as.numeric() %>% sum(-.5))
  dec_max = reactive(Opp_Trends_df_dates() %>% filter(month==12) %>% arrange(desc(day)) %>% .[1,"game_num"] %>% as.numeric() %>% sum(.5))
  feb_min = reactive(Opp_Trends_df_dates() %>% filter(month=="02") %>% arrange(day) %>% .[1,"game_num"] %>% as.numeric() %>% sum(-.5))
  feb_max = reactive(Opp_Trends_df_dates() %>% filter(month=="02") %>% arrange(desc(day)) %>% .[1,"game_num"] %>% as.numeric() %>% sum(.5))
  
  #set min and maxs for y axis 
  OO_ymin = reactive(ifelse(min(Opp_Trends_df_filtered()[,input$trendingStat]) >= 0,
                            min(Opp_Trends_df_filtered()[,input$trendingStat]) - min(Opp_Trends_df_filtered()[,input$trendingStat]) * .15,
                            min(Opp_Trends_df_filtered()[,input$trendingStat]) + min(Opp_Trends_df_filtered()[,input$trendingStat]) * .05))
  
  OO_ymax = reactive(ifelse(max(Opp_Trends_df_filtered()[,input$trendingStat]) >= 0,
                            max(Opp_Trends_df_filtered()[,input$trendingStat]) + max(Opp_Trends_df_filtered()[,input$trendingStat]) * .05,
                            max(Opp_Trends_df_filtered()[,input$trendingStat]) - max(Opp_Trends_df_filtered()[,input$trendingStat]) * .05))
  
  output$OppTrends = renderPlot(
    
    if(input$trendSplits == "OFF"){
      
      if(input$trendIndicators == "OFF"){
        ggplot(data = Opp_Trends_df_filtered(), aes(x=reorder(game_code, game_date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df_filtered()$opponent) + 
          xlab("") + ylab(gsub("pct", "%", gsub("_", " ", input$trendingStat))) + coord_cartesian(ylim = c(OO_ymin(), OO_ymax())) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2),
                            breaks = c("W", "L")) +
          guides(fill = guide_legend(title = NULL)) +
          theme_bw() + theme(axis.text.x = element_cfb_logo(size=1.5),
                             legend.position = "bottom",
                             legend.direction = "horizontal")}
      
      else if(input$trendIndicators == "Location"){
        ggplot(data = Opp_Trends_df_filtered(), aes(x=reorder(game_code, game_date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df_filtered()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) + coord_cartesian(ylim = c(OO_ymin(), OO_ymax())) + 
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2),
                            breaks = c("W", "L")) +
          scale_colour_manual(values = c("Home" = color3,
                                         "Away" = color4),
                              breaks = c("Home", "Away")) +
          guides(fill = guide_legend(title = NULL, order=1),
                 color = guide_legend(title = NULL, order=2)) +
          theme_bw() + theme(axis.text.x = element_cfb_logo(size=1.5),
                             legend.position = "bottom",
                             legend.direction = 'horizontal') +
          geom_point(aes_string(y=input$trendingStat, colour = "location"), shape = 18, size = 4)}
      
      else if(input$trendIndicators == "NET"){
        ggplot(data = Opp_Trends_df_filtered(), aes(x=reorder(game_code, game_date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df_filtered()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) + coord_cartesian(ylim = c(OO_ymin(), OO_ymax())) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2),
                            breaks = c("W", "L")) +
          scale_colour_manual(values = c("NET Top 50" = color3,
                                         "NET Top 100" = color4),
                              breaks = c("NET Top 50", "NET Top 100")) +
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
        ggplot(data = Opp_Trends_df_filtered(), aes(x=reorder(game_code, game_date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df_filtered()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) + coord_cartesian(ylim = c(OO_ymin(), OO_ymax())) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2),
                            breaks = c("W", "L")) +
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
        ggplot(data = Opp_Trends_df_filtered(), aes(x=reorder(game_code, game_date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df_filtered()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) + coord_cartesian(ylim = c(OO_ymin(), OO_ymax())) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2),
                            breaks = c("W", "L")) +
          scale_linetype_manual(paste0("Average ", gsub("_", " ", input$trendingStat)), 
                                values = c("All Games" = "longdash")) +
          guides(fill = guide_legend(title = NULL, order=2),
                 linetype = guide_legend(keywidth = 5, title.position = "top", title.hjust = .5, order=1)) +
          theme_bw() + theme(axis.text.x = element_cfb_logo(size=1.5),
                             legend.position = "bottom",
                             legend.direction = "horizontal") +
          geom_hline(aes(yintercept = mean(Opp_Trends_df_filtered()[,input$trendingStat], na.rm=T),
                         linetype="All Games"), color = "black" , size = 1)}
      
      else if(input$trendIndicators == "Location"){
        ggplot(data = Opp_Trends_df_filtered(), aes(x=reorder(game_code, game_date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df_filtered()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) + coord_cartesian(ylim = c(OO_ymin(), OO_ymax())) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2),
                            breaks = c("W", "L")) +
          scale_colour_manual(values = c("Home" = color3,
                                         "Away" = color4),
                              breaks = c("Home", "Away")) +
          scale_linetype_manual(paste0("Average ", gsub("_", " ", input$trendingStat)), 
                                values = c("All Games" = "longdash")) +
          guides(fill = guide_legend(title = NULL, order=2),
                 color = guide_legend(title = NULL, order=3),
                 linetype = guide_legend(keywidth = 5, title.position = "top", title.hjust = .5, order=1)) +
          theme_bw() + theme(axis.text.x = element_cfb_logo(size=1.5),
                             legend.position = "bottom",
                             legend.direction = 'horizontal') +
          geom_hline(aes(yintercept = mean(Opp_Trends_df_filtered()[,input$trendingStat], na.rm=T),
                         linetype="All Games"), color = "black" , size = 1) +
          geom_point(aes_string(y=input$trendingStat, colour = "location"), shape = 18, size = 4)}
      
      else if(input$trendIndicators == "NET"){
        ggplot(data = Opp_Trends_df_filtered(), aes(x=reorder(game_code, game_date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df_filtered()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) + coord_cartesian(ylim = c(OO_ymin(), OO_ymax())) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2),
                            breaks = c("W", "L")) +
          scale_colour_manual(values = c("NET Top 50" = color3,
                                         "NET Top 100" = color4),
                              breaks = c("NET Top 50", "NET Top 100")) +
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
          geom_hline(aes(yintercept = mean(Opp_Trends_df_filtered()[,input$trendingStat], na.rm=T),
                         linetype="All Games"), color = "black" , size = 1) +
          geom_point(aes_string(y=input$trendingStat, colour = "net_rk", size = "net_rk"), shape = 18)}
      
      else if(input$trendIndicators == "Conference"){
        ggplot(data = Opp_Trends_df_filtered(), aes(x=reorder(game_code, game_date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df_filtered()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) + coord_cartesian(ylim = c(OO_ymin(), OO_ymax())) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2),
                            breaks = c("W", "L")) +
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
          geom_hline(aes(yintercept = mean(Opp_Trends_df_filtered()[,input$trendingStat], na.rm=T),
                         linetype="All Games"), color = "black" , size = 1) +
          geom_point(aes_string(y=input$trendingStat, colour = "conference_game", size = "conference_game"), shape = 18)}
    }
    
    else if(input$trendSplits == "Recency"){
      
      if(input$trendIndicators == "OFF"){
        ggplot(data = Opp_Trends_df_filtered(), aes(x=reorder(game_code, game_date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df_filtered()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) + coord_cartesian(ylim = c(OO_ymin(), OO_ymax())) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2),
                            breaks = c("W", "L")) +
          scale_linetype_manual(name = paste0("Average ", gsub("_", " ", input$trendingStat)), 
                                values = c("All Games" = "longdash", 
                                           "Last 5 Games" = "dashed",
                                           "Last 10 Games" = "dashed"),
                                breaks = c("All Games", "Last 5 Games", "Last 10 Games")) +
          guides(fill = guide_legend(title = NULL, order=2),
                 linetype = guide_legend(keywidth = 3, title.position = "top", title.hjust = .5, order=1,
                                         override.aes = list(colour=c("black", color5, color6)))) +
          theme_bw() + theme(axis.text.x = element_cfb_logo(size=1.5),
                             legend.position = "bottom",
                             legend.direction = "horizontal") +
          geom_hline(aes(yintercept = mean(Opp_Trends_df_filtered()[,input$trendingStat], na.rm=T),
                         linetype="All Games"), color = "black" , size = 1) +
          geom_hline(aes(yintercept = recency5_mean(),
                         linetype="Last 5 Games"), color = color5 , size = 1) +
          geom_hline(aes(yintercept = recency10_mean(),
                         linetype="Last 10 Games"), color = color6 , size = 1)}
      
      else if(input$trendIndicators == "Location"){
        ggplot(data = Opp_Trends_df_filtered(), aes(x=reorder(game_code, game_date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df_filtered()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) + coord_cartesian(ylim = c(OO_ymin(), OO_ymax())) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2),
                            breaks = c("W", "L")) +
          scale_colour_manual(values = c("Home" = color3,
                                         "Away" = color4),
                              breaks = c("Home", "Away")) +
          scale_linetype_manual(name = paste0("Average ", gsub("_", " ", input$trendingStat)), 
                                values = c("All Games" = "longdash",
                                           "Last 5 Games" = "dashed",
                                           "Last 10 Games" = "dashed"),
                                breaks = c("All Games", "Last 5 Games", "Last 10 Games")) +
          guides(fill = guide_legend(title = NULL, order=2),
                 color = guide_legend(title = NULL, order=3),
                 linetype = guide_legend(keywidth = 5, title.position = "top", title.hjust = .5, order=1,
                                         override.aes = list(colour=c("black", color5, color6)))) +
          theme_bw() + theme(axis.text.x = element_cfb_logo(size=1.5),
                             legend.position = "bottom",
                             legend.direction = 'horizontal') +
          geom_hline(aes(yintercept = mean(Opp_Trends_df_filtered()[,input$trendingStat], na.rm=T),
                         linetype="All Games"), color = "black" , size = 1) +
          geom_point(aes_string(y=input$trendingStat, colour = "location"), shape = 18, size = 4) +
          geom_hline(aes(yintercept = recency5_mean(),
                         linetype="Last 5 Games"), color = color5 , size = 1) +
          geom_hline(aes(yintercept = recency10_mean(),
                         linetype="Last 10 Games"), color = color6 , size = 1)}
      
      else if(input$trendIndicators == "NET"){
        ggplot(data = Opp_Trends_df_filtered(), aes(x=reorder(game_code, game_date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df_filtered()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) + coord_cartesian(ylim = c(OO_ymin(), OO_ymax())) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2),
                            breaks = c("W", "L")) +
          scale_colour_manual(values = c("NET Top 50" = color3,
                                         "NET Top 100" = color4),
                              breaks = c("NET Top 50", "NET Top 100")) +
          scale_linetype_manual(name = paste0("Average ", gsub("_", " ", input$trendingStat)), 
                                values = c("All Games" = "longdash",
                                           "Last 5 Games" = "dashed",
                                           "Last 10 Games" = "dashed"),
                                breaks = c("All Games", "Last 5 Games", "Last 10 Games")) +
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
          geom_hline(aes(yintercept = mean(Opp_Trends_df_filtered()[,input$trendingStat], na.rm=T),
                         linetype="All Games"), color = "black" , size = 1) +
          geom_point(aes_string(y=input$trendingStat, colour = "net_rk", size = "net_rk"), shape = 18) +
          geom_hline(aes(yintercept = recency5_mean(),
                         linetype="Last 5 Games"), color = color5 , size = 1) +
          geom_hline(aes(yintercept = recency10_mean(),
                         linetype="Last 10 Games"), color = color6 , size = 1)}
      
      else if(input$trendIndicators == "Conference"){
        ggplot(data = Opp_Trends_df_filtered(), aes(x=reorder(game_code, game_date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df_filtered()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) + coord_cartesian(ylim = c(OO_ymin(), OO_ymax())) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2),
                            breaks = c("W", "L")) +
          scale_colour_manual(values = c(color3, color3),
                              limits = c(opp_conf())) +
          scale_linetype_manual(name = paste0("Average ", gsub("_", " ", input$trendingStat)), 
                                values = c("All Games" = "longdash",
                                           "Last 5 Games" = "dashed",
                                           "Last 10 Games" = "dashed"),
                                breaks = c("All Games", "Last 5 Games", "Last 10 Games")) +
          scale_size_manual(values = c(4), limits = c(opp_conf())) +
          guides(fill = guide_legend(title = NULL, order=2),
                 color = guide_legend(title = NULL, order=3),
                 linetype = guide_legend(keywidth = 5, title.position = "top", title.hjust = .5, order=1,
                                         override.aes = list(colour=c("black", color5, color6))),
                 size = FALSE) +
          theme_bw() + theme(axis.text.x = element_cfb_logo(size=1.5),
                             legend.position = "bottom",
                             legend.direction = 'horizontal') +
          geom_hline(aes(yintercept = mean(Opp_Trends_df_filtered()[,input$trendingStat], na.rm=T),
                         linetype="All Games"), color = "black" , size = 1) +
          geom_point(aes_string(y=input$trendingStat, colour = "conference_game", size = "conference_game"), shape = 18) +
          geom_hline(aes(yintercept = recency5_mean(),
                         linetype="Last 5 Games"), color = color5 , size = 1) +
          geom_hline(aes(yintercept = recency10_mean(),
                         linetype="Last 10 Games"), color = color6 , size = 1)}
    }
    
    else if(input$trendSplits == "Location"){
      
      if(input$trendIndicators == "OFF"){
        ggplot(data = Opp_Trends_df_filtered(), aes(x=reorder(game_code, game_date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df_filtered()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) + coord_cartesian(ylim = c(OO_ymin(), OO_ymax())) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2),
                            breaks = c("W", "L")) +
          scale_linetype_manual(name = paste0("Average ", gsub("_", " ", input$trendingStat)), 
                                values = c("All Games" = "longdash", 
                                           "Home" = "dashed",
                                           "Away" = "dashed"),
                                breaks = c("All Games", "Home", "Away")) +
          guides(fill = guide_legend(title = NULL, order=2),
                 linetype = guide_legend(keywidth = 3, title.position = "top", title.hjust = .5, order=1,
                                         override.aes = list(colour=c("black", color5, color6)))) +
          theme_bw() + theme(axis.text.x = element_cfb_logo(size=1.5),
                             legend.position = "bottom",
                             legend.direction = "horizontal") +
          geom_hline(aes(yintercept = mean(Opp_Trends_df_filtered()[,input$trendingStat], na.rm=T),
                         linetype="All Games"), color = "black" , size = 1) +
          geom_hline(aes(yintercept = home_mean(),
                         linetype="Home"), color = color5 , size = 1) +
          geom_hline(aes(yintercept = away_mean(),
                         linetype="Away"), color = color6 , size = 1)}
      
      else if(input$trendIndicators == "Location"){
        ggplot(data = Opp_Trends_df_filtered(), aes(x=reorder(game_code, game_date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df_filtered()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) + coord_cartesian(ylim = c(OO_ymin(), OO_ymax())) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2),
                            breaks = c("W", "L")) +
          scale_colour_manual(values = c("Home" = color3,
                                         "Away" = color4),
                              breaks = c("Home", "Away")) +
          scale_linetype_manual(name = paste0("Average ", gsub("_", " ", input$trendingStat)), 
                                values = c("All Games" = "longdash",
                                           "Home" = "dashed",
                                           "Away" = "dashed"),
                                breaks = c("All Games", "Home", "Away")) +
          guides(fill = guide_legend(title = NULL, order=2),
                 color = guide_legend(title = NULL, order=3),
                 linetype = guide_legend(keywidth = 5, title.position = "top", title.hjust = .5, order=1,
                                         override.aes = list(colour=c("black", color3, color4)))) +
          theme_bw() + theme(axis.text.x = element_cfb_logo(size=1.5),
                             legend.position = "bottom",
                             legend.direction = 'horizontal') +
          geom_hline(aes(yintercept = mean(Opp_Trends_df_filtered()[,input$trendingStat], na.rm=T),
                         linetype="All Games"), color = "black" , size = 1) +
          geom_point(aes_string(y=input$trendingStat, colour = "location"), shape = 18, size = 4) +
          geom_hline(aes(yintercept = home_mean(),
                         linetype="Home"), color = color3 , size = 1) +
          geom_hline(aes(yintercept = away_mean(),
                         linetype="Away"), color = color4 , size = 1)}
      
      else if(input$trendIndicators == "NET"){
        ggplot(data = Opp_Trends_df_filtered(), aes(x=reorder(game_code, game_date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df_filtered()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) + coord_cartesian(ylim = c(OO_ymin(), OO_ymax())) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2),
                            breaks = c("W", "L")) +
          scale_colour_manual(values = c("NET Top 50" = color3,
                                         "NET Top 100" = color4),
                              breaks = c("NET Top 50", "NET Top 100")) +
          scale_linetype_manual(name = paste0("Average ", gsub("_", " ", input$trendingStat)), 
                                values = c("All Games" = "longdash",
                                           "Home" = "dashed",
                                           "Away" = "dashed"),
                                breaks = c("All Games", "Home", "Away")) +
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
          geom_hline(aes(yintercept = mean(Opp_Trends_df_filtered()[,input$trendingStat], na.rm=T),
                         linetype="All Games"), color = "black" , size = 1) +
          geom_point(aes_string(y=input$trendingStat, colour = "net_rk", size = "net_rk"), shape = 18) +
          geom_hline(aes(yintercept = home_mean(),
                         linetype="Home"), color = color5 , size = 1) +
          geom_hline(aes(yintercept = away_mean(),
                         linetype="Away"), color = color6 , size = 1)}
      
      else if(input$trendIndicators == "Conference"){
        ggplot(data = Opp_Trends_df_filtered(), aes(x=reorder(game_code, game_date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df_filtered()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) + coord_cartesian(ylim = c(OO_ymin(), OO_ymax())) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2),
                            breaks = c("W", "L")) +
          scale_colour_manual(values = c(color3, color3),
                              limits = c(opp_conf())) +
          scale_linetype_manual(name = paste0("Average ", gsub("_", " ", input$trendingStat)), 
                                values = c("All Games" = "longdash",
                                           "Home" = "dashed",
                                           "Away" = "dashed"),
                                breaks = c("All Games", "Home", "Away")) +
          scale_size_manual(values = c(4), limits = c(opp_conf())) +
          guides(fill = guide_legend(title = NULL, order=2),
                 color = guide_legend(title = NULL, order=3),
                 linetype = guide_legend(keywidth = 5, title.position = "top", title.hjust = .5, order=1,
                                         override.aes = list(colour=c("black", color5, color6))),
                 size = FALSE) +
          theme_bw() + theme(axis.text.x = element_cfb_logo(size=1.5),
                             legend.position = "bottom",
                             legend.direction = 'horizontal') +
          geom_hline(aes(yintercept = mean(Opp_Trends_df_filtered()[,input$trendingStat], na.rm=T),
                         linetype="All Games"), color = "black" , size = 1) +
          geom_point(aes_string(y=input$trendingStat, colour = "conference_game", size = "conference_game"), shape = 18) +
          geom_hline(aes(yintercept = home_mean(),
                         linetype="Home"), color = color5 , size = 1) +
          geom_hline(aes(yintercept = away_mean(),
                         linetype="Away"), color = color6 , size = 1)}
    }
    
    else if(input$trendSplits == "Result"){
      
      if(input$trendIndicators == "OFF"){
        ggplot(data = Opp_Trends_df_filtered(), aes(x=reorder(game_code, game_date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df_filtered()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) + coord_cartesian(ylim = c(OO_ymin(), OO_ymax())) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2),
                            breaks = c("W", "L")) +
          scale_linetype_manual(name = paste0("Average ", gsub("_", " ", input$trendingStat)), 
                                values = c("All Games" = "longdash", 
                                           "Wins" = "dashed",
                                           "Losses" = "dashed"),
                                breaks = c("All Games", "Wins", "Losses")) +
          guides(fill = guide_legend(title = NULL, order=2),
                 linetype = guide_legend(keywidth = 3, title.position = "top", title.hjust = .5, order=1,
                                         override.aes = list(colour=c("black", color1, color2)))) +
          theme_bw() + theme(axis.text.x = element_cfb_logo(size=1.5),
                             legend.position = "bottom",
                             legend.direction = "horizontal") +
          geom_hline(aes(yintercept = mean(Opp_Trends_df_filtered()[,input$trendingStat]),
                         linetype="All Games"), color = "black" , size = 1) +
          geom_hline(aes(yintercept = wins_mean(),
                         linetype="Wins"), color = color1 , size = 1) +
          geom_hline(aes(yintercept = losses_mean(),
                         linetype="Losses"), color = color2 , size = 1)}
      
      else if(input$trendIndicators == "Location"){
        ggplot(data = Opp_Trends_df_filtered(), aes(x=reorder(game_code, game_date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df_filtered()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) + coord_cartesian(ylim = c(OO_ymin(), OO_ymax())) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2),
                            breaks = c("W", "L")) +
          scale_colour_manual(values = c("Home" = color3,
                                         "Away" = color4),
                              breaks = c("Home", "Away")) +
          scale_linetype_manual(name = paste0("Average ", gsub("_", " ", input$trendingStat)), 
                                values = c("All Games" = "longdash",
                                           "Wins" = "dashed",
                                           "Losses" = "dashed"),
                                breaks = c("All Games", "Wins", "Losses")) +
          guides(fill = guide_legend(title = NULL, order=2),
                 color = guide_legend(title = NULL, order=3),
                 linetype = guide_legend(keywidth = 5, title.position = "top", title.hjust = .5, order=1,
                                         override.aes = list(colour=c("black", color1, color2)))) +
          theme_bw() + theme(axis.text.x = element_cfb_logo(size=1.5),
                             legend.position = "bottom",
                             legend.direction = 'horizontal') +
          geom_hline(aes(yintercept = mean(Opp_Trends_df_filtered()[,input$trendingStat], na.rm=T),
                         linetype="All Games"), color = "black" , size = 1) +
          geom_point(aes_string(y=input$trendingStat, colour = "location"), shape = 18, size = 4) +
          geom_hline(aes(yintercept = wins_mean(),
                         linetype="Wins"), color = color1 , size = 1) +
          geom_hline(aes(yintercept = losses_mean(),
                         linetype="Losses"), color = color2 , size = 1)}
      
      else if(input$trendIndicators == "NET"){
        ggplot(data = Opp_Trends_df_filtered(), aes(x=reorder(game_code, game_date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df_filtered()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) + coord_cartesian(ylim = c(OO_ymin(), OO_ymax())) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2),
                            breaks = c("W", "L")) +
          scale_colour_manual(values = c("NET Top 50" = color3,
                                         "NET Top 100" = color4),
                              breaks = c("NET Top 50", "NET Top 100")) +
          scale_linetype_manual(name = paste0("Average ", gsub("_", " ", input$trendingStat)), 
                                values = c("All Games" = "longdash",
                                           "Wins" = "dashed",
                                           "Losses" = "dashed"),
                                breaks = c("All Games", "Wins", "Losses")) +
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
          geom_hline(aes(yintercept = mean(Opp_Trends_df_filtered()[,input$trendingStat], na.rm=T),
                         linetype="All Games"), color = "black" , size = 1) +
          geom_point(aes_string(y=input$trendingStat, colour = "net_rk", size = "net_rk"), shape = 18) +
          geom_hline(aes(yintercept = wins_mean(),
                         linetype="Wins"), color = color1 , size = 1) +
          geom_hline(aes(yintercept = losses_mean(),
                         linetype="Losses"), color = color2 , size = 1)}
      
      else if(input$trendIndicators == "Conference"){
        ggplot(data = Opp_Trends_df_filtered(), aes(x=reorder(game_code, game_date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df_filtered()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) + coord_cartesian(ylim = c(OO_ymin(), OO_ymax())) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2),
                            breaks = c("W", "L")) +
          scale_colour_manual(values = c(color3, color3),
                              limits = c(opp_conf())) +
          scale_linetype_manual(name = paste0("Average ", gsub("_", " ", input$trendingStat)), 
                                values = c("All Games" = "longdash",
                                           "Wins" = "dashed",
                                           "Losses" = "dashed"),
                                breaks = c("All Games", "Wins", "Losses")) +
          scale_size_manual(values = c(4), limits = c(opp_conf())) +
          guides(fill = guide_legend(title = NULL, order=2),
                 color = guide_legend(title = NULL, order=3),
                 linetype = guide_legend(keywidth = 5, title.position = "top", title.hjust = .5, order=1,
                                         override.aes = list(colour=c("black", color1, color2))),
                 size = FALSE) +
          theme_bw() + theme(axis.text.x = element_cfb_logo(size=1.5),
                             legend.position = "bottom",
                             legend.direction = 'horizontal') +
          geom_hline(aes(yintercept = mean(Opp_Trends_df_filtered()[,input$trendingStat], na.rm=T),
                         linetype="All Games"), color = "black" , size = 1) +
          geom_point(aes_string(y=input$trendingStat, colour = "conference_game", size = "conference_game"), shape = 18) +
          geom_hline(aes(yintercept = wins_mean(),
                         linetype="Wins"), color = color1 , size = 1) +
          geom_hline(aes(yintercept = losses_mean(),
                         linetype="Losses"), color = color2 , size = 1)}
    }
    
    else if(input$trendSplits == "NET"){
      
      if(input$trendIndicators == "OFF"){
        ggplot(data = Opp_Trends_df_filtered(), aes(x=reorder(game_code, game_date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df_filtered()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) + coord_cartesian(ylim = c(OO_ymin(), OO_ymax())) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2),
                            breaks = c("W", "L")) +
          scale_linetype_manual(name = paste0("Average ", gsub("_", " ", input$trendingStat)), 
                                values = c("All Games" = "longdash", 
                                           "NET Top 50" = "dashed",
                                           "NET Top 100" = "dashed"),
                                breaks = c("All Games", "NET Top 50", "NET Top 100")) +
          guides(fill = guide_legend(title = NULL, order=2),
                 linetype = guide_legend(keywidth = 3, title.position = "top", title.hjust = .5, order=1,
                                         override.aes = list(colour=c("black", color5, color6)))) +
          theme_bw() + theme(axis.text.x = element_cfb_logo(size=1.5),
                             legend.position = "bottom",
                             legend.direction = "horizontal") +
          geom_hline(aes(yintercept = mean(Opp_Trends_df_filtered()[,input$trendingStat], na.rm=T),
                         linetype="All Games"), color = "black" , size = 1) +
          geom_hline(aes(yintercept = net50_mean(),
                         linetype="NET Top 50"), color = color5 , size = 1) +
          geom_hline(aes(yintercept = net100_mean(),
                         linetype="NET Top 100"), color = color6 , size = 1)}
      
      else if(input$trendIndicators == "Location"){
        ggplot(data = Opp_Trends_df_filtered(), aes(x=reorder(game_code, game_date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df_filtered()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) + coord_cartesian(ylim = c(OO_ymin(), OO_ymax())) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2),
                            breaks = c("W", "L")) +
          scale_colour_manual(values = c("Home" = color3,
                                         "Away" = color4),
                              breaks = c("Home", "Away")) +
          scale_linetype_manual(name = paste0("Average ", gsub("_", " ", input$trendingStat)), 
                                values = c("All Games" = "longdash",
                                           "NET Top 50" = "dashed",
                                           "NET Top 100" = "dashed"),
                                breaks = c("All Games", "NET Top 50", "NET Top 100")) +
          guides(fill = guide_legend(title = NULL, order=2),
                 color = guide_legend(title = NULL, order=3),
                 linetype = guide_legend(keywidth = 5, title.position = "top", title.hjust = .5, order=1,
                                         override.aes = list(colour=c("black", color5, color6)))) +
          theme_bw() + theme(axis.text.x = element_cfb_logo(size=1.5),
                             legend.position = "bottom",
                             legend.direction = 'horizontal') +
          geom_hline(aes(yintercept = mean(Opp_Trends_df_filtered()[,input$trendingStat], na.rm=T),
                         linetype="All Games"), color = "black" , size = 1) +
          geom_point(aes_string(y=input$trendingStat, colour = "location"), shape = 18, size = 4) +
          geom_hline(aes(yintercept = net50_mean(),
                         linetype="NET Top 50"), color = color5 , size = 1) +
          geom_hline(aes(yintercept = net100_mean(),
                         linetype="NET Top 100"), color = color6 , size = 1)}
      
      else if(input$trendIndicators == "NET"){
        ggplot(data = Opp_Trends_df_filtered(), aes(x=reorder(game_code, game_date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df_filtered()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) + coord_cartesian(ylim = c(OO_ymin(), OO_ymax())) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2),
                            breaks = c("W", "L")) +
          scale_colour_manual(values = c("NET Top 50" = color3,
                                         "NET Top 100" = color4),
                              breaks = c("NET Top 50", "NET Top 100")) +
          scale_linetype_manual(name = paste0("Average ", gsub("_", " ", input$trendingStat)), 
                                values = c("All Games" = "longdash",
                                           "NET Top 50" = "dashed",
                                           "NET Top 100" = "dashed"),
                                breaks = c("All Games", "NET Top 50", "NET Top 100")) +
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
          geom_hline(aes(yintercept = mean(Opp_Trends_df_filtered()[,input$trendingStat], na.rm=T),
                         linetype="All Games"), color = "black" , size = 1) +
          geom_point(aes_string(y=input$trendingStat, colour = "net_rk", size = "net_rk"), shape = 18) +
          geom_hline(aes(yintercept = net50_mean(),
                         linetype="NET Top 50"), color = color3 , size = 1) +
          geom_hline(aes(yintercept = net100_mean(),
                         linetype="NET Top 100"), color = color4 , size = 1)}
      
      else if(input$trendIndicators == "Conference"){
        ggplot(data = Opp_Trends_df_filtered(), aes(x=reorder(game_code, game_date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df_filtered()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) + coord_cartesian(ylim = c(OO_ymin(), OO_ymax())) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2),
                            breaks = c("W", "L")) +
          scale_colour_manual(values = c(color3, color3),
                              limits = c(opp_conf())) +
          scale_linetype_manual(name = paste0("Average ", gsub("_", " ", input$trendingStat)), 
                                values = c("All Games" = "longdash",
                                           "NET Top 50" = "dashed",
                                           "NET Top 100" = "dashed"),
                                breaks = c("All Games", "NET Top 50", "NET Top 100")) +
          scale_size_manual(values = c(4), limits = c(opp_conf())) +
          guides(fill = guide_legend(title = NULL, order=2),
                 color = guide_legend(title = NULL, order=3),
                 linetype = guide_legend(keywidth = 5, title.position = "top", title.hjust = .5, order=1,
                                         override.aes = list(colour=c("black", color5, color6))),
                 size = FALSE) +
          theme_bw() + theme(axis.text.x = element_cfb_logo(size=1.5),
                             legend.position = "bottom",
                             legend.direction = 'horizontal') +
          geom_hline(aes(yintercept = mean(Opp_Trends_df_filtered()[,input$trendingStat], na.rm=T),
                         linetype="All Games"), color = "black" , size = 1) +
          geom_point(aes_string(y=input$trendingStat, colour = "conference_game", size = "conference_game"), shape = 18) +
          geom_hline(aes(yintercept = net50_mean(),
                         linetype="NET Top 50"), color = color5 , size = 1) +
          geom_hline(aes(yintercept = net100_mean(),
                         linetype="NET Top 100"), color = color6 , size = 1)}
    }
    
    else if(input$trendSplits == "Conference"){
      
      if(input$trendIndicators == "OFF"){
        ggplot(data = Opp_Trends_df_filtered(), aes(x=reorder(game_code, game_date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df_filtered()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) + coord_cartesian(ylim = c(OO_ymin(), OO_ymax())) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2),
                            breaks = c("W", "L")) +
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
          geom_hline(aes(yintercept = mean(Opp_Trends_df_filtered()[,input$trendingStat], na.rm=T),
                         linetype="All Games"), color = "black" , size = 1) +
          geom_hline(aes(yintercept = conf_mean(),
                         linetype="Conference Games"), color = color6 , size = 1)}
      
      else if(input$trendIndicators == "Location"){
        ggplot(data = Opp_Trends_df_filtered(), aes(x=reorder(game_code, game_date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df_filtered()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) + coord_cartesian(ylim = c(OO_ymin(), OO_ymax())) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2),
                            breaks = c("W", "L")) +
          scale_colour_manual(values = c("Home" = color3,
                                         "Away" = color4),
                              breaks = c("Home", "Away")) +
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
          geom_hline(aes(yintercept = mean(Opp_Trends_df_filtered()[,input$trendingStat], na.rm=T),
                         linetype="All Games"), color = "black" , size = 1) +
          geom_point(aes_string(y=input$trendingStat, colour = "location"), shape = 18, size = 4) +
          geom_hline(aes(yintercept = conf_mean(),
                         linetype="Conference Games"), color = color6 , size = 1)}
      
      else if(input$trendIndicators == "NET"){
        ggplot(data = Opp_Trends_df_filtered(), aes(x=reorder(game_code, game_date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df_filtered()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) + coord_cartesian(ylim = c(OO_ymin(), OO_ymax())) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2),
                            breaks = c("W", "L")) +
          scale_colour_manual(values = c("NET Top 50" = color3,
                                         "NET Top 100" = color4),
                              breaks = c("NET Top 50", "NET Top 100")) +
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
          geom_hline(aes(yintercept = mean(Opp_Trends_df_filtered()[,input$trendingStat], na.rm=T),
                         linetype="All Games"), color = "black" , size = 1) +
          geom_point(aes_string(y=input$trendingStat, colour = "net_rk", size = "net_rk"), shape = 18) +
          geom_hline(aes(yintercept = conf_mean(),
                         linetype="Conference Games"), color = color6 , size = 1)}
      
      else if(input$trendIndicators == "Conference"){
        ggplot(data = Opp_Trends_df_filtered(), aes(x=reorder(game_code, game_date))) + 
          annotate("rect", xmin=dec_min(), xmax=dec_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          annotate("rect", xmin=feb_min(), xmax=feb_max(), ymin=-Inf, ymax=Inf, 
                   fill="grey", alpha = .3) +
          geom_col(aes_string(y = input$trendingStat, fill = "wl")) +
          scale_x_discrete(labels = Opp_Trends_df_filtered()$opponent) + 
          xlab("") + ylab(gsub("_", " ", input$trendingStat)) + coord_cartesian(ylim = c(OO_ymin(), OO_ymax())) +
          scale_fill_manual(values = c("W" = color1, 
                                       "L" = color2),
                            breaks = c("W", "L")) +
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
          geom_hline(aes(yintercept = mean(Opp_Trends_df_filtered()[,input$trendingStat], na.rm=T),
                         linetype="All Games"), color = "black" , size = 1) +
          geom_point(aes_string(y=input$trendingStat, colour = "conference_game", size = "conference_game"), shape = 18) +
          geom_hline(aes(yintercept = conf_mean(),
                         linetype="Conference Games"), color = color3 , size = 1)}
    }
    )

  
  } #end of server

shinyApp(ui, server)

