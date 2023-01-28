#read headshot url table
headshot_urls_db = read_xlsx("CBB_Prescout/headshot_url.xlsx")














ui = navbarPage("Pre-Scout Portal", fluid = TRUE,
                tabPanel("Graphical Metric Comparison",
                         fluidRow(column(3, selectInput("opponent1", "Opponent", opponentList)),
                                  column(6, h1(strong("Pre-Scout Portal")), uiOutput("header")),
                                  column(3, img(src = "logo_oregon.png", height = 180, width = 240))),
                         sidebarLayout(
                           sidebarPanel(radioButtons("whichGraph", "Metric Comparison", MetricCompList),
                                        checkboxInput("focus", "Focused View?")),
                           mainPanel(plotOutput("MetricComp", height = "600px")))),
                tabPanel("Player Personnel", 
                         fluidRow(column(3, selectInput("opponent2", "Opponent", opponentList)),
                                  column(6, h1(strong("Pre-Scout Portal")), uiOutput("header2")),
                                  column(3, img(src = "logo_oregon.png", height = 180, width = 240))),
                         fluidRow(
                           column(4, selectInput("filterPersonnel", "Filter", TableFilterList), sliderInput("minMinsPPT", "Min/G Minimum", value=5, min=0, max=40), style = "background-color:#f5f5f5"),
                           column(4, selectInput("sortPersonnel", "Sort", TableSortList), sliderInput("minGamesPPT", "GP Minimum", value=0, min=0, max=30), style = "background-color:#f5f5f5"),
                           column(4, checkboxGroupInput("columnsPersonnel", "Visible Columns", choices = TableColumnList, selected = "Player Info"),style = "background-color:#f5f5f5")),
                         fluidRow(12, gt_output("PlayerPersonnel"))),
                tabPanel("Opponent Trends", h5("Under Construction")),
                tabPanel("Shot Charts", h5("Under Construction")),
                tabPanel("Lineups", h5("Under Construction"))
                
                
)


server = function(input, output, session) {
  
  
  #get starters from opponent's last game
  #different name formatting used for certain opponents
  uconn = reactive(ifelse(input$opponent2=='Connecticut', "UConn", input$opponent2))
  lastGdate = reactive(kp_team_schedule(opp_kp(), year=year) %>% filter(is.na(pre_wp)) %>% arrange(desc(date)) %>% .[[1,18]])
  opponentGID = reactive(espn_mbb_scoreboard(lastGdate()) %>% filter(home_team_location == input$opponent2 | away_team_location == input$opponent2 | home_team_location == uconn() | away_team_location == uconn()) %>% .[[1,6]])
  lastGstarters = reactive(espn_mbb_player_box(opponentGID()) %>% filter(starter==TRUE) %>% filter(team_short_display_name==input$opponent2 | team_short_display_name==uconn() | team_short_display_name==opp_kp() | team_short_display_name==opp_kp2()) %>% 
                             select(athlete_display_name, athlete_jersey, starter) %>% mutate(athlete_display_name = gsub("\\.", "", athlete_display_name)) %>% separate(athlete_display_name, into = c("first","last"), extra = "drop", sep = "[^\\w']") %>% mutate(last = str_to_title(last)) %>%
                             mutate(athlete_jersey = as.numeric(athlete_jersey)) %>% rename('#' = athlete_jersey))
  
  #base table for personnel output
  PPtable_raw = reactive(SRopponentTables()[[1]] %>% mutate(team=input$opponent2) %>%
                           select(team, Player, "#", Class, Pos, Height) %>%
                           mutate(Player = gsub("\\.", "", Player)) %>%
                           separate(Player, into = c("first","last"), extra = "drop", sep = "[^\\w']") %>%
                           mutate(last = str_to_title(last)) %>% 
                           right_join(opp_pos2(), by=c('#')) %>% select(-first.y, -last.y) %>% rename(first = first.x, last = last.x) %>%
                           #create dummy variables for the TableFilterList to filter by
                           mutate("All Players" = 1,
                                  "Guards" = ifelse(PG+SG>.6, 1, 0),
                                  "Bigs" = ifelse(CC+PF>.75 & CC>0 , 1, 0),
                                  "Wings" = ifelse(Guards+Bigs==0, 1, 0)) %>%
                           left_join(lastGstarters(), by=c('#')) %>% select(-first.y, -last.y) %>% rename(first = first.x, last = last.x) %>%
                           mutate("Starters" = ifelse(starter==TRUE, 1, 0)) %>% select(-starter) %>%
                           left_join(SRopponentTables()[[6]] %>% select(Player, "MpG" = MP) %>% mutate(Player = gsub("\\.", "", Player)) %>% separate(Player, into = c("first","last"), extra = "drop", sep = "[^\\w']") %>% mutate(last = str_to_title(last)), by=c('last', 'first')) %>%
                           mutate(PG = ifelse(PG==0, NA, PG*100), SG = ifelse(SG==0, NA, SG*100), SF = ifelse(SF==0, NA, SF*100), PF = ifelse(PF==0, NA, PF*100), CC = ifelse(CC==0, NA, CC*100)))
  
  
  #join headshots to PPtable
  PPtable_raw2 = reactive(left_join(PPtable_raw(), headshot_urls(), by=c("first", "last", "team"="Team")))
  
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
  
  
  
  output$PlayerPersonnel = render_gt({
    PPtable() %>% gt() %>%
      gt_merge_stack_team_color(first, last, team) %>%
      gt_img_rows(columns = URL, img_source = "web", height = 90) %>%
      cols_hide(team) %>% cols_label(URL = "",
                                     first = "Name") %>%
      fmt_number(columns = starts_with("PG") | matches("SG") | matches("SF") | matches("PF") | matches("CC"), decimals=0, drop_trailing_zeros = T) %>%
      tab_style(
        style = list(
          cell_fill(color = "lightgrey")),
        locations = cells_body(
          columns = starts_with("PG") | matches("SG") | matches("SF") | matches("PF") | matches("CC")
        )) %>%
      sub_missing(
        columns = starts_with("PG") | matches("SG") | matches("SF") | matches("PF") | matches("CC"),
        missing_text = " "
      )
  })
  
}

shinyApp(ui, server)