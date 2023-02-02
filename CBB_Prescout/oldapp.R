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