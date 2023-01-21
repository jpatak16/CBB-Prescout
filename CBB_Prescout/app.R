library(shiny)
library(pacman)
p_load(rvest, tidyverse, janitor, cfbplotR, stringr, gt, gtExtras, readxl, hoopR)
#remotes::install_github("sportsdataverse/hoopR")

our_team = "Oregon"
opponentList = c("Washington State", "UCLA")
opponentSRurl_db = read_xlsx("opp_url.xlsx")
year=2023


















ui = navbarPage("Pre-Scout Portal", fluid = TRUE,
                tabPanel("Matchup Selection", 
                         fluidRow(column(3, selectInput("opponent", "Opponent", opponentList)),
                                  column(6, h1(strong("Pre-Scout Portal")), uiOutput("header")),
                                  column(3, img(src="logo_oregon.png", height = 180, width = 240))
                                  ) #end of header fluidRow
                         ), #end of matchup selector tabPanel
                
                tabPanel("Graphical Metric Comparison"
                         ), #end of GMC tabPanel
                
                tabPanel("Player Personnel", h5("Coming Soon")),
                tabPanel("Opponent Overview", h5("Coming Soon")),
                tabPanel("Shot Charts", h5("Coming Soon")),
                tabPanel("Lineups", h5("Coming Soon"))
                ) #end of navbarPage


server = function(input, output, session) {
  
  output$header = renderUI(HTML(paste('<h1 style="color:green;font-size:50px">', our_team, " vs ", input$opponent, '</h1>', sep = "")))
  
  
  
  } #end of server

shinyApp(ui, server)

