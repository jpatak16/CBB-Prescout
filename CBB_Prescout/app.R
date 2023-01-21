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
                         fluidRow(column(3, selectizeInput("opponent", "Opponent", opponentList)),
                                  column(6, h1(strong("Pre-Scout Portal")), uiOutput("header")),
                                  column(3, img(src="logo_oregon.png", height = 180, width = 240))
                                  ) #end of header fluidRow
                         ), #end of matchup selector tabPanel
                
                tabPanel("Graphical Metric Comparison",
                         fluidRow(column(9, h1(strong("Pre-Scout Portal")), uiOutput("header2")),
                                  column(3, img(src="logo_oregon.png", height = 180, width = 240))
                                  ) #end of header fluidRow
                         ), #end of GMC tabPanel
                
                tabPanel("Player Personnel", 
                         fluidRow(column(9, h1(strong("Pre-Scout Portal")), uiOutput("header3")),
                                  column(3, img(src="logo_oregon.png", height = 180, width = 240))
                                  ), #end of header fluidRow
                         h5("Coming Soon")
                         ), #end of PP tabPanel
                
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
                         ), #end of PP tabPanel
                
                tabPanel("Lineups", 
                         fluidRow(column(9, h1(strong("Pre-Scout Portal")), uiOutput("header6")),
                                  column(3, img(src="logo_oregon.png", height = 180, width = 240))
                                  ), #end of header fluidRow
                         h5("Coming Soon")
                         ), #end of PP tabPanel
                
                ) #end of navbarPage


server = function(input, output, session) {
  
  #Oregon vs TEAM text for header on top of all pages
  output$header6 = output$header5 = output$header4 = output$header3 = output$header2 = output$header = 
    renderUI(HTML(paste('<h1 style="color:green;font-size:50px">', our_team, " vs ", input$opponent, '</h1>', sep = "")))
  
  
  
  } #end of server

shinyApp(ui, server)

