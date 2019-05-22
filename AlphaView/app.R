# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)
library(quantmod)
library(TTR)

# source functions
source("Basics.R")

ui <- dashboardPage(
  
  # skin color
  skin = "black",
  
  ### *** HEADER *** ###
  dashboardHeader(
    # header font
    title = span("AlphaView", 
                 style = "font-size: 30px")
  ),
  
  ### *** SIDEBAR *** ###
  dashboardSidebar(
    # font size 
    tags$head( 
      tags$style(HTML(".main-sidebar { font-size: 25px; }")) #change the font size to 20
    ),
    # menu items
    sidebarMenu(
      # item (1) appearance
      menuItem("Summary", tabName = "summary", icon = icon("poll-h")),
      # item (2) appearance
      menuItem(" Research", tabName = "research", icon = icon("book")),
      # item (3) appearance
      menuItem("Backtesting", tabName = "backtesting", icon = icon("code-branch"))
    )
  ),
  
  ### *** BODY *** ###
  dashboardBody(
    # tabs 
    tabItems(
      # tab content (1)
      tabItem(tabName = "summary",
        # define layout      
        sidebarLayout(
          # plot
          mainPanel(
            width = 9,
            plotOutput("plot")
          ),
          # sidebar panel
          sidebarPanel(
            tags$style(type="text/css", "input.form-control { font-size:20px; height:50px;}"),
            tags$style(type="text/css", "input.shiny-bound-input { font-size:20px; height:50px;}"),
            # width 
            width = 3,
            # symbol input
            textInput("symb", "Symbol", "AAPL"),
            # date range input
            dateRangeInput("dates", 
                           "Date range",
                           start = "2019-01-01", 
                           end = "2019-01-29"),
            # moving average shift input
            numericInput("days", "MA Shift", 5, min = 0, max = 50),
            # sma switch
            checkboxInput("sma", "SMA", value = F),
            # equilibrium points switch
            checkboxInput("equilBuy", "Buy EPts", value = F),
            checkboxInput("equilSell", "Sell EPtx", value = F)
          )
        )
      ),
      # tab content (2)
      tabItem(tabName = "research",
        # define layout
        navbarPage("Options",
          tabPanel("SVM",
            # add rows, columns
            fluidRow(
              column(10, "Training Data",
                     plotOutput("trainData")),
              column(10, "SVM Data",
                     plotOutput("svmData"))
            )
                   ),
          tabPanel("LSTM")
        )
      ),
      # tab content (3)
      tabItem(tabName = "backtesting"
        # define layout
      )
    )
  ) 

) # END PAGE

### *** Server Function *** ###

server <- function(input, output) {
  
  ### * Reactive Input Summary * ###
  
  # symbol, date 
  dataInput <- reactive({
    getSymbols(input$symb, src = "yahoo",
               from = input$dates[1],
               to = input$dates[2],
               auto.assign = FALSE)
  })
  
  # sma
  smaInput <- reactive({
    addSMA(n = input$days)
    })
  
  # buy, sell dates data
  equilBuyInput <- reactive({
    x_vline <- equilPoints(dataInput(), input$days)
    addTA(x_vline[[1]], on = -1, col = "lightblue", border='darkgreen')
  })
  equilSellInput <- reactive({
    x_vline <- equilPoints(dataInput(), input$days)
    addTA(x_vline[[2]], on = -1, col = "gold", border = "darkred")
  })
  
  ### * Reactive Input Research * ###
  
  ### * Output Summary * ###
  
  # plot data
  output$plot <- renderPlot({
    Chart <- dataInput()
    
    # add candlestick chart
    chartSeries(Chart, type = "candlesticks", 
                theme = chartTheme("white"), up.col = "green", dn.col = "red")
    
    # toggle sma, equilPts
    if (input$sma) {
      smaInput()
    }
    else if (input$equilBuy) {
      equilBuyInput()
    }
    else if (input$equilSell) {
      equilSellInput()
    }
    else {
      return(NULL)
    }
    
  }, height = 700)
  
  ### * Output Research * ###
}

shinyApp(ui, server)