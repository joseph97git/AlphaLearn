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
      menuItem("Summary", tabName = "summary", icon = icon("clipboard")),
      # item (2) appearance
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
      tabItem(tabName = "backtesting"
        # define layout
      )
    )
  ) 

) # END PAGE

### *** Server Function *** ###

server <- function(input, output) {
  
  # symbol, date 
  dataInput <- reactive({
    getSymbols(input$symb, src = "yahoo",
               from = input$dates[1],
               to = input$dates[2],
               auto.assign = FALSE)
  })
  
  smaInput <- reactive({
    addSMA(n = input$days)
    })
  
  equilBuyInput <- reactive({
    x_vline <- equilPoints(dataInput(), input$days)
    addTA(x_vline[[1]], on = -1, col = "lightblue", border='darkgreen')
  })
  
  equilSellInput <- reactive({
    x_vline <- equilPoints(dataInput(), input$days)
    addTA(x_vline[[2]], on = -1, col = "gold", border = "darkred")
  })
  
  # plot data
  output$plot <- renderPlot({
    Data <- dataInput()
    
    # add candlestick chart
    chartSeries(Data, type = "candlesticks", 
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
    
  }, height = 550)
  
  
}

shinyApp(ui, server)