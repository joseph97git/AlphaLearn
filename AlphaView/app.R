
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)
library(quantmod)

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
            tags$style(type="text/css", "input.form-control { font-size:30px; height:50px;}"),
            tags$style(type="text/css", "input.shiny-bound-input { font-size:35px; height:50px;}"),
            # width 
            width = 3,
            # symbol input
            textInput("symb", "Symbol", "AAPL"),
            # date range input
            dateRangeInput("dates", 
                           "Date range",
                           start = "2019-01-01", 
                           end = as.character(Sys.Date()))
          )
        )
      ),
      # tab content (2)
      tabItem(tabName = "backtesting"
      )
    )
  ) 

) # END PAGE


### *** Utility Functions *** ###

discreteDeriv <- function(data, shift) {
  # calculates derivative of closing prices
  # applies specifically to moving averages
  discrete <- rep(NA, shift)
  for (i in 1:length(data)-1) {
    discrete <- append(discrete, data[i+1]-data[i])
  }
  return(discrete)
}

### *** Server Function *** ###

server <- function(input, output) {
  
  # symbol, date 
  dataInput <- reactive({
    getSymbols(input$symb, src = "yahoo",
               from = input$dates[1],
               to = input$dates[2],
               auto.assign = FALSE)
  })
  
  # plot data
  output$plot <- renderPlot({
    Data <- dataInput()
    
    chartSeries(Data, type = "candlesticks", 
                theme = chartTheme("white"), up.col = "green", dn.col = "red")
  }, height = 940)
  
  
}

shinyApp(ui, server)