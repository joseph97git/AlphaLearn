# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)
library(quantmod)
library(plyr)
library(TTR)
library(e1071)
library(ggplot2)
library(caret)

# source functions
source("Basics.R")
source("Research.R")
source("SVM.R")

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
                           start = "2018-12-21", 
                           end = "2019-05-18"),
            # moving average shift input
            numericInput("days", "MA Shift", 3, min = 0, max = 50),
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
          # SVM tab
          tabPanel("SVM",
            # define layout
            sidebarLayout(
              # plots
              mainPanel(
                # svm plot
                fluidRow(
                  div(style = "padding:0px"),
                  width = 7, 
                  plotOutput("svmData")
                ),
                fluidRow(
                  div(style = "padding:160px"),
                  width = 7, 
                  plotOutput("trainData")
                )
              ),
              # sidebar panel
              sidebarPanel(
                # split ratio
                numericInput("splitsize", "Train Test Split", 0.6, min = 0, max = 1, step = 0.01),
                # seed number
                numericInput("seed", "Set Seed", 13, min = 0, max = 1000),
                # variable options 1
                selectInput("var1", "Variable 1:", c("ROC" = "ROC", "DX" = "DX", "Momentum" = "Momentum")),
                # variable options 2
                selectInput("var2", "Variable 2:", c("DX" = "DX", "ROC" = "ROC", "Momentum" = "Momentum")),
                # kernel options
                selectInput("kern", "Kernel:", c("Linear" = "linear", "Polynomial" = "polynomial", 
                            "Radial Basis" = "radial", "Sigmoid" = "sigmoid")),
                # degree
                numericInput("deg", "Degree:", 1, min = 1, max = 10)
              )
            ) # end layout
            
            
          ), # end tabPanel
          # LSTM tab
          tabPanel("LSTM"
          )
        )
      ),
      # tab content (3)
      tabItem(tabName = "backtesting"
        # define layout
      )
    )
  ) 

) # END PAGE

### *** Server Function *** ### ------------------------------------------------------------------------------------%

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
  
  # buy dates data
  equilBuyInput <- reactive({
    x_vline <- equilPoints(dataInput(), input$days)
    addTA(x_vline[[1]], on = -1, col = "lightblue", border='darkgreen')
  })
  
  # sell dates data
  equilSellInput <- reactive({
    x_vline <- equilPoints(dataInput(), input$days)
    addTA(x_vline[[2]], on = -1, col = "gold", border = "darkred")
  })
  
  ### * Reactive Input Research * ### ------------------------------------------------------------------------------%
  
  indDataInput <- reactive({
    tmpsize <- input$splitsize
    tmp <- getIndicators(dataInput(), input$days, input$splitsize, input$seed)
  })
  
  ### * Output Summary * ### ---------------------------------------------------------------------------------------%
  
  # plot data
  output$plot <- renderPlot({
    # get data
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
  
  ### * Output Research * ### --------------------------------------------------------------------------------------%
  
  # plot training data 
  output$trainData <- renderPlot({
    # get data
    indData <- indDataInput()
    trainData <- indData[[1]]
    
    # temp data frame for plotting
    tmpTrainData <- data.frame(trainData[,toString(input$var1)], 
                                trainData[,toString(input$var2)], 
                                "signal" = as.factor(trainData[,"signal"]))
    
    # add training data plot
    ggplot(data = tmpTrainData, aes_string(x = input$var2, y = input$var1, col = "signal")) + 
      geom_point(size = 7) + geom_point(shape = 1, stroke = 1, size = 7.1, col = "black") + 
      ggtitle(paste(input$var1, " ", "vs.", " ", input$var2)) +
      scale_color_manual('Signal', values = c("red", "green"), labels = c("Sell", "Buy")) +
      theme(title = element_text(size = 20, face = "bold"),
            axis.text = element_text(size = 20), 
            axis.title = element_text(size = 20, face = "bold"), 
            legend.text = element_text(size = 20),
            legend.title = element_text(size = 20))
  }, height = 700)
  
  ### * Output SVM * ### -------------------------------------------------------------------------------------------%
  
  # plot svm data
  output$svmData <- renderPlot({
    # get data
    indData <- indDataInput()
    trainData <<- indData[[1]]
    testData <<- indData[[2]]
    
    # temp data frame for plotting
    tmpTrainData <- data.frame(trainData[,toString(input$var1)], 
                               trainData[,toString(input$var2)], 
                               "signal" = as.factor(trainData[,"signal"]))
    
    # fit model
    svm_fit <<- getSVMFit(tmpTrainData, input$var1, input$var2, input$kern, input$deg)
    
    # get formula for plot
    svmPlotFormula <- as.formula(paste(input$var1, "~", input$var2))
    
    # plot svm
    plot(svm_fit, data=tmpTrainData, formula = svmPlotFormula, fill=TRUE, svSymbol = 21, 
         dataSymbol = 19, symbolPalette = c("red", "#268417"), col = c("#f9dbca", "#e5f9ca"))
    
  }, height = 700)
  
}

shinyApp(ui, server)