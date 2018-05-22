library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Project 5 Game"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      actionButton("newGameBut", label = "New Game"),
      hr(size="2"),
      h3("Scores"),
      fluidRow(column(6,h4("You", align = "center")),
               column(6,h4("DP", align = "center"))),
      
      fluidRow(column(6,htmlOutput("score")),
               column(6,htmlOutput("cScore")))
      
      
      ),
  
      
    
    # Show a plot of the generated distribution
    mainPanel(
      column(6,
          hr(),
          htmlOutput("uTurn"),
          hr(),
          #conditionalPanel(condition = "input.uTurn == 'your'",
                           h3("Turn total :"),
                           htmlOutput("k"),
                           br(),
                           actionButton("holdBut","Hold"),
                           br(),
                           actionButton("rollBut","Roll"),                           
                           htmlOutput("r")            
           #                )
             ),
      column(6,
             hr(),
             htmlOutput("dTurn"),
             hr(),
            # conditionalPanel(condition = "input.dTurn == 'dp'",
                              br(),
                              actionButton("playDPBut","Click to let DP play"),
                              htmlOutput("dpPlayMsg")
             #)
      )
      
      
      
    )
  )
))