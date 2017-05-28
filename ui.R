#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Data handler"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
                
           textInput("name","Enter your name:", "NAME") , selectInput("variable", "Select the data to be handled:",
                    c("Cars" = "mtcars",
                            "Longley's Economic Regression " = "longley",
                            "Crimes"="USArrests"
                            ,"Average Heights and Weights for American Women"="women","Judge Ratings"="USJudgeRatings",
                            "Students sleep"="sleep")),
            uiOutput("choice1"), uiOutput("choice2")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       h2(textOutput("hola")), h4(textOutput("dataf"),tabsetPanel(
               tabPanel("Plot", plotOutput("plot")),
               tabPanel("Table",dataTableOutput("tabla")),
               tabPanel("Summary",verbatimTextOutput("summary"))
       ),h3("Documentation:"),h5("1. First Enter your name on the text input",br(),"2. Select the data set you want to analyze",br(),"3. Select X and Y axis values",br(),"4. Go to the tab you desire"))
    )
  )
))
