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
  titlePanel("Data Science for the Most Common Unisex Names In America"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       h1("Move the slider"),  
       sliderInput("slider1", "Total number of living Americans with the name?", 10000, 200000, value = 50000, step = 10000),
       submitButton("Submit")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       h3("Main panel"),
       plotOutput("plot1"), 
       h3("Second Plot with prediction summary"),
       plotOutput("plot2"),
       h6("Prediction:"),
       textOutput("pred1"),
       br()
    )
  )
))
