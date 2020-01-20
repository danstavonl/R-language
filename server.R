#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(xml2)
library(rvest)
library(stringr)
library(ggplot2)

url <- "https://github.com/fivethirtyeight/data/blob/master/unisex-names/unisex_names_table.csv"
sc <- read_html(url)
table <- sc %>%
        html_nodes("table") %>%
        html_table(header=T)

record <- table[[1]] 
record <- record[, colSums(is.na(record)) != nrow(record)]
setnames(record, c(""), c("id"))
record$male_share <- round(record$male_share, 3)
record$female_share <- round(record$female_share, 3)
record$gap <- round(record$gap, 3)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
        
        set.seed(20190430) 
        
        model1 <- lm(male_share ~ total, data=record) 
        
        resid <- resid(model1) 
        
        pred1 <- reactive({
                
                predict(model1, newdata = data.frame(total = input$slider1))
                
        }) 
 
    
    output$plot1 <- renderPlot({
    
    plot(record$total, resid, ylab = "Residuals", 
         xlab = "Total number of living Americans with the name", main="Residual Scatter Plot")
         
            points(record$total, resid, col = "red", pch = 16, cex = 1)
            
    })
            
    
    output$plot2 <- renderPlot({
                  
       plot(record$total, record$male_share, ylab = "Percentage of people with the name who are male", 
                 xlab = "Total number of living Americans with the name", main="Plot 2")
           
            abline(model1, col="blue", lwd=2)
            points(input$slider1, pred1(), col = "blue", pch = 16, cex = 2)
    }) 
    
    output$pred1 <- renderText({
           pred1()
    })
     
  
})
