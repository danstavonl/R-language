#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
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

# Define UI for application that draws a histogram
ui <- fluidPage(
   
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
                        h2("The Most Common Unisex Names in America Data Analysis"),
                        br(),           
                        h3("Source:"),
                        p("https://github.com/fivethirtyeight/data/blob/master/unisex-names/unisex_names_table.csv"),
                        br(),
                        h3("Project Description"),
                        hr(),
                        p("The data contains 900 names given to each sex at least one-third of the time and with a minimum of 100 people. It has the following variables:"),
                        br(),
                        p("Name:	First names from the Social Security Administration"),
                        p("Total:	Total number of living Americans with the name"),
                        p("Male Share:	Percentage of people with the name who are male"),
                        p("Female Share:	Percentage of people with the name who are female"),
                        p("Gap:	 Gap between male_share and female_share"),
                        br(),
                        h3("Question:"),
                        hr(),
                        p("Using the data from the Unisex Names survey, I am curious to know the effect of the total population of living Americans with the name on the percentage of people with the name who are male."),
                        br(),
                        h3("Main panel"),
                        plotOutput("plot1"), 
                        h3("Second Plot with prediction summary"),
                        plotOutput("plot2"),
                        h4("Prediction:"),
                        textOutput("pred1"),
                        br(),
                        h4("Summary:"),
                        verbatimTextOutput("summary"),
                        br(),
                        h4("Summary 2:"),
                        verbatimTextOutput("summary2"),
                        hr(),
                        h4("Observation:"),
                        p("My significance level is 0.05. The p-values (0.219) for the independent variable is greater than the significance level of the model which implies that the variable Gap is statistically not significant to the model. The points in the plot were randomly dispersed around the horizontal axis which impliies a linear model is more appropriate for this analysis."),
                        hr(),
                        p("From the output, the total p-value is greater than the significance level 0.05 implying that the distribution of the data is not significantly different from normal distribution. Correlation:"),
                        verbatimTextOutput("cor"),
                        br()
                )
        )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
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
        
        output$summary <- renderPrint({
                summary(model1)$coef
        })
        
        output$summary2 <- renderPrint({ 
                summary(model1)
        })
        
        output$cor <- renderPrint({ 
                cor(record$total, record$male_share,  method = "pearson", use = "everything")
        })
        
}

# Run the application 
shinyApp(ui = ui, server = server)

