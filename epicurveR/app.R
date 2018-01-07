#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Epicurve"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("days",
                     "Number of days:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotlyOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  count2list <- reactive({
    counts <- round(faithful[, 2][1:input$days]/10)
    rowList <- data.frame()
    for(i in 1:length(counts)){
      for(j in 1:counts[i]){
        rowList <- dplyr::bind_rows(rowList,c(x=i,y=j))
      }
    }
    return(rowList)
  })
  
   output$distPlot <- renderPlotly({
      # generate bins based on input$bins from ui.R
      
      # bins <- seq(min(x), max(x), length.out = input$bins + 1)
      # 
      # # draw the histogram with the specified number of bins
      # hist(x, breaks = bins, col = 'darkgray', border = 'white')
      
      data <- count2list()
      
      ggplotly(ggplot(data) + geom_tile(aes(x=x, y=y, fill=y), width=0.9, height=0.9) + theme_minimal())
      #plot_ly(data = data, x=~x,y=~y,z=~y type = "heatmap",)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

