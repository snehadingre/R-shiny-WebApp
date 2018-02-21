#
# This is a simple Shiny web application demonstrating an interactive dashboard to 
# visualize the Cereals dataset. This Shiny applications shows how plots and other outputs can
# be dynamically updated when user interacts with the application using radio buttons,
# checkboxes, et cetera.


#Load libraries
library(shiny)
library(ggplot2)
library(dplyr)

#Load dataset
cereal_data<-read.csv("C:/Users/pc/Desktop/cereals.csv", stringsAsFactors = FALSE)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Know more about your favorite cereal's nutritional value!"),
  br(),
  tags$div( style= "border: 2px solid black", column( width = 2,
                                                      fluidRow(
                                                        
                                                        #***CHECK BOXES FOR CEREAL BRAND
                                                        checkboxGroupInput("cerealBrand", h3("Pick your brand:"),
                                                                           choiceNames = 
                                                                             list("Nabisco", "Quaker", "Kellogs", "General Mills", "Ralston", "AHFP"),
                                                                           choiceValues = 
                                                                             list("N","Q","K","G","R","A"), selected = "N"),
                                                        
                                                        #***RADIO BUTTONS FOR METRICS
                                                        radioButtons("cerealMetric", h3("Choose a metric:"),
                                                                     choiceNames = 
                                                                       list("Calories", "Protein", "Fat", "Carbohydrates", "Sugars", "Vitamins"),
                                                                     choiceValues = 
                                                                       list("Calories","Protein", "Fat", "Carbo", "Sugars", "Vitamins"),
                                                                     selected = "Calories")))),
  
  #Main Panel to display graph
  mainPanel(
    fluidRow(
      column( width=10, offset = 1,
              plotOutput("plot"))),
    fluidRow(
      hr(),
      column( width = 5,
              br(),
              br(),
              tableOutput("results")),
      column( width=3, offset=10,
              textInput("searchText", NULL, placeholder = "Search..."))
      
    ),
    verbatimTextOutput("text")
  )
) #fluidPage


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$plot <- renderPlot({
    
    cerealMetric_data<- cereal_data[trimws(cereal_data$Manuf)==input$cerealBrand,]
    
    if(input$cerealMetric=="Calories"){
      temp_data <- cerealMetric_data[,4]
    }
    
    if(input$cerealMetric=="Protein"){
      temp_data <- cerealMetric_data[,5]
    }
    
    if(input$cerealMetric=="Fat"){
      temp_data <- cerealMetric_data[,6]
    }
    
    if(input$cerealMetric=="Carbo"){
      temp_data <- cerealMetric_data[,9]
    }
    
    if(input$cerealMetric=="Sugars"){
      temp_data <- cerealMetric_data[,10]
    }
    
    if(input$cerealMetric=="Vitamins"){
      temp_data <- cerealMetric_data[,12]
    }
    
    #Render dot plot using user input
    ggplot(cerealMetric_data, aes(Name,temp_data)) +
      geom_point(aes(colour = factor(cerealMetric_data$Manuf)), size=3) +
      labs(x = "Cereal Names", y = input$cerealMetric)+
      theme(legend.box = "horizontal", legend.position = "top")+
      coord_flip()
    
  })
  
  #Render text from textbox and search for cereal name in dataset
  output$text <- renderPrint({
    output$results <- renderTable({
      cereal_data[cereal_data$Name == input$searchText,c(1,2,4,5,6,9,10,12,16)]
    })
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

