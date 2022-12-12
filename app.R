

library(shiny)
library(ggplot2)

incendios <- read.csv("n_incendios (1).csv", header = T, sep = ";")
incendios <- as.data.frame(incendios)
ardida <- read.csv("area_ardida (1).csv", header = T, sep = ";")
ardida$Anos <- as.character(ardida$Anos)

ui <- fluidPage(
  
  # App title ----
  titlePanel("Fires"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select the random distribution type ----
      selectInput("region", "Região:", 
                  choices=colnames(ardida[,-1])),
      # Include clarifying text ----
      helpText(" Escolha o país de interesse e o separador no lado direito que lhe interesse.")
      
      ),
      
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Area ardida", plotOutput("firePlot")),
                  tabPanel("Numero de incendios", plotOutput("numberPlot")),
                  tabPanel("Modelo linear area vs n incendios", verbatimTextOutput("summary")),
                  tabPanel("Area vs N incendios", plotOutput("vsplot"))
                  
      )
      
    )
  )
)


# Define server logic required to draw a histogram
server = function(input, output) {
  
  d <- reactive({
    dist <- switch(input$region)
    
   
  })
  
  # Fill in the spot we created for a plot
  output$firePlot <- renderPlot({
    
    dist <- input$region
    
    # Render a barplot
    ggplot(ardida, aes(main=input$region, y=ardida[,input$region], x=Anos))+
      geom_bar(stat = "identity", fill = "#FF6A6A") + 
      xlab("Anos") +
      ylab("") + 
      coord_flip()
  })
  
  output$numberPlot <- renderPlot({
    
    dist <- input$region
    
    # Render a barplot
    ggplot(incendios, aes(main=input$region, y=incendios[,input$region], x=Anos))+
      geom_bar(stat = "identity", fill = "#FF6A6A") + 
      xlab("Anos") +
      ylab("")+
      coord_flip()
  })
  

  # Generate a summary of the data ----

  
  output$summary <- renderPrint({
    dist <- input$region
    summary(lm(ardida[,input$region] ~ incendios[, input$region]))
  })
  
  
  
  output$vsplot <- renderPlot({
    dist <- input$region
    
   plot( incendios[,input$region], ardida[,input$region], xlab="N incendios", ylab = "Area ardida", main=input$region)
   abline(lm(ardida[,input$region] ~ incendios[, input$region]))
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)