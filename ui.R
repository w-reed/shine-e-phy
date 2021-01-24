library(shiny)
library(plotly)

ui = fluidPage(
  
  mainPanel(
    fluidRow(
      actionButton(inputId = "collapse", label = "Collapse"),
      actionButton(inputId = "expand", label = "Expand")
    ),
    fluidRow(
      
      column(1, 
             plotOutput(outputId = "Phylo", width = 400, height = 600)
             ),
    
      column(5, offset = 4,
             plotlyOutput(outputId = "richness", width = 500, height = 200),
             plotlyOutput(outputId = "diversity", width = 500, height = 200),
             plotlyOutput(outputId = "faithsPD", width = 500, height = 200)
             )

      )

      
    )


  )
  
