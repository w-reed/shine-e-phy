library(shiny)
library(shinydashboard)
library(plotly)

ui = fluidPage(
  
  title = "Phylofeast",
  includeCSS("www/style.css"),
  
  navbarPage(title = img(class = "topimg", src = 'logo.png', height = '70px', width = '280px', bottom = "10px"),
                
                tabPanel("Data",
                         sidebarPanel(
                           fileInput(inputId = 'foodJournal', "Food Journal Data", width = 200),
                           actionButton(inputId = "demo", label = "Demo Mode")
                         ),
                         mainPanel(
                           dataTableOutput(outputId = 'data')
                         )
                ),
             
                tabPanel("Phylogeny",
                         sidebarPanel(
                           
                         ),
                         mainPanel(
                           plotOutput(outputId = "Phylo", width = 1000, height = 600)
                         )
                         
                ),
                
                tabPanel("Diversity",
                         mainPanel(
                           plotlyOutput(outputId = "richness", width = 500, height = 200),
                           plotlyOutput(outputId = "diversity", width = 500, height = 200),
                           plotlyOutput(outputId = "faithsPD", width = 500, height = 200)
                         )
                         
                ),
                
                tabPanel("Geography",
                         mainPanel(
                           plotlyOutput(outputId = "map")
                         )
                )
)
) 

server = function(input, output, session) {
  
  source('functions.R')
  
  reactive({
    inFile = input$foodJounal
    if(is.null(input$foodJournal))     
      return(NULL) 
    
    
    s1 <<- read.csv(inFile$datapath, header = TRUE)
    
    output$data = renderDataTable({

    })
    
    
    
  })
  
  #source('core.R')
  
  observeEvent(input$demo,{
    source("demo.R")
    
    output$data = renderDataTable({
      s1.1 = as.data.frame(t(s1))
      s1.1 = s1.1[c(2:nrow(s1.1)), ]
      colnames(s1.1) = c("Day 1", "Day 2", "Day 3", "Day 4", "Day 5", "Day 6", "Day 7")
      return(s1.1)
    })
    
  })
  
  

  
  

  
  


  
  output$Phylo = renderPlot({
    ggtree(tree.pruned) + 
      geom_tiplab3() 
  })
  
  output$richness = renderPlotly({
    pr = plot_ly(r1, x = ~Day, y = ~Richness, type = "scatter", mode = "lines+markers", line = list(color = rgb(0,0,0.8), width = 4), marker = list(color = rgb(0.5882353, 0.5882353,0.5882353), size = 11))
    add_trace(pr, data = r2, x = ~Day, y = ~Richness, mode = "lines+markers", line = list(color = rgb(0.2,0.2,0.2), width = 4), marker = list(color = rgb(0.5882353, 0.5882353,0.5882353), size = 11))
  })
  
  output$diversity = renderPlotly({
    pd = plot_ly(d1, x = ~Day, y = ~Diversity, type = 'scatter', mode = 'lines+markers', line = list(color = rgb(0.8,0,0), width = 4), marker = list(color = rgb(0.5882353, 0.5882353,0.5882353), size = 11))
    add_trace(pd, data = d2, x = ~Day, y = ~Diversity, mode = "lines+markers", line = list(color = rgb(0.2,0.2,0.2), width = 4), marker = list(color = rgb(0.5882353, 0.5882353,0.5882353), size = 11))
  })
  
  output$faithsPD = renderPlotly({
    pf = plot_ly(f1, x = ~Day, y = ~PD, type = 'scatter', mode = "lines+markers", line = list(color = rgb(0,0.8,0), width = 4), marker = list(color = rgb(0.5882353, 0.5882353,0.5882353), size = 11))
    add_trace(pf, data = f2, x = ~Day, y = ~PD, mode = "lines+markers", line = list(color = rgb(0.2,0.2,0.2), width = 4), marker = list(color = rgb(0.5882353, 0.5882353,0.5882353), size = 11))
  })
  
  
  output$map = renderPlotly({
    maply = ggplot(world) + 
      geom_sf() + 
      geom_sf(data = sample_gs, fill = rgb(0.01, 0.02, 0.3, 0.7), size = 0.1)
    
    ggplotly(maply)
    
  })
  
  
}

shinyApp(ui = ui, server = server)

