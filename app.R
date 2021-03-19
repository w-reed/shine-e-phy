library(shiny)
library(shinydashboard)
library(plotly)

rm(list = ls())

common_species = c("Solanum lycopersicum", "Solanum tuberosum", "Malus domestica", "Triticum aestivum", "Zea mays", "Oryza sativa")

ui = fluidPage(
  
  title = "Phylofeast",
  includeCSS("www/style.css"),
  
  navbarPage(title = img(class = "topimg", src = 'logo.png', height = '65px', width = '260px', bottom = "10px"),
                
                tabPanel("Data",
                         sidebarPanel(
                           fileInput(inputId = 'foodJournal', "Food Journal Data"),
                           fileInput(inputId = 'classJournal', "Class Data"),
                           selectInput(inputId = "display", label = "Data to display:", choices = c("Individual","Class"), selected = "Individual"),
                           actionButton(inputId = "demo", label = "Demo Mode"),
                           actionButton(inputId = "clear", label = "Clear data")
                         ),
                         mainPanel(
                           dataTableOutput(outputId = 'data')
                         )
                ),
             
                tabPanel("Phylogeny",
                         sidebarPanel(
                           selectInput(inputId = "tree_type", label = "Tree to plot",choices = c("Individual", "Class"))
                         ),
                         mainPanel(
                           plotOutput(outputId = "Phylo", width = 1000, height = 600)
                         )
                         
                ),
                
                tabPanel("Diversity",
                         sidebarPanel(
                            selectInput(inputId = "diversity", label = "Measure of Diversity", 
                                        choices = c("Species Richness",
                                                    "Shannon's Diversity Index",
                                                    "Phylogenetic Diversity"))
                         ),
                         mainPanel(
                           plotlyOutput(outputId = "diversity_plot", width = 1000, height = 400),
                           textOutput(outputId = "diversity_description")
                         )
                         
                ),
                
                tabPanel("Geography",
                         sidebarPanel(
                           selectInput(inputId = "species_to_map", label = "Species", choices = common_species),
                           p("Select one of the available species to plot the geographic range. For some species this includes agricultural ranges as well as native ranges. More species coming soon!")
                         ),
                         mainPanel(
                           plotlyOutput(outputId = "map", width = 1000, height = 600)
                         )
                )
)
) 

server = function(input, output, session) {
  
  source('functions.R')
  
  dataInput = reactive({
    inFile = input$foodJournal
    validate(
      need(input$foodJournal, "Select a file!")
    )
    
    read.csv(inFile$datapath, header = TRUE)
    
  })
  
  dataInput2 = reactive({
    inFile = input$classJournal
    validate(
      need(input$classJournal, "Select a file!")
    )
    
    read.csv(inFile$datapath, header = TRUE)
    
  })
  
  output$data = renderDataTable({
    
    if(input$display == "Individual"){
      x = as.data.frame(dataInput())
      Genus = colnames(x)[2:length(colnames(x))]
      s1.1 = as.data.frame(t(dataInput()))
    } else if (input$display == "Class"){
      x = as.data.frame(dataInput2())
      Genus = colnames(x)[2:length(colnames(x))]
      s1.1 = as.data.frame(t(dataInput2()))
    }
    

    s1.1 = s1.1[c(2:nrow(s1.1)), ]
    colnames(s1.1) = c("Day 1", "Day 2", "Day 3", "Day 4", "Day 5", "Day 6", "Day 7")
    s1.2 = cbind(Genus, s1.1)
    
    return(s1.2)
    })
  
  observeEvent(input$foodJournal,{
    s1 <<- dataInput()
    source("core.R")
    
  })
  
  observeEvent(input$classJournal,{
    s2 <<- dataInput2()
    print(s2)
    source("core.R")
  })
  
  observeEvent(input$clear, {
    session$reload()
  })
  
  observeEvent(input$demo,{
    source("demo.R")
  
    output$data = renderDataTable({
      
      Genus = colnames(s1)[2:length(colnames(s1))]
      
      s1.1 = as.data.frame(t(s1))
      s1.1 = s1.1[c(2:nrow(s1.1)), ]
      colnames(s1.1) = c("Day 1", "Day 2", "Day 3", "Day 4", "Day 5", "Day 6", "Day 7")
      s1.2 = cbind(Genus, s1.1)
      return(s1.2)
    })
    
  })
  
  output$Phylo = renderPlot({
    if(input$tree_type == "Individual"){
      tree = tree.pruned
    } else if (input$tree_type == "Class"){
      tree = tree.pruned2
    }
    
    ggtree(tree) + 
      geom_tiplab3() 
  })
  
  output$diversity_plot = renderPlotly({
    if(input$diversity == "Species Richness"){
      pr = plot_ly(r1, x = ~Day, y = ~Richness, type = "scatter", mode = "lines+markers", 
                   line = list(color = rgb(0.5882353, 0.5882353,0.5882353), width = 3, dash = "dash"), 
                   marker = list(color = rgb(0.5882353, 0.5882353,0.5882353, 0.6),
                                 line = list(color = rgb(0.1,0.1,0.1, 0.8), width = 2), size = 16), name = "Individual")
      if(exists("s2")){
        pr = add_trace(pr, data = r2, x = ~Day, y = ~Richness, mode = "lines+markers", 
                  line = list(color = rgb(0.5882353, 0.5882353,0.5882353), width = 4, dash = "dash"), 
                  marker = list(color = rgb(220/255,229/255,216/255, 0.9), size = 16), name = "Class")
      }
      return(pr)
    } else if (input$diversity == "Shannon's Diversity Index"){
      pd = plot_ly(d1, x = ~Day, y = ~Diversity, type = 'scatter', mode = 'lines+markers', 
                   line = list(color = rgb(0.5882353, 0.5882353,0.5882353), width = 3, dash = "dash"), 
                   marker = list(color = rgb(0.5882353, 0.5882353,0.5882353, 0.6),
                                 line = list(color = rgb(0.1,0.1,0.1, 0.8), width = 2), size = 16), name = "Individual")
      if(exists("s2")){
        pd = add_trace(pd, data = d2, x = ~Day, y = ~Diversity, mode = "lines+markers", 
                  line = list(color = rgb(0.5882353, 0.5882353,0.5882353), width = 4, dash = "dash"), 
                  marker = list(color = rgb(220/255,229/255,216/255, 0.9), size = 16), name = "Class")
      }
      return(pd)
    } else if (input$diversity == "Phylogenetic Diversity"){
      pf = plot_ly(f1, x = ~Day, y = ~PD, type = 'scatter', mode = "lines+markers", 
                   line = list(color = rgb(0.5882353, 0.5882353,0.5882353), width = 3, dash = "dash"), 
                   marker = list(color = rgb(0.5882353, 0.5882353,0.5882353, 0.6),
                                 line = list(color = rgb(0.1,0.1,0.1, 0.8), width = 2), size = 16), name = "Individual")
      if(exists("s2")){
        pf = add_trace(pf, data = f2, x = ~Day, y = ~PD, mode = "lines+markers", 
                  line = list(color = rgb(0.5882353, 0.5882353,0.5882353), width = 4, dash = "dash"), 
                  marker = list(color = rgb(220/255,229/255,216/255, 0.9), size = 16), name = "Class")
      }
      return(pf)
    }
    
    

    
  })
  
  output$diversity_description = renderText({
    if(input$diversity == "Species Richness"){
      return("Species richness is the number of different species represented in an ecological community, landscape or region. Species richness is simply a count of species, and it does not take into account the abundances of the species or their relative abundance distributions.")
    } else if (input$diversity == "Shannon's Diversity Index"){
      return("The Shannon diversity index (H) is another index that is commonly used to characterize species diversity in a community. Like Simpson's index, Shannon's index accounts for both abundance and evenness of the species present. The proportion of species i relative to the total number of species (pi) is calculated, and then multiplied by the natural logarithm of this proportion (lnpi). The resulting product is summed across species, and multiplied by -1:")
    } else if (input$diversity == "Phylogenetic Diversity"){
      return("Phylogenetic diversity (“PD”) is a measure of biodiversity, based on phylogeny (the tree of life).  Faith (1992) defined the phylogenetic diversity of a set of species as equal to the sum of the lengths of all those branches on the tree that span the  members of the set. The branch lengths on the tree are informative because they count the relative number of new features arising along that part of the tree. This means that PD indicates “feature diversity” and “option value”")

    }
  })
  
  output$map = renderPlotly({
    ggplotly(plot_range(input$species_to_map))
    
  })
  
  
}

shinyApp(ui = ui, server = server)

