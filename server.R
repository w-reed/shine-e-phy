library(plotly)
library(htmlwidgets)


## source the base code and functions for the app
source("app.R")

## Read in big tree
tree = read.tree("PhylofeastTree.tre")

## set up null objects to be populated within server
xx = NULL
action = "none"
node_to_collapse = NULL
node_to_expand = NULL

## simulate a food journal
s1 = simulate.diet(tree$tip.label)
r1 = richness(s1)
d1 = diversity(s1)

s2 = simulate.diet(tree$tip.label)
r2 = richness(s2)
d2 = diversity(s2)


## Prune tree
tips2drop = tree$tip.label[which(!tree$tip.label%in%colnames(s1))]
tree.pruned = drop.tip(tree,tips2drop)
tips2drop = tree$tip.label[which(!tree$tip.label%in%colnames(s2))]
tree.pruned2 = drop.tip(tree, tips2drop)

f1 = faithsPD(s1, tree.pruned)
f2 = faithsPD(s2, tree.pruned2)

server = function(input, output, session) {

  eventReactive(input$collapse,{
    
  })
  
  eventReactive(input$expand,{
    
  })
  
  output$Phylo = renderPlotly({
  
    xx = event_data("plotly_click")
    x1 = ggtree(tree.pruned)
    
    print(x1$data)
    
    print(action)
    
    if(!is.null(xx) & (input$collapse %% 2) == 1){ 
     points_data = as.matrix(dist(rbind(x1$data[, c("x", "y")], xx[, c("x", "y")])))
     click_distances = points_data[1:nrow(points_data) - 1,ncol(points_data)]
     node_to_collapse <<- c(node_to_collapse, which(click_distances %in% min(click_distances)))
    }
    
    if(!is.null(xx) & (input$expand %% 2) == 1){ 
      points_data = as.matrix(dist(rbind(x1$data[, c("x", "y")], xx[, c("x", "y")])))
      click_distances = points_data[1:nrow(points_data) - 1,ncol(points_data)]
      node_to_expand <<- c(node_to_expand, which(click_distances %in% min(click_distances)))
    }
    
    print(node_to_collapse)
    print(node_to_expand)
    
    if(!is.null(node_to_collapse)){
      for (i in 1:length(node_to_collapse))
        x1 = collapse(x1, node = node_to_collapse[i]) +
              geom_point(aes(subset=(node==node_to_collapse[i])), shape=23, size=5, fill='red')  
    }
    if(!is.null(node_to_expand)){
      for (i in 1:length(node_to_expand))
        x1 = expand(x1, node = node_to_expand[i])
    }
    
    
    ggplotly(x1)
   
    
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
  
}

