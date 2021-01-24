## source the base code and functions for the app
source("functions.R")
source("app.R")

server = function(input, output, session) {

  output$Phylo = renderPlot({
    ggtree(tree.pruned) + 
      geom_tiplab3() +
      geom_highlight(node = fs$node) + 
      xlim(0, max(tree.pruned$edge.length) * 2)
   
     
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

