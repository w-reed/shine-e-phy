library(phylocanvas)
library(ape)
library(ggtree)
library(picante)
library(plotly)
library(htmlwidgets)
library(shinyjs)
library(maps)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(BIEN)

tree <- read.tree("tree/PhylofeastTree.tre")
world <- ne_countries(scale = "medium", returnclass = "sf")

## simulate a food journal
r1 = richness(s1)
d1 = diversity(s1)

# r2 = richness(s2)
# d2 = diversity(s2)

## Prune tree
tips2drop = tree$tip.label[which(!tree$tip.label%in%colnames(s1))]
tree.pruned = drop.tip(tree,tips2drop)
#tips2drop = tree$tip.label[which(!tree$tip.label%in%colnames(s2))]
#tree.pruned2 = drop.tip(tree, tips2drop)

## calculated PD
f1 = faithsPD(s1, tree.pruned)
#f2 = faithsPD(s2, tree.pruned2)

#tree.pruned$Nnode
#tree.pruned$node.label

#fs = as.data.frame(cbind(tree.pruned$node.label,  c(1:length(tree.pruned$node.label))))
#colnames(fs) = c("family", "node")

#fs = fs[fs$family != "", ]
#fs


for(i in common_species){
  BIEN_ranges_species(i, directory = paste("data", i, sep = "/"))
}

