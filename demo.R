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

## Read in the plants
plants = read.csv("data/plants.csv")
plants$Species = sub("^(\\S*\\s+\\S+).*", "\\1", plants$Scientific.Name.with.Author)
plants$Genus = gsub(' [A-z ]*', '' , plants$Species)

## filter out duplicates of genus
plants_singular = plants[, c("Genus", "Family")]
plants_singular$Family = ifelse(plants_singular$Family == "", NA, plants_singular$Family)
plants_singular = plants_singular[!duplicated(plants_singular[,"Genus"]) & !is.na(plants_singular$Family), ]

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

## calculated PD
f1 = faithsPD(s1, tree.pruned)
f2 = faithsPD(s2, tree.pruned2)

tree.pruned$Nnode
tree.pruned$node.label

fs = as.data.frame(cbind(tree.pruned$node.label,  c(1:length(tree.pruned$node.label))))
colnames(fs) = c("family", "node")

fs = fs[fs$family != "", ]
fs