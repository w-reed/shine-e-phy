library(phylocanvas)
library(ape)
library(ggtree)
library(picante)

tree <- read.tree("PhylofeastTree.tre")

## Read in the plants
plants = read.csv("plants.csv")
plants$Species = sub("^(\\S*\\s+\\S+).*", "\\1", plants$Scientific.Name.with.Author)
plants$Genus = gsub(' [A-z ]*', '' , plants$Species)

## filter out duplicates of genus
plants_singular = plants[, c("Genus", "Family")]
plants_singular$Family = ifelse(plants_singular$Family == "", NA, plants_singular$Family)
plants_singular = plants_singular[!duplicated(plants_singular[,"Genus"]) & !is.na(plants_singular$Family), ]

simulate.diet = function(tips){
  abs = c(0,0,0,1,2,3,4,5,6,7,8,9)
  
  ## sample the number of genera consumed from a normal distribution, tweak-able here but 
  n1 = round(rnorm(1, 25, 3))
  t1 = sample(tips, n1, replace = FALSE)

  
  j1 = as.data.frame(matrix(nrow = 7, ncol = (n1 + 1)))
  colnames(j1) = c("Genus", t1) 
  
  ## Loop to get abundance of each Genus, also populate Genus "column" with Day + i (Day 1, Day 2, etc.)
  for(i in 1:7){
    n2 = sample(abs, n1, replace = TRUE)
    j1[i, ] = c(paste("Day", i), n2)
  }

  for(j in 1:ncol(j1)){
    j1[, j] = ifelse(j1[, j] == 0, NA, j1[,j])
  }
  
  return(j1)
  

  
}

s1 = simulate.diet(tree$tip.label)

richness = function(log){
  
  r1 = as.data.frame(matrix(nrow = 0, ncol = 2))
  colnames(r1) = c("Day", "Richness")
  
  for(i in 1:nrow(log)){
    filled = (sum(!is.na(log[i, ])))
    r1[i,"Day"] = i
    r1[i, "Richness"] = filled - 1
  }
  return(r1)
  
}


diversity = function(log){
  d1 = as.data.frame(matrix(nrow = 0, ncol = 2))
  colnames(d1) = c("Day", "Diversity")
  
  for(i in 1:nrow(log)){
    x1 = log[i, 2:ncol(log)]
    x2 = as.numeric(x1[, !is.na(x1)])
    x3 = x2 / sum(x2)
    x4 = x3 ^2
    d1[i,"Day"] = i
    d1[i,"Diversity"] = 1 / sum(x4)
    
  }

  return(d1)
}

faithsPD = function(log, tree){
  
  rownames(log) = log[, 1]
  log[is.na(log)] = 0
  
  x1 <- as.matrix(log[, 2:ncol(log)])
  class(x1) = "numeric"
  total <- apply(x1,MARGIN=2,FUN=sum)
  x3 <- rbind(x1,total)
  
  f1 = pd(x3, tree)
  Day = 1:7
  
  f1 = cbind(f1[c(1:7), ], Day)
  
}



