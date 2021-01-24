#### simulate.diet() ####
## This function is mostly used for development, simulates a food journal of 1
## student. It only has one parameter, and it outputs a data frame in an 
## approriate format for making figures. It  is probably unrealistic in that it 
## will pull some strange genera... could add probabilities to df.
## Params:
## 'tips' = A vector of possible genera (or species) to make a simulated diet


simulate.diet = function(tips){
  abs = c(0,0,0,1,2,3,4,5,6,7,8,9)
  
  ## sample the number of genera consumed from a normal distribution, tweak-able 
  n1 = round(rnorm(1, 25, 3))
  t1 = sample(tips, n1, replace = FALSE)
  
  
  j1 = as.data.frame(matrix(nrow = 7, ncol = (n1 + 1)))
  colnames(j1) = c("Genus", t1) 
  
  ## Loop to get abundance of each Genus, also populate Genus "column" with 
  ## Day + i (Day 1, Day 2, etc.)
  for(i in 1:7){
    n2 = sample(abs, n1, replace = TRUE)
    j1[i, ] = c(paste("Day", i), n2)
  }
  
  for(j in 1:ncol(j1)){
    j1[, j] = ifelse(j1[, j] == 0, NA, j1[,j])
  }
  
  return(j1)
  
}

#### richness() ####
## This function uses the output simulated food journal, or an actual food 
## journal to calculated species richness for each day of the 7 day logging.
## Params:
## 'log' = a food log object in the appropriate format

richness = function(log){
  
  ## Make a holder data frame to populate with loop
  r1 = as.data.frame(matrix(nrow = 0, ncol = 2))
  colnames(r1) = c("Day", "Richness")
  
  ## Loop through each row and total species present, remove 1 for the weird
  ## first column
  for(i in 1:nrow(log)){
    filled = (sum(!is.na(log[i, ])))
    r1[i,"Day"] = i
    r1[i, "Richness"] = filled - 1
  }
  return(r1)
  
}

#### diversity() ####
## This function, similar to richness() takes a food log - either simulated 
## or real - and returens the daily diversity index. "Shannon's Diversity Index"
## in this case.
## Params:
## 'log' = a food log object in the appropriate format

diversity = function(log){
  
  ## Make a holder data frame to populate with loop
  d1 = as.data.frame(matrix(nrow = 0, ncol = 2))
  colnames(d1) = c("Day", "Diversity")
  
  ## Loop through and calculated shannon's DI 
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

#### faithsPD() ####
## This function is another similar function to diversity() and richness(), but
## needs to do a little reformatting to properly calculated phylogenetic 
## diverstiy (PD). 
## Params:
## 'log' = a food log object in the appropriate format
## 'tree' = a tree object for the present species 

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

#### geom_tiplab3 ####
## this is just a modification of geom_tiplab() from ggtree. It was not working
## in shiny because of the match.call function. Maybe this will be fixed in a 
## future release but for now just hard set nodelab to false.
geom_tiplab3 = function (mapping = NULL, hjust = 0, align = FALSE, linetype = "dotted", 
                         linesize = 0.5, geom = "text", offset = 0, as_ylab = FALSE, 
                         ...){
  #.call <- match.call(call = sys.call(sys.parent(1)))
  #nodelab <- ifelse(as.list(.call)[[1]] == "geom_nodelab", TRUE, FALSE)
  nodelab = FALSE
  
  structure(list(mapping = mapping, hjust = hjust, align = align, 
                 linetype = linetype, linesize = linesize, geom = geom, 
                 offset = offset, as_ylab = as_ylab, nodelab = nodelab, 
                 ...), class = "tiplab")
}
