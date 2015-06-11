library(dendextend)

set_diff = lapply(8:2, function(i) unlist(setdiff(hp[[i-1]], hp[[i]])))
cummulative = lapply(1:7, function(till, set_diff){
  if(till>1)
    Reduce('union', set_diff[1:(till-1)])
  else
    integer(0)
}, set_diff)
set_diff2 = mapply(intersect, set_diff, cummulative)

leafs = mapply(setdiff, set_diff, set_diff2)




a = list()
a$merge = do.call('rbind', lapply(1:7, function(i){
  if(length(leafs[[i]]) == 2){
    return(-leafs[[i]])
  }
  if(length(leafs[[i]]) == 1){
    i1 = max(which(sapply(set_diff[1:(i-1)], 
                          function(s) 
                            length(setdiff(setdiff(set_diff[[i]], leafs[[i]]), s)) == 0)))
    return(c(-leafs[[i]], i1))
  }
  if(length(leafs[[i]]) == 0){
    i1 = max(which(sapply(set_diff[1:(i-1)], 
                          function(s) 
                            length(setdiff(s, setdiff(set_diff[[i]], leafs[[i]]))) == 0)))
    i2 = max(which(sapply(set_diff[1:(i-1)], 
                          function(s) 
                            length(setdiff(s, setdiff(setdiff(set_diff[[i]], set_diff[[i1]]), leafs[[i]]))) == 0)))
    return(c(i1, i2))
  }
}))
a$height <- 1:nrow(a$merge)
a$order <- hp[[1]][[1]]
a$labels <- rep("", 8)#hp[[1]][[1]]
class(a) <- "hclust"        # make it an hclust object

hc = as.dendrogram(a)

# Function to color branches
colbranches <- function(n, col)
{
  a <- attributes(n) # Find the attributes of current node
  # Color edges with requested color
  attr(n, "edgePar") <- c(a$edgePar, list(col=col, lwd=2))
  n # Don't forget to return the node!
}

plt_den = function(LVL, h){
  d1=color_branches(hc, k=LVL, col = rainbow(8)[1+(3+sapply(hp[[LVL]], function(i) i[[1]]))%% 8])
  d1 = assign_values_to_branches_edgePar(d1, value = 3, edgePar = "lwd")
  
  plot(d1, lwd=20, lty = 3, axes = FALSE)
  rect(0, h, 10, 10, col='white', border=NA)
}

lvl = 6
plt_den(lvl, 8.5-lvl)

# d1=color_branches(hc, k=7, col = rainbow(7))
# d1 = assign_values_to_leaves_edgePar(d1, value = 3, edgePar = "lwd")


