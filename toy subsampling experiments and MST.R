library(here)
library(lhs)
library(DiceDesign)
library(clhs)

# taken from DiceDesign - added inputs for color of scenarios and option to plot background point
mstCriteria_modified=function (design, plot2d = "FALSE", point_col='black', background_points=all, b_point_col='gray50', point_size=2) 
{
  m <- design
  n <- nrow(m)
  D <- as.matrix(dist(m))
  diag(D) <- rep(Inf, n)
  x <- rep(0, n - 1)
  Peak <- 1
  p <- Peak
  Tree <- list(c())
  Tree <- rep(Tree, n)
  if (plot2d) {
    plot(all, pch = 19, col = b_point_col, xlab = "", ylab = "", xaxt='n', yaxt='n', cex=point_size)
    points(m, pch = 19, col = point_col, xlab = "", ylab = "", xaxt='n', yaxt='n',cex=point_size)
  }
  for (i in 1:(n - 1)) {
    if (length(Peak) == 1) {
      new_p <- which.min(as.numeric(D[p, ]))
      x[i] <- D[p, new_p]
      D[new_p, Peak] = D[Peak, new_p] <- Inf
      Peak <- c(Peak, new_p)
    }
    else {
      A <- D[Peak, ]
      w <- which.min(A)
      reste <- w%%nrow(A)
      if (reste != 0) {
        p <- Peak[reste]
        new_p <- w%/%nrow(A) + 1
      }
      else {
        p <- Peak[nrow(A)]
        new_p <- w%/%nrow(A)
      }
      x[i] <- D[p, new_p]
      D[new_p, Peak] <- D[Peak, new_p] <- Inf
      Peak <- c(Peak, new_p)
    }
    Tree[[p]] <- c(Tree[[p]], new_p)
    Tree[[new_p]] <- c(Tree[[new_p]], p)
    if (plot2d == "TRUE") {
      lines(rbind(m[p, ], m[new_p, ]), col = "black")
    }
  }
  return(list(tree = Tree, stats = c(mean(x), sqrt(var(x)))))
}

set.seed(10)

all=data.frame(geneticLHS(n=100, k = 2))

keep=clhs(data.frame(all), 60, use.cpp = T)
clhs_many=all[keep,]

keep=clhs(data.frame(all), 20, use.cpp = T)
clhs_few=all[keep,]

pt=2
all_col="#d94801"
b_point_col=all_col
    

dev.off()

pdf(here('figures', "toy subsample experiments - MST.pdf"), width = 8.73, height = 3.22)

par(mfrow=c(1,3))


all_tree=mstCriteria_modified(design = all, TRUE, point_col =all_col,b_point_col = all_col, point_size = pt)

tree_few=mstCriteria_modified(design = clhs_few, TRUE, point_col = "#238b45",b_point_col = 'grey80', point_size = pt)

tree_many=mstCriteria_modified(design = clhs_many, TRUE, point_col = "#2171b5",b_point_col ='grey80', point_size = pt)

dev.off()

scores=data.frame(all=all_tree$stats, few=tree_few$stats, many=tree_many$stats)
scores=rbind(scores, c(mindist(all), mindist(clhs_few), mindist(clhs_many)))

row.names(scores)=c('MSTmean', 'MSTsd', 'mindist')

scores_scaled=scores
scores_scaled[1,]=scores_scaled[1,]/scores_scaled$all[1]
scores_scaled[2,]=scores_scaled[2,]/scores_scaled$all[2]
scores_scaled[3,]=scores_scaled[3,]/scores_scaled$all[3]


# plot points without MST

pt=2
b_point_col="#d94801"

dev.off()

pdf(here('figures', "toy subsample experiments - no MST.pdf"), width = 8.73, height = 3.22)

par(mfrow=c(1,3))

plot(all, pch = 19, col = b_point_col, xlab = "", ylab = "", xaxt='n', yaxt='n', cex=pt)

plot(all, pch = 19, col = 'grey80', xlab = "", ylab = "", xaxt='n', yaxt='n', cex=pt)
points(clhs_few, pch = 19, col = "#238b45", xlab = "", ylab = "", xaxt='n', yaxt='n',cex=pt)

plot(all, pch = 19, col = 'grey80', xlab = "", ylab = "", xaxt='n', yaxt='n', cex=pt)
points(clhs_many, pch = 19, col = "#2171b5", xlab = "", ylab = "", xaxt='n', yaxt='n',cex=pt)

dev.off()



