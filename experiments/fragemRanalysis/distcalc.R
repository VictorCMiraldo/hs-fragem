library(apcluster)
library(transport)
library(emdist)
library(fastR)
library(tidyr)

# naming data after import and time series plots 
x1 <- hanon2.csv
p1 <- `ij221-g2.csv`[1,]
p2 <- `ij221-g2.csv`[2,]
c1 <- `ij221-g2.csv`
c2 <- `ij224-g2.csv`

# data manipulation and cleaning
a <- t(as.matrix(sapply(unlist(c(p1)),as.double)))
b <- t(as.matrix(sapply(unlist(c(p2)),as.double)))

a[is.na(a)] <- 0
b[is.na(b)] <- 0

p1 <- data.matrix(`ij221-g2.csv`)
p2 <- data.matrix(`hanon2.csv`)
p3 <- data.matrix(`ij224-g2.csv`)
p4 <- data.matrix(`cellosui-g2.csv`)
p5 <- data.matrix(`ij224-g2-s1.csv`)
p6 <- data.matrix(`ij221-g2-s.csv`)

`ij221-g2-s.csv` %>% drop_na()

p1[is.na(p1)] <- -100
p1 <- p1[p1[,2]!=-100 & p1[,ncol(p1)]!=-100,]
p3 <- p3[p3[,2]!=-100,]
p3[is.na(p3)] <- -100
p3 <- p3[p3[,2]!=-100 & p3[,ncol(p3)]!=-100,]
p3 <- p3[p3[,2]!=-100,]

p[is.na(p)] <- -100
p <- p[p[,2]!=-100 & p[,ncol(p)]!=-100,]
p <- p[p[,2]!=-100,]
x <- rbind(p1[1:18,1:14], p2[1:18,1:14],p3[1:18,1:14],p4[,1:14])
x <- scale(x)

c1 <- data.matrix(`ij221-g2.csv`[1:96,1:22])
c2 <- data.matrix(`ij224-g2.csv`)
c1[is.na(c1)] <- 0
c2[is.na(c2)] <- 0

# plain distance
d <- dist(x,diag = TRUE, upper = TRUE, p = 2)
d <- dist(x,diag = TRUE, upper = TRUE,method='manhattan')
d <- dist(x,diag = TRUE, upper = TRUE,method='canberra')
d <- dist(x,diag = TRUE, upper = TRUE,method='minkowski')
d <- dist(x,diag = TRUE, upper = TRUE,method='binary')
heatmap.2(as.matrix(d),Rowv=FALSE, Colv=FALSE, trace="none")
##Example
# a <- c(7,1,9,1,3)
# b <- c(4,9,1,3,1)
# c <- c(9,1,7,2,2)
# dist(rbind(a,b,c))

# distance between vectors and matrix
d <- dista(a,b,type="euclidean") # problem with package version

# Earth Mover's package
emdi <- emd2d(c1, c2)
emd(a,b)
## Example
A <- matrix(1:6 / sum(1:6), 2)
B <- matrix(c(0, 0, 0, 0, 0, 1), 2)
emd2d(A, B)
# if we bring the rows closer, the distance will be reduced
# since mass from the first row has to move down
emd2d(A, B, 0.1)

# use Manhattan distance instead
emd2d(A, B, dist="manhattan")
# same, but using R-side closure
emd2d(A, B, dist=function(x, y) sum(abs(x - y)))

# the positions can overlap - this is a degenerate case of that
emd2d(A, B, rep(0, 3), rep(0, 2))
# and just a sanity check
emd2d(A, A) + emd2d(B, B)

# and the weight/location code should, hopefully have the same results
A. <- matrix(c(1:6 / sum(1:6), 1,2,1,2,1,2, 1,1,2,2,3,3), 6)
B. <- matrix(c(1, 2, 3), 1)
stopifnot(emd(A., B.) == emd2d(A, B))
stopifnot(emd(A., B.) == emdw(A.[,-1], A.[,1], B.[,-1,drop=FALSE], B.[,1]))


# The transport package
nc <- 14
nr <- 18 * 4
ws <- matrix(ncol=nr,nrow=nr)
for (i in 1:nr) {
  for (j in 1:nr) {
    print(i)
    print(j)
      ws[i,j] <- wasserstein1d(x[i,1:nc],x[j,1:nc])  # calculate new column and bind to calculated ones
  }
}
heatmap.2(as.matrix(ws),Rowv=FALSE, Colv=FALSE, trace="none")

wasserstein1d(a,b,p=1)
wasserstein(c(x),c(x))
## Example
x <- rnorm(200)
y <- rnorm(150,2)
wasserstein1d(x,y)
## Example
x <- pp(matrix(runif(500),250,2))
y <- pp(matrix(runif(500),250,2))
wasserstein(x,y,p=1)
wasserstein(x,y,p=2)
costm <- matrix(sample(1:20, 100, replace=TRUE), 10, 10)
transport(a, b, costm, method = c("shortsimplex", "revsimplex", "primaldual"), control = list())
## S3 method for class 'pgrid'
transport(a, b, p = NULL, control = list())
## S3 method for class 'pp'
transport(a, b, p = 1, method = "revsimplex", control = list())
## S3 method for class 'wpp'
transport(a, b, p = 1, method = c("revsimplex", "shortsimplex", "primaldual"),control = list())

# -------------------------------------------------------------------------------------
# Example from apcluster {
## create two Gaussian clouds
cl1 <- cbind(rnorm(100, 0.2, 0.05), rnorm(100, 0.8, 0.06))
cl2 <- cbind(rnorm(100, 0.7, 0.08), rnorm(100, 0.3, 0.05))
x <- rbind(cl1, cl2)


## create negative distance matrix (default Euclidean)
sim1 <- negDistMat(p)

## compute similarities as squared negative distances
## (in accordance with Frey's and Dueck's demos)
sim2 <- negDistMat(x, r=2)

## compute RBF kernel
sim3 <- expSimMat(x, r=2)

# add heatmap visualisation
heatmap.2(sim1, Rowv=FALSE, Colv=FALSE,trace="none")   
heatmap.2(sim2, Rowv=FALSE, Colv=FALSE)
heatmap.2(sim3, Rowv=FALSE, Colv=FALSE)

## compute similarities as squared negative distances
## all samples versus a randomly chosen subset 
## of 50 samples (for leveraged AP clustering)
sel <- sort(sample(1:nrow(x), nrow(x)*0.25)) 
sim4 <- negDistMat(x, sel, r=2)
heatmap.2(sim4)

## example of leveraged AP using Minkowski distance with non-default
## parameter p
cl1 <- cbind(rnorm(150, 0.2, 0.05), rnorm(150, 0.8, 0.06))
cl2 <- cbind(rnorm(100, 0.7, 0.08), rnorm(100, 0.3, 0.05))
x <- rbind(cl1, cl2)

apres <- apclusterL(s=negDistMat(method="minkowski", p=2.5, r=2),
                    x, frac=0.2, sweeps=3, p=-0.2)
show(apres)
# }