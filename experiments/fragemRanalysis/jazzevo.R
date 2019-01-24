p1 <- complexityGlobal_Symbolic[,2]
dp1<- diff(p1, differences = 1)

p2 <- complexityGlobal_AudioSourceSep[,2]
dp2<- diff(p2, differences = 1)

p3 <- complexityGlobal_AudioHarmonicPart[,2]
dp3<- diff(p3, differences = 1)

p4 <- complexityGlobal_Audio[,2]
dp4<- diff(p4, differences = 1)

x <- as.double(as.matrix(dp3))
x <-  as.double(as.matrix(dp4))

x <- as.matrix(rbind(p1,p2,p3,p4))

x <- scale(x)

d <- dist(x, diag = TRUE, upper = TRUE, p = 2)
d <- dist(x,diag = TRUE, upper = TRUE,method='manhattan')
d <- dist(x,diag = TRUE, upper = TRUE,method='canberra')
d <- dist(x,diag = TRUE, upper = TRUE,method='minkowski')
d <- dist(x,diag = TRUE, upper = TRUE,method='binary')

library(ggplot2)
library(gplots)

heatmap.2(as.matrix(d), Rowv=FALSE, Colv=FALSE, trace="none", col=rev(heat.colors(255)))

          

jazzz <- as.matrix(transpose(as.data.frame(x)))
z <- zoo(jazzz)

library(TSdist)
# dissimapproxDistance()
zoo.series1 <- zoo(dp1)
zoo.series2 <- zoo(dp2)
zoo.series3 <- zoo(dp3)
zoo.series4 <- zoo(dp4)
TSDistances(zoo.series1, zoo.series2, distance="euclidean")
TSDistances(zoo.series1, zoo.series3, distance="euclidean")
TSDistances(zoo.series1, zoo.series4, distance="euclidean")
TSDistances(zoo.series2, zoo.series3, distance="euclidean")
TSDistances(zoo.series2, zoo.series4, distance="euclidean")
TSDistances(zoo.series3, zoo.series4, distance="euclidean")
# 
