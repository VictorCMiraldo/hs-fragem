library(gplots)
library(pheatmap)

df <-`iji2a2-22224-g4-s-x2-y4.csv`
df1 <-`ij221-g4-s-x2-y4.csv`

df <-`ij221-g2.csv`
df1 <-`ij221-g4.csv`

# different amount of randomness ()
df <-`ij221-g4-s-x2-y4.csv`
df1 <-`ij224-g4-s-x2-y4.csv`

# differences in the density of notes (the eight note pieces are too short)
df <-`ij224-240-g2.csv`
df1 <-`ij224-g2.csv`

# zoom level gives smooth transitions
df <-`ij221-g4-s-x2-y4.csv`
df1 <-`ij221-g4-s-x3-y4.csv`

# cleaning
df <- df[,2:ncol(df)]
df <- data.matrix(df)
df1 <- df1[,2:ncol(df1)]
df1 <- data.matrix(df1)
df <- df[is.na(df[,2])!=TRUE,]
df1 <- df1[is.na(df1[,2])!=TRUE,]

# plot two hm
data <- list(df, df1)
gl <- lapply(1:length(data), function(i){
  heatmap.2(data.matrix(data[[i]]),Rowv=FALSE, Colv=FALSE,trace='none')
  grab_grob()
})
grid.newpage()
grid.arrange(grobs=gl, ncol=2, clip=TRUE)

# elementary plots
sdf <- as.data.frame(scale(df))
sdf1 <- as.data.frame(scale(df1))

heatmap.2(as.matrix(sdf),Rowv=FALSE, Colv=FALSE,trace='none')
heatmap.2(as.matrix(sdf1),Rowv=FALSE, Colv=FALSE,trace='none')

pheatmap(as.matrix(x),color = colorRampPalette(c("navy", "white", "firebrick3"))(50),cluster_rows = FALSE,cluster_cols = FALSE)
pheatmap(as.matrix(sdf),color = colorRampPalette(c("navy", "white", "firebrick3"))(50),cluster_rows = FALSE,cluster_cols = FALSE,annotation_names_row = TRUE)



library(gridGraphics)
library(grid)

grab_grob <- function(){
  grid.echo()
  grid.grab()
}

library(gplots)
library(gridExtra)

