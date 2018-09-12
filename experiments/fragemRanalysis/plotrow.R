library(ggplot2)
library(readr)
library(zoo)
library(data.table)

# df <- data.frame(read_csv("~/Downloads/testfra/hs-fragem/experiments/file.csv",
df <- data.frame(read_csv("~/Dropbox/111Projects/hs-fragem/experiments/file.csv",
                 col_names = FALSE))

df <- `ij221-g2.csv`
df <- `iji2a2-22224-g2.csv`
df <- `ij224-g2.csv`
df <- `hanon4-s1.csv`
df <- `folkrnn-g2.csv`
df[is.na(df)] <- -100
df <- df[df$X2!=-100 & df[,ncol(df)]!=-100,]
dft <- transpose(df)

z <- zoo(dft)

# classic graphics
plot(z[,1:10], type = "o") # multiple panels
plot(z, screens = 1, col = 1:16) # one panel

library(lattice)
xyplot(z)
xyplot(z, screens = 1, col = 1:16)

library(ggplot2)
autoplot(z)
autoplot(z, facets = NULL) + aes(linetype = NULL)
