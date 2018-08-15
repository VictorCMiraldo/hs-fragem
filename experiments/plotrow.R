library(ggplot2)
library(readr)
library(zoo)
library(data.table)

# df <- data.frame(read_csv("~/Downloads/testfra/hs-fragem/experiments/file.csv",
df <- data.frame(read_csv("~/Dropbox/111Projects/hs-fragem/experiments/file.csv",
                 col_names = FALSE))

df <- `folkrnn-g2.csv`
dft <- transpose(df)[,1:80]

z <- zoo(dft)

# classic graphics
plot(z, type = "o") # multiple panels
plot(z, screens = 1, col = 1:16) # one panel

library(lattice)
xyplot(z)
xyplot(z, screens = 1, col = 1:16)

library(ggplot2)
autoplot(z)
autoplot(z, facets = NULL) + aes(linetype = NULL)
