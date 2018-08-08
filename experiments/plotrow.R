library(ggplot2)
library(readr)
library(zoo)
library(data.table)

df <- data.frame(read_csv("~/Downloads/testfra/hs-fragem/experiments/file.csv", 
                 col_names = FALSE))


dft <- transpose(df)

z <- zoo(dft)

# classic graphics
plot(z) # multiple panels
plot(z, screens = 1, col = 1:16) # one panel

library(lattice)
xyplot(z)
xyplot(z, screens = 1, col = 1:16)

library(ggplot2)
autoplot(z)
autoplot(z, facets = NULL) + aes(linetype = NULL)
