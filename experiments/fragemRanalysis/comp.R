

jsym[is.na(jsym)] <-0
jsymn <- jsym[,2:150]
jsymn <- jsymn[,apply(jsymn ,2, var,na.rm=TRUE) != 0]

classdata_g4[is.na(classdata_g4)] <-0
fn <- classdata_g4[,2:47]

filetime <- sapply(strsplit(as.character(jsym$X1), "[/]"), "[[", 9)
file <- sapply(strsplit(as.character(filetime), "[_]"), "[[", 1)
time <- sapply(strsplit(as.character(filetime), "[_]"), "[[", 2)

t <- data.frame(t(classdata_g4))

library(zoo)
z <- zoo(t)
plot(z)

minuet <- jsymn[1:39,]
prelud <- jsymn[40:(39+61),]
jfugue3 <- jsym[618:(618+81),]
jmid1 <- jsymn[(39+61+1):(61+107),]
jhanon1 <- jsymn[424:(425+15),]

three <- cbind(jfugue3[1:15,],t$X10[1:15],jmid1[1:15,],t$X3[1:15], jhanon1[1:15,],t$X11[1:15])

threet <- data.frame(t(three))
z <- zoo(threet)
plot(z)

preludboth <- cbind(jfugue3[1:15,],as.numeric(t$X10[2:16]))
preludt <- data.frame(t(preludboth))
z <- zoo(preludboth)
plot(z)

library(ggplot2)
autoplot(z, facets = NULL) + aes(linetype = NULL) + theme(legend.position = "none")

library(corrplot)
preludboth[is.na(preludboth)] <- 0
M <- cor(preludboth[2:15,2:151])
corvec <- M[150,]
corrplot(M)

corrplot(cor(preludboth[2:15,2:151]), diag = FALSE,
         tl.pos = "td", tl.cex = 0.5, method = "color", type = "upper")

