df <- `X01prelud_ima`
df <- `X01prelud_beat`

df <- df[df$X1==4,]
dft <- transpose(df)
dftdel <- dft[4:45,]
z <- zoo(dftdel)

autoplot(z, facets = NULL) + aes(linetype = NULL) + geom_vline(xintercept = 1:41)
# + theme(legend.position = "none") 

