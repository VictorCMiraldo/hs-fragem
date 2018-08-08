library(ggplot2)
library(readr)
df <- data.frame(read_csv("~/Downloads/testfra/hs-fragem/experiments/file.csv", 
                 col_names = FALSE))
library("dplyr") # or library("tidyverse")

df <- df %>% mutate(id = row_number())

dfcut <- df[,1:14]

ggplot(df, aes(variable, value, group=factor(rowid))) + geom_line(aes(color=factor(rowid)))
