library(readr)

filenames <- list.files(path="~/Dropbox/111Projects/hs-fragem/experiments/",
                        pattern="bachselet*.csv")

for(i in filenames){
  filepath <- file.path("~/Dropbox/111Projects/hs-fragem/experiments/",paste(i,sep=""))
  assign(i,  read_csv(filepath,col_names = FALSE))
}
