library(readr)

filenames <- list.files(path="~/Dropbox/111Projects/hs-fragem/experiments/csvs/jsbach",
                        pattern="c+.*csv")

for(i in filenames){
  filepath <- file.path("~/Dropbox/111Projects/hs-fragem/experiments/csvs/jsbach",paste(i,sep=""))
  assign(i,  read_csv(filepath,col_names = FALSE))
}
