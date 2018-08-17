library(readr)

filenames <- list.files(path="~/Dropbox/111Projects/hs-fragem/experiments/csvs/iji2a2-22224/",
                        pattern="*csv")

for(i in filenames){
  filepath <- file.path("~/Dropbox/111Projects/hs-fragem/experiments/csvs/iji2a2-22224/",paste(i,sep=""))
  assign(i,  read_csv(filepath,col_names = FALSE))
}
