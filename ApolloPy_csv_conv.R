# ApolloPy Analysis
# Conversion from pickle (pkl) to csv

# clear workspace
rm(list = ls())

# dir containing 'dps' data tables (pickle format)
home_dir <- '/dir to files/'
setwd(home_dir)

# read pickle files and convert to csv
library(reticulate)
pd <- import("pandas")

# list pkl files
setwd(paste0(home_dir, "data_pkl/"))
files <- list.files(path = ".")

# read all files
pickle_data <- lapply(files, pd$read_pickle)

# string splicing helper functions
str_ind_right <- function(x, n) {
  # returns str from index n up to end
  substr(x, nchar(x)-n+1, nchar(x))
}

str_ind_right_excl <- function(x, n) {
  # returns str from index 1 up to last index - n
  substr(x, 1, nchar(x)-n)
}

# save each dataset with its original name in csv format in folder '../data_csv'
for (i in 1:length(files)) {
  
  tmp_data <- pickle_data[[i]]
  tmp_name <- str_ind_right_excl(files[i], 4)
  write.csv(tmp_data, paste0("../data_csv/", tmp_name, ".csv"), row.names = F)
  
}

