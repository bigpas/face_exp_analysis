list_of_packages <- c("plyr", "tidyverse", "data.table")
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

lapply(list_of_packages, require, character.only = TRUE)
