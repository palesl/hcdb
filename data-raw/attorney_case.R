## code to prepare `Special Leave 2003-2018 [PAT THIS ONE].xlsx` dataset goes here
# input from shared folder and storage in Raw ####
library(readxl)


# save a copy of the excel file in data-raw

file.copy(from = "/Users/u1040068/Dropbox/High Court Project/Data Files/CURRENT DATASETS/attorney_bio_data_1995_2019.xlsx",
          to="data-raw/attorney_bio_data_1995_2019.xlsx",
          overwrite=T)

# input from shared folder and storage in Raw ####
attorney_case <- readxl::read_excel("data-raw/attorney_bio_data_1995_2019.xlsx")


#cleaning  data ####

# no cleaning for this early version

library(dplyr)
library(labelled)


# saving ####

attorney_case<-as_tibble(attorney_case)

#save the processed data in raw data folder
save(attorney_case, file="data-raw/attorney_case.rda")

# save a copy of the raw data for the package
usethis::use_data(attorney_case, overwrite = TRUE)

