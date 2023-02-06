## code to prepare `justice_bio_November_7_2021` dataset goes here
# input from shared folder and storage in Raw ####
library(readxl)
justice_bio <- read_excel("~/Dropbox/High Court Project/Data Files/CURRENT DATASETS/justice_bio_November_7_2021.xlsx",
                                          skip = 2)

# save a copy of the excel file in data-raw

file.copy(from="~/Dropbox/High Court Project/Data Files/CURRENT DATASETS/justice_bio_November_7_2021.xlsx",
          to="data-raw/justice_bio_November_7_2021.xlsx",
          overwrite = T)

#cleaning justice decision data ####


#adding names...
library(dplyr)

justiceID<- 55:33
justiceName<-c('Steward', 'JGleeson','Edelman',
               'Gordon','Nettle', 'Keane',
               'Gageler','Bell','French',
               'Kiefel','Crennan','Heydon',
               'MGleeson','Callinan','Hayne',
               'Kirby','Gummow','McHugh',
               'Gaudron','Toohey','Dawson',
               'Deane','Brennan')

names<-bind_cols(justiceID=justiceID,
                 justiceName=justiceName)

justice_bio<-names|>left_join(justice_bio)


#cleaning missing value
justice_bio[justice_bio==999]<-NA

# dates...
names(justice_bio)[grep("date",names(justice_bio), ignore.case = T)]


justice_bio<-justice_bio|>
  mutate(dateNom = lubridate::ymd(dateNom),
         dateSwearingIn = lubridate::ymd(dateSwearingIn),
         promotionDate = lubridate::ymd(promotionDate),
         dateSwearingInPromotion = lubridate::ymd(dateSwearingInPromotion),
         nomDateBirth = lubridate::ymd(nomDateBirth),
         dateDeath = lubridate::ymd(dateDeath),
         dateDeparture = lubridate::ymd(dateDeparture),
         priorJudicialService1StartDate = lubridate::ymd(priorJudicialService1StartDate),
         priorJudicialService1EndDate = lubridate::ymd(priorJudicialService1EndDate),
         priorJudicialService2StartDate = lubridate::ymd(priorJudicialService2StartDate),
         priorJudicialService2EndDate = lubridate::ymd(priorJudicialService2EndDate),
         priorJudicialService3StartDate = lubridate::ymd(priorJudicialService3StartDate),
         priorJudicialService3EndDate = lubridate::ymd(priorJudicialService3EndDate))

justice_bio<-justice_bio |> dplyr::rename(justice=justiceID)
# saving ####

#save the processed data in raw data folder
save(justice_bio, file="data-raw/justice_bio.rda")

# save a copy of the raw data for the package
usethis::use_data(justice_bio, overwrite = TRUE)
