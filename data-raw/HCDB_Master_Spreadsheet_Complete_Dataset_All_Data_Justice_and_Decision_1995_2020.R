## code to prepare `HCDB_Master_Spreadsheet_Complete_Dataset_All_Data_Justice_and_Decision_1995_2021` dataset goes here


# input from shared folder and storage in Raw ####
justice_decision <- readxl::read_excel("~/Dropbox/High Court Project/Data Files/CURRENT DATASETS/HCDB Master Spreadsheet, Complete Dataset (All Data, Justice and Decision), 1995-2020.xlsx")


# save a copy of the excel file in data-raw
file.copy(from = "~/Dropbox/High Court Project/Data Files/CURRENT DATASETS/HCDB Master Spreadsheet, Complete Dataset (All Data, Justice and Decision), 1995-2020.xlsx",
          to="data-raw/HCDB Master Spreadsheet, Complete Dataset (All Data, Justice and Decision), 1995-2020.xlsx",
          overwrite=T)

#cleaning justice decision data ####

library(dplyr)


#removing empty spaces
justice_decision<-justice_decision[!is.na(justice_decision$HCDBcaseId),]


# ensuring justice number is a numeric variable

justice_decision$justice<- as.numeric(justice_decision$justice)


# new variable for unanimous and non-unanimous cases

justice_decision$unan<-0
justice_decision$unan[justice_decision$minVotes<1]<-1

# New string variable for chief justice

justice_decision$ChiefJustice[justice_decision$chiefDecision==10]<- 'Brennan'
justice_decision$ChiefJustice[justice_decision$chiefDecision==11]<- 'Gleeson'
justice_decision$ChiefJustice[justice_decision$chiefDecision==12]<- 'French'
justice_decision$ChiefJustice[justice_decision$chiefDecision==13]<- 'Keifel'


#Cleaning case outcomes such that missing and undetermined outcomes are set to NA; setting 0=conservative & 1=liberal
justice_decision$direction[justice_decision$direction==999]<-NA
justice_decision$direction[justice_decision$direction==3]<-NA
justice_decision$direction<- justice_decision$direction-1

#Setting 999 cases (cases with no argumentation) to 0 days
justice_decision$totalTimeArgumentDays[justice_decision$totalTimeArgumentDays==999]<-0

#cleaning data for lower court dissent to 0=no dissent; 1=dissent
justice_decision$lcDissent[justice_decision$lcDissent==999]<-NA
justice_decision$lcDissent[justice_decision$lcDissent==0]<-1
justice_decision$lcDissent<- justice_decision$lcDissent-1


#cleaning data for lower court disposition direction
justice_decision$lcDispositionDirection[justice_decision$lcDispositionDirection==999]<-NA
justice_decision$lcDispositionDirection[justice_decision$lcDispositionDirection==3]<-NA
justice_decision$lcDispositionDirection<- justice_decision$lcDispositionDirection-1
justice_decision$lcDispositionDirection[justice_decision$lcDispositionDirection==0]<- 'conservative'
justice_decision$lcDispositionDirection[justice_decision$lcDispositionDirection==1]<- 'liberal'


#removing known errors with classification of issue sub areas
justice_decision$primaryIssueSubArea[justice_decision$primaryIssueSubArea==10002]<-1002
justice_decision$primaryIssueSubArea[justice_decision$primaryIssueSubArea==105]<-104


#grouping cases into issue areas
justice_decision$Area<-NA
justice_decision$Area[justice_decision$primaryIssueSubArea==101|
                  justice_decision$primaryIssueSubArea==102|
                  justice_decision$primaryIssueSubArea==103|
                  justice_decision$primaryIssueSubArea==104]<-'Common Law'

justice_decision$Area[justice_decision$primaryIssueSubArea==201|
                  justice_decision$primaryIssueSubArea==202|
                  justice_decision$primaryIssueSubArea==301|
                  justice_decision$primaryIssueSubArea==302]<-'Public and Constitutional Law'

justice_decision$Area[justice_decision$primaryIssueSubArea==401|
                  justice_decision$primaryIssueSubArea==402|
                  justice_decision$primaryIssueSubArea==403|
                  justice_decision$primaryIssueSubArea==404|
                  justice_decision$primaryIssueSubArea==405|
                  justice_decision$primaryIssueSubArea==1002|
                  justice_decision$primaryIssueSubArea==1003|
                  justice_decision$primaryIssueSubArea==1004|
                  justice_decision$primaryIssueSubArea==1005]<-'Civil Rights and Vulnerable Persons'


justice_decision$Area[justice_decision$primaryIssueSubArea==501|
                  justice_decision$primaryIssueSubArea==502|
                  justice_decision$primaryIssueSubArea==503|
                  justice_decision$primaryIssueSubArea==504]<-'Criminal Law and Procedure'

justice_decision$Area[justice_decision$primaryIssueSubArea==601|
                  justice_decision$primaryIssueSubArea==602|
                  justice_decision$primaryIssueSubArea==603|
                  justice_decision$primaryIssueSubArea==604|
                  justice_decision$primaryIssueSubArea==605|
                  justice_decision$primaryIssueSubArea==606|
                  justice_decision$primaryIssueSubArea==607|
                  justice_decision$primaryIssueSubArea==608|
                  justice_decision$primaryIssueSubArea==701]<-'Economic Relations'

justice_decision$Area[justice_decision$primaryIssueSubArea==901|
                  justice_decision$primaryIssueSubArea==902|
                  justice_decision$primaryIssueSubArea==903|
                  justice_decision$primaryIssueSubArea==904|
                  justice_decision$primaryIssueSubArea==905|
                  justice_decision$primaryIssueSubArea==1101]<-'Procedure and Ethics'

justice_decision$Area[justice_decision$primaryIssueSubArea==801|
                  justice_decision$primaryIssueSubArea==1001]<-'International and Maritime Law'


# Assinging string values to primary issue sub areas

justice_decision$primaryIssueSubArea[justice_decision$primaryIssueSubArea==101] <- 'Tort'
justice_decision$primaryIssueSubArea[justice_decision$primaryIssueSubArea==102] <- 'Contract'
justice_decision$primaryIssueSubArea[justice_decision$primaryIssueSubArea==103] <- 'Equity'
justice_decision$primaryIssueSubArea[justice_decision$primaryIssueSubArea==104] <- 'Trusts'
justice_decision$primaryIssueSubArea[justice_decision$primaryIssueSubArea==201] <- 'Constitutional law (Federal)'
justice_decision$primaryIssueSubArea[justice_decision$primaryIssueSubArea==202] <- 'Administrative law (Federal)'
justice_decision$primaryIssueSubArea[justice_decision$primaryIssueSubArea==301] <- 'Constitutional law (State)'
justice_decision$primaryIssueSubArea[justice_decision$primaryIssueSubArea==302] <- 'Administrative law (State)'
justice_decision$primaryIssueSubArea[justice_decision$primaryIssueSubArea==401] <- 'Statutory rights'
justice_decision$primaryIssueSubArea[justice_decision$primaryIssueSubArea==402] <- 'State bills of rights'
justice_decision$primaryIssueSubArea[justice_decision$primaryIssueSubArea==403] <- 'Common law rights'
justice_decision$primaryIssueSubArea[justice_decision$primaryIssueSubArea==404] <- 'Indigenous rights (including native title)'
justice_decision$primaryIssueSubArea[justice_decision$primaryIssueSubArea==405] <- 'Refugees'
justice_decision$primaryIssueSubArea[justice_decision$primaryIssueSubArea==501] <- 'Federal criminal law'
justice_decision$primaryIssueSubArea[justice_decision$primaryIssueSubArea==502] <- 'Federal criminal procedure'
justice_decision$primaryIssueSubArea[justice_decision$primaryIssueSubArea==503] <- 'State criminal law'
justice_decision$primaryIssueSubArea[justice_decision$primaryIssueSubArea==504] <- 'State criminal procedure'
justice_decision$primaryIssueSubArea[justice_decision$primaryIssueSubArea==601] <- 'Corporate and business'
justice_decision$primaryIssueSubArea[justice_decision$primaryIssueSubArea==602] <- 'Bankruptcy and insolvency'
justice_decision$primaryIssueSubArea[justice_decision$primaryIssueSubArea==603] <- 'Property'
justice_decision$primaryIssueSubArea[justice_decision$primaryIssueSubArea==604] <- 'Intellectual property'
justice_decision$primaryIssueSubArea[justice_decision$primaryIssueSubArea==605] <- 'Consumer and competition'
justice_decision$primaryIssueSubArea[justice_decision$primaryIssueSubArea==606] <- 'Taxation'
justice_decision$primaryIssueSubArea[justice_decision$primaryIssueSubArea==607] <- 'Banking and Finance'
justice_decision$primaryIssueSubArea[justice_decision$primaryIssueSubArea==608] <- 'Economic Relations (Succession)'
justice_decision$primaryIssueSubArea[justice_decision$primaryIssueSubArea==701] <- 'Employment and industrial relations'
justice_decision$primaryIssueSubArea[justice_decision$primaryIssueSubArea==801] <- 'Admiralty and maritime'
justice_decision$primaryIssueSubArea[justice_decision$primaryIssueSubArea==901] <- 'Civil procedure/litigation'
justice_decision$primaryIssueSubArea[justice_decision$primaryIssueSubArea==902] <- 'Evidence'
justice_decision$primaryIssueSubArea[justice_decision$primaryIssueSubArea==903] <- 'Statutory interpretation (Acts Interpretation Act)'
justice_decision$primaryIssueSubArea[justice_decision$primaryIssueSubArea==904] <- 'Legal profession (ethics)'
justice_decision$primaryIssueSubArea[justice_decision$primaryIssueSubArea==905] <- 'Inherent power of the Court'
justice_decision$primaryIssueSubArea[justice_decision$primaryIssueSubArea==1001] <- 'International law'
justice_decision$primaryIssueSubArea[justice_decision$primaryIssueSubArea==1002] <- 'Family law'
justice_decision$primaryIssueSubArea[justice_decision$primaryIssueSubArea==1003] <- 'Migration (non-refugee)'
justice_decision$primaryIssueSubArea[justice_decision$primaryIssueSubArea==1004] <- 'Environmental law'
justice_decision$primaryIssueSubArea[justice_decision$primaryIssueSubArea==1005] <- 'Vulnerable persons (e.g. child protection, disabled persons etc)'
justice_decision$primaryIssueSubArea[justice_decision$primaryIssueSubArea==1101] <- 'Costs'



#adding a variable with the number of state government parties in each case

justice_decision$numStateGovParty<- justice_decision$numStateGovResp + justice_decision$numStateGovAppellant

#adding a variable with the number of federal government parties in each case

justice_decision$numFedGovParty<- justice_decision$numFedGovResp + justice_decision$numFedGovAppellant


#adding a variable indicating whether or not the judge is chief justice


justice_decision$Intervener<- justice_decision$intervener-1


#tagging chief justice

justice_decision$IsChiefJustice<-0
justice_decision$IsChiefJustice[justice_decision$chiefDecision==10&justice_decision$justice==33]<-1
justice_decision$IsChiefJustice[justice_decision$chiefDecision==11&justice_decision$justice==43]<-1
justice_decision$IsChiefJustice[justice_decision$chiefDecision==12&justice_decision$justice==47]<-1
justice_decision$IsChiefJustice[justice_decision$chiefDecision==13&justice_decision$justice==46]<-1

justice_decision<-dplyr::as_tibble(justice_decision)

# saving ####

#save the processed data in raw data folder
save(justice_decision, file="data-raw/justice_decision.rda")

# save a copy of the raw data for the package
usethis::use_data(justice_decision, overwrite = TRUE)
