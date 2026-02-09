## code to prepare `HCDB_Master_Spreadsheet_Complete_Dataset_All_Data_Justice_and_Decision_1995_2021` dataset goes here

# save a copy of the excel file in data-raw
# file.copy(from = "~/Dropbox/High Court Project/Data Files/CURRENT DATASETS/HCDB Master Spreadsheet, Complete Dataset (All Data, Justice and Decision), 1995-2020.xlsx",
#           to="data-raw/HCDB Master Spreadsheet, Complete Dataset (All Data, Justice and Decision), 1995-2020.xlsx",
#           overwrite=T)
#

# input from shared folder and storage in Raw ####
justice_decision <- readxl::read_excel("data-raw/HCDB Master Spreadsheet, Complete Dataset (All Data, Justice and Decision), 1995-2020.xlsx")




#cleaning justice decision data ####

library(dplyr)
library(labelled)

#removing empty spaces
justice_decision<-justice_decision[!is.na(justice_decision$HCDBcaseId),]

# converting 888 to 999 for now...

justice_decision$caseNumber[justice_decision$caseNumber=='888']<-'999'
justice_decision[justice_decision==888]<-999

#removing NAs
justice_decision[justice_decision==999]<-NA



# IDENTIFICATION VARIABLES	11 ####
# 1.	HCDB Case ID	12

table(justice_decision$HCDBcaseId, useNA="ifany")

# 2.	Commonwealth Law Reports Citation	13
# 3.	Australian Law Journal Report Citation	14
# 4.	Australian Law Report Citation	15
# 5.	High Court Citation	16

#correcting an error

justice_decision$hcaCite[justice_decision$hcaCite=="[1991] HCA 66"]<-"[1999] HCA 66"

# 6.	Case Number	17
# 7.	Case Name	18
# 8.	Multiple Matters	19

table(justice_decision$multipleMatters, useNA="ifany")

varMultipleMatters<-c(No=1,Yes=2)
val_labels(justice_decision$multipleMatters)<-varMultipleMatters


# 9.	Number of Multiple Matters	20

justice_decision$numMultipleMatters[justice_decision$numMultipleMatters==999]<-NA

table(justice_decision$numMultipleMatters, useNA="ifany")



# 10.	Multiple Matters HCDB Identification 1	21

table(justice_decision$multipleMatterHCDBID1, useNA="ifany")


# 11.	Multiple Matters HCDB Identification 2	21

table(justice_decision$multipleMatterHCDBID2, useNA="ifany")

# 12.	Multiple Matters HCDB Identification 3	21

table(justice_decision$multipleMatterHCDBID3, useNA="ifany")

# 13.	Multiple Matters HCDB Identification 4	21

table(justice_decision$multipleMatterHCDBID4, useNA="ifany")

# 14.	Multiple Matters HCDB Identification 5	21

table(justice_decision$multipleMatterHCDBID5, useNA="ifany")

# 15.	Multiple Matters HCDB Identification 6	21

table(justice_decision$multipleMatterHCDBID6, useNA="ifany")

# 16.	Multiple Matters HCDB Identification 7	21

table(justice_decision$multipleMatterHCDBID7, useNA="ifany")

# 17.	Multiple Matters HCDB Identification 8	21

table(justice_decision$multipleMatterHCDBID8, useNA="ifany")

# BACKGROUND VARIABLES	22 ####

# 18.	Number of Appellants/Petitioners	23

justice_decision$numAppellants[justice_decision$numAppellants==999]<-NA

table(justice_decision$numAppellants, useNA="ifany")

# 19.	Number of Appellants, Federal Government	24
justice_decision$numFedGovAppellant[justice_decision$numFedGovAppellant==999]<-NA

table(justice_decision$numFedGovAppellant, useNA="ifany")

# 20.	Number of Appellants, State Government	25
justice_decision$numStateGovAppellant[justice_decision$numStateGovAppellant==999]<-NA

table(justice_decision$numStateGovAppellant, useNA="ifany")

# 21.	Number of Appellants, Corporation	26
justice_decision$numCorpAppellant[justice_decision$numCorpAppellant==999]<-NA

table(justice_decision$numCorpAppellant, useNA="ifany")

# 22.	Number of Appellants, Non-Corporate Entity	27
justice_decision$numNonCorpOrgAppellant[justice_decision$numNonCorpOrgAppellant==999]<-NA

justice_decision$HCDBcaseId[is.na(justice_decision$numNonCorpOrgAppellant)]

table(justice_decision$numNonCorpOrgAppellant, useNA="ifany")

# 23.	Number of Appellants, Individuals	28
justice_decision$numIndividualAppellant[justice_decision$numIndividualAppellant==999]<-NA

table(justice_decision$numIndividualAppellant, useNA="ifany")

justice_decision$HCDBcaseId[is.na(justice_decision$numIndividualAppellant)]


# 24.	Appellant/Petitioner [1] to [34]	29


varParties<-c(`Bankrupt person or business, or business in reorganization` = 1,
              `Buyer/purchaser` = 2,
              `Corporation` = 3,
              `Creditor` = 4,
              `Criminal defendant, female` = 5,
              `Criminal defendant, male` = 6,
              `Criminal defendant, minority (non-white)` = 7,
              `Distributor` = 8,
              `Employer, including government where litigation in capacity as employer` = 9,
              `Federal government, including non-corporate and corporate entities, and Commonwealth companies (e.g. NBN)` = 10,
              `Federal government official` = 11,
              `Foreign nation or instrumentality` = 12,
              `Foreign non-government entity` = 13,
              `Heir or beneficiary, or person claiming to be` = 14,
              `Injured person, physical or emotional` = 15,
              `Injured person, economic (including defamed person)` = 16,
              `Insurance company` = 17,
              `International entity` = 18,
              `Investor` = 19,
              `IP rights-holder (e.g. patent holder, author, copyright holder)` = 20,
              `Journalist` = 21,
              `Lawyer` = 22,
              `Manufacturer` = 23,
              `Medical professional, including doctor, dentist etc` = 24,
              `Migrant, all classifications, including persons seeking to migrate to Australia` = 25,
              `Military personnel, including veterans` = 26,
              `Military, including army, navy, airforce` = 27,
              `Minority, female` = 28,
              `Minority, male` = 29,
              `Non-profit organization or association` = 30,
              `Parent` = 31,
              `Prisoner or inmate of penal institution` = 32,
              `Professional association` = 33,
              `Public interest group (including environmental, social rights etc)` = 34,
              `Real property developer` = 35,
              `Real property holder, corporation` = 36,
              `Real property holder, individual or collective of individuals` = 37,
              `Religious person` = 38,
              `Small business` = 39,
              `State government (including local government entities, e.g., city council)` = 40,
              `State government official (including local government officials, e.g., city councillor)` = 41,
              `Stockholder, bondholder, or shareholder` = 42,
              `Student` = 43,
              `Taxpayer` = 44,
              `Tenant or lessee` = 45,
              `Unemployed person, including applicant for unemployment benefits` = 46,
              `Unidentifiable` = 47,
              `Union` = 48,
              `Utility company` = 49,
              `Voter` = 50,
              `Vulnerable person (defined as child, disabled person (mental or physical), elderly person, indigent person, etc)` = 51,
              `Other` = 52,
              `Company director` = 53,
              `Person discriminating against another in discrimination action` = 54,
              `Spouse` = 55,
              `Individual, unable to be characterised by any other designation in party list` = 56,
              `Fiduciary (including trustee)` = 57,
              `Body corporate` = 58,
              `Guarantor` = 59,
              `Not Applicable` = 999)


justice_decision$appellant1|>unique()|>sort()
val_labels(justice_decision$appellant1)<-varParties

justice_decision$appellant2|>unique()|>sort()
val_labels(justice_decision$appellant2)<-varParties

justice_decision$appellant3|>unique()|>sort()
val_labels(justice_decision$appellant3)<-varParties

justice_decision$appellant4|>unique()|>sort()
val_labels(justice_decision$appellant4)<-varParties

justice_decision$appellant5|>unique()|>sort()
val_labels(justice_decision$appellant5)<-varParties

justice_decision$appellant6|>unique()|>sort()
val_labels(justice_decision$appellant6)<-varParties

justice_decision$appellant7|>unique()|>sort()
val_labels(justice_decision$appellant7)<-varParties

justice_decision$appellant8|>unique()|>sort()
val_labels(justice_decision$appellant8)<-varParties

justice_decision$appellant9|>unique()|>sort()
val_labels(justice_decision$appellant9)<-varParties

justice_decision$appellant10|>unique()|>sort()
val_labels(justice_decision$appellant10)<-varParties

justice_decision$appellant11|>unique()|>sort()
val_labels(justice_decision$appellant11)<-varParties

justice_decision$appellant12|>unique()|>sort()
val_labels(justice_decision$appellant12)<-varParties

justice_decision$appellant13|>unique()|>sort()
val_labels(justice_decision$appellant13)<-varParties

justice_decision$appellant14|>unique()|>sort()
val_labels(justice_decision$appellant14)<-varParties

justice_decision$appellant15|>unique()|>sort()
val_labels(justice_decision$appellant15)<-varParties

justice_decision$appellant16|>unique()|>sort()
val_labels(justice_decision$appellant16)<-varParties

justice_decision$appellant17|>unique()|>sort()
val_labels(justice_decision$appellant17)<-varParties

justice_decision$appellant18|>unique()|>sort()
val_labels(justice_decision$appellant18)<-varParties

justice_decision$appellant19|>unique()|>sort()
val_labels(justice_decision$appellant19)<-varParties

justice_decision$appellant20|>unique()|>sort()
val_labels(justice_decision$appellant20)<-varParties

justice_decision$appellant21|>unique()|>sort()
val_labels(justice_decision$appellant21)<-varParties

justice_decision$appellant22|>unique()|>sort()
val_labels(justice_decision$appellant22)<-varParties

justice_decision$appellant23|>unique()|>sort()
val_labels(justice_decision$appellant23)<-varParties

justice_decision$appellant24|>unique()|>sort()
val_labels(justice_decision$appellant24)<-varParties

justice_decision$appellant25|>unique()|>sort()
val_labels(justice_decision$appellant25)<-varParties

justice_decision$appellant26|>unique()|>sort()
val_labels(justice_decision$appellant26)<-varParties

justice_decision$appellant27|>unique()|>sort()
val_labels(justice_decision$appellant27)<-varParties

justice_decision$appellant28|>unique()|>sort()
val_labels(justice_decision$appellant28)<-varParties

justice_decision$appellant29|>unique()|>sort()
val_labels(justice_decision$appellant29)<-varParties

justice_decision$appellant30|>unique()|>sort()
val_labels(justice_decision$appellant30)<-varParties

justice_decision$appellant31|>unique()|>sort()
val_labels(justice_decision$appellant31)<-varParties

justice_decision$appellant32|>unique()|>sort()
val_labels(justice_decision$appellant32)<-varParties

justice_decision$appellant33|>unique()|>sort()
val_labels(justice_decision$appellant33)<-varParties

justice_decision$appellant34|>unique()|>sort()
val_labels(justice_decision$appellant34)<-varParties


# 25.	Appellant/Petitioner State [1] to [34]	30

case_source_state <-matrix(c(1, 'Australia (federal actor)',
                             2, 'Australian Capital Territory',
                             3, 'New South Wales',
                             4, 'Northern Territory',
                             5, 'Queensland' ,
                             6, 'South Australia',
                             7, 'Tasmania',
                             8, 'Victoria',
                             9, 'Norfolk Island',
                             10, 'Nauru',
                             11, 'Western Australia',
                             999, "Not Applicable"), byrow = T, ncol=2)|>as_data_frame()
names(case_source_state)<- c( 'codes','labels')
case_source_state$codes<-as.numeric(case_source_state$codes)
varStates <- case_source_state$codes
names(varStates)<-case_source_state$labels


justice_decision$appellant1State|>unique()|>sort()
val_labels(justice_decision$appellant1State)<-varStates

justice_decision$appellant2State|>unique()|>sort()
val_labels(justice_decision$appellant2State)<-varStates

justice_decision$appellant3State|>unique()|>sort()
val_labels(justice_decision$appellant3State)<-varStates

justice_decision$appellant4State|>unique()|>sort()
val_labels(justice_decision$appellant4State)<-varStates

justice_decision$appellant5State|>unique()|>sort()
val_labels(justice_decision$appellant5State)<-varStates

justice_decision$appellant6State|>unique()|>sort()
val_labels(justice_decision$appellant6State)<-varStates

justice_decision$appellant7State|>unique()|>sort()
val_labels(justice_decision$appellant7State)<-varStates

justice_decision$appellant8State|>unique()|>sort()
val_labels(justice_decision$appellant1State)<-varStates

justice_decision$appellant9State|>unique()|>sort()
val_labels(justice_decision$appellant9State)<-varStates

justice_decision$appellant10State|>unique()|>sort()
val_labels(justice_decision$appellant10State)<-varStates

justice_decision$appellant11State|>unique()|>sort()
val_labels(justice_decision$appellant11State)<-varStates

justice_decision$appellant12State|>unique()|>sort()
val_labels(justice_decision$appellant12State)<-varStates

justice_decision$appellant13State|>unique()|>sort()
val_labels(justice_decision$appellant13State)<-varStates

justice_decision$appellant14State|>unique()|>sort()
val_labels(justice_decision$appellant14State)<-varStates

justice_decision$appellant15State|>unique()|>sort()
val_labels(justice_decision$appellant15State)<-varStates

justice_decision$appellant16State|>unique()|>sort()
val_labels(justice_decision$appellant16State)<-varStates

justice_decision$appellant17State|>unique()|>sort()
val_labels(justice_decision$appellant17State)<-varStates

justice_decision$appellant18State|>unique()|>sort()
val_labels(justice_decision$appellant18State)<-varStates

justice_decision$appellant19State|>unique()|>sort()
val_labels(justice_decision$appellant19State)<-varStates

justice_decision$appellant20State|>unique()|>sort()
val_labels(justice_decision$appellant20State)<-varStates

justice_decision$appellant21State|>unique()|>sort()
val_labels(justice_decision$appellant21State)<-varStates

justice_decision$appellant22State|>unique()|>sort()
val_labels(justice_decision$appellant22State)<-varStates

justice_decision$appellant23State|>unique()|>sort()
val_labels(justice_decision$appellant23State)<-varStates

justice_decision$appellant24State|>unique()|>sort()
val_labels(justice_decision$appellant24State)<-varStates

justice_decision$appellant25State|>unique()|>sort()
val_labels(justice_decision$appellant25State)<-varStates

justice_decision$appellant26State|>unique()|>sort()
val_labels(justice_decision$appellant26State)<-varStates

justice_decision$appellant27State|>unique()|>sort()
val_labels(justice_decision$appellant27State)<-varStates

justice_decision$appellant28State|>unique()|>sort()
val_labels(justice_decision$appellant28State)<-varStates

justice_decision$appellant29State|>unique()|>sort()
val_labels(justice_decision$appellant29State)<-varStates

justice_decision$appellant30State|>unique()|>sort()
val_labels(justice_decision$appellant30State)<-varStates

justice_decision$appellant31State|>unique()|>sort()
val_labels(justice_decision$appellant31State)<-varStates

justice_decision$appellant32State|>unique()|>sort()
val_labels(justice_decision$appellant32State)<-varStates

justice_decision$appellant33State|>unique()|>sort()
val_labels(justice_decision$appellant33State)<-varStates

justice_decision$appellant34State|>unique()|>sort()
val_labels(justice_decision$appellant34State)<-varStates

# 26.	Number of Respondents	31
justice_decision$numRespondents[justice_decision$numRespondents==999]<-NA

table(justice_decision$numRespondents, useNA="ifany")


# 27.	Number of Respondents, Federal Government	32

justice_decision$numFedGovResp[justice_decision$numFedGovResp==999]<-NA

table(justice_decision$numFedGovResp, useNA="ifany")


# 28.	Number of Respondent, State Government	33

justice_decision$numStateGovResp[justice_decision$numStateGovResp==999]<-NA

table(justice_decision$numStateGovResp, useNA="ifany")



# 29.	Number of Respondent, Corporation	34
justice_decision$numCorpResp[justice_decision$numCorpResp==999]<-NA

table(justice_decision$numCorpResp, useNA="ifany")

# 30.	Number of Respondent, Non-Corporate Entity	35

justice_decision$numNonCorpOrgResp[justice_decision$numNonCorpOrgResp==999]<-NA

table(justice_decision$numNonCorpOrgResp, useNA="ifany")



# 31.	Number of Respondents, Individuals	36
justice_decision$numIndividualResp[justice_decision$numIndividualResp==999]<-NA

table(justice_decision$numIndividualResp, useNA="ifany")




# 32.	Respondent [1] to [49]	37


justice_decision$respondent1|>unique()|>sort()
val_labels(justice_decision$respondent1)<-varParties

justice_decision$respondent2|>unique()|>sort()
val_labels(justice_decision$respondent2)<-varParties

justice_decision$respondent3|>unique()|>sort()
val_labels(justice_decision$respondent3)<-varParties

justice_decision$respondent4|>unique()|>sort()
val_labels(justice_decision$respondent4)<-varParties

justice_decision$respondent5|>unique()|>sort()
val_labels(justice_decision$respondent5)<-varParties

justice_decision$respondent6|>unique()|>sort()
val_labels(justice_decision$respondent6)<-varParties

justice_decision$respondent7|>unique()|>sort()
val_labels(justice_decision$respondent7)<-varParties

justice_decision$respondent8|>unique()|>sort()
val_labels(justice_decision$respondent8)<-varParties

justice_decision$respondent9|>unique()|>sort()
val_labels(justice_decision$respondent9)<-varParties

justice_decision$respondent10|>unique()|>sort()
val_labels(justice_decision$respondent10)<-varParties

justice_decision$respondent11|>unique()|>sort()
val_labels(justice_decision$respondent11)<-varParties

justice_decision$respondent12|>unique()|>sort()
val_labels(justice_decision$respondent12)<-varParties

justice_decision$respondent13|>unique()|>sort()
val_labels(justice_decision$respondent13)<-varParties

justice_decision$respondent14|>unique()|>sort()
val_labels(justice_decision$respondent14)<-varParties

justice_decision$respondent15|>unique()|>sort()
val_labels(justice_decision$respondent15)<-varParties

justice_decision$respondent16|>unique()|>sort()
val_labels(justice_decision$respondent16)<-varParties

justice_decision$respondent17|>unique()|>sort()
val_labels(justice_decision$respondent17)<-varParties

justice_decision$respondent18|>unique()|>sort()
val_labels(justice_decision$respondent18)<-varParties

justice_decision$respondent19|>unique()|>sort()
val_labels(justice_decision$respondent19)<-varParties

justice_decision$respondent20|>unique()|>sort()
val_labels(justice_decision$respondent20)<-varParties

justice_decision$respondent21|>unique()|>sort()
val_labels(justice_decision$respondent21)<-varParties

justice_decision$respondent22|>unique()|>sort()
val_labels(justice_decision$respondent22)<-varParties

justice_decision$respondent23|>unique()|>sort()
val_labels(justice_decision$respondent23)<-varParties

justice_decision$respondent24|>unique()|>sort()
val_labels(justice_decision$respondent24)<-varParties

justice_decision$respondent25|>unique()|>sort()
val_labels(justice_decision$respondent25)<-varParties

justice_decision$respondent26|>unique()|>sort()
val_labels(justice_decision$respondent26)<-varParties

justice_decision$respondent27|>unique()|>sort()
val_labels(justice_decision$respondent27)<-varParties

justice_decision$respondent28|>unique()|>sort()
val_labels(justice_decision$respondent28)<-varParties

justice_decision$respondent29|>unique()|>sort()
val_labels(justice_decision$respondent29)<-varParties

justice_decision$respondent30|>unique()|>sort()
val_labels(justice_decision$respondent30)<-varParties

justice_decision$respondent31|>unique()|>sort()
val_labels(justice_decision$respondent31)<-varParties

justice_decision$respondent32|>unique()|>sort()
val_labels(justice_decision$respondent32)<-varParties

justice_decision$respondent33|>unique()|>sort()
val_labels(justice_decision$respondent33)<-varParties

justice_decision$respondent34|>unique()|>sort()
val_labels(justice_decision$respondent34)<-varParties

justice_decision$respondent35|>unique()|>sort()
val_labels(justice_decision$respondent35)<-varParties

justice_decision$respondent36|>unique()|>sort()
val_labels(justice_decision$respondent36)<-varParties

justice_decision$respondent37|>unique()|>sort()
val_labels(justice_decision$respondent37)<-varParties

justice_decision$respondent38|>unique()|>sort()
val_labels(justice_decision$respondent38)<-varParties

justice_decision$respondent39|>unique()|>sort()
val_labels(justice_decision$respondent39)<-varParties

justice_decision$respondent40|>unique()|>sort()
val_labels(justice_decision$respondent40)<-varParties

justice_decision$respondent41|>unique()|>sort()
val_labels(justice_decision$respondent41)<-varParties

justice_decision$respondent42|>unique()|>sort()
val_labels(justice_decision$respondent42)<-varParties

justice_decision$respondent43|>unique()|>sort()
val_labels(justice_decision$respondent43)<-varParties

justice_decision$respondent44|>unique()|>sort()
val_labels(justice_decision$respondent44)<-varParties

justice_decision$respondent45|>unique()|>sort()
val_labels(justice_decision$respondent45)<-varParties

justice_decision$respondent46|>unique()|>sort()
val_labels(justice_decision$respondent46)<-varParties

justice_decision$respondent47|>unique()|>sort()
val_labels(justice_decision$respondent47)<-varParties

justice_decision$respondent48|>unique()|>sort()
val_labels(justice_decision$respondent48)<-varParties

justice_decision$respondent49|>unique()|>sort()
val_labels(justice_decision$respondent49)<-varParties

# 33.	Respondent State [1] to [49]	38



justice_decision$respondent1State|>unique()|>sort()
val_labels(justice_decision$respondent1State)<-varStates

justice_decision$respondent2State|>unique()|>sort()
val_labels(justice_decision$respondent2State)<-varStates

justice_decision$respondent3State|>unique()|>sort()
val_labels(justice_decision$respondent3State)<-varStates

justice_decision$respondent4State|>unique()|>sort()
val_labels(justice_decision$respondent4State)<-varStates

justice_decision$respondent5State|>unique()|>sort()
val_labels(justice_decision$respondent5State)<-varStates

justice_decision$respondent6State|>unique()|>sort()
val_labels(justice_decision$respondent6State)<-varStates

justice_decision$respondent7State|>unique()|>sort()
val_labels(justice_decision$respondent7State)<-varStates

justice_decision$respondent8State|>unique()|>sort()
val_labels(justice_decision$respondent8State)<-varStates

justice_decision$respondent9State|>unique()|>sort()
val_labels(justice_decision$respondent9State)<-varStates

justice_decision$respondent10State|>unique()|>sort()
val_labels(justice_decision$respondent10State)<-varStates

justice_decision$respondent11State|>unique()|>sort()
val_labels(justice_decision$respondent11State)<-varStates

justice_decision$respondent12State|>unique()|>sort()
val_labels(justice_decision$respondent12State)<-varStates

justice_decision$respondent13State|>unique()|>sort()
val_labels(justice_decision$respondent13State)<-varStates

justice_decision$respondent14State|>unique()|>sort()
val_labels(justice_decision$respondent14State)<-varStates

justice_decision$respondent15State|>unique()|>sort()
val_labels(justice_decision$respondent15State)<-varStates

justice_decision$respondent16State|>unique()|>sort()
val_labels(justice_decision$respondent16State)<-varStates

justice_decision$respondent17State|>unique()|>sort()
val_labels(justice_decision$respondent17State)<-varStates

justice_decision$respondent18State|>unique()|>sort()
val_labels(justice_decision$respondent18State)<-varStates

justice_decision$respondent19State|>unique()|>sort()
val_labels(justice_decision$respondent19State)<-varStates

justice_decision$respondent20State|>unique()|>sort()
val_labels(justice_decision$respondent20State)<-varStates

justice_decision$respondent21State|>unique()|>sort()
val_labels(justice_decision$respondent21State)<-varStates

justice_decision$respondent22State|>unique()|>sort()
val_labels(justice_decision$respondent22State)<-varStates

justice_decision$respondent23State|>unique()|>sort()
val_labels(justice_decision$respondent23State)<-varStates

justice_decision$respondent24State|>unique()|>sort()
val_labels(justice_decision$respondent24State)<-varStates

justice_decision$respondent25State|>unique()|>sort()
val_labels(justice_decision$respondent25State)<-varStates

justice_decision$respondent26State|>unique()|>sort()
val_labels(justice_decision$respondent26State)<-varStates

justice_decision$respondent27State|>unique()|>sort()
val_labels(justice_decision$respondent27State)<-varStates

justice_decision$respondent28State|>unique()|>sort()
val_labels(justice_decision$respondent28State)<-varStates

justice_decision$respondent29State|>unique()|>sort()
val_labels(justice_decision$respondent29State)<-varStates

justice_decision$respondent30State|>unique()|>sort()
val_labels(justice_decision$respondent30State)<-varStates

justice_decision$respondent31State|>unique()|>sort()
val_labels(justice_decision$respondent31State)<-varStates

justice_decision$respondent32State|>unique()|>sort()
val_labels(justice_decision$respondent32State)<-varStates

justice_decision$respondent33State|>unique()|>sort()
val_labels(justice_decision$respondent33State)<-varStates

justice_decision$respondent34State|>unique()|>sort()
val_labels(justice_decision$respondent34State)<-varStates

justice_decision$respondent35State|>unique()|>sort()
val_labels(justice_decision$respondent35State)<-varStates

justice_decision$respondent36State|>unique()|>sort()
val_labels(justice_decision$respondent36State)<-varStates

justice_decision$respondent37State|>unique()|>sort()
val_labels(justice_decision$respondent37State)<-varStates

justice_decision$respondent38State|>unique()|>sort()
val_labels(justice_decision$respondent38State)<-varStates

justice_decision$respondent39State|>unique()|>sort()
val_labels(justice_decision$respondent39State)<-varStates

justice_decision$respondent40State|>unique()|>sort()
val_labels(justice_decision$respondent40State)<-varStates

justice_decision$respondent41State|>unique()|>sort()
val_labels(justice_decision$respondent41State)<-varStates

justice_decision$respondent42State|>unique()|>sort()
val_labels(justice_decision$respondent42State)<-varStates

justice_decision$respondent43State|>unique()|>sort()
val_labels(justice_decision$respondent43State)<-varStates

justice_decision$respondent44State|>unique()|>sort()
val_labels(justice_decision$respondent44State)<-varStates

justice_decision$respondent45State|>unique()|>sort()
val_labels(justice_decision$respondent45State)<-varStates

justice_decision$respondent46State|>unique()|>sort()
val_labels(justice_decision$respondent46State)<-varStates

justice_decision$respondent47State|>unique()|>sort()
val_labels(justice_decision$respondent47State)<-varStates

justice_decision$respondent48State|>unique()|>sort()
val_labels(justice_decision$respondent48State)<-varStates

justice_decision$respondent49State|>unique()|>sort()
val_labels(justice_decision$respondent49State)<-varStates


# 34.	Intervener	39

interv <- c(intervener=2,
            `no intervener`=1,
            `Not Applicable` =999)

justice_decision$intervener|>unique()|>sort()
val_labels(justice_decision$intervener)<-interv


# 35.	Number of Interveners	40

justice_decision$numIntervener|>unique()|>sort()
justice_decision$numIntervener[justice_decision$numIntervener==999]<-NA

# 36.	Intervener Identity [1] to [10]	41

justice_decision$intervener1|>unique()|>sort()
justice_decision$intervener2|>unique()|>sort()
justice_decision$intervener3|>unique()|>sort()
justice_decision$intervener4|>unique()|>sort()
justice_decision$intervener5|>unique()|>sort()
justice_decision$intervener6|>unique()|>sort()
justice_decision$intervener7|>unique()|>sort()
justice_decision$intervener8|>unique()|>sort()
justice_decision$intervener9|>unique()|>sort()
justice_decision$intervener10|>unique()|>sort()

varStatesOther <-matrix(c(1, 'Australia (federal actor)',
                             2, 'Australian Capital Territory',
                             3, 'New South Wales',
                             4, 'Northern Territory',
                             5, 'Queensland' ,
                             6, 'South Australia',
                             7, 'Tasmania',
                             8, 'Victoria',
                             9, 'Norfolk Island',
                             10, 'Nauru',
                             11, 'Western Australia',
                             12,'Other',
                             999, "Not Applicable"), byrow = T, ncol=2)|>as_data_frame()
names(varStatesOther)<- c( 'codes','labels')
varStatesOther$codes<-as.numeric(varStatesOther$codes)

varStateOther <- varStatesOther$codes
names(varStateOther)<-varStatesOther$labels

val_labels(justice_decision$intervener1)<-varStateOther
val_labels(justice_decision$intervener2)<-varStateOther
val_labels(justice_decision$intervener3)<-varStateOther
val_labels(justice_decision$intervener4)<-varStateOther
val_labels(justice_decision$intervener5)<-varStateOther
val_labels(justice_decision$intervener6)<-varStateOther
val_labels(justice_decision$intervener7)<-varStateOther
val_labels(justice_decision$intervener8)<-varStateOther
val_labels(justice_decision$intervener9)<-varStateOther
val_labels(justice_decision$intervener10)<-varStateOther


# 37.	Amicus	42

amici <- c(amici=2,
            `no amici`=1,
            `Not Applicable` =999)

justice_decision$amicus|>unique()|>sort()
val_labels(justice_decision$amicus)<-amici



# 38.	Number of Amici	43

justice_decision$numAmici|>unique()|>sort()
justice_decision$numAmici[justice_decision$numAmici==999]<-NA

# 39.	Manner in which Court takes Jurisdiction General	44

varJurisdictionGeneral <- c(`special leave`=1,
                            `appeal as of right`=2 ,
                            `original jurisdiction`=3,
                            `reference`=4 ,
                            `removal`=5,
                            `other`=6,
                            `Not Applicable`=999)



justice_decision$jurisdictionGeneral|>table()
val_labels(justice_decision$jurisdictionGeneral)<-varJurisdictionGeneral



# 40.	Manner in which Court takes Jurisdiction Specific	46

juris_s<-matrix(c(1 , 'leave (appellate)—from High Court justice exercising original jurisdiction',
                  2 , 'special leave (appellate)—from any court exercising original jurisdiction upon remittal from HCA',
                  3 , 'special leave (appellate)—from lower federal court' ,
                  4 , 'special leave (appellate)—from state or territory supreme court',
                  5 , 'special leave (appellate)—other',
                  6 , 'removal from lower court (pursuant to Sec 40, Judiciary Act)',
                  7 , 'appeal as of right',
                  8 , 'original (Constitution, section 75)',
                  9 , 'original—all matters arising under the Constitution or involving its interpretation',
                  10 , 'original—trials of indictable offenses against the laws of the Commonwealth',
                  11 , 'reference—matter referred to full court by single justice (Judiciary Act sec 18)',
                  12 , 'special—Court of Disputed Returns',
                  13 , 'other',
                  999, "Not Applicable"), byrow = T, ncol=2)|>as_data_frame()

names(juris_s)<- c( 'codes','labels')

juris_s$codes<-as.numeric(juris_s$codes)
varJurisdictionSpecific <- juris_s$codes
names(varJurisdictionSpecific)<-juris_s$labels


justice_decision$jurisdictionSpecific|>unique()|>sort()
val_labels(justice_decision$jurisdictionSpecific)<- varJurisdictionSpecific




# 41.	Administrative Action Preceding Litigation	47

varAdminAction <-c(`Australian Crime Commission` = 100,
                   `Australian Information Commissioner, and Related Delegates` = 101,
                   `Comcare, and Related Delegates` = 102,
                   `Commissioner of Patents, and Related Delegates` = 103,
                   `Commissioner of Taxation, and Related Delegates` = 104,
                   `Comptroller General of Customs, and Related Delegates` = 105,
                   `Copyright Tribunal of Australia` = 106,
                   `Human Rights and Equal Opportunity Commission` = 107,
                   `Migration Agents Registration Authority` = 108,
                   `Military Rehabilitation and Compensation Commission` = 109,
                   `Minister for Justice and Home Affairs, and Related Delegates (including Secretary) ` = 110,
                   `Minister for Treasury (Treasurer), and Related Delegates (including Secretary)` = 111,
                   `Minister for Immigration and Border Protection, and Related Delegates (including Secretary) ` = 112,
                   `Minister for Immigration and Citizenship, and Related Delegates (including Secretary) ` = 113,
                   `Minister for Immigration and Multicultural Affairs, and Related Delegates (including Secretary)/Minister for Immigration and Ethnic Affairs, and Related Delegates` = 114,
                   `Minister for Immigration and Multicultural and Indigenous Affairs, and Related Delegates (including Secretary) ` = 115,
                   `Minister for Immigration, Multicultural Affairs and Citizenship, and Related Delegates (including Secretary) ` = 116,
                   `Minister for Lands, Planning and Environment, and Related Delegates (including Secretary)` = 117,
                   `National Competition Council ` = 118,
                   `Australian Communications and Media Authority` = 119,
                   `Minister for Home Affairs, and Related Delegates (including Secretary) ` = 120,
                   `Minister for Justice, and Related Delegates (including Secretary) ` = 121,
                   `Professional Services Review` = 122,
                   `Australian Prudential Regulation Authority, and Related Delegates ` = 123,
                   `Minister for Employment and Workplace Relations, and Related Delegates (including Secretary` = 124,
                   `Companies Auditors and Liquidators Disciplinary Board` = 125,
                   `Australian Competition and Consumer Commission` = 126,
                   `Repatriation Commission` = 127,
                   `Minister for Customs and Justice, and Related Delegates (including Secretary)` = 128,
                   `Commonwealth Attorney-General` = 129,
                   `Development Allowance Authority` = 130,
                   `Commissioner of Australian Federal Police` = 131,
                   `Australian Heritage Commission` = 132,
                   `Minister for Social Security, and Related Delegates (including Secretary)` = 133,
                   `Other` = 134,
                   `ACT Government Minister, and Related Delegates ` = 200,
                   `ACT Independent Body, and Related Delegates` = 201,
                   `NSW Government Minister, and Related Delegates` = 202,
                   `NSW Independent Body, and Related Delegates` = 203,
                   `Northern Territory Government Minister, and Related Delegates` = 204,
                   `Northern Territory Independent Body, and Related Delegates` = 205,
                   `Queensland Government Minister, and Related Delegates` = 206,
                   `Queensland Independent Body, and Related Delegates` = 207,
                   `South Australia Government Minister, and Related Delegates` = 208,
                   `South Australia Independent Body, and Related Delegates` = 209,
                   `Tasmania Government Minister, and Related Delegates` = 210,
                   `Tasmania Independent Body, and Related Delegates` = 211,
                   `Victoria Government Minister, and Related Delegates` = 212,
                   `Victoria Independent Body, and Related Delegates` = 213,
                   `Western Australia Government Minister, and Related Delegates` = 214,
                   `Western Australia Independent Body, and Related Delegates` = 215,
                   `Norfolk Island Government Minister, and Related Delegates` = 216,
                   `Norfolk Island Independent Body, and Related Delegates` = 217,
                   `Nauru Government Minister, and Related Delegates` = 218,
                   `Nauru Independent Body, and Related Delegates` = 219,
                   `Not Applicable` = 999)

justice_decision$adminAction|>unique()|>sort()
val_labels(justice_decision$adminAction)<- varAdminAction

justice_decision$adminAction|>labelled::to_factor()|>unique()|>levels()

# 42.	Administrative Review Preceding Litigation 1	48

varAdminReview <- c(`Australian Competition Tribunal` = 1,
                    `Australian Conciliation and Arbitration Commission` = 2,
                    `Australian Coal Industry Tribunal` = 3,
                    `Australian Copyright Tribunal` = 4,
                    `Australian Industrial Relations Commission` = 5,
                    `Commonwealth Administrative Appeals Tribunal (pre June 30, 2015)` = 6,
                    `Commonwealth Amalgamated Administrative Appeals Tribunal (post July 1, 2015)` = 7,
                    `Commonwealth Conciliation and Arbitration Commission ` = 8,
                    `Commonwealth Defence Force Remuneration Review Tribunal` = 9,
                    `Commonwealth Defence Force Discipline Appeal Tribunal` = 10,
                    `Commonwealth Fair Work Australia` = 11,
                    `Commonwealth Fair Work Commission` = 12,
                    `Commonwealth Federal Police Disciplinary Tribunal` = 13,
                    `Commonwealth Migration Review Tribunal` = 14,
                    `Commonwealth Native Title Tribunal` = 15,
                    `Commonwealth Refugee Review Committee` = 16,
                    `Commonwealth Refugee Review Tribunal` = 17,
                    `Commonwealth Social Security Appeals Tribunal ` = 18,
                    `Commonwealth Superannuation Complaints Tribunal` = 19,
                    `Commonwealth Other` = 20,
                    `Australian Capital Territory Civil and Administrative Tribunal` = 21,
                    `Australian Capital Territory Other` = 22,
                    `New South Wales Industrial Relations Commission` = 23,
                    `New South Wales Civil and Administrative Tribunal` = 24,
                    `New South Wales Workers Compensation Commission ` = 25,
                    `New South Wales Guardianship Tribunal` = 26,
                    `New South Wales Independent Commission Against Corruption` = 27,
                    `New South Wales Administrative Decisions Tribunal` = 28,
                    `New South Wales Chief Industrial Magistrate’s Court` = 29,
                    `New South Wales Local Government Pecuniary Interest and Disciplinary Tribunal` = 30,
                    `New South Wales Mental Health Review Tribunal` = 31,
                    `New South Wales Victims Support Scheme` = 32,
                    `New South Wales Other` = 33,
                    `Queensland Industrial Relations Commission` = 34,
                    `Queensland Civil and Administrative Tribunal` = 35,
                    `Queensland Other` = 36,
                    `Victorian Civil and Administrative Tribunal` = 37,
                    `Victorian Victims of Crime Assistance Tribunal` = 38,
                    `Victoria Other` = 39,
                    `South Australian Civil and Administrative Tribunal` = 40,
                    `State Administrative Tribunal of Western Australia` = 41,
                    `South Australia Other` = 42,
                    `Nauru Refugee Status Review Tribunal` = 43,
                    `Administrative Tribunal of Western Australia` = 44,
                    `Land and Mining Tribunal of the Northern Territory` = 45,
                    `Western Australia Other` = 46,
                    `Not Applicable` = 999)

justice_decision$adminReview1|>unique()|>sort()
val_labels(justice_decision$adminReview1)<- varAdminReview

justice_decision$adminReview1|>labelled::to_factor()|>unique()|>levels()

# 43.	Administrative Review Preceding Litigation 2	49

justice_decision$adminReview2|>unique()|>sort()
val_labels(justice_decision$adminReview2)<- varAdminReview

justice_decision$adminReview2|>labelled::to_factor()|>unique()|>levels()


# 44.	Origin of Case General	50

varCaseSourceGeneral<-c(`Federal court—trial level` = 1,
                        `Federal court—appellate level` = 2,
                        `State supreme court—trial level` = 3,
                        `State supreme court—appellate level` = 4,
                        `State district court (county court)` = 5,
                        `State local court (magistrates court)` = 6,
                        `State speciality court` = 7,
                        `Not Applicable` = 999)

justice_decision$caseOriginGeneral|>unique()|>sort()
val_labels(justice_decision$caseOriginGeneral)<- varCaseSourceGeneral


# 45.	Origin of Case Specific	51

varCaseSourceSpecific <- c(`Family Court of Australia—Single Judge` = 1,
                           `Federal Magistrates Court` = 2,
                           `Federal Circuit Court of Australia` = 3,
                           `Federal Court of Australia—Single Judge` = 4,
                           `Federal Court of Australia—Full Court` = 5,
                           `Australian Industrial Court` = 6,
                           `Australia Military Court` = 7,
                           `Commonwealth Court of Conciliation and Arbitration` = 8,
                           `Commonwealth Industrial Court` = 9,
                           `Federal Court of Bankruptcy` = 10,
                           `Industrial Relations Court of Australia` = 11,
                           `Supreme Court of New South Wales—Common Law Division` = 12,
                           `Supreme Court of New South Wales—Equity Division` = 13,
                           `Supreme Court of New South Wales—Court of Appeal` = 14,
                           `Supreme Court of New South Wales—Court of Criminal Appeal` = 15,
                           `Land and Environment Court of New South Wales` = 16,
                           `District Court of New South Wales` = 17,
                           `Local Court of New South Wales` = 18,
                           `New South Wales Industrial Court` = 19,
                           `The Children’s Court of New South Wales` = 20,
                           `New South Wales Dust Diseases Tribunal` = 21,
                           `Drug Court of New South Wales` = 22,
                           `New South Wales Chief Industrial Magistrate’s Court` = 23,
                           `New South Wales Coroner’s Court` = 24,
                           `Supreme Court of Queensland—Court of Appeal` = 25,
                           `Supreme Court of Queensland—Trial Division (Criminal)` = 26,
                           `Supreme Court of Queensland—Trial Division (Civil)` = 27,
                           `Industrial Court of Queensland` = 28,
                           `Queensland Planning and Environment Court` = 29,
                           `Queensland Land Appeal Court` = 30,
                           `District Court of Queensland` = 31,
                           `Magistrates Court of Queensland` = 32,
                           `Queensland Children’s Court` = 33,
                           `Queensland Mental Health Court` = 34,
                           `Queensland Murri Court` = 35,
                           `Queensland Domestic Violence Court` = 36,
                           `Queensland Drug and Alcohol Court` = 37,
                           `Queensland Coroner’s Court` = 38,
                           `Supreme Court of Victoria—Court of Appeals` = 39,
                           `Supreme Court of Victoria—Trial Court Common Law Division` = 40,
                           `Supreme Court of Victoria—Trial Court Commercial Division` = 41,
                           `Supreme Court of Victoria—Trial Court Criminal Division` = 42,
                           `County Court of Victoria` = 43,
                           `Magistrates’ Court of Victoria` = 44,
                           `Children’s Court of Victoria` = 45,
                           `Victorian Coroner’s Court` = 46,
                           `Supreme Court of South Australia—Trial Division` = 47,
                           `Supreme Court of South Australia—Criminal Appeal Division` = 48,
                           `Supreme Court of South Australia—Civil Appeal Division` = 49,
                           `District Court of South Australia` = 50,
                           `Magistrates Court of South Australia` = 51,
                           `Environment, Resources, and Development Court of South Australia` = 52,
                           `South Australia Industrial Relations Court` = 53,
                           `Wardens Court of South Australia` = 54,
                           `Youth Court of South Australia` = 55,
                           `South Australia Coroner’s Court` = 56,
                           `Supreme Court of Western Australia—Court of Appeals` = 57,
                           `Supreme Court of Western Australia—General (Trial) Division` = 58,
                           `Family Court of Western Australia` = 59,
                           `District Court of Western Australia` = 60,
                           `Magistrates Court of Western Australia` = 61,
                           `Aboriginal Community Court of Western Australia` = 62,
                           `Children’s Court of Western Australia` = 63,
                           `Drug Court of Western Australia` = 64,
                           `Geraldton Family Violence Court, Western Australia` = 65,
                           `Western Australia Coroner’s Court` = 66,
                           `Supreme Court of Tasmania—Criminal Division` = 67,
                           `Supreme Court of Tasmania—Civil Division` = 68,
                           `Supreme Court of Tasmania—Court of Appeals` = 69,
                           `Magistrates Court of Tasmania` = 70,
                           `Tasmanian Coroner’s Court` = 71,
                           `Supreme Court of the Northern Territory—Court of Appeal` = 72,
                           `Supreme Court of the Northern Territory—Court of Criminal Appeal` = 73,
                           `Supreme Court of the Northern Territory—Civil Trial` = 74,
                           `Supreme Court of the Northern Territory—Criminal Trial` = 75,
                           `Northern Territory Local Court (Magistrate’s Court)` = 76,
                           `Coroner’s Court of the Northern Territory` = 77,
                           `Supreme Court of the Australian Capital Territory` = 78,
                           `Supreme Court of the Australian Capital Territory—Court of Appeal` = 79,
                           `Magistrates Court of the Australian Capital Territory` = 80,
                           `Coroner’s Court of the Australian Capital Territory` = 81,
                           `Supreme Court of Norfolk Island` = 82,
                           `Court of Petty Sessions for Norfolk Island` = 83,
                           `Nauru Supreme Court` = 84,
                           `Other` = 85,
                           `Family Court of Australia—Full Court` = 86,
                           `Warden’s Court of Western Australia` = 87,
                           `Nauru District Court` = 88,
                           `Not Applicable` = 999)

justice_decision$caseOriginSpecific|>unique()|>sort()
val_labels(justice_decision$caseOriginSpecific)<- varCaseSourceSpecific
justice_decision$caseOriginSpecific|>labelled::to_factor()|>unique()|>levels()



# 46.	Origin of Case State	52

justice_decision$caseOriginState|>unique()|>sort()
val_labels(justice_decision$caseOriginState)<- varStateOther
justice_decision$caseOriginState|>labelled::to_factor()|>unique()|>levels()


# 47.	Source of Case General	53

justice_decision$caseSourceGeneral|>unique()|>sort()
val_labels(justice_decision$caseSourceGeneral)<- varCaseSourceGeneral


# 48.	Source of Case Specific	54

justice_decision$caseSourceSpecific|>unique()|>sort()
val_labels(justice_decision$caseSourceSpecific)<- varCaseSourceSpecific
justice_decision$caseSourceSpecific|>labelled::to_factor()|>unique()|>levels()



# 49.	Source of Case State	55
justice_decision$caseSourceState|>unique()|>sort()
val_labels(justice_decision$caseSourceState)<- varStateOther

# 50.	Lower Court Disposition	56
varLowerCourtDisposition <- c(`Appeal/application allowed, in whole or in part, and/or order below set aside and/or varied in whole or in part, and/or matter remitted` = 1,
                              `Appeal/application dismissed` = 2,
                              `Other disposition` = 3)


justice_decision$lcDisposition|>unique()|>sort()

val_labels(justice_decision$lcDisposition)<- varLowerCourtDisposition


# 51.	Lower Court Disposition Direction	57

varDispositionDirection<-c(`conservative` = 1,
  `liberal` = 2,
  `unspecifiable` = 3,
  `Not Applicable` =999)

justice_decision$lcDispositionDirection|>unique()|>sort()
val_labels(justice_decision$lcDispositionDirection)<- varDispositionDirection


# 52.	Lower Court Dissent	58

varLowerCourtDissent <-c(No=1,
                         Yes=2,
                         `Not Applicable` =999)

justice_decision$lcDissent|>unique()|>sort()
justice_decision$lcDissent|>table()

val_labels(justice_decision$lcDissent)<- varLowerCourtDissent


# 53.	Lower Court Panel Size	59

justice_decision$lcPanelSize|>unique()|>sort()
justice_decision$lcPanelSize[justice_decision$lcPanelSize==999]<-NA

# 54.	Number of Lower Court Dissents	60

justice_decision$lcDissentNum|>unique()|>sort()
justice_decision$lcDissentNum[justice_decision$lcDissentNum==999]<-NA


# GATEKEEPING VARIABLES	61 ####

# 55.	Special Leave Method	62
varSpecialLeaveMethod<-c(`Oral hearing`=1,
                         Papers=2,
                         `not applicable`=999)

justice_decision$specialLeaveMethod|>table()
justice_decision$specialLeaveMethod|>unique()|>sort()

val_labels(justice_decision$specialLeaveMethod)<- varSpecialLeaveMethod

# 56.	Number of Justices on Preliminary Special Leave Panel	63

justice_decision$numJusticesSL|>unique()|>sort()
justice_decision$numJusticesSL|>table()

# 58.	Preliminary Special Leave Hearing	64
varPreliminarySpecialLeaveHearing<-c(Yes=1,
  `No, referred directly to full court on special leave question`=2,
  `Not Applicable`=999)

justice_decision$prelimSLHearing|>unique()|>sort()
val_labels(justice_decision$prelimSLHearing)<- varPreliminarySpecialLeaveHearing

# 59.	Preliminary Special Leave Date	65

justice_decision$prelimSLDate[justice_decision$prelimSLDate==999]<-NA
justice_decision$prelimSLDate|>unique()|>sort()



justice_decision$prelimSLDate<-lubridate::ymd(justice_decision$prelimSLDate)

# 60.	Natural Court at Preliminary Special Leave Hearing	66

varNaturalCourt<-c(`Gibbs 1 (12-Feb-81-11-May-82)` = 801,
                   `Gibbs 2 (25-Jun-82-21-Oct-86)` = 802,
                   `Gibbs 3 (30-Jul-82-5-Feb-87)` = 803,
                   `Mason 1 (6-Feb-87-13-Feb-89)` = 901,
                   `Mason 2 (6-Feb-89-20-Apr-95)` = 902,
                   `Brennan 1 (21-Apr-95-5-Feb-96)` = 1001,
                   `Brennan 2 (6-Feb-96-21 Sep-97)` = 1002,
                   `Brennan 3 (22-Sep-97-2-Feb-98)` = 1003,
                   `Brennan 4 (3-Feb-98-21-May-98)` = 1004,
                   `Gleeson 1 (22-May-98-10-Feb-03)` = 1101,
                   `Gleeson 2 (11-Feb-03-7-Nov-05)` = 1102,
                   `Gleeson 3 (8-Nov-05-2-Sep-07)` = 1103,
                   `Gleeson 4 (3-Sep-07-31-Aug-08)` = 1104,
                   `French 1 (1-Sep-08-2-Feb-09)` = 1201,
                   `French 2 (3-Feb-09-8-Oct-12)` = 1202,
                   `French 3 (9-Oct-12-02-Mar-13)` = 1203,
                   `French 4 (3-Mar-13-2-Feb-15)` = 1204,
                   `French 5 (3-Feb-15-8-Jun-15)` = 1205,
                   `French 6 (9-Jun-15-29-Jan-17)` = 1206,
                   `Kiefel 1 (30-Jan-17-30-Nov-2020)` = 1301,
                   `Kiefel 2 (01-Dec-20-28-02-21)` = 1302,
                   `Kiefel 3 (01-03-21-16-10-22)` = 1303,
                   `Kiefel 4 (17-10-22-Current)` = 1304,
                   `Not Applicable` = 999)

justice_decision$prelimSLNatCourt|>unique()|>sort()
val_labels(justice_decision$prelimSLNatCourt)<- varNaturalCourt

justice_decision$prelimSLNatCourt|>labelled::to_factor()|>unique()|>levels()


# 61.	Referral Justice Where no Preliminary Special Leave Hearing	67

varJustice <- c(`Griffith` = 01,
                `Barton` = 02,
                `O'Connor` = 03,
                `Isaacs` = 04,
                `Higgins` = 05,
                `Duffy` = 06,
                `Powers` = 07,
                `Piddington` = 08,
                `Rich` = 09,
                `Knox` = 10,
                `Starke` = 11,
                `Dixon` = 12,
                `Evatt` = 13,
                `McTiernan` = 14,
                `Latham` = 15,
                `Williams` = 16,
                `Webb` = 17,
                `Fullagar` = 18,
                `Kitto` = 19,
                `Taylor` = 20,
                `Menzies` = 21,
                `Windeyer` = 22,
                `Owen` = 23,
                `Barwick` = 24,
                `Walsh` = 25,
                `Gibbs` = 26,
                `Stephen` = 27,
                `Mason` = 28,
                `Jacobs` = 29,
                `Murphy` = 30,
                `Aickin` = 31,
                `Wilson` = 32,
                `Brennan` = 33,
                `Deane` = 34,
                `Dawson` = 35,
                `Toohey` = 36,
                `Gaudron` = 37,
                `McHugh` = 38,
                `Gummow` = 39,
                `Kirby` = 40,
                `Hayne` = 41,
                `Callinan` = 42,
                `Gleeson, M` = 43,
                `Heydon` = 44,
                `Crennan` = 45,
                `Kiefel` = 46,
                `French` = 47,
                `Bell` = 48,
                `Gageler` = 49,
                `Keane` = 50,
                `Nettle` = 51,
                `Gordon` = 52,
                `Edelman` = 53,
                `Steward` = 54,
                `Gleeson, J` = 55,
                `Jagot` = 56,
                `Not Applicable` = 999)

justice_decision$referralJustice|>unique()|>sort()
val_labels(justice_decision$referralJustice)<- varJustice
justice_decision$referralJustice|>labelled::to_factor()|>unique()|>levels()



# 62.	Preliminary Special Leave Outcome	68

varPreliminarySpecialLeaveOutcome<-
  c(`Special leave granted` = 1,
    `Special leave question referred to the full court`=2,
    `Appeal decided by preliminary special leave panel`=3,
    `Special leave refused`=4,
    `Not Applicable`=999)

justice_decision$prelimSLOutcome|>table()
val_labels(justice_decision$prelimSLOutcome)<- varPreliminarySpecialLeaveOutcome


# 63.	Justice Granting Special Leave 1	69

justice_decision$justiceSpecialLeave1|>unique()|>sort()
val_labels(justice_decision$justiceSpecialLeave1)<- varJustice

# 64.	Justice Granting Special Leave 2	70
justice_decision$justiceSpecialLeave2|>unique()|>sort()
val_labels(justice_decision$justiceSpecialLeave2)<- varJustice

# 65.	Justice Granting Special Leave 3	71
justice_decision$justiceSpecialLeave3|>unique()|>sort()
val_labels(justice_decision$justiceSpecialLeave3)<- varJustice

# 66.	Special Leave Location	72 ### big problem here with varRegistry
varRegistry<-c(`Adelaide` = 1,
               `Brisbane` = 2,
               `Canberra` = 3,
               `Darwin` = 4,
               `Hobart` = 5,
               `Melbourne` = 6,
               `Perth` = 7,
               `Sydney` = 8,
               `Not Applicable`=999)

justice_decision$specialLeaveLocation|>table()

val_labels(justice_decision$specialLeaveLocation)<- varRegistry



# 67.	Political Power on Date of Preliminary Special Leave Hearing	73

varPoliticalPower <-c(`Coalition House/Coalition Senate`=1,
         `Coalition House/Not Coalition Senate`=2,
         `Labour House/Labour Senate`=3,
         `Labour House/Not Labour Senate`=4,
         `Minority Labour House/Not Labour Senate`=5,
         `Not Applicable` = 999)


justice_decision$politicalPowerSL|>unique()|>sort()

val_labels(justice_decision$politicalPowerSL)<- varPoliticalPower

# 68.	year of Court on Date of Preliminary Special Leave Hearing	74

justice_decision$yearSL[justice_decision$yearSL==999]<-NA

justice_decision$yearSL<- as.factor(justice_decision$yearSL)

justice_decision$yearSL|>unique()|>sort()

# 68a. Term of Court on Date of Preliminary Special Leave Hearing	74


classify_financial_year <- function(date) {
  year <- lubridate::year(date)
  month <- lubridate::month(date)

  if (!is.na(month) && month >= 7) {
    paste(year, year + 1, sep = "-")
  } else if(!is.na(month)) {
    paste(year - 1, year, sep = "-")
  } else if (is.na(month)){
    NA
  }
}

justice_decision$termSL<-sapply(justice_decision$prelimSLDate, classify_financial_year)|>
  unlist()|>as.factor()

justice_decision$termSL|>unique()|>sort()

# 69.	Chief Justice Special Leave	75

varChief<-c(Griffith =1,
      Knox=2,
      Isaacs=3,
      Duffy=4,
      Latham=5,
      Dixon=6,
      Barwick=7,
      Gibbs=8,
      Mason=9,
      Brennan=10,
      Gleeson=11,
      French=12,
      Kiefel=13,
      `Not Applicable`=999)

justice_decision$chiefSL|>unique()|>sort()

val_labels(justice_decision$chiefSL)<- varChief


# 70.	Prime Minister on Date of Preliminary Special Leave Hearing	76

varPrimeMinister<- c(`Barton (01.01.1901 - 24.09.1903)`=1,
              `Deakin1 (24.09.1903 - 27.04.1904)`=2,
              `Watson (27.04.1904 - 17.08.1904)`=3,
              `Reid (18.08.1904 - 05.07.1905)`=4,
              `Deakin 2 (05.07.1905 - 13.11.1908)`=5,
              `Fisher 1 (13.11.1908 - 02.06.1909)`=6,
              `Deakin 3 (02.06.1909 - 29.04.1910)`=7,
              `Fisher 2 (29.04.1910 - 24.06.1913)`=8,
              `Cook (24.06.1913 - 17.09.1914)`=9,
              `Fisher 3 (17.09.1914 - 27.10.1915)`=10,
              `Hughes (27.10.1915 - 09.02.1923)`=11,
              `Bruce (09.02.1923 - 22.10.1929)`=12,
              `Scullin (22.10.1929 - 06.01.1932)`=13,
              `Lyons (06.01.1932 - 07.04.1939)`=14,
              `Page (07.04.1939 - 26.04.1939)`=15,
              `Menzies 1 (26.04.1939 - 29.08.194)`=16,
              `Fadden (29.08.1941 - 07.10.1941)`=17,
              `Curtin (07.10.1941 - 05.07.1945)`=18,
              `Forde (06.07.1945 - 13.07.1945)`=19,
              `Chifley (13.07.1945 - 19.12.1949)`=20,
              `Menzies 2 (19.12.1949 - 26.01.1966)`=21,
              `Holt (26.01.1966 - 19.12.1967)`=22,
              `McEwen (19.12.1967 - 10.01.1968)`=23,
              `Gorton (10.01.1968 - 10.03.1971)`=24,
              `McMahon (10.03.1971 - 05.12.1972)` =25,
              `Whitlam (05.12.1972 - 11.11.1975)`=26,
              `Fraser (11.11.1975 - 11.03.1983)`=27,
              `Hawke (11.03.1983 - 20.12.1991)`=28,
              `Keating (20.12.1991 - 11.03.1996)`=29,
              `Howard (11.03.1996 - 3.12.2007)`=30,
              `Rudd 1 (03.12.2007 - 24.06.2010)`=31,
              `Gillard (24.06.2010 - 27.06.2013)`=32,
              `Rudd 2 (27.06.2013 - 18.09.2013)`=33,
              `Abbott (18.09.2013 - 15.09.2015)`=34,
              `Turnbull (15.09.2015 - 24.08.2018)`=35,
              `Morrison (24.08.2018 - 23.05.2022)`=36,
              `Albanese (23.05.2022 - present)`=37,
              `Not Applicable` = 999)

justice_decision$pmSL|>unique()|>sort()
val_labels(justice_decision$pmSL)<- varPrimeMinister



# CHRONOLOGICAL VARIABLES	77

# 71.	Date of Decision	78

justice_decision$dateDecision[justice_decision$dateDecision==999]<-NA

justice_decision$dateDecision|>unique()|>sort()
justice_decision$dateDecision <-lubridate::ymd(justice_decision$dateDecision)


# 72.	Year of Court Decision	79

justice_decision$yearDecision|>unique()|>sort()

justice_decision$yearDecision[justice_decision$yearDecision==999]<-NA

justice_decision$yearDecision<- as.factor(justice_decision$yearDecision)

# 72a.	Term of Court Decision	79

justice_decision$termDecision <-sapply(justice_decision$dateDecision, classify_financial_year)|>
  unlist()|>as.factor()

# 73.	Natural Court Decision	80


justice_decision$naturalCourtDecision|>unique()|>sort()
val_labels(justice_decision$naturalCourtDecision)<- varNaturalCourt

# 74.	Chief Justice of Date of Decision	81

justice_decision$chiefDecision|>unique()|>sort()

val_labels(justice_decision$chiefDecision)<- varChief

# 75.	Political Power on Date of High Court Decision	82

justice_decision$politicalPowerHcDecision|>unique()|>sort()

val_labels(justice_decision$politicalPowerHcDecision)<- varPoliticalPower

# 76.	Prime Minister on Date of Decision	83

justice_decision$pmHcDecision|>unique()|>sort()
val_labels(justice_decision$pmHcDecision)<- varPrimeMinister


# 77.	Number of Justices on Panel	84

justice_decision$numberJustices|>unique()|>sort()
justice_decision$numberJustices[justice_decision$numberJustices==999]<-NA

# 78.	Date of Commencement of Oral Argument	85
justice_decision$dateArgumentBegin|>unique()|>sort()
justice_decision$dateArgumentBegin[justice_decision$dateArgumentBegin==999]<-NA
justice_decision$dateArgumentBegin <-lubridate::ymd(justice_decision$dateArgumentBegin)


# 79.	Date Oral Argument Concluded	86

justice_decision$dateArgumentConclude|>unique()|>sort()
justice_decision$dateArgumentConclude[justice_decision$dateArgumentConclude==999]<-NA
justice_decision$dateArgumentConclude <-lubridate::ymd(justice_decision$dateArgumentConclude)

# 80.	Total Time for Oral Argument Days	87
justice_decision$totalTimeArgumentDays|>unique()|>sort()
justice_decision$totalTimeArgumentDays[justice_decision$totalTimeArgumentDays==999]<-NA

# 81.	Total Time for Oral Argument Minutes	88

justice_decision$totalTimeArgumentMins|>unique()|>sort()
justice_decision$totalTimeArgumentMins[justice_decision$totalTimeArgumentMins==999]<-NA

# 82.	Year of Court Oral Argument	89

justice_decision$yearArgument|>unique()|>sort()

justice_decision$yearArgument[justice_decision$yearArgument==999]<-NA

justice_decision$yearArgument<- as.factor(justice_decision$yearArgument)


# 82a.	Term of Court Oral Argument	89

justice_decision$termArgument <-sapply(justice_decision$dateArgumentBegin, classify_financial_year)|>
  unlist()|>as.factor()

# 83.	Natural Court Oral Argument	90

justice_decision$naturalCourtArgument|>unique()|>sort()
val_labels(justice_decision$naturalCourtArgument)<- varNaturalCourt

# 84.	Chief Justice Oral Argument	91

justice_decision$chiefArgument|>unique()|>sort()

val_labels(justice_decision$chiefArgument)<- varChief

# 85.	Political Power Oral Argument	92
justice_decision$politicalPowerArgument|>unique()|>sort()

val_labels(justice_decision$politicalPowerArgument)<- varPoliticalPower

# 86.	Prime Minister on Date of Oral Argument	93
justice_decision$pmArgument|>unique()|>sort()
val_labels(justice_decision$pmArgument)<- varPrimeMinister

# 87.	Location of Oral Argument	94 ### big problem here with varRegistry

justice_decision$locationOralArgument|>table()

val_labels(justice_decision$locationOralArgument)<- varRegistry


# 88.	Commonwealth Solicitor-General Representation at Oral Argument 95

varSGRep <- c(`Yes SG appeared`=1,
              `No SG appearance`=2,
              `Not Applicable`=999)

justice_decision$cthSGRepOralArgument|>unique()|>sort()
val_labels(justice_decision$cthSGRepOralArgument)<- varSGRep

# 89.	Commonwealth Solicitor-General Party Representation at Oral Argument 1	96
varSGRepParty <- c(Appellant =1,
                   Respondent=2,
                   Intervener=3,
                   Amicus=4,
                   `Not Applicable`=999)


justice_decision$cthSGRepPartyOralArgument1|>unique()|>sort()

val_labels(justice_decision$cthSGRepPartyOralArgument1)<- varSGRepParty

# 90. cthSGIntvSupporting1 ### CHECK THIS AFTER ZOE LOOKS INTO IT

#justice_decision$cthSGIntvSupporting1|>table()



# 90.	Commonwealth Solicitor-General Party Representation at Oral Argument 2	97

justice_decision$cthSGRepPartyOralArgument2|>table()
val_labels(justice_decision$cthSGRepPartyOralArgument2)<- varSGRepParty




# SUBSTANTIVE VARIABLES	98
# 91.	Primary Issue Area	99


varIssueArea <- c(`Common Law` = 1,
                  `Public Law—Federal` = 2,
                  `Public Law—State` = 3,
                  `Civil Rights (non-constitutional)` = 4,
                  `Criminal Law and Procedure` = 5,
                  `Economic Relations` = 6,
                  `Employment and Industrial Relations` = 7,
                  `Admiralty and Maritime` = 8,
                  `Procedure and Ethics` = 9,
                  `Miscellaneous` = 10,
                  `Costs` = 11,
                  `Not Applicable` = 999)


justice_decision$primaryIssueArea|>unique()|>sort()

val_labels(justice_decision$primaryIssueArea)<- varIssueArea

# 92.	Primary Issue Sub Area	101
varIssueSubArea <- c(
  Tort = 101,
  Contract = 102,
  Equity = 103,
  Trusts = 104,
  `Constitutional law` = 201,
  `Administrative law` = 202,
  `Constitutional law (other than Aboriginal and Torres Strait Islander law)` = 301,
  `Administrative law (other than Aboriginal and Torres Strait Islander law)` = 302,
  `Statutory rights` = 401,
  `State bills of rights` = 402,
  `Common law rights` = 403,
  `Indigenous rights (including native title)` = 404,
  Refugees = 405,
  `Federal criminal law` = 501,
  `Federal criminal procedure` = 502,
  `State criminal law` = 503,
  `State criminal procedure` = 504,
  `Corporate and business law` = 601,
  `Bankruptcy and insolvency` = 602,
  Property = 603,
  `Intellectual property` = 604,
  `Consumer and competition law` = 605,
  Taxation = 606,
  `Banking and finance` = 607,
  `Succession (wills and estates)` = 608,
  `Employment and industrial relations` = 701,
  `Admiralty and maritime` = 801,
  `Civil procedure/litigation` = 901,
  Evidence = 902,
  `Statutory interpretation (Acts Interpretation Act)` = 903,
  `Legal profession (ethics)` = 904,
  `Inherent power of the Court` = 905,
  `International law` = 1001,
  `Family law` = 1002,
  `Migration (non-refugee)` = 1003,
  `Environmental law` = 1004,
  `Vulnerable persons (e.g. child protection, disabled persons etc)` = 1005,
  Costs = 1101,
  `Not Applicable` = 999)

justice_decision$primaryIssueSubArea|>unique()|>sort()

val_labels(justice_decision$primaryIssueSubArea)<- varIssueSubArea
justice_decision$primaryIssueSubArea|>labelled::to_factor()|>unique()|>levels()


# 93.	Primary Issue	102

varIssue<- c(`Common Law—Tort—Negligence` = 10100,
             `Common Law—Tort—Professional malpractice` = 10101,
             `Common Law—Tort—Breach of public and statutory duties ` = 10102,
             `Common Law—Tort—Misrepresentation (including defamation)` = 10103,
             `Common Law—Tort—Trespass against person ` = 10104,
             `Common Law—Tort—Trespass against property` = 10105,
             `Common Law—Tort—Occupation or possession of land` = 10106,
             `Common Law—Tort—Intentional damage to economic interest` = 10107,
             `Common Law—Tort—Interference with employment and family relations` = 10108,
             `Common Law—Tort—Limitation of actions` = 10109,
             `Common Law—Tort—Defenses to tort liability` = 10110,
             `Common Law—Tort—Remedies` = 10111,
             `Common Law—Tort—Responsibility for liability (vicarious liability etc)` = 10112,
             `Common Law—Tort—Other` = 10113,
             `Common Law—Contract—Formation ` = 10200,
             `Common Law—Contract—Scope and content` = 10201,
             `Common Law—Contract—Avoidance` = 10202,
             `Common Law—Contract—Performance and termination` = 10203,
             `Common Law—Contract—Remedies` = 10204,
             `Common Law—Contract—Other ` = 10205,
             `Common Law—Equity—Fraud, undue influence, and breach of confidence` = 10300,
             `Common Law—Equity—Estoppel` = 10301,
             `Common Law—Equity—Fiduciary law` = 10302,
             `Common Law—Equity—Other equitable relief (including set-off, contribution etc)` = 10303,
             `Common Law—Trusts—General` = 10400,
             `Public Law—Federal constitutional law—Executive power—general ` = 20100,
             `Public Law—Federal constitutional law—Executive power—prerogative powers` = 20101,
             `Public Law—Federal constitutional law—Executive power—nationhood power ` = 20102,
             `Public Law—Federal constitutional law—Legislative power` = 20103,
             `Public Law—Federal constitutional law—Legislative power—Trade and commerce power` = 20104,
             `Public Law—Federal constitutional law— Legislative power—Taxation power` = 20105,
             `Public Law—Federal constitutional law— Legislative power—Corporations power` = 20106,
             `Public Law—Federal constitutional law—Legislative power—Races power ` = 20107,
             `Public Law—Federal constitutional law—Legislative power—External affairs power` = 20108,
             `Public Law—Federal constitutional law—Legislative power—Appropriations and grants ` = 20109,
             `Public Law—Federal constitutional law—Legislative power—Territories power` = 20110,
             `Public Law—Federal constitutional law—Legislative power—conciliation and arbitration power` = 20111,
             `Public Law—Federal constitutional law—Legislative power—defense power ` = 20112,
             `Public Law—Federal constitutional law—Legislative power—other ` = 20113,
             `Public Law—Federal constitutional law—Federal judicial power` = 20114,
             `Public Law—Federal constitutional law—Federal judicial power—definition and scope of judicial power` = 20115,
             `Public Law—Federal constitutional law—Federal judicial power—limits on judicial power` = 20116,
             `Public Law—Federal constitutional law—Federal judicial power—other` = 20117,
             `Public Law—Federal constitutional law—Separation of powers` = 20118,
             `Public Law—Federal constitutional law—Federalism—general ` = 20119,
             `Public Law—Federal constitutional law—Federalism—inconsistency (section 109)` = 20120,
             `Public Law—Federal constitutional law—Federalism—Cooperative federalism` = 20121,
             `Public Law—Federal constitutional law—Constitutional rights—Section 80` = 20122,
             `Public Law—Federal constitutional law—Constitutional rights—Section 116` = 20123,
             `Public Law—Federal constitutional law—Constitutional rights—Section 117` = 20124,
             `Public Law—Federal constitutional law—Constitutional rights—Section 92` = 20125,
             `Public Law—Federal constitutional law—Constitutional rights—Section 51(xxxi), “just terms” property acquisition` = 20126,
             `Public Law—Federal constitutional law—Constitutional rights—Implied freedom of political communication ` = 20127,
             `Public Law—Federal constitutional law—Constitutional rights—Other ` = 20128,
             `Public Law—Federal constitutional law—Elections ` = 20129,
             `Public Law—Federal constitutional law—other ` = 20130,
             `Public Law—Federal administrative law—Relevant and irrelevant considerations` = 20200,
             `Public Law—Federal administrative law—Power of tribunal` = 20201,
             `Public Law—Federal administrative law—Natural justice and/or procedural fairness` = 20202,
             `Public Law—Federal administrative law—Judicial review of delegated legislation` = 20203,
             `Public Law—Federal administrative law—Judicial review of government action, including scope of power, improper exercise of power, consequences of unlawful exercise of power, limits on judicial review` = 20204,
             `Public Law—Federal administrative law—Delegated legislation other` = 20205,
             `Public Law—Federal administrative law—Freedom of information` = 20206,
             `Public Law—Federal administrative law—Merits review of administrative decisions` = 20207,
             `Public Law—Federal administrative law—Other ` = 20208,
             `Public Law—State constitutional law—legislative power` = 30100,
             `Public Law—State constitutional law—executive power` = 30101,
             `Public Law—State constitutional law—judicial power` = 30102,
             `Public Law—State administrative law—General ` = 30200,
             `Civil Rights—Statutory rights—Race discrimination` = 40100,
             `Civil Rights—Statutory rights—Sex discrimination` = 40101,
             `Civil Rights—Statutory rights—Age discrimination ` = 40102,
             `Civil Rights—Statutory rights—Religious discrimination` = 40103,
             `Civil Rights—Statutory rights—Disability discrimination` = 40104,
             `Civil Rights—Statutory rights—Other federal rights ` = 40105,
             `Civil Rights—Statutory rights—Other state rights (excluding statutory bills of rights) ` = 40106,
             `Civil Rights—State bills of rights—Australian Capital Territory` = 40200,
             `Civil Rights—State bills of rights—Victoria ` = 40201,
             `Civil Rights—State bills of rights—Other` = 40202,
             `Civil Rights—Common law rights—General ` = 40300,
             `Civil Rights—Indigenous rights—Native title` = 40400,
             `Civil Rights—Indigenous rights—Other rights ` = 40401,
             `Civil Rights—Refugees—general ` = 40500,
             `Criminal Law and Procedure—Federal criminal law—offenses against the Commonwealth  ` = 50100,
             `Criminal Law and Procedure—Federal criminal law—other offenses, individual crime` = 50101,
             `Criminal Law and Procedure—Federal criminal law—other offenses, corporate crime` = 50102,
             `Criminal Law and Procedure—Federal criminal procedure—misconduct of government actor ` = 50200,
             `Criminal Law and Procedure—Federal criminal procedure—misconduct of counsel ` = 50201,
             `Criminal Law and Procedure—Federal criminal procedure—sentencing ` = 50202,
             `Criminal Law and Procedure—Federal criminal procedure—questioning and arrest` = 50203,
             `Criminal Law and Procedure—Federal criminal procedure—pre-trial ` = 50204,
             `Criminal Law and Procedure—Federal criminal procedure—trial conduct, general` = 50205,
             `Criminal Law and Procedure—Federal criminal procedure—Other ` = 50206,
             `Criminal Law and Procedure—State criminal law—offenses against the person leading to death (e.g. murder, manslaughter)` = 50300,
             `Criminal Law and Procedure—State criminal law—other offenses against the person leading (e.g. assault, armed robbery, kidnap)` = 50301,
             `Criminal Law and Procedure—State criminal law—attempted offenses against the person` = 50302,
             `Criminal Law and Procedure—State criminal law—sexual violence (including rape and attempted rape) ` = 50303,
             `Criminal Law and Procedure—State criminal law—offenses against property` = 50304,
             `Criminal Law and Procedure—State criminal law—drug offenses ` = 50305,
             `Criminal Law and Procedure—State criminal law—morality offenses (e.g. disorderly conduct, alcohol-related offenses, gambling offenses)` = 50306,
             `Criminal Law and Procedure—State criminal law—white collar crime` = 50307,
             `Criminal Law and Procedure—State criminal law—political crimes, including corruption` = 50308,
             `Criminal Law and Procedure—State criminal law—other  ` = 50309,
             `Criminal Law and Procedure—State criminal procedure—misconduct of government actor ` = 50400,
             `Criminal Law and Procedure—State criminal procedure—misconduct of counsel` = 50401,
             `Criminal Law and Procedure—State criminal procedure—sentencing ` = 50402,
             `Criminal Law and Procedure— State criminal procedure—questioning and arrest` = 50403,
             `Criminal Law and Procedure—State criminal procedure—pre-trial ` = 50404,
             `Criminal Law and Procedure— State criminal procedure—trial conduct, general` = 50405,
             `Criminal Law and Procedure— State criminal procedure—other ` = 50406,
             `Criminal Law and Procedure— State criminal procedure—evidence` = 50407,
             `Economic Relations—Corporate and business—Actions by or against ASIC` = 60100,
             `Economic Relations—Corporate and business—Corporations law—Company formation` = 60101,
             `Economic Relations— Corporate and business—Corporations law—Shareholders rights` = 60102,
             `Economic Relations— Corporate and business—Corporations law—Directors’ duties` = 60103,
             `Economic Relations— Corporate and business—Corporations law—Shareholder litigation ` = 60104,
             `Economic Relations— Corporate and business—Corporations law—Takeovers` = 60105,
             `Economic Relations— Corporate and business—Corporations law—General corporate governance ` = 60106,
             `Economic Relations— Corporate and business—International commercial arbitration` = 60107,
             `Economic Relations— Corporate and business—Insurance` = 60108,
             `Economic Relations— Corporate and business—Commercial disputes` = 60109,
             `Economic Relations— Corporate and business—Disputes between individuals and banking and finance organizations (includes guarantor issues)` = 60110,
             `Economic Relations— Corporate and business—Creditor-debtor disputes` = 60111,
             `Economic Relations— Corporate and business—Other ` = 60112,
             `Economic Relations—Bankruptcy and Insolvency—Individual` = 60200,
             `Economic Relations—Bankruptcy and Insolvency—Corporate Insolvency` = 60201,
             `Economic Relations—Bankruptcy and Insolvency—Liquidation` = 60202,
             `Economic Relations—Bankruptcy and Insolvency—Director Liability` = 60203,
             `Economic Relations—Bankruptcy and Insolvency—Restructuring (voluntary administration, deed of company arrangement)` = 60204,
             `Economic Relations—Bankruptcy and Insolvency—Avoidance of transactions` = 60205,
             `Economic Relations—Bankruptcy and Insolvency—Other` = 60206,
             `Economic Relations—Property—Landlord-tenant disputes` = 60300,
             `Economic Relations—Property—Disputes over entitlement to land` = 60301,
             `Economic Relations—Property—Disputes between landowners` = 60302,
             `Economic Relations—Property—Other  ` = 60303,
             `Economic Relations—Intellectual property—patents ` = 60400,
             `Economic Relations—Intellectual property—exclusive rights but not patents` = 60401,
             `Economic Relations—Intellectual property—trademarks` = 60402,
             `Economic Relations—Intellectual property—copyright disputes ` = 60403,
             `Economic Relations—Intellectual property—design and other disputes ` = 60404,
             `Economic Relations—Consumer and competition law—restrictive trade practices—cartel conduct and price fixing` = 60500,
             `Economic Relations—Consumer and competition law—restrictive trade practices—anti-competitive agreements` = 60501,
             `Economic Relations—Consumer and competition law— restrictive trade practices—exclusionary provisions` = 60502,
             `Economic Relations—Consumer and competition law— restrictive trade practices—misuse of market power` = 60503,
             `Economic Relations—Consumer and competition law— restrictive trade practices—exclusive dealing` = 60504,
             `Economic Relations—Consumer and competition law— restrictive trade practices—resale price maintenance` = 60505,
             `Economic Relations—Consumer and competition law— restrictive trade practices—mergers` = 60506,
             `Economic Relations—Consumer and competition law—consumer protection—misleading and deceptive conduct` = 60507,
             `Economic Relations—Consumer and competition law— consumer protection—unconscionable conduct` = 60508,
             `Economic Relations—Consumer and competition law— consumer protection—conditions or warranties in consumer agreements ` = 60509,
             `Economic Relations—Consumer and competition law— consumer protection—other unfair practices ` = 60510,
             `Economic Relations—Consumer and competition law—access to services` = 60511,
             `Economic Relations—Consumer and competition law—violation of statutory industry code ` = 60512,
             `Economic Relations—Consumer and competition law—immunity and cooperation` = 60513,
             `Economic Relations—Consumer and competition law—remedies` = 60514,
             `Economic Relations—Consumer and competition law—restraint of trade` = 60515,
             `Economic Relations—Consumer and competition law—telecommunications ` = 60516,
             `Economic Relations—Consumer and competition law—product liability general` = 60517,
             `Economic Relations—Consumer and competition law—Other` = 60518,
             `Economic Relations—Taxation—Individual income taxation liability disputes` = 60600,
             `Economic Relations—Taxation—Corporate taxation liability disputes` = 60601,
             `Economic Relations—Taxation—Charitable taxation liability disputes` = 60602,
             `Economic Relations—Taxation—Individual taxation other` = 60603,
             `Economic Relations—Taxation—Corporate taxation other` = 60604,
             `Economic Relations—Taxation—Charitable taxation other ` = 60605,
             `Economic Relations—Taxation—Conduct of Taxation Commissioner` = 60606,
             `Economic Relations—Taxation—GST disputes` = 60607,
             `Economic Relations—Taxation—Land taxation disputes` = 60608,
             `Economic Relations—Taxation—Taxation recovery` = 60609,
             `Economic Relations—Taxation—Civil or criminal penalties` = 60610,
             `Economic Relations—Taxation—Departure Prohibition Order` = 60611,
             `Economic Relations—Taxation—Other ` = 60612,
             `Economic Relations—Succession—General` = 60700,
             `Economic Relations—Banking and finance—General` = 60800,
             `Employment and Industrial Relations—nature and scope of employment ` = 70100,
             `Employment and Industrial Relations—unlawful termination ` = 70101,
             `Employment and Industrial Relations—employer contract violations` = 70102,
             `Employment and Industrial Relations—workplace conduct ` = 70103,
             `Employment and Industrial Relations—employee entitlements` = 70104,
             `Employment and Industrial Relations—independent contractors` = 70105,
             `Employment and Industrial Relations—workers compensation ` = 70106,
             `Employment and Industrial Relations—actions between government-employer and individual(s)` = 70107,
             `Employment and Industrial Relations—actions brought by union, either for itself or on behalf of workers` = 70108,
             `Employment and Industrial Relations—conduct of unions (including boycotts and conduct of unions)` = 70109,
             `Employment and Industrial Relations—Other` = 70110,
             `Admiralty and Maritime—in rem proceedings` = 80100,
             `Admiralty and Maritime—in personam proceedings` = 80101,
             `Admiralty and Maritime—maritime insurance` = 80102,
             `Admiralty and Maritime—cargo claims` = 80103,
             `Admiralty and Maritime—other  ` = 80104,
             `Procedure and Ethics—Civil procedure/litigation—Choice of law` = 90100,
             `Procedure and Ethics—Civil procedure/litigation—Mediation, negotiation, and settlement` = 90101,
             `Procedure and Ethics—Civil procedure/litigation—Judgment enforcement` = 90102,
             `Procedure and Ethics—Civil procedure/litigation—Trial procedure` = 90103,
             `Procedure and Ethics—Civil procedure/litigation—Pre-trial procedure (e.g. directions hearings, motions, subpoenas, judgment before trial etc) ` = 90104,
             `Procedure and Ethics—Civil procedure/litigation—other` = 90105,
             `Procedure and Ethics—Evidence—Interpretation of rule or principle` = 90200,
             `Procedure and Ethics—Evidence—Admissibility of evidence (documentary and testimonial)` = 90201,
             `Procedure and Ethics—Evidence—Privilege` = 90202,
             `Procedure and Ethics—Evidence—Witness credibility` = 90203,
             `Procedure and Ethics—Evidence—Sufficiency of evidence` = 90204,
             `Procedure and Ethics—Evidence—Other` = 90205,
             `Procedure and Ethics—Statutory interpretation (Acts Interpretation Act)—General` = 90300,
             `Procedure and Ethics—Legal profession (ethics)—General` = 90400,
             `Procedure and Ethics—Inherent power of the Court—General` = 90500,
             `Miscellaneous —International law—Public international law` = 100100,
             `Miscellaneous —International law—Private international law` = 100101,
             `Miscellaneous —Family law—General ` = 100200,
             `Miscellaneous —Migration (non-refugee)—immigration ` = 100300,
             `Miscellaneous —Migration (non-refugee)—deportation` = 100301,
             `Miscellaneous —Migration (non-refugee)—citizenship ` = 100302,
             `Miscellaneous —Environmental law—General ` = 100400,
             `Miscellaneous —Vulnerable persons—General` = 100500,
             `Costs—Costs` = 110100,
             `Not Applicable`=999)


justice_decision$primaryIssue|>unique()|>sort()

val_labels(justice_decision$primaryIssue)<- varIssue

justice_decision$primaryIssue|>labelled::to_factor()|>unique()|>levels()
justice_decision$HCDBcaseId[justice_decision$primaryIssue==50207]|>unique()


# 94.	Secondary Issue Area	103

justice_decision$secondaryIssueArea|>unique()|>sort()

val_labels(justice_decision$secondaryIssueArea)<- varIssueArea
justice_decision$secondaryIssueArea|>labelled::to_factor()|>unique()|>levels()


# 95.	Secondary Issue Sub Area	104
justice_decision$secondaryIssueSubArea|>unique()|>sort()

val_labels(justice_decision$secondaryIssueSubArea)<- varIssueSubArea

justice_decision$secondaryIssueSubArea|>labelled::to_factor()|>unique()|>levels() ## CHECK TO ENSURE THIS IS CORRECT






# 96.	Secondary Issue	105
justice_decision$secondaryIssue|>unique()|>sort()

val_labels(justice_decision$secondaryIssue)<- varIssue

justice_decision$secondaryIssue|>labelled::to_factor()|>unique()|>levels() ## CHECK TO ENSURE THIS IS CORRECT

justice_decision$HCDBcaseId[justice_decision$secondaryIssue==30204]|>unique()


# 97.	Tertiary Issue Area	106
justice_decision$tertiaryIssueArea|>unique()|>sort()

val_labels(justice_decision$tertiaryIssueArea)<- varIssueArea

# 98.	Tertiary Issue Sub Area	107

justice_decision$tertiaryIssueSubArea|>unique()|>sort()

val_labels(justice_decision$tertiaryIssueSubArea)<- varIssueSubArea

justice_decision$tertiaryIssueSubArea|>labelled::to_factor()|>unique()|>levels()

# 99.	Tertiary Issue	108

justice_decision$tertiaryIssue|>unique()|>sort()

val_labels(justice_decision$tertiaryIssue)<- varIssue

justice_decision$tertiaryIssue|>labelled::to_factor()|>unique()|>levels()



# 100.	Decision Direction	109

varDecisionDirection<- c(`conservative` = 1,
                         `liberal` = 2,
                         `unspecifiable` = 3,
                         `Not applicable`= 999)

justice_decision$decisionDirection|>unique()|>sort()


val_labels(justice_decision$decisionDirection)<- varDecisionDirection


# 101.	Decision Direction Dissent	112
varDecisionDirectionDissent<-c(`dissent in opposite direction` = 0,
                               `majority and dissent in same direction` = 1,
                               `Not applicable`=999)

justice_decision$decisionDirectionDissent|>unique()|>sort()

val_labels(justice_decision$decisionDirectionDissent)<- varDecisionDirectionDissent

# 102.	Authority for Decision 1	113


varAuthorityDecision<-c(`judicial review—federal constitution` = 1,
                        `judicial review—state constitution ` = 2,
                        `High Court supervision of lower federal courts` = 3,
                        `High Court supervision of state courts` = 4,
                        `High Court supervision of administrative decision makers—federal ` = 5,
                        `High Court supervision of administrative decision makers—state` = 6,
                        `inherent power` = 7,
                        `statutory interpretation—federal` = 8,
                        `statutory interpretation—state ` = 9,
                        `regulation/administrative rule interpretation—federal` = 10,
                        `regulation/administrative rule interpretation—state` = 11,
                        `common law` = 12,
                        `Not applicable` = 999)

justice_decision$authorityDecision1|>unique()|>sort()

val_labels(justice_decision$authorityDecision1)<- varAuthorityDecision


# 103.	Authority for Decision 1 State	114

justice_decision$authorityDecision1State|>unique()|>sort()
val_labels(justice_decision$authorityDecision1State)<- varStates



# 104.	Authority for Decision 2	115

justice_decision$authorityDecision2|>unique()|>sort()
val_labels(justice_decision$authorityDecision2)<- varAuthorityDecision


# 105.	Authority for Decision 2 State	116

justice_decision$authorityDecision2State|>unique()|>sort()
val_labels(justice_decision$authorityDecision2State)<- varStates

# 106.	Authority for Decision 3	117
justice_decision$authorityDecision3|>unique()|>sort()
val_labels(justice_decision$authorityDecision3)<- varAuthorityDecision


# 107.	Authority for Decision 3 State	118

justice_decision$authorityDecision3State|>unique()|>sort()
val_labels(justice_decision$authorityDecision3State)<- varStates

# 108.	Panel Special Leave Decision	119
varSpecialLeavePanel<-c(No=1,Yes=2, `Not applicable`=999)

justice_decision$panelSLDecision|>unique()|>sort()

val_labels(justice_decision$panelSLDecision)<- varSpecialLeavePanel


# 109.	Panel Special Leave Outcome General	120

varSpecialLeaveOutcomesGeneral<-c(granted=1,denied=2, `Not applicable`=999)

justice_decision$panelSLOutcomeGen|>unique()|>sort()
val_labels(justice_decision$panelSLOutcomeGen)<- varSpecialLeaveOutcomesGeneral

# 110.	Panel Special Leave Outcome Specific	121


varSpecialLeaveOutcomeSpecific<-c(`Special leave denied (refused)` = 1,
                                  `Special leave revoked` = 2,
                                  `Special leave granted, heard instanter, appeal dismissed` = 3,
                                  `Special leave granted, heard instaner, appeal allowed` = 4,
                                  `Special leave granted, appeal allowed` = 5,
                                  `Special leave granted, appeal dismissed` = 6,
                                  `Other` = 7,
                                  `Not applicable`=999)

justice_decision$panelSLOutcomeSpecific|>unique()|>sort()
val_labels(justice_decision$panelSLOutcomeSpecific)<- varSpecialLeaveOutcomeSpecific

# !!!! waiting on zoe to check: 2005085


# 111.	Costs Decision	122

varCosts<-c(No=1,Yes=2, `Not applicable`=999)
justice_decision$costsDecision|>unique()|>sort()
val_labels(justice_decision$costsDecision)<- varCosts

# 112.	Costs Decision Related HCDBID	123

justice_decision$costsRelationHCDBID[
  justice_decision$costsRelationHCDBID==999]<-NA

justice_decision$costsRelationHCDBID[
  justice_decision$costsRelationHCDBID==555]<-NA

justice_decision$costsRelationHCDBID|>unique()|>sort()

# 113.	Legal Provision Considered by the Court 1	124
varLawArea<- c(`Federal Constitution` = 1,
               `Federal statute` = 2,
               `Federal regulation` = 3,
               `Court rules` = 4,
               `Common law (federal)` = 5,
               `State constitution` = 6,
               `State or local statute or regulation` = 7,
               `Common law (state)` = 8,
               `No legal provision` = 9,
               `Other` = 10,
               `Not applicable` = 999)

justice_decision$lawType1|>unique()|>sort()

val_labels(justice_decision$lawType1)<- varLawArea


# 114.	Legal Provision Supplement 1	125


varLegalProvisions<-c(`Constitution, Sections 7-10, Composition of Senate` = 100,
                      `Constitution, Section 24, 31-31, Composition of House of Representatives` = 101,
                      `Constitution, Sections 7-10 and 24, 30-31, Composition of Both Senate and House of Representatives` = 102,
                      `Constitution, Section 44, Disqualification of Member or Senator` = 103,
                      `Constitution, Section 51 Legislative Power` = 104,
                      `Constitution, Section 52-56, Legislative Power Other than Section 51` = 105,
                      `Constitution, Section 61, Executive Power` = 106,
                      `Constitution, Section 71, Judicial Power` = 107,
                      `Constitution, Section 73-74, Appellate Jurisdiction` = 108,
                      `Constitution, Section 75-76, Original Jurisdiction ` = 109,
                      `Constitution, Section 77, Power to Define Jurisdiction of Courts Other than High Court` = 110,
                      `Constitution, Section 80, Right to Trial by Jury` = 111,
                      `Constitution, Sections 81-83, Appropriations` = 112,
                      `Constitution, Section 90, Power over Customs, Excise, and Bounty` = 113,
                      `Constitution, Section 92, Freedom of Interstate Trade` = 114,
                      `Constitution, Section 96, Grant Power` = 115,
                      `Constitution, Section 109, Inconsistency of Laws` = 116,
                      `Constitution, Section 116, Religious Liberty` = 117,
                      `Constitution, Section 117, Rights of Residents in States` = 118,
                      `Constitution, Section 122, Government of Territories` = 119,
                      `Constitution, Section 128, Amendment of Constitution` = 120,
                      `Constitution, Implied Freedom of Political Communication` = 121,
                      `Constitution, Chapter I, Part I, General` = 122,
                      `Constitution, Chapter I, Part II, Senate` = 123,
                      `Constitution, Chapter I, Part III, House of Representatives` = 124,
                      `Constitution, Chapter I, Part IV, Both Houses` = 125,
                      `Constitution, Chapter I, Part V, Legislative Power Other than Sections 51-57` = 126,
                      `Constitution, Chapter II, Executive Government Other than Section 61` = 127,
                      `Constitution, Chapter III Judicial Power, General` = 128,
                      `Constitution, Chapter IV, Finance and Trade General ` = 129,
                      `Constitution, Chapter V, States, General` = 130,
                      `Constitution, Other` = 131,
                      `A New Tax System (Goods and Services Tax) Act 1999 (Cth)` = 200,
                      `Aboriginal and Torres Strait Islander Heritage Protection Act 1984 (Cth)` = 201,
                      `Appropriation Act (Cth), any version` = 202,
                      `Australia Act 1986 (Cth)` = 203,
                      `Australian Crime Commission Act 2002 (Cth)` = 204,
                      `Australian Securities and Investments Commission Act 2001 (Cth), or related prior version` = 205,
                      `Bankruptcy Act 1966 (Cth)` = 206,
                      `Broadcasting Services Act 1992 (Cth)` = 207,
                      `Commonwealth Electoral Act 1918 (Cth)` = 208,
                      `Copyright Act 1968 (Cth)` = 209,
                      `Corporations Act 1989 (Cth)` = 210,
                      `Corporations Act 2001 (Cth) ` = 211,
                      `Corporations Law 1989 (Cth)` = 212,
                      `Crimes Act 1914 (Cth)` = 213,
                      `Customs Act 1901 (Cth)` = 214,
                      `Designs Act 1906 (Cth)` = 215,
                      `Excise Act 1901 (Cth)` = 216,
                      `Extradition Act 1988 (Cth)` = 217,
                      `Fair Work Act 2009 (Cth)` = 218,
                      `Family Law Act 1975 (Cth)` = 219,
                      `Freedom of Information Act 1982 (Cth)` = 220,
                      `Income Tax Assessment Act 1936 (Cth) ` = 221,
                      `Income Tax Assessment Act 1997 (Cth)` = 222,
                      `Industrial Relations Act 1988 (Cth)` = 223,
                      `Insurance Act 1973 (Cth)` = 224,
                      `Insurance Contracts Act 1984 (Cth)` = 225,
                      `Lands Acquisition Act 1955 or 1989 (Cth)` = 226,
                      `Marriage Act 1961 (Cth)` = 227,
                      `Migration Act 1958 (Cth)` = 228,
                      `Native Title Act 1993 (Cth)` = 229,
                      `Navigation Act 1912 (Cth)` = 230,
                      `Patents Act 1952 or 1990 (Cth)` = 231,
                      `Safety, Rehabilitation and Compensation Act 1988 (Cth)` = 232,
                      `Social Security (Administration) Act 1999 (Cth), or other social security legislation` = 233,
                      `Telecommunications Act 1997 (Cth)` = 234,
                      `Trade Marks Act 1955 or 1995 (Cth)` = 235,
                      `Trade Practices Act 1974 (Cth)` = 236,
                      `Veterans' Entitlements Act 1986 (Cth)` = 237,
                      `Workplace Relations Act 1996 (Cth)` = 238,
                      `Workplace Relations Amendment (Work Choices) Act 2005 (Cth)` = 239,
                      `Criminal Code (Cth)` = 240,
                      `Aboriginal and Indigenous Affairs` = 250,
                      `Admiralty and Maritime` = 251,
                      `Agriculture` = 252,
                      `Aviation` = 253,
                      `Commonwealth authorities and commissions, establishment legislation` = 254,
                      `Communication, generally conceived (including advertising etc)` = 255,
                      `Customs and excise` = 256,
                      `Crimes, including legislation relating to DPP, proceeds of crime` = 257,
                      `Defense legislation, including defense force management` = 258,
                      `Migration other, including deportation and extradition` = 259,
                      `Discrimination` = 260,
                      `Economic general` = 261,
                      `Education` = 262,
                      `Elections, including member qualifications and entitlements` = 263,
                      `Family and children` = 264,
                      `Health` = 265,
                      `Insurance` = 266,
                      `Mining and land management` = 267,
                      `Taxation` = 268,
                      `Territory management and governance, including independence legislation` = 269,
                      `Workplace relations, including superannuation and OH&S` = 270,
                      `Other` = 271,
                      `Bankruptcy` = 300,
                      `Customs` = 301,
                      `Corporations` = 302,
                      `Criminal` = 303,
                      `Defense` = 304,
                      `Environment` = 305,
                      `Extradition` = 306,
                      `Family Law` = 307,
                      `Migration` = 308,
                      `National Security` = 309,
                      `Tax` = 310,
                      `Trade Practices` = 311,
                      `Other` = 312,
                      `High Court of Australia Act, all versions` = 401,
                      `High Court of Australia Rules, all versions` = 402,
                      `Federal Court of Australia Act, all versions` = 403,
                      `Federal Court of Australia Rules, all versions` = 404,
                      `Act Interpretation Act (Cth), all versions` = 405,
                      `Administrative Tribunal Act, all versions` = 406,
                      `Administrative Decisions (Judicial Review) Act, all versions` = 407,
                      `Criminal Appeal Act (Cth), all versions` = 408,
                      `Evidence Act (Cth), all versions` = 409,
                      `Federal Proceedings (Costs) Act (Cth), all versions` = 410,
                      `Foreign Judgments Act (Cth), all versions` = 411,
                      `Judicial Act (Cth), all versions` = 412,
                      `Juries Act (Cth), all versions` = 413,
                      `Nauru (High Court Appeals) Act, all versions` = 414,
                      `Territory courts legislation, all versions` = 415,
                      `Legislation relating to costs of proceedings` = 416,
                      `Legislation relating to service and execution of process` = 417,
                      `Foreign States Immunities Act (Cth), all versions` = 418,
                      `Other` = 419,
                      `High Court of Australia Act, all versions (federal)` = 40101,
                      `High Court of Australia Rules, all versions (federal)` = 40201,
                      `Federal Court of Australia Act, all versions (federal)` = 40301,
                      `Federal Court of Australia Rules, all versions (federal)` = 40401,
                      `Act Interpretation Act (Cth), all versions (federal)` = 40501,
                      `Administrative Tribunal Act, all versions (federal)` = 40601,
                      `Administrative Decisions (Judicial Review) Act, all versions (federal)` = 40701,
                      `Criminal Appeal Act (Cth), all versions (federal)` = 40801,
                      `Evidence Act (Cth), all versions (federal)` = 40901,
                      `Federal Proceedings (Costs) Act (Cth), all versions (federal)` = 41001,
                      `Foreign Judgments Act (Cth), all versions (federal)` = 41101,
                      `Judicial Act (Cth), all versions (federal)` = 41201,
                      `Juries Act (Cth), all versions (federal)` = 41301,
                      `Nauru (High Court Appeals) Act, all versions (federal)` = 41401,
                      `Territory courts legislation, all versions (federal)` = 41501,
                      `Legislation relating to costs of proceedings (federal)` = 41601,
                      `Legislation relating to service and execution of process (federal)` = 41701,
                      `Supreme Court Act, any version (ACT)` = 42002,
                      `Lower court legislation, any level, any version (ACT)` = 42102,
                      `Court rules, any level, any version (ACT)` = 42202,
                      `Legislation or rules relating to tribunal and/or administrative, any version (ACT)` = 42302,
                      `Criminal procedure legislation or rules, any version, including criminal appeals acts (ACT)` = 42402,
                      `Civil procedure legislation or rules, any version, including civil appeals acts and limitation of actions (ACT)` = 42502,
                      `Evidence legislation or rules, any version (ACT)` = 42602,
                      `Interpretation legislation or rules, any version (ACT)` = 42702,
                      `Juries legislation, any version (ACT)` = 42802,
                      `Legislation relating to costs of proceedings (ACT)` = 42902,
                      `Other relevant legislation or rules (ACT)` = 43002,
                      `Legislation relating to service and execution of process (ACT)` = 43102,
                      `Supreme Court Act, any version (NSW)` = 42003,
                      `Lower court legislation, any level, any version (NSW)` = 42103,
                      `Court rules, any level, any version (NSW)` = 42203,
                      `Legislation or rules relating to tribunal and/or administrative, any version (NSW)` = 42303,
                      `Criminal procedure legislation or rules, any version, including criminal appeals acts (NSW)` = 42403,
                      `Civil procedure legislation or rules, any version, including civil appeals acts and limitation of actions (NSW)` = 42503,
                      `Evidence legislation or rules, any version (NSW)` = 42603,
                      `Interpretation legislation or rules, any version (NSW)` = 42703,
                      `Juries legislation, any version (NSW)` = 42803,
                      `Legislation relating to costs of proceedings (NSW)` = 42903,
                      `Other relevant legislation or rules (NSW)` = 43003,
                      `Legislation relating to service and execution of process (NSW)` = 43103,
                      `Supreme Court Act, any version (NT)` = 42004,
                      `Lower court legislation, any level, any version (NT)` = 42104,
                      `Court rules, any level, any version (NT)` = 42204,
                      `Legislation or rules relating to tribunal and/or administrative, any version (NT)` = 42304,
                      `Criminal procedure legislation or rules, any version, including criminal appeals acts (NT)` = 42404,
                      `Civil procedure legislation or rules, any version, including civil appeals acts and limitation of actions (NT)` = 42504,
                      `Evidence legislation or rules, any version (NT)` = 42604,
                      `Interpretation legislation or rules, any version (NT)` = 42704,
                      `Juries legislation, any version (NT)` = 42804,
                      `Legislation relating to costs of proceedings (NT)` = 42904,
                      `Other relevant legislation or rules (NT)` = 43004,
                      `Legislation relating to service and execution of process (NT)` = 43104,
                      `Supreme Court Act, any version (Qld.)` = 42005,
                      `Lower court legislation, any level, any version (Qld.)` = 42105,
                      `Court rules, any level, any version (Qld.)` = 42205,
                      `Legislation or rules relating to tribunal and/or administrative, any version (Qld.)` = 42305,
                      `Criminal procedure legislation or rules, any version, including criminal appeals acts (Qld.)` = 42405,
                      `Civil procedure legislation or rules, any version, including civil appeals acts and limitation of actions (Qld.)` = 42505,
                      `Evidence legislation or rules, any version (Qld.)` = 42605,
                      `Interpretation legislation or rules, any version (Qld.)` = 42705,
                      `Juries legislation, any version (Qld.)` = 42805,
                      `Legislation relating to costs of proceedings (Qld.)` = 42905,
                      `Other relevant legislation or rules (Qld.)` = 43005,
                      `Legislation relating to service and execution of process (Qld.)` = 43105,
                      `Supreme Court Act, any version (SA)` = 42006,
                      `Lower court legislation, any level, any version (SA)` = 42106,
                      `Court rules, any level, any version (SA)` = 42206,
                      `Legislation or rules relating to tribunal and/or administrative, any version (SA)` = 42306,
                      `Criminal procedure legislation or rules, any version, including criminal appeals acts (SA)` = 42406,
                      `Civil procedure legislation or rules, any version, including civil appeals acts and limitation of actions (SA)` = 42506,
                      `Evidence legislation or rules, any version (SA)` = 42606,
                      `Interpretation legislation or rules, any version (SA)` = 42706,
                      `Juries legislation, any version (SA)` = 42806,
                      `Legislation relating to costs of proceedings (SA)` = 42906,
                      `Other relevant legislation or rules (SA)` = 43006,
                      `Legislation relating to service and execution of process (SA)` = 43106,
                      `Supreme Court Act, any version (Tas.)` = 42007,
                      `Lower court legislation, any level, any version (Tas.)` = 42107,
                      `Court rules, any level, any version (Tas.)` = 42207,
                      `Legislation or rules relating to tribunal and/or administrative, any version (Tas.)` = 42307,
                      `Criminal procedure legislation or rules, any version, including criminal appeals acts (Tas.)` = 42407,
                      `Civil procedure legislation or rules, any version, including civil appeals acts and limitation of actions (Tas.)` = 42507,
                      `Evidence legislation or rules, any version (Tas.)` = 42607,
                      `Interpretation legislation or rules, any version (Tas.)` = 42707,
                      `Juries legislation, any version (Tas.)` = 42807,
                      `Legislation relating to costs of proceedings (Tas.)` = 42907,
                      `Other relevant legislation or rules (Tas.)` = 43007,
                      `Legislation relating to service and execution of process (Tas.)` = 43107,
                      `Supreme Court Act, any version (Vic.)` = 42008,
                      `Lower court legislation, any level, any version (Vic.)` = 42108,
                      `Court rules, any level, any version (Vic.)` = 42208,
                      `Legislation or rules relating to tribunal and/or administrative, any version (Vic.)` = 42308,
                      `Criminal procedure legislation or rules, any version, including criminal appeals acts (Vic.)` = 42408,
                      `Civil procedure legislation or rules, any version, including civil appeals acts and limitation of actions (Vic.)` = 42508,
                      `Evidence legislation or rules, any version (Vic.)` = 42608,
                      `Interpretation legislation or rules, any version (Vic.)` = 42708,
                      `Juries legislation, any version (Vic.)` = 42808,
                      `Legislation relating to costs of proceedings (Vic.)` = 42908,
                      `Other relevant legislation or rules (Vic.)` = 43008,
                      `Legislation relating to service and execution of process (Vic.)` = 43108,
                      `Supreme Court Act, any version (Norfolk Island)` = 42009,
                      `Lower court legislation, any level, any version (Norfolk Island)` = 42109,
                      `Court rules, any level, any version (Norfolk Island)` = 42209,
                      `Legislation or rules relating to tribunal and/or administrative, any version (Norfolk Island)` = 42309,
                      `Criminal procedure legislation or rules, any version, including criminal appeals acts (Norfolk Island)` = 42409,
                      `Civil procedure legislation or rules, any version, including civil appeals acts and limitation of actions (Norfolk Island)` = 42509,
                      `Evidence legislation or rules, any version (Norfolk Island)` = 42609,
                      `Interpretation legislation or rules, any version (Norfolk Island)` = 42709,
                      `Juries legislation, any version (Norfolk Island)` = 42809,
                      `Legislation relating to costs of proceedings (Norfolk Island)` = 42909,
                      `Other relevant legislation or rules (Norfolk Island)` = 43009,
                      `Legislation relating to service and execution of process (Norfolk Island)` = 43109,
                      `Supreme Court Act, any version (Nauru)` = 42010,
                      `Lower court legislation, any level, any version (Nauru)` = 42110,
                      `Court rules, any level, any version (Nauru)` = 42210,
                      `Legislation or rules relating to tribunal and/or administrative, any version (Nauru)` = 42310,
                      `Criminal procedure legislation or rules, any version, including criminal appeals acts (Nauru)` = 42410,
                      `Civil procedure legislation or rules, any version, including civil appeals acts and limitation of actions (Nauru)` = 42510,
                      `Evidence legislation or rules, any version (Nauru)` = 42610,
                      `Interpretation legislation or rules, any version (Nauru)` = 42710,
                      `Juries legislation, any version (Nauru)` = 42810,
                      `Legislation relating to costs of proceedings (Nauru)` = 42910,
                      `Other relevant legislation or rules (Nauru)` = 43010,
                      `Legislation relating to service and execution of process (Nauru)` = 43110,
                      `Supreme Court Act, any version (WA)` = 42011,
                      `Lower court legislation, any level, any version (WA)` = 42111,
                      `Court rules, any level, any version (WA)` = 42211,
                      `Legislation or rules relating to tribunal and/or administrative, any version (WA)` = 42311,
                      `Criminal procedure legislation or rules, any version, including criminal appeals acts (WA)` = 42411,
                      `Civil procedure legislation or rules, any version, including civil appeals acts and limitation of actions (WA)` = 42511,
                      `Evidence legislation or rules, any version (WA)` = 42611,
                      `Interpretation legislation or rules, any version (WA)` = 42711,
                      `Juries legislation, any version (WA)` = 42811,
                      `Legislation relating to costs of proceedings (WA)` = 42911,
                      `Other relevant legislation or rules (WA)` = 43011,
                      `Legislation relating to service and execution of process (WA)` = 43111,
                      `Australian Capital Territory Constitution (self-government legislation)` = 602,
                      `New South Wales Constitution, any version` = 603,
                      `Northern Territory Constitution (self-government legislation)` = 604,
                      `Queensland Constitution, any version` = 605,
                      `South Australia, any version` = 606,
                      `Tasmania, any version` = 607,
                      `Victoria, any version` = 608,
                      `Norfolk Island, any version ((self-government legislation)` = 609,
                      `Nauru, any version (self-government legislation)` = 610,
                      `Western Australia, any version` = 611,
                      `Other` = 612,
                      `Aboriginal and Indigenous, legislation and regulations (ACT)` = 70002,
                      `Civil wrongs, including wrongs legislation, civil liability, defamation, accident liability, compensation (ACT)` = 70102,
                      `Community regulation general, including roads acts, community welfare, animals, police regulations (ACT)` = 70202,
                      `Corporations, including commercial activity,  business regulations (ACT)` = 70302,
                      `Criminal law legislation and regulations, including parole, witness protection, substantive crimes (ACT)` = 70402,
                      `Government legislation, including elections, FOI, anti-corruption, police regulations (ACT)` = 70502,
                      `Environment (ACT)` = 70602,
                      `Families and children, including births, deaths, and marriages legislation, succession law (ACT)` = 70702,
                      `Fishing and farming, and related legislation (ACT)` = 70802,
                      `Human rights, including discrimination legislation and legislation relating to vulnerable persons (ACT)` = 70902,
                      `Land, development and planning legislation (ACT)` = 71002,
                      `Land, government entitlement  (ACT)` = 71102,
                      `Land, non-government including conveyancing (ACT)` = 71202,
                      `Law reform (ACT)` = 71302,
                      `Legal profession (ACT)` = 71402,
                      `Local government, regulation of, legislation and regulations (ACT)` = 71502,
                      `Industry regulation (ACT)` = 71602,
                      `Morality legislation, including gambling, racing, alcohol, firearms (ACT)` = 71702,
                      `Natural resources and mining, including water, coal, gas pipelines (ACT)` = 71802,
                      `Property, general, including property act, conveyancing, leases (ACT)` = 71902,
                      `Refugees and migration (ACT)` = 72002,
                      `Taxation, including stamp duty  (ACT)` = 72102,
                      `Trade practices, including consumer legislation (ACT)` = 72202,
                      `Trusts (ACT)` = 72302,
                      `Workplace relations, broadly conceived, including superannuation (ACT)` = 72402,
                      `Other (ACT)` = 72502,
                      `Aboriginal and Indigenous, legislation and regulations (NSW)` = 70003,
                      `Civil wrongs, including wrongs legislation, civil liability, defamation, accident liability, compensation (NSW)` = 70103,
                      `Community regulation general, including roads acts, community welfare, animals, police regulations (NSW)` = 70203,
                      `Corporations, including commercial activity,  business regulations (NSW)` = 70303,
                      `Criminal law legislation and regulations, including parole, witness protection, substantive crimes (NSW)` = 70403,
                      `Government legislation, including elections, FOI, anti-corruption, police regulations (NSW)` = 70503,
                      `Environment (NSW)` = 70603,
                      `Families and children, including births, deaths, and marriages legislation, succession law (NSW)` = 70703,
                      `Fishing and farming, and related legislation (NSW)` = 70803,
                      `Human rights, including discrimination legislation and legislation relating to vulnerable persons (NSW)` = 70903,
                      `Land, development and planning legislation (NSW)` = 71003,
                      `Land, government entitlement  (NSW)` = 71103,
                      `Land, non-government including conveyancing (NSW)` = 71203,
                      `Law reform (NSW)` = 71303,
                      `Legal profession (NSW)` = 71403,
                      `Local government, regulation of, legislation and regulations (NSW)` = 71503,
                      `Industry regulation (NSW)` = 71603,
                      `Morality legislation, including gambling, racing, alcohol, firearms (NSW)` = 71703,
                      `Natural resources and mining, including water, coal, gas pipelines (NSW)` = 71803,
                      `Property, general, including property act, conveyancing, leases (NSW)` = 71903,
                      `Refugees and migration (NSW)` = 72003,
                      `Taxation, including stamp duty  (NSW)` = 72103,
                      `Trade practices, including consumer legislation (NSW)` = 72203,
                      `Trusts (NSW)` = 72303,
                      `Workplace relations, broadly conceived, including superannuation (NSW)` = 72403,
                      `Other (NSW)` = 72503,
                      `Aboriginal and Indigenous, legislation and regulations (NT)` = 70004,
                      `Civil wrongs, including wrongs legislation, civil liability, defamation, accident liability, compensation (NT)` = 70104,
                      `Community regulation general, including roads acts, community welfare, animals, police regulations (NT)` = 70204,
                      `Corporations, including commercial activity,  business regulations (NT)` = 70304,
                      `Criminal law legislation and regulations, including parole, witness protection, substantive crimes (NT)` = 70404,
                      `Government legislation, including elections, FOI, anti-corruption, police regulations (NT)` = 70504,
                      `Environment (NT)` = 70604,
                      `Families and children, including births, deaths, and marriages legislation, succession law (NT)` = 70704,
                      `Fishing and farming, and related legislation (NT)` = 70804,
                      `Human rights, including discrimination legislation and legislation relating to vulnerable persons (NT)` = 70904,
                      `Land, development and planning legislation (NT)` = 71004,
                      `Land, government entitlement  (NT)` = 71104,
                      `Land, non-government including conveyancing (NT)` = 71204,
                      `Law reform (NT)` = 71304,
                      `Legal profession (NT)` = 71404,
                      `Local government, regulation of, legislation and regulations (NT)` = 71504,
                      `Industry regulation (NT)` = 71604,
                      `Morality legislation, including gambling, racing, alcohol, firearms (NT)` = 71704,
                      `Natural resources and mining, including water, coal, gas pipelines (NT)` = 71804,
                      `Property, general, including property act, conveyancing, leases (NT)` = 71904,
                      `Refugees and migration (NT)` = 72004,
                      `Taxation, including stamp duty  (NT)` = 72104,
                      `Trade practices, including consumer legislation (NT)` = 72204,
                      `Trusts (NT)` = 72304,
                      `Workplace relations, broadly conceived, including superannuation (NT)` = 72404,
                      `Other (NT)` = 72504,
                      `Aboriginal and Indigenous, legislation and regulations (Qld.)` = 70005,
                      `Civil wrongs, including wrongs legislation, civil liability, defamation, accident liability, compensation (Qld.)` = 70105,
                      `Community regulation general, including roads acts, community welfare, animals, police regulations (Qld.)` = 70205,
                      `Corporations, including commercial activity,  business regulations (Qld.)` = 70305,
                      `Criminal law legislation and regulations, including parole, witness protection, substantive crimes (Qld.)` = 70405,
                      `Government legislation, including elections, FOI, anti-corruption, police regulations (Qld.)` = 70505,
                      `Environment (Qld.)` = 70605,
                      `Families and children, including births, deaths, and marriages legislation, succession law (Qld.)` = 70705,
                      `Fishing and farming, and related legislation (Qld.)` = 70805,
                      `Human rights, including discrimination legislation and legislation relating to vulnerable persons (Qld.)` = 70905,
                      `Land, development and planning legislation (Qld.)` = 71005,
                      `Land, government entitlement  (Qld.)` = 71105,
                      `Land, non-government including conveyancing (Qld.)` = 71205,
                      `Law reform (Qld.)` = 71305,
                      `Legal profession (Qld.)` = 71405,
                      `Local government, regulation of, legislation and regulations (Qld.)` = 71505,
                      `Industry regulation (Qld.)` = 71605,
                      `Morality legislation, including gambling, racing, alcohol, firearms (Qld.)` = 71705,
                      `Natural resources and mining, including water, coal, gas pipelines (Qld.)` = 71805,
                      `Property, general, including property act, conveyancing, leases (Qld.)` = 71905,
                      `Refugees and migration (Qld.)` = 72005,
                      `Taxation, including stamp duty  (Qld.)` = 72105,
                      `Trade practices, including consumer legislation (Qld.)` = 72205,
                      `Trusts (Qld.)` = 72305,
                      `Workplace relations, broadly conceived, including superannuation (Qld.)` = 72405,
                      `Other (Qld.)` = 72505,
                      `Aboriginal and Indigenous, legislation and regulations (SA)` = 70006,
                      `Civil wrongs, including wrongs legislation, civil liability, defamation, accident liability, compensation (SA)` = 70106,
                      `Community regulation general, including roads acts, community welfare, animals, police regulations (SA)` = 70206,
                      `Corporations, including commercial activity,  business regulations (SA)` = 70306,
                      `Criminal law legislation and regulations, including parole, witness protection, substantive crimes (SA)` = 70406,
                      `Government legislation, including elections, FOI, anti-corruption, police regulations (SA)` = 70506,
                      `Environment (SA)` = 70606,
                      `Families and children, including births, deaths, and marriages legislation, succession law (SA)` = 70706,
                      `Fishing and farming, and related legislation (SA)` = 70806,
                      `Human rights, including discrimination legislation and legislation relating to vulnerable persons (SA)` = 70906,
                      `Land, development and planning legislation (SA)` = 71006,
                      `Land, government entitlement  (SA)` = 71106,
                      `Land, non-government including conveyancing (SA)` = 71206,
                      `Law reform (SA)` = 71306,
                      `Legal profession (SA)` = 71406,
                      `Local government, regulation of, legislation and regulations (SA)` = 71506,
                      `Industry regulation (SA)` = 71606,
                      `Morality legislation, including gambling, racing, alcohol, firearms (SA)` = 71706,
                      `Natural resources and mining, including water, coal, gas pipelines (SA)` = 71806,
                      `Property, general, including property act, conveyancing, leases (SA)` = 71906,
                      `Refugees and migration (SA)` = 72006,
                      `Taxation, including stamp duty  (SA)` = 72106,
                      `Trade practices, including consumer legislation (SA)` = 72206,
                      `Trusts (SA)` = 72306,
                      `Workplace relations, broadly conceived, including superannuation (SA)` = 72406,
                      `Other (SA)` = 72506,
                      `Aboriginal and Indigenous, legislation and regulations (Tas.)` = 70007,
                      `Civil wrongs, including wrongs legislation, civil liability, defamation, accident liability, compensation (Tas.)` = 70107,
                      `Community regulation general, including roads acts, community welfare, animals, police regulations (Tas.)` = 70207,
                      `Corporations, including commercial activity,  business regulations (Tas.)` = 70307,
                      `Criminal law legislation and regulations, including parole, witness protection, substantive crimes (Tas.)` = 70407,
                      `Government legislation, including elections, FOI, anti-corruption, police regulations (Tas.)` = 70507,
                      `Environment (Tas.)` = 70607,
                      `Families and children, including births, deaths, and marriages legislation, succession law (Tas.)` = 70707,
                      `Fishing and farming, and related legislation (Tas.)` = 70807,
                      `Human rights, including discrimination legislation and legislation relating to vulnerable persons (Tas.)` = 70907,
                      `Land, development and planning legislation (Tas.)` = 71007,
                      `Land, government entitlement  (Tas.)` = 71107,
                      `Land, non-government including conveyancing (Tas.)` = 71207,
                      `Law reform (Tas.)` = 71307,
                      `Legal profession (Tas.)` = 71407,
                      `Local government, regulation of, legislation and regulations (Tas.)` = 71507,
                      `Industry regulation (Tas.)` = 71607,
                      `Morality legislation, including gambling, racing, alcohol, firearms (Tas.)` = 71707,
                      `Natural resources and mining, including water, coal, gas pipelines (Tas.)` = 71807,
                      `Property, general, including property act, conveyancing, leases (Tas.)` = 71907,
                      `Refugees and migration (Tas.)` = 72007,
                      `Taxation, including stamp duty  (Tas.)` = 72107,
                      `Trade practices, including consumer legislation (Tas.)` = 72207,
                      `Trusts (Tas.)` = 72307,
                      `Workplace relations, broadly conceived, including superannuation (Tas.)` = 72407,
                      `Other (Tas.)` = 72507,
                      `Aboriginal and Indigenous, legislation and regulations (Vic.)` = 70008,
                      `Civil wrongs, including wrongs legislation, civil liability, defamation, accident liability, compensation (Vic.)` = 70108,
                      `Community regulation general, including roads acts, community welfare, animals, police regulations (Vic.)` = 70208,
                      `Corporations, including commercial activity,  business regulations (Vic.)` = 70308,
                      `Criminal law legislation and regulations, including parole, witness protection, substantive crimes (Vic.)` = 70408,
                      `Government legislation, including elections, FOI, anti-corruption, police regulations (Vic.)` = 70508,
                      `Environment (Vic.)` = 70608,
                      `Families and children, including births, deaths, and marriages legislation, succession law (Vic.)` = 70708,
                      `Fishing and farming, and related legislation (Vic.)` = 70808,
                      `Human rights, including discrimination legislation and legislation relating to vulnerable persons (Vic.)` = 70908,
                      `Land, development and planning legislation (Vic.)` = 71008,
                      `Land, government entitlement  (Vic.)` = 71108,
                      `Land, non-government including conveyancing (Vic.)` = 71208,
                      `Law reform (Vic.)` = 71308,
                      `Legal profession (Vic.)` = 71408,
                      `Local government, regulation of, legislation and regulations (Vic.)` = 71508,
                      `Industry regulation (Vic.)` = 71608,
                      `Morality legislation, including gambling, racing, alcohol, firearms (Vic.)` = 71708,
                      `Natural resources and mining, including water, coal, gas pipelines (Vic.)` = 71808,
                      `Property, general, including property act, conveyancing, leases (Vic.)` = 71908,
                      `Refugees and migration (Vic.)` = 72008,
                      `Taxation, including stamp duty  (Vic.)` = 72108,
                      `Trade practices, including consumer legislation (Vic.)` = 72208,
                      `Trusts (Vic.)` = 72308,
                      `Workplace relations, broadly conceived, including superannuation (Vic.)` = 72408,
                      `Other (Vic.)` = 72508,
                      `Aboriginal and Indigenous, legislation and regulations (Norfolk Island)` = 70009,
                      `Civil wrongs, including wrongs legislation, civil liability, defamation, accident liability, compensation (Norfolk Island)` = 70109,
                      `Community regulation general, including roads acts, community welfare, animals, police regulations (Norfolk Island)` = 70209,
                      `Corporations, including commercial activity,  business regulations (Norfolk Island)` = 70309,
                      `Criminal law legislation and regulations, including parole, witness protection, substantive crimes (Norfolk Island)` = 70409,
                      `Government legislation, including elections, FOI, anti-corruption, police regulations (Norfolk Island)` = 70509,
                      `Environment (Norfolk Island)` = 70609,
                      `Families and children, including births, deaths, and marriages legislation, succession law (Norfolk Island)` = 70709,
                      `Fishing and farming, and related legislation (Norfolk Island)` = 70809,
                      `Human rights, including discrimination legislation and legislation relating to vulnerable persons (Norfolk Island)` = 70909,
                      `Land, development and planning legislation (Norfolk Island)` = 71009,
                      `Land, government entitlement  (Norfolk Island)` = 71109,
                      `Land, non-government including conveyancing (Norfolk Island)` = 71209,
                      `Law reform (Norfolk Island)` = 71309,
                      `Legal profession (Norfolk Island)` = 71409,
                      `Local government, regulation of, legislation and regulations (Norfolk Island)` = 71509,
                      `Industry regulation (Norfolk Island)` = 71609,
                      `Morality legislation, including gambling, racing, alcohol, firearms (Norfolk Island)` = 71709,
                      `Natural resources and mining, including water, coal, gas pipelines (Norfolk Island)` = 71809,
                      `Property, general, including property act, conveyancing, leases (Norfolk Island)` = 71909,
                      `Refugees and migration (Norfolk Island)` = 72009,
                      `Taxation, including stamp duty  (Norfolk Island)` = 72109,
                      `Trade practices, including consumer legislation (Norfolk Island)` = 72209,
                      `Trusts (Norfolk Island)` = 72309,
                      `Workplace relations, broadly conceived, including superannuation (Norfolk Island)` = 72409,
                      `Other (Norfolk Island)` = 72509,
                      `Aboriginal and Indigenous, legislation and regulations (Nauru)` = 70010,
                      `Civil wrongs, including wrongs legislation, civil liability, defamation, accident liability, compensation (Nauru)` = 70110,
                      `Community regulation general, including roads acts, community welfare, animals, police regulations (Nauru)` = 70210,
                      `Corporations, including commercial activity,  business regulations (Nauru)` = 70310,
                      `Criminal law legislation and regulations, including parole, witness protection, substantive crimes (Nauru)` = 70410,
                      `Government legislation, including elections, FOI, anti-corruption, police regulations (Nauru)` = 70510,
                      `Environment (Nauru)` = 70610,
                      `Families and children, including births, deaths, and marriages legislation, succession law (Nauru)` = 70710,
                      `Fishing and farming, and related legislation (Nauru)` = 70810,
                      `Human rights, including discrimination legislation and legislation relating to vulnerable persons (Nauru)` = 70910,
                      `Land, development and planning legislation (Nauru)` = 71010,
                      `Land, government entitlement  (Nauru)` = 71110,
                      `Land, non-government including conveyancing (Nauru)` = 71210,
                      `Law reform (Nauru)` = 71310,
                      `Legal profession (Nauru)` = 71410,
                      `Local government, regulation of, legislation and regulations (Nauru)` = 71510,
                      `Industry regulation (Nauru)` = 71610,
                      `Morality legislation, including gambling, racing, alcohol, firearms (Nauru)` = 71710,
                      `Natural resources and mining, including water, coal, gas pipelines (Nauru)` = 71810,
                      `Property, general, including property act, conveyancing, leases (Nauru)` = 71910,
                      `Refugees and migration (Nauru)` = 72010,
                      `Taxation, including stamp duty  (Nauru)` = 72110,
                      `Trade practices, including consumer legislation (Nauru)` = 72210,
                      `Trusts (Nauru)` = 72310,
                      `Workplace relations, broadly conceived, including superannuation (Nauru)` = 72410,
                      `Other (Nauru)` = 72510,
                      `Aboriginal and Indigenous, legislation and regulations (WA)` = 70011,
                      `Civil wrongs, including wrongs legislation, civil liability, defamation, accident liability, compensation (WA)` = 70111,
                      `Community regulation general, including roads acts, community welfare, animals, police regulations (WA)` = 70211,
                      `Corporations, including commercial activity,  business regulations (WA)` = 70311,
                      `Criminal law legislation and regulations, including parole, witness protection, substantive crimes (WA)` = 70411,
                      `Government legislation, including elections, FOI, anti-corruption, police regulations (WA)` = 70511,
                      `Environment (WA)` = 70611,
                      `Families and children, including births, deaths, and marriages legislation, succession law (WA)` = 70711,
                      `Fishing and farming, and related legislation (WA)` = 70811,
                      `Human rights, including discrimination legislation and legislation relating to vulnerable persons (WA)` = 70911,
                      `Land, development and planning legislation (WA)` = 71011,
                      `Land, government entitlement  (WA)` = 71111,
                      `Land, non-government including conveyancing (WA)` = 71211,
                      `Law reform (WA)` = 71311,
                      `Legal profession (WA)` = 71411,
                      `Local government, regulation of, legislation and regulations (WA)` = 71511,
                      `Industry regulation (WA)` = 71611,
                      `Morality legislation, including gambling, racing, alcohol, firearms (WA)` = 71711,
                      `Natural resources and mining, including water, coal, gas pipelines (WA)` = 71811,
                      `Property, general, including property act, conveyancing, leases (WA)` = 71911,
                      `Refugees and migration (WA)` = 72011,
                      `Taxation, including stamp duty  (WA)` = 72111,
                      `Trade practices, including consumer legislation (WA)` = 72211,
                      `Trusts (WA)` = 72311,
                      `Workplace relations, broadly conceived, including superannuation (WA)` = 72411,
                      `Other (WA)` = 72511,
                      `Foreign Statute` = 1000,
                      `Imperial Statute` = 1001,
                      `International Convention` = 1002,
                      `Treaty` = 1003,
                      `Other` = 1004,
                      `Territory courts legislation, all versions (ACT)` = 41502,
                      `Territory courts legislation, all versions (NT)` = 41504,
                      `Not applicable` = 999)

justice_decision$lawSupp1|>unique()|>sort()

val_labels(justice_decision$lawSupp1)<- varLegalProvisions



justice_decision$lawSupp1|>labelled::to_factor()|>unique()|>levels()


# 115.	Legal Provision Considered by the Court 2	126


justice_decision$lawType2|>unique()|>sort()

val_labels(justice_decision$lawType2)<- varLawArea

# 116.	Legal Provision Supplement 2	127

justice_decision$lawSupp2|>unique()|>sort()

val_labels(justice_decision$lawSupp2)<- varLegalProvisions

justice_decision$lawSupp2|>labelled::to_factor()|>unique()|>levels()

# 117.	Legal Provision Considered by the Court 3	128

justice_decision$lawType3|>unique()|>sort()

val_labels(justice_decision$lawType3)<- varLawArea

# 118.	Legal Provision Supplement 3	129

justice_decision$lawSupp3|>unique()|>sort()

val_labels(justice_decision$lawSupp3)<- varLegalProvisions

justice_decision$lawSupp3|>labelled::to_factor()|>unique()|>levels()

# 119.	Decision Type	131
varDecisionTypes<-c(`Cases of the Court decided by a signed opinion, or multiple opinions after oral argument.` = 1,
                    `Cases decided with an opinion but without oral argument ` = 2,
                    `Cases decided with an evenly divided vote that that was appealed from a justice of the High Court, the Supreme Court of a State or Territory, a decision of the Federal Court of Australia, or a decision of the Family Court of Australia` = 3,
                    `Cases decided with an evenly divided vote that do not fit within the description in Decision Type 3` = 4,
                    `Not applicable`=999)

justice_decision$decisionType |>unique()|>sort()

val_labels(justice_decision$decisionType)<- varDecisionTypes

# 120.	Constitutional Matter	132

varConMatter<-c(`not categorized by L&W as a constitutional law matter` = 1,
                `yes, is categorized by L&W as a constitutional law matter` = 2)

justice_decision$lwConMatter |>unique()|>sort()

val_labels(justice_decision$lwConMatter)<- varConMatter

varJudicialReview<-c(`Not Applicable` = 999,
                     `no JR` = 1,
                     `yes, JR of federal statute v federal Constitution` = 2,
                     `yes, JR of federal executive action v federal Constitution` = 3,
                     `yes, JR of state statute v federal Constitution` = 4,
                     `yes, JR of state executive action v federal Constitution` = 5,
                     `yes, JR of state statute v state Constitution` = 6,
                     `yes, JR of state executive action v state Constitution` = 7,
                     `yes, JR of local government action v federal Constitution` = 8,
                     `yes, JR of local government action v federal Constitution` = 9)

justice_decision$judicialReview |>unique()|>sort()
val_labels(justice_decision$judicialReview)<- varJudicialReview


# # 122.	Law Reviewed	134
# justice_decision$lawReviewed[justice_decision$lawReviewed=='999']<-NA
#
# justice_decision$lawReviewed |>unique()|>sort()

# # 123.	Date Assent	135
# justice_decision$dateAssent[justice_decision$dateAssent=='999']<-NA
#
# justice_decision$dateAssent |>unique()|>sort()
#
# justice_decision$dateAssent<-lubridate::ymd(justice_decision$dateRoyalAssent)
#

# 124.	Lower Court Constitutional Decision	136

varLcConDecision<-c(`Not applicable` = 999,
                    `lower held law constitutional` = 1,
                    `lower court held law unconstitutional (void)` = 2,
                    `lower court narrowed scope of law in order to uphold as constitutional` = 3,
                    `lower court didn’t consider constitutionality of law` = 4,
                    `no lower court decision in the matter (e.g., original jurisdiction, Court of Disputed Returns etc) ` = 5
                    )

justice_decision$lcConDecision |>unique()|>sort()

val_labels(justice_decision$lcConDecision)<- varLcConDecision


# 125.	Declaration of Unconstitutionality	137

varDeclarationUncon<-c(`no declaration of unconstitutionality` = 1,
                       `act of Parliament declared unconstitutional pursuant to Commonwealth Constitution` = 2,
                       `state or territorial law, regulation, or constitutional provision unconstitutional pursuant to Commonwealth Constitution` = 3,
                       `local government regulation/ordinance unconstitutional pursuant to Commonwealth Constitution` = 4,
                       `state or territorial law, regulation, or constitutional provision unconstitutional pursuant to state constitution` = 5,
                       `local government regulation/ordinance unconstitutional pursuant to state constitution` = 6,
                       `act of Parliament restricted in scope to avoid declaration of unconstitutionality pursuant to Commonwealth Constitution` = 7,
                       `state or territorial law, regulation, or constitutional provision restricted in scope to avoid declaration of unconstitutionality pursuant to Commonwealth Constitution` = 8,
                       `local government regulation/ordinance restricted in scope to avoid declaration of unconstitutionality pursuant to Commonwealth Constitution` = 9,
                       `state or territorial law, regulation, or constitutional provision restricted in scope to avoid declaration of unconstitutionality pursuant to state constitution` = 10,
                       `local government regulation/ordinance restricted in scope to avoid declaration of unconstitutionality pursuant to state constitution` = 11,
                       `Not Applicable`=999)

justice_decision$declarationUncon |>unique()|>sort()

val_labels(justice_decision$declarationUncon)<- varDeclarationUncon


# 126.	Disposition of Case	138

varCaseDisposition <- c(`Appeal/application allowed` = 1,
                        `Appeal/application allowed, order below set aside and/or varied` = 2,
                        `Appeal/application allowed, order below set aside and/or varied in part` = 3,
                        `Appeal/application allowed, order below set aside and/or varied, matter remitted` = 4,
                        `Appeal/application allowed, order below set aside and/or varied in part, matter remitted` = 5,
                        `Appeal/application dismissed ` = 6,
                        `Appeal/application allowed, remit for further determination` = 7,
                        `Appeal/application allowed in part, remit for further determination` = 8,
                        `Appeal/application allowed in part, order below set aside and/or varied` = 9,
                        `Appeal/application allowed in part, order below set aside and/or varied in part` = 10,
                        `Appeal/application allowed in part, order below set aside and/or varied, matter remitted` = 11,
                        `Appeal/application allowed in part, order below set aside and/or varied in part, matter remitted` = 12,
                        `Special leave revoked` = 13,
                        `Appeal granted and appeal allowed instantia` = 14,
                        `Other disposition` = 15,
                        `Questions answered (special case, case referred, etc)` = 16,
                        `Special leave denied by enlarged bench` = 17)


justice_decision$caseDisposition |>unique()|>sort()

val_labels(justice_decision$caseDisposition)<- varCaseDisposition


# 127.	Winning Party	139

varPartyWinning<-c(`no favourable disposition for appealing/petitioning party apparent` = 0,
                   `appealing/petitioning party received a favourable disposition` = 1,
                   `favourable disposition for petitioning party unclear` = 2)

justice_decision$partyWinning |>unique()|>sort()
val_labels(justice_decision$partyWinning)<- varPartyWinning

# VOTING AND OPINION VARIABLES	140

# 128.	Majority Votes	141
justice_decision$majVotes |>unique()|>sort()

# 129.	Minority Votes	142
justice_decision$minVotes |>unique()|>sort()

# 130.	Justice ID	143

justice_decision$justice |>unique()|>sort()
val_labels(justice_decision$justice)<- varJustice


# 131.	Vote in the Case	144

varVote<-c(`wrote the opinion of the Court (i.e. majority opinion)` = 1,
  `wrote opinion for plurality opinion of Court majority` = 2,
  `participated in opinion of the Court (i.e. majority opinion)` = 3,
  `participated in plurality opinion of Court majority` = 4,
  `wrote substantive concurring opinion` = 5,
  `participated in substantive concurring opinion` = 6,
  `wrote formal concurring opinion` = 7,
  `participated in formal concurring opinion` = 8,
  `wrote dissenting opinion` = 9,
  `participated in dissenting opinion` = 10,
  `judgment of the Court (unanimous, single judgment)` = 11,
  `other` = 12,
  `Not Applicable`=999)

justice_decision$vote |>unique()|>sort()

val_labels(justice_decision$vote)<- varVote

# 132.	Direction of the Individual Justice’s Votes	145


varJusticeDirection <-c(`conservative` = 1,
                        `liberal` = 2,
                        `unspecifiable`=3,
                        `Not Applicable`=999)

justice_decision$direction |>unique()|>sort()

val_labels(justice_decision$direction)<- varJusticeDirection

# 133.	Majority and Minority Voting by Justice	146

varJusticeMajority<-c(`dissent` = 1,
                      `majority` = 2,
                      `Not Applicable`=999)

justice_decision$majority |>unique()|>sort()
val_labels(justice_decision$majority)<- varJusticeMajority


# 134.	Justice Coalition 1	147

justice_decision$justiceCoalition1 |>unique()|>sort()
val_labels(justice_decision$justiceCoalition1)<- varJustice

# 135.	Justice Coalition 2	147
justice_decision$justiceCoalition2 |>unique()|>sort()
val_labels(justice_decision$justiceCoalition2)<- varJustice

# 136.	Justice Coalition 3	147
justice_decision$justiceCoalition3 |>unique()|>sort()
val_labels(justice_decision$justiceCoalition3)<- varJustice

# 137.	Justice Coalition 4	147
justice_decision$justiceCoalition4 |>unique()|>sort()
val_labels(justice_decision$justiceCoalition4)<- varJustice

# 138.	Justice Coalition 5	147
justice_decision$justiceCoalition5 |>unique()|>sort()
val_labels(justice_decision$justiceCoalition5)<- varJustice

# 139.	Justice Coalition 6	147
justice_decision$justiceCoalition6 |>unique()|>sort()
val_labels(justice_decision$justiceCoalition6)<- varJustice

# Extras

# proportionLiberalPanel

justice_decision$proportionLiberalPanel |>unique()|>sort()

# proportionLiberalCourt
justice_decision$proportionLiberalPanel |>unique()|>sort()

# proportionWomenPanel
justice_decision$proportionWomenPanel |>unique()|>sort()

# proportionWomenCourt
justice_decision$proportionWomenCourt |>unique()|>sort()

# rlsIdeologyScore
justice_decision$rlsIdeologyScore |>unique()|>sort()

# appPMParty

varPolParties<-c(ALP=1, Coalition=2)
justice_decision$appPMParty |>unique()|>sort()
val_labels(justice_decision$appPMParty)<- varPolParties

# gender

justice_decision$gender |>table()

varGender<-c(Male=1, Female=2)
val_labels(justice_decision$gender)<- varGender

# yearBirth

justice_decision$yearBirth|>unique()|>sort()

justice_decision$HCDBcaseId[justice_decision$primaryIssue==50406]|>unique()


# Label variables

var_label(justice_decision)<- list(HCDBcaseId = "HCDB Case ID",
                                   clrCite = "Commonwealth Law Reports Citation",
                                   aljrCite = "Australian Law Journal Report Citation",
                                   alrCite = "Australian Law Report Citation",
                                   hcaCite = "High Court Citation",
                                   caseNumber = "Case Number",
                                   caseName = "Case Name",
                                   multipleMatters = "Multiple Matters",
                                   numMultipleMatters = "Number of Multiple Matters",
                                   multipleMatterHCDBID1 = "Multiple Matters HCDB Identification 1",
                                   multipleMatterHCDBID2 = "Multiple Matters HCDB Identification 2",
                                   multipleMatterHCDBID3 = "Multiple Matters HCDB Identification 3",
                                   multipleMatterHCDBID4 = "Multiple Matters HCDB Identification 4",
                                   multipleMatterHCDBID5 = "Multiple Matters HCDB Identification 5",
                                   multipleMatterHCDBID6 = "Multiple Matters HCDB Identification 6",
                                   multipleMatterHCDBID7 = "Multiple Matters HCDB Identification 7",
                                   multipleMatterHCDBID8 = "Multiple Matters HCDB Identification 8",
                                   numAppellants = "Number of Appellants/Petitioners",
                                   numFedGovAppellant = "Number of Appellants, Federal Government",
                                   numStateGovAppellant = "Number of Appellants, State Government",
                                   numCorpAppellant = "Number of Appellants, Corporation",
                                   numNonCorpOrgAppellant = "Number of Appellants, Non-Corporate Entity",
                                   numIndividualAppellant = "Number of Appellants, Individuals",
                                   appellant1 = "Appellant/Petitioner [1] to [34]",
                                   appellant1State = "Appellant/Petitioner State [1] to [34]",
                                   appellant2 = "Appellant/Petitioner [1] to [34]",
                                   appellant2State = "Appellant/Petitioner State [1] to [34]",
                                   appellant3 = "Appellant/Petitioner [1] to [34]",
                                   appellant3State = "Appellant/Petitioner State [1] to [34]",
                                   appellant4 = "Appellant/Petitioner [1] to [34]",
                                   appellant4State = "Appellant/Petitioner State [1] to [34]",
                                   appellant5 = "Appellant/Petitioner [1] to [34]",
                                   appellant5State = "Appellant/Petitioner State [1] to [34]",
                                   appellant6 = "Appellant/Petitioner [1] to [34]",
                                   appellant6State = "Appellant/Petitioner State [1] to [34]",
                                   appellant7 = "Appellant/Petitioner [1] to [34]",
                                   appellant7State = "Appellant/Petitioner State [1] to [34]",
                                   appellant8 = "Appellant/Petitioner [1] to [34]",
                                   appellant8State = "Appellant/Petitioner State [1] to [34]",
                                   appellant9 = "Appellant/Petitioner [1] to [34]",
                                   appellant9State = "Appellant/Petitioner State [1] to [34]",
                                   appellant10 = "Appellant/Petitioner [1] to [34]",
                                   appellant10State = "Appellant/Petitioner State [1] to [34]",
                                   appellant11 = "Appellant/Petitioner [1] to [34]",
                                   appellant11State = "Appellant/Petitioner State [1] to [34]",
                                   appellant12 = "Appellant/Petitioner [1] to [34]",
                                   appellant12State = "Appellant/Petitioner State [1] to [34]",
                                   appellant13 = "Appellant/Petitioner [1] to [34]",
                                   appellant13State = "Appellant/Petitioner State [1] to [34]",
                                   appellant14 = "Appellant/Petitioner [1] to [34]",
                                   appellant14State = "Appellant/Petitioner State [1] to [34]",
                                   appellant15 = "Appellant/Petitioner [1] to [34]",
                                   appellant15State = "Appellant/Petitioner State [1] to [34]",
                                   appellant16 = "Appellant/Petitioner [1] to [34]",
                                   appellant16State = "Appellant/Petitioner State [1] to [34]",
                                   appellant17 = "Appellant/Petitioner [1] to [34]",
                                   appellant17State = "Appellant/Petitioner State [1] to [34]",
                                   appellant18 = "Appellant/Petitioner [1] to [34]",
                                   appellant18State = "Appellant/Petitioner State [1] to [34]",
                                   appellant19 = "Appellant/Petitioner [1] to [34]",
                                   appellant19State = "Appellant/Petitioner State [1] to [34]",
                                   appellant20 = "Appellant/Petitioner [1] to [34]",
                                   appellant20State = "Appellant/Petitioner State [1] to [34]",
                                   appellant21 = "Appellant/Petitioner [1] to [34]",
                                   appellant21State = "Appellant/Petitioner State [1] to [34]",
                                   appellant22 = "Appellant/Petitioner [1] to [34]",
                                   appellant22State = "Appellant/Petitioner State [1] to [34]",
                                   appellant23 = "Appellant/Petitioner [1] to [34]",
                                   appellant23State = "Appellant/Petitioner State [1] to [34]",
                                   appellant24 = "Appellant/Petitioner [1] to [34]",
                                   appellant24State = "Appellant/Petitioner State [1] to [34]",
                                   appellant25 = "Appellant/Petitioner [1] to [34]",
                                   appellant25State = "Appellant/Petitioner State [1] to [34]",
                                   appellant26 = "Appellant/Petitioner [1] to [34]",
                                   appellant26State = "Appellant/Petitioner State [1] to [34]",
                                   appellant27 = "Appellant/Petitioner [1] to [34]",
                                   appellant27State = "Appellant/Petitioner State [1] to [34]",
                                   appellant28 = "Appellant/Petitioner [1] to [34]",
                                   appellant28State = "Appellant/Petitioner State [1] to [34]",
                                   appellant29 = "Appellant/Petitioner [1] to [34]",
                                   appellant29State = "Appellant/Petitioner State [1] to [34]",
                                   appellant30 = "Appellant/Petitioner [1] to [34]",
                                   appellant30State = "Appellant/Petitioner State [1] to [34]",
                                   appellant31 = "Appellant/Petitioner [1] to [34]",
                                   appellant31State = "Appellant/Petitioner State [1] to [34]",
                                   appellant32 = "Appellant/Petitioner [1] to [34]",
                                   appellant32State = "Appellant/Petitioner State [1] to [34]",
                                   appellant33 = "Appellant/Petitioner [1] to [34]",
                                   appellant33State = "Appellant/Petitioner State [1] to [34]",
                                   appellant34 = "Appellant/Petitioner [1] to [34]",
                                   appellant34State = "Appellant/Petitioner State [1] to [34]",
                                   numRespondents = "Number of Respondents",
                                   numFedGovResp = "Number of Respondents, Federal Government",
                                   numStateGovResp = "Number of Respondent, State Government",
                                   numCorpResp = "Number of Respondent, Corporation",
                                   numNonCorpOrgResp = "Number of Respondent, Non-Corporate Entity",
                                   numIndividualResp = "Number of Respondents, Individuals",
                                   respondent1 = "Respondent [1] to [49]",
                                   respondent1State = "Respondent State [1] to [49]",
                                   respondent2 = "Respondent [1] to [49]",
                                   respondent2State = "Respondent State [1] to [49]",
                                   respondent3 = "Respondent [1] to [49]",
                                   respondent3State = "Respondent State [1] to [49]",
                                   respondent4 = "Respondent [1] to [49]",
                                   respondent4State = "Respondent State [1] to [49]",
                                   respondent5 = "Respondent [1] to [49]",
                                   respondent5State = "Respondent State [1] to [49]",
                                   respondent6 = "Respondent [1] to [49]",
                                   respondent6State = "Respondent State [1] to [49]",
                                   respondent7 = "Respondent [1] to [49]",
                                   respondent7State = "Respondent State [1] to [49]",
                                   respondent8 = "Respondent [1] to [49]",
                                   respondent8State = "Respondent State [1] to [49]",
                                   respondent9 = "Respondent [1] to [49]",
                                   respondent9State = "Respondent State [1] to [49]",
                                   respondent10 = "Respondent [1] to [49]",
                                   respondent10State = "Respondent State [1] to [49]",
                                   respondent11 = "Respondent [1] to [49]",
                                   respondent11State = "Respondent State [1] to [49]",
                                   respondent12 = "Respondent [1] to [49]",
                                   respondent12State = "Respondent State [1] to [49]",
                                   respondent13 = "Respondent [1] to [49]",
                                   respondent13State = "Respondent State [1] to [49]",
                                   respondent14 = "Respondent [1] to [49]",
                                   respondent14State = "Respondent State [1] to [49]",
                                   respondent15 = "Respondent [1] to [49]",
                                   respondent15State = "Respondent State [1] to [49]",
                                   respondent16 = "Respondent [1] to [49]",
                                   respondent16State = "Respondent State [1] to [49]",
                                   respondent17 = "Respondent [1] to [49]",
                                   respondent17State = "Respondent State [1] to [49]",
                                   respondent18 = "Respondent [1] to [49]",
                                   respondent18State = "Respondent State [1] to [49]",
                                   respondent19 = "Respondent [1] to [49]",
                                   respondent19State = "Respondent State [1] to [49]",
                                   respondent20 = "Respondent [1] to [49]",
                                   respondent20State = "Respondent State [1] to [49]",
                                   respondent21 = "Respondent [1] to [49]",
                                   respondent21State = "Respondent State [1] to [49]",
                                   respondent22 = "Respondent [1] to [49]",
                                   respondent22State = "Respondent State [1] to [49]",
                                   respondent23 = "Respondent [1] to [49]",
                                   respondent23State = "Respondent State [1] to [49]",
                                   respondent24 = "Respondent [1] to [49]",
                                   respondent24State = "Respondent State [1] to [49]",
                                   respondent25 = "Respondent [1] to [49]",
                                   respondent25State = "Respondent State [1] to [49]",
                                   respondent26 = "Respondent [1] to [49]",
                                   respondent26State = "Respondent State [1] to [49]",
                                   respondent27 = "Respondent [1] to [49]",
                                   respondent27State = "Respondent State [1] to [49]",
                                   respondent28 = "Respondent [1] to [49]",
                                   respondent28State = "Respondent State [1] to [49]",
                                   respondent29 = "Respondent [1] to [49]",
                                   respondent29State = "Respondent State [1] to [49]",
                                   respondent30 = "Respondent [1] to [49]",
                                   respondent30State = "Respondent State [1] to [49]",
                                   respondent31 = "Respondent [1] to [49]",
                                   respondent31State = "Respondent State [1] to [49]",
                                   respondent32 = "Respondent [1] to [49]",
                                   respondent32State = "Respondent State [1] to [49]",
                                   respondent33 = "Respondent [1] to [49]",
                                   respondent33State = "Respondent State [1] to [49]",
                                   respondent34 = "Respondent [1] to [49]",
                                   respondent34State = "Respondent State [1] to [49]",
                                   respondent35 = "Respondent [1] to [49]",
                                   respondent35State = "Respondent State [1] to [49]",
                                   respondent36 = "Respondent [1] to [49]",
                                   respondent36State = "Respondent State [1] to [49]",
                                   respondent37 = "Respondent [1] to [49]",
                                   respondent37State = "Respondent State [1] to [49]",
                                   respondent38 = "Respondent [1] to [49]",
                                   respondent38State = "Respondent State [1] to [49]",
                                   respondent39 = "Respondent [1] to [49]",
                                   respondent39State = "Respondent State [1] to [49]",
                                   respondent40 = "Respondent [1] to [49]",
                                   respondent40State = "Respondent State [1] to [49]",
                                   respondent41 = "Respondent [1] to [49]",
                                   respondent41State = "Respondent State [1] to [49]",
                                   respondent42 = "Respondent [1] to [49]",
                                   respondent42State = "Respondent State [1] to [49]",
                                   respondent43 = "Respondent [1] to [49]",
                                   respondent43State = "Respondent State [1] to [49]",
                                   respondent44 = "Respondent [1] to [49]",
                                   respondent44State = "Respondent State [1] to [49]",
                                   respondent45 = "Respondent [1] to [49]",
                                   respondent45State = "Respondent State [1] to [49]",
                                   respondent46 = "Respondent [1] to [49]",
                                   respondent46State = "Respondent State [1] to [49]",
                                   respondent47 = "Respondent [1] to [49]",
                                   respondent47State = "Respondent State [1] to [49]",
                                   respondent48 = "Respondent [1] to [49]",
                                   respondent48State = "Respondent State [1] to [49]",
                                   respondent49 = "Respondent [1] to [49]",
                                   respondent49State = "Respondent State [1] to [49]",
                                   intervener = "Intervener",
                                   numIntervener = "Number of Interveners",
                                   intervener1 = "Intervener Identity [1] to [7]",
                                   intervener2 = "Intervener Identity [1] to [7]",
                                   intervener3 = "Intervener Identity [1] to [7]",
                                   intervener4 = "Intervener Identity [1] to [7]",
                                   intervener5 = "Intervener Identity [1] to [7]",
                                   intervener6 = "Intervener Identity [1] to [7]",
                                   intervener7 = "Intervener Identity [1] to [7]",
                                   intervener8 = "Intervener Identity [1] to [7]",
                                   intervener9 = "Intervener Identity [1] to [7]",
                                   intervener10 = "Intervener Identity [1] to [7]",
                                   amicus = "Amicus",
                                   numAmici = "Number of Amici",
                                   jurisdictionGeneral = "Manner in which Court takes Jurisdiction General",
                                   jurisdictionSpecific = "Manner in which Court takes Jurisdiction Specific",
                                   adminAction = "Administrative Action Preceding Litigation",
                                   adminReview1 = "Administrative Review Preceding Litigation 1",
                                   adminReview2 = "Administrative Review Preceding Litigation 2",
                                   caseOriginGeneral = "Origin of Case General",
                                   caseOriginSpecific = "Origin of Case Specific",
                                   caseOriginState = "Origin of Case State",
                                   caseSourceGeneral = "Source of Case General",
                                   caseSourceSpecific = "Source of Case Specific",
                                   caseSourceState = "Source of Case State",
                                   lcDisposition = "Lower Court Disposition",
                                   lcDispositionDirection = "Lower Court Disposition Direction",
                                   lcDissent = "Lower Court Dissent",
                                   lcPanelSize = "Lower Court Panel Size",
                                   lcDissentNum = "Number of Lower Court Dissents",
                                   specialLeaveMethod = "Special Leave Method",
                                   numJusticesSL = "Number of Justices on Preliminary Special Leave Panel",
                                   prelimSLHearing = "Preliminary Special Leave Hearing",
                                   prelimSLDate = "Preliminary Special Leave Date",
                                   prelimSLNatCourt = "Natural Court at Preliminary Special Leave Hearing",
                                   referralJustice = "Referral Justice Where no Preliminary Special Leave Hearing",
                                   prelimSLOutcome = "Preliminary Special Leave Outcome",
                                   justiceSpecialLeave1 = "Justice Granting Special Leave 1",
                                   justiceSpecialLeave2 = "Justice Granting Special Leave 2",
                                   justiceSpecialLeave3 = "Justice Granting Special Leave 3",
                                   specialLeaveLocation = "Special Leave Location",
                                   politicalPowerSL = "Political Power on Date of Preliminary Special Leave Hearing",
                                   yearSL = "Year of Court on Date of Preliminary Special Leave Hearing",
                                   termSL = "Term of Court on Date of Preliminary Special Leave Hearing (financial year)",
                                   chiefSL = "Chief Justice Special Leave",
                                   pmSL = "Prime Minister on Date of Preliminary Special Leave Hearing",
                                   dateDecision = "Date of Decision",
                                   yearDecision = "Year of Decision",
                                   termDecision = "Term of Court Decision (financial year)",
                                   naturalCourtDecision = "Natural Court Decision",
                                   chiefDecision = "Chief Justice of Date of Decision",
                                   politicalPowerHcDecision = "Political Power on Date of High Court Decision",
                                   pmHcDecision = "Prime Minister on Date of Decision",
                                   numberJustices = "Number of Justices on Panel",
                                   dateArgumentBegin = "Date of Commencement of Oral Argument",
                                   dateArgumentConclude = "Date Oral Argument Concluded",
                                   totalTimeArgumentDays = "Total Time for Oral Argument Days",
                                   totalTimeArgumentMins = "Total Time for Oral Argument Minutes",
                                   yearArgument = "Year of Court Oral Argument",
                                   termArgument = "Term of Court Oral Argument (financial year)",
                                   naturalCourtArgument = "Natural Court Oral Argument",
                                   chiefArgument = "Chief Justice Oral Argument",
                                   politicalPowerArgument = "Political Power Oral Argument",
                                   pmArgument = "Prime Minister on Date of Oral Argument",
                                   locationOralArgument = "Location of Oral Argument",
                                   cthSGRepOralArgument = "Commonwealth Solicitor-General Representation at Oral Argument",
                                   cthSGRepPartyOralArgument1 = "Commonwealth Solicitor-General Party Representation at Oral Argument 1",
                                   #cthSGIntvSupporting1 = " ",
                                   cthSGRepPartyOralArgument2 = "Commonwealth Solicitor-General Party Representation at Oral Argument 2",
                                   primaryIssueArea = "Primary Issue Area",
                                   primaryIssueSubArea = "Primary Issue Sub Area",
                                   primaryIssue = "Primary Issue",
                                   secondaryIssueArea = "Secondary Issue Area",
                                   secondaryIssueSubArea = "Secondary Issue Sub Area",
                                   secondaryIssue = "Secondary Issue",
                                   tertiaryIssueArea = "Tertiary Issue Area",
                                   tertiaryIssueSubArea = "Tertiary Issue Sub Area",
                                   tertiaryIssue = "Tertiary Issue",
                                   decisionDirection = "Decision Direction",
                                   decisionDirectionDissent = "Decision Direction Dissent",
                                   authorityDecision1 = "Authority for Decision 1",
                                   authorityDecision1State = "Authority for Decision 1 State",
                                   authorityDecision2 = "Authority for Decision 2",
                                   authorityDecision2State = "Authority for Decision 2 State",
                                   authorityDecision3 = "Authority for Decision 3",
                                   authorityDecision3State = "Authority for Decision 3 State",
                                   panelSLDecision = "Panel Special Leave Decision",
                                   panelSLOutcomeGen = "Panel Special Leave Outcome General",
                                   panelSLOutcomeSpecific = "Panel Special Leave Outcome Specific",
                                   costsDecision = "Costs Decision",
                                   costsRelationHCDBID = "Costs Decision Related HCDBID",
                                   lawType1 = "Legal Provision Considered by the Court 1",
                                   lawSupp1 = "Legal Provision Supplement 1",
                                   lawType2 = "Legal Provision Considered by the Court 2",
                                   lawSupp2 = "Legal Provision Supplement 2",
                                   lawType3 = "Legal Provision Considered by the Court 3",
                                   lawSupp3 = "Legal Provision Supplement 3",
                                   decisionType = "Decision Type",
                                   lwConMatter = "Constitutional Matter",
                                   judicialReview = "Judicial Review",
                                   # lawReviewed = "Law Reviewed",
                                   # dateRoyalAssent = "Date Assent",
                                   lcConDecision = "Lower Court Constitutional Decision",
                                   declarationUncon = "Declaration of Unconstitutionality",
                                   caseDisposition = "Disposition of Case",
                                   partyWinning = "Winning Party",
                                   majVotes = "Majority Votes",
                                   minVotes = "Minority Votes",
                                   justice = "Justice ID",
                                   vote = "Vote in the Case",
                                   direction = "Direction of the Individual Justice’s Votes",
                                   majority = "Majority and Minority Voting by Justice",
                                   justiceCoalition1 = "Justice Coalition 1",
                                   justiceCoalition2 = "Justice Coalition 2",
                                   justiceCoalition3 = "Justice Coalition 3",
                                   justiceCoalition4 = "Justice Coalition 4",
                                   justiceCoalition5 = "Justice Coalition 5",
                                   justiceCoalition6 = "Justice Coalition 6",
                                   proportionLiberalPanel = "Proportion of the panel coded as 'Liberal'",
                                   proportionLiberalCourt = "Proportion of the court coded as 'Liberal'",
                                   proportionWomenPanel = "Proportion of women on the panel",
                                   proportionWomenCourt = "Proportion of women on the court",
                                   rlsIdeologyScore = "Ideology score of the justice. 1= liberal; 0=conservative",
                                   appPMParty = "Party of the Prime Minister who appointed the Justice",
                                   gender = "Gender of the Justice",
                                   yearBirth = "Year of Birth of the Justice",
                                   Notes = "Notes")





# Function to check if all values in a variable are within its label set
check_labels <- function(data, var_name) {
  # Get the variable
  var <- data[[var_name]]

  # Get the value labels
  labels <- val_labels(var)

  # If no labels, skip
  if (is.null(labels) || length(labels) == 0) {
    return(NULL)
  }

  # Get unique values in the data (excluding NA)
  actual_values <- unique(var[!is.na(var)])

  # Get expected values from labels
  expected_values <- unname(labels)

  # Find values that are not in the label set
  unexpected <- actual_values[!actual_values %in% expected_values]

  if (length(unexpected) > 0) {
    return(data.frame(
      variable = var_name,
      unexpected_values = paste(unexpected, collapse = ", "),
      expected_values = paste(expected_values, collapse = ", "),
      stringsAsFactors = FALSE
    ))
  }

  return(NULL)
}

# Get ONLY variables with VALUE labels (not just variable labels)
all_vars <- names(justice_decision)
labeled_vars <- character()

for (var_name in all_vars) {
  var <- justice_decision[[var_name]]
  if (!is.null(val_labels(var)) && length(val_labels(var)) > 0) {
    labeled_vars <- c(labeled_vars, var_name)
  }
}

cat("\n=== Checking Value Labels ===\n")
cat("Found", length(labeled_vars), "variables with value labels\n")
cat("(Out of", length(all_vars), "total variables)\n\n")

if (length(labeled_vars) == 0) {
  cat("No variables with value labels found. Have you applied val_labels() yet?\n")
} else {

  # Check each labeled variable
  problems <- lapply(labeled_vars, function(v) check_labels(justice_decision, v))
  problems <- do.call(rbind, problems[!sapply(problems, is.null)])

  if (is.null(problems) || nrow(problems) == 0) {
    cat("✓ SUCCESS! All values are within expected ranges for all labeled variables.\n")
  } else {
    cat("✗ PROBLEMS FOUND!\n\n")
    cat("The following variables have values outside their label sets:\n\n")

    for (i in 1:nrow(problems)) {
      cat("Variable:", problems$variable[i], "\n")
      cat("  Unexpected values:", problems$unexpected_values[i], "\n")
      cat("  Expected values:", problems$expected_values[i], "\n\n")
    }

    cat("\nTotal variables with problems:", nrow(problems), "\n")
  }

  # Also create a summary table
  cat("\n=== Summary of All Value-Labeled Variables ===\n")
  summary_table <- data.frame(
    variable = character(),
    n_labels = integer(),
    n_unique_values = integer(),
    all_labeled = character(),
    stringsAsFactors = FALSE
  )

  for (var_name in labeled_vars) {
    var <- justice_decision[[var_name]]
    labels <- val_labels(var)
    actual_values <- unique(var[!is.na(var)])
    expected_values <- unname(labels)

    all_within <- all(actual_values %in% expected_values)

    summary_table <- rbind(summary_table, data.frame(
      variable = var_name,
      n_labels = length(labels),
      n_unique_values = length(actual_values),
      all_labeled = ifelse(all_within, "✓", "✗"),
      stringsAsFactors = FALSE
    ))
  }

  print(summary_table)
}





#save the processed data in raw data folder
haven::write_sav(justice_decision, path="data-raw/justice_decision.sav")
haven::write_dta(justice_decision, path="data-raw/justice_decision.dta")



all_vars <- names(justice_decision)
labeled_vars <- character()

for (var_name in all_vars) {
  var <- justice_decision[[var_name]]
  if (!is.null(val_labels(var)) && length(val_labels(var)) > 0) {
    labeled_vars <- c(labeled_vars, var_name)
  }
}

# Convert each to factor using the labels
for (var_name in labeled_vars) {
  justice_decision[[var_name]] <- to_factor(justice_decision[[var_name]])
  cat("✓ Converted:", var_name, "\n")
}

save(justice_decision, file="data-raw/justice_decision.rda")
writexl::write_xlsx(justice_decision, path="data-raw/justice_decision.xlsx")

# save a copy of the raw data for the package
usethis::use_data(justice_decision, overwrite = TRUE)

