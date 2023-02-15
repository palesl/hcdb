## code to prepare `Special Leave 2003-2018 [PAT THIS ONE].xlsx` dataset goes here
# input from shared folder and storage in Raw ####
library(readxl)


# save a copy of the excel file in data-raw

file.copy(from="/Users/u1040068/Dropbox/High Court Project/Special Leave DP22/Special Leave 2003-2018 [PAT THIS ONE].xlsx",
          to="data-raw/Special Leave 2003-2018 [PAT THIS ONE].xlsx",
          overwrite = T)

# opening from the raw folder

special_leave <- read_excel("data-raw/Special Leave 2003-2018 [PAT THIS ONE].xlsx")

#cleaning justice decision data ####



library(dplyr)


#multipleMatters

#install.packages("labelled")
library(labelled)

special_leave$multipleMatters|>unique()|>sort()

val_labels(special_leave$multipleMatters)<-c(No=1,Yes=2)
special_leave$multipleMatters

# caseType

special_leave$caseType|>unique()|>sort()


val_labels(special_leave$caseType) <- c(Immigration=1,
                                        Criminal=2,
                                        Civil=3,
                                        Unknown=999)
special_leave$caseType


# issue areas

issue_areas<-matrix(c('Common Law'   ,1,
                      'Public Law—Federal' ,2,
                      'Public Law—State' ,3,
                      'Civil Rights (non-constitutional)' ,4,
                      'Criminal Law and Procedure' ,5,
                      'Economic Relations' ,6,
                      'Employment and Industrial Relations' ,7,
                      'Admiralty and Maritime' ,8,
                      'Procedure and Ethics' ,9,
                      'Miscellaneous' ,10,
                      'Costs' ,11,
                      "Unknown", 999), ncol=2, byrow=T)|>as_data_frame()

names(issue_areas)<- c('labels', 'codes')
issue_areas$codes<-as.numeric(issue_areas$codes)

special_leave$primaryIssueArea|>unique()|>sort()
special_leave$secondaryIssueArea|>unique()|>sort()

issue_areas_vec <- issue_areas$codes
names(issue_areas_vec)<-issue_areas$labels

val_labels(special_leave$primaryIssueArea)<- issue_areas_vec
special_leave$primaryIssueArea

val_labels(special_leave$secondaryIssueArea)<- issue_areas_vec
special_leave$secondaryIssueArea



# issue sub areas

issue_sub_areas<-matrix(c('Tort' ,101,
                          'Contract' ,102,
                          'Equity' ,103,
                          'Trusts' ,104,
                          'Constitutional law' ,201,
                          'Administrative law' ,202,
                          'Constitutional law' ,301,
                          'Administrative law' ,302,
                          'Statutory rights' ,401,
                          'State bills of rights' ,402,
                          'Common law rights' ,403,
                          'Indigenous rights (including native title)' ,404,
                          'Refugees' ,405,
                          'Federal criminal law' ,501,
                          'Federal criminal procedure' ,502,
                          'State criminal law' ,503,
                          'State criminal procedure' ,504,
                          'Corporate and business' ,601,
                          'Bankruptcy and insolvency' ,602,
                          'Property' ,603,
                          'Intellectual property' ,604,
                          'Consumer and competition' ,605,
                          'Taxation' ,606,
                          'Succession (wills and estates)' ,607,
                          'Employment and industrial relations' ,701,
                          'Admiralty and maritime' ,801,
                          'Civil procedure/litigation' ,901,
                          'Evidence' ,902,
                          'Statutory interpretation (Acts Interpretation Act)' ,903,
                          'Legal profession (ethics)' ,904,
                          'Inherent power of the Court' ,905,
                          'International law' ,1001,
                          'Family law' ,1002,
                          'Migration (non-refugee)' ,1003,
                          'Environmental law' ,1004,
                          'Vulnerable persons (e.g. child protection, disabled persons etc)' ,1005,
                          'Costs' ,1101,
                          'Unknown' ,999), ncol=2, byrow=T)|>as_data_frame()

special_leave$primaryIssueSubArea|>unique()|>sort()
special_leave$secondaryIssueSubArea|>unique()|>sort()


names(issue_sub_areas)<- c('labels', 'codes')
issue_sub_areas$codes<-as.numeric(issue_sub_areas$codes)

issue_sub_areas_vec <- issue_sub_areas$codes
names(issue_sub_areas_vec)<-issue_sub_areas$labels

val_labels(special_leave$primaryIssueSubArea)<- issue_sub_areas_vec
special_leave$primaryIssueSubArea

val_labels(special_leave$secondaryIssueSubArea)<- issue_sub_areas_vec
special_leave$secondaryIssueSubArea

# jurisdiction general

juris_g<-matrix(c(1, 'special leave',
                  2,'appeal as of right' ,
                  3 , 'original jurisdiction' ,
                  4 ,'reference' ,
                  5 , 'removal',
                  6 , 'other',
                  999, "unknown"), byrow = T, ncol=2)|>as_data_frame()

names(juris_g)<- c( 'codes','labels')

juris_g$codes<-as.numeric(juris_g$codes)

special_leave$jurisdictionGeneral|>unique()

juris_g_vec <- juris_g$codes
names(juris_g_vec)<-juris_g$labels
val_labels(special_leave$jurisdictionGeneral)<- juris_g_vec
special_leave$jurisdictionGeneral


# Jurisdiction specific

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
                  999, "unknown"), byrow = T, ncol=2)|>as_data_frame()

names(juris_s)<- c( 'codes','labels')

juris_s$codes<-as.numeric(juris_s$codes)

special_leave$jurisdictionSpecific|>unique()

juris_s_vec <- juris_s$codes
names(juris_s_vec)<-juris_s$labels
val_labels(special_leave$jurisdictionSpecific)<- juris_s_vec
special_leave$jurisdictionSpecific


# case_source_general

case_source_general <-matrix(c( 1 , 'Federal court—trial level',
                                2 , 'Federal court—appellate level',
                                3 , 'State supreme court—trial level',
                                4 , 'State supreme court—appellate level',
                                5 , 'State district court (county court)',
                                6 , 'State local court (magistrates court)',
                                7 , 'State speciality court',
                                999, "unknown"), byrow = T, ncol=2)|>as_data_frame()

names(case_source_general)<- c( 'codes','labels')

case_source_general$codes<-as.numeric(case_source_general$codes)

special_leave$caseSourceGeneral|>unique()

case_source_general_vec <- case_source_general$codes
names(case_source_general_vec)<-case_source_general$labels
val_labels(special_leave$caseSourceGeneral)<- case_source_general_vec
special_leave$caseSourceGeneral

# case_source_specific

case_source_specific <-matrix(c(1, 'Family Court of Australia—Single Judge',
                                2, 'Federal Magistrates Court',
                                3, 'Federal Circuit Court of Australia',
                                4, 'Federal Court of Australia—Single Judge',
                                5, 'Federal Court of Australia—Full Court',
                                6, 'Australian Industrial Court',
                                7, 'Australia Military Court',
                                8, 'Commonwealth Court of Conciliation and Arbitration',
                                9, 'Commonwealth Industrial Court',
                                10, 'Federal Court of Bankruptcy',
                                11, 'Industrial Relations Court of Australia',
                                12, 'Supreme Court of New South Wales—Common Law Division',
                                13, 'Supreme Court of New South Wales—Equity Division',
                                14, 'Supreme Court of New South Wales—Court of Appeal',
                                15, 'Supreme Court of New South Wales—Court of Criminal Appeal',
                                16, 'Land and Environment Court of New South Wales',
                                17, 'District Court of New South Wales',
                                18, 'Local Court of New South Wales',
                                19, 'New South Wales Industrial Court',
                                20, 'The Children’s Court of New South Wales',
                                21, 'New South Wales Dust Diseases Tribunal',
                                22, 'Drug Court of New South Wales',
                                23, 'New South Wales Chief Industrial Magistrate’s Court',
                                24, 'New South Wales Coroner’s Court',
                                25, 'Supreme Court of Queensland—Court of Appeal',
                                26, 'Supreme Court of Queensland—Trial Division (Criminal)',
                                27, 'Supreme Court of Queensland—Trial Division (Civil)',
                                28, 'Industrial Court of Queensland',
                                29, 'Queensland Planning and Environment Court',
                                30, 'Queensland Land Appeal Court',
                                31, 'District Court of Queensland',
                                32, 'Magistrates Court of Queensland',
                                33, 'Queensland Children’s Court',
                                34, 'Queensland Mental Health Court',
                                35, 'Queensland Murri Court',
                                36, 'Queensland Domestic Violence Court',
                                37, 'Queensland Drug and Alcohol Court',
                                38, 'Queensland Coroner’s Court',
                                39, 'Supreme Court of Victoria—Court of Appeals',
                                40, 'Supreme Court of Victoria—Trial Court Common Law Division',
                                41, 'Supreme Court of Victoria—Trial Court Commercial Division',
                                42, 'Supreme Court of Victoria—Trial Court Criminal Division',
                                43, 'County Court of Victoria',
                                44, 'Magistrates’ Court of Victoria',
                                45, 'Children’s Court of Victoria',
                                46, 'Victorian Coroner’s Court',
                                47, 'Supreme Court of South Australia—Trial Division',
                                48, 'Supreme Court of South Australia—Criminal Appeal Division',
                                49, 'Supreme Court of South Australia—Civil Appeal Division',
                                50, 'District Court of South Australia',
                                51, 'Magistrates Court of South Australia',
                                52, 'Environment, Resources, and Development Court of South Australia',
                                53, 'South Australia Industrial Relations Court',
                                54, 'Wardens Court of South Australia',
                                55, 'Youth Court of South Australia',
                                56, 'South Australia Coroner’s Court',
                                57, 'Supreme Court of Western Australia—Court of Appeals',
                                58, 'Supreme Court of Western Australia—General (Trial) Division',
                                59, 'Family Court of Western Australia',
                                60, 'District Court of Western Australia',
                                61, 'Magistrates Court of Western Australia',
                                62, 'Aboriginal Community Court of Western Australia',
                                63, 'Children’s Court of Western Australia',
                                64, 'Drug Court of Western Australia',
                                65, 'Geraldton Family Violence Court, Western Australia',
                                66, 'Western Australia Coroner’s Court',
                                67, 'Supreme Court of Tasmania—Criminal Division',
                                68, 'Supreme Court of Tasmania—Civil Division',
                                69, 'Supreme Court of Tasmania—Court of Appeals',
                                70, 'Magistrates Court of Tasmania',
                                71, 'Tasmanian Coroner’s Court',
                                72, 'Supreme Court of the Northern Territory—Court of Appeal',
                                73, 'Supreme Court of the Northern Territory—Court of Criminal Appeal',
                                74, 'Supreme Court of the Northern Territory—Civil Trial',
                                75, 'Supreme Court of the Northern Territory—Criminal Trial',
                                76, 'Northern Territory Local Court (Magistrate’s Court)',
                                77, 'Coroner’s Court of the Northern Territory',
                                78, 'Supreme Court of the Australian Capital Territory',
                                79, 'Supreme Court of the Australian Capital Territory—Court of Appeal',
                                80, 'Magistrates Court of the Australian Capital Territory',
                                81, 'Coroner’s Court of the Australian Capital Territory',
                                82, 'Supreme Court of Norfolk Island',
                                83, 'Court of Petty Sessions for Norfolk Island',
                                84, 'Nauru Supreme Court',
                                85, 'Other',
                                86, 'Family Court of Australia—Full Court',
                                87, 'Warden’s Court of Western Australia',
                                88, 'Nauru District Court',
                                999, "Unknown"), byrow = T, ncol=2)|>as_data_frame()


names(case_source_specific)<- c( 'codes','labels')

case_source_specific$codes<-as.numeric(case_source_specific$codes)

special_leave$caseSourceSpecific|>unique()|>sort()

case_source_specific_vec <- case_source_specific$codes
names(case_source_specific_vec)<-case_source_specific$labels
val_labels(special_leave$caseSourceSpecific)<- case_source_specific_vec
special_leave$caseSourceSpecific

# case_source_state


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
                             999, "Unknown"), byrow = T, ncol=2)|>as_data_frame()


names(case_source_state)<- c( 'codes','labels')

case_source_state$codes<-as.numeric(case_source_state$codes)

special_leave$caseSourceState|>unique()|>sort()

case_source_state_vec <- case_source_state$codes
names(case_source_state_vec)<-case_source_state$labels
val_labels(special_leave$caseSourceState)<- case_source_state_vec
special_leave$caseSourceState

#lcDisposition

lcd<- matrix(c(1, 'Appeal/application allowed',
               2, 'Appeal/application allowed, order below set aside and/or varied',
               3, 'Appeal/application allowed, order below set aside and/or varied in part',
               4, 'Appeal/application allowed, order below set aside and/or varied, matter remitted',
               5, 'Appeal/application allowed, order below set aside and/or varied in part, matter remitted',
               6, 'Appeal/application dismissed',
               7, 'Appeal/application allowed, remit for further determination',
               8, 'Appeal/application allowed in part, remit for further determination',
               9, 'Appeal/application allowed in part, order below set aside and/or varied',
               10, 'Appeal/application allowed in part, order below set aside and/or varied in part',
               11, 'Appeal/application allowed in part, order below set aside and/or varied, matter remitted',
               12, 'Appeal/application allowed in part, order below set aside and/or varied in part, matter remitted',
               13, 'Special leave revoked',
               14, 'Appeal granted and appeal allowed instantia',
               15, 'Other disposition',
               16, 'Questions answered (special case, case referred, etc)',
               17, 'Special leave denied by enlarged bench',
               999, "Unknown"), byrow = T, ncol=2)|>as_data_frame()

names(lcd)<- c( 'codes','labels')

lcd$codes<-as.numeric(lcd$codes)



special_leave$lcDisposition|>unique()|>sort()

lcd_vec <- lcd$codes
names(lcd_vec)<-lcd$labels
val_labels(special_leave$lcDisposition)<- lcd_vec
special_leave$lcDisposition


#lc_dissent

lc_dissent<-matrix(c(1, 'No',
                     2, 'Yes',
                     999, "Unknown"), byrow = T, ncol=2)|>as_data_frame()

names(lc_dissent)<- c( 'codes','labels')

lc_dissent$codes<-as.numeric(lc_dissent$codes)

special_leave$lcDissent|>unique()|>sort()

lc_dissent_vec <- lc_dissent$codes
names(lc_dissent_vec)<-lc_dissent$labels

val_labels(special_leave$lcDissent)<- lc_dissent_vec

special_leave$lcDissent

# 33.	Special Leave Method


special_leave$specialLeaveMethod|>unique()|>sort()


val_labels(special_leave$specialLeaveMethod) <- c(`Oral Hearing`=1,
                                                  Papers=2,
                                                  Unknown=999)
special_leave$specialLeaveMethod


#Justice Names

justiceID<- 55:33
justiceName<-c('JGleeson', 'Steward', 'Edelman',
               'Gordon','Nettle', 'Keane',
               'Gageler','Bell','French',
               'Kiefel','Crennan','Heydon',
               'MGleeson','Callinan','Hayne',
               'Kirby','Gummow','McHugh',
               'Gaudron','Toohey','Dawson',
               'Deane','Brennan')

names<-bind_cols(justiceID=justiceID,
                 justiceName=justiceName)

names_vec <- names$justiceID
names(names_vec)<-names$justiceName

special_leave$justiceSpecialLeave1|>unique()|>sort()
special_leave$justiceSpecialLeave2|>unique()|>sort()
special_leave$justiceSpecialLeave3|>unique()|>sort()


val_labels(special_leave$justiceSpecialLeave1)<- names_vec
special_leave$justiceSpecialLeave1

val_labels(special_leave$justiceSpecialLeave2)<- names_vec
special_leave$justiceSpecialLeave2

val_labels(special_leave$justiceSpecialLeave3)<- names_vec
special_leave$justiceSpecialLeave3


# Special leave location

sll<-c(Adelaide=1,
       Brisbane=2,
       Canberra=3,
       Darwin=4,
       Hobart=5,
       Melbourne=6,
       Perth=7,
       Sydney=8)

unique(special_leave$specialLeaveLocation)|>sort()

val_labels(special_leave$specialLeaveLocation)<-sll
special_leave$specialLeaveLocation

# special leave outcome


slo<- c(`Special leave granted` = 1,
        `Special leave referred to full court` =2,
        `Appeal and special leave determined concurrently`=3,
        `Special leave refused`=4,
        `Special leave refused with costs`=5)

unique(special_leave$specialLeaveOutcome)|>sort()

val_labels(special_leave$specialLeaveOutcome)<- slo

special_leave$specialLeaveOutcome

# natural court special leave

nc_names<- data.frame(
  nc=c(1001:1004,1101:1104,1201:1206,1301:1304),
  nc_name=c("Brennan Court 1","Brennan Court 2",
            "Brennan Court 3","Brennan Court 4",

            "Gleeson Court 1","Gleeson Court 2",
            "Gleeson Court 3","Gleeson Court 4",

            "French Court 1","French Court 2",
            "French Court 3","French Court 4",
            "French Court 5","French Court 6",

            "Kiefel Court 1","Kiefel Court 2",
            "Kiefel Court 3","Kiefel Court 4")
)

nc<-nc_names$nc
names(nc)<-nc_names$nc_name


unique(special_leave$naturalCourtSpecialLeave)|>sort()

val_labels(special_leave$naturalCourtSpecialLeave)<-nc
special_leave$naturalCourtSpecialLeave

# chief justice special leave
cj<-c(Griffith =1,
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
      Kiefel=13)

special_leave$chiefSL|>unique()|>sort()

val_labels(special_leave$chiefSL)<-cj

special_leave$chiefSL

#PM special leave

pms<-c(`Barton (01.01.1901 - 24.09.1903)`=1,
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
       `Albanese (23.05.2022 - present)`=37)

pms

special_leave$pmSL|>unique()|>sort()

val_labels(special_leave$pmSL)<-pms

special_leave$pmSL

# Power in parl

power<-c(`Coalition House/Coalition Senate`=1,
         `Coalition House/Not Coalition Senate`=2,
         `Labour House/Labour Senate`=3,
         `Labour House/Not Labour Senate`=4,
         `Neither/Neither`=5)


special_leave$politicalPowerSL|>unique()|>sort()

val_labels(special_leave$politicalPowerSL)<-power

special_leave$politicalPowerSL

# dates
library(lubridate)

special_leave$dateLcDecision[special_leave$dateLcDecision==999]<-NA
special_leave$dateLcDecision|>unique()|>sort()
special_leave$dateLcDecision<-ymd(special_leave$dateLcDecision)

special_leave$specialLeaveDate<-special_leave$specialLeaveDate |>as.Date()

# saving ####

special_leave<-as_tibble(special_leave)

#save the processed data in raw data folder
save(special_leave, file="data-raw/special_leave.rda")

# save a copy of the raw data for the package
usethis::use_data(special_leave, overwrite = TRUE)

