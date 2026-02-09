## code to prepare `justice_bio_November_7_2021` dataset goes here
# input from shared folder and storage in Raw ####
library(readxl)
justice_bio <- read_excel("data-raw/justice_bio_Feb_2023.xlsx",
                                          skip = 2)

# save a copy of the excel file in data-raw

# file.copy(from="~/Dropbox/High Court Project/Data Files/CURRENT DATASETS/justice_bio_Feb_2023.xlsx",
#           to="data-raw/justice_bio_Feb_2023.xlsx",
#           overwrite = T)

#cleaning justice decision data ####


#adding names...
library(dplyr)
library(labelled)


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

justice_bio<-names|>left_join(justice_bio)


#cleaning missing value
justice_bio[justice_bio==999]<-NA
justice_bio[justice_bio==888]<-NA


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


# going through and labelling variables...



# Identification Variables	5
# 1.	Justice ID	5

# 2.	Date First Nomination Announced	7

# 3.	Position to which First Nominated to Court	7
varPosition <- c(`Chief Justice` = 1,
                  `Puisne Justice` = 2)

justice_bio$position|>unique()|>sort()

val_labels(justice_bio$position)<- varPosition


# 4.	Date of Swearing In Ceremony	7
# 5.	Home Registry	8
varRegistry<-c(`Adelaide` = 1,
               `Brisbane` = 2,
               `Canberra` = 3,
               `Darwin` = 4,
               `Hobart` = 5,
               `Melbourne` = 6,
               `Perth` = 7,
               `Sydney` = 8,
               `Not Applicable`=999)

justice_bio$homeRegistry|>unique()|>sort()

val_labels(justice_bio$homeRegistry)<- varRegistry

# 6.	Seat Identification Number	9


# 7.	Justice who Nominee Replaced	9

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

justice_bio$justiceReplaced|>unique()|>sort()

val_labels(justice_bio$justiceReplaced)<- varJustice


# 8.	Chief Justice When Nomination Announced	10
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

justice_bio$chiefAnnounce|>unique()|>sort()

val_labels(justice_bio$chiefAnnounce)<- varChief

# 9.	Chief Justice When Sworn In	11
justice_bio$chiefSworn|>unique()|>sort()

val_labels(justice_bio$chiefSworn)<- varChief


# 10.	Nominating Prime Minister	11


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


justice_bio$nomPM|>unique()|>sort()

val_labels(justice_bio$nomPM)<- varPrimeMinister

# 11.	Nominating Prime Minister Party	12

varPolParty<- c(Liberal=1,
                  Labor=2,
                  Country=3,
                  `United Australia`=4,
                  Nationalist=5,
                  Protectionist=6,
                  `Commonwealth Liberal`=7,
                  `Free Trade`=8)

justice_bio$nomPMParty|>unique()|>sort()

val_labels(justice_bio$nomPMParty)<- varPolParty


# 12.	Political Power on Date of Nomination	13

varPoliticalPower <-c(`Coalition House/Coalition Senate`=1,
                      `Coalition House/Not Coalition Senate`=2,
                      `Labour House/Labour Senate`=3,
                      `Labour House/Not Labour Senate`=4,
                      `Minority Labour House/Not Labour Senate`=5,
                      `Not Applicable` = 999)

justice_bio$politicalPowerDateNom|>unique()|>sort()

val_labels(justice_bio$politicalPowerDateNom)<- varPoliticalPower

# 13.	Prime Minister on Date of Swearing In	13


justice_bio$PMSwearingIn|>unique()|>sort()

val_labels(justice_bio$PMSwearingIn)<- varPrimeMinister

# 14.	Prime Minister Party on Date of Swearing In	14

justice_bio$PMPartySwearingIn|>unique()|>sort()

val_labels(justice_bio$PMPartySwearingIn)<- varPolParty


# 15.	Political Power on Date of Swearing In Ceremony	15
justice_bio$politicalPowerSwearingIn|>unique()|>sort()

val_labels(justice_bio$politicalPowerSwearingIn)<- varPoliticalPower

# 16.	Date of Nomination of Sitting Puisne Justice to Chief Justice	16


# 17.	Nominating Prime Minister to Chief Justice from Puisne Justice	16
justice_bio$nomPMPromotion|>unique()|>sort()
val_labels(justice_bio$nomPMPromotion)<- varPrimeMinister


# 18.	Nominating Prime Minister Party for Chief Justice from Puisne Justice	17
justice_bio$nomPMPartyPromotion|>unique()|>sort()
val_labels(justice_bio$nomPMPartyPromotion)<- varPolParty


# 19.	Political Power Variable for Chief Justice from Puisne Justice on Date of Nomination	17
justice_bio$politicalPowerPromotion|>unique()|>sort()
val_labels(justice_bio$politicalPowerPromotion)<- varPoliticalPower

# 20.	Year of Appointment as Chief Justice from Puisne Justice	18
# 21.	Date of Swearing In Ceremony as Chief Justice from Puisne Justice	18
# 22.	Who Puisne Justice Replaced as Chief Justice	18

justice_bio$replacementCJPromotion|>unique()|>sort()
val_labels(justice_bio$replacementCJPromotion)<- varChief

# 23.	Ideology Score	19

# Personal Background Variables	19

# 24.	Year of Birth	19

# 25.	Exact Date of Birth	19

# 26.	State or Country of Birth	20

varBirthPlace<-c(`Australian Capital Territory`=1,
                 `New South Wales`=2,
                 `Northern Territory`=3,
                 `Queensland` =4,
                 `South Australia`=5,
                 Tasmania=6,
                 Victoria=7,
                 `Western Australia`=8,
                 `Overseas, Canada`=9,
                 `Overseas, UK`=10,
                 `Overseas, USA`=11)

justice_bio$placeBirthGen|>unique()|>sort()
val_labels(justice_bio$placeBirthGen)<- varBirthPlace

# 27.	Religion—General	20

varGenReligion<-c(Christian=1,
                  `Not Christian`=2)

justice_bio$religionGeneral|>unique()|>sort()
val_labels(justice_bio$religionGeneral)<- varGenReligion


# 28.	Religion—Specific	20

varReligionSpecific<-c(`Christian—Anglican` = 1,
                       `Christian—Baptist` = 2,
                       `Christian—Brethren` = 3,
                       `Christian—Catholic` = 4,
                       `Christian—Churches of Christ` = 5,
                       `Christian—Jehovah’s Witnesses` = 6,
                       `Christian—Latter-day Saints` = 7,
                       `Christian—Lutheran` = 8,
                       `Christian—Oriental Orthodox` = 9,
                       `Christian—Assyrian Apostolic` = 10,
                       `Christian—Eastern Orthodox` = 11,
                       `Christian—Presbyterian and Reformed` = 12,
                       `Christian—Salvation Army` = 13,
                       `Christian—Seventh-day Adventist` = 14,
                       `Christian—Uniting Church` = 15,
                       `Christian—Pentecostal` = 16,
                       `Other Christian` = 17,
                       `Hinduism` = 18,
                       `Islam` = 19,
                       `Judaism` = 20,
                       `Australian Aboriginal Traditional Religions` = 21,
                       `Baha'i` = 22,
                       `Chinese Religions` = 23,
                       `Druse` = 24,
                       `Japanese Religions` = 25,
                       `Nature Religions` = 26,
                       `Sikhism` = 27,
                       `Spiritualism` = 28,
                       `Miscellaneous Religions` = 29,
                       `No Religion, so described` = 30,
                       `Secular Beliefs` = 31,
                       `Other Spiritual Beliefs` = 32)

justice_bio$religionSpecific|>unique()|>sort()
val_labels(justice_bio$religionSpecific)<- varReligionSpecific

# 29.	Nominee’s Race	21


varRace<-c(`White` = 1,
          `Asian` = 2,
          `Indigenous` = 3,
          `Black` = 4)


justice_bio$race|>unique()|>sort()
val_labels(justice_bio$race)<- varRace

# 30.	Nominee’s Gender	22

varGender<-c(Male=1, Female=2)
justice_bio$gender|>unique()|>sort()

val_labels(justice_bio$gender)<- varGender


# 31.	Year of Death	22


# 32.	Exact Date of Death	22

# 33.	Age at Date of Death	22

# 34.	Year Departed Court	22

# 35.	Exact Date of Departure from Court	23

# 36.	Age on Date of Departure	23

# 37.	Reason for Departing Court	23

# 38.	Resignation Party	23

justice_bio$resignationParty|>unique()|>sort()

val_labels(justice_bio$resignationParty)<- varPolParty


# 39.	Prime Minister on Date of Departure	24

justice_bio$PMDeparture|>unique()|>sort()

val_labels(justice_bio$PMDeparture)<- varPrimeMinister

# 40.	Prime Minister Party on Date of Departure	25

justice_bio$PMPartyDeparture|>unique()|>sort()
val_labels(justice_bio$PMPartyDeparture)<- varPolParty

# 41.	Political Power on Date Retirement Announced	25

justice_bio$politicalPowerRetirementAnnounce|>unique()|>sort()
val_labels(justice_bio$politicalPowerRetirementAnnounce)<- varPoliticalPower

# 42.	Position Held by Justice After Departure from Court 1	26
varPostPosition<-c(`Barrister` = 1,
                   `Academic (honorary)` = 2,
                   `Academic (faculty)` = 3,
                   `Academic (administrative, e.g., VC)` = 4,
                   `Foreign judicial service` = 5,
                   `International judicial service` = 6,
                   `Commissioner (Cth)` = 7,
                   `Commissioner (state)` = 8,
                   `Director/chair of foundation or similar organization` = 9,
                   `Governor-General` = 10,
                   `Other government service (Cth)` = 11,
                   `Other government service (state)` = 12)


justice_bio$positionAfterDeparture1|>unique()|>sort()
val_labels(justice_bio$positionAfterDeparture1)<- varPostPosition

# 43.	Party Appointing to Position Held by Justice After Departure from Court 1	26
justice_bio$positionAfterDepartureParty1|>unique()|>sort()
val_labels(justice_bio$positionAfterDepartureParty1)<- varPolParty

# 44.	Position Held by Justice After Departure from Court 2	26

justice_bio$positionAfterDeparture2|>unique()|>sort()
val_labels(justice_bio$positionAfterDeparture2)<- varPostPosition

# 45.	Party Appointing to Position Held by Justice After Departure from Court 2	27
justice_bio$positionAfterDepartureParty2|>unique()|>sort()
val_labels(justice_bio$positionAfterDepartureParty2)<- varPolParty
# 46.	Position Held by Justice After Departure from Court 3	27
justice_bio$positionAfterDeparture3|>unique()|>sort()
val_labels(justice_bio$positionAfterDeparture3)<- varPostPosition
# 47.	Party Appointing to Position Held by Justice After Departure from Court 3	27
justice_bio$positionAfterDepartureParty3|>unique()|>sort()
val_labels(justice_bio$positionAfterDepartureParty3)<- varPolParty

# 48.	Position Held by Justice After Departure from Court 4	27
justice_bio$positionAfterDeparture4|>unique()|>sort()
val_labels(justice_bio$positionAfterDeparture4)<- varPostPosition
# 49.	Party Appointing to Position Held by Justice After Departure from Court 4	28
justice_bio$positionAfterDepartureParty4|>unique()|>sort()
val_labels(justice_bio$positionAfterDepartureParty4)<- varPolParty

# 50.	Childhood Location—General (Country or State)	28
varChildhoodLocGen<-c(`Australian Capital Territory` = 1,
                      `New South Wales` = 2,
                      `Northern Territory` = 3,
                      `Queensland ` = 4,
                      `South Australia` = 5,
                      `Tasmania` = 6,
                      `Victoria` = 7,
                      `Western Australia` = 8,
                      `UK` = 9,
                      `USA` = 10)

justice_bio$childhoodLocationGeneral|>unique()|>sort()
val_labels(justice_bio$childhoodLocationGeneral)<- varChildhoodLocGen


# 51.	Childhood Location—Specific (City or Town)	28

varChildhoodLocSpecific<-c(`Capital city` = 1,
                           `Town` = 2)
justice_bio$childhoodLocationSpecific|>unique()|>sort()

val_labels(justice_bio$childhoodLocationSpecific)<- varChildhoodLocSpecific


# 52.	Childhood Surrounds	29
varChildhoodSurrounds<-c(`Family farm` = 1,
                         `Rural` = 2,
                         `Small town` = 3,
                         `Small city` = 4,
                         `Urban (large/larger city)` = 5)

justice_bio$childhoodSurrounds|>unique()|>sort()
val_labels(justice_bio$childhoodSurrounds)<- varChildhoodSurrounds


# Family Information	29


# 53.	Childhood Economic Status	29
varEconStatus<-c(`Lower` = 1,
                 `Lower-middle` = 2,
                 `Middle` = 3,
                 `Upper-middle` = 4,
                 `Upper` = 5)

justice_bio$economicStatus|>unique()|>sort()
val_labels(justice_bio$economicStatus)<- varEconStatus

# 54.	Primary Occupation of Father	29
varOccupation<-c(`Homemaker` = 1,
                 `Politics` = 2,
                 `Public service` = 3,
                 `Professional, employee` = 4,
                 `Military` = 5,
                 `Professional, self-employed` = 6,
                 `Blue collar` = 7,
                 `Lawyer` = 8,
                 `Academic` = 9,
                 `Teacher` = 10,
                 `Administrative role` = 11)
justice_bio$occupationFather|>unique()|>sort()
val_labels(justice_bio$occupationFather)<- varOccupation

# 55.	Political Affiliation Father	30

justice_bio$politicalFather|>unique()|>sort()
val_labels(justice_bio$politicalFather)<- varPolParty


# 56.	Primary Occupation of Mother	30

justice_bio$occupationMother|>unique()|>sort()
val_labels(justice_bio$occupationMother)<- varOccupation

# 57.	Political Affiliation Mother	30
justice_bio$politicalMother|>unique()|>sort()
val_labels(justice_bio$politicalMother)<- varPolParty
# 58.	Primary Occupation of Spouse	31
justice_bio$occupationSpouse|>unique()|>sort()
val_labels(justice_bio$occupationSpouse)<- varOccupation

# 59.	Political Affiliation Spouse	31
justice_bio$politicalSpouse|>unique()|>sort()
val_labels(justice_bio$politicalSpouse)<- varPolParty

# 60.	Number of Marriages	31
# 61.	Date of Last Marriage	31
# 62.	Number of Children	31
# Educational Background Variables	32
# 65.	Type of Secondary Education	32

varHighSchool<-c(`Public ` = 1,
                 `Private—coeducational` = 2,
                 `Private—single sex ` = 3)

justice_bio$secondaryEducationType|>unique()|>sort()
val_labels(justice_bio$secondaryEducationType)<- varHighSchool

# 66.	First Degree	32

varDegreeType<-c(`Bachelor of Laws` = 1,
                 `Bachelor of Arts` = 2,
                 `Bachelor of Economics` = 3,
                 `Bachelor of Commerce` = 4,
                 `Bachelor of Jurisprudence` = 5,
                 `Bachelor of Science` = 6)

justice_bio$firstDegree|>unique()|>sort()
val_labels(justice_bio$firstDegree)<- varDegreeType

# 67.	First Degree Institution	33

varUni<-c(`Australian National University` = 1,
          `Barristers Admission Board` = 2,
          `University of Melbourne` = 3,
          `University of Queensland` = 4,
          `University of Sydney` = 5,
          `University of Western Australia` = 6,
          `Murdoch University` = 7,
          `Oxford` = 8)

justice_bio$firstDegreeInstitution|>unique()|>sort()
val_labels(justice_bio$firstDegreeInstitution)<- varUni

# 68.	First Degree Status	33

varDegreeStatus<-c(`Completed` = 1,
                   `Not completed` = 2)

justice_bio$firstDegreeStatus|>unique()|>sort()
val_labels(justice_bio$firstDegreeStatus)<- varDegreeStatus

# 69.	First Degree Year Started	33
# 70.	First Degree Year Completed	34

# 71.	Second Degree	34
justice_bio$secondDegree|>unique()|>sort()
val_labels(justice_bio$secondDegree)<- varDegreeType
# 72.	Second Degree Institution	34
justice_bio$secondDegreeInstitution|>unique()|>sort()
val_labels(justice_bio$secondDegreeInstitution)<- varUni

# 73.	Second Degree Status	34
justice_bio$secondDegreeStatus|>unique()|>sort()
val_labels(justice_bio$secondDegreeStatus)<- varDegreeStatus
# 74.	Second Degree Year Started	34
# 75.	Second Degree Year Completed	35
# 76.	Third Degree	35
justice_bio$thirdDegree|>unique()|>sort()
val_labels(justice_bio$thirdDegree)<- varDegreeType
# 77.	Third Degree Institution	35
justice_bio$thirdDegreeInstitution|>unique()|>sort()
val_labels(justice_bio$secondDegreeInstitution)<- varUni
# 78.	Third Degree Status	35
justice_bio$thirdDegreeStatus|>unique()|>sort()
val_labels(justice_bio$thirdDegreeStatus)<- varDegreeStatus
# 79.	Third Degree Year Started	35
# 80.	Third Degree Year Completed	36
# 81.	Name of Law School	36

justice_bio$nameLawSchool|>unique()|>sort()
val_labels(justice_bio$nameLawSchool)<- varUni
# 82.	Year of Law School	36
# 83.	Academic Honours Medal	36
varAcMedal<-c(`Law` = 1,
              `Non-law` = 2,
              `Law and non-law (multiple)` = 3,
              `None recorded` = 4)

justice_bio$academicHonoursMedal|>unique()|>sort()
val_labels(justice_bio$academicHonoursMedal)<- varAcMedal

# 84.	Academic Honours First Class Honours	37
justice_bio$academicHonoursFirstClass|>unique()|>sort()
val_labels(justice_bio$academicHonoursFirstClass)<- varAcMedal
# 85.	Academic Honours Overseas Scholarship	37
varOverseasScholarship<-c(`Rhodes` = 1,
                    `Fulbright` = 2,
                    `Other` = 3,
                    `None recorded` = 4)
justice_bio$academicHonoursOverseas|>unique()|>sort()
val_labels(justice_bio$academicHonoursOverseas)<- varOverseasScholarship

# 86.	Overseas Study	37

varOSStudy<-c(`Studied overseas` = 1,
              `Did not study overseas` = 2)

justice_bio$overseasStudy|>unique()|>sort()
val_labels(justice_bio$overseasStudy)<- varOSStudy

# 87.	Overseas Study Location	37
varOSStudyLoc<-c(`UK` = 1,
                 `USA` = 2,
                 `Other` = 3)

justice_bio$overseasStudyLocation|>unique()|>sort()
val_labels(justice_bio$overseasStudyLocation)<- varOSStudyLoc


# 88.	First Graduate Degree	38

varGraduateDegree<-c(`Doctor of Philosophy` = 1,
                     `Master of Laws` = 2,
                     `BCL` = 3,
                     `Other, law` = 4,
                     `Other, non-law` = 5)
justice_bio$firstGradDegree|>unique()|>sort()
val_labels(justice_bio$firstGradDegree)<- varGraduateDegree

# 89.	First Graduate Degree Institution	38

varGradDegreeInst<-c(`Oxford` = 1,
                     `Cambridge` = 2,
                     `Harvard` = 3,
                     `Yale` = 4,
                     `University of Melbourne` = 5,
                     `University of Sydney` = 6,
                     `Hague` = 7)

justice_bio$firstGradDegreeInstitution|>unique()|>sort()
val_labels(justice_bio$firstGradDegreeInstitution)<- varGradDegreeInst

# 90.	First Graduate Degree Status	38
justice_bio$firstGradDegreeStatus|>unique()|>sort()
val_labels(justice_bio$firstGradDegreeStatus)<- varDegreeStatus


# 91. Second Graduate Degree (secondGradDegree)
justice_bio$secondGradDegree |> unique() |> sort()
val_labels(justice_bio$secondGradDegree) <- varGraduateDegree

# 92. Second Graduate Degree Institution (secondGradDegreeInstitution)
justice_bio$secondGradDegreeInstitution |> unique() |> sort()
val_labels(justice_bio$secondGradDegreeInstitution) <- varGradDegreeInst

# 93. Second Graduate Degree Status (secondGradDegreeStatus)
justice_bio$secondGradDegreeStatus |> unique() |> sort()
val_labels(justice_bio$secondGradDegreeStatus) <- varDegreeStatus

# 94. Third Graduate Degree (thirdGradDegree)
justice_bio$thirdGradDegree |> unique() |> sort()
val_labels(justice_bio$thirdGradDegree) <- varGraduateDegree

# 95. Third Graduate Degree Institution (thirdGradDegreeInstitution)
justice_bio$thirdGradDegreeInstitution |> unique() |> sort()
val_labels(justice_bio$thirdGradDegreeInstitution) <- varGradDegreeInst

# 96. Third Graduate Degree Status (thirdGradDegreeStatus)
justice_bio$thirdGradDegreeStatus |> unique() |> sort()
val_labels(justice_bio$thirdGradDegreeStatus) <- varDegreeStatus

# 97. Last Year Attended Graduate Institution (lastYearAttendedGrad) - numeric, no labels

# Employment Background Variables

# 98. Position When Nominated (postitionWhenNom - note the typo in actual data!)
varPositionNominated <- c(`Chief Justice of Federal Court of Australia` = 1,
                          `Justice of Federal Court of Australia` = 2,
                          `Chief Justice of state supreme court/court of appeals` = 3,
                          `Justice of state supreme court/court of appeals` = 4,
                          `Commonwealth Solicitor General` = 5,
                          `State solicitor general` = 6,
                          `Barrister` = 7,
                          `Solicitor` = 8,
                          `Other` = 9)

justice_bio$postitionWhenNom |> unique() |> sort()
val_labels(justice_bio$postitionWhenNom) <- varPositionNominated

# 99. Position When Nominated Location (positionNomLocation)
varPositionNomLocation <- c(`Australian Capital Territory` = 1,
                            `New South Wales` = 2,
                            `Northern Territory` = 3,
                            `Queensland` = 4,
                            `South Australia` = 5,
                            `Tasmania` = 6,
                            `Victoria` = 7,
                            `Western Australia` = 8,
                            `Federal (Cth SG only)` = 9)

justice_bio$positionNomLocation |> unique() |> sort()
val_labels(justice_bio$positionNomLocation) <- varPositionNomLocation

# 100. Nominated from Judiciary (nomFromJudiciary)
varYesNo <- c(`Yes` = 1,
              `No` = 2)

justice_bio$nomFromJudiciary |> unique() |> sort()
val_labels(justice_bio$nomFromJudiciary) <- varYesNo

# 101. Prior Judicial Service (priorJudicialService)
justice_bio$priorJudicialService |> unique() |> sort()
val_labels(justice_bio$priorJudicialService) <- varYesNo

# 102-106. Months variables - numeric, no labels
# totalNumMonthsJudicialService
# numMonthsStateCourtService
# numMonthsFedCourtService (note: Fed not Federal)
# numMonthsAppCourtService (note: App not Appellate)
# numMonthsTrialCourtService

# 107. Prior Judicial Service 1—General (PriorJudicialService1General - note capital P!)
varPriorJudServiceGen <- c(`Federal court—trial level` = 1,
                           `Federal court—appellate level (FCA)` = 2,
                           `Family Court of Australia` = 3,
                           `State supreme court—trial level` = 4,
                           `State supreme court—appellate level` = 5,
                           `State district court (county court)` = 6,
                           `State local court (magistrates court)` = 7,
                           `State speciality court` = 8)

justice_bio$PriorJudicialService1General |> unique() |> sort()
val_labels(justice_bio$PriorJudicialService1General) <- varPriorJudServiceGen

# 108. Prior Judicial Service 1—Location (priorJudicialService1Location)
varJudServiceLocation <- c(`Australia (federal actor)` = 1,
                           `Australian Capital Territory` = 2,
                           `New South Wales` = 3,
                           `Northern Territory` = 4,
                           `Queensland` = 5,
                           `South Australia` = 6,
                           `Tasmania` = 7,
                           `Victoria` = 8,
                           `Western Australia` = 9)

justice_bio$priorJudicialService1Location |> unique() |> sort()
val_labels(justice_bio$priorJudicialService1Location) <- varJudServiceLocation

# 109. Prior Judicial Service 1—Specific (priorJudicialService1Specific)
varPriorJudServiceSpecific <- c(
  `Family Court of Australia` = 1,
  `Federal Magistrates Court` = 2,
  `Federal Circuit Court of Australia` = 3,
  `Federal Court of Australia` = 5,
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
  `The Children's Court of New South Wales` = 20,
  `New South Wales Dust Diseases Tribunal` = 21,
  `Drug Court of New South Wales` = 22,
  `New South Wales Chief Industrial Magistrate's Court` = 23,
  `New South Wales Coroner's Court` = 24,
  `Supreme Court of Queensland—Court of Appeal` = 25,
  `Supreme Court of Queensland—Trial Division (Criminal)` = 26,
  `Supreme Court of Queensland—Trial Division (Civil)` = 27,
  `Industrial Court of Queensland` = 28,
  `Queensland Planning and Environment Court` = 29,
  `Queensland Land Appeal Court` = 30,
  `District Court of Queensland` = 31,
  `Magistrates Court of Queensland` = 32,
  `Queensland Children's Court` = 33,
  `Queensland Mental Health Court` = 34,
  `Queensland Murri Court` = 35,
  `Queensland Domestic Violence Court` = 36,
  `Queensland Drug and Alcohol Court` = 37,
  `Queensland Coroner's Court` = 38,
  `Supreme Court of Victoria—Court of Appeals` = 39,
  `Supreme Court of Victoria—Trial Court Common Law Division` = 40,
  `Supreme Court of Victoria—Trial Court Commercial Division` = 41,
  `Supreme Court of Victoria—Trial Court Criminal Division` = 42,
  `County Court of Victoria` = 43,
  `Magistrates' Court of Victoria` = 44,
  `Children's Court of Victoria` = 45,
  `Victorian Coroner's Court` = 46,
  `Supreme Court of South Australia—Trial Division` = 47,
  `Supreme Court of South Australia—Criminal Appeal Division` = 48,
  `Supreme Court of South Australia—Civil Appeal Division` = 49,
  `District Court of South Australia` = 50,
  `Magistrates Court of South Australia` = 51,
  `Environment, Resources, and Development Court of South Australia` = 52,
  `South Australia Industrial Relations Court` = 53,
  `Wardens Court of South Australia` = 54,
  `Youth Court of South Australia` = 55,
  `South Australia Coroner's Court` = 56,
  `Supreme Court of Western Australia—Court of Appeals` = 57,
  `Supreme Court of Western Australia—General (Trial) Division` = 58,
  `Family Court of Western Australia` = 59,
  `District Court of Western Australia` = 60,
  `Magistrates Court of Western Australia` = 61,
  `Aboriginal Community Court of Western Australia` = 62,
  `Children's Court of Western Australia` = 63,
  `Drug Court of Western Australia` = 64,
  `Geraldton Family Violence Court, Western Australia` = 65,
  `Western Australia Coroner's Court` = 66,
  `Supreme Court of Tasmania—Criminal Division` = 67,
  `Supreme Court of Tasmania—Civil Division` = 68,
  `Supreme Court of Tasmania—Court of Appeals` = 69,
  `Magistrates Court of Tasmania` = 70,
  `Tasmanian Coroner's Court` = 71,
  `Supreme Court of the Northern Territory—Court of Appeal` = 72,
  `Supreme Court of the Northern Territory—Court of Criminal Appeal` = 73,
  `Supreme Court of the Northern Territory—Civil Trial` = 74,
  `Supreme Court of the Northern Territory—Criminal Trial` = 75,
  `Northern Territory Local Court (Magistrate's Court)` = 76,
  `Coroner's Court of the Northern Territory` = 77,
  `Supreme Court of the Australian Capital Territory` = 78,
  `Supreme Court of the Australian Capital Territory—Court of Appeal` = 79,
  `Magistrates Court of the Australian Capital Territory` = 80,
  `Coroner's Court of the Australian Capital Territory` = 81,
  `Supreme Court of Norfolk Island` = 82,
  `Court of Petty Sessions for Norfolk Island` = 83,
  `Nauru Supreme Court` = 84,
  `Other` = 85,
  `Family Court of Australia—Full Court` = 86
)

justice_bio$priorJudicialService1Specific |> unique() |> sort()
val_labels(justice_bio$priorJudicialService1Specific) <- varPriorJudServiceSpecific

# 110-116. Service 1 additional fields - years/parties (no labels for numeric/text fields)

# 117. Prior Judicial Service 2—General (PriorJudicialService2General - capital P!)
justice_bio$PriorJudicialService2General |> unique() |> sort()
val_labels(justice_bio$PriorJudicialService2General) <- varPriorJudServiceGen

# 118. Prior Judicial Service 2—Location (priorJudicialService2Location)
justice_bio$priorJudicialService2Location |> unique() |> sort()
val_labels(justice_bio$priorJudicialService2Location) <- varJudServiceLocation

# 119. Prior Judicial Service 2—Specific (priorJudicialService2Specific)
justice_bio$priorJudicialService2Specific |> unique() |> sort()
val_labels(justice_bio$priorJudicialService2Specific) <- varPriorJudServiceSpecific

# 120-125. Service 2 additional fields (no labels)

# 126. Prior Judicial Service 3—General (PriorJudicialService3General - capital P!)
justice_bio$PriorJudicialService3General |> unique() |> sort()
val_labels(justice_bio$PriorJudicialService3General) <- varPriorJudServiceGen

# 127. Prior Judicial Service 3—Location (priorJudicialService3Location)
justice_bio$priorJudicialService3Location |> unique() |> sort()
val_labels(justice_bio$priorJudicialService3Location) <- varJudServiceLocation

# 128. Prior Judicial Service 3—Specific (priorJudicialService3Specific)
justice_bio$priorJudicialService3Specific |> unique() |> sort()
val_labels(justice_bio$priorJudicialService3Specific) <- varPriorJudServiceSpecific

# 129-134. Service 3 additional fields (no labels)

# 135. Admission to the Bar (barAdmission)
justice_bio$barAdmission |> unique() |> sort()
val_labels(justice_bio$barAdmission) <- varYesNo

# 136. Year of First Admission to the Bar (barAdmissionYear) - numeric, no labels

# 137. State in Which First Admitted to the Bar (barAdmissionState)
varBarState <- c(`Australian Capital Territory` = 1,
                 `New South Wales` = 2,
                 `Northern Territory` = 3,
                 `Queensland` = 4,
                 `South Australia` = 5,
                 `Tasmania` = 6,
                 `Victoria` = 7,
                 `Western Australia` = 8,
                 `Overseas` = 9)

justice_bio$barAdmissionState |> unique() |> sort()
val_labels(justice_bio$barAdmissionState) <- varBarState

# 138. Promotion to Senior Counsel (barPromotion)
varBarPromotion <- c(`Yes, SC/QC` = 1,
                     `No` = 2)

justice_bio$barPromotion |> unique() |> sort()
val_labels(justice_bio$barPromotion) <- varBarPromotion

# 139. Year Promoted to Senior Counsel (barPromotionYear) - numeric, no labels

# 140. High Court Appearances (hcaAppearances - note the 's' plural!)
varHCAAppearance <- c(`No` = 1,
                      `Yes—Infrequent (1-5 appearances)` = 2,
                      `Yes—Moderate frequency (6-15 appearances)` = 3,
                      `Yes—Frequent (more than 15 appearances)` = 4)

justice_bio$hcaAppearances |> unique() |> sort()
val_labels(justice_bio$hcaAppearances) <- varHCAAppearance

# 141. Practice as Solicitor (solicitor)
justice_bio$solicitor |> unique() |> sort()
val_labels(justice_bio$solicitor) <- varYesNo

# 142-143. Years (solicitorFirstYear, solicitorLastYear) - numeric, no labels

# 144. State Where First Admitted as Solicitor (solicitorState)
justice_bio$solicitorState |> unique() |> sort()
val_labels(justice_bio$solicitorState) <- varBarState

# 145. Law Teaching (teaching)
varTeaching <- c(`Yes—Fulltime academic` = 1,
                 `Yes—Adjunct/casual academic` = 2,
                 `No` = 3)

justice_bio$teaching |> unique() |> sort()
val_labels(justice_bio$teaching) <- varTeaching

# 146. High Court Associateship (hcaAssociate)
justice_bio$hcaAssociate |> unique() |> sort()
val_labels(justice_bio$hcaAssociate) <- varYesNo

# 147. Year of High Court Associateship (hcaAssociateYear) - numeric, no labels

# 148. Justice for High Court Associateship (hcaAssociateJustice)
justice_bio$hcaAssociateJustice |> unique() |> sort()
val_labels(justice_bio$hcaAssociateJustice) <- varJustice

# 149. Other Judicial Associateship (otherAssociate - CORRECTED!)
justice_bio$otherAssociate |> unique() |> sort()
val_labels(justice_bio$otherAssociate) <- varYesNo

# 150. Year of Other Judicial Associateship (otherAssociateYear) - numeric, no labels

# 151. Government Appointment Type (govAppointmentType)
varGovAppointmentType <- c(`Commonwealth Solicitor-General` = 1,
                           `State Solicitor-General` = 2,
                           `Commonwealth tribunal appointment` = 3,
                           `Commonwealth commission (major)` = 4,
                           `Not applicable` = 999)

justice_bio$govAppointmentType |> unique() |> sort()
val_labels(justice_bio$govAppointmentType) <- varGovAppointmentType

# 152. Government Appointment Party (govAppointmentParty)
varGovAppointParty <- c(`Liberal` = 1,
                        `Labor` = 2,
                        `Not applicable` = 999)

justice_bio$govAppointmentParty |> unique() |> sort()
val_labels(justice_bio$govAppointmentParty) <- varGovAppointParty

# 153. Political Party Commonwealth (politicalPartyCth)
justice_bio$politicalPartyCth |> unique() |> sort()
val_labels(justice_bio$politicalPartyCth) <- varGovAppointParty

# 154. Known Political Identification (politicalIdentification)
justice_bio$politicalIdentification |> unique() |> sort()
val_labels(justice_bio$politicalIdentification) <- varGovAppointParty


# Add comprehensive variable labels
justice_bio <- justice_bio |>
  set_variable_labels(
    # Identification Variables
    justice = "Justice ID",
    justiceName = "Justice name",
    dateNom = "Date first nomination announced",
    position = "Position to which first nominated to court",
    dateSwearingIn = "Date of swearing in ceremony",
    seatID = "Seat identification number",
    homeRegistry = "Home registry",
    justiceReplaced = "Justice who nominee replaced",
    chiefAnnounce = "Chief Justice when nomination announced",
    chiefSworn = "Chief Justice when sworn in",
    nomPM = "Nominating Prime Minister",
    nomPMParty = "Nominating Prime Minister party",
    politicalPowerDateNom = "Political power on date of nomination",
    PMSwearingIn = "Prime Minister on date of swearing in",
    PMPartySwearingIn = "Prime Minister party on date of swearing in",
    politicalPowerSwearingIn = "Political power on date of swearing in",
    promotionDate = "Date of nomination of puisne justice to Chief Justice",
    nomPMPromotion = "Nominating PM for Chief Justice from puisne",
    nomPMPartyPromotion = "Nominating PM party for CJ from puisne",
    politicalPowerPromotion = "Political power for CJ from puisne",
    yearPromotion = "Year of appointment as Chief Justice from puisne",
    dateSwearingInPromotion = "Date of swearing in as Chief Justice from puisne",
    replacementCJPromotion = "Who puisne justice replaced as Chief Justice",
    ideologyScore = "Ideology score",

    # Personal Background Variables
    nomYearBirth = "Year of birth",
    nomDateBirth = "Exact date of birth",
    placeBirthGen = "State or country of birth",
    religionGeneral = "Religion—general",
    religionSpecific = "Religion—specific",
    race = "Nominee's race",
    gender = "Nominee's gender",
    yearDeath = "Year of death",
    dateDeath = "Exact date of death",
    ageDeath = "Age at date of death",
    yearDeparture = "Year departed court",
    dateDeparture = "Exact date of departure from court",
    ageDateDeparture = "Age on date of departure",
    reasonDeparture = "Reason for departing court",
    resignationParty = "Resignation party",
    PMDeparture = "Prime Minister on date of departure",
    PMPartyDeparture = "PM party on date of departure",
    politicalPowerRetirementAnnounce = "Political power on date retirement announced",
    positionAfterDeparture1 = "Position held after departure 1",
    positionAfterDepartureParty1 = "Party appointing to position after departure 1",
    positionAfterDeparture2 = "Position held after departure 2",
    positionAfterDepartureParty2 = "Party appointing to position after departure 2",
    positionAfterDeparture3 = "Position held after departure 3",
    positionAfterDepartureParty3 = "Party appointing to position after departure 3",
    positionAfterDeparture4 = "Position held after departure 4",
    positionAfterDepartureParty4 = "Party appointing to position after departure 4",
    childhoodLocationGeneral = "Childhood location—general",
    childhoodLocationSpecific = "Childhood location—specific",
    childhoodSurrounds = "Childhood surrounds",

    # Family Information
    economicStatus = "Childhood economic status",
    occupationFather = "Primary occupation of father",
    politicalFather = "Political affiliation father",
    occupationMother = "Primary occupation of mother",
    politicalMother = "Political affiliation mother",
    occupationSpouse = "Primary occupation of spouse",
    politicalSpouse = "Political affiliation spouse",
    numberMarriages = "Number of marriages",
    dateMarriage = "Date of last marriage",
    numberChildren = "Number of children",
    numberDaughters = "Number of daughters",
    numberSons = "Number of sons",

    # Educational Background Variables
    secondaryEducationType = "Type of secondary education",
    firstDegree = "First degree",
    firstDegreeInstitution = "First degree institution",
    firstDegreeStatus = "First degree status",
    firstDegreeYearStart = "First degree year started",
    firstDegreeYearEnd = "First degree year completed",
    secondDegree = "Second degree",
    secondDegreeInstitution = "Second degree institution",
    secondDegreeStatus = "Second degree status",
    secondDegreeYearStart = "Second degree year started",  # CORRECTED!
    secondDegreeYearEnd = "Second degree year completed",
    thirdDegree = "Third degree",
    thirdDegreeInstitution = "Third degree institution",
    thirdDegreeStatus = "Third degree status",
    thirdDegreeYearStart = "Third degree year started",
    thirdDegreeYearEnd = "Third degree year completed",
    nameLawSchool = "Name of law school",
    lastYearLawSchool = "Year of law school",
    academicHonoursMedal = "Academic honours medal",
    academicHonoursFirstClass = "Academic honours first class honours",
    academicHonoursOverseas = "Academic honours overseas scholarship",
    overseasStudy = "Overseas study",
    overseasStudyLocation = "Overseas study location",
    firstGradDegree = "First graduate degree",  # CORRECTED!
    firstGradDegreeInstitution = "First graduate degree institution",
    firstGradDegreeStatus = "First graduate degree status",
    secondGradDegree = "Second graduate degree",
    secondGradDegreeInstitution = "Second graduate degree institution",
    secondGradDegreeStatus = "Second graduate degree status",
    thirdGradDegree = "Third graduate degree",
    thirdGradDegreeInstitution = "Third graduate degree institution",
    thirdGradDegreeStatus = "Third graduate degree status",
    lastYearAttendedGrad = "Last year attended graduate institution",

    # Employment Background Variables
    postitionWhenNom = "Position when nominated",
    positionNomLocation = "Position when nominated location",
    nomFromJudiciary = "Nominated from judiciary",
    priorJudicialService = "Prior judicial service",
    totalNumMonthsJudicialService = "Total months judicial service",
    numMonthsStateCourtService = "Number of months state court service",
    numMonthsFedCourtService = "Number of months federal court service",
    numMonthsAppCourtService = "Number of months appellate court service",
    numMonthsTrialCourtService = "Number of months trial court service",
    PriorJudicialService1General = "Prior judicial service 1—general",
    priorJudicialService1Location = "Prior judicial service 1—location",
    priorJudicialService1Specific = "Prior judicial service 1—specific",
    priorJudicialService1StartDate = "Prior judicial service 1—start date",
    priorJudicialService1EndDate = "Prior judicial service 1—end date",
    numMonthsJudicialService1 = "Number of months judicial service 1",
    priorJudicialService1FirstYear = "Prior judicial service 1—year first served",
    priorJudicialService1LastYear = "Prior judicial service 1—year last served",
    priorJudicialService1Party = "Prior judicial service 1—party appointing",
    PriorJudicialService2General = "Prior judicial service 2—general",
    priorJudicialService2Location = "Prior judicial service 2—location",
    priorJudicialService2Specific = "Prior judicial service 2—specific",
    priorJudicialService2StartDate = "Prior judicial service 2—start date",
    priorJudicialService2EndDate = "Prior judicial service 2—end date",
    numMonthsJudicialService2 = "Number of months judicial service 2",
    priorJudicialService2FirstYear = "Prior judicial service 2—year first served",
    priorJudicialService2LastYear = "Prior judicial service 2—year last served",
    priorJudicialService2Party = "Prior judicial service 2—party appointing",
    PriorJudicialService3General = "Prior judicial service 3—general",
    priorJudicialService3Location = "Prior judicial service 3—location",
    priorJudicialService3Specific = "Prior judicial service 3—specific",
    priorJudicialService3FirstYear = "Prior judicial service 3—year first served",
    priorJudicialService3LastYear = "Prior judicial service 3—year last served",
    priorJudicialService3StartDate = "Prior judicial service 3—start date",
    priorJudicialService3EndDate = "Prior judicial service 3—end date",
    numMonthsJudicialService3 = "Number of months judicial service 3",
    priorJudicialService3Party = "Prior judicial service 3—party appointing",
    barAdmission = "Admission to the bar",
    barAdmissionYear = "Year of first admission to the bar",
    barAdmissionState = "State in which first admitted to the bar",
    barPromotion = "Promotion to senior counsel",
    barPromotionYear = "Year promoted to senior counsel",
    hcaAppearances = "High Court appearances",
    solicitor = "Practice as solicitor",
    solicitorFirstYear = "Year first practiced as solicitor",
    solicitorLastYear = "Year last practiced as solicitor",
    solicitorState = "State where first admitted as solicitor",
    teaching = "Law teaching",
    hcaAssociate = "High Court associateship",
    hcaAssociateYear = "Year of High Court associateship",
    hcaAssociateJustice = "Justice for High Court associateship",
    otherAssociate = "Other judicial associateship",  # CORRECTED!
    otherAssociateYear = "Year of other judicial associateship",
    govAppointmentType = "Government appointment type",
    govAppointmentParty = "Government appointment party",
    politicalPartyCth = "Political party Commonwealth",
    politicalIdentification = "Known political identification",
    Notes = "Notes"
  )


# checking to make sure everything is good:


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
all_vars <- names(justice_bio)
labeled_vars <- character()

for (var_name in all_vars) {
  var <- justice_bio[[var_name]]
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
  problems <- lapply(labeled_vars, function(v) check_labels(justice_bio, v))
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
    var <- justice_bio[[var_name]]
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

# saving ####
library(haven)

#save the processed data in raw data folder

# Save as SPSS file (.sav)
write_sav(justice_bio, "data-raw/justice_bio.sav")

# Save as Stata file (.dta)
write_dta(justice_bio, "data-raw/justice_bio.dta")


# Get all variables with value labels
all_vars <- names(justice_bio)
labeled_vars <- character()

for (var_name in all_vars) {
  var <- justice_bio[[var_name]]
  if (!is.null(val_labels(var)) && length(val_labels(var)) > 0) {
    labeled_vars <- c(labeled_vars, var_name)
  }
}

# Convert each to factor using the labels
for (var_name in labeled_vars) {
  justice_bio[[var_name]] <- to_factor(justice_bio[[var_name]])
  cat("✓ Converted:", var_name, "\n")
}



#save the processed data in raw data folder
save(justice_bio, file="data-raw/justice_bio.rda")
write_excel_csv(justice_bio, file="data-raw/justice_bio.csv")
writexl::write_xlsx(justice_bio, path="data-raw/justice_bio.xlsx")

# save a copy of the raw data for the package
usethis::use_data(justice_bio, overwrite = TRUE)
