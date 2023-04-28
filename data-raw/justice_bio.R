## code to prepare `justice_bio_November_7_2021` dataset goes here
# input from shared folder and storage in Raw ####
library(readxl)
justice_bio <- read_excel("~/Dropbox/High Court Project/Data Files/CURRENT DATASETS/justice_bio_Feb_2023.xlsx",
                                          skip = 2)

# save a copy of the excel file in data-raw

file.copy(from="~/Dropbox/High Court Project/Data Files/CURRENT DATASETS/justice_bio_Feb_2023.xlsx",
          to="data-raw/justice_bio_Feb_2023.xlsx",
          overwrite = T)

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
justice_bio$chiefSwornIn|>unique()|>sort()

val_labels(justice_bio$chiefSwornIn)<- varChief


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

resignationParty
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
nameLawSchool
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
justice_bio$firstGraduateDegree|>unique()|>sort()
val_labels(justice_bio$firstGraduateDegree)<- varGraduateDegree

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

# 91.	Second Graduate Degree	39
# 92.	Second Graduate Degree Institution	39
# 93.	Second Graduate Degree Status	39
# 94.	Third Graduate Degree	39s
# 95.	Third Graduate Degree Institution	39
# 96.	Third Graduate Degree Status	40
# 97.	Last Year Attended Graduate Institution	40
# Employment Background Variables	40
# 98.	Position When Nominated	40
# 99.	Position When Nominated Location	41
# 100.	Nominated from Judiciary	41
# 101.	Prior Judicial Service	41
# 102.	Total Months Judicial Service	42
# 103.	Number of Months State Court Service	42
# 104.	Number of Months Federal Court Service	42
# 105.	Number of Months Appellate Court Service	42
# 106.	Number of Months Trial Court Service	43
# 107.	Prior Judicial Service 1—General	43
# 108.	Prior Judicial Service 1—Location	43
# 109.	Prior Judicial Service 1—Specific	44
# 110.	Prior Judicial Service 1—Year First Served	46
# 111.	Prior Judicial Service 1—Year Last Served	46
# 112.	Prior Judicial Service 1—Party Appointing	46
# 113.	Prior Judicial Service 1—Person(s) Appointing	46
# 114.	Prior Judicial Service 2—General	47
# 115.	Prior Judicial Service 2—Location	47
# 116.	Prior Judicial Service 2—Specific	47
# 117.	Prior Judicial Service 2—Year First Served	49
# 118.	Prior Judicial Service 2—Year Last Served	49
# 119.	Prior Judicial Service 2—Party Appointing	50
# 120.	Prior Judicial Service 2—Person(s) Appointing	50
# 121.	Prior Judicial Service 3—General	50
# 122.	Prior Judicial Service 3—Location	50
# 123.	Prior Judicial Service 3—Specific	51
# 124.	Prior Judicial Service 3—Year First Served	53
# 125.	Prior Judicial Service 3—Year Last Served	53
# 126.	Prior Judicial Service 3—Party Appointing	53
# 127.	Prior Judicial Service 3—Person(s) Appointing	53
# 128.	Admission to the Bar	53
# 129.	Year of First Admission to the Bar	54
# 130.	State in Which First Admitted to the Bar	54
# 131.	Promotion to Senior Counsel	54
# 132.	Year Promoted to Senior Counsel	55
# 133.	High Court Appearances	55
# 134.	Practice as Solicitor	55
# 135.	Year First Practiced as Solicitor	55
# 136.	Year Last Practiced as Solicitor	55
# 137.	State Where First Admitted as Solicitor	56
# 138.	Law Teaching	56
# 139.	High Court Associateship	56
# 140.	Year of High Court Associateship	56
# 141.	Justice for High Court Associateship	57
# 142.	Other Judicial Associateship	57
# 143.	Other Judicial Associateship Court	57
# 144.	Year of Other Judicial Associateship	57
# 145.	Government Appointment Type	57
# 146.	Government Appointment Party	58
# 147.	Political Party Commonwealth	58
# 148.	Known Political Identification	58



# saving ####

#save the processed data in raw data folder
save(justice_bio, file="data-raw/justice_bio.rda")

# save a copy of the raw data for the package
usethis::use_data(justice_bio, overwrite = TRUE)
