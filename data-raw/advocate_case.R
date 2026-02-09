## code to prepare `Special Leave 2003-2018 [PAT THIS ONE].xlsx` dataset goes here
# input from shared folder and storage in Raw ####
library(readxl)


# save a copy of the excel file in data-raw

# file.copy(from = "/Users/u1040068/Dropbox/High Court Project/Data Files/CURRENT DATASETS/attorney_bio_data_1995_2019.xlsx",
#           to="data-raw/attorney_bio_data_1995_2019.xlsx",
#           overwrite=T)

# input from shared folder and storage in Raw ####
advocate_case <- readxl::read_excel("data-raw/attorney_bio_data_1995_2019.xlsx")


#cleaning  data ####


library(dplyr)
library(tidyverse)
library(labelled)


advocate_case<-as_tibble(advocate_case)





# Function to reshape one type of representative
reshape_reps <- function(data, rep_type, side_name) {

  data[data==999]<-NA
  data[data==888]<-NA
  data %>%
    select(HCDBcaseId, matches(paste0("^", rep_type, "\\d+"))) %>%
    # Clean up column names: RepGen -> General
    rename_with(~str_replace(., "General", "RepGen"), matches("General")) %>%
    rename_with(~str_replace(., "RepGen", "RepGeneral"), matches("RepGen")) %>%
    rename_with(~str_replace(., "RepSpecific", "Specific"), matches("RepSpecific")) %>%
    rename_with(~str_replace(., "Specific", "RepSpecific"), matches("Specific")) %>%

    # Convert all columns to character to avoid type conflicts
    mutate(across(-HCDBcaseId, as.character)) %>%
    pivot_longer(
      cols = -HCDBcaseId,
      names_to = c("rep_number", "variable"),
      names_pattern = paste0("^", rep_type, "(\\d+)(.*)"),
      values_to = "value"
    ) %>%
    pivot_wider(
      names_from = variable,
      values_from = value
    ) %>%
    mutate(
      representative_side = side_name,
      rep_number = as.integer(rep_number)
    ) %>%
    # Remove rows where all rep variables are NA or empty strings
    filter(!if_all(-c(HCDBcaseId, representative_side, rep_number),
                   ~is.na(.) | . == ""))
}

# Reshape each type and combine
advocate_case_long <- bind_rows(
  reshape_reps(advocate_case, "AppRep", "Appellant"),
  reshape_reps(advocate_case, "RespRep", "Respondent"),
  reshape_reps(advocate_case, "IntvRep", "Intervener")
) %>%
  # Join back case-level variables
  left_join(
    advocate_case %>%
      select(HCDBcaseId, caseNumber, caseName, Notes),
    by = "HCDBcaseId"
  ) %>%
  # Rearrange columns sensibly: case info first, then rep info, then attributes
  select(HCDBcaseId, caseNumber, caseName, representative_side, rep_number,
         everything(), -Notes, Notes) %>%
  arrange(HCDBcaseId, representative_side, rep_number)

advocate_case_long|>names()
# cleaning up individual variables

# "General"


# "RepGeneral"
advocate_case_long$RepGeneral[is.na(advocate_case_long$RepGeneral)  &
                            advocate_case_long$representative_side=="Intervener" ]<- '3'

advocate_case_long$RepGeneral|>unique()

# "RepSpecific"

advocate_case_long$RepSpecific|>unique()


# "Name"

advocate_case_long$Name|>unique()|>sort()


# "Title"

advocate_case_long$Title|>unique()|>sort()

# "Status1"

advocate_case_long$Status1|>unique()|>sort()

# "Status2"

advocate_case_long$Status2|>unique()|>sort()

# "Status2State"

advocate_case_long$Status2State|>unique()|>sort()


# "Gender"

advocate_case_long$Gender|>unique()|>sort()

# "Role"

advocate_case_long$Role|>unique()|>sort()



# label data ----

library(labelled)

# Define value labels based on the codebook

# General (representative representation general)
general_labels <- c(
  "Appellant/applicant/plaintiff/petitioner" = 1,
  "Respondent/defendant" = 2,
  "Intervener" = 3,
  "Amicus" = 4,
  "Interested party in Section 44 matters" = 5
)

# Title labels
title_labels <- c(
  "Mr" = 1,
  "Dr" = 2,
  "Ms" = 3,
  "Mrs" = 4,
  "Miss" = 5,
  "Other" = 6
)

# Status1 labels
status1_labels <- c(
  "QC" = 1,
  "SC" = 2,
  "Barrister or solicitor" = 3,
  "Self" = 4,
  "Other" = 5
)

# Status2 labels (Solicitor General)
status2_labels <- c(
  "Yes solicitor general" = 1,
  "No, not solicitor general" = 2
)

# Status2State labels
status2state_labels <- c(
  "Australia (federal)" = 1,
  "Australian Capital Territory" = 2,
  "New South Wales" = 3,
  "Northern Territory" = 4,
  "Queensland" = 5,
  "South Australia" = 6,
  "Tasmania" = 7,
  "Victoria" = 8,
  "Norfolk Island" = 9,
  "Nauru" = 10,
  "Western Australia" = 11,
  "Not applicable" = 999
)

# Gender labels
gender_labels <- c(
  "Male" = 1,
  "Female" = 2,
  "Unidentified" = 3
)

# Role labels
role_labels <- c(
  "Speaker - Major" = 1,
  "Speaker - Minor" = 2,
  "Appearance (non-speaker)" = 3,
  "No appearance, written submissions only" = 4
)

# Apply labels to advocate_case_long
advocate_case_long <- advocate_case_long %>%
  mutate(
    # Convert to numeric first (they're currently character from the pivot)
    RepGeneral = as.numeric(RepGeneral),
    Title = as.numeric(Title),
    Status1 = as.numeric(Status1),
    Status2 = as.numeric(Status2),
    Status2State = as.numeric(Status2State),
    Gender = as.numeric(Gender),
    Role = as.numeric(Role)
  ) %>%
  # Apply value labels
  set_value_labels(
    RepGeneral = general_labels,
    Title = title_labels,
    Status1 = status1_labels,
    Status2 = status2_labels,
    Status2State = status2state_labels,
    Gender = gender_labels,
    Role = role_labels
  ) %>%
  # Apply variable labels (descriptive labels for the variables themselves)
  set_variable_labels(
    HCDBcaseId = "High Court Database Case ID",
    caseNumber = "Case number",
    caseName = "Case name",
    representative_side = "Side represented (Appellant/Respondent/Intervener)",
    rep_number = "Representative number within side",
    RepGeneral = "Representative representation general",
    RepSpecific = "Representative representation specific (parties represented)",
    Name = "Representative name",
    Title = "Representative title",
    Status1 = "Representative status (QC/SC/Barrister/etc.)",
    Status2 = "Solicitor General status",
    Status2State = "Solicitor General jurisdiction",
    Gender = "Representative gender",
    Role = "Representative role at hearing",
    Notes = "Additional notes"
  )

# Verify labels were applied
str(advocate_case_long)



# saving ####

library(haven)

advocate_case<-advocate_case_long
#save the processed data in raw data folder

# Save as SPSS file (.sav)
write_sav(advocate_case, "data-raw/advocate_case.sav")

# Save as Stata file (.dta)
write_dta(advocate_case, "data-raw/advocate_case.dta")

advocate_case<-advocate_case %>%
  mutate(across(c(RepGeneral, Title, Status1, Status2, Gender, Role), to_factor))

# Save as R file (.rda)
save(advocate_case, file="data-raw/advocate_case.rda")

# Save as excel file (.csv)
write_excel_csv(advocate_case, file="data-raw/advocate_case.csv")
writexl::write_xlsx(advocate_case, path="data-raw/advocate_case.xlsx")

# save a copy of the raw data for the package
usethis::use_data(advocate_case, overwrite = TRUE)

