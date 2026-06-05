## BSTA5030 Research Project - Natasha Alegro
# Date created: 26/02/26
# Title: 1. Descriptive summary of data

# Load libraries
library(dplyr) #%>%
library(labelled) #generate_dictionary
library(gtsummary) #tbl_summary
library(tidyverse) #read_rds

# Set directories
rds <- "/Volumes/PRJ-ASSET_MH_NSW"
main_dir <- file.path(rds, "4. Statistical Analyses/Natasha WPP non-proportional hazards")
data_dir <- file.path(main_dir, "Data") 
output_dir <- file.path(main_dir, "Tables and figures")

# Loading in data
analysis_data <- read_rds(file.path(data_dir, "original_data_nsw_3nations.Rds"))

# Check all variables are in correct format
analysis_data %>% generate_dictionary()

# Check for duplicates
sum(duplicated(analysis_data))
  #no duplicates found

# Count id
analysis_data %>% 
  count(id) %>%
  filter(n > 1)
  #shows that each id has one row

# Create Table 1 dataset
table1_data <- analysis_data %>%
  # there are 149 missing records in variable bmi
  drop_na(bmi) %>%
  # reordering factor levels of variable deprivation
  mutate(deprivation = factor(deprivation, 
    levels = c("1 (Most deprived 20%)", 2, 3, 4, "5 (Least deprived 20%)"))) %>%
  #add new variable for age categories
  mutate(age_cat = factor(case_when(
    age >= 18 & age <= 34 ~ "18 - 34",
    age >= 35 & age <= 54 ~ "35 - 54",
    age >= 55 & age <= 74 ~ "55 - 74",
    age >= 75 ~ "75+"),
    levels = c("18 - 34", "35 - 54", "55 - 74", "75+"))) %>%
  # renaming variable labels
  set_variable_labels(
    time = "Time to transplant (years)",
    mi_kf = "Mental Health Service Use (MHSU)",
    age = "Age",
    age_cat = "Age Category",
    sex = "Sex",
    ethnicity = "Ethnicity",
    bmi = "BMI",
    bmicat = "BMI Category",
    remoteness = "Remoteness",
    deprivation = "Deprivation",
    ihd = "Ischaemic heart disease",
    stroke = "Stroke",
    heart_failure = "Heart failure",
    pvd = "Peripheral vascular disease",
    hypertension = "Hypertension",
    cpd = "Chronic pulmonary disease",
    diabetes = "Diabetes",
    cancer = "Cancer",
    liver_disease = "Liver disease") %>%
  # selecting variables shown in Table 1
  select(mi_kf, age, age_cat, sex, ethnicity, bmi, bmicat, remoteness, deprivation, 
         ihd, stroke, heart_failure, pvd, hypertension, cpd, diabetes, cancer) 

# Creating Table 1
tbl_var <- table1_data %>%
  tbl_summary(by = mi_kf,
              statistic = list(all_categorical()~"{n} ({p}%)",
                               all_continuous()~"{median} ({p25}, {p75})")) %>%
  modify_header(
    label = "**Variable**",
    stat_1 = "**None**",
    stat_2 = "**Moderate**",
    stat_3 = "**Severe**"
  ) %>%
  modify_caption("**Table 1. Baseline Characteristics**")

# export table as csv file
write_csv(tbl_var %>% as_tibble(), file.path(output_dir, "Table1.csv"))
