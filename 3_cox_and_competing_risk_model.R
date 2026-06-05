## BSTA5030 Research Project - Natasha Alegro
# Title: 3. Cox model and competing risk model

# Load libraries
library(survival) #Surv, coxph
library(survminer) #ggsurvplot

#Create data set for Cox model
cox_data <- analysis_data %>%
  #there are 149 missing records in variable bmi
  drop_na(bmi, bmicat) %>%
  #reordering factor levels of variable deprivation
  mutate(deprivation = factor(deprivation, 
    levels = c("1 (Most deprived 20%)", 2, 3, 4, "5 (Least deprived 20%)"))) %>%
  #271 rows had a value of 0 for time, adding 1 to all rows
  mutate(time = time + 1) %>%
  #add new variable for age categories
  mutate(age_cat = factor(case_when(
    age >= 18 & age <= 34 ~ "18 - 34",
    age >= 35 & age <= 54 ~ "35 - 54",
    age >= 55 & age <= 74 ~ "55 - 74",
    age >= 75 ~ "75+"),
    levels = c("18 - 34", "35 - 54", "55 - 74", "75+"))) %>%
  #create event variable
  mutate(event = case_when(
    txtype == "Deceased" ~ 1,
    txtype == "Living" ~ 0,
    TRUE ~ 0)) %>%
  set_variable_labels(
    time = "Time to transplant (years)",
    mi_kf = "Mental Health Service Use (MHSU)",
    age = "Age",
    age_cat = "Age Category",
    sex = "Sex",
    ethnicity = "Ethnicity",
    bmi = "Weight (kg)",
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
    liver_disease = "Liver disease")

#Fit cox model to time to deceased transplant
cox_model <- coxph(
  Surv(time, event) ~ mi_kf + age_cat + sex + ethnicity + bmicat + remoteness
  + deprivation + ihd + stroke + heart_failure + pvd + hypertension + cpd + diabetes + 
    cancer + liver_disease,
  data = cox_data)

#Table of model estimates for cox model
tbl_cox <- cox_model %>%
  tbl_regression(exponentiate = TRUE,
                 add_estimate_to_reference_rows = TRUE,
                 include = mi_kf) %>%
  add_global_p()

#Fit competing risk model
crmodel <- crr(
  Surv(time, cmprsk) ~ mi_kf + age_cat + sex + ethnicity  + bmicat + remoteness
  + deprivation + ihd + stroke + heart_failure + pvd + hypertension + cpd + diabetes + 
    cancer + liver_disease,
    data = crr_data)

#Table of model estimates for competing risk model
tbl_cr <- crmodel %>%
  tbl_regression(exponentiate = TRUE,
                 add_estimate_to_reference_rows = TRUE,
                 include = mi_kf) %>%
  add_global_p()

#Comparison table of Cox model hazard ratios with Competing risks model subdistribution hazard ratios
tbl_comp <- tbl_merge(
  tbls = list(tbl_cox, tbl_cr),
  tab_spanner = c("**Cox model (HR)**", "**Competing risks model (SHR)**")) %>%
  modify_caption("Table 3. Comparison of Cox and Competing risks model for time 
                 to deceased donor transplant")

write_csv(tbl_comp %>% as_tibble(), file.path(output_dir, "Table3.csv"))
  #export as csv file
