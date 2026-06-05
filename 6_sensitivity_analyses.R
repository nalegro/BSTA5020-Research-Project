## BSTA5030 Research Project - Natasha Alegro
# Title: 5. Sensitivity Analyses

#Create new model removing variables with missing values
sa2_data <- analysis_data %>%
  #there are 149 missing records in variable bmi
  #removing bmi and bmicat as there are missing values in these variables
  select(-bmi, -bmicat) %>%
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
    sex = "Sex",
    ethnicity = "Ethnicity",
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

model_sa2 <- coxph(
  Surv(time, event) ~ mi_kf + age + sex + ethnicity + remoteness
  + deprivation + ihd + stroke + heart_failure + pvd + hypertension + cpd + diabetes + 
    cancer + liver_disease,
  data = sa2_data)
  #fit cox model to time to deceased donor transplant

# Method 2: Time-dependent effect
#create model 5b data set
model_sa3 <- coxph(
  Surv(time, event) ~ mi_kf + age_cat + sex + ethnicity + tt(ethnicity) 
  + remoteness + deprivation + tt(deprivation) + ihd + stroke 
  + heart_failure + tt(heart_failure) + pvd + hypertension + cpd + diabetes 
  + tt(diabetes) + cancer + tt(cancer) + liver_disease + tt(liver_disease),
  data = sa2_data,
  tt = function(x, t, ...){
    #turning categorical variables into dummy variables inside the function
    mm <- model.matrix(~ x)[, -1, drop = FALSE]
    mm * log(t)})

# Method 3: Removing violating variables
model_sa4 <- coxph(
  Surv(time, event) ~ mi_kf + age_cat + sex + remoteness + ihd + stroke + pvd 
  + hypertension + cpd,
  data = sa2_data)

#Create comparison table of both models
tbl_sa2 <- model_sa2 %>%
  tbl_regression(exponentiate = TRUE,
                 add_estimate_to_reference_rows = TRUE,
                 include = mi_kf) %>% #only displaying exposure of interest
  add_global_p()
  #table of estimates for ignoring non-proportional hazards

tbl_sa3 <- model_sa3 %>%
  tbl_regression(exponentiate = TRUE,
                 add_estimate_to_reference_rows = TRUE,
                 include = mi_kf) %>%
  add_global_p()
  #table of estimates for time-dependent effect

tbl_sa4 <- model_sa4 %>%
  tbl_regression(exponentiate = TRUE,
                 add_estimate_to_reference_rows = TRUE, 
                 include = mi_kf) %>% #displaying only variable of interest
  add_global_p()
  #table of estimates after removing variables

tbl_missdat <- tbl_merge(
  tbls = list(tbl_sa2, tbl_sa3, tbl_sa4),
  tab_spanner = c("**Ignore**", 
                  "**Time-dependent effect**", "**Remove**")) %>%
  modify_caption("Table 5. Sensitivity analysis of the association between mental 
                 illness and time to deceased donor kidney transplant using 
                 alternative Cox regression model")
  #comparing full cox model with model with removed variables

write_csv(tbl_missdat %>% as_tibble(), file.path(output_dir, "Table8.csv"))
  #export as csv file


