## BSTA5030 Research Project - Natasha Alegro
# Title: 5. 

## Comparison of model with multiple variables with PH violation
model_4 <- coxph(
  Surv(time, event) ~ mi_kf + age_cat + sex + ethnicity + deprivation + diabetes,
  data = cox_data)

cox.zph(model_4)
plot(cox.zph(model_4))

# Method 1: Stratification
model_4a <- coxph(
  Surv(time, event) ~ mi_kf + age_cat + sex + strata(ethnicity) + strata(deprivation) + 
    strata(diabetes),
  data = cox_data)
  #stratified categorical variables with a strong violation of the PH assumption

cox.zph(model_4a)
plot(cox.zph(model_4a))

# Method 2: Time-dependent effect
#create model 4b data set
model_4b <- coxph(
  Surv(time, event) ~ mi_kf + age_cat + sex + ethnicity + tt(ethnicity) 
  + deprivation + tt(deprivation) + diabetes + tt(diabetes),
  data = cox_data,
  tt = function(x, t, ...){
    #turning categorical variables into dummy variables inside the function
    mm <- model.matrix(~ x)[, -1, drop = FALSE]
    mm * log(t)})
  #variables ethnicity, bmicat and diabetes modelled as a time-dependent effect

#Create table comparing all methods
tbl_4 <- model_4 %>%
  tbl_regression(exponentiate = TRUE,
                 add_estimate_to_reference_rows = TRUE,
                 include = mi_kf) %>% #only displaying exposure of interest
  add_global_p()
  #table of estimates for ignoring non-proportional hazards

tbl_4a <- model_4a %>%
  tbl_regression(exponentiate = TRUE,
                 add_estimate_to_reference_rows = TRUE,
                 include = mi_kf) %>%
  add_global_p()
  #table of estimates for stratification

tbl_4b <- model_4b %>%
  tbl_regression(exponentiate = TRUE,
                 add_estimate_to_reference_rows = TRUE,
                 include = mi_kf) %>%
  add_global_p()
  #table of estimates for time-dependent effect

tbl_model4 <- tbl_merge(
  tbls = list(tbl_4, tbl_4a, tbl_4b, tbl_1c),
  tab_spanner = c("**Ignoring non-proportional hazards**", "**Stratification**",
                  "**Time-dependent Variable**", "**Violating variables removed**")) %>%
  modify_caption("Table 5. Comparison of hazard ratio estimates for 
                 mental illness service use under different approaches to handling 
                proportional hazards violation of three variabels in a partial Cox regression")
  #merge all tables

write_csv(tbl_model4 %>% as_tibble(), file.path(output_dir, "Table5.csv"))
#export as csv file

## Comparison of full cox model
# Method 1: Stratification
model_5a <- coxph(
  Surv(time, event) ~ mi_kf + age_cat + sex + strata(ethnicity) + strata(bmicat) + 
    remoteness + strata(deprivation) + ihd + stroke + strata(heart_failure) + 
    pvd + hypertension + cpd + strata(diabetes) + 
    strata(cancer) + strata(liver_disease),
  data = cox_data)
  #stratified categorical variables with a strong violation of the PH assumption

cox.zph(model_5a)
plot(cox.zph(model_5a))

# Method 2: Time-dependent effect
#create model 5b data set
model_5b <- coxph(
  Surv(time, event) ~ mi_kf + age_cat + sex + ethnicity + tt(ethnicity) + bmicat 
  + tt(bmicat) + remoteness + deprivation + tt(deprivation) + ihd + stroke 
  + heart_failure + tt(heart_failure) + pvd + hypertension + cpd + diabetes 
  + tt(diabetes) + cancer + tt(cancer) + liver_disease + tt(liver_disease),
  data = cox_data,
  tt = function(x, t, ...){
    #turning categorical variables into dummy variables inside the function
    mm <- model.matrix(~ x)[, -1, drop = FALSE]
    mm * log(t)})

# Method 3: Removing violating variables
model_5c <- coxph(
  Surv(time, event) ~ mi_kf + age_cat + sex + remoteness + ihd + stroke + pvd 
  + hypertension + cpd,
  data = cox_data)

#Create table comparing all methods
tbl_5 <- cox_model %>%
  tbl_regression(exponentiate = TRUE,
                 add_estimate_to_reference_rows = TRUE,
                 include = mi_kf) %>% #only displaying exposure of interest
  add_global_p()
  #table of estimates for ignoring non-proportional hazards

tbl_5a <- model_5a %>%
  tbl_regression(exponentiate = TRUE,
                 add_estimate_to_reference_rows = TRUE,
                 include = mi_kf) 
  #table of estimates for stratification

tbl_5b <- model_5b %>%
  tbl_regression(exponentiate = TRUE,
                 add_estimate_to_reference_rows = TRUE,
                 include = mi_kf) %>%
  add_global_p()
  #table of estimates for time-dependent effect

tbl_5c <- model_5c %>%
  tbl_regression(exponentiate = TRUE,
                 add_estimate_to_reference_rows = TRUE,
                 include = mi_kf) %>%
  add_global_p()
  #table of estimates for after removing variables that violate PH assumption

tbl_model5 <- tbl_merge(
  tbls = list(tbl_5, tbl_5a, tbl_5b, tbl_5c),
  tab_spanner = c("**Ignoring non-proportional hazards**", "**Stratification**",
                  "**Time-dependent Variable**", "**Violating variables removed**")) %>%
  modify_caption("Table 6. Comparison of hazard ratio estimates for mental illness 
                 service use under different approaches to proportional hazards 
                 violation in a fully adjusted Cox regression")
  #merge all tables

write_csv(tbl_model5 %>% as_tibble(), file.path(output_dir, "Table6.csv"))

#Create table comparing all models using one violating variable
tbl_anal2 <- tbl_stack(tbls = list(tbl_model4, tbl_model5),
                       group_header = c("Partially adjusted model",
                                        "Fullly adjusted model")) %>% 
  modify_caption("Table 7. Comparison of hazard ratio estimates for 
                 mental illness service use under different approaches to handling 
                 non-proportional hazards in Cox regression")

write_csv(tbl_anal2 %>% as_tibble(), file.path(output_dir, "Table7.csv"))
#export as csv file
