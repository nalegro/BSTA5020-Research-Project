## BSTA5030 Research Project - Natasha Alegro
# Title: 4. Analysis 1

#Load libraries
library(nnet)
library(car)

#Testing the proportional hazards assumption
cox.zph(cox_model)
  #variables that violate PH assumption with a p < 0.05 are
    #ethnicity, bmicat, deprivation, heart_failure, diabetes, cancer, liver_disease

#Visual representation shows violation
par(mfrow = c(1,1))
plot(cox.zph(cox_model))

## Comparison of variable with one severe PH violation
model_1 <- coxph(
  Surv(time, event) ~ mi_kf + age_cat + sex + ethnicity,
  data = cox_data)
  #p = 1.6e-06

cox.zph(model_1)
plot(cox.zph(model_1))

#analyse association between MHSU and ethnicity
model_1_multnom <- multinom(mi_kf ~ ethnicity + age_cat + sex, data = cox_data)
tidy(model_1_multnom, exponentiate = TRUE, conf.int = TRUE)
Anova(model_1_multnom)
  #ethnicity is association with both time to transplant and MHSU

# Method 1: Stratification
model_1a <- coxph(
  Surv(time, event) ~ mi_kf + age_cat + sex + strata(ethnicity),
  data = cox_data)
  #stratified variable ethnicity

cox.zph(model_1a)
plot(cox.zph(model_1a))

# Method 2: Time-dependent effect
#create model 1b data set
model_1b <- coxph(
  Surv(time, event) ~ mi_kf + age_cat + sex + ethnicity + tt(ethnicity),
  data = cox_data,
  tt = function(x, t, ...){
    #turning categorical variables into dummy variables inside the function
    mm <- model.matrix(~ x)[, -1, drop = FALSE] 
    mm * log(t)})
  #ethnicity variable modelled as a time-dependent effect

# Method 3: Removing violating variables
model_1c <- coxph(
  Surv(time, event) ~ mi_kf + age_cat + sex,
  data = cox_data)
  #ethnicity

#Create table comparing all methods
tbl_1 <- model_1 %>%
  tbl_regression(exponentiate = TRUE,
                 add_estimate_to_reference_rows = TRUE,
                 include = mi_kf) %>% #only displaying exposure of interest
  add_global_p()
  #table of estimates for ignoring non-proportional hazards

tbl_1a <- model_1a %>%
  tbl_regression(exponentiate = TRUE,
                 add_estimate_to_reference_rows = TRUE,
                 include = mi_kf) %>%
  add_global_p()
  #table of estimates for stratification

tbl_1b <- model_1b %>%
  tbl_regression(exponentiate = TRUE,
                 add_estimate_to_reference_rows = TRUE,
                 include = mi_kf) %>%
  add_global_p()
  #table of estimates for time-dependent effect

tbl_1c<- model_1c %>%
  tbl_regression(exponentiate = TRUE,
                 add_estimate_to_reference_rows = TRUE,
                 include = mi_kf) %>%
  add_global_p()
  #table of estimates for after removing variables that violate PH assumption

tbl_model1 <- tbl_merge(
  tbls = list(tbl_1, tbl_1a, tbl_1b, tbl_1c),
  tab_spanner = c("**Ignoring non-proportional hazards**", "**Stratification**",
                  "**Time-dependent Variable**", "**Violating variables removed**")) %>%
  modify_caption("Table 4a. Comparison of hazard ratio estimates for 
                 mental illness service use under different approaches to handling 
                 severe proportional hazards violation in a partial Cox regression")
  #merge all tables

write_csv(tbl_model1 %>% as_tibble(), file.path(output_dir, "Table4a.csv"))
  #export as csv file

## Comparison of variable with one moderate PH violation
model_2 <- coxph(
  Surv(time, event) ~ mi_kf + age_cat + sex + deprivation,
  data = cox_data)
  #p-value = 0.00581

cox.zph(model_2)
plot(cox.zph(model_2))

#analyse association between MHSU and deprivation 
model_2_multnom <- multinom(mi_kf ~ deprivation + age_cat + sex, data = cox_data)
tidy(model_2_multnom, exponentiate = TRUE, conf.int = TRUE)
Anova(model_2_multnom)
  #deprivation is association with both time to transplant and MHSU

# Method 1: Stratification
model_2a <- coxph(
  Surv(time, event) ~ mi_kf + age_cat + sex + strata(deprivation),
  data = cox_data)
  #stratified variable deprivation with a moderate violation of the PH assumption

cox.zph(model_2a)
plot(cox.zph(model_2a))

# Method 2: Time-dependent effect
model_2b <- coxph(
  Surv(time, event) ~ mi_kf + age_cat + sex + deprivation + tt(deprivation),
  data = cox_data,
  tt = function(x, t, ...){
    #turning categorical variables into dummy variables inside the function
    mm <- model.matrix(~ x)[, -1, drop = FALSE]
    mm * log(t)})
  #deprivation modelled as a time-dependent effect

#Create table comparing all methods
tbl_2 <- model_2 %>%
  tbl_regression(exponentiate = TRUE,
                 add_estimate_to_reference_rows = TRUE,
                 include = mi_kf) %>% #only displaying exposure of interest
  add_global_p()
  #table of estimates for ignoring non-proportional hazards

tbl_2a <- model_2a %>%
  tbl_regression(exponentiate = TRUE,
                 add_estimate_to_reference_rows = TRUE,
                 include = mi_kf) %>%
  add_global_p()
  #table of estimates for stratification

tbl_2b <- model_2b %>%
  tbl_regression(exponentiate = TRUE,
                 add_estimate_to_reference_rows = TRUE,
                 include = mi_kf) %>%
  add_global_p()
  #table of estimates for time-dependent effect

tbl_model2 <- tbl_merge(
  tbls = list(tbl_2, tbl_2a, tbl_2b, tbl_1c),
  tab_spanner = c("**Ignoring non-proportional hazards**", "**Stratification**",
                  "**Time-dependent Variable**", "**Violating variables removed**")) %>%
  modify_caption("Table 4b. Comparison of hazard ratio estimates for 
                 mental illness service use under different approaches to handling 
                 moderate proportional hazards violation in a partial Cox regression")
  #merge all tables

write_csv(tbl_model2 %>% as_tibble(), file.path(output_dir, "Table4b.csv"))
  #export as csv file

## Comparison of variable with one mild PH violation
model_3 <- coxph(
  Surv(time, event) ~ mi_kf + age_cat + sex + diabetes,
  data = cox_data)
  #p-value= 0.04605

cox.zph(model_3)
plot(cox.zph(model_3))

#analyse association between MHSU and diabetes
model_3_multnom <- multinom(mi_kf ~ diabetes + age_cat + sex, data = cox_data)
tidy(model_3_multnom, exponentiate = TRUE, conf.int = TRUE)
Anova(model_3_multnom)
  #diabetes is associated with both time to transplant and MHSU

# Method 1: Stratification
model_3a <- coxph(
  Surv(time, event) ~ mi_kf + age_cat + sex + strata(diabetes),
  data = cox_data)
  #stratified variable with a mild violation of the PH assumption
  #p-value = 0.04605

cox.zph(model_3a)
plot(cox.zph(model_3a))

# Method 2: Time-dependent effect
model_3b <- coxph(
  Surv(time, event) ~ mi_kf + age_cat + sex + diabetes + tt(diabetes),
  data = cox_data,
  tt = function(x, t, ...)x*log(t))
  #diabetes modelled as a time-dependent effect

#Create table comparing all methods
tbl_3 <- model_3 %>%
  tbl_regression(exponentiate = TRUE,
                 add_estimate_to_reference_rows = TRUE,
                 include = mi_kf) %>% #only displaying exposure of interest
  add_global_p()
  #table of estimates for ignoring non-proportional hazards

tbl_3a <- model_3a %>%
  tbl_regression(exponentiate = TRUE,
                 add_estimate_to_reference_rows = TRUE,
                 include = mi_kf) %>%
  add_global_p()
  #table of estimates for stratification

tbl_3b <- model_3b %>%
  tbl_regression(exponentiate = TRUE,
                 add_estimate_to_reference_rows = TRUE,
                 include = mi_kf) %>%
  add_global_p()
  #table of estimates for time-dependent effect

tbl_model3 <- tbl_merge(
  tbls = list(tbl_3, tbl_3a, tbl_3b, tbl_1c),
  tab_spanner = c("**Ignoring non-proportional hazards**", "**Stratification**",
                  "**Time-dependent Variable**", "**Violating variables removed**")) %>%
  modify_caption("Table 4c. Comparison of hazard ratio estimates for 
                 mental illness service use under different approaches to handling 
                 mild proportional hazards violation in a partial Cox regression")
  #merge all tables

write_csv(tbl_model3 %>% as_tibble(), file.path(output_dir, "Table4c.csv"))
  #export as csv file

#Create table comparing all approaches depending of severity of violating variable
tbl_anal1 <- tbl_stack(tbls = list(tbl_model1, tbl_model2, tbl_model3),
                       group_header = c("Severe violation",
                                        "Moderate violation",
                                        "Mild violation")) %>% 
  modify_caption("Table 4. Comparison of hazard ratio estimates for 
                 mental illness service use under different approaches to handling 
                 non-proportional hazards in Cox regression")

write_csv(tbl_anal1 %>% as_tibble(), file.path(output_dir, "Table4.csv"))
  #export as csv file
