## BSTA5030 Research Project - Natasha Alegro
# Title: 2. Cumulative Incidence Functions

# Loading libraries
library(survival) #Surv
library(tidycmprsk) #cumulative incidence table
library(ggsurvfit) #cumulative incidence plot

# Create Figure 1 dataset
crr_data <- analysis_data %>%
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
  # 0 - censor
  # 1 - deceased donor transplant
  # 2 - living donor transplant
  mutate(cmprsk = case_when(
    txtype == "Deceased" ~ 1,
    txtype == "Living" ~ 2,
    TRUE ~ 0)) %>%
  # turn cmprsk variable into factor
  mutate(cmprsk = factor(cmprsk,
                    levels = c(0, 1, 2),
                    labels = c("Censor",
                               "Deceased",
                               "Living"))) %>%
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

#Cumulative incidence table of estimates
tbl_cum <- cuminc(Surv(time, cmprsk) ~ mi_kf, data = crr_data) %>%
  tbl_cuminc(
    times = 5,
    label_header = "**5-year cuminc**") %>%
  add_p() %>%
  modify_caption("**Table 2. Unadjusted cumulative incidence estimate of transplant by mental illness**")

write_csv(tbl_cum %>% as_tibble(), file.path(output_dir, "Table2.csv"))
  #export as csv file

#Cumulative incidence plot
plot_cuminc <- cuminc(Surv(time, cmprsk) ~ mi_kf, data = crr_data) %>%
  ggcuminc() +
  coord_cartesian(xlim = c(0, 5), ylim = c(0, 0.4)) +
  labs(
    title = "Figure 1. Unadjusted cumulative incidence of transplant by mental illness",
    x = "Years",
    y = "Proportion of transplanted") +
  add_confidence_interval() +
  add_risktable() 

ggsave(
  filename = file.path(output_dir, "fig1.png"), 
  plot = plot_cuminc,
  width = 8, height = 7, dpi = 300)
  #export figure as a png

#Kaplan-Meier plot of unadjusted time to transplant by MHSU
survfit_mi <- survfit(Surv(time, event) ~ mi_kf, data = cox_data)
plot_km <- ggsurvplot(survfit_mi, data = cox_data,
                      conf.int = TRUE,
                      xlab = "Time to transplant",
                      ylab = "Probability of remaining transplant-free",
                      pval = TRUE,
                      risk.table = TRUE,
                      legend.title = "Mental Health Service Use",
                      legend.labs = c("None", "Moderate", "Severe"),
                      ggtheme = theme_minimal())