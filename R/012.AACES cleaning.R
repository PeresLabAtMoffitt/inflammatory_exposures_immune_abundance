# Import data
library(tidyverse)

# Load data
clinical_data <- readRDS(paste0(here::here(), "/clinical_data.rds"))

####################################### Data cleaning
clinical_data1 <- clinical_data %>% 
  mutate(smokever = factor(smokever, levels = c("never", "ever"))) %>% 
  mutate(smokcurrent = factor(smokcurrent, levels = c("never", "former", "current"))) %>% 
  mutate(packyrs_cat = case_when(
    packyrs <= 15                        ~ "≤ 15",
    packyrs > 15                         ~ "> 15",
    smokcurrent == "never"               ~ "none"
  )) %>%
  mutate(packyrs_cat = factor(packyrs_cat, levels = c("none", "≤ 15", "> 15"))) %>% 
  mutate(education = case_when(
    education == "high school graduate/GED or less"|
      education == "some college"                     ~ "didn't graduated",
    education == "college graduate"|
      education == "graduate/professional"            ~ "graduated"
  )) %>% 
  mutate(education = factor(education, levels = c("graduated", "didn't graduated"))) %>% 
  mutate(anyfhdur_cat = case_when(
    anyfhdur <= 100                      ~ "≤ 100",
    anyfhdur > 100                       ~ "> 100",
    anyfhever == "no"                    ~ "none"
  )) %>%
  mutate(anyfhdur_cat = factor(anyfhdur_cat, levels = c("none", "≤ 100", "> 100"))) %>% 
  mutate(BMI_recent_grp = case_when(
    BMI_recent <25                       ~ "<25 kg/m2",
    BMI_recent >=25 & BMI_recent <= 30   ~ "25-30 kg/m2",
    BMI_recent > 30.0                    ~ ">30 kg/m2"
  )) %>% 
  mutate(BMI_YA_grp = case_when(
    BMI_YA <25                           ~ "<25 kg/m2",
    BMI_YA >=25 & BMI_YA <= 30           ~ "25-30 kg/m2",
    BMI_YA > 30.0                        ~ ">30 kg/m2"
  )) %>% 
  mutate(stage = case_when(
    stage == "Localized"                 ~ "Regional",
    TRUE                                 ~ stage
  )) %>% 
  mutate(married = case_when(
    married == "married/living as married"      ~ "married",
    TRUE                                        ~ "others"
  ))









write_rds(clinical_data1, "clinical_data1.rds")
