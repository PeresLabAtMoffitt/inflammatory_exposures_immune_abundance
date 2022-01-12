# Import data
library(tidyverse)

# Load data
clinical_data <- readRDS(paste0(here::here(), "/clinical_data.rds"))

####################################### Data cleaning
clinical_data1 <- clinical_data %>% 
  mutate(smokever = factor(smokever, levels = c("never", "ever"))) %>% 
  mutate(smokcurrent = factor(smokcurrent, levels = c("never", "former", "current"))) %>% 
  mutate(packyrs = case_when(
    smokcurrent == "never"               ~ 0, 
    TRUE                                 ~ packyrs
  )) %>% 
  mutate(education = case_when(
    education == "high school graduate/GED or less"|
      education == "some college"                     ~ "didn't graduated",
    education == "college graduate"|
      education == "graduate/professional"            ~ "graduated"
  )) %>% 
  mutate(education = factor(education, levels = c("graduated", "didn't graduated"))) %>% 
  mutate(anyfhdur = case_when(
    anyfhever == "no"                    ~ 0, 
    TRUE                                 ~ anyfhdur
  )) %>% 
  mutate(BMI_recent_grp = case_when(
    BMI_recent <25                       ~ "<25 kg/m2",
    BMI_recent >=25 & BMI_recent <= 30   ~ "25-30 kg/m2",
    BMI_recent > 30.0                    ~ ">30 kg/m2"
  )) %>% 
  mutate(BMI_YA_grp = case_when(
    BMI_YA <25                           ~ "<25 kg/m2",
    BMI_YA >=25 & BMI_YA <= 30           ~ "25-30 kg/m2",
    BMI_YA > 30.0                        ~ ">30 kg/m2"
  ))
  select(suid, BMI_recent, BMI_YA, "BMI_recent_grp", "BMI_YA_grp")


"BMI_recent_grp", "BMI_YA_grp"










write_rds(clinical_data, "/clinical_data.rds")
