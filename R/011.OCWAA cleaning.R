## Import library
library(tidyverse)
library(haven)


###################################################################### I ### Load data
path <- fs::path("", "Volumes", "Peres_Research", "AACES", "Concept_risk by tumor immune profiles")
clinical_OCWAA <-
  read_sas(paste0(path, 
                    "/data/aaces_inflammation.sas7bdat"))
markers <- 
  read_rds(paste0(path, 
                  "/data/allmarkers_AACES_NCOCS_global.rds"))


###################################################################### II ### Clinical
clinical_OCWAA <- clinical_OCWAA %>% 
  mutate(suid = factor(suid)) %>% 
  mutate(casecon = case_when(
    casecon == 1                                       ~ "Case",
    casecon == 2                                       ~ "Control"
  )) %>% 
  mutate(site = case_when(
    site == "IL" |
      site == "MI"                                     ~ "IL+MI",
    site == "GA" |
      site == "TN"                                     ~ "GA+TN",
    TRUE                                               ~ site
  )) %>% 
  mutate(diagyear = case_when(
    diagyear == 2010 |
      diagyear == 2011                                 ~ "2010-2011",
    TRUE                                               ~ as.character(diagyear)
  )) %>% 
  mutate(refage_cat = case_when(
    refage < 50                      ~ "<50",
    refage >= 50 &
      refage < 60                    ~ "50-59",
    refage >= 60 &
      refage < 70                    ~ "60-69",
    refage >= 70 &
      refage < 80                    ~ "70-79",
    refage >= 80                     ~ "≥80"
  )) %>% 
  mutate(histotype1 = case_when(
    histotype == 1                                     ~ "high-grade serous",
    histotype == 2                                     ~ "low-grade serous",
    histotype == 5 |
      histotype == 10                                  ~ "mucinous",
    histotype == 3                                     ~ "endometrioid",
    histotype == 4                                     ~ "clear cell",
    histotype %in% (6:13)                              ~ "other epithelial", # Will not take the 10
    TRUE                                               ~ NA_character_
  )) %>% 
  mutate(histotype2 = case_when(
    histotype == 1                                     ~ "high-grade serous",
    histotype %in% (2:13)                              ~ "non-high-grade serous",
    TRUE                                               ~ NA_character_
  )) %>%
  mutate(histotype = case_when(
    histotype == 1                                     ~ "high-grade serous",
    histotype == 2                                     ~ "low-grade serous",
    histotype == 3                                     ~ "endometrioid",
    histotype == 4                                     ~ "clear cell",
    histotype == 5                                     ~ "mucinous",
    histotype == 6                                     ~ "carcinosarcoma",
    histotype == 7                                     ~ "other epithelial ovarian cancer \n(e.g. Malignant Brenner, mixed, carcinoma, NOS)",
    histotype == 9                                     ~ "serous borderline",
    histotype == 10                                    ~ "mucinous borderline",
    histotype == 11                                    ~ "other epithelial borderline",
    histotype == 13                                    ~ "synchronous",
    TRUE                                               ~ NA_character_
  )) %>% 
  mutate(education = case_when(
    education == 1                                      ~ "high school graduate/GED or less",
    education == 2                                      ~ "some college",
    education == 3                                      ~ "college graduate",
    education == 4                                      ~ "graduate/professional school",
    TRUE                                                ~ NA_character_
  )) %>% 
  mutate(education = case_when(
    education == "high school graduate/GED or less"|
      education == "some college"                     ~ "didn't graduated",
    education == "college graduate"|
      education == "graduate/professional school"     ~ "graduated"
  )) %>% 
  mutate(education = factor(education, levels = c("graduated", "didn't graduated"))) %>% 
  mutate(hysterreason = case_when(
    hysterreason == 1                                      ~ "Ovarian/FT/peritoneal cancer diagnosis",
    hysterreason == 2                                      ~ "Any reason not due to ovarian/FT/peritoneal cancer diagnosis",
    TRUE                                                ~ NA_character_
  )) %>% 
  mutate(smokever = case_when(
    smokever == 1                                       ~ "ever",
    smokever == 2                                       ~ "never",
    TRUE                                                ~ NA_character_
  )) %>% 
  mutate(smokever = factor(smokever, levels = c("never", "ever"))) %>% 
  mutate(smokcurrent = case_when(
    smokcurrent == 1                                    ~ "current",
    smokcurrent == 2                                    ~ "never",
    smokcurrent == 3                                    ~ "former",
    TRUE                                                ~ NA_character_
  )) %>% 
  mutate(smokcurrent = factor(smokcurrent, levels = c("never", "former", "current"))) %>% 
  mutate(packyrs_cat = case_when(
    smokcurrent == "never"               ~ "none",
    packyrs <= 15                        ~ "inf_eq_15",
    packyrs > 15                         ~ "sup_15"
  )) %>%
  mutate(packyrs_cat = factor(packyrs_cat, levels = c("none", "inf_eq_15", "sup_15"))) %>% 
  mutate(inactive = case_when(
    inactive == 1                                       ~ "Less than 2 hours of physical activity per week",
    inactive == 2                                       ~ "2+ hours of physical activity per week",
    TRUE                                                ~ NA_character_
  )) %>% 
  mutate_at(c("pregnum", "fullpregnum", "hysterage", "tubeligage", "ocdur",
              "talcgenfreq", "talcgendur", "talcnongenfreq", "talcnongendur",
              "endomet_age", "fibroids_age", "pid_age", "diabage", "hrtdisage", 
              "hbpage", "hcholage", "BMI_recent", "BMI_YA"), 
            ~ case_when(
              . %in% c("88","98", "99", 
                       "888", "998", "999")            ~ NA_real_,
              TRUE                                     ~ as.numeric(.)
            )) %>% 
  mutate_at(c("pregever",
              "nulliparous", "hyster", "hyster1yr", "hyster2yr", "hystertype",
              "tubelig", "tubelig1yr", "ocever", "talcever",
              "famhxbr", "famhxov", "endomet", "fibroids",
              "pid", "aspirin", "NSAID", "aceta", "diab", "hrtdis", "hbp",
              "hchol", "paga"), 
            ~ case_when(
              . == 1                                              ~ "YES",
              . == 2                                              ~ "NO",
              TRUE                                                ~ NA_character_
            )) %>% 
  mutate(talcgen = case_when(
    talcever == "NO"                                          ~ "never",
    talcgen == 1                                              ~ "talc user applied to only genital areas",
    talcgen == 2                                              ~ "talc user applied to other non-genital areas",
    TRUE                                                      ~ NA_character_
  ), talcgen = factor(talcgen, levels = c("never",
                                          "talc user applied to only genital areas",
                                          "talc user applied to other non-genital areas"))
  ) %>% 
  mutate(talcnongen = case_when(
    talcever == "NO"                                          ~ "never",
    talcnongen == 1                                           ~ "talc user applied only to non-genital areas",
    talcnongen == 2                                           ~ "talc user applied to other areas",
    TRUE                                                      ~ NA_character_
  ), talcnongen = factor(talcnongen, levels = c("never",
                                                "talc user applied only to non-genital areas",
                                                "talc user applied to other areas"))
  ) %>% 
  mutate(ctrl_loc = case_when(
    casecon == "Control"               ~ loc,
    TRUE                               ~ NA_real_
  )) %>% 
  mutate(tertile33 = quantile(ctrl_loc, probs = c(0.333, 0.666), na.rm = TRUE)[1]
  ) %>% 
  mutate(tertile66 = quantile(ctrl_loc, probs = c(0.333, 0.666), na.rm = TRUE)[2]
  ) %>% 
  mutate(tertile_loc = case_when(
    loc < tertile33           ~ "Low",
    loc >= tertile33 &
      loc < tertile66         ~ "Medium",
    loc >= tertile66          ~ "High",
  )) %>% 
  mutate(BMI_recent_grp = case_when(
    BMI_recent < 25                  ~ "<25",
    BMI_recent >= 25 &
      BMI_recent < 30                ~ "25-29",
    BMI_recent >= 30 &
      BMI_recent < 35                ~ "30-35",
    BMI_recent >= 35                 ~ "≥35"
  )) %>% 
  mutate(BMI_recent_grp = factor(BMI_recent_grp, levels = c("<25", "25-29", "30-35", "≥35"))) %>% 
  mutate(BMI_YA_grp = case_when(
    BMI_YA < 20                      ~ "<20",
    BMI_YA >= 20 &
      BMI_YA < 25                    ~ "20-24",
    BMI_YA >= 25                     ~ "≥25"
  )) %>% 
  mutate(BMI_YA_grp = factor(BMI_YA_grp, levels = c("<20", "20-24", "≥25"))) %>% 
  mutate(BMI_recent_5 = BMI_recent / 5) %>% 
  mutate(BMI_YA_5 = BMI_YA / 5) %>% 
  mutate(BMI_YA_2grp = case_when(
    BMI_YA < 30                      ~ "inf-30",
    BMI_YA >= 30                     ~ "sup-eq-30"
  )) %>% 
  select(-c(tertile33, tertile66))
  
write_rds(clinical_OCWAA, "clinical_OCWAA.rds")


###################################################################### II ### Markers
markers <- markers %>% 
  # Remove bad slides
  filter(!image_tag %in% c("Peres_P1_150163  3D_[44113,18531].tif", 
                           "Peres_P1_43004 A2_[45654,5840].tif",
                           "Peres_P1_43773 D2_[60849,15244].tif", 
                           "Peres_P1_AACES 130033_[37754,17929].tif",
                           "Peres_P1_AACES 130033_[38351,13944].tif"
  )) %>% 
  mutate(percent_tumor = round((tumor_total / total)*100, 2) # Calculate percent of tumor cell
  ) %>% 
  mutate(percent_stroma = round((stroma_total / total)*100, 2) # Calculate percent of stromal cell
  )



suid_summarize <- function(data){
  data <- data %>% 
    group_by(suid, slide_type) %>% 
    summarize(
      mean_tumor = mean(percent_tumor),
      mean_stroma = mean(percent_stroma),
      percent_CD3_tumor = mean(tumor_percent_cd3),
      percent_CD8_tumor = mean(tumor_percent_cd8),
      percent_CD3_CD8_tumor = mean(tumor_percent_cd3plus_cd8plus),
      percent_FoxP3_tumor = mean(tumor_percent_foxp3),
      percent_CD3_FoxP3_tumor = mean(tumor_percent_cd3plus_foxp3plus),
      percent_CD11b_tumor = mean(tumor_percent_cd11b),
      percent_CD15_tumor = mean(tumor_percent_cd15),
      percent_CD11b_CD15_tumor = mean(tumor_percent_cd11bplus_cd15plus),
      percent_CD3_stroma = mean(stroma_percent_cd3),
      percent_CD8_stroma = mean(stroma_percent_cd8),
      percent_CD3_CD8_stroma = mean(stroma_percent_cd3plus_cd8plus),
      percent_FoxP3_stroma = mean(stroma_percent_foxp3),
      percent_CD3_FoxP3_stroma = mean(stroma_percent_cd3plus_foxp3plus),
      percent_CD11b_stroma = mean(stroma_percent_cd11b),
      percent_CD15_stroma = mean(stroma_percent_cd15),
      percent_CD11b_CD15_stroma = mean(stroma_percent_cd11bplus_cd15plus),
      percent_CD3_total = mean(total_percent_cd3),
      percent_CD8_total = mean(total_percent_cd8),
      percent_CD3_CD8_total = mean(total_percent_cd3plus_cd8plus),
      percent_FoxP3_total = mean(total_percent_foxp3),
      percent_CD3_FoxP3_total = mean(total_percent_cd3plus_foxp3plus),
      percent_CD11b_total = mean(total_percent_cd11b),
      percent_CD15_total = mean(total_percent_cd15),
      percent_CD11b_CD15_total = mean(total_percent_cd11bplus_cd15plus)
    )
}

markers_TMA <- markers %>% 
  filter(slide_type == "TMA")
  
markers_ROIi <- markers %>% 
  filter(annotation == "Intratumoral")
markers_ROIp <- markers %>% 
  filter(annotation == "Peripheral")

markers_TMA <- suid_summarize(markers_TMA)
markers_ROIi <- suid_summarize(markers_ROIi)
markers_ROIp <- suid_summarize(markers_ROIp)


###################################################################### III ### Merge
colnames(markers_TMA)[3:ncol(markers_TMA)] <- paste(
  colnames(markers_TMA)[3:ncol(markers_TMA)], "tma", sep = "_")
# Join Intratumoral and Peropheral ROI
markers <- full_join(markers_ROIi, markers_ROIp,
                         by= c("suid", "slide_type"),
                         suffix= c(".i", ".p")) %>% 
  full_join(., markers_TMA, 
            by= c("suid")) %>% 
  unite("slide_type", c(slide_type.x, slide_type.y), 
        sep = " + ",
        remove = FALSE, na.rm = TRUE)

write_rds(markers, file = "markers_AACES_06172022.rds")

OCWAA_markers <- full_join(markers, clinical_OCWAA, by = "suid")

write_rds(OCWAA_markers, file = "markers_AACES_clinical_OCWAA.rds")


# END
