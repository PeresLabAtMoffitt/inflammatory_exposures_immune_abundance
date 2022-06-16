markers_ROI <- readRDS(paste0(here::here(), "/markers_clinical_ROI_allAACES_OCWAA.rds"))
###################################################################### Create category and new var
markers_ROI <- markers_ROI %>% 
  
  mutate(CD3_tumor.i = case_when(
    percent_CD3_tumor.i <= 1      ~ "low",
    percent_CD3_tumor.i > 1       ~ "high"
  ), CD3_tumor.i = factor(CD3_tumor.i, levels = c("low","high"))) %>% 
  mutate(CD3_CD8_tumor.i = case_when(
    percent_CD3_CD8_tumor.i <= 1      ~ "low",
    percent_CD3_CD8_tumor.i > 1       ~ "high"
  ), CD3_CD8_tumor.i = factor(CD3_CD8_tumor.i, levels = c("low","high"))) %>% 
  mutate(CD3_FoxP3_tumor.i = case_when(
    percent_CD3_FoxP3_tumor.i <= 1      ~ "low",
    percent_CD3_FoxP3_tumor.i > 1       ~ "high"
  ), CD3_FoxP3_tumor.i = factor(CD3_FoxP3_tumor.i, levels = c("low","high"))) %>% 
  mutate(CD11b_tumor.i = case_when(
    percent_CD11b_tumor.i == 0      ~ "Absence",
    percent_CD11b_tumor.i > 0       ~ "Presence"
  )) %>% 
  mutate(CD11b_CD15_tumor.i = case_when(
    percent_CD11b_CD15_tumor.i == 0      ~ "Absence",
    percent_CD11b_CD15_tumor.i > 0       ~ "Presence"
  )) %>% 
  
  mutate(CD3_stroma.i = case_when(
    percent_CD3_stroma.i <= 1      ~ "low",
    percent_CD3_stroma.i > 1       ~ "high"
  ), CD3_stroma.i = factor(CD3_stroma.i, levels = c("low","high"))) %>% 
  mutate(CD3_CD8_stroma.i = case_when(
    percent_CD3_CD8_stroma.i <= 1      ~ "low",
    percent_CD3_CD8_stroma.i > 1       ~ "high"
  ), CD3_CD8_stroma.i = factor(CD3_CD8_stroma.i, levels = c("low","high"))) %>% 
  mutate(CD3_FoxP3_stroma.i = case_when(
    percent_CD3_FoxP3_stroma.i <= 1      ~ "low",
    percent_CD3_FoxP3_stroma.i > 1       ~ "high"
  ), CD3_FoxP3_stroma.i = factor(CD3_FoxP3_stroma.i, levels = c("low","high"))) %>% 
  mutate(CD11b_stroma.i = case_when(
    percent_CD11b_stroma.i == 0      ~ "Absence",
    percent_CD11b_stroma.i > 0       ~ "Presence"
  )) %>%
  mutate(CD11b_CD15_stroma.i = case_when(
    percent_CD11b_CD15_stroma.i == 0      ~ "Absence",
    percent_CD11b_CD15_stroma.i > 0       ~ "Presence"
  )) %>% 
  mutate(CD11b_stroma.i3 = case_when(
    percent_CD11b_stroma.i <= 1      ~ "low",
    percent_CD11b_stroma.i > 1       ~ "high"
  ), CD11b_stroma.i3 = factor(CD11b_stroma.i3, levels = c("low","high"))) %>%
  mutate(CD11b_CD15_stroma.i3 = case_when(
    percent_CD11b_CD15_stroma.i <= 1      ~ "low",
    percent_CD11b_CD15_stroma.i > 1       ~ "high"
  ), CD11b_CD15_stroma.i3 = factor(CD11b_CD15_stroma.i3, levels = c("low","high"))) %>% 
  mutate(CD3_total.i = case_when(
    percent_CD3_total.i <= 1      ~ "low",
    percent_CD3_total.i > 1       ~ "high"
  ), CD3_total.i = factor(CD3_total.i, levels = c("low","high"))) %>% 
  mutate(CD3_CD8_total.i = case_when(
    percent_CD3_CD8_total.i <= 1      ~ "low",
    percent_CD3_CD8_total.i > 1       ~ "high"
  ), CD3_CD8_total.i = factor(CD3_CD8_total.i, levels = c("low","high"))) %>% 
  mutate(CD3_FoxP3_total.i = case_when(
    percent_CD3_FoxP3_total.i <= 1      ~ "low",
    percent_CD3_FoxP3_total.i > 1       ~ "high"
  ), CD3_FoxP3_total.i = factor(CD3_FoxP3_total.i, levels = c("low","high"))) %>% 
  mutate(CD11b_total.i = case_when(
    percent_CD11b_total.i == 0      ~ "Absence",
    percent_CD11b_total.i > 0       ~ "Presence"
  )) %>% 
  mutate(CD11b_CD15_total.i = case_when(
    percent_CD11b_CD15_total.i == 0      ~ "Absence",
    percent_CD11b_CD15_total.i > 0       ~ "Presence"
  )) %>% 
  mutate(CD11b_total.i3 = case_when(
    percent_CD11b_total.i <= 1      ~ "low",
    percent_CD11b_total.i > 1       ~ "high"
  ), CD11b_total.i3 = factor(CD11b_total.i3, levels = c("low","high"))) %>% 
  mutate(CD11b_CD15_total.i3 = case_when(
    percent_CD11b_CD15_total.i <= 1      ~ "low",
    percent_CD11b_CD15_total.i > 1       ~ "high"
  ), CD11b_CD15_total.i3 = factor(CD11b_CD15_total.i3, levels = c("low","high"))) %>% 
  
  
  mutate(CD3_tumor.p = case_when(
    percent_CD3_tumor.p <= 1      ~ "low",
    percent_CD3_tumor.p > 1       ~ "high"
  ), CD3_tumor.p = factor(CD3_tumor.p, levels = c("low","high"))) %>% 
  mutate(CD3_CD8_tumor.p = case_when(
    percent_CD3_CD8_tumor.p <= 1      ~ "low",
    percent_CD3_CD8_tumor.p > 1       ~ "high"
  ), CD3_CD8_tumor.p = factor(CD3_CD8_tumor.p, levels = c("low","high"))) %>% 
  mutate(CD3_FoxP3_tumor.p = case_when(
    percent_CD3_FoxP3_tumor.p <= 1      ~ "low",
    percent_CD3_FoxP3_tumor.p > 1       ~ "high"
  ), CD3_FoxP3_tumor.p = factor(CD3_FoxP3_tumor.p, levels = c("low","high"))) %>% 
  mutate(CD11b_tumor.p = case_when(
    percent_CD11b_tumor.p == 0      ~ "Absence",
    percent_CD11b_tumor.p > 0       ~ "Presence"
  )) %>% 
  mutate(CD11b_CD15_tumor.p = case_when(
    percent_CD11b_CD15_tumor.p == 0      ~ "Absence",
    percent_CD11b_CD15_tumor.p > 0       ~ "Presence"
  )) %>% 
  mutate(CD11b_tumor.p3 = case_when(
    percent_CD11b_tumor.p <= 1      ~ "low",
    percent_CD11b_tumor.p > 1         ~ "high"
  ), CD11b_tumor.p3 = factor(CD11b_tumor.p3, levels = c("low","high"))) %>% 
  mutate(CD11b_CD15_tumor.p3 = case_when(
    percent_CD11b_CD15_tumor.p <= 1      ~ "low",
    percent_CD11b_CD15_tumor.p > 1       ~ "high"
  ), CD11b_CD15_tumor.p3 = factor(CD11b_CD15_tumor.p3, levels = c("low","high"))) %>% 
  mutate(CD3_stroma.p = case_when(
    percent_CD3_stroma.p <= 1      ~ "low",
    percent_CD3_stroma.p > 1       ~ "high"
  ), CD3_stroma.p = factor(CD3_stroma.p, levels = c("low","high"))) %>% 
  mutate(CD3_CD8_stroma.p = case_when(
    percent_CD3_CD8_stroma.p <= 1      ~ "low",
    percent_CD3_CD8_stroma.p > 1       ~ "high"
  ), CD3_CD8_stroma.p = factor(CD3_CD8_stroma.p, levels = c("low","high"))) %>% 
  mutate(CD3_FoxP3_stroma.p = case_when(
    percent_CD3_FoxP3_stroma.p <= 1      ~ "low",
    percent_CD3_FoxP3_stroma.p > 1       ~ "high"
  ), CD3_FoxP3_stroma.p = factor(CD3_FoxP3_stroma.p, levels = c("low","high"))) %>% 
  mutate(CD11b_stroma.p = case_when(
    percent_CD11b_stroma.p == 0      ~ "Absence",
    percent_CD11b_stroma.p > 0       ~ "Presence"
  )) %>% 
  mutate(CD11b_CD15_stroma.p = case_when(
    percent_CD11b_CD15_stroma.p == 0      ~ "Absence",
    percent_CD11b_CD15_stroma.p > 0       ~ "Presence"
  )) %>% 
  mutate(CD11b_stroma.p3 = case_when(
    percent_CD11b_stroma.p <= 1      ~ "low",
    percent_CD11b_stroma.p > 1       ~ "high"
  ), CD11b_stroma.p3 = factor(CD11b_stroma.p3, levels = c("low","high"))) %>% 
  mutate(CD11b_CD15_stroma.p3 = case_when(
    percent_CD11b_CD15_stroma.p <= 1      ~ "low",
    percent_CD11b_CD15_stroma.p > 1       ~ "high"
  ), CD11b_CD15_stroma.p3 = factor(CD11b_CD15_stroma.p3, levels = c("low","high"))) %>% 
  mutate(CD3_total.p = case_when(
    percent_CD3_total.p <= 1      ~ "low",
    percent_CD3_total.p > 1       ~ "high"
  ), CD3_total.p = factor(CD3_total.p, levels = c("low","high"))) %>% 
  mutate(CD3_CD8_total.p = case_when(
    percent_CD3_CD8_total.p <= 1      ~ "low",
    percent_CD3_CD8_total.p > 1       ~ "high"
  ), CD3_CD8_total.p = factor(CD3_CD8_total.p, levels = c("low","high"))) %>% 
  mutate(CD3_FoxP3_total.p = case_when(
    percent_CD3_FoxP3_total.p <= 1      ~ "low",
    percent_CD3_FoxP3_total.p > 1       ~ "high"
  ), CD3_FoxP3_total.p = factor(CD3_FoxP3_total.p, levels = c("low","high"))) %>% 
  mutate(CD11b_total.p = case_when(
    percent_CD11b_total.p == 0      ~ "Absence",
    percent_CD11b_total.p > 0       ~ "Presence"
  )) %>% 
  mutate(CD11b_CD15_total.p = case_when(
    percent_CD11b_CD15_total.p == 0      ~ "Absence",
    percent_CD11b_CD15_total.p > 0       ~ "Presence"
  )) %>% 
  mutate(CD11b_total.p3 = case_when(
    percent_CD11b_total.p <= 1      ~ "low",
    percent_CD11b_total.p > 1       ~ "high"
  ), CD11b_total.p3 = factor(CD11b_total.p3, levels = c("low","high"))) %>% 
  mutate(CD11b_CD15_total.p3 = case_when(
    percent_CD11b_CD15_total.p <= 1      ~ "low",
    percent_CD11b_CD15_total.p > 1       ~ "high"
  ), CD11b_CD15_total.p3 = factor(CD11b_CD15_total.p3, levels = c("low","high"))) %>% 
  mutate(across(where(is.character), ~ as.factor(.))) %>% 
  mutate(BMI_recent_5 = BMI_recent / 5) %>% 
  mutate(BMI_YA_5 = BMI_YA / 5)


markers_ROI <- markers_ROI %>% 
  mutate(percentile_score_CD3_i = ntile(percent_CD3_total.i, 100) ) %>% 
  mutate(percentile_score_CD3_p = ntile(percent_CD3_total.p, 100) ) %>% 
  mutate(percentile_score_CD8_i = ntile(percent_CD8_total.i, 100) ) %>% 
  mutate(percentile_score_CD8_p = ntile(percent_CD8_total.p, 100) ) 
markers_ROI <- markers_ROI %>%
  mutate(percentile_score_mean = rowMeans( markers_ROI[c("percentile_score_CD3_i", "percentile_score_CD3_p", 
                                                         "percentile_score_CD8_i", "percentile_score_CD8_p")] ))
markers_ROI <- markers_ROI %>%
  # mutate(immunoscore_patients = case_when(
  #   percentile_score_mean <= 10        ~ 0,
  #   percentile_score_mean <= 25        ~ 1,
  #   percentile_score_mean <= 70        ~ 2,
  #   percentile_score_mean <= 95        ~ 3,
  #   percentile_score_mean > 95         ~ 4 
  # )) %>% 
  # mutate(immunoscore_patients = factor(immunoscore_patients)) %>% 
  mutate(immunoscore_2018lancet_patients = case_when(
    percentile_score_mean <= 25        ~ "Low",
    percentile_score_mean <= 70        ~ "Intermediate",
    percentile_score_mean > 70         ~ "High"
  )) %>% 
  mutate(immunoscore_2018lancet_patients = factor(immunoscore_2018lancet_patients, levels = c("Low", "Intermediate", "High"))) 


write_rds(markers_ROI, "markers_ROI_category.rds")

