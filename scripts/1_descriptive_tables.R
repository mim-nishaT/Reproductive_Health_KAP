# Load packages 
library(tidyverse)
library(gtsummary)
library(flextable)
library(officer)
library(likert)
library(gt)

# Descriptive tables 
# Load data into R 
data <- readxl::read_excel("data/DHI_Access_Health_Literacy.xlsx")

data |> 
  dplyr::select(1:9) %>% 
  tbl_summary(missing = "no", 
              type = everything() ~ "categorical") %>% 
  as_gt() |> 
  gtsave("tables/1_Demogrphic_summary.docx")



# Table 2: Demographic characteristics 
table2 <- data %>% 
  dplyr::select(1:9) %>% 
  tbl_summary(missing = "no", 
              type = everything() ~ "categorical") %>% 
  as_flex_table() %>% 
  set_table_properties(width = 1, layout = "autofit")

table2
# Export table 1 
save_as_docx(table2, path = "tables/2_Demographic_summary.docx")



# Selecting the four total scores and short names
kap <- data %>%
  transmute(
    `Digital Health Information Access (0-18)` = as.numeric(TotalDigitalHealthInformationAccess),
    `Knowledge (0-8)`                          = as.numeric(TotalKnowledgeScore),
    `Attitude (9-45)`                          = as.numeric(TotalAttitudeScore),
    `Practice (10-50)`                         = as.numeric(TotalPracticeScore)
  )

# Optional: composite KAP score (Knowledge % + Attitude % + Practice %) / 3
kap_pct <- data %>%
  transmute(
    K = as.numeric(KnowledgeScorePCT),
    A = as.numeric(TotalAttitudeScore) / 45 * 100,
    P = as.numeric(PracticeScorePTC)
  )
kap$`Composite KAP (%)` <- rowMeans(kap_pct, na.rm = FALSE)

# Descriptive statistics
desc_tbl <- kap %>%
  pivot_longer(everything(), names_to = "Scale", values_to = "value") %>%
  group_by(Scale) %>%
  summarise(
    N       = sum(!is.na(value)),
    Mean    = round(mean(value, na.rm = TRUE), 2),
    SD      = round(sd(value, na.rm = TRUE), 2),
    Median  = round(median(value, na.rm = TRUE), 2),
    Minimum = round(min(value, na.rm = TRUE), 2),
    Maximum = round(max(value, na.rm = TRUE), 2),
    Range   = round(max(value, na.rm = TRUE) - min(value, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(factor(Scale, levels = names(kap)))

desc_tbl

# Format as a Word-ready table
ft_desc <- flextable(desc_tbl) %>%
  set_caption("Table 2. Descriptive statistics of KAP scales") %>%
  theme_booktabs() %>%
  flextable::align(align = "center", part = "all") %>%
  flextable::align(j = 1, align = "left", part = "all") %>%
  autofit()

ft_desc
save_as_docx(ft_desc, path = "tables/Descriptive_Statistics_AllScales.docx")

# Create categories (EDIT CUT-OFFS)
cat_data <- data %>%
  transmute(
    `Digital Health Access` = cut(DigitalHealthInformationAccessPCT,
                                  breaks = c(-Inf, 33.33, 66.67, Inf),
                                  labels = c("Low", "Moderate", "High"),
                                  right = FALSE),
    Knowledge = cut(KnowledgeScorePCT,
                    breaks = c(-Inf, 50, 70, 85, Inf),
                    labels = c("Poor", "Fair", "Good", "Excellent"),
                    right = FALSE),
    Attitude = cut(TotalAttitudeScore / 45 * 100,
                   breaks = c(-Inf, 40, 60, 80, Inf),
                   labels = c("Unfavorable", "Neutral", "Favorable", "Very Favorable"),
                   right = FALSE),
    Practice = cut(PracticeScorePTC,
                   breaks = c(-Inf, 40, 55, 75, Inf),
                   labels = c("Poor", "Fair", "Good", "Excellent"),
                   right = FALSE)
  )

# Frequency table (n and %), using valid (non-missing) responses
cat_tbl <- cat_data %>%
  pivot_longer(everything(), names_to = "Scale", values_to = "Category") %>%
  filter(!is.na(Category)) %>%
  count(Scale, Category) %>%
  group_by(Scale) %>%
  mutate(
    Total   = sum(n),
    Percent = round(n / Total * 100, 2),
    `n (%)` = paste0(n, " (", Percent, "%)")
  ) %>%
  ungroup() %>%
  select(Scale, Category, `n (%)`) %>%
  mutate(Scale = factor(Scale, levels = c("Digital Health Access",
                                          "Knowledge", "Attitude", "Practice"))) %>%
  arrange(Scale)

cat_tbl

# Word-ready table
ft_cat <- flextable(cat_tbl) %>%
  flextable::merge_v(j = "Scale") %>%
  set_caption("Table 3. Distribution of participants by KAP category") %>%
  theme_booktabs() %>%
  flextable::valign(j = 1, valign = "top") %>%
  autofit()

ft_cat
save_as_docx(ft_cat, path = "tables/Category_Distribution_KAP.docx")



library(tidyverse)
library(flextable)

get_outlier <- function(x, name) {
  x     <- as.numeric(x)
  x     <- x[!is.na(x)]
  q1    <- quantile(x, 0.25)
  q3    <- quantile(x, 0.75)
  iqr   <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  out   <- x[x < lower | x > upper]
  
  tibble(
    Scale             = name,
    N                 = length(x),
    Q1                = round(unname(q1), 2),
    Q3                = round(unname(q3), 2),
    IQR               = round(unname(iqr), 2),
    `Lower bound`     = round(unname(lower), 2),
    `Upper bound`     = round(unname(upper), 2),
    `Outliers (n)`    = length(out),
    `Outliers (%)`    = round(length(out) / length(x) * 100, 1),
    `Outlier range`   = ifelse(length(out) == 0, "None",
                               paste0(round(min(out), 2), "-", round(max(out), 2)))
  )
}

outlier_tbl <- bind_rows(
  get_outlier(data$TotalDigitalHealthInformationAccess, "Digital Health Information Access"),
  get_outlier(data$TotalKnowledgeScore,                 "Knowledge"),
  get_outlier(data$TotalAttitudeScore,                  "Attitude"),
  get_outlier(data$TotalPracticeScore,                  "Practice")
)

outlier_tbl

ft_out <- flextable(outlier_tbl) %>%
  set_caption("Table 9. Outlier detection by the interquartile range (IQR) method") %>%
  theme_booktabs() %>%
  flextable::align(align = "center", part = "all") %>%
  flextable::align(j = 1, align = "left", part = "all") %>%
  autofit()

ft_out
save_as_docx(ft_out, path = "tables/Outlier_Detection_Results.docx")




# Table 3: Digital Health Literacy  
table3 <- data %>% 
  dplyr::select(10:19) %>% 
  tbl_summary(missing = "no", 
              type = everything() ~ "categorical") %>% 
  as_flex_table() %>% 
  set_table_properties(width = 1, layout = "autofit")

table3
# Export table 3 
save_as_docx(table3, path = "tables/3_DigitalHelathLiteracy_summary.docx")


# Table 4: Knowledge Level Summary 
table4 <- data %>% 
  dplyr::select(22:29) %>% 
  tbl_summary(missing = "no", 
              type = everything() ~ "categorical") %>% 
  as_flex_table() %>% 
  set_table_properties(width = 1, layout = "autofit")

table4
save_as_docx(table4, path = "tables/4_KnowledgeLevel_summary.docx")

# Table 5: Attitude Level Summary 
table5 <- data %>% 
  dplyr::select(32:40) %>% 
  tbl_summary(missing = "no", 
              type = everything() ~ "categorical") %>% 
  as_flex_table() %>% 
  set_table_properties(width = 1, layout = "autofit")

table5
save_as_docx(table5, path = "tables/5_AttitudeLevel_summary.docx")

# Table 6: Practice Level Summary 
table6 <- data %>% 
  dplyr::select(43:52) %>% 
  tbl_summary(missing = "no", 
              type = everything() ~ "categorical") %>% 
  as_flex_table() %>% 
  set_table_properties(width = 1, layout = "autofit")

table6
save_as_docx(table6, path = "tables/6_PracticeLevel_summary.docx")

#More tables

# Demographics
tbl7<- data %>% 
  select(1:9) %>% 
  tbl_summary(missing="no", type=everything()~"categorical") %>% 
  as_flex_table() %>% save_as_docx(path="tables/7_Demographics.docx")



# Digital Health by Sex
data %>% select(2, 20:21) %>% tbl_summary(by=`What is your sex?`, missing="no") %>% 
  add_p() %>% as_flex_table() %>% save_as_docx(path="tables/8_DHI_by_Sex.docx")

# 3. All Scores Summary
data %>% select(20, 30, 41, 54) %>% tbl_summary(missing="no", 
                                                type=everything()~"continuous",
                                                statistic=list(all_continuous()~"{mean} ({sd})")) %>% 
  as_flex_table() %>% save_as_docx(path="tables/9_Scores_Summary.docx")

# 4. Score Categories
data %>% select(20, 30, 41, 54) %>% 
  mutate(across(everything(), as.factor)) %>%
  tbl_summary(missing="no") %>% 
  as_flex_table() %>% save_as_docx(path="tables/10_Score_Categories.docx")
