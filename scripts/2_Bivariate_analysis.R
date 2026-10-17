library(tidyverse)
library(rstatix)
library(flextable)

data <- readxl::read_excel("data/DHI_Access_Health_Literacy.xlsx")



# 1. Set residence variable (column 3) and the four total scores
df <- data %>%
  transmute(
    Residence = factor(`Where do you live?`),
    `Digital Health Information Access` = as.numeric(TotalDigitalHealthInformationAccess),
    Knowledge = as.numeric(TotalKnowledgeScore),
    Attitude  = as.numeric(TotalAttitudeScore),
    Practice  = as.numeric(TotalPracticeScore)
  )

levels(df$Residence)   # check the group names (Rural / Urban)

# 2. Long format
long <- df %>%
  pivot_longer(-Residence, names_to = "Scale", values_to = "Score") %>%
  filter(!is.na(Score), !is.na(Residence)) %>%
  mutate(Scale = factor(Scale, levels = c("Digital Health Information Access",
                                          "Knowledge", "Attitude", "Practice")))

# 3. Medians (IQR) by group
desc <- long %>%
  group_by(Scale, Residence) %>%
  summarise(n = n(),
            Median = median(Score),
            Q1 = quantile(Score, 0.25),
            Q3 = quantile(Score, 0.75),
            .groups = "drop") %>%
  mutate(`Median (IQR)` = paste0(Median, " (", Q1, "-", Q3, ")")) %>%
  select(Scale, Residence, n, `Median (IQR)`) %>%
  pivot_wider(names_from = Residence,
              values_from = c(n, `Median (IQR)`))

# 4. Wilcoxon rank-sum test + effect size (r)
test <- long %>%
  group_by(Scale) %>%
  wilcox_test(Score ~ Residence, detailed = FALSE) %>%
  adjust_pvalue(method = "bonferroni") %>%
  select(Scale, U = statistic, p, p.adj)

eff <- long %>%
  group_by(Scale) %>%
  wilcox_effsize(Score ~ Residence) %>%
  select(Scale, r = effsize, Magnitude = magnitude)

# 5. Combine into the final table
res_tbl <- desc %>%
  left_join(test, by = "Scale") %>%
  left_join(eff,  by = "Scale") %>%
  mutate(
    U     = round(U, 0),
    r     = round(r, 2),
    p     = ifelse(p < 0.001, "< 0.001", format(round(p, 3), nsmall = 3)),
    p.adj = ifelse(p.adj < 0.001, "< 0.001", format(round(p.adj, 3), nsmall = 3))
  ) %>%
  rename(`p (Bonferroni)` = p.adj)

res_tbl

# 6. Word-ready table
ft_res <- flextable(res_tbl) %>%
  set_caption("Table 10. Comparison of KAP scores by residence (Wilcoxon rank-sum test)") %>%
  theme_booktabs() %>%
  flextable::align(align = "center", part = "all") %>%
  flextable::align(j = 1, align = "left", part = "all") %>%
  autofit()

ft_res
save_as_docx(ft_res, path = "tables/Residence_Wilcoxon_Results.docx")


#Plot
library(tidyverse)
library(patchwork)

# 1. Prepare data
df <- data %>%
  transmute(
    Residence = factor(`Where do you live?`),
    `Digital Health Information Access` = as.numeric(TotalDigitalHealthInformationAccess),
    Knowledge = as.numeric(TotalKnowledgeScore),
    Attitude  = as.numeric(TotalAttitudeScore),
    Practice  = as.numeric(TotalPracticeScore)
  )

# 2. Function: box plot + jitter by residence, with p-value label
make_box_res <- function(var, ylab, p_label) {
  d <- df %>%
    select(Residence, value = all_of(var)) %>%
    filter(!is.na(value), !is.na(Residence))
  
  ggplot(d, aes(x = Residence, y = value, fill = Residence)) +
    geom_boxplot(alpha = 0.6, width = 0.5, outlier.shape = NA) +
    stat_boxplot(geom = "errorbar", width = 0.2) +
    geom_jitter(width = 0.15, alpha = 0.4, size = 1.3, color = "grey20") +
    scale_fill_manual(values = c("#E69F00", "#56B4E9")) +
    labs(title = var, subtitle = p_label, x = NULL, y = ylab) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, size = 10, color = "grey30"),
          legend.position = "none",
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())
}

# 3. Build the plots (p-values from Table 10)
r1 <- make_box_res("Digital Health Information Access", "Score (0-18)",
                   "Wilcoxon p < 0.001")
r2 <- make_box_res("Knowledge", "Score (0-8)",  "Wilcoxon p = 0.017")
r3 <- make_box_res("Attitude",  "Score (9-45)", "Wilcoxon p = 0.931")
r4 <- make_box_res("Practice",  "Score (10-50)", "Wilcoxon p = 0.057")

# 4. Combine into 2 x 2 figure
fig_res <- (r1 | r2) / (r3 | r4)
fig_res

# 5. Save as high-resolution PNG
ggsave("figures/BoxPlots_by_Residence_AllScales.png",
       plot = fig_res, width = 11, height = 8, dpi = 300, bg = "white")


library(tidyverse)
library(rstatix)
library(flextable)

data <- readxl::read_xlsx("data/DHI_Access_Health_Literacy.xlsx")
# 1. Prepare data (age = column 1)
df <- data %>%
  transmute(
    Age = factor(`What is your age?`),
    `Digital Health Information Access` = as.numeric(TotalDigitalHealthInformationAccess),
    Knowledge = as.numeric(TotalKnowledgeScore),
    Attitude  = as.numeric(TotalAttitudeScore),
    Practice  = as.numeric(TotalPracticeScore)
  )

levels(df$Age)   # check the age group names

# 2. Long format
long <- df %>%
  pivot_longer(-Age, names_to = "Scale", values_to = "Score") %>%
  filter(!is.na(Score), !is.na(Age)) %>%
  mutate(Scale = factor(Scale, levels = c("Digital Health Information Access",
                                          "Knowledge", "Attitude", "Practice")))

# 3. Kruskal-Wallis H test (H, df, p) + Bonferroni adjustment
kw <- long %>%
  group_by(Scale) %>%
  kruskal_test(Score ~ Age) %>%
  adjust_pvalue(method = "bonferroni") %>%
  ungroup() %>%
  select(Scale, N = n, H = statistic, df, p, p.adj)

# 4. Effect size (epsilon-squared)
eff <- long %>%
  group_by(Scale) %>%
  kruskal_effsize(Score ~ Age) %>%
  ungroup() %>%
  select(Scale, `Epsilon-sq` = effsize, Magnitude = magnitude)

# 5. Median for each age group
med <- long %>%
  group_by(Scale, Age) %>%
  summarise(Median = median(Score), .groups = "drop") %>%
  pivot_wider(names_from = Age, values_from = Median, names_prefix = "Mdn: ")

# 6. Combine into final table
kw_tbl <- kw %>%
  left_join(eff, by = "Scale") %>%
  left_join(med, by = "Scale") %>%
  mutate(
    Interpretation = case_when(
      p < 0.05 & p.adj < 0.05 ~ "Significant difference between age groups",
      p < 0.05                ~ "Significant before, but not after, Bonferroni correction",
      TRUE                    ~ "No significant difference between age groups"
    ),
    H           = round(H, 2),
    `Epsilon-sq` = round(`Epsilon-sq`, 3),
    p     = ifelse(p < 0.001, "< 0.001", format(round(p, 3), nsmall = 3)),
    p.adj = ifelse(p.adj < 0.001, "< 0.001", format(round(p.adj, 3), nsmall = 3))
  ) %>%
  rename(`H statistic` = H,
         `df` = df,
         `p-value` = p,
         `p (Bonferroni)` = p.adj) %>%
  relocate(Interpretation, .after = Magnitude)

kw_tbl

# 7. Word-ready table
ft_kw <- flextable(kw_tbl) %>%
  set_caption("Table 11. Kruskal-Wallis H test results of KAP scores by age group") %>%
  theme_booktabs() %>%
  flextable::align(align = "center", part = "all") %>%
  flextable::align(j = c(1, 9), align = "left", part = "all") %>%
  autofit()

ft_kw
save_as_docx(ft_kw, path = "tables/KruskalWallis_Results_Age.docx")
