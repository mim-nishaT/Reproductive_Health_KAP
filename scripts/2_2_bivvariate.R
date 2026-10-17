library(tidyverse)
library(readxl)
library(rstatix)

data <- readxl::read_excel("data/DHI_Access_Health_Literacy.xlsx")

data <- data %>%
  mutate(Residence = `Where do you live?`)

scores <- c("TotalDigitalHealthInformationAccess",
            "TotalKnowledgeScore",
            "TotalAttitudeScore",
            "TotalPracticeScore")

# Simple Wilcoxon test - one line per score
wilcox_results <- map_dfr(scores, ~ {
  wilcox_test(data, as.formula(paste(.x, "~ Residence"))) %>%
    add_significance() %>%
    mutate(Score = .x)
})

print(wilcox_results)

# Export
library(flextable)
wilcox_results %>%
  as_flextable() %>%
  autofit() %>%
  save_as_docx(path = "tables/11_Bivariate_wilcox_rural_urban.docx")




#compare by age
# Create age variable
data <- data %>%
  mutate(Age = `What is your age?`)

cat("\nAge groups in data:\n")
print(table(data$Age))

# BOX PLOT - DIGITAL HEALTH ACCESS BY AGE

p1 <- ggplot(data, aes(x = Age, y = TotalDigitalHealthInformationAccess, 
                       fill = Age)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 2) +
  labs(title = "Digital Health Information Access by Age",
       x = "Age Group", y = "DHIA Score",
       fill = "Age Group") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")

ggsave("figures/BoxPlot_DHIA_by_Age.png", p1, width = 10, height = 6)
print(p1)

# ============================================================================
# 2. BOX PLOT - KNOWLEDGE BY AGE
# ============================================================================

p2 <- ggplot(data, aes(x = Age, y = TotalKnowledgeScore, 
                       fill = Age)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 2) +
  labs(title = "Knowledge Score by Age",
       x = "Age Group", y = "Knowledge Score",
       fill = "Age Group") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")

ggsave("figures/BoxPlot_Knowledge_by_Age.png", p2, width = 10, height = 6)
print(p2)

# ============================================================================
# 3. BOX PLOT - ATTITUDE BY AGE
# ============================================================================

p3 <- ggplot(data, aes(x = Age, y = TotalAttitudeScore, 
                       fill = Age)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 2) +
  labs(title = "Attitude Score by Age",
       x = "Age Group", y = "Attitude Score",
       fill = "Age Group") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")

ggsave("figures/BoxPlot_Attitude_by_Age.png", p3, width = 10, height = 6)
print(p3)

# ============================================================================
# 4. BOX PLOT - PRACTICE BY AGE
# ============================================================================

p4 <- ggplot(data, aes(x = Age, y = TotalPracticeScore, 
                       fill = Age)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 2) +
  labs(title = "Practice Score by Age",
       x = "Age Group", y = "Practice Score",
       fill = "Age Group") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")

ggsave("figures/BoxPlot_Practice_by_Age.png", p4, width = 10, height = 6)
print(p4)

# ============================================================================
# 5. COMBINED PLOT (All 4 in one)
# ============================================================================

cat("\n📊 Creating combined plot...\n")

library(patchwork)

combined <- p1 + p2 + p3 + p4 +
  plot_layout(ncol = 2) +
  plot_annotation(title = "All Scores by Age Group",
                  theme = theme(plot.title = element_text(hjust = 0.5, 
                                                          face = "bold", 
                                                          size = 14)))

ggsave("figures/Combined_BoxPlots_by_Age.png", combined, width = 14, height = 10)
print(combined)

# DESCRIPTIVE STATISTICS BY AGE

cat("\n\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║        DESCRIPTIVE STATISTICS BY AGE GROUP                    ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

summary_by_age <- data %>%
  group_by(Age) %>%
  summarise(
    N = n(),
    DHIA_Mean = round(mean(TotalDigitalHealthInformationAccess, na.rm = TRUE), 2),
    DHIA_SD = round(sd(TotalDigitalHealthInformationAccess, na.rm = TRUE), 2),
    Know_Mean = round(mean(TotalKnowledgeScore, na.rm = TRUE), 2),
    Know_SD = round(sd(TotalKnowledgeScore, na.rm = TRUE), 2),
    Att_Mean = round(mean(TotalAttitudeScore, na.rm = TRUE), 2),
    Att_SD = round(sd(TotalAttitudeScore, na.rm = TRUE), 2),
    Prac_Mean = round(mean(TotalPracticeScore, na.rm = TRUE), 2),
    Prac_SD = round(sd(TotalPracticeScore, na.rm = TRUE), 2)
  )

print(summary_by_age)

# 7. KRUSKAL-WALLIS TEST (Compare groups - use this for non-normal data)

cat("\n\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║      KRUSKAL-WALLIS TEST (Compare 4 Age Groups)              ║\n")
cat("║      H0: All age groups have same distribution               ║\n")
cat("║      p < 0.05 = Significant difference between groups        ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

library(rstatix)

kruskal_results <- map_dfr(c("TotalDigitalHealthInformationAccess",
                             "TotalKnowledgeScore",
                             "TotalAttitudeScore",
                             "TotalPracticeScore"), ~ {
                               data %>%
                                 kruskal_test(as.formula(paste(.x, "~ Age"))) %>%
                                 add_significance() %>%
                                 mutate(Score = .x)
                             })

print(kruskal_results %>% select(Score, statistic, p, p.signif))

# 8. EXPORT TO WORD

library(flextable)

summary_by_age %>%
  as_flextable() %>%
  autofit() %>%
  save_as_docx(path = "tables/12_Summary_Stats_by_Age.docx")

kruskal_results %>%
  select(Score, statistic, p, p.signif) %>%
  rename(
    "Score Type" = Score,
    "Test Statistic" = statistic,
    "P-Value" = p,
    "Significance" = p.signif
  ) %>%
  as_flextable() %>%
  autofit() %>%
  save_as_docx(path = "tables/13_Kruskal_Wallis_by_Age.docx")

