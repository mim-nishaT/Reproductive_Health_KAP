# UNIVARIATE ANALYSIS

library(tidyverse)
library(readxl)
library(psych)
library(ggplot2)

# Load data
data <- readxl::read_excel("data/DHI_Access_Health_Literacy.xlsx")

#-------------------FREQUENCY DISTRIBUTIONS----------------

# Digital Health Access
cat("\n1. DIGITAL HEALTH INFORMATION ACCESS\n")
freq_dhia <- table(cut(data$TotalDigitalHealthInformationAccess, 
                       breaks = c(0, 5.33, 10.67, 18),
                       labels = c("Low", "Moderate", "High")))
print(freq_dhia)
cat("Percentages:\n")
print(round(prop.table(freq_dhia) * 100, 2))

# Knowledge
cat("\n2. REPRODUCTIVE HEALTH KNOWLEDGE\n")
freq_know <- table(cut(data$TotalKnowledgeScore,
                       breaks = c(0, 2, 4, 6, 8),
                       labels = c("Poor", "Fair", "Good", "Excellent")))
print(freq_know)
cat("Percentages:\n")
print(round(prop.table(freq_know) * 100, 2))

# Attitude
cat("\n3. ATTITUDE TOWARD RH\n")
freq_att <- table(cut(data$TotalAttitudeScore,
                      breaks = c(9, 18, 27, 36, 45),
                      labels = c("Unfavorable", "Neutral", "Favorable", "Very Favorable")))
print(freq_att)
cat("Percentages:\n")
print(round(prop.table(freq_att) * 100, 2))

# Practice
cat("\n4. REPRODUCTIVE HEALTH PRACTICE\n")
freq_prac <- table(cut(data$TotalPracticeScore,
                       breaks = c(10, 20, 30, 40, 50),
                       labels = c("Poor", "Fair", "Good", "Excellent")))
print(freq_prac)
cat("Percentages:\n")
print(round(prop.table(freq_prac) * 100, 2))

# 2. NORMALITY TESTING (SHAPIRO-WILK TEST)

cat("\n\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║           NORMALITY TESTING (SHAPIRO-WILK)                    ║\n")
cat("║  H0: Data is normally distributed                             ║\n")
cat("║  p > 0.05 = Normal, p < 0.05 = Not Normal                    ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n")

scores <- data %>% 
  select(TotalDigitalHealthInformationAccess, TotalKnowledgeScore, 
         TotalAttitudeScore, TotalPracticeScore) %>%
  drop_na()

# Create normality test results
normality_results <- data.frame(
  Scale = c("Digital Health Access", "Knowledge", "Attitude", "Practice"),
  W_statistic = NA,
  P_value = NA,
  Normality = NA
)

# Test each scale
scales_list <- list(
  scores$TotalDigitalHealthInformationAccess,
  scores$TotalKnowledgeScore,
  scores$TotalAttitudeScore,
  scores$TotalPracticeScore
)

for(i in 1:4) {
  test <- shapiro.test(scales_list[[i]])
  normality_results$W_statistic[i] <- round(test$statistic, 4)
  normality_results$P_value[i] <- round(test$p.value, 4)
  normality_results$Normality[i] <- ifelse(test$p.value > 0.05, "Normal ✓", "Not Normal")
}

print(normality_results)

cat("\nINTERPRETATION:\n")
cat("- W close to 1 = More normal\n")
cat("- p > 0.05 = Data is normally distributed\n")
cat("- p < 0.05 = Data is NOT normally distributed (use non-parametric tests)\n")

# 3. OUTLIER DETECTION

cat("\n\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║           OUTLIER DETECTION (IQR METHOD)                      ║\n")
cat("║  Outliers: values > Q3 + 1.5×IQR or < Q1 - 1.5×IQR          ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n")

detect_outliers <- function(data, name) {
  Q1 <- quantile(data, 0.25, na.rm = TRUE)
  Q3 <- quantile(data, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  outliers <- data[(data < lower_bound | data > upper_bound) & !is.na(data)]
  
  cat("\n", name, "\n")
  cat("  Q1:", round(Q1, 2), "\n")
  cat("  Q3:", round(Q3, 2), "\n")
  cat("  IQR:", round(IQR, 2), "\n")
  cat("  Lower Bound:", round(lower_bound, 2), "\n")
  cat("  Upper Bound:", round(upper_bound, 2), "\n")
  cat("  Number of Outliers:", length(outliers), "\n")
  
  if(length(outliers) > 0) {
    cat("  Outlier Values:", paste(round(sort(outliers), 2), collapse = ", "), "\n")
  }
  
  return(data.frame(
    Scale = name,
    Q1 = Q1,
    Q3 = Q3,
    IQR = IQR,
    Lower_Bound = lower_bound,
    Upper_Bound = upper_bound,
    N_Outliers = length(outliers)
  ))
}

outlier_results <- rbind(
  detect_outliers(scores$TotalDigitalHealthInformationAccess, "Digital Health Access"),
  detect_outliers(scores$TotalKnowledgeScore, "Knowledge"),
  detect_outliers(scores$TotalAttitudeScore, "Attitude"),
  detect_outliers(scores$TotalPracticeScore, "Practice")
)

# 4. VISUALIZATIONS

cat("\n\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║              HISTOGRAMS WITH NORMALITY CURVES                 ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n")

# Histogram - Digital Health
p1 <- ggplot(data, aes(x = TotalDigitalHealthInformationAccess)) +
  geom_histogram(bins = 20, fill = "#3498db", alpha = 0.7, color = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(scores$TotalDigitalHealthInformationAccess, na.rm = TRUE),
                                         sd = sd(scores$TotalDigitalHealthInformationAccess, na.rm = TRUE)),
                color = "red", size = 1, linetype = "dashed") +
  labs(title = "Digital Health Access - Distribution",
       x = "Score", y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("figures/Univariate_1_DHIA_Histogram.png", p1, width = 8, height = 6)
print(p1)

# Histogram - Knowledge
p2 <- ggplot(data, aes(x = TotalKnowledgeScore)) +
  geom_histogram(bins = 15, fill = "#2ecc71", alpha = 0.7, color = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(scores$TotalKnowledgeScore, na.rm = TRUE),
                                         sd = sd(scores$TotalKnowledgeScore, na.rm = TRUE)),
                color = "red", size = 1, linetype = "dashed") +
  labs(title = "Knowledge - Distribution",
       x = "Score", y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("figures/Univariate_2_Knowledge_Histogram.png", p2, width = 8, height = 6)
print(p2)

# Histogram - Attitude
p3 <- ggplot(data, aes(x = TotalAttitudeScore)) +
  geom_histogram(bins = 15, fill = "#e74c3c", alpha = 0.7, color = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(scores$TotalAttitudeScore, na.rm = TRUE),
                                         sd = sd(scores$TotalAttitudeScore, na.rm = TRUE)),
                color = "red", size = 1, linetype = "dashed") +
  labs(title = "Attitude - Distribution",
       x = "Score", y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("figures/Univariate_3_Attitude_Histogram.png", p3, width = 8, height = 6)
print(p3)

# Histogram - Practice
p4 <- ggplot(data, aes(x = TotalPracticeScore)) +
  geom_histogram(bins = 20, fill = "#f39c12", alpha = 0.7, color = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(scores$TotalPracticeScore, na.rm = TRUE),
                                         sd = sd(scores$TotalPracticeScore, na.rm = TRUE)),
                color = "red", size = 1, linetype = "dashed") +
  labs(title = "Practice - Distribution",
       x = "Score", y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("figures/Univariate_4_Practice_Histogram.png", p4, width = 8, height = 6)
print(p4)

# Q-Q PLOTS (For Normality Visual Inspection)

cat("\n📊 Q-Q Plots generated...\n")

p5 <- ggplot(data, aes(sample = TotalDigitalHealthInformationAccess)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot: Digital Health Access") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

p6 <- ggplot(data, aes(sample = TotalKnowledgeScore)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot: Knowledge") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

p7 <- ggplot(data, aes(sample = TotalAttitudeScore)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot: Attitude") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

p8 <- ggplot(data, aes(sample = TotalPracticeScore)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot: Practice") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("figures/Univariate_5_QQplots.png",
       gridExtra::grid.arrange(p5, p6, p7, p8, ncol=2), width = 10, height = 8)

# BOX PLOTS FOR OUTLIERS

cat("\n📊 Box plots for outlier visualization...\n")

p9 <- ggplot(scores %>% gather(key = "Scale", value = "Score"),
             aes(x = Scale, y = Score, fill = Scale)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  labs(title = "Box Plots - All Scales with Outliers",
       x = "Scale", y = "Score") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("figures/Univariate_6_Boxplots_Outliers.png", p9, width = 10, height = 6)
print(p9)

