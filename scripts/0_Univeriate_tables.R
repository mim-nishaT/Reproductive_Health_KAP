library(tidyverse)
library(psych)
library(flextable)

# Shapiro-Wilk test + skewness/kurtosis for one scale
get_sw <- function(x, name) {
  x  <- as.numeric(x)
  x  <- x[!is.na(x)]
  sw <- shapiro.test(x)
  sk <- psych::skew(x)
  ku <- psych::kurtosi(x)
  
  tibble(
    Scale            = name,
    N                = length(x),
    `W statistic`    = round(unname(sw$statistic), 3),
    `p-value`        = ifelse(sw$p.value < 0.001, "< 0.001", format(round(sw$p.value, 3), nsmall = 3)),
    Skewness         = round(sk, 2),
    Kurtosis         = round(ku, 2),
    Interpretation   = ifelse(sw$p.value < 0.05, "Not normal", "Normal"),
    `Reason for non-normality` = case_when(
      sw$p.value >= 0.05 ~ "Not applicable",
      sk <= -0.5 & ku < 0 ~ "Left-skewed with flat (platykurtic) distribution; responses concentrated at high scores",
      sk <= -0.5          ~ "Left-skewed; responses concentrated at high scores",
      sk >= 0.5           ~ "Right-skewed; responses concentrated at low scores",
      ku < -0.5           ~ "Flat (platykurtic) or multimodal distribution",
      TRUE                ~ "Departure from normality in shape (e.g., multimodality or outliers)"
    )
  )
}

# Build the table
sw_tbl <- bind_rows(
  get_sw(data$TotalDigitalHealthInformationAccess, "Digital Health Information Access"),
  get_sw(data$TotalKnowledgeScore,                 "Knowledge"),
  get_sw(data$TotalAttitudeScore,                  "Attitude"),
  get_sw(data$TotalPracticeScore,                  "Practice")
)

sw_tbl

# Word-ready table
ft_sw <- flextable(sw_tbl) %>%
  set_caption("Table 8. Shapiro-Wilk normality test results for the KAP scales") %>%
  theme_booktabs() %>%
  flextable::align(align = "center", part = "all") %>%
  flextable::align(j = c(1, 8), align = "left", part = "all") %>%
  autofit()

ft_sw
save_as_docx(ft_sw, path = "tables/Normality_Test_Results.docx")


library(tidyverse)
library(patchwork)

# 1. Function: box plot + jittered points for one scale
make_box <- function(x, title, ylab, fill_col) {
  df <- data.frame(Scale = title, value = as.numeric(x)) %>% filter(!is.na(value))
  
  ggplot(df, aes(x = Scale, y = value)) +
    geom_boxplot(fill = fill_col, alpha = 0.6, width = 0.5,
                 outlier.shape = NA) +                      # outliers shown via jitter
    geom_jitter(width = 0.15, alpha = 0.5, size = 1.5, color = "grey20") +
    stat_boxplot(geom = "errorbar", width = 0.2) +
    labs(title = title, x = NULL, y = ylab) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5),
          axis.text.x = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())
}

# 2. Build the plots
b1 <- make_box(data$TotalDigitalHealthInformationAccess,
               "Digital Health Access", "Score (0-18)", "#6BAED6")
b2 <- make_box(data$TotalKnowledgeScore,
               "Knowledge", "Score (0-8)", "#74C69D")
b3 <- make_box(data$TotalAttitudeScore,
               "Attitude", "Score (9-45)", "#EE8577")
b4 <- make_box(data$TotalPracticeScore,
               "Practice", "Score (10-50)", "#F5B85A")

# 3. Combine into a 2 x 2 figure
fig_box <- (b1 | b2) / (b3 | b4)
fig_box

# 4. Save as high-resolution PNG
ggsave("figures/Univariate_BoxPlots_Outliers.png",
       plot = fig_box, width = 11, height = 8, dpi = 300, bg = "white")


q  <- quantile(data$TotalAttitudeScore, c(0.25, 0.75), na.rm = TRUE)
lo <- q[1] - 1.5 * IQR(data$TotalAttitudeScore, na.rm = TRUE)
hi <- q[2] + 1.5 * IQR(data$TotalAttitudeScore, na.rm = TRUE)

df_att <- data %>%
  transmute(Scale = "Attitude",
            value = as.numeric(TotalAttitudeScore),
            outlier = value < lo | value > hi)

b3 <- ggplot(df_att, aes(x = Scale, y = value)) +
  geom_boxplot(fill = "#EE8577", alpha = 0.6, width = 0.5, outlier.shape = NA) +
  stat_boxplot(geom = "errorbar", width = 0.2) +
  geom_jitter(aes(color = outlier), width = 0.15, alpha = 0.6, size = 1.5) +
  scale_color_manual(values = c("grey20", "red"), guide = "none") +
  labs(title = "Attitude", x = NULL, y = "Score (9-45)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())
b3
