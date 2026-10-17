library(tidyverse)
library(patchwork)

# 1. Function: histogram + normal curve for one scale
make_hist <- function(x, title, xlab, binwidth = NULL, bins = 15) {
  x <- as.numeric(x)
  x <- x[!is.na(x)]
  m <- mean(x)
  s <- sd(x)
  
  df <- data.frame(x = x)
  
  p <- ggplot(df, aes(x = x))
  
  if (is.null(binwidth)) {
    p <- p + geom_histogram(aes(y = after_stat(density)),
                            bins = bins, fill = "#4C78A8",
                            color = "white", alpha = 0.85)
  } else {
    p <- p + geom_histogram(aes(y = after_stat(density)),
                            binwidth = binwidth, fill = "#4C78A8",
                            color = "white", alpha = 0.85)
  }
  
  p +
    stat_function(fun = dnorm, args = list(mean = m, sd = s),
                  color = "red", linetype = "dashed", linewidth = 1) +
    labs(title = title,
         subtitle = paste0("Mean = ", round(m, 2), ", SD = ", round(s, 2),
                           ", n = ", length(x)),
         x = xlab, y = "Density") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(size = 10, color = "grey30"),
          panel.grid.minor = element_blank())
}

# 2. Build the plots
p_dha <- make_hist(data$TotalDigitalHealthInformationAccess,
                   "A. Digital Health Information Access",
                   "Score (0-18)", binwidth = 1)

p_k <- make_hist(data$TotalKnowledgeScore,
                 "B. Knowledge",
                 "Score (0-8)", binwidth = 1)

p_a <- make_hist(data$TotalAttitudeScore,
                 "C. Attitude",
                 "Score (9-45)", binwidth = 2)

p_p <- make_hist(data$TotalPracticeScore,
                 "D. Practice",
                 "Score (10-50)", binwidth = 3)

# 3. Combine into one 2 x 2 figure
fig <- (p_dha | p_k) / (p_a | p_p)

fig

# 4. Save as high-resolution PNG
ggsave("figures/Univariate_Histograms_AllScales.png",
       plot = fig, width = 11, height = 8, dpi = 300, bg = "white")


library(ggplot2)
library(ggpubr)

p1 <- data %>%
  ggplot(aes(x = `What is your sex?`, y = TotalDigitalHealthInformationAccess, 
             fill = `What is your sex?`)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  labs(title = "Digital Health Information Access by Sex",
       x = "Sex", y = "DHIA Score",
       fill = "Sex") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("figures/1_DHI_by_Sex_Boxplot.png", p1, width = 8, height = 6)