library(tidyverse)
library(MASS)      # polr()
library(brant)     # Brant test
library(flextable)

# 1. Prepare data: ordered Practice category + the three predictors
#    (use the SAME cut-offs as in your Table 3)
ord_df <- data %>%
  transmute(
    Practice_cat = cut(PracticeScorePTC,
                       breaks = c(-Inf, 40, 55, 75, Inf),
                       labels = c("Poor", "Fair", "Good", "Excellent"),
                       right = FALSE,
                       ordered_result = TRUE),
    DHIA      = as.numeric(TotalDigitalHealthInformationAccess),
    Knowledge = as.numeric(TotalKnowledgeScore),
    Attitude  = as.numeric(TotalAttitudeScore)
  ) %>%
  drop_na()

table(ord_df$Practice_cat)

# 2. Fit proportional odds model and the intercept-only (null) model
m_ord  <- MASS::polr(Practice_cat ~ DHIA + Knowledge + Attitude,
                     data = ord_df, Hess = TRUE, method = "logistic")
m_null <- MASS::polr(Practice_cat ~ 1, data = ord_df, Hess = TRUE)

summary(m_ord)

# 3. Coefficients, SE, Z, p, OR and 95% CI (Wald)
sm   <- summary(m_ord)$coefficients
b    <- sm[names(coef(m_ord)), "Value"]
se   <- sm[names(coef(m_ord)), "Std. Error"]
z    <- b / se
pval <- 2 * pnorm(abs(z), lower.tail = FALSE)

coef_tbl <- tibble(
  Predictor        = c("Digital health information access", "Knowledge", "Attitude"),
  `Coefficient (β)` = round(b, 3),
  SE               = round(se, 3),
  `Z`              = round(z, 2),
  `p-value`        = ifelse(pval < 0.001, "< 0.001", format(round(pval, 3), nsmall = 3)),
  `Odds ratio`     = round(exp(b), 2),
  `95% CI (OR)`    = paste0(round(exp(b - 1.96 * se), 2), " to ",
                            round(exp(b + 1.96 * se), 2)),
  Significance     = case_when(pval < 0.001 ~ "***",
                               pval < 0.01  ~ "**",
                               pval < 0.05  ~ "*",
                               TRUE         ~ "ns")
)

# Thresholds (intercepts)
th_names <- names(m_ord$zeta)
th_tbl <- tibble(
  Predictor        = paste0("Threshold: ", th_names),
  `Coefficient (β)` = round(sm[th_names, "Value"], 3),
  SE               = round(sm[th_names, "Std. Error"], 3),
  `Z`              = round(sm[th_names, "Value"] / sm[th_names, "Std. Error"], 2),
  `p-value`        = "-",
  `Odds ratio`     = NA_real_,
  `95% CI (OR)`    = "-",
  Significance     = "-"
)

ord_coef_tbl <- bind_rows(coef_tbl, th_tbl) %>%
  mutate(`Odds ratio` = ifelse(is.na(`Odds ratio`), "-", as.character(`Odds ratio`)))

ord_coef_tbl

ft_ord <- flextable(ord_coef_tbl) %>%
  set_caption("Table 16. Ordinal logistic regression: coefficients and odds ratios for Practice category") %>%
  theme_booktabs() %>%
  flextable::align(align = "center", part = "all") %>%
  flextable::align(j = 1, align = "left", part = "all") %>%
  autofit()

ft_ord
save_as_docx(ft_ord, path = "tables/17_OrdinalLogistic_Coefficients.docx")

# 4. Model fit statistics
ll_model <- as.numeric(logLik(m_ord))
ll_null  <- as.numeric(logLik(m_null))
lr_stat  <- 2 * (ll_model - ll_null)
lr_df    <- length(coef(m_ord))
lr_p     <- pchisq(lr_stat, df = lr_df, lower.tail = FALSE)
mcfadden <- 1 - ll_model / ll_null

# Brant test (proportional odds assumption)
br <- brant::brant(m_ord)
print(br)
brant_omni_p <- br["Omnibus", "probability"]
brant_omni_x <- br["Omnibus", "X2"]
brant_df     <- br["Omnibus", "df"]

fit_tbl <- tibble(
  Statistic = c("N", "Log-likelihood (model)", "Log-likelihood (null)",
                "LR χ² (df)", "LR p-value", "McFadden R²",
                "AIC", "BIC", "Brant omnibus test"),
  Value = c(nrow(ord_df),
            round(ll_model, 2),
            round(ll_null, 2),
            paste0(round(lr_stat, 2), " (", lr_df, ")"),
            ifelse(lr_p < 0.001, "< 0.001", format(round(lr_p, 3), nsmall = 3)),
            round(mcfadden, 3),
            round(AIC(m_ord), 1),
            round(BIC(m_ord), 1),
            paste0("χ² = ", round(brant_omni_x, 2), ", df = ", brant_df,
                   ", p = ", format(round(brant_omni_p, 3), nsmall = 3),
                   ifelse(brant_omni_p > 0.05, " (assumption met)", " (assumption violated)")))
)

fit_tbl

ft_fit <- flextable(fit_tbl) %>%
  set_caption("Table 17. Ordinal logistic regression model fit statistics") %>%
  theme_booktabs() %>%
  flextable::align(align = "left", part = "all") %>%
  autofit()

ft_fit
save_as_docx(ft_fit, path = "tables/18_OrdinalLogistic_ModelFit.docx")



library(tidyverse)
library(MASS)
library(flextable)
library(patchwork)

# Uses: m_ord (polr model) and ord_df from the previous step
# (Practice_cat, DHIA, Knowledge, Attitude)

cats <- levels(ord_df$Practice_cat)

# ---------------------------------------------------------------
# 1. TABLE: Predicted probabilities for example cases
# ---------------------------------------------------------------
q <- function(x, p) as.numeric(quantile(x, p, na.rm = TRUE))

scenarios <- tibble(
  Scenario  = c("Low", "Medium", "High"),
  DHIA      = c(q(ord_df$DHIA, 0.10), q(ord_df$DHIA, 0.50), q(ord_df$DHIA, 0.90)),
  Knowledge = c(q(ord_df$Knowledge, 0.10), q(ord_df$Knowledge, 0.50), q(ord_df$Knowledge, 0.90)),
  Attitude  = c(q(ord_df$Attitude, 0.10), q(ord_df$Attitude, 0.50), q(ord_df$Attitude, 0.90))
)

pp <- predict(m_ord, newdata = scenarios, type = "probs")

pred_tbl <- bind_cols(scenarios, as_tibble(pp)) %>%
  mutate(`Most likely category` = cats[max.col(pp, ties.method = "first")]) %>%
  mutate(across(all_of(cats), ~ paste0(round(.x * 100, 1), "%"))) %>%
  rename(`Example scenario` = Scenario,
         `Digital Health` = DHIA)

pred_tbl

ft_pred <- flextable(pred_tbl) %>%
  set_caption("Table 18. Predicted probabilities of Practice category for example cases (low = 10th, medium = 50th, high = 90th percentile of predictors)") %>%
  theme_booktabs() %>%
  flextable::align(align = "center", part = "all") %>%
  flextable::align(j = 1, align = "left", part = "all") %>%
  autofit()

ft_pred
save_as_docx(ft_pred, path = "tables/19_OrdinalLogistic_PredictedProbs.docx")

# ---------------------------------------------------------------
# 2. Probability at minimum vs maximum DHIA (for the text)
# ---------------------------------------------------------------
range_dhia <- tibble(
  DHIA      = range(ord_df$DHIA),
  Knowledge = mean(ord_df$Knowledge),
  Attitude  = mean(ord_df$Attitude)
)
round(predict(m_ord, newdata = range_dhia, type = "probs") * 100, 1)

# ---------------------------------------------------------------
# 3. FIGURE 9: Predicted probability curves
# ---------------------------------------------------------------
curve_data <- function(var) {
  grid <- tibble(
    DHIA      = mean(ord_df$DHIA),
    Knowledge = mean(ord_df$Knowledge),
    Attitude  = mean(ord_df$Attitude)
  )[rep(1, 100), ]
  grid[[var]] <- seq(min(ord_df[[var]]), max(ord_df[[var]]), length.out = 100)
  
  as_tibble(predict(m_ord, newdata = grid, type = "probs")) %>%
    mutate(x = grid[[var]]) %>%
    pivot_longer(-x, names_to = "Category", values_to = "Probability") %>%
    mutate(Category = factor(Category, levels = cats))
}

cols <- c(Poor = "#E74C3C", Fair = "#F39C12", Good = "#3498DB", Excellent = "#2ECC71")

make_curve <- function(var, xlab, title) {
  ggplot(curve_data(var), aes(x, Probability, color = Category)) +
    geom_line(linewidth = 1.2) +
    scale_color_manual(values = cols) +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    labs(title = title, x = xlab, y = "Predicted probability", color = "Practice category") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5),
          panel.grid.minor = element_blank())
}

p_dhia <- make_curve("DHIA", "Digital Health Information Access score", "A. Digital Health Access")
p_know <- make_curve("Knowledge", "Knowledge score", "B. Knowledge")

fig9 <- (p_dhia | p_know) + plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
fig9

ggsave("figures/OrdinalLogistic_PredictedProbabilities_Combined.png",
       plot = fig9, width = 12, height = 5.5, dpi = 300, bg = "white")

# ---------------------------------------------------------------
# 4. Classification accuracy and confusion matrix
# ---------------------------------------------------------------
ord_df$Predicted <- predict(m_ord, type = "class")

cm <- table(Actual = ord_df$Practice_cat, Predicted = ord_df$Predicted)
cm

overall_acc <- sum(diag(cm)) / sum(cm)
per_cat <- tibble(
  Category               = rownames(cm),
  N                      = as.numeric(rowSums(cm)),
  Correct                = diag(cm),
  `Accuracy (recall, %)` = round(diag(cm) / rowSums(cm) * 100, 1),
  `Precision (%)`        = round(diag(cm) / pmax(colSums(cm), 1) * 100, 1)
)

cat("Overall accuracy:", round(overall_acc * 100, 2), "%\n")
per_cat

# Accuracy within one category (adjacent misclassification)
idx <- outer(as.integer(ord_df$Practice_cat), as.integer(ord_df$Predicted), "-")
mean(abs(diag(idx)) <= 1)   # proportion within one category
mean(abs(as.integer(ord_df$Practice_cat) - as.integer(ord_df$Predicted)) <= 1)

ft_cls <- flextable(per_cat) %>%
  set_caption(paste0("Table 19. Classification performance by Practice category (overall accuracy = ",
                     round(overall_acc * 100, 1), "%)")) %>%
  theme_booktabs() %>%
  flextable::align(align = "center", part = "all") %>%
  flextable::align(j = 1, align = "left", part = "all") %>%
  autofit()
ft_cls
save_as_docx(ft_cls, path = "tables/20_Ordinal_Classification_Performance.docx")

# ---------------------------------------------------------------
# 5. FIGURE 10: Confusion matrix heatmap
# ---------------------------------------------------------------
cm_df <- as.data.frame(cm) %>%
  mutate(Actual    = factor(Actual, levels = rev(cats)),
         Predicted = factor(Predicted, levels = cats))

fig10 <- ggplot(cm_df, aes(Predicted, Actual, fill = Freq)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = Freq), size = 6, fontface = "bold") +
  scale_fill_gradient(low = "#EAF2F8", high = "#2874A6") +
  labs(title = "Confusion Matrix: Practice Category",
       subtitle = paste0("Overall accuracy = ", round(overall_acc * 100, 1), "%"),
       x = "Predicted category", y = "Actual category", fill = "Count") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "grey30"),
        panel.grid = element_blank())

fig10
ggsave("figures/OrdinalLogistic_ConfusionMatrix.png",
       plot = fig10, width = 7, height = 6, dpi = 300, bg = "white")
