library(tidyverse)
library(broom)
library(lmtest)
library(sandwich)
library(flextable)

data<- readxl::read_xlsx("data/DHI_Access_Health_Literacy.xlsx")
library(tidyverse)
library(broom)
library(flextable)

main_tbl <- map2_dfr(models, names(models), function(m, outcome) {
  vc <- sandwich::vcovHC(m, type = "HC3")
  ct <- lmtest::coeftest(m, vcov. = vc)
  ci <- lmtest::coefci(m, vcov. = vc)
  g  <- broom::glance(m)
  sd_x <- sd(df$DHIA, na.rm = TRUE)
  sd_y <- sd(m$model[[1]], na.rm = TRUE)
  
  tibble(
    Outcome   = outcome,
    B         = round(ct["DHIA", 1], 3),
    `95% CI`  = paste0(round(ci["DHIA", 1], 3), " to ", round(ci["DHIA", 2], 3)),
    Beta      = round(ct["DHIA", 1] * sd_x / sd_y, 3),
    p         = ifelse(ct["DHIA", 4] < 0.001, "< 0.001",
                       format(round(ct["DHIA", 4], 3), nsmall = 3)),
    `Adj. R²` = round(g$adj.r.squared, 3),
    N         = g$nobs
  )
})

main_tbl

ft_main <- flextable(main_tbl) %>%
  set_caption("Table 12. Association of digital health information access with KAP scores (linear regression, adjusted for socio-demographic factors)") %>%
  theme_booktabs() %>%
  flextable::align(align = "center", part = "all") %>%
  flextable::align(j = 1, align = "left", part = "all") %>%
  autofit()

ft_main
save_as_docx(ft_main, path = "tables/LinearRegression_Maintable.docx")


# 4. Model fit table
fit_tbl <- map2_dfr(models, outcomes, function(m, outcome) {
  g <- broom::glance(m)
  tibble(
    Outcome         = outcome,
    N               = g$nobs,
    `R-squared`     = round(g$r.squared, 3),
    `Adj. R-squared`= round(g$adj.r.squared, 3),
    `F statistic`   = round(g$statistic, 2),
    `df`            = paste0(g$df, ", ", g$df.residual),
    `p-value`       = ifelse(g$p.value < 0.001, "< 0.001", format(round(g$p.value, 3), nsmall = 3)),
    RMSE            = round(g$sigma, 2),
    AIC             = round(g$AIC, 1)
  )
})

fit_tbl

ft_fit <- flextable(fit_tbl) %>%
  set_caption("Table 12. Linear regression model fit statistics") %>%
  theme_booktabs() %>%
  flextable::align(align = "center", part = "all") %>%
  flextable::align(j = 1, align = "left", part = "all") %>%
  autofit()

ft_fit
save_as_docx(ft_fit, path = "tables/LinearRegression_ModelFit.docx")

library(tidyverse)
library(car)
library(lmtest)
library(flextable)

# 1. Diagnostic tests per model
diag_tbl <- map2_dfr(models, names(models), function(m, outcome) {
  sw <- shapiro.test(residuals(m))
  bp <- lmtest::bptest(m)
  v  <- car::vif(m)
  gv <- if (is.matrix(v)) v[, "GVIF^(1/(2*Df))"] else v
  cd <- cooks.distance(m)
  
  tibble(
    Outcome                = outcome,
    `Shapiro-Wilk W`       = round(unname(sw$statistic), 3),
    `SW p-value`           = ifelse(sw$p.value < 0.001, "< 0.001", format(round(sw$p.value, 3), nsmall = 3)),
    `Breusch-Pagan χ²`     = round(unname(bp$statistic), 2),
    `BP df`                = unname(bp$parameter),
    `BP p-value`           = ifelse(bp$p.value < 0.001, "< 0.001", format(round(bp$p.value, 3), nsmall = 3)),
    `Max adj. GVIF`        = round(max(gv), 2),
    `Max Cook's D`         = round(max(cd), 3)
  )
})

diag_tbl

ft_diag <- flextable(diag_tbl) %>%
  set_caption("Table 14. Regression assumption tests for the three models") %>%
  theme_booktabs() %>%
  flextable::align(align = "center", part = "all") %>%
  flextable::align(j = 1, align = "left", part = "all") %>%
  autofit()
ft_diag

# 2. Diagnostic plots: one PNG per model (4 panels each)
for (nm in names(models)) {
  png(paste0("figures/Linear_Regression_Diagnostics_", nm, ".png"),
      width = 2400, height = 2000, res = 300)
  par(mfrow = c(2, 2))
  plot(models[[nm]], main = nm)
  dev.off()
}


library(tidyverse)

# 1. Observed vs predicted from the final adjusted Practice model
plot_df <- tibble(
  Observed  = df$Practice[as.numeric(names(fitted(models$Practice)))],
  Predicted = as.numeric(fitted(models$Practice))
)

# 2. Plot
fig_op <- ggplot(plot_df, aes(x = Observed, y = Predicted)) +
  geom_point(color = "#3498DB", alpha = 0.5, size = 2) +
  geom_abline(slope = 1, intercept = 0, color = "red",
              linetype = "dashed", linewidth = 1) +
  coord_cartesian(xlim = c(10, 50), ylim = c(10, 52)) +
  labs(title = "Observed vs Predicted Practice Scores",
       subtitle = paste0("Adjusted R² = ",
                         round(summary(models$Practice)$adj.r.squared, 3),
                         ", N = ", nrow(plot_df)),
       x = "Observed Practice Score",
       y = "Predicted Practice Score") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 10, color = "grey30"),
        panel.grid.minor = element_blank())

fig_op

# 3. Save
ggsave("figures/Observed_vs_Predicted_Practice.png",
       plot = fig_op, width = 8, height = 6, dpi = 300, bg = "white")

op_all <- map2_dfr(models, names(models), function(m, nm) {
  tibble(Outcome   = nm,
         Observed  = m$model[[1]],
         Predicted = as.numeric(fitted(m)))
}) %>%
  mutate(Outcome = factor(Outcome, levels = c("Knowledge", "Attitude", "Practice")))

fig_op_all <- ggplot(op_all, aes(Observed, Predicted)) +
  geom_point(color = "#3498DB", alpha = 0.5, size = 1.8) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  facet_wrap(~ Outcome, scales = "free") +
  labs(x = "Observed score", y = "Predicted score") +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank())

fig_op_all

# 3. Save
ggsave("figures/Observed_vs_Predicted_all.png",
       plot = fig_op_all, width = 8, height = 6, dpi = 300, bg = "white")
