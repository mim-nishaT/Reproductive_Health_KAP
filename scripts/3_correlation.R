library(tidyverse)
library(Hmisc)
library(corrplot)
library(flextable)

data <- readxl::read_xlsx("data/DHI_Access_Health_Literacy.xlsx")

# 1. Prepare the four total scores
kap <- data %>%
  transmute(
    `Digital Health` = as.numeric(TotalDigitalHealthInformationAccess),
    Knowledge        = as.numeric(TotalKnowledgeScore),
    Attitude         = as.numeric(TotalAttitudeScore),
    Practice         = as.numeric(TotalPracticeScore)
  )

# 2. Spearman correlation matrix, n and p-values
rc <- Hmisc::rcorr(as.matrix(kap), type = "spearman")
rho <- rc$r
p   <- rc$P

round(rho, 3)
round(p, 4)

# 3. Pairwise table (rho, 95% CI, p, Holm-adjusted p, strength)
pairs <- combn(names(kap), 2, simplify = FALSE)

cor_tbl <- map_dfr(pairs, function(v) {
  x  <- kap[[v[1]]]
  y  <- kap[[v[2]]]
  ok <- complete.cases(x, y)
  ct <- suppressWarnings(cor.test(x[ok], y[ok], method = "spearman", exact = FALSE))
  r  <- unname(ct$estimate)
  n  <- sum(ok)
  # 95% CI via Fisher z (approximate for Spearman)
  z  <- atanh(r); se <- 1.06 / sqrt(n - 3)
  tibble(
    `Variable 1` = v[1],
    `Variable 2` = v[2],
    N            = n,
    rho          = r,
    CI_low       = tanh(z - 1.96 * se),
    CI_high      = tanh(z + 1.96 * se),
    p_raw        = ct$p.value
  )
}) %>%
  mutate(
    p_holm   = p.adjust(p_raw, method = "holm"),
    Strength = case_when(
      abs(rho) >= 0.70 ~ "Strong",
      abs(rho) >= 0.40 ~ "Moderate",
      abs(rho) >= 0.20 ~ "Weak",
      TRUE             ~ "Negligible"
    ),
    rho        = round(rho, 3),
    `95% CI`   = paste0(round(CI_low, 3), " to ", round(CI_high, 3)),
    `p-value`  = ifelse(p_raw < 0.001, "< 0.001", format(round(p_raw, 3), nsmall = 3)),
    `p (Holm)` = ifelse(p_holm < 0.001, "< 0.001", format(round(p_holm, 3), nsmall = 3))
  ) %>%
  select(`Variable 1`, `Variable 2`, N, rho, `95% CI`, `p-value`, `p (Holm)`, Strength)

cor_tbl

ft_cor <- flextable(cor_tbl) %>%
  set_caption("Table 15. Spearman rank correlations among the KAP domains") %>%
  theme_booktabs() %>%
  flextable::align(align = "center", part = "all") %>%
  flextable::align(j = 1:2, align = "left", part = "all") %>%
  autofit()

ft_cor
save_as_docx(ft_cor, path = "tables/Spearman_Correlations.docx")

# Fix: set the diagonal of the p-matrix to 0 and use plain matrices
p_mat <- p
diag(p_mat) <- 0
rho_mat <- as.matrix(rho)

dev.off()   # close any open device from the failed call

png("figures/Spearman_Correlation_Matrix.png", width = 2400, height = 2400, res = 300)
corrplot::corrplot(rho_mat, method = "circle", type = "upper",
                   p.mat = p_mat, sig.level = 0.05, insig = "pch",
                   addCoef.col = "black", number.cex = 1.1,
                   tl.col = "black", tl.srt = 45,
                   col = colorRampPalette(c("#E74C3C", "white", "#2ECC71"))(200),
                   title = "Spearman Correlation Matrix", mar = c(0, 0, 2, 0))
dev.off()
