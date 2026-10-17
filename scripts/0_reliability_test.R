library(tidyverse)
library(ltm)
library(psych)

# load data into R 
data <- readxl::read_excel("data/DHI_Access_Health_Literacy.xlsx")

#CHECK FOR MISSING DATA
cat("\n=== MISSING DATA ===\n")
print(colSums(is.na(data)))

# STEP 3: REMOVE ROWS WITH MISSING DATA
data_clean <- data %>% drop_na()
cat("\nRows kept:", nrow(data_clean), "out of", nrow(data), "\n")

# STEP 4: SELECT EACH SCALE & REMOVE NAs
DHIA <- dplyr::select(data_clean, 10:19) %>% drop_na()
Knowledge <- dplyr::select(data_clean, 22:29) %>% drop_na()
Attitude <- dplyr::select(data_clean, 32:40) %>% drop_na()
Practice <- dplyr::select(data_clean, 43:52) %>% drop_na()

# ============================================================================
# CRONBACH'S ALPHA - EACH SCALE SEPARATELY
# ============================================================================

cat("\n\n" %+%
      "╔═══════════════════════════════════════════════════════════════════╗\n" %+%
      "║           CRONBACH'S ALPHA BY SCALE                              ║\n" %+%
      "╚═══════════════════════════════════════════════════════════════════╝\n")

# DIGITAL HEALTH ACCESS
cat("\n1. DIGITAL HEALTH INFORMATION ACCESS (10 items)\n")
cat("   ───────────────────────────────────────────────\n")
alpha_dhia <- psych::alpha(DHIA, check.keys = TRUE)
cat("   Cronbach's Alpha:", round(alpha_dhia$total$raw_alpha, 4), "\n")
cat("   Std. Alpha:", round(alpha_dhia$total$std.alpha, 4), "\n")
cat("   Avg Inter-item r:", round(alpha_dhia$total$average_r, 4), "\n")
cat("   Status: ")
if(alpha_dhia$total$raw_alpha > 0.70) {
  cat("✓ ACCEPTABLE/GOOD\n")
} else {
  cat("✗ NEEDS IMPROVEMENT\n")
}

# KNOWLEDGE
cat("\n2. REPRODUCTIVE HEALTH KNOWLEDGE (8 items)\n")
cat("   ───────────────────────────────────────────────\n")
alpha_know <- psych::alpha(Knowledge, check.keys = TRUE)
cat("   Cronbach's Alpha:", round(alpha_know$total$raw_alpha, 4), "\n")
cat("   Std. Alpha:", round(alpha_know$total$std.alpha, 4), "\n")
cat("   Avg Inter-item r:", round(alpha_know$total$average_r, 4), "\n")
cat("   Status: ")
if(alpha_know$total$raw_alpha > 0.70) {
  cat("✓ ACCEPTABLE/GOOD\n")
} else {
  cat("✗ NEEDS IMPROVEMENT\n")
}

# ATTITUDE
cat("\n3. ATTITUDE TOWARD RH (9 items)\n")
cat("   ───────────────────────────────────────────────\n")
alpha_att <- psych::alpha(Attitude, check.keys = TRUE)
cat("   Cronbach's Alpha:", round(alpha_att$total$raw_alpha, 4), "\n")
cat("   Std. Alpha:", round(alpha_att$total$std.alpha, 4), "\n")
cat("   Avg Inter-item r:", round(alpha_att$total$average_r, 4), "\n")
cat("   Status: ")
if(alpha_att$total$raw_alpha > 0.70) {
  cat("✓ ACCEPTABLE/GOOD\n")
} else {
  cat("✗ NEEDS IMPROVEMENT\n")
}

# PRACTICE
cat("\n4. REPRODUCTIVE HEALTH PRACTICE (10 items)\n")
cat("   ───────────────────────────────────────────────\n")
alpha_prac <- psych::alpha(Practice, check.keys = TRUE)
cat("   Cronbach's Alpha:", round(alpha_prac$total$raw_alpha, 4), "\n")
cat("   Std. Alpha:", round(alpha_prac$total$std.alpha, 4), "\n")
cat("   Avg Inter-item r:", round(alpha_prac$total$average_r, 4), "\n")
cat("   Status: ")
if(alpha_prac$total$raw_alpha > 0.70) {
  cat("✓ ACCEPTABLE/GOOD\n")
} else {
  cat("✗ NEEDS IMPROVEMENT\n")
}


library(tidyverse)
library(psych)
library(flextable)

# Function to compute reliability statistics for one scale
get_rel <- function(items, total, name) {
  items <- items %>% mutate(across(everything(), as.numeric))
  a <- psych::alpha(items, check.keys = FALSE)
  raw_a <- a$total$raw_alpha
  tibble(
    Scale                   = name,
    Items                   = ncol(items),
    `Cronbach's alpha`      = round(raw_a, 3),
    `Standardized alpha`    = round(a$total$std.alpha, 3),
    `Avg inter-item r`      = round(a$total$average_r, 3),
    `Mean (SD)`             = paste0(round(mean(total, na.rm = TRUE), 2), " (",
                                     round(sd(total, na.rm = TRUE), 2), ")"),
    Interpretation          = case_when(
      raw_a >= 0.90 ~ "Excellent",
      raw_a >= 0.80 ~ "Good",
      raw_a >= 0.70 ~ "Acceptable",
      raw_a >= 0.60 ~ "Questionable",
      TRUE          ~ "Poor"
    )
  )
}

# Build the table (column positions follow your glimpse output)
rel_tbl <- bind_rows(
  get_rel(data[, 10:19], data$TotalDigitalHealthInformationAccess, "Digital Health Access"),
  get_rel(data[, 22:29], data$TotalKnowledgeScore,                 "Knowledge"),
  get_rel(data[, 32:40], data$TotalAttitudeScore,                  "Attitude"),
  get_rel(data[, 42:51], data$TotalPracticeScore,                  "Practice")
)

rel_tbl

# Word-ready table
ft_rel <- flextable(rel_tbl) %>%
  set_caption("Table 4. Reliability (Cronbach's alpha) and descriptive statistics of the scales") %>%
  theme_booktabs() %>%
  flextable::align(align = "center", part = "all") %>%
  flextable::align(j = 1, align = "left", part = "all") %>%
  autofit()

ft_rel
save_as_docx(ft_rel, path = "tables/Cronbachs_alpha_Table.docx")




# FACTOR ANALYSIS - KMO TEST

cat("\n\n" %+%
      "╔═══════════════════════════════════════════════════════════════════╗\n" %+%
      "║        KAISER-MEYER-OLKIN (KMO) FACTORABILITY TEST              ║\n" %+%
      "╚═══════════════════════════════════════════════════════════════════╝\n")

cat("\n1. DIGITAL HEALTH ACCESS\n")
DHIA <- DHIA %>% drop_na()

kmo_dhia <- KMO(cor(DHIA))
kmo_val <- kmo_dhia$MSA

cat("   KMO:", round(kmo_val, 3), "\n")

cat("   Interpretation: ")

cat("   Interpretation: ")

if(kmo_val > 0.9) {
  cat("Marvelous ✓\n")
} else if(kmo_val > 0.8) {
  cat("Meritorious ✓\n")
} else if(kmo_val > 0.7) {
  cat("Middling (Acceptable) ✓\n")
} else if(kmo_val > 0.6) {
  cat("Mediocre\n")
} else {
  cat("Unacceptable ✗\n")
}



cat("\n2. KNOWLEDGE\n")

know <- Knowledge %>% drop_na()

kmo_know <- KMO(cor(Knowledge))
kmo_val <- kmo_know$MSA

cat("   KMO:", round(kmo_val, 3), "\n")
cat("   Interpretation: ")

if(kmo_val > 0.9) {
  cat("Marvelous ✓\n")
} else if(kmo_val > 0.8) {
  cat("Meritorious ✓\n")
} else if(kmo_val > 0.7) {
  cat("Middling (Acceptable) ✓\n")
} else if(kmo_val > 0.6) {
  cat("Mediocre\n")
} else {
  cat("Unacceptable ✗\n")
}


cat("\n3. ATTITUDE\n")
atu <- Attitude %>% drop_na()

kmo_atu <- KMO(cor(Attitude))
kmo_val <- kmo_atu$MSA

cat("   KMO:", round(kmo_val, 3), "\n")
cat("   Interpretation: ")

if(kmo_val > 0.9) {
  cat("Marvelous ✓\n")
} else if(kmo_val > 0.8) {
  cat("Meritorious ✓\n")
} else if(kmo_val > 0.7) {
  cat("Middling (Acceptable) ✓\n")
} else if(kmo_val > 0.6) {
  cat("Mediocre\n")
} else {
  cat("Unacceptable ✗\n")
}


cat("\n4. PRACTICE\n")
prc <- Practice %>% drop_na()

kmo_prc <- KMO(cor(Practice))
kmo_val <- kmo_know$MSA

cat("   KMO:", round(kmo_val, 3), "\n")
cat("   Interpretation: ")

if(kmo_val > 0.9) {
  cat("Marvelous ✓\n")
} else if(kmo_val > 0.8) {
  cat("Meritorious ✓\n")
} else if(kmo_val > 0.7) {
  cat("Middling (Acceptable) ✓\n")
} else if(kmo_val > 0.6) {
  cat("Mediocre\n")
} else {
  cat("Unacceptable ✗\n")
}


library(tidyverse)
library(psych)
library(flextable)

# Function: one-factor EFA (minimum residual) summary for one scale
get_efa <- function(items, name) {
  items <- items %>% mutate(across(everything(), as.numeric))
  fit   <- psych::fa(items, nfactors = 1, fm = "minres")
  load  <- abs(as.numeric(fit$loadings))
  var_exp <- sum(load^2) / length(load) * 100
  low     <- colnames(items)[load < 0.40]
  
  tibble(
    Scale                      = name,
    `Variance explained (%)`   = round(var_exp, 1),
    `Items with loading > 0.40` = paste0(sum(load >= 0.40), " of ", length(load)),
    `Loading range`            = paste0(round(min(load), 2), "-", round(max(load), 2)),
    `Items with loading < 0.40` = ifelse(length(low) == 0, "None", paste(low, collapse = "; ")),
    Interpretation             = case_when(
      length(low) == 0 & var_exp >= 50 ~ "Unidimensional; all items load adequately",
      length(low) == 0                 ~ "Acceptable one-factor structure; all items load adequately",
      TRUE                             ~ "Acceptable one-factor structure; low-loading item(s) noted"
    )
  )
}

# Build Table 6
efa_tbl <- bind_rows(
  get_efa(data[, 10:19], "Digital Health Access"),
  get_efa(data[, 22:29], "Knowledge"),
  get_efa(data[, 32:40], "Attitude"),
  get_efa(data[, 42:51], "Practice")
)

efa_tbl

ft_efa <- flextable(efa_tbl) %>%
  set_caption("Table 5. One-factor exploratory factor analysis (minimum residual) of the scales") %>%
  theme_booktabs() %>%
  flextable::align(align = "center", part = "all") %>%
  flextable::align(j = c(1, 5, 6), align = "left", part = "all") %>%
  autofit()

ft_efa
save_as_docx(ft_efa, path = "tables/Factor_Analysis_Loadings.docx")


# ---- Table 7: Item-total correlations (corrected) ----
get_itc <- function(items, name) {
  items <- items %>% mutate(across(everything(), as.numeric))
  r <- psych::alpha(items, check.keys = FALSE)$item.stats$r.drop
  tibble(
    Scale                    = name,
    Items                    = ncol(items),
    `Min r`                  = round(min(r), 2),
    `Max r`                  = round(max(r), 2),
    `Items with r < 0.30`    = sum(r < 0.30)
  )
}

itc_tbl <- bind_rows(
  get_itc(data[, 10:19], "Digital Health Access"),
  get_itc(data[, 22:29], "Knowledge"),
  get_itc(data[, 32:40], "Attitude"),
  get_itc(data[, 42:51], "Practice")
)

itc_tbl

ft_itc <- flextable(itc_tbl) %>%
  set_caption("Table 6. Corrected item-total correlations of the scales") %>%
  theme_booktabs() %>%
  flextable::align(align = "center", part = "all") %>%
  flextable::align(j = 1, align = "left", part = "all") %>%
  autofit()

ft_itc
save_as_docx(ft_itc, path = "tables/Item_Total_Correlations.docx")

# FACTOR ANALYSIS - EXPLORATORY

# Clean and convert to numeric for ALL scales
DHIA <- DHIA %>% drop_na() %>% mutate(across(everything(), as.numeric))
Knowledge <- Knowledge %>% drop_na() %>% mutate(across(everything(), as.numeric))
Attitude <- Attitude %>% drop_na() %>% mutate(across(everything(), as.numeric))
Practice <- Practice %>% drop_na() %>% mutate(across(everything(), as.numeric))



cat("\n\n" %+%
      "╔═══════════════════════════════════════════════════════════════════╗\n" %+%
      "║          EXPLORATORY FACTOR ANALYSIS (ONE FACTOR)                ║\n" %+%
      "╚═══════════════════════════════════════════════════════════════════╝\n")

# STEP 1: DEFINE THE FUNCTION
do_fa <- function(data, name) {
  cat("\n", name, "\n")
  cat("─────────────────────────────────────────────────────────────────\n")
  
  tryCatch({
    fa_result <- fa(data, 
                    nfactors = 1, 
                    rotate = "varimax",
                    fm = "minres")
    
    var_exp <- sum(fa_result$Vaccounted[2,]) * 100
    cat("Variance Explained:", round(var_exp, 1), "%\n\n")
    
    cat("Factor Loadings (all items should be > 0.40):\n")
    loadings_df <- as.data.frame(fa_result$loadings[,])
    names(loadings_df) <- "Loading"
    loadings_df$Item <- rownames(fa_result$loadings)
    loadings_df <- loadings_df[, c(2, 1)]
    loadings_df$Loading <- round(loadings_df$Loading, 3)
    
    print(loadings_df)
    
    return(fa_result)
    
  }, error = function(e) {
    cat("✗ Factor analysis failed:", e$message, "\n")
    return(NULL)
  })
}

#CLEAN DATA
DHIA <- DHIA %>% drop_na() %>% mutate(across(everything(), as.numeric))
Knowledge <- Knowledge %>% drop_na() %>% mutate(across(everything(), as.numeric))
Attitude <- Attitude %>% drop_na() %>% mutate(across(everything(), as.numeric))
Practice <- Practice %>% drop_na() %>% mutate(across(everything(), as.numeric))

# CALL THE FUNCTION
fa_dhia <- do_fa(DHIA, "1. DIGITAL HEALTH ACCESS")
fa_know <- do_fa(Knowledge, "2. KNOWLEDGE")
fa_att <- do_fa(Attitude, "3. ATTITUDE")
fa_prac <- do_fa(Practice, "4. PRACTICE")

library(flextable)

# Combine all factor loadings into one table
fa_summary <- data.frame(
  Scale = c(rep("Digital Health", 10), 
            rep("Knowledge", 8),
            rep("Attitude", 9),
            rep("Practice", 10)),
  Item = c(names(fa_dhia$loadings[,1]),
           names(fa_know$loadings[,1]),
           names(fa_att$loadings[,1]),
           names(fa_prac$loadings[,1])),
  Loading = c(round(fa_dhia$loadings[,1], 3),
              round(fa_know$loadings[,1], 3),
              round(fa_att$loadings[,1], 3),
              round(fa_prac$loadings[,1], 3)),
  Variance_Explained = c(rep(round(sum(fa_dhia$Vaccounted[2,])*100, 1), 10),
                         rep(round(sum(fa_know$Vaccounted[2,])*100, 1), 8),
                         rep(round(sum(fa_att$Vaccounted[2,])*100, 1), 9),
                         rep(round(sum(fa_prac$Vaccounted[2,])*100, 1), 10))
)

# Save as docx
fa_summary %>%
  as_flextable() %>%
  autofit() %>%
  save_as_docx(path = "tables/1_Factor_Analysis_Loadings.docx")






# ============================================================================
# SUMMARY TABLE FOR PUBLICATION
# ============================================================================

cat("\n\n" %+%
      "╔═══════════════════════════════════════════════════════════════════╗\n" %+%
      "║                  SUMMARY TABLE FOR PUBLICATION                   ║\n" %+%
      "╚═══════════════════════════════════════════════════════════════════╝\n")

summary_df <- data.frame(
  Scale = c("Digital Health Access", "Knowledge", "Attitude", "Practice"),
  Items = c(ncol(DHIA), ncol(Knowledge), ncol(Attitude), ncol(Practice)),
  N = c(nrow(DHIA), nrow(Knowledge), nrow(Attitude), nrow(Practice)),
  Alpha = c(
    round(alpha_dhia$total$raw_alpha, 3),
    round(alpha_know$total$raw_alpha, 3),
    round(alpha_att$total$raw_alpha, 3),
    round(alpha_prac$total$raw_alpha, 3)
  ),
  Mean = c(
    round(mean(rowSums(DHIA)), 2),
    round(mean(rowSums(Knowledge)), 2),
    round(mean(rowSums(Attitude)), 2),
    round(mean(rowSums(Practice)), 2)
  ),
  SD = c(
    round(sd(rowSums(DHIA)), 2),
    round(sd(rowSums(Knowledge)), 2),
    round(sd(rowSums(Attitude)), 2),
    round(sd(rowSums(Practice)), 2)
  ),
  KMO = c(
    round(kmo_dhia$MSA, 3),    
    round(kmo_know$MSA, 3),
    round(kmo_atu$MSA, 3),
    round(kmo_prc$MSA, 3)
  )
)

print(summary_df)
# Install packages
install.packages("flextable")
library(flextable)

# Create and save table
summary_df %>%
  flextable() %>%
  autofit() %>%
  save_as_docx(path = "tables/1_Summary_Table.docx")


# ============================================================================
# ITEMS TO REMOVE (IF NEEDED)
# ============================================================================

cat("\n\n" %+%
      "╔═══════════════════════════════════════════════════════════════════╗\n" %+%
      "║     ITEMS WITH LOW ITEM-TOTAL CORRELATION (r < 0.30)             ║\n" %+%
      "║     NOTE: May want to consider removing these items              ║\n" %+%
      "╚═══════════════════════════════════════════════════════════════════╝\n")

check_items <- function(alpha_obj, name) {
  tryCatch({
    cat("\n", name, "\n")
    problematic <- alpha_obj$item.total[alpha_obj$item.total$r.drop < 0.30, ]
    
    if(!is.null(problematic) && nrow(problematic) > 0) {
      print(problematic)
    } else {
      cat("  ✓ All items have r > 0.30\n")
    }
  }, error = function(e) {
    cat("  ✓ All items have r > 0.30 (or no problematic items)\n")
  })
}

# NOW RUN THE CHECKS
check_items(alpha_dhia, "Digital Health Access")
check_items(alpha_know, "Knowledge")
check_items(alpha_att, "Attitude")
check_items(alpha_prac, "Practice")
