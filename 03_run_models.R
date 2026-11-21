# STEP 3: Run the three regression models
library(car)
dir.create("output", showWarnings = FALSE)
panel <- read.csv("data/analysis_dataset.csv")

# ================= PART A: one stock (Hermes) =================
hermes <- subset(panel, stock == "Hermes")

# Model 1: CAPM
capm <- lm(ExRet ~ MKT, data = hermes)
summary(capm)

# Model 2: Fama-French
ff3 <- lm(ExRet ~ MKT + SMB + HML, data = hermes)
summary(ff3)

# Model 3: Crisis model
hermes$MKT_c <- hermes$MKT - mean(hermes$MKT)
crisis <- lm(ExRet ~ MKT_c + Crisis + MKT_c:Crisis, data = hermes)
summary(crisis)

# Hypothesis tests
linearHypothesis(capm, "MKT = 1")
linearHypothesis(crisis, c("Crisis = 0", "MKT_c:Crisis = 0"))

# ================= PART B: all four stocks =================
results <- data.frame()
tests   <- data.frame()

for (s in unique(panel$stock)) {
  d <- subset(panel, stock == s)
  d$MKT_c <- d$MKT - mean(d$MKT)
  
  models <- list(
    "1 CAPM"        = lm(ExRet ~ MKT, data = d),
    "2 Fama-French" = lm(ExRet ~ MKT + SMB + HML, data = d),
    "3 Crisis"      = lm(ExRet ~ MKT_c + Crisis + MKT_c:Crisis, data = d))
  
  for (m_name in names(models)) {
    m  <- models[[m_name]]
    ct <- summary(m)$coefficients
    f  <- summary(m)$fstatistic
    results <- rbind(results, data.frame(
      Stock = s, Model = m_name, Term = rownames(ct),
      Estimate = ct[, 1], SE = ct[, 2], t = ct[, 3], p = ct[, 4],
      R2 = summary(m)$r.squared, Adj_R2 = summary(m)$adj.r.squared,
      F_stat = unname(f[1]),
      F_p = unname(pf(f[1], f[2], f[3], lower.tail = FALSE)),
      row.names = NULL))
  }
  
  h1 <- linearHypothesis(models[["1 CAPM"]], "MKT = 1")
  h3 <- linearHypothesis(models[["3 Crisis"]],
                         c("Crisis = 0", "MKT_c:Crisis = 0"))
  
  tests <- rbind(tests, data.frame(
    Stock = s,
    H1_F = h1[2, "F"], H1_p = h1[2, "Pr(>F)"],
    H3_F = h3[2, "F"], H3_p = h3[2, "Pr(>F)"]))
}

results[, 4:11] <- round(results[, 4:11], 3)
tests[, 2:5]    <- round(tests[, 2:5], 3)

write.csv(results, "output/regression_results.csv", row.names = FALSE)
write.csv(tests,   "output/hypothesis_tests.csv",   row.names = FALSE)

print(results)
print(tests)