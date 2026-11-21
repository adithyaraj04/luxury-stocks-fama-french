# STEP 4: Diagnostic tests and robust standard errors
library(car)         # VIF and hypothesis tests
library(lmtest)      # Breusch-Pagan, Durbin-Watson, Breusch-Godfrey, coeftest
library(sandwich)    # Newey-West robust standard errors
library(tseries)     # Jarque-Bera normality test
library(strucchange) # Chow test

dir.create("output", showWarnings = FALSE)
panel <- read.csv("data/analysis_dataset.csv")

# ================= PART A: learn the tests on Hermes =================
hermes <- subset(panel, stock == "Hermes")
ff3 <- lm(ExRet ~ MKT + SMB + HML, data = hermes)

vif(ff3)
bptest(ff3)
dwtest(ff3, alternative = "two.sided")
bgtest(ff3, order = 12)
jarque.bera.test(residuals(ff3))

plot(fitted(ff3), residuals(ff3), main = "Residuals vs fitted: Hermes",
     xlab = "Fitted excess return (%)", ylab = "Residual (%)")
abline(h = 0)

coeftest(ff3)
coeftest(ff3, vcov = NeweyWest(ff3))

hermes$MKT_c <- hermes$MKT - mean(hermes$MKT)
vif(lm(ExRet ~ MKT + Crisis + MKT:Crisis, data = hermes))
vif(lm(ExRet ~ MKT_c + Crisis + MKT_c:Crisis, data = hermes))

# ================= PART B: all four stocks =================
diag   <- data.frame()
robust <- data.frame()
tests  <- data.frame()

for (s in unique(panel$stock)) {
  d <- subset(panel, stock == s)
  d$MKT_c <- d$MKT - mean(d$MKT)
  
  models <- list(
    "1 CAPM"        = lm(ExRet ~ MKT, data = d),
    "2 Fama-French" = lm(ExRet ~ MKT + SMB + HML, data = d),
    "3 Crisis"      = lm(ExRet ~ MKT_c + Crisis + MKT_c:Crisis, data = d))
  
  for (m_name in names(models)) {
    m  <- models[[m_name]]
    dw <- dwtest(m, alternative = "two.sided")
    
    diag <- rbind(diag, data.frame(
      Stock = s, Model = m_name,
      Max_VIF = if (m_name == "1 CAPM") NA else max(vif(m)),
      BP_p = bptest(m)$p.value,
      DW   = unname(dw$statistic),
      DW_p = dw$p.value,
      BG_p = bgtest(m, order = 12)$p.value,
      JB_p = jarque.bera.test(residuals(m))$p.value))
    
    rob <- coeftest(m, vcov = NeweyWest(m))
    robust <- rbind(robust, data.frame(
      Stock = s, Model = m_name, Term = rownames(rob),
      Estimate = rob[, 1], SE_NW = rob[, 2],
      t_NW = rob[, 3], p_NW = rob[, 4],
      row.names = NULL))
  }
  
  h1 <- linearHypothesis(models[["1 CAPM"]], "MKT = 1",
                         vcov. = NeweyWest(models[["1 CAPM"]]))
  h3 <- linearHypothesis(models[["3 Crisis"]],
                         c("Crisis = 0", "MKT_c:Crisis = 0"),
                         vcov. = NeweyWest(models[["3 Crisis"]]))
  
  k    <- which(d$ym == "2020-02")
  chow <- sctest(ExRet ~ MKT + SMB + HML, data = d, type = "Chow", point = k)
  
  tests <- rbind(tests, data.frame(
    Stock = s,
    H1_p_NW = h1[2, "Pr(>F)"], H3_p_NW = h3[2, "Pr(>F)"],
    Chow_F = unname(chow$statistic), Chow_p = chow$p.value))
}

diag[, 3:8]   <- round(diag[, 3:8], 3)
robust[, 4:7] <- round(robust[, 4:7], 3)
tests[, 2:5]  <- round(tests[, 2:5], 3)

write.csv(diag,   "output/diagnostics.csv",         row.names = FALSE)
write.csv(robust, "output/robust_results.csv",      row.names = FALSE)
write.csv(tests,  "output/robust_tests_chow.csv",   row.names = FALSE)

print(diag)
print(robust)
print(tests)