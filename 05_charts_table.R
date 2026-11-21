# STEP 5: Summary statistics, charts and slide-ready tables
library(ggplot2)
library(car)

panel  <- read.csv("data/analysis_dataset.csv")
robust <- read.csv("output/robust_results.csv")
ols    <- read.csv("output/regression_results.csv")

dir.create("output/charts", showWarnings = FALSE)

panel$date <- as.Date(paste0(panel$ym, "-01"))
stocks  <- c("Hermes", "LVMH", "Kering", "Burberry")
colours <- c(Hermes = "#E36C0A", LVMH = "#6B4E9B", Kering = "#2E86AB",
             Burberry = "#8C6D3F", "European market" = "black")
source_note <- "Source: Yahoo Finance; Kenneth French Data Library; own calculations in R"

one <- subset(panel, stock == "Hermes")

# ---- 1. Summary statistics ----
stat_row <- function(x, name) {
  data.frame(Variable = name, N = length(x), Mean = mean(x), SD = sd(x),
             Min = min(x), Max = max(x))
}

sumstats <- data.frame()
for (s in stocks) {
  sumstats <- rbind(sumstats, stat_row(subset(panel, stock == s)$ExRet,
                                       paste(s, "excess return (%)")))
}
sumstats <- rbind(sumstats,
                  stat_row(one$MKT,    "Market excess return, MKT (%)"),
                  stat_row(one$SMB,    "SMB (%)"),
                  stat_row(one$HML,    "HML (%)"),
                  stat_row(one$RF,     "Risk-free rate, RF (%)"),
                  stat_row(one$Crisis, "Crisis dummy (0/1)"))

sumstats[, 3:6] <- round(sumstats[, 3:6], 2)
write.csv(sumstats, "output/slide_summary_statistics.csv", row.names = FALSE)
print(sumstats)

# ---- 2. VIF before and after centring ----
h <- one
h$MKT_c <- h$MKT - mean(h$MKT)

vif_table <- data.frame(
  Term = c("Market", "Crisis", "Market x Crisis"),
  VIF_not_centred = unname(vif(lm(ExRet ~ MKT + Crisis + MKT:Crisis, data = h))),
  VIF_centred     = unname(vif(lm(ExRet ~ MKT_c + Crisis + MKT_c:Crisis, data = h))))

vif_table[, 2:3] <- round(vif_table[, 2:3], 2)
write.csv(vif_table, "output/slide_vif_centring.csv", row.names = FALSE)
print(vif_table)

# ---- 3. Slide-ready results tables ----
slide <- merge(robust,
               ols[, c("Stock", "Model", "Term", "R2", "Adj_R2", "F_stat", "F_p")],
               by = c("Stock", "Model", "Term"), sort = FALSE)

for (mod in unique(slide$Model)) {
  write.csv(subset(slide, Model == mod),
            paste0("output/slide_table_", gsub(" ", "_", mod), ".csv"),
            row.names = FALSE)
}

# ---- 4. Chart 1: growth of $100 ----
panel$Value <- 100 * ave(1 + panel$R / 100, panel$stock, FUN = cumprod)
one$Value   <- 100 * cumprod(1 + (one$MKT + one$RF) / 100)

growth <- rbind(
  data.frame(date = panel$date, Series = panel$stock, Value = panel$Value),
  data.frame(date = one$date, Series = "European market", Value = one$Value))

crises <- data.frame(start = as.Date(c("2008-09-01", "2020-02-01")),
                     end   = as.Date(c("2009-03-31", "2020-04-30")))

chart1 <- ggplot(growth, aes(date, Value, colour = Series)) +
  geom_rect(data = crises, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = "grey85") +
  geom_line(linewidth = 1) +
  scale_y_log10() +
  scale_colour_manual(values = colours) +
  labs(title = "Growth of $100 invested in January 2006",
       subtitle = "Log scale, USD. Grey bands = crisis months",
       x = NULL, y = "Value of $100", colour = NULL, caption = source_note) +
  theme_minimal(base_size = 16) + theme(legend.position = "bottom")

ggsave("output/charts/1_growth_of_100.png", chart1, width = 10, height = 5.6, dpi = 200)

# ---- 5. Chart 2: scatter plot, Hermes vs Burberry ----
sc <- subset(panel, stock %in% c("Hermes", "Burberry"))
beta <- sapply(c("Hermes", "Burberry"), function(s)
  unname(coef(lm(ExRet ~ MKT, data = subset(panel, stock == s)))["MKT"]))
sc$Label <- paste0(sc$stock, ": beta = ", round(beta[sc$stock], 2))

chart2 <- ggplot(sc, aes(MKT, ExRet)) +
  geom_point(aes(colour = stock), alpha = 0.5, size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = "black") +
  facet_wrap(~ Label) +
  scale_colour_manual(values = colours, guide = "none") +
  labs(title = "Stock vs market monthly excess returns",
       subtitle = "Solid line = fitted CAPM regression; dashed line = beta of 1",
       x = "Market excess return (%)", y = "Stock excess return (%)",
       caption = source_note) +
  theme_minimal(base_size = 16)

ggsave("output/charts/2_scatter_hermes_burberry.png", chart2, width = 10, height = 5.6, dpi = 200)

# ---- 6. Chart 3: CAPM beta with 95% confidence intervals ----
b <- subset(robust, Model == "1 CAPM" & Term == "MKT")
b$Stock <- factor(b$Stock, levels = stocks)

chart3 <- ggplot(b, aes(Stock, Estimate, fill = Stock)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = Estimate - 1.96 * SE_NW, ymax = Estimate + 1.96 * SE_NW),
                width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_fill_manual(values = colours, guide = "none") +
  labs(title = "Market beta (CAPM) with 95% confidence intervals",
       subtitle = "Dashed line = beta of 1. Newey-West robust standard errors",
       x = NULL, y = "Beta", caption = source_note) +
  theme_minimal(base_size = 16)

ggsave("output/charts/3_beta_capm.png", chart3, width = 10, height = 5.6, dpi = 200)

# ---- 7. Chart 4: Fama-French alpha with 95% confidence intervals ----
a <- subset(robust, Model == "2 Fama-French" & Term == "(Intercept)")
a$Stock <- factor(a$Stock, levels = stocks)

chart4 <- ggplot(a, aes(Stock, Estimate, fill = Stock)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = Estimate - 1.96 * SE_NW, ymax = Estimate + 1.96 * SE_NW),
                width = 0.2) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = colours, guide = "none") +
  labs(title = "Alpha after Fama-French controls, with 95% confidence intervals",
       subtitle = "Extra monthly return not explained by market, size or value. Newey-West SEs",
       x = NULL, y = "Alpha (% per month)", caption = source_note) +
  theme_minimal(base_size = 16)

ggsave("output/charts/4_alpha_fama_french.png", chart4, width = 10, height = 5.6, dpi = 200)

# ---- 8. Chart 5: residuals vs fitted (Fama-French) ----
res <- data.frame()
for (s in stocks) {
  m <- lm(ExRet ~ MKT + SMB + HML, data = subset(panel, stock == s))
  res <- rbind(res, data.frame(Stock = s, Fitted = fitted(m), Residual = residuals(m)))
}
res$Stock <- factor(res$Stock, levels = stocks)

chart5 <- ggplot(res, aes(Fitted, Residual)) +
  geom_hline(yintercept = 0, colour = "grey40") +
  geom_point(alpha = 0.5) +
  facet_wrap(~ Stock, scales = "free") +
  labs(title = "Residuals vs fitted values (Fama-French model)",
       subtitle = "A random cloud is good; a funnel shape signals heteroskedasticity",
       x = "Fitted excess return (%)", y = "Residual (%)", caption = source_note) +
  theme_minimal(base_size = 16)

ggsave("output/charts/5_residuals_vs_fitted.png", chart5, width = 10, height = 6.5, dpi = 200)

cat("DONE! Charts saved in output/charts\n")