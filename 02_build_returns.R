prices <- read.csv("data/prices_monthly.csv")

ff_lines <- readLines("data/Europe_3_Factors.csv")
ff_rows  <- ff_lines[grepl("^\\s*[0-9]{6}\\s*,", ff_lines)]
ff <- read.csv(text = ff_rows, header = FALSE,
               col.names = c("yyyymm", "MKT", "SMB", "HML", "RF"))
ff$ym <- paste0(substr(ff$yyyymm, 1, 4), "-", substr(ff$yyyymm, 5, 6))

stocks <- c(Hermes = "EUR", LVMH = "EUR", Kering = "EUR", Burberry = "GBP")

panel <- data.frame()
for (s in names(stocks)) {
  p  <- subset(prices, series == s)[, c("ym", "price")]
  fx <- subset(prices, series == stocks[[s]])[, c("ym", "price")]
  names(fx)[2] <- "fx"
  p  <- merge(p, fx, by = "ym")
  p  <- p[order(p$ym), ]
  p$usd <- p$price * p$fx
  p$R   <- 100 * (p$usd / c(NA, head(p$usd, -1)) - 1)
  p$stock <- s
  panel <- rbind(panel, p)
}

panel <- merge(panel, ff[, c("ym", "MKT", "SMB", "HML", "RF")], by = "ym")
panel <- subset(panel, !is.na(R) & ym >= "2006-01" & ym <= "2025-12")
panel$ExRet <- panel$R - panel$RF

crisis_months <- c(
  format(seq(as.Date("2008-09-01"), as.Date("2009-03-01"), by = "month"), "%Y-%m"),
  format(seq(as.Date("2020-02-01"), as.Date("2020-04-01"), by = "month"), "%Y-%m"))
panel$Crisis <- ifelse(panel$ym %in% crisis_months, 1, 0)

panel <- panel[order(panel$stock, panel$ym), ]
write.csv(panel, "data/analysis_dataset.csv", row.names = FALSE)

print(table(panel$stock))
cat("Crisis months:", sum(panel$Crisis) / 4, "\n")
print(panel[abs(panel$R) > 60, c("stock", "ym", "R")])
