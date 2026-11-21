library(quantmod)
library(frenchdata)
dir.create("data", showWarnings = FALSE)

# ---- Prices ----
series <- c(Hermes = "RMS.PA", LVMH = "MC.PA", Kering = "KER.PA",
            Burberry = "BRBY.L", EUR = "EURUSD=X", GBP = "GBPUSD=X")

all_prices <- data.frame()
for (name in names(series)) {
  x <- getSymbols(series[[name]], src = "yahoo",
                  from = "2005-12-01", to = "2025-12-31", auto.assign = FALSE)
  m <- to.monthly(na.omit(Ad(x)), OHLC = FALSE)
  all_prices <- rbind(all_prices, data.frame(
    ym = format(index(m), "%Y-%m"), price = as.numeric(m), series = name))
  cat("Downloaded", name, "\n")
}
write.csv(all_prices, "data/prices_monthly.csv", row.names = FALSE)

# ---- Fama-French Europe factors (monthly) ----
ff_raw <- download_french_data("Fama/French European 3 Factors")
ff_m   <- ff_raw$subsets$data[[1]]

d <- ff_m$date
yyyymm <- if (inherits(d, "Date")) format(d, "%Y%m") else as.character(d)

out <- data.frame(yyyymm = yyyymm,
                  MKT = ff_m[["Mkt-RF"]],
                  SMB = ff_m$SMB,
                  HML = ff_m$HML,
                  RF  = ff_m$RF)
write.table(out, "data/Europe_3_Factors.csv", sep = ",",
            row.names = FALSE, col.names = FALSE, quote = FALSE)

cat("DONE! Rows of price data:", nrow(all_prices), "\n")
cat("Factor rows:", nrow(out), "\n")
