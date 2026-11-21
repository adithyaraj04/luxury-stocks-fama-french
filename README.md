# Luxury Stocks: CAPM, Fama-French and Crisis Regressions

Do luxury stocks earn extra return beyond what the market explains, and did they behave differently in crises? This project studies four European luxury companies: Hermès, LVMH, Kering and Burberry. It uses monthly data from January 2006 to December 2025 (240 months per stock), all in US dollars.

**Author:** Adithya Raj

## Research questions
1. How sensitive is each stock to the market (its **beta**), and is beta different from 1?
2. Does a stock earn **alpha** (extra return) after controlling for size and value factors?
3. Did the stock's behaviour change during the **2008-09 financial crisis** and the **2020 Covid crash**?

## Data
- **Stock prices and exchange rates:** Yahoo Finance (via the `quantmod` package), adjusted monthly prices. Burberry (GBP) and the other three (EUR) are converted to USD.
- **Factors:** Fama/French European 3 Factors (monthly), from Kenneth French's Data Library (via the `frenchdata` package): market excess return (MKT), size (SMB), value (HML) and the risk-free rate (RF).
- **Crisis dummy:** 1 for Sep 2008 - Mar 2009 and Feb - Apr 2020, otherwise 0.

## Methods
Three regressions are run for each stock, where the dependent variable is the stock's monthly excess return (return minus risk-free rate):

| Model | Equation idea | What it tells us |
|---|---|---|
| 1. CAPM | ExRet ~ MKT | Beta (market risk) and alpha |
| 2. Fama-French 3-factor | ExRet ~ MKT + SMB + HML | Alpha after controlling for size and value |
| 3. Crisis model | ExRet ~ MKT (centred) + Crisis + MKT x Crisis | Whether returns and beta shift in crises |

**Hypothesis tests:** (H1) beta = 1 in CAPM; (H3) the crisis terms are jointly zero.

**Diagnostics and robustness:** VIF (multicollinearity), Breusch-Pagan (heteroskedasticity), Durbin-Watson and Breusch-Godfrey (autocorrelation), Jarque-Bera (normality), Newey-West robust standard errors, and a Chow test for a structural break at February 2020.

## Project structure
| File | Purpose |
|---|---|
| `01_download_data.R` | Downloads prices, exchange rates and Fama-French factors |
| `02_build_returns.R` | Converts to USD, builds returns, excess returns and the crisis dummy |
| `03_run_models.R` | Runs the three models and the hypothesis tests |
| `04_diagnostics.R` | Diagnostic tests, Newey-West standard errors, Chow test |
| `05_charts_table.R` | Summary statistics, slide-ready tables and 5 charts |
| `output/` | Result tables (CSV) and `charts/` (PNG) |

The `data/` folder is not included. It is created when you run script 01.

## How to run
1. Open R or RStudio and set the working directory to this project folder.
2. Install the packages once:

```r
install.packages(c("quantmod", "frenchdata", "car", "lmtest",
                   "sandwich", "tseries", "strucchange", "ggplot2"))
```

3. Run the scripts in order, from `01_download_data.R` to `05_charts_table.R`.

## Charts
![Growth of $100](output/charts/1_growth_of_100.png)
![CAPM betas](output/charts/3_beta_capm.png)
![Fama-French alpha](output/charts/4_alpha_fama_french.png)

## Sources
- Yahoo Finance
- Kenneth R. French Data Library
