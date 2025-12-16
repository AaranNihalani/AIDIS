# AIDIS Loan Analysis

Analyze household loan data from AIDIS (All India Debt & Investment Survey, Visit 1) to understand lender types (formal vs informal), loan purposes, microfinance participation, and urban–rural differences. The core workflow cleans and merges the provided Stata `.dta` files, computes weighted distributions, runs t‑tests, and produces publication‑ready visualizations.

## Project Structure

- `AIDIS.r` — end‑to‑end R script for loading, cleaning, analysis, and plotting
- `Visit1  Level - 02 (Block 3) - Demographic and other particulars of household members.dta` — individual/household roster
- `Visit1  Level - 03 (Block 4) - Household characteristics.dta` — household attributes (part 1)
- `Visit1  Level - 04 (Block 4) - Household characteristics.dta` — household attributes (part 2)
- `Visit1  Level - 14 (Block 12) - particulars of cash loans ... .dta` — loan details and transactions
- `.gitignore` — standard ignore rules

## Requirements

- R (tested with ≥ 4.2 recommended)
- R packages:
  - `haven`, `dplyr`, `tidyr`, `ggplot2`, `labelled`, `scales`, `broom`, `modelsummary`, `sandwich`, `gt`, `fixest`, `ggrepel`, `margins`, `patchwork`

Install packages:

```r
install.packages(c(
  "haven","dplyr","tidyr","ggplot2","labelled","scales","broom",
  "modelsummary","sandwich","gt","fixest","ggrepel","margins","patchwork"
))
```

## Data Setup

- Place the four `.dta` files in the project root (same directory as `AIDIS.r`).
- Filenames must match exactly as referenced in the script.

## How to Run

Interactive (recommended with plots):

```r
# From R or RStudio with working directory set to the project root
source("AIDIS.r")
```

Non‑interactive:

```sh
Rscript AIDIS.r
```

Note: When running non‑interactively, plots render to the default device; for saving, see “Saving Outputs”.

## Analysis Overview

- Load roster and create individual ID (`PID`) using `HHID` + `b3q1` (`AIDIS.r:31–38`).
- Merge household characteristics (`AIDIS.r:56–65`) and loan details.
- Clean and recode variables, add readable factor labels (`AIDIS.r:86–128`).
- Group lenders into broader categories (`AIDIS.r:131–138`).
- Filter total rows and build unique `loan_id` (`AIDIS.r:144–148`).
- Microfinance indicator from lender type (`AIDIS.r:151–153`).
- Formal vs informal lender classification (`AIDIS.r:202–210`, refined at `AIDIS.r:329–338`).
- Weighted distributions:
  - Loan purpose (`AIDIS.r:214–221`, `AIDIS.r:381–391`)
  - Lender type by sector (`AIDIS.r:225–244`, `AIDIS.r:368–376`)
- Visualizations:
  - Lender type distribution (`AIDIS.r:248–269`)
  - Loan purpose distribution (`AIDIS.r:274–289`)
  - Urban vs rural lender and purpose panels (`AIDIS.r:395–474`)
- Statistical tests (t‑tests) contrasting formal vs informal loans (`AIDIS.r:294–301`).

## Outputs

- Console:
  - T‑test results for land ownership, household size, and monthly consumer expenditure.
- Plots:
  - Loan distribution by lender type (formal/informal).
  - Loan distribution by purpose.
  - Urban vs rural comparisons for lender categories and loan purposes.

## Saving Outputs

To save any plot, wrap plotting code with `ggsave()` after the plot call. Example:

```r
p <- ggplot(... ) + geom_bar(...) + labs(...)
ggsave("outputs/loan_distribution_formal.png", p, width = 8, height = 6, dpi = 300)
```

Create an `outputs/` folder if needed and pick meaningful filenames per figure.

## Citation

Source: Author’s calculations from AIDIS (2019), All India Debt & Investment Survey, Visit 1.

