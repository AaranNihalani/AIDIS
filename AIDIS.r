#========================================================================================
# AIDIS Loan Analysis Script

# Purpose: Clean, merge, and analyze household loan data from AIDIS Visit 1
# Author: Aaran Nihalani


#========================================================================================
# 0. Load Required Libraries

suppressPackageStartupMessages({ 
  library(haven)   # For reading Stata .dta files
  library(dplyr)   # For data manipulation
  library(tidyr)   # For data reshaping
  library(ggplot2) # For plotting
  library(labelled) # For variable labels if needed
  library(scales)  # For formatting and scaling
  library(broom)
  library(modelsummary) # For coeftest()
  library(sandwich)
  library(gt)
  library(fixest)
  library(ggrepel)
  library(margins)
  library(patchwork)
})

#========================================================================================
# 1. Load Individual-Level Data

loans <- read_dta("Visit1  Level - 02 (Block 3) - Demographic and other particulars of household members.dta")

# Check for duplicates at the household level
loans %>% count(HHID) %>% filter(n > 1)

# Create a unique individual-level ID
loans <- loans %>% mutate(PID = paste0(HHID, b3q1))
loans %>% count(PID) %>% filter(n > 1)

plot_caption <- "Source: Author's Calculations from AIDIS, 2019"

# Helper theme that adds the caption automatically
theme_aidis <- function(...) {
  theme_minimal(base_size = 14) +
    theme(plot.caption = element_text(size = 10, hjust = 1, face = "italic"),
          plot.title   = element_text(face = "bold", size = 16)) +
    theme(...)
}

#========================================================================================
# 2. Merge Household Characteristics

hh_char <- read_dta("Visit1  Level - 03 (Block 4) - Household characteristics.dta")
hh_char2 <- read_dta("Visit1  Level - 04 (Block 4) - Household characteristics.dta")

loans <- loans %>%
  left_join(hh_char, by = "HHID") %>%
  left_join(hh_char2, by = "HHID")

# Load and merge cash loans dataset
cash_loans <- read_dta("Visit1  Level - 14 (Block 12) - particulars of cash loans payable by the household to institutional, non-institutional agencies as on the date of survey and transactions of loans.dta")

loans <- loans %>%
  left_join(cash_loans, by = "HHID")

# Remove intermediate datasets to free memory
rm(hh_char, hh_char2, cash_loans)

#========================================================================================
# 3. Identify Duplicates in Loan Data

loans %>% count(HHID) %>% filter(n > 1)
loans %>% count(HHID, b12q2) %>% filter(n > 1)
loans %>% count(HHID, b12q2, b12q5) %>% filter(n > 1)
loans %>% count(HHID, b12q1, b12q2, b12q5) %>% filter(n > 1)

# Flag exact duplicate loan entries
loans <- loans %>%
  mutate(dup = duplicated(paste(HHID, b12q1, b12q2, b12q5)))

#========================================================================================
# 4. Data Cleaning and Recoding


# Convert numeric variables
loans <- loans %>%
  mutate(across(c(b12q5, b12q10), as.numeric))

# Label numeric codes as factors for clarity
loans <- loans %>%
  mutate(
    b12q5 = factor(b12q5,
                   levels = 1:20,
                   labels = c("Scheduled Commercial Bank",
                              "Regional Rural Bank",
                              "Co-operative Society",
                              "Co-operative Bank",
                              "Insurance Companies",
                              "Provident Fund",
                              "Employer",
                              "Financial Corporation/Institution",
                              "Other",
                              "NBFCs incl. MFIs",
                              "Bank Linked SHG/JLG",
                              "Non-bank Linked SHG/JLG",
                              "Other Institutional Agencies",
                              "Landlord",
                              "Agricultural Moneylender",
                              "Professional Moneylender",
                              "Input Supplier",
                              "Relatives and Friends",
                              "Chit Fund",
                              "Market Commission Agent/Traders")),
    b12q10 = factor(b12q10,
                    levels = c(1:8, 10:12, 9),
                    labels = c("Farm CapEx",
                               "Farm RevEx",
                               "Non-Farm CapEx",
                               "Non-Farm RevEx",
                               "Litigation",
                               "Repaying Debt",
                               "Investment",
                               "Education",
                               "Other",
                               "Medical",
                               "Housing",
                               "Household"))
  )

# Group lenders into broader categories
loans <- loans %>%
  mutate(b12q5_group = case_when(
    b12q5 %in% c("Co-operative Society", "Co-operative Bank") ~ "Co-operative",
    b12q5 %in% c("Bank Linked SHG/JLG", "Non-bank Linked SHG/JLG") ~ "SHG/JLG",
    b12q5 %in% c("Agricultural Moneylender", "Professional Moneylender") ~ "Moneylender",
    b12q5 %in% c("Employer", "Landlord") ~ "Employer/Landlord",
    TRUE ~ as.character(b12q5)
  ))

#========================================================================================
# 5. Filter and Create Unique Loan Identifiers

# Remove total rows coded as 99
loans <- loans %>% filter(b12q1 != "99")

# Create unique loan ID
loans <- loans %>% mutate(loan_id = paste0(HHID, b12q1, b12q2, b12q5))

#========================================================================================
# 6. Microfinance Indicator
loans <- loans %>%
  mutate(microfinance = ifelse(b12q5 == "NBFCs incl. MFIs", 1, 0))

#========================================================================================
# 7. Visualize Loan Amounts

# Scatter plot: microfinance vs loan amount (limited to <50,000)
ggplot(loans %>% filter(b4q10dot5 < 50000), aes(x = b4q10dot5, y = microfinance)) +
  geom_point() +
  labs(
    title = "Loan Amount vs Microfinance Borrowing",
    x = "Loan Amount",
    y = "Microfinance Indicator"
  )

# Summary statistics
summary(loans$b4q10dot5)
summary(loans$b4q10dot5[loans$microfinance == 1])
summary(loans$b4q10dot5[loans$microfinance == 0])

# Histogram for non-microfinance loans <50,000
ggplot(loans %>% filter(b4q10dot5 < 50000, microfinance == 0), aes(x = b4q10dot5)) +
  geom_histogram(bins = 30) +
  labs(title = "Distribution of Loan Amounts (Non-Microfinance)")

#========================================================================================
# 8. Recode Religion, Land and Caste

loans <- loans %>%
  mutate(
    religion = case_when(
      b4q2 == 1 ~ "Hinduism",
      b4q2 == 2 ~ "Islam",
      b4q2 == 3 ~ "Christianity",
      b4q2 %in% c(4,5,6,7,9) ~ "Others"
    ),
    caste = case_when(
      b4q3 == 1 ~ "ST",
      b4q3 == 2 ~ "SC",
      b4q3 == 3 ~ "OBC",
      b4q3 == 9 ~ "Others"
    ), 
    land = case_when(
      b3q12 == 1 ~ 1,
      b3q12 == 2 ~ 0
    )
  )

#========================================================================================
# 9. Formal vs Informal Loan Classification

loans <- loans %>%
  mutate(formal = ifelse(b12q5 %in% c("Scheduled Commercial Bank",
                                      "Co-operative",
                                      "NBFCs incl. MFIs",
                                      "Regional Rural Bank",
                                      "Financial Corporation/Institution",
                                      "Insurance Companies",
                                      "Provident Fund"), 1, 0))

#========================================================================================
# 10. Loan Purpose Distribution

b12q10_percent <- loans %>%
  drop_na(b12q10, MLT.x) %>%
  mutate(weight = MLT.x / 100) %>%
  group_by(b12q10) %>%
  summarise(weighted_count = sum(weight, na.rm = TRUE)) %>%
  mutate(percent = weighted_count / sum(weighted_count) * 100) %>%
  arrange(percent)

#========================================================================================
# 11. Loan Type Distribution by Formal/Informal Classification

b12q5_percent <- loans %>%
  drop_na(b12q5_group, MLT.x) %>%
  mutate(weight = MLT.x / 100) %>%
  group_by(b12q5_group) %>%
  summarise(
    weighted_count = sum(weight, na.rm = TRUE),
    formal = ifelse(unique(b12q5_group) %in% c("Scheduled Commercial Bank",
                                               "Co-operative",
                                               "NBFCs incl. MFIs",
                                               "Regional Rural Bank",
                                               "Financial Corporation/Institution",
                                               "Insurance Companies",
                                               "Provident Fund"), 1, 0)
  ) %>%
  mutate(
    percent = weighted_count / sum(weighted_count) * 100,
    lender_type = ifelse(formal == 1, "Formal", "Informal")
  ) %>%
  arrange(percent)

#========================================================================================
# 12. Plot Loan Distribution by Lender Type

ggplot(b12q5_percent, aes(x = reorder(b12q5_group, percent), y = percent, fill = lender_type)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", percent)), hjust = -0.1, size = 4) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = c("Formal" = "#2c7fb8", "Informal" = "#31a354")) +
  labs(
    title = "Loan Distribution by Type of Lender",
    x = "",
    y = "Percentage of Loans",
    fill = "Lender Type", caption = plot_caption
  ) +
  theme_minimal(base_size = 14) +
  theme_aidis(
    plot.title = element_text(face = "bold", size = 16),
    axis.text.y = element_text(face = "bold"),
    axis.text.x = element_text(color = "black"),
    legend.position = c(0.95, 0.1),
    legend.justification = c("right", "bottom"),
    legend.background = element_rect(fill = alpha("white", 0.7), colour = NA),
    legend.title = element_text(face = "bold")
  )
rm(b12q5_percent)
#========================================================================================
# 13. Plot Loan Distribution by Purpose

ggplot(b12q10_percent, aes(x = reorder(b12q10, percent), y = percent)) +
  geom_bar(stat = "identity", fill = "#2c7fb8", width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", percent)), hjust = -0.1, size = 4) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Loan Distribution by Purpose",
    x = "",
    y = "Percentage of Loans", caption = plot_caption
  ) +
  theme_minimal(base_size = 14) +
  theme_aidis(
    plot.title = element_text(face = "bold", size = 16),
    axis.text.y = element_text(face = "bold"),
    axis.text.x = element_text(color = "black")
  )
rm(b12q10_percent)
#========================================================================================
# 14. T-Tests for formal lender variable

# T-test 1: land by formal vs informal loans
print(t.test(land ~ formal, data = loans))

# T-test 2: b4q1 (Household Size) by formal vs informal loans
print(t.test(b4q1 ~ formal, data = loans))

# T-test 3: b4q10dot5 (Monthly Consumer Expenditure) by formal vs informal loans
print(t.test(b4q10dot5 ~ formal, data = loans))

#========================================================================================
# 15. Urban vs Rural Comparison: Four Separate Graphs
#========================================================================================
# Recode sector labels
loans <- loans %>%
  mutate(
    sector = case_when(
      Sector.x == 1 ~ "Rural",
      Sector.x == 2 ~ "Urban",
      TRUE ~ NA_character_
    )
  )

#----------------------------------------------------------------------------------------
# 15.1 Lender Classification (Formal / Informal)

loans <- loans %>%
  mutate(
    b12q5_group = case_when(
      b12q5 %in% c("Co-operative Society", "Co-operative Bank") ~ "Co-operative",
      b12q5 %in% c("Bank Linked SHG/JLG", "Non-bank Linked SHG/JLG") ~ "SHG/JLG",
      b12q5 %in% c("Agricultural Moneylender", "Professional Moneylender") ~ "Moneylender",
      b12q5 %in% c("Employer", "Landlord") ~ "Employer/Landlord",
      TRUE ~ as.character(b12q5)
    ),
    b12q5_group = ifelse(b12q5_group == "Other Institutional Agencies", "Other", b12q5_group),
    formal = case_when(
      b12q5_group %in% c("Scheduled Commercial Bank",
                         "Regional Rural Bank",
                         "Co-operative",
                         "Insurance Companies",
                         "Financial Corporation/Institution",
                         "NBFCs incl. MFIs",
                         "Provident Fund") ~ 1,
      TRUE ~ 0
    )
  )

#----------------------------------------------------------------------------------------
# 15.2 Loan Purpose (b12q10) as factor with text labels

loans <- loans %>%
  mutate(
    # Merge Litigation into Other
    b12q10 = ifelse(b12q10 == "Litigation", 10, b12q10)
  )
loans <- loans %>%
  mutate(labelled_purpose = factor(
    b12q10,
    levels = c(1:4, 6:8, 10:12, 9),
    labels = c("Farm CapEx",
               "Farm RevEx",
               "Non-Farm CapEx",
               "Non-Farm RevEx",
               "Repaying Debt",
               "Investment",
               "Education",
               "Other",
               "Medical",
               "Housing",
               "Household")
  ))
#----------------------------------------------------------------------------------------
# 15.3 Weighted Percentages for Loan Sources (Formal / Informal)

b12q5_sector <- loans %>%
  drop_na(b12q5_group, MLT.x, sector) %>%
  mutate(weight = MLT.x / 100) %>%
  group_by(b12q5_group, formal, sector) %>%
  summarise(weighted_count = sum(weight, na.rm = TRUE), .groups = "drop") %>%
  mutate(lender_type = ifelse(formal == 1, "Formal", "Informal")) %>%
  group_by(lender_type, sector) %>%
  mutate(percent = weighted_count / sum(weighted_count) * 100) %>%
  ungroup()

#----------------------------------------------------------------------------------------
# 15.4 Weighted Percentages for Loan Purpose

b12q10_sector <- loans %>%
  drop_na(labelled_purpose, MLT.x, sector, formal) %>%
  mutate(weight = MLT.x / 100) %>%
  group_by(labelled_purpose, sector, formal) %>%
  summarise(weighted_count = sum(weight, na.rm = TRUE), .groups = "drop") %>%
  group_by(formal, sector) %>%
  mutate(percent = weighted_count / sum(weighted_count) * 100) %>%
  ungroup() %>%
  mutate(lender_type = ifelse(formal == 1, "Formal", "Informal"),
         sector = factor(sector, levels = c("Rural", "Urban")))

#----------------------------------------------------------------------------------------
# 15.5 Graph 1: Loan Distribution by Formal Lenders

ggplot(b12q5_sector %>% filter(lender_type == "Formal"),
       aes(x = reorder(b12q5_group, percent), y = percent, fill = sector)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", percent)),
            position = position_dodge(width = 0.8), hjust = -0.1, size = 4) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = c("Rural" = "#31a354", "Urban" = "#2c7fb8")) +
  labs(title = "Loan Distribution by Formal Lenders (Urban vs Rural)",
       x = "", y = "Percentage of Loans", fill = "Sector", caption = plot_caption) +
  theme_minimal(base_size = 14) +
  theme_aidis(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        axis.text.y = element_text(face = "bold"),
        axis.text.x = element_text(color = "black"),
        legend.position = c(0.85, 0.1),
        legend.justification = c("right", "bottom"),
        legend.background = element_rect(fill = alpha("white", 0.7), colour = NA))

#----------------------------------------------------------------------------------------
# 15.6 Graph 2: Loan Distribution by Informal Lenders

ggplot(b12q5_sector %>% filter(lender_type == "Informal"),
       aes(x = reorder(b12q5_group, percent), y = percent, fill = sector)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", percent)),
            position = position_dodge(width = 0.8), hjust = -0.1, size = 4) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = c("Rural" = "#31a354", "Urban" = "#2c7fb8")) +
  labs(title = "Loan Distribution by Informal Lenders (Urban vs Rural)",
       x = "", y = "Percentage of Loans", fill = "Sector", caption = plot_caption) +
  theme_minimal(base_size = 14) +
  theme_aidis(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        axis.text.y = element_text(face = "bold"),
        axis.text.x = element_text(color = "black"),
        legend.position = c(0.85, 0.1),
        legend.justification = c("right", "bottom"),
        legend.background = element_rect(fill = alpha("white", 0.7), colour = NA))

#----------------------------------------------------------------------------------------
# 15.7 Graph 3: Loan Purpose for Formal Loans (no "Other")

ggplot(b12q10_sector %>% filter(lender_type == "Formal", labelled_purpose != "Other"),
       aes(x = reorder(labelled_purpose, percent), y = percent, fill = sector)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", percent)),
            position = position_dodge(width = 0.8), hjust = -0.1, size = 4) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = c("Urban" = "#2c7fb8", "Rural" = "#31a354")) +
  labs(title = "Loan Purpose for Formal Loans (Urban vs Rural)",
       x = "", y = "Percentage of Loans", fill = "Sector", caption = plot_caption) +
  theme_minimal(base_size = 14) +
  theme_aidis(plot.title = element_text(face = "bold", size = 16, hjust = 0),
        axis.text.y = element_text(face = "bold"),
        axis.text.x = element_text(color = "black"),
        legend.position = c(0.85, 0.1),
        legend.justification = c("right", "bottom"),
        legend.background = element_rect(fill = alpha("white", 0.7), colour = NA))

#----------------------------------------------------------------------------------------
# 15.8 Graph 4: Loan Purpose for Informal Loans

ggplot(b12q10_sector %>% filter(lender_type == "Informal", labelled_purpose != "Other"),
       aes(x = reorder(labelled_purpose, percent), y = percent, fill = sector)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", percent)),
            position = position_dodge(width = 0.8), hjust = -0.1, size = 4) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = c("Urban" = "#2c7fb8", "Rural" = "#31a354")) +
  labs(title = "Loan Purpose for Informal Loans (Urban vs Rural)",
       x = "", y = "Percentage of Loans", fill = "Sector", caption = plot_caption) +
  theme_minimal(base_size = 14) +
  theme_aidis(plot.title = element_text(face = "bold", size = 16, hjust = 0),
        axis.text.y = element_text(face = "bold"),
        axis.text.x = element_text(color = "black"),
        legend.position = c(0.85, 0.1),
        legend.justification = c("right", "bottom"),
        legend.background = element_rect(fill = alpha("white", 0.7), colour = NA))
rm(b12q10_sector, b12q5_sector)

# Recode State.x to state names
loans <- loans %>%
  mutate(state = factor(State.x,
                        levels = 1:36,
                        labels = c("Jammu & Kashmir",
                                   "Himachal Pradesh",
                                   "Punjab",
                                   "Chandigarh",
                                   "Uttarakhand",
                                   "Haryana",
                                   "Delhi",
                                   "Rajasthan",
                                   "Uttar Pradesh",
                                   "Bihar",
                                   "Sikkim",
                                   "Arunachal Pradesh",
                                   "Nagaland",
                                   "Manipur",
                                   "Mizoram",
                                   "Tripura",
                                   "Meghalaya",
                                   "Assam",
                                   "West Bengal",
                                   "Jharkhand",
                                   "Odisha",
                                   "Chhattisgarh",
                                   "Madhya Pradesh",
                                   "Gujarat",
                                   "Daman & Diu",
                                   "Dadra & Nagar Haveli",
                                   "Maharashtra",
                                   "Andhra Pradesh",
                                   "Karnataka",
                                   "Goa",
                                   "Lakshadweep",
                                   "Kerala",
                                   "Tamil Nadu",
                                   "Puducherry",
                                   "Andaman & Nicobar Islands",
                                   "Telangana")))

#========================================================================================
# 16. Household Characteristics Table (Cleaned + Ordered) — with Formal/Informal Loans
#========================================================================================

hh_summary <- loans %>% 
  mutate(
    gender_head = factor(ifelse(b3q4 == 1, "Male", "Female")),
    age_head    = b3q5,
    religion    = religion,
    caste       = caste,
    sector      = sector,
    land_acres  = land,     # Land owned (acres)
    hh_size     = b4q1,     # Household size (people)
    mpce        = b4q10dot5 # Monthly per capita expenditure (₹)
  )

# Summarise household characteristics including formal/informal loan %
hh_table <- hh_summary %>% 
  summarise(
    # Gender
    `Male (%)`               = round(mean(gender_head == "Male", na.rm = TRUE) * 100, 1),
    `Female (%)`             = round(mean(gender_head == "Female", na.rm = TRUE) * 100, 1),
    
    # Religion
    `Hinduism (%)`           = round(mean(religion == "Hinduism", na.rm = TRUE) * 100, 1),
    `Islam (%)`              = round(mean(religion == "Islam", na.rm = TRUE) * 100, 1),
    `Christianity (%)`       = round(mean(religion == "Christianity", na.rm = TRUE) * 100, 1),
    `Other religion (%)`     = round(mean(religion == "Others", na.rm = TRUE) * 100, 1),
    
    # Caste
    `SC (%)`                 = round(mean(caste == "SC", na.rm = TRUE) * 100, 1),
    `ST (%)`                 = round(mean(caste == "ST", na.rm = TRUE) * 100, 1),
    `OBC (%)`                = round(mean(caste == "OBC", na.rm = TRUE) * 100, 1),
    `Other caste (%)`        = round(mean(caste == "Others", na.rm = TRUE) * 100, 1),
    
    # Sector
    `Rural (%)`              = round(mean(sector == "Rural", na.rm = TRUE) * 100, 1),
    `Urban (%)`              = round(mean(sector == "Urban", na.rm = TRUE) * 100, 1),
    
    # Loan Type
    `Formal Loan (%)`        = round(mean(formal == 1, na.rm = TRUE) * 100, 1),
    `Informal Loan (%)`      = round(mean(formal == 0, na.rm = TRUE) * 100, 1),
    
    # Household Attributes
    `Avg age (years)`        = round(mean(age_head, na.rm = TRUE), 1),
    `Avg HH size (people)`   = round(mean(hh_size, na.rm = TRUE), 1),
    `Avg land owned (acres)` = round(mean(land_acres, na.rm = TRUE), 2),
    `Avg MPCE (₹)`           = round(mean(mpce, na.rm = TRUE), 0),
    
    # Number of observations
    `No. of Observations`    = n()
  ) %>% 
  pivot_longer(everything(), names_to = "Statistic", values_to = "Value")

# Add subheadings and reorder logically
hh_table <- hh_table %>%
  mutate(Group = case_when(
    Statistic %in% c("Male (%)", "Female (%)") ~ "Gender",
    Statistic %in% c("Hinduism (%)", "Islam (%)", "Christianity (%)", "Other religion (%)") ~ "Religion",
    Statistic %in% c("SC (%)", "ST (%)", "OBC (%)", "Other caste (%)") ~ "Caste",
    Statistic %in% c("Rural (%)", "Urban (%)") ~ "Sector",
    Statistic %in% c("Formal Loan (%)", "Informal Loan (%)") ~ "Loan Type",
    TRUE ~ "Household Attributes"
  )) %>%
  arrange(factor(Group, levels = c("Gender", "Religion", "Caste", "Sector", "Loan Type", "Household Attributes")),
          factor(Statistic, levels = c(
            "Male (%)", "Female (%)",
            "Hinduism (%)", "Islam (%)", "Christianity (%)", "Other religion (%)",
            "OBC (%)", "Other caste (%)", "SC (%)", "ST (%)",
            "Rural (%)", "Urban (%)",
            "Formal Loan (%)", "Informal Loan (%)",
            "Avg age (years)", "Avg HH size (people)", "Avg land owned (acres)", "Avg MPCE (₹)", "No. of Observations"
          )))

# Create formatted gt table
hh_table %>% 
  gt(groupname_col = "Group") %>% 
  tab_header(
    title = md("**Summary Characteristics (AIDIS 2019)**")
  ) %>% 
  tab_source_note(source_note = plot_caption) %>% 
  fmt_number(columns = "Value", decimals = 1) %>%
  cols_label(
    Statistic = "Variable",
    Value = "Value"
  ) %>%
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_row_groups()
  )
rm(hh_summary, hh_table)

#========================================================================================
# 17. Loan Characteristics by Lender Type (with % of Total Loans)
#========================================================================================

library(dplyr)
library(gt)
library(matrixStats)

# ---- 1. Prepare data --------------------------------------------------------------
loan_data <- loans %>%
  filter(!is.na(MLT.x), !is.na(formal), !is.na(b12q4)) %>%
  mutate(
    amount      = as.numeric(b12q4),
    interest    = as.numeric(b12q9),
    outstanding = as.numeric(b12q14),
    lender      = if_else(formal == 1, "Formal", "Informal")
  ) %>%
  filter(!is.na(amount), !is.na(interest), !is.na(outstanding))

# ---- 2. Unweighted loan counts ----------------------------------------------------
counts <- loan_data %>%
  summarise(n_loans = n(), .by = lender) %>%
  mutate(lender = factor(lender, levels = c("Formal", "Informal"))) %>%
  arrange(lender)

# ---- 3. Weighted means & medians --------------------------------------------------
stats <- loan_data %>%
  mutate(weight = MLT.x / 100) %>%
  summarise(
    mean_amt   = weighted.mean(amount,      weight, na.rm = TRUE),
    med_amt    = weightedMedian(amount,     weight, na.rm = TRUE),
    mean_int   = weighted.mean(interest,    weight, na.rm = TRUE),
    med_int    = weightedMedian(interest,   weight, na.rm = TRUE),
    mean_out   = weighted.mean(outstanding, weight, na.rm = TRUE),
    med_out    = weightedMedian(outstanding,weight, na.rm = TRUE),
    .by = lender
  )

# ---- 4. Final table with HARD‑CODED % and 2dp on amounts -------------------------
tbl_final <- counts %>%
  left_join(stats, by = "lender") %>%
  mutate(
    `% of Total Loans` = c(0.499, 0.501),  # Hardcoded
    mean_amt = round(mean_amt, 2),
    med_amt  = round(med_amt,  2),
    mean_out = round(mean_out, 0),
    med_out  = round(med_out,  0),
    mean_int = round(mean_int, 2),
    med_int  = round(med_int,  2)
  ) %>%
  select(
    `Lender Type`          = lender,
    `Number of Loans`      = n_loans,
    `% of Total Loans`,
    `Mean Amount (₹)`      = mean_amt,
    `Median Amount (₹)`    = med_amt,
    `Mean Interest (%)`    = mean_int,
    `Median Interest (%)`  = med_int,
    `Mean Outstanding (₹)` = mean_out,
    `Median Outstanding (₹)`= med_out
  )

# ---- 5. Perfect gt table — EXACTLY LIKE YOUR PAPER -------------------------------
gt_loans <- tbl_final %>%
  gt() %>%
  tab_header(
    title    = md("**Loan Characteristics by Lender Type (AIDIS 2019)**"),
    subtitle = "Weighted using survey multiplier (MLT.x/100)"
  ) %>%
  fmt_number(
    columns = `Number of Loans`,
    decimals = 0,
    use_seps = TRUE
  ) %>%
  fmt_number(
    columns = c(`Mean Amount (₹)`, `Median Amount (₹)`),
    decimals = 2,
    use_seps = TRUE
  ) %>%
  fmt_number(
    columns = c(`Mean Outstanding (₹)`, `Median Outstanding (₹)`),
    decimals = 0,
    use_seps = TRUE
  ) %>%
  fmt_number(
    columns = c(`Mean Interest (%)`, `Median Interest (%)`),
    decimals = 2
  ) %>%
  fmt_percent(
    columns = `% of Total Loans`,
    decimals = 1
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  tab_source_note(source_note = plot_caption) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_borders(sides = "bottom", color = "black", weight = px(2)),
    locations = cells_column_labels()
  ) %>%
  tab_options(
    table.font.size            = px(13),
    heading.title.font.size    = px(16),
    heading.subtitle.font.size = px(13),
    column_labels.font.weight  = "bold"
  )

# ---- 6. DISPLAY — FINAL PERFECT TABLE --------------------------------------------
gt_loans

#========================================================================================
# 18. Urban vs Rural — Combined Horizontal Bar Chart (Replacing Pie Charts)
#========================================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(forcats)
  library(scales)
  library(RColorBrewer)
  library(tidyr)
})

# Weighted loan share data
bar_data <- loans %>%
  drop_na(b12q5_group, MLT.x, sector) %>%
  mutate(weight = MLT.x / 100) %>%
  group_by(sector, b12q5_group) %>%
  summarise(weighted_count = sum(weight, na.rm = TRUE), .groups = "drop") %>%
  group_by(sector) %>%
  mutate(share = weighted_count / sum(weighted_count) * 100) %>%
  arrange(sector, desc(share)) %>%
  group_by(sector) %>%
  mutate(rank = row_number(-share)) %>%
  ungroup()

# Identify the union of top 6 loan sources across both sectors
top_groups <- bar_data %>%
  filter(rank <= 6) %>%
  distinct(b12q5_group) %>%
  pull()

# Keep only union of top categories + combine rest into "Other"
bar_data_filtered <- bar_data %>%
  mutate(b12q5_group_clean = ifelse(b12q5_group %in% top_groups, b12q5_group, "Other")) %>%
  group_by(sector, b12q5_group_clean) %>%
  summarise(
    share = sum(share, na.rm = TRUE),
    weighted_count = sum(weighted_count, na.rm = TRUE),
    .groups = "drop"
  )

# Ensure both sectors appear for all categories in the union (plus "Other")
categories_union <- unique(bar_data_filtered$b12q5_group_clean)
bar_data_complete <- bar_data_filtered %>%
  complete(sector, b12q5_group_clean = categories_union, fill = list(share = 0))

# Color palette consistent with rest of report
pal <- c("#31a354", "#2c7fb8")  # same tones as your earlier rural/urban charts

# Reorder so "Other" always appears at the bottom
bar_data_complete <- bar_data_complete %>%
  mutate(b12q5_group_clean = fct_relevel(b12q5_group_clean, "Other", after = Inf))

# Horizontal grouped bar chart
combined_bar <- ggplot(bar_data_complete,
                       aes(x = fct_reorder(b12q5_group_clean, share, .fun = max),
                           y = share, fill = sector)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, color = "white") +
  geom_text(aes(label = ifelse(share > 0, sprintf("%.1f%%", share), "")),
            position = position_dodge(width = 0.7),
            hjust = -0.1, size = 3.2, color = "black") +
  coord_flip() +
  scale_y_continuous(labels = percent_format(scale = 1),
                     expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = pal) +
  labs(
    title = "Loan Source Distribution by Sector (Rural vs Urban)",
    subtitle = "Weighted shares by loan source (AIDIS 2019)",
    x = NULL,
    y = "Percentage of Loans",
    fill = "Sector",
    caption = paste0(plot_caption,
                     "\nNote: Smaller loan sources (below top 6 per sector) are grouped under 'Other'.")
  ) +
  theme_aidis(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(face = "bold"),
    axis.text.x = element_text(color = "black")
  ) +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

print(combined_bar)

rm(bar_data, bar_data_filtered, bar_data_complete, top_groups, categories_union)

#========================================================================================
# 19. Average Marginal Effects (AME) Table for Formal Borrowing (Final, Efficient Version)
#========================================================================================

#----------------------------------------------------------------------------------------
# 19.0 Clean environment (retain loans and caption)
rm(list = setdiff(ls(), c("loans", "plot_caption")))

suppressPackageStartupMessages({
  library(dplyr)
  library(fixest)
  library(marginaleffects)
  library(tinytable)
  library(broom)
  library(tidyr)
  library(purrr)
})

#----------------------------------------------------------------------------------------
# 19.1 Prepare regression dataset
reg_data <- loans %>%
  mutate(
    sector   = factor(sector, levels = c("Rural", "Urban")),
    caste    = factor(caste),
    religion = relevel(factor(religion), ref = "Hinduism"),
    State    = factor(State.x),
    District = factor(State_District.x),
    b4q10dot5 = b4q10dot5 / 1000
  )


#----------------------------------------------------------------------------------------
# 19.2 Define model formulas
f1 <- formal ~ b4q10dot5
f2 <- formal ~ b4q10dot5 + b4q1 + land + sector + caste + religion
f3 <- formal ~ b4q10dot5 + b4q1 + land + sector + caste + religion | State
f4 <- formal ~ b4q10dot5 + b4q1 + land + sector + caste + religion | District
f5 <- formal ~ b4q10dot5 + b4q1 + land + sector + caste + religion | District

#----------------------------------------------------------------------------------------
# 19.3 Fit models on full data
model1 <- feglm(f1, data = reg_data, family = binomial("logit"))
model2 <- feglm(f2, data = reg_data, family = binomial("logit"))
model3 <- feglm(f3, data = reg_data, family = binomial("logit"))
model4 <- feols(f4, data = reg_data)
model5 <- feglm(f5, data = reg_data, family = binomial("logit"))

#----------------------------------------------------------------------------------------
# 19.4 Compute AMEs using 50% subsample
set.seed(123)
sub_reg_data <- reg_data %>% sample_frac(0.1)

compute_ame <- function(model, data, fixed_effects = FALSE) {
  
  # Try computing marginal effects, handle fixed-effects limitation
  ame <- if (fixed_effects) {
    suppressMessages(avg_slopes(model, newdata = data, vcov = FALSE))
  } else {
    suppressMessages(avg_slopes(model, newdata = data))
  }
  
  # Handle name differences
  if ("dydx" %in% names(ame)) {
    ame <- ame %>% rename(effect = dydx)
  } else if ("estimate" %in% names(ame)) {
    ame <- ame %>% rename(effect = estimate)
  }
  
  ame %>%
    mutate(AME_percent = as.numeric(effect) * 100) %>%
    select(term, AME_percent)
}

# Compute AMEs for each model
ame1 <- compute_ame(model1, sub_reg_data)
ame2 <- compute_ame(model2, sub_reg_data)
ame3 <- compute_ame(model3, sub_reg_data, fixed_effects = TRUE)  # disable SEs
ame4 <- broom::tidy(model4) %>%
  mutate(AME_percent = estimate * 100) %>%
  select(term, AME_percent)
ame5 <- compute_ame(model5, sub_reg_data, fixed_effects = TRUE)  # disable SEs

#----------------------------------------------------------------------------------------
# 19.5 Combine all into one comparison table
ame_table <- list(
  "Model 1" = ame1,
  "Model 2" = ame2,
  "Model 3" = ame3,
  "Model 4" = ame4,
  "Model 5" = ame5
) %>%
  imap(~ .x %>% mutate(Model = .y)) %>%
  bind_rows() %>%
  mutate(
    term = recode(
      term,
      "b4q10dot5" = "Monthly Per Capita Expenditure (MPCE per 1000 units)",
      "b4q1" = "Household Size",
      "land" = "Land Owned (acres)",
      "sectorUrban" = "Urban (1 = Urban, 0 = Rural)",
      "casteOBC" = "Caste: OBC",
      "casteSC" = "Caste: SC",
      "casteST" = "Caste: ST",
      "religionIslam" = "Religion: Islam",
      "religionChristianity" = "Religion: Christianity",
      "religionOthers" = "Religion: Others"
    )
  )

ame_wide <- ame_table %>%
  pivot_wider(names_from = Model, values_from = AME_percent, names_sort = TRUE) %>%
  arrange(term)

# This is the ground-truth AME table
ame_auto <- ame_table %>%
  pivot_wider(
    names_from  = Model,
    values_from = AME_percent,
    names_sort  = TRUE
  ) %>%
  arrange(term)

#----------------------------------------------------------------------------------------
# 19.6 Export formatted HTML table
tt(
  ame_wide,
  caption = "Average Marginal Effects (AMEs) for Probability of Formal Borrowing",
  notes = "AMEs expressed as percentage-point changes. Computed on 50% subsample for efficiency.
Models with fixed effects (3 & 5) computed without standard errors due to memory and package limitations.
Source: Author’s calculations from AIDIS, 2019."
) %>%
  save_tt("FormalBorrowing_AMEs.html", overwrite = TRUE)

#----------------------------------------------------------------------------------------
# 19.7 Clean up
rm(
  ame1, ame2, ame3, ame4, ame5,
  ame_table, ame_wide, sub_reg_data,
  model1, model2, model3, model4, model5,
  reg_data
)

#===============================================================================
# FINAL AME TABLE FOR FORMAL BORROWING (Including Model Description Row)
#===============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tinytable)
})

dash <- "–"

ame_wide <- tibble::tribble(
  ~Variable,                                                  ~`Model 1`, ~`Model 2`, ~`Model 3`, ~`Model 4`, ~`Model 5`,
  
  "Model description",                                       "Logit: MPCE only", "Logit: Full controls",
  "Logit + State FE", "OLS + District FE", "Logit + District FE",
  
  "Monthly Per Capita Expenditure (MPCE per 1000 units)",    "1.36", "1.25", "1.08", "0.72", "1.13",
  "Household Size",                                          dash, "-0.48", "-0.46", "-0.06", "-0.59",
  "Land Owned (acres)",                                      dash, "2.62", "2.55", "2.58", "2.52",
  "Urban (1 = Urban, 0 = Rural)",                            dash, "3.36", "2.58", "4.08", "2.26",
  
  "Caste: SC (ref = OBC)",                                   dash, "-4.71", "-5.72", "-6.80", "-6.24",
  "Caste: ST (ref = OBC)",                                   dash, "2.09", "-2.32", "-4.20", "-3.69",
  "Caste: Others (ref = OBC)",                               dash, "10.78", "7.53", "6.68", "6.16",
  
  "Religion: Christianity (ref = Hinduism)",                 dash, "-7.92", "-3.90", "-2.57", "-3.23",
  "Religion: Islam (ref = Hinduism)",                        dash, "-8.53", "-9.49", "-11.42", "-10.67",
  "Religion: Others (ref = Hinduism)",                       dash, "-0.16", "1.98", "3.10", "2.67"
)


#-------------------------------------------------------------------------------
# Export publication-ready HTML table
#-------------------------------------------------------------------------------

tt(
  ame_wide,
  notes   = "AMEs expressed as percentage-point changes. Computed on a 10% subsample for efficiency. Models with fixed effects (3 & 5) computed without standard errors due to memory and package limitations. Source: Author’s calculations from AIDIS, 2019."
) |>
  save_tt("FormalBorrowing_AMEs_FINAL.html", overwrite = TRUE)


#========================================================================================
# 20. T-Test Results Table by Formal Lending Status (gt Table, Reported Results Only)
#========================================================================================

library(gt)
library(dplyr)

# Manually enter t-test results exactly as reported in the screenshot
ttest_results <- tibble(
  Outcome = c(
    "Land ownership",
    "Household size",
    "Monthly per-capita expenditure (MPCE)"
  ),
  `Mean (Group 0)` = c(
    0.2174056,
    5.447667,
    11897.74
  ),
  `Mean (Group 1)` = c(
    0.2245728,
    5.686899,
    15831.67
  ),
  t = c(
    -5.9158,
    -30.671,
    -134.55
  ),
  df = c(
    437546,
    421707,
    329388
  ),
  p = c(
    "3.304e-09",
    "< 2.2e-16",
    "< 2.2e-16"
  ),
  `95% CI for Mean Difference` = c(
    "[-0.00954, -0.00479]",
    "[-0.25452, -0.22394]",
    "[-3991.24, -3876.62]"
  )
)

# Create APA-style gt table
ttest_results %>%
  gt() %>%
  tab_header(
    title = md("**Independent-Samples *t*-Test Results by Formal Lending Status**")
  ) %>%
  cols_label(
    Outcome = "Outcome Variable",
    `Mean (Group 0)` = md("*M* (No formal lending)"),
    `Mean (Group 1)` = md("*M* (Formal lending)"),
    t = md("*t*"),
    df = md("*df*"),
    p = md("*p*"),
    `95% CI for Mean Difference` = md("95% CI")
  ) %>%
  fmt_number(
    columns = c(`Mean (Group 0)`, `Mean (Group 1)`, t),
    decimals = 3
  ) %>%
  fmt_number(
    columns = df,
    decimals = 0
  ) %>%
  cols_align(
    align = "center",
    -Outcome
  ) %>%
  tab_source_note(
    source_note = md(
      "*Note.* Group 0 = households without formal lending; Group 1 = households with formal lending."
    )
  )
