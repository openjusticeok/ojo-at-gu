library(tidyverse)
library(lubridate)

# Read data set
d <- read_rds("ev-data-sample.rds")

# Function to clean plaintiff names
clean_pl_names <- function(x) {
  str_to_upper({{x}}) |>
    str_remove_all( " INC| CO(?=($| ))|LLC|LP|LIMITED PARTNERSHIP|QLA|[[:punct:]]|THE | PHASE.{1,30}$|COMPANY|C\\/O.{1,30}| (AT|OF)(?= )| AND|II|DBA.*") |>
    str_replace_all("APT([[:alpha:]]|)|APARTMENT([[:alpha:]]|)$|APARTMENT HOMES", "APARTMENTS") |>
    str_replace("MHC|MHP|MOBILE HOME.*", "MOBILE HOMES") |>
    str_remove_all(" [[:alpha:]]$") |>
    str_squish() |>
    str_replace(".*HOUSING AUTH.*", "COUNTY HOUSING AUTHORITY") |>
    str_replace("MENTAL HEALTH AS.*", "MENTAL HEALTH ASSOCIATION") |>
    str_replace("CHAT.*68.*", "CHATEAU 68 APARTMENTS") |>
    str_replace(".*AVONDALE.*", "JA AVONDALE") |>
    str_remove("DBA") |>
    str_remove("(?<=APARTMENTS).*") |>
    str_remove("COMPANIES.*") |>
    str_squish()
}

# Create new variable ('plaint_clean') for cleaned plaintiff name
plaintiff <- d |>
  mutate(plaint_clean = clean_pl_names(plaintiff))

# Function to plot most frequently appearing values of column ('col')
plaint_plot <- function(col, title) {
  ggplot(plaintiff |>
           count({{col}}) |>
           top_n(10),
         aes(x = reorder({{col}}, n), y = n)) +
    geom_col(fill = "black") +
    geom_text(aes(y = n + 150, label = n)) +
    theme_bw() +
    coord_flip() +
    labs(title = title,
         subtitle = "Tulsa County eviction cases, 2016-2022",
         x = NULL,
         y = NULL) +
    ylim(0, 2200)
}

plaint_plot(plaintiff, "Top 10 plaintiff names (raw)")
plaint_plot(plaint_clean, "Top 10 plaintiff names (cleaned)")

# Count distinct values of 'plaintiff' and 'plaint_clean'
pcount <- plaintiff |>
  summarize(pl = n_distinct(plaintiff),
            clean = n_distinct(plaint_clean))

# Identify serial eviction filings
# Calculate days since last filing with same plaintiff and defendant, filter to evictions filed within 70 days of previous
serial <- plaintiff |>
  group_by(plaint_clean, def_id) |>
  arrange(plaint_clean, def_id, date_filed) |>
  select(district, case_number, date_filed, plaint_clean, def_id) |>
  arrange(plaint_clean, def_id) |>
  mutate(days_since_last = as.numeric(date_filed) - as.numeric(lag(date_filed))) |>
  filter(days_since_last < 70) |>
  count(plaint_clean, def_id)

# Group by plaintiff and count number of tenants with 3 or more serial filings
serial_plaintiff <- serial |>
  filter(n >= 3) |>
  ungroup() |>
  count(plaint_clean) |>
  top_n(15)

ggplot(serial_plaintiff, aes(x = reorder(plaint_clean, n), y = n)) +
  geom_col(fill = "black") +
  geom_text(aes(y = n + 6, label = n),
            family = "Menlo") +
  theme_bw() +
  coord_flip() +
  labs(title = "Number of tenants with 3+ serial filings",
       subtitle = "Tulsa County eviction cases, 2016-2022",
       x = NULL,
       y = NULL) +
  ylim(0, 140)

# Extract debt amounts (available only for cases 2019 on)
# This takes several minutes to run
debt <- d |>
  rename(min_desc = description) |>
  # Extract debt amount
  mutate(min = str_remove_all(min_desc, "AMOUNT IN DEBT OF |\\$|,") |>
           str_remove_all("( PER |Document)(.|\n)*") |>
           str_squish()) |>
  # Clean strings to just digits and decimals
  mutate(fees = str_extract_all(min, "(\\d|\\.)+") |>
           as.character() |>
           str_remove_all('c\\("|"\\)')) |>
  # Separate listed fees into columns
  separate(fees,
           into = paste0("fee", 1:4),
           sep = '", "') |>
  rowwise(district, case_number, min_desc) |>
  # Convert all fee columns to numeric and sum
  mutate(across(contains("fee"), as.numeric),
         debt_amt = sum(fee1, fee2, fee3, fee4, na.rm = TRUE),
         late_fee = str_detect(min_desc, "LATE")) |>
  group_by(district, case_number) |>
  summarize(debt_amt = sum(debt_amt),
            late_fee = any(late_fee == TRUE))

ggplot(debt |>
         filter(debt_amt < 5000)) +
  geom_histogram(aes(x = debt_amt))

debt |>
  left_join(d |>
              select(district, case_number, date_filed)) |>
  filter(!is.na(minute),
         date_filed < ymd("2022-09-01")) |>
  count(month = floor_date(date_filed, "month"),
        has_debt = debt_amt > 100) |>
  group_by(month) |>
  mutate(lab = round(n/sum(n) * 100, digits = 0)) |>
  filter(has_debt == TRUE) |>
  ggplot() +
  geom_col(aes(x = month,
               y = lab/100),
           fill = "black") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  labs(title = "Percent of eviction filings with debt > $100",
       subtitle = "Evictions filed 2019-present, Tulsa County",
       x = NULL,
       y = NULL)

defs |>
  group_by(def_id) |>
  count() |>
  view()
