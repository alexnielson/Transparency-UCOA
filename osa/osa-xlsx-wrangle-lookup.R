# ==============================================================================
# osa-xlsx-wrangle-lookup
# Alexander Nielson
# 2/25/2020
# ==============================================================================

#lib
library(readxl) 
library(openxlsx)
library(tidyverse)
library(stringi) 
library(tidyr)

osa_file_name <- "ucoa.xlsx"

# fund =========================================================================
osa_fund <-
  read_xlsx(osa_file_name, sheet = excel_sheets(osa_file_name)[[1]]) %>%
  mutate(number = number %>% str_pad(3, pad = "0")) %>%
  mutate(
    level_primary =
      ifelse(
        str_detect(full_description, "^(.*)[:space:]-[:space:]"),
        stri_extract_first(full_description, regex = "^(.*)[:space:]-[:space:]"),
        full_description
      ),
    level_secondary = stri_extract_first(full_description, regex = "[:space:]-[:space:](.*)$"),
    level_primary = str_replace(level_primary, "-", "") %>% str_trim() %>% str_to_upper(),
    level_secondary = str_replace(level_secondary, "-", "") %>% str_trim()
  )

# add additional fund numbers.
extra_fund_numbers <-
  c(202:298, 302:398, 402:448, 452:498, 502:598, 602:698, 702:798) %>%
  as.character() %>%
  enframe(name = NULL) %>%
  rename("number" = "value") %>%
  mutate(
    short_description = NA,
    full_description = case_when(
      number %in% (202:298) ~ "Special Revenue Funds (as assigned by local government)",
      number %in% (302:398) ~ "Debt Service Funds (as assigned by local government)",
      number %in% (402:448) ~ "Capital Projects Funds (as assigned by local government)",
      number %in% (452:498) ~ "Permanent Funds (as assigned by local government)",
      number %in% (502:598) ~ "Enterprise Funds (as assigned by local government)",
      number %in% (602:698) ~ "Internal Service Funds (as assigned by local government)",
      number %in% (702:798) ~ "Trust and Agency Funds (as assigned by local government)"
    ),
    detail = NA,
    level_primary = case_when(
      number %in% (202:298) ~ "Special Revenue Funds",
      number %in% (302:398) ~ "Debt Service Funds",
      number %in% (402:448) ~ "Capital Projects Funds ",
      number %in% (452:498) ~ "Permanent Funds",
      number %in% (502:598) ~ "Enterprise Funds",
      number %in% (602:698) ~ "Internal Service Funds",
      number %in% (702:798) ~ "Trust and Agency Funds"
    ),
    level_secondary = paste(level_primary, str_extract(number, "[:digit:][:digit:]$"))
  ) %>%
  mutate(level_primary = str_to_upper(level_primary))


osa_fund <-
  osa_fund %>%
  bind_rows(extra_fund_numbers) %>%
  mutate(code_primary = str_sub(number, 0, 2),
         code_secondary = str_sub(number, 2, 3)) %>%
  select(-short_description, -detail) %>%
  rename(description = full_description)


# funct ========================================================================
osa_funct <-
  read_xlsx(osa_file_name, sheet = excel_sheets(osa_file_name)[[2]])

# quicker than str_pad to the entire column
osa_funct$number[[1]] <- "000000"

osa_funct <- osa_funct %>%
  mutate(
    number = as.character(number),
    code_primary = str_sub(number, 0, 2),
    code_secondary = str_sub(number, 3, 4),
    code_tertiary = str_sub(number, 5, 6),
    temp_description = full_description
  ) %>%
  separate(
    temp_description,
    into = c("level_primary", "level_secondary", "level_tertiary"),
    sep = "[:space:]-[:space:]"
  ) %>%
  select(-short_description, -detail) %>%
  rename(description = full_description)

# account ======================================================================
osa_account <-
  read_xlsx(osa_file_name, sheet = excel_sheets(osa_file_name)[[3]]) %>%
  mutate(
    number = as.character(number),
    code_primary = str_sub(number, 0, 2),
    code_secondary = str_sub(number, 3, 4),
    code_tertiary = str_sub(number, 5, 6),
    code_quaternary = str_sub(number, 7, 8),
    temp_description = full_description,
    level_primary = case_when(
      str_extract(number, "^[:digit:][:digit:]") == "10"  ~ "ASSETS",
      str_extract(number, "^[:digit:][:digit:]") == "11"  ~ "DEFERRED OUTFLOWS OF RESOURCES",
      str_extract(number, "^[:digit:][:digit:]") == "20"  ~ "LIABILITIES",
      str_extract(number, "^[:digit:][:digit:]") == "21"  ~ "DEFERRED INFLOWS OF RESOURCES",
      str_extract(number, "^[:digit:][:digit:]") == "22"  ~ "FUND BALANCE",
      str_extract(number, "^[:digit:][:digit:]") == "23"  ~ "NET POSITION",
      str_extract(number, "^[:digit:][:digit:]") == "30"  ~ "REVENUES",
      str_extract(number, "^[:digit:][:digit:]") == "40"  ~ "EXPENDITURES"
    )
    
  ) %>%
  separate(
    temp_description,
    into = c("level_secondary", "level_tertiary", "level_quaternary"),
    sep = "[:space:]-[:space:]"
  ) %>%
  select(
    number,
    full_description,
    code_primary,
    code_secondary,
    code_tertiary,
    code_quaternary,
    level_primary,
    level_secondary,
    level_tertiary,
    level_quaternary
  ) %>%
  rename(description = full_description)

# consolidate ==================================================================
osa_lookup <-
  list(fund        = osa_fund,
       funct       = osa_funct,
       account     = osa_account)

# export to a csv document =====================================================
osa_names <- names(osa_lookup)

wb <- createWorkbook()

for (name in osa_names) {
  sheet_name <-
    name #str_trunc(name, 30, side = c("right"), ellipsis = "...")
  tempdata <- osa_lookup %>% pluck(name)
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, tempdata, colNames = TRUE)
}

saveWorkbook(wb, file = "osa_lookup.xlsx",  overwrite = TRUE)

# empty garbage ================================================================
rm(
  name,
  osa_file_name,
  osa_names,
  sheet_name,
  wb,
  tempdata,
  osa_lookup,
  osa_fund,
  osa_funct,
  osa_account,
  extra_fund_numbers
)
