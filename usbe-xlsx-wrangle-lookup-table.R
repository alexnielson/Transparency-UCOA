# This script will wrangle the USBE finance department's COA excel file into a usable lookup table.

#lib
library(readxl) 
library(openxlsx)
library(tidyverse)
library(stringi) 
library(tidyr)
library(beepr)

#load data
usbe_file_name <- "FY20 COA-USBE.xlsx"

# Fund =========================================================================
usbe_fund <-
  read_xlsx(usbe_file_name, sheet = excel_sheets(usbe_file_name)[[1]]) %>% 
  rename(number = FundCode,
         description = FundDescription,
         can_submit = CanSubmit) %>% 
  mutate(number = as.character(number))

##add back the ranges
extra_fund_numbers <- c(27:29, 61:69) %>%
  as.character() %>%
  enframe(name = NULL) %>%
  rename("number" = "value") %>%
  mutate(
    description =
      case_when(
        number %in% (27:29) ~ "Special Revenue Funds - (Assigned by District)",
        number %in% (61:69) ~ "Interal Service Fund - (Assigned by District)"
      ),
    can_submit = TRUE
  )

usbe_fund <-
  usbe_fund %>%
  bind_rows(extra_fund_numbers)

# Location =====================================================================   
usbe_location<- c(0:499,600:999) %>%
  # str_pad(3, pad = "0") %>% 
  as.character() %>%
  enframe(name = NULL) %>%
  rename("number" = "value") %>%
  mutate( description =
            case_when(
              as.numeric(number) <= 10    ~ "Pre School",
              as.numeric(number) >= 11 & as.numeric(number) <= 99   ~ "District Level",
              number %in% (100:299) ~ "Elementary Schools",
              number %in% (300:399) ~ "Middle Schools",
              number %in% (400:499) ~ "Junior High Schools/Middle Schools",
              number %in% (600:699) ~ "Special Schools, Secondary",
              number %in% (700:799) ~ "High Schools",
              number %in% (800:899) ~ "Special Schools, Elementary",
              number %in% (900:975) ~ "Private Schools",
              number %in% (976:999) ~ "Adult Education Schools"
            ),
          can_submit = TRUE
  ) %>% 
  mutate(number = number %>% str_pad(3, pad = "0")) 


# program  =====================================================================
usbe_program <-
  read_xlsx(usbe_file_name, sheet = excel_sheets(usbe_file_name)[[2]]) %>%
  rename(number = ProgramCode,
         description = ProgramDescription,
         can_submit = CanSubmit) %>% 
  mutate(number = number %>% str_pad(4, pad = "0"))

extra_program_numbers <- c(
  2000:3699,
  # 2000 SCHOOL LEVEL PROGRAMS
  5251:5254,
  5257:5259,
  5261:5269,
  5271:5290,
  7804:7809,
  7811:7819,
  7821:7829,
  7831:7839,
  7841:7849,
  7851:7859,
  7871:7879,
  7891:7899,
  7916:7919,
  7911:7914,
  7921:7929,
  7931:7939,
  7941:7949,
  7951:7959
) %>%
  as.character() %>%
  enframe(name = NULL) %>%
  rename("number" = "value") %>%
  mutate(
    description =
      case_when(
        number %in% (2000:2099) ~ "General School",
        number %in% (2100:2199) ~ "General Studentbody",
        number %in% (2200:3299) ~ "Instructional Classes",
        number %in% (3300:3599) ~ "Other Instructional Classes",
        number %in% (3600:3699) ~ "Student Activity Funds",
        
        number %in% (5251:5259) ~ "OTHER MINIMUM SCHOOL PROGRAMS - Reserved for Numbers Assigned by Districts",
        number %in% (5260:5269) ~ "OTHER MINIMUM SCHOOL PROGRAMS - Reserved for Numbers Assigned by Districts",
        number %in% (5270:5290) ~ "OTHER MINIMUM SCHOOL PROGRAMS - Reserved for Numbers Assigned by Districts",
        
        number %in% (7804:7809) ~ "(NCLB) Numbers reserved for districts",
        number %in% (7811:7819) ~ "(NCLB) Numbers reserved for districts",
        number %in% (7821:7829) ~ "(NCLB) Numbers reserved for districts",
        number %in% (7831:7839) ~ "(NCLB) Numbers reserved for districts",
        number %in% (7841:7849) ~ "(NCLB) Numbers reserved for districts",
        number %in% (7851:7859) ~ "(NCLB) Numbers reserved for districts",
        number %in% (7871:7879) ~ "(NCLB) Numbers reserved for districts",
        number %in% (7891:7899) ~ "(NCLB) Numbers reserved for districts",
        number %in% (7916:7919) ~ "(NCLB) Numbers reserved for districts",
        number %in% (7911:7914) ~ "(NCLB) Numbers reserved for districts",
        number %in% (7921:7929) ~ "(NCLB) Numbers reserved for districts",
        number %in% (7931:7939) ~ "(NCLB) Numbers reserved for districts",
        number %in% (7941:7949) ~ "(NCLB) Numbers reserved for districts",
        number %in% (7951:7959) ~ "(NCLB) Numbers reserved for districts"
      ),
    can_submit = TRUE
  )

usbe_program <-
  usbe_program %>%
  bind_rows(extra_program_numbers) %>% 
  group_by(number, description, can_submit) %>% 
  summarize(n=n()) %>% 
  filter(n==1)

# Function =====================================================================
usbe_funct <-
  read_xlsx(usbe_file_name, sheet = excel_sheets(usbe_file_name)[[4]]) %>%
  rename(number = FunctionCode,
         description = FunctionDescription,
         can_submit = CanSubmit) %>% 
  mutate(number = as.character(number))

# Object =======================================================================

usbe_object <-
  read_xlsx(usbe_file_name, sheet = excel_sheets(usbe_file_name)[[5]]) %>%
  rename(number = ObjectCode,
         description = ObjectDescription,
         can_submit = CanSubmit) %>% 
  mutate(number = as.character(number))

# Revenue Classification" ======================================================
usbe_revenue <-
  read_xlsx(usbe_file_name, sheet = excel_sheets(usbe_file_name)[[3]]) %>%
  rename(number = RevenueCode,
         description = RevenueDescription,
         can_submit = CanSubmit) %>% 
  mutate(number = as.character(number))

# Many LEA put 0000 for their revenue code. To keep them from all being removed,
# I created a temporary holder. I will likely change the name once I understand
# why this code is always being used.
extra_revenue_numbers <- c("0000") %>%
  as.character() %>%
  enframe(name = NULL) %>%
  rename("number" = "value") %>%
  mutate(
    description =
      case_when(number == "0000" ~ "TEMPORARY HOLDER - ALEX NIELSON",),
    can_submit = TRUE
  )

usbe_revenue <-
  usbe_revenue %>%
  bind_rows(extra_revenue_numbers) %>% 
  group_by(number, description, can_submit) %>% 
  summarize(n=n()) %>% 
  filter(n==1) 


#consolidate
usbe_lookup <- list(
  fund     = usbe_fund,
  location = usbe_location,
  program  = usbe_program,
  funct    = usbe_funct,
  object   = usbe_object,
  revenue  = usbe_revenue
)

usbe_names <- names(usbe_lookup)

#Export to a csv document ======================================================

wb <- createWorkbook()

for(name in usbe_names){
  sheet_name <- name #str_trunc(name, 30, side = c("right"), ellipsis = "...")
  tempdata<- usbe_lookup %>% pluck(name)
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name,tempdata,colNames = TRUE)
}

saveWorkbook(wb, file = "usbe_lookup.xlsx",  overwrite = TRUE)



# Empty Garbage:
rm(
  usbe_names,
  tempdata,
  name,
  sheet_name,
  wb,
  usbe_fund,
  usbe_location,
  usbe_funct,
  usbe_program,
  usbe_revenue,
  usbe_object,
  extra_program_numbers,
  extra_fund_numbers,
  extra_revenue_numbers,
  usbe_file_name
)


