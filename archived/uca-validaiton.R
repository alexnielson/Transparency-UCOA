# batch validation scripts
# Alexander Nielson 3/11/2020

# Options
options(scipen = 6)

#lib
library(lubridate)
library(magrittr)
library(odbc)
library(openxlsx)
library(readxl)
library(tidyverse)
library(stringi)
library(tidyr)

#connect to dbms
dsn_aws        <- "transpAWS"
dsn_salesforce <- "salesforce"
odbc_aws <- dbConnect(odbc::odbc(), dsn_aws)
odbc_sf  <- dbConnect(odbc::odbc(), dsn_salesforce)

rm(dsn_aws, dsn_salesforce)


# a. database validation =======================================================
# validate_nrow ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

validate_nrow <- function(df, expected_nrow) {
  # Verify the number of rows in a data frame rounds to the expected value.
  #
  # Arguments:
  #   df (tbl): The data frame to validate.
  #   expected_nrow (num): The number of rows the report should contain, rounded
  #     to one significant digit.
  #
  # Value:
  #   Nothing, or a `stop()` message.
  
  if (!identical(df %>% nrow() %>% signif(1),
                 expected_nrow)) {
    paste0(
      "The rounded number of rows in ",
      deparse(substitute(df)),
      " is not near the expected ",
      expected_nrow,
      ". Did the data frame import correctly?
           Did the number of entities we monitor change significantly?"
    ) %>%
      stop()
  }
}

# make_current_begin_fy ++++++++++++++++++++++++++++++++++++++++++++++++++++++++

make_current_begin_fy <- function(begin_fy_unchecked) {
  # Update an outdated fiscal year begin date.
  #
  # Arguments:
  #   begin_fy_unchecked (Date): The first day of the entity's fiscal year, not
  #     yet verified to reflect the current fiscal year.
  #
  # Value:
  #   Date.
  #
  # Comments:
  #   As of 2019-03, our instance of Salesforce does not reliably update the
  #   Fiscal_Year_Begins__c field. Coding the update in this program was a
  #   quicker solution than waiting for the problem to be fixed in Salesforce.!(begin_fy_unchecked > today()) ||
  
  stop("make_current_begin_fy() must not be applied to future dates.",
       call. = FALSE)
  
  fy_interval <-
    begin_fy_unchecked %--% ((begin_fy_unchecked + years(1)) - 1)
  
  if (today() %within% fy_interval) {
    begin_fy_unchecked
    
  } else {
    year_correction <-
      (year(today()) - year(begin_fy_unchecked)) %>%
      if_else(month(begin_fy_unchecked) != 1 &
                month(begin_fy_unchecked) > month(today()),
              . - 1,
              .)
    
    begin_fy_unchecked + months(12 * year_correction)
  }
}

# query_batch_ids ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

query_batch_ids <- function(t_id) {
  # Query an entity's processed and processing batch IDs.
  #
  # Arguments:
  #   t_id (num): The entity's Transparency ID, as queried from Salesforce
  #     rather than Transparency.
  #
  # Value:
  #   Numeric.
  #
  # Comments:
  #   The 'DONTDELETE' batch status is a variation of 'PROCESSED.' DONTDELETE
  #   indicates the data in the batch has been split among multiple transaction
  #   tables, as the batch contains data from a fiscal year that has been
  #   archived.
  
  if (is.na(t_id)) {
    return(vector(mode = "numeric", length = 0))
  }
  
  dbGetQuery(
    odbc_aws,
    paste(
      "
          SELECT id
          FROM batch
          WHERE entity_id = ",
      t_id,
      "
          AND status IN ('PROCESSED', 'PROCESSING', 'DONTDELETE')"
    )
  ) %>%
    .[["id"]] %>%
    as.numeric()
}

# b. UCA related ===============================================================
# query_transactions +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
query_transactions <- function(batches_list, lookup_table) {
  # wrangled the batches to be readable in MySQL.
  # I know this is more work, and
  # that the batches could have been checked in sql, but I want my report to be
  # able to show exactly which batches were examined. I want this the repor
  # visible at the R level and not some additional thing happens on the SQL
  # backend.
  batches_wrangled <- batches_list %>%
    unlist() %>%
    as.character() %>%
    str_split(" ") %>%
    unlist() %>%
    #sQuote() %>%
    paste(collapse = ", ")
  
  
  # 1. Query the database, get all the records -----------------------------------
  
  transaction_report <-
    dbGetQuery(
      odbc_aws,
      paste(
        "
      SELECT id,
      batch_id,
      amount,
      fiscal_year,
      account_number,
      type
      FROM transaction
      WHERE batch_id IN (",
        batches_wrangled,
        ")"
      )
    ) %>%
    as_tibble()
  
  # because osome wierd "invalid mutlibyte string, element 372860"
  transaction_report$account_number  <-
    iconv(transaction_report$account_number , "WINDOWS-1252", "UTF-8")
  
  # 2. check for NA or "" ------------------------------------------------------
  
  # generates a invalid table we can send the entity.
  invalid_reported_na <- transaction_report %>%
    filter(account_number == "",
           is.na(account_number))
  
  # filter out the bad uca
  transaction_report <- transaction_report %>%
    filter(account_number != "",
           !is.na(account_number))
  
  # 3. check correct length ----------------------------------------------------
  
  
  
  # 19 is for normal, 25 is for USBE (dashes are included in string legth)
  invalid_length <- transaction_report %>%
    filter(nchar(account_number) != 19) %>%
    mutate(
      account_number_length = nchar(account_number),
      reason = case_when(
        account_number_length < 19 ~ paste(
          "Invalid Length - Code block too short - length =",
          account_number_length
        ),
        account_number_length > 19 ~ paste(
          "Invalid Length - Code block too long  - length =",
          account_number_length
        )
      )
    )
  
  
  
  transaction_report <- transaction_report %>%
    filter(nchar(account_number) == 19)
  
  # 4. check for valid form. ie: ##-###-####-####-###-#### ---------------------
  
  invalid_form <-
    transaction_report %>%
    filter(!str_detect(
      .$account_number,
      regex("^[:digit:]{3}-[:digit:]{6}-[:digit:]{8}$")
    )) %>%
    mutate(reason = "Invalid Form - form must be : ###-######-########")
  
  transaction_report <-
    transaction_report %>%
    filter(str_detect(
      .$account_number,
      regex("^[:digit:]{3}-[:digit:]{6}-[:digit:]{8}$")
    ))
  
  #5. enrich the transaction_report --------------------------------------------
  
  transaction_report <- transaction_report %>%
    mutate(
      fund                    = .[["account_number"]] %>% substr(0, 3),
      funct                   = .[["account_number"]] %>% substr(5, 10),
      account                 = .[["account_number"]] %>%  substr(12, 19),
      fund_code_primary       = str_sub(fund, 0, 2),
      fund_code_secondary     = str_sub(fund, 2, 3),
      funct_code_primary      = str_sub(funct, 0, 2),
      funct_code_secondary    = str_sub(funct, 3, 4),
      funct_code_tertiary     = str_sub(funct, 5, 6),
      account_code_primary    = str_sub(account, 0, 2),
      account_code_secondary  = str_sub(account, 3, 4),
      account_code_tertiary   = str_sub(account, 5, 6),
      account_code_quaternary = str_sub(account, 7, 8)
    ) %>%
    
    # Now we need to join ucoa.xlsx, so we know what the codes mean.
    
    # join the funds
    left_join(
      lookup_table %>%
        pluck("fund") %>%
        as_tibble() %>%
        select(
          number,
          fund_description = description,
          fund_level_primary = level_primary,
          fund_level_secondary = level_secondary
        ),
      by = c("fund" = "number")
    ) %>%
    # join the functions
    left_join(
      lookup_table %>%
        pluck("funct") %>%
        as_tibble() %>%
        select(
          number,
          funct_description = description,
          funct_level_primary = level_primary,
          funct_level_secondary = level_secondary,
          funct_level_tertiary = level_tertiary
        ),
      by = c("funct" = "number")
    ) %>%
    # join the accounts
    left_join(
      lookup_table %>%
        pluck("account") %>%
        as_tibble() %>%
        select(
          number,
          account_description = description,
          account_level_primary = level_primary,
          account_level_secondary = level_secondary,
          account_level_tertiary = level_tertiary
        ),
      by = c("account" = "number")
    )
  
  #6. Filter out any non revenue and expense uca -------------------------------
  
  invalid_account_type <- transaction_report %>%
    filter((str_sub(account, 0, 1) == 1  & type == 1) |
             (str_sub(account, 0, 1) == 1  & type == 2) |
             (str_sub(account, 0, 1) == 2  & type == 1) |
             (str_sub(account, 0, 1) == 2  & type == 1)
    ) %>%
    mutate(reason = "Invalid Account Chunk - Account chunk must start with a 3 (revenue) or 4 (expenditure).")
  
  transaction_report <- transaction_report %>%
    anti_join(invalid_account_type, by = "id")
  
  
  #7. Find unmapped uca codes --------------------------------------------------
  
  #find wrong fund code
  unmapped_fund_code <- transaction_report %>%
    filter(!fund %in% (lookup_table %>%
                         pluck("fund")  %>%
                         select(number) %>%
                         unlist())) %>%
    select(
      id,
      batch_id,
      amount,
      type,
      fiscal_year,
      account_number,
      fund,
      fund_code_primary,
      fund_code_secondary
    ) %>%
    mutate(reason = "Invalid Fund Code - The fund code does not exist in the current uniform chart of accounts. ")
  
  #If the fund code does not map, we want to know which component of the code
  #failed. ie: was it the primary or secondary component?
  
  # check primary
  unmapped_fund_code_primary <- unmapped_fund_code %>%
    filter(!fund_code_primary %in% (
      lookup_table %>%
        pluck("fund")  %>%
        select(code_primary) %>%
        unlist()
    )) %>%
    mutate(reason = "Invalid Fund Code - The primary piece of the fund code does not exist in the current uniform chart of accounts. ")
  
  #check secondary
  unmapped_fund_code_primary <- unmapped_fund_code %>%
    filter(!fund_code_primary %in% (
      lookup_table %>%
        pluck("fund")  %>%
        select(code_primary) %>%
        unlist()
    )) %>%
    mutate(reason = "Invalid Fund Code - The secondary piece the fund code does not exist in the current uniform chart of accounts. ")
  
  
  #find wrong function code
  unmapped_funct_code <- transaction_report %>%
    filter(!funct %in% (
      lookup_table %>%
        pluck("funct") %>%
        select(number) %>%
        unlist()
    )) %>%
    select(
      id,
      batch_id,
      amount,
      type,
      fiscal_year,
      account_number,
      funct,
      funct_code_primary,
      funct_code_secondary,
      funct_code_tertiary
    ) %>%
    mutate(reason = "Invalid Function Code - The function code does not exist in the current uniform chart of accounts. ")
  
  #If the funct code does not map, we want to know which component of the code
  #failed. ie: was it the primary, secondary, or tertiary component?
  
  #check funct primary
  unmapped_funct_code_primary <- unmapped_funct_code %>%
    filter(!funct_code_primary %in% (
      lookup_table %>%
        pluck("funct")  %>%
        select(code_primary) %>%
        unlist()
    )) %>%
    mutate(reason = "Invalid Function Code - The first and/or second digits of the function code do not exist in the current uniform chart of accounts. ")
  
  
  #check funct secondary
  unmapped_funct_code_secondary <- unmapped_funct_code %>%
    filter(
      funct_code_primary %in% (
        lookup_table %>%
          pluck("funct")  %>%
          select(code_primary) %>%
          unlist()
      )
      ,
      !funct_code_secondary %in% (
        lookup_table %>%
          pluck("funct")  %>%
          select(code_secondary) %>%
          unlist()
      )
    ) %>%
    mutate(reason = "Invalid Function Code - The third and/or fourth digits of the function code do not exist in the current uniform chart of accounts. ")
  
  
  # check funct tertiary
  unmapped_funct_code_tertiary <- unmapped_funct_code %>%
    filter(
      funct_code_primary %in% (
        lookup_table %>%
          pluck("funct")  %>%
          select(code_primary) %>%
          unlist()
      )
      ,
      funct_code_secondary %in% (
        lookup_table %>%
          pluck("funct")  %>%
          select(code_secondary) %>%
          unlist()
      )
      ,
      !funct_code_tertiary %in% (
        lookup_table %>%
          pluck("funct")  %>%
          select(code_tertiary) %>%
          unlist()
      )
    ) %>%
    mutate(reason = "Invalid Function Code - The fifth and/or sixth digits of the function code do not exist in the current uniform chart of accounts. ")
  
  # check account
  unmapped_account_code <- transaction_report %>%
    filter(!account %in% (
      lookup_table %>%
        pluck("account") %>%
        select(number)   %>%
        unlist()
    )) %>%
    select(
      id,
      batch_id,
      amount,
      type,
      fiscal_year,
      account_number,
      account,
      account_code_primary,
      account_code_secondary,
      account_code_tertiary,
      account_code_quaternary
    ) %>%
    mutate(reason = "Invalid Account Code - The account code does not exist in the current uniform chart of accounts. ")
  
  #If the funct code does not map, we want to know which component of the code
  #failed. ie: was it the primary, secondary, tertiary, quaternary component?
  
  #check account primary
  unmapped_account_code_primary <- unmapped_account_code %>%
    filter(!account_code_primary %in% (
      lookup_table %>%
        pluck("account")  %>%
        select(code_primary) %>%
        unlist()
    )) %>%
    mutate(reason = "Invalid Account Code - The first and/or second digits of account code do not exist in the current uniform chart of accounts. ")
  
  
  #check account secondary
  unmapped_account_code_secondary <- unmapped_account_code %>%
    filter(
      account_code_primary %in% (
        lookup_table %>%
          pluck("account")  %>%
          select(code_primary) %>%
          unlist()
      ),
      !account_code_secondary %in% (
        lookup_table %>%
          pluck("account")  %>%
          select(code_secondary) %>%
          unlist()
      )
    ) %>%
    mutate(reason = "Invalid Account Code - The third and/or fourth digits of the account code do not exist in the current uniform chart of accounts. ")
  
  
  unmapped_account_code_tertiary <- unmapped_account_code %>%
    filter(
      account_code_primary %in% (
        lookup_table %>%
          pluck("account")  %>%
          select(code_primary) %>%
          unlist()
      ),
      account_code_secondary %in% (
        lookup_table %>%
          pluck("account")  %>%
          select(code_secondary) %>%
          unlist()
      ),
      !account_code_tertiary %in% (
        lookup_table %>%
          pluck("account")  %>%
          select(code_tertiary) %>%
          unlist()
      )
    ) %>%
    mutate(reason = "Invalid Account Code - The fifth and/or sixth digits of the account code do not exist in the current uniform chart of accounts. ")
  
  unmapped_account_code_quaternary <- unmapped_account_code %>%
    filter(
      account_code_primary %in% (
        lookup_table %>%
          pluck("account")  %>%
          select(code_primary) %>%
          unlist()
      ),
      account_code_secondary %in% (
        lookup_table %>%
          pluck("account")  %>%
          select(code_secondary) %>%
          unlist()
      ),
      account_code_tertiary %in% (
        lookup_table %>%
          pluck("account")  %>%
          select(code_tertiary) %>%
          unlist()
      )#,
      # !account_code_quaternary %in% (
      #   lookup_table %>%
      #     pluck("account")  %>%
      #     select(code_quaternary) %>%
      #     unlist()
      # )
    ) %>%
    mutate(reason = "Invalid Account Code - The seventh and/or eighth digits of the account code do not exist in the current uniform chart of accounts. ")
  
  #now remove incorrectly mapped. we will save them to report, but do not want
  #them in our analysis.
  transaction_report <- transaction_report %>%
    anti_join(unmapped_fund_code, by = "id") %>%
    anti_join(unmapped_funct_code, by = "id") %>%
    anti_join(unmapped_account_code, by = "id")
  
  #8. check account type -------------------------------------------------------
  
  # finally check that the account is mapped to the transaction type
  # 1 = expediture
  # 2 = revenue
  
  exp_transaction_but_rev_account <- transaction_report %>%
    filter(type == 1 & str_sub(account, 0, 1) == 3) %>%
    mutate(reason = "Invalid Account Code or Transaction Type - the transaction is listed as an expenditure, but has an account code correpsonding to a revenue ") %>%
    select(id,
           batch_id,
           amount,
           type,
           fiscal_year,
           account_number,
           account,
           reason)
  
  rev_transaction_but_exp_account <- transaction_report %>%
    filter(type == 2 & str_sub(account, 0, 1) == 4) %>%
    mutate(reason = "Invalid Account Code or Transaction Type - the transaction is listed as a revenue, but has an account code correpsonding to a expenditure ") %>%
    select(id,
           batch_id,
           amount,
           type,
           fiscal_year,
           account_number,
           account,
           reason)
  
  transaction_report <- transaction_report %>%
    anti_join(exp_transaction_but_rev_account, by = "id") %>%
    anti_join(rev_transaction_but_exp_account, by = "id")
  
  
  # Deal with NOT Applicable Function Code. We need to filter these out now.
  
  rev_invalid_function_use <- transaction_report %>%
    filter(type == 2 & funct == "000000") %>%
    mutate(reason = "Invalid Function Code - The 'Not Applicable' function code (000000) is no longer acceptable. Fix by listing an appropriate function code.")
  
  
  exp_invalid_function_use <- transaction_report %>%
    filter(type == 1  & funct == "000000") %>%
    mutate(reason = "Invalid Function Code - The 'Not Applicable' function code (000000) is no longer acceptable. Fix by listing an appropriate function code.")
  
  invalid_function_use <-
    rev_invalid_function_use %>% bind_rows(exp_invalid_function_use)
  
  transaction_report <- transaction_report %>%
    anti_join(invalid_function_use, by = "id")
  
  
  #now create a tibble which can let us know if there are errors present.
  if (transaction_report %>% nrow() == 0) {
    no_transactions = TRUE
  } else{
    no_transactions = FALSE
  }
  if (invalid_reported_na %>% nrow() > 0) {
    fail_na = TRUE
  } else{
    fail_na = FALSE
  }
  if (invalid_length %>% nrow() > 0) {
    fail_length = TRUE
  } else{
    fail_length = FALSE
  }
  if (invalid_form %>% nrow() > 0) {
    fail_form = TRUE
  } else{
    fail_form = FALSE
  }
  #  if(invalid_transaction_type %>% nrow()>0){fail_transaction_type = TRUE }else{fail_transaction_type =FALSE}
  if (invalid_account_type %>% nrow() > 0) {
    fail_account_type = TRUE
  } else{
    fail_account_type = FALSE
  }
  if (unmapped_fund_code %>% nrow() > 0) {
    fail_fund = TRUE
  } else{
    fail_fund = FALSE
  }
  if (unmapped_funct_code %>% nrow() > 0) {
    fail_funct = TRUE
  } else{
    fail_funct = FALSE
  }
  if (unmapped_funct_code_primary %>% nrow() > 0) {
    fail_funct_pri = TRUE
  } else{
    fail_funct_pri = FALSE
  }
  if (unmapped_funct_code_secondary %>% nrow() > 0) {
    fail_funct_sec = TRUE
  } else{
    fail_funct_sec = FALSE
  }
  if (unmapped_funct_code_tertiary %>% nrow() > 0) {
    fail_funct_ter = TRUE
  } else{
    fail_funct_ter = FALSE
  }
  if (unmapped_account_code %>% nrow() > 0) {
    fail_account = TRUE
  } else{
    fail_account = FALSE
  }
  if (unmapped_account_code_primary %>% nrow() > 0) {
    fail_account_pri = TRUE
  } else{
    fail_account_pri = FALSE
  }
  if (unmapped_account_code_secondary %>% nrow() > 0) {
    fail_account_sec = TRUE
  } else{
    fail_account_sec = FALSE
  }
  if (unmapped_account_code_tertiary %>% nrow() > 0) {
    fail_account_ter = TRUE
  } else{
    fail_account_ter = FALSE
  }
  if (unmapped_account_code_quaternary %>% nrow() > 0) {
    fail_account_qua = TRUE
  } else{
    fail_account_qua = FALSE
  }
  if (exp_transaction_but_rev_account %>% nrow() > 0) {
    fail_exp_as_rev = TRUE
  } else{
    fail_exp_as_rev = FALSE
  }
  if (rev_transaction_but_exp_account %>% nrow() > 0) {
    fail_rev_as_exp = TRUE
  } else{
    fail_rev_as_exp = FALSE
  }
  if (invalid_function_use %>% nrow() > 0) {
    fail_na_function_use = TRUE
  } else{
    fail_na_function_use = FALSE
  }
  
  summary_tibble <- tibble(
    name = c(
      "transaction_report",
      "invalid_reported_na",
      "invalid_length",
      "invalid_form",
      #      "fail_transaction_type",
      "fail_account_type",
      "unmapped_fund_code",
      "unmapped_funct_code",
      "unmapped_funct_code_primary",
      "unmapped_funct_code_secondary",
      "unmapped_funct_code_tertiary",
      "unmapped_account_code",
      "unmapped_account_code_primary",
      "unmapped_account_code_secondary",
      "unmapped_account_code_tertiary",
      "unmapped_account_code_quaternary",
      "exp_transaction_but_rev_account",
      "rev_transaction_but_exp_account",
      "fail_na_function_use"
    ),
    
    status = c(
      no_transactions,
      fail_na,
      fail_length,
      fail_form,
      #      fail_transaction_type,
      fail_account_type,
      fail_fund,
      fail_funct,
      fail_funct_pri,
      fail_funct_sec,
      fail_funct_ter,
      fail_account,
      fail_account_pri,
      fail_account_sec,
      fail_account_ter,
      fail_account_qua,
      fail_exp_as_rev,
      fail_rev_as_exp,
      fail_na_function_use
    )
    
  )
  
  
  
  # consolidate ----------------------------------------------------------------
  entity_info <-
    list(
      "transaction_report" = transaction_report,
      #1
      "invalid_reported_na" = invalid_reported_na,
      #2
      "invalid_length" = invalid_length,
      #3
      "invalid_form" = invalid_form,
      #4
      "unmapped_fund_code" = unmapped_fund_code,
      #5
      "unmapped_funct_code" = unmapped_funct_code,
      #6
      "unmapped_funct_code_primary" = unmapped_funct_code_primary,
      #7
      "unmapped_funct_code_secondary" = unmapped_funct_code_secondary,
      #8
      "unmapped_funct_code_tertiary" = unmapped_funct_code_tertiary,
      #9
      "unmapped_account_code" = unmapped_account_code,
      #10
      "unmapped_account_code_primary" = unmapped_account_code_primary,
      #11
      "unmapped_account_code_secondary" = unmapped_account_code_secondary,
      #12
      "unmapped_account_code_tertiary" = unmapped_account_code_tertiary,
      #13
      "unmapped_account_code_quaternary" = unmapped_account_code_quaternary,
      #14
      "exp_transaction_but_rev_account" = exp_transaction_but_rev_account,
      #15
      "rev_transaction_but_exp_account" = rev_transaction_but_exp_account,
      #      "invalid_transaction_type" = invalid_transaction_type,
      "invalid_account_type" = invalid_account_type,
      "invalid_function_use" = invalid_function_use,
      "summary_tibble" = summary_tibble
      
    ) #%>% enframe()
}

## extract_summary_tibble_errors +++++++++++++++++++++++++++++++++++++++++++++++
extract_summary_tibble_errors <- function(entity_name, report_df){
  report_df <- report_df %>% 
    filter(name == entity_name)
  
  if(report_df %>% 
     pull(queried_transactions) %>% 
     pluck(1) %>% 
     pluck("summary_tibble") %>% 
     filter(status=="TRUE") %>% 
     nrow() > 0){
    wrong_types <- report_df %>%
      pull(queried_transactions) %>%
      pluck(1) %>%
      pluck("summary_tibble") %>%
      filter(status == "TRUE") %>%
      pull(name) %>% 
      paste(., collapse = ', ')
  }else{
    wrong_types <- paste("no_errors")
  }
  
}

## pluck_queried_transactions +++++++++++++++++++++++++++++++++++++++++++++++++++
pluck_queried_transactions <- function(entity_name, report_df){
  report_df %>% 
    filter(name==entity_name) %>%
    pull(queried_transactions) %>%
    pluck(1) 
}


#Export ========================================================================
export_uca_problems<- function(entity_name, bad_uca_entities){
  print(paste(entity_name,"exporting"))
  uca_problems <- list()
  
  #invalid_length-----------------------------------------------------------------
  if(entity_name %>%
     pluck_queried_transactions(report_df = bad_uca_entities) %>%
     pluck("invalid_length")%>%
     nrow() > 0) {
    invalid_length <- entity_name %>%
      pluck_queried_transactions(report_df = bad_uca_entities) %>%
      pluck("invalid_length") %>%
      mutate(transaction_type = case_when(type == 1 ~ "expenditure",
                                          type == 2 ~ "revenue",
                                          type == 3 ~ "w2")) %>%
      select(
        fiscal_year,
        batch_id,
        "transaction_id" = id,
        amount,
        type,
        transaction_type,
        account_number,
        account_number_length,
        reason,
      )
    
    uca_problems[["invalid_length"]] <- invalid_length
  }
  
  #invalid_form-------------------------------------------------------------------
  if(entity_name %>%
     pluck_queried_transactions(report_df = bad_uca_entities) %>%
     pluck("invalid_form")%>%
     nrow() > 0) {
    invalid_form <-  entity_name %>%
      pluck_queried_transactions(report_df = bad_uca_entities) %>%
      pluck("invalid_form") %>%
      mutate(
        transaction_type = case_when(
          type == 1 ~ "expenditure",
          type == 2 ~ "revenue",
          type == 3 ~ "employee compensation",
          type == 4 ~ "stimulus expense",
          type == 5 ~ "stimulus revenue",
          type == 6 ~ "budget",
          type == 7 ~ "balance sheet"
        )
      ) %>%
      select(
        fiscal_year,
        batch_id,
        "transaction_id" = id,
        amount,
        type,
        transaction_type,
        account_number,
        reason,
      )
    uca_problems[["invalid_form"]] <- invalid_form
  }
  #umapped_fund_code--------------------------------------------------------------
  
  if(entity_name %>%
     pluck_queried_transactions(report_df = bad_uca_entities) %>%
     pluck("unmapped_fund_code")%>%
     nrow() > 0) {
    invalid_fund_code <- entity_name %>%
      pluck_queried_transactions(report_df = bad_uca_entities) %>%
      pluck("unmapped_fund_code") %>%
      mutate(
        transaction_type = case_when(
          type == 1 ~ "expenditure",
          type == 2 ~ "revenue",
          type == 3 ~ "employee compensation",
          type == 4 ~ "stimulus expense",
          type == 5 ~ "stimulus revenue",
          type == 6 ~ "budget",
          type == 7 ~ "balance sheet"
        )
      ) %>%
      select(
        fiscal_year,
        batch_id,
        "transaction_id" = id,
        amount,
        type,
        transaction_type,
        account_number,
        reason,
      )
    
    uca_problems[["invalid_fund_code"]] <- invalid_fund_code
  }
  
  
  
  
  #umapped_funct_code-------------------------------------------------------------
  if(entity_name %>%
     pluck_queried_transactions(report_df = bad_uca_entities) %>%
     pluck("unmapped_funct_code")%>%
     nrow() > 0) {
    
    #   unmapped_funct_code <-  entity_name %>%
    #   pluck_queried_transactions(report_df = bad_uca_entities) %>%
    #   pluck("unmapped_funct_code") %>%
    #   mutate(
    #     transaction_type = case_when(
    #       type == 1 ~ "expenditure",
    #       type == 2 ~ "revenue",
    #       type == 3 ~ "employee compensation",
    #       type == 4 ~ "stimulus expense",
    #       type == 5 ~ "stimulus revenue",
    #       type == 6 ~ "budget",
    #       type == 7 ~ "balance sheet"
    #     )
    #   ) %>%
    #   select(
    #     fiscal_year,
    #     batch_id,
    #     "transaction_id" = id,
    #     amount,
    #     type,
    #     transaction_type,
    #     account_number,
    #     reason,
    #   )
    # uca_problems[["invalid_funct_code"]] <- unmapped_funct_code
    
    # check primary level ````````````````````````````````````````````````````````
    if(entity_name %>%
       pluck_queried_transactions(report_df = bad_uca_entities) %>%
       pluck("unmapped_funct_code_primary")%>%
       nrow() > 0){
      unmapped_funct_code_primary <-  entity_name %>%
        pluck_queried_transactions(report_df = bad_uca_entities) %>%
        pluck("unmapped_funct_code_primary") %>%
        mutate(
          transaction_type = case_when(
            type == 1 ~ "expenditure",
            type == 2 ~ "revenue",
            type == 3 ~ "employee compensation",
            type == 4 ~ "stimulus expense",
            type == 5 ~ "stimulus revenue",
            type == 6 ~ "budget",
            type == 7 ~ "balance sheet"
          )
        ) %>%
        select(
          fiscal_year,
          batch_id,
          "transaction_id" = id,
          amount,
          type,
          transaction_type,
          account_number,
          reason,
        )
      uca_problems[["invalid_funct_code_primary"]] <- unmapped_funct_code_primary
    }
    
    # check secondary level ````````````````````````````````````````````````````
    if(entity_name %>%
       pluck_queried_transactions(report_df = bad_uca_entities) %>%
       pluck("unmapped_funct_code_secondary")%>%
       nrow() > 0){
      unmapped_funct_code_secondary <-  entity_name %>%
        pluck_queried_transactions(report_df = bad_uca_entities) %>%
        pluck("unmapped_funct_code_secondary") %>%
        mutate(
          transaction_type = case_when(
            type == 1 ~ "expenditure",
            type == 2 ~ "revenue",
            type == 3 ~ "employee compensation",
            type == 4 ~ "stimulus expense",
            type == 5 ~ "stimulus revenue",
            type == 6 ~ "budget",
            type == 7 ~ "balance sheet"
          )
        ) %>%
        select(
          fiscal_year,
          batch_id,
          "transaction_id" = id,
          amount,
          type,
          transaction_type,
          account_number,
          reason,
        )
      uca_problems[["invalid_funct_code_secondary"]] <- unmapped_funct_code_secondary
    }
    
    # check tertiary level ````````````````````````````````````````````````````
    if(entity_name %>%
       pluck_queried_transactions(report_df = bad_uca_entities) %>%
       pluck("unmapped_funct_code_tertiary")%>%
       nrow() > 0){
      unmapped_funct_code_tertiary <-  entity_name %>%
        pluck_queried_transactions(report_df = bad_uca_entities) %>%
        pluck("unmapped_funct_code_tertiary") %>%
        mutate(
          transaction_type = case_when(
            type == 1 ~ "expenditure",
            type == 2 ~ "revenue",
            type == 3 ~ "employee compensation",
            type == 4 ~ "stimulus expense",
            type == 5 ~ "stimulus revenue",
            type == 6 ~ "budget",
            type == 7 ~ "balance sheet"
          )
        ) %>%
        select(
          fiscal_year,
          batch_id,
          "transaction_id" = id,
          amount,
          type,
          transaction_type,
          account_number,
          reason,
        )
      uca_problems[["invalid_funct_code_tertiary"]] <- unmapped_funct_code_tertiary
    }
    
    
  }    
  
  #umapped_account_code ----------------------------------------------------------
  if(entity_name %>%
     pluck_queried_transactions(report_df = bad_uca_entities) %>%
     pluck("unmapped_account_code")%>%
     nrow() > 0) {
    
    # unmapped_account_code <-  entity_name %>%
    #   pluck_queried_transactions(report_df = bad_uca_entities) %>%
    #   pluck("unmapped_account_code") %>%
    #   mutate(
    #     transaction_type = case_when(
    #       type == 1 ~ "expenditure",
    #       type == 2 ~ "revenue",
    #       type == 3 ~ "employee compensation",
    #       type == 4 ~ "stimulus expense",
    #       type == 5 ~ "stimulus revenue",
    #       type == 6 ~ "budget",
    #       type == 7 ~ "balance sheet"
    #     )
    #   ) %>%
    #   select(
    #     fiscal_year,
    #     batch_id,
    #     "transaction_id" = id,
    #     amount,
    #     type,
    #     transaction_type,
    #     account_number,
    #     reason,
    #   )
    # uca_problems[["invalid_account_code"]] <- unmapped_account_code
    
    # primary level ``````````````````````````````````````````````````````````````
    if (entity_name %>%
        pluck_queried_transactions(report_df = bad_uca_entities) %>%
        pluck("unmapped_account_code_primary") %>%
        nrow() > 0) {
      unmapped_account_code_primary <-  entity_name %>%
        pluck_queried_transactions(report_df = bad_uca_entities) %>%
        pluck("unmapped_account_code_primary") %>%
        mutate(
          transaction_type = case_when(
            type == 1 ~ "expenditure",
            type == 2 ~ "revenue",
            type == 3 ~ "employee compensation",
            type == 4 ~ "stimulus expense",
            type == 5 ~ "stimulus revenue",
            type == 6 ~ "budget",
            type == 7 ~ "balance sheet"
          )
        ) %>%
        select(
          fiscal_year,
          batch_id,
          "transaction_id" = id,
          amount,
          type,
          transaction_type,
          account_number,
          reason,
        )
      
      uca_problems[["invalid_account_code_primary"]] <- unmapped_account_code_primary
      
    }
    
    # secondary level ````````````````````````````````````````````````````````````
    if (entity_name %>%
        pluck_queried_transactions(report_df = bad_uca_entities) %>%
        pluck("unmapped_account_code_secondary") %>%
        nrow() > 0) {
      unmapped_account_code_secondary <-  entity_name %>%
        pluck_queried_transactions(report_df = bad_uca_entities) %>%
        pluck("unmapped_account_code_secondary") %>%
        mutate(
          transaction_type = case_when(
            type == 1 ~ "expenditure",
            type == 2 ~ "revenue",
            type == 3 ~ "employee compensation",
            type == 4 ~ "stimulus expense",
            type == 5 ~ "stimulus revenue",
            type == 6 ~ "budget",
            type == 7 ~ "balance sheet"
          )
        ) %>%
        select(
          fiscal_year,
          batch_id,
          "transaction_id" = id,
          amount,
          type,
          transaction_type,
          account_number,
          reason,
        )
      
      uca_problems[["invalid_account_code_secondary"]] <- unmapped_account_code_secondary
      
    }
    
    # tertiary level `````````````````````````````````````````````````````````````
    if (entity_name %>%
        pluck_queried_transactions(report_df = bad_uca_entities) %>%
        pluck("unmapped_account_code_tertiary") %>%
        nrow() > 0) {
      unmapped_account_code_tertiary <-  entity_name %>%
        pluck_queried_transactions(report_df = bad_uca_entities) %>%
        pluck("unmapped_account_code_tertiary") %>%
        mutate(
          transaction_type = case_when(
            type == 1 ~ "expenditure",
            type == 2 ~ "revenue",
            type == 3 ~ "employee compensation",
            type == 4 ~ "stimulus expense",
            type == 5 ~ "stimulus revenue",
            type == 6 ~ "budget",
            type == 7 ~ "balance sheet"
          )
        ) %>%
        select(
          fiscal_year,
          batch_id,
          "transaction_id" = id,
          amount,
          type,
          transaction_type,
          account_number,
          reason,
        )
      
      uca_problems[["invalid_account_code_tertiary"]] <- unmapped_account_code_tertiary
      
    }
    
    
    # quaternary level ```````````````````````````````````````````````````````````
    if (entity_name %>%
        pluck_queried_transactions(report_df = bad_uca_entities) %>%
        pluck("unmapped_account_code_quaternary") %>%
        nrow() > 0) {
      unmapped_account_code_quaternary <-  entity_name %>%
        pluck_queried_transactions(report_df = bad_uca_entities) %>%
        pluck("unmapped_account_code_quaternary") %>%
        mutate(
          transaction_type = case_when(
            type == 1 ~ "expenditure",
            type == 2 ~ "revenue",
            type == 3 ~ "employee compensation",
            type == 4 ~ "stimulus expense",
            type == 5 ~ "stimulus revenue",
            type == 6 ~ "budget",
            type == 7 ~ "balance sheet"
          )
        ) %>%
        select(
          fiscal_year,
          batch_id,
          "transaction_id" = id,
          amount,
          type,
          transaction_type,
          account_number,
          reason,
        )
      
      uca_problems[["invalid_account_code_quaternary"]] <- unmapped_account_code_quaternary
      
    }
    
  }    
  #exp_transaction_but_rev_account------------------------------------------------
  
  if(entity_name %>%
     pluck_queried_transactions(report_df = bad_uca_entities) %>%
     pluck("exp_transaction_but_rev_account")%>%
     nrow() > 0) {
    print(paste("--",entity_name,"exp_transaction_but_rev_account"))
    incorrect_account_code_expenditure <- entity_name %>%
      pluck_queried_transactions(report_df = bad_uca_entities) %>%
      pluck("exp_transaction_but_rev_account") %>%
      mutate(
        transaction_type = case_when(
          type == 1 ~ "expenditure",
          type == 2 ~ "revenue",
          type == 3 ~ "employee compensation",
          type == 4 ~ "stimulus expense",
          type == 5 ~ "stimulus revenue",
          type == 6 ~ "budget",
          type == 7 ~ "balance sheet"
        )
      ) %>%
      select(
        fiscal_year,
        batch_id,
        "transaction_id" = id,
        amount,
        type,
        transaction_type,
        account_number,
        reason,
      )
    
    uca_problems[["fail_trans_type_exp"]] <- incorrect_account_code_expenditure
  }    
  #exp_transaction_but_rev_account--------------------------------------------
  if (entity_name %>%
      pluck_queried_transactions(report_df = bad_uca_entities) %>%
      pluck("rev_transaction_but_exp_account") %>%
      nrow() > 0) {
    incorrect_account_code_revenue <- entity_name %>%
      pluck_queried_transactions(report_df = bad_uca_entities) %>%
      pluck("rev_transaction_but_exp_account") %>%
      mutate(
        transaction_type = case_when(
          type == 1 ~ "expenditure",
          type == 2 ~ "revenue",
          type == 3 ~ "employee compensation",
          type == 4 ~ "stimulus expense",
          type == 5 ~ "stimulus revenue",
          type == 6 ~ "budget",
          type == 7 ~ "balance sheet"
        )
      ) %>%
      select(
        fiscal_year,
        batch_id,
        "transaction_id" = id,
        amount,
        type,
        transaction_type,
        account_number,
        reason,
      )
    
    uca_problems[["fail_trans_type_rev"]] <-
      incorrect_account_code_revenue
  }
  
  # get the ones with invalid transaction types ==================================
  # if (entity_name %>%
  #     pluck_queried_transactions(report_df = bad_uca_entities) %>%
  #     pluck("invalid_transaction_type") %>%
  #     nrow() > 0) {
  #   incorrect_transaction_type <- entity_name %>%
  #     pluck_queried_transactions(report_df = bad_uca_entities) %>%
  #     pluck("invalid_transaction_type") %>%
  #     mutate(
  #       transaction_type = case_when(
  #         # type == 1 ~ "expenditure",
  #         # type == 2 ~ "revenue",
  #         # type == 3 ~ "employee compensation",
  #         type == 4 ~ "stimulus expense",
  #         type == 5 ~ "stimulus revenue",
  #         type == 6 ~ "budget",
  #         type == 7 ~ "balance sheet"
  #       )
  #     ) %>%
  #     select(
  #       fiscal_year,
  #       batch_id,
  #       "transaction_id" = id,
  #       amount,
  #       type,
  #       transaction_type,
  #       account_number,
  #       reason,
  #     )
  #   
  #   uca_problems[["incorrect_transaction_type"]] <-
  #     incorrect_transaction_type
  # }
  
  # get the ones with invalid account code types =================================
  if (entity_name %>%
      pluck_queried_transactions(report_df = bad_uca_entities) %>%
      pluck("invalid_account_type") %>%
      nrow() > 0) {
    
    #print(paste(entity_name,"invalid account code found!"))
    
    
    invalid_account_type <- entity_name %>%
      pluck_queried_transactions(report_df = bad_uca_entities) %>%
      pluck("invalid_account_type") %>%
      mutate(
        transaction_type = case_when(
          type == 1 ~ "expenditure",
          type == 2 ~ "revenue",
          type == 3 ~ "employee compensation",
          type == 4 ~ "stimulus expense",
          type == 5 ~ "stimulus revenue",
          type == 6 ~ "budget",
          type == 7 ~ "balance sheet"
        )
      ) %>%
      select(
        fiscal_year,
        batch_id,
        "transaction_id" = id,
        amount,
        type,
        transaction_type,
        account_number,
        reason,
      )
    
    uca_problems[["invalid_account_type"]] <-
      invalid_account_type
  }
  
  
  # get the ones with 'Not Applicable' Function ==================================
  if (entity_name %>%
      pluck_queried_transactions(report_df = bad_uca_entities) %>%
      pluck("invalid_function_use") %>%
      nrow() > 0) {
    incorrect_function_use <- entity_name %>%
      pluck_queried_transactions(report_df = bad_uca_entities) %>%
      pluck("invalid_function_use") %>%
      mutate(
        transaction_type = case_when(
          type == 1 ~ "expenditure",
          type == 2 ~ "revenue",
          type == 3 ~ "employee compensation",
          type == 4 ~ "stimulus expense",
          type == 5 ~ "stimulus revenue",
          type == 6 ~ "budget",
          type == 7 ~ "balance sheet"
        )
      ) %>%
      select(
        fiscal_year,
        batch_id,
        "transaction_id" = id,
        amount,
        type,
        transaction_type,
        account_number,
        reason,
      )
    
    uca_problems[["not_applicable_function_code"]] <-
      incorrect_function_use
  }
  
  
  # summary of problems:
  summary_uca <- tibble()
  account_holder <- tibble()
  
  for (tib in uca_problems) {
    
    unique_account_nums <- tib %>% pull(account_number) %>% unique()
    
    for (account_num in unique_account_nums) {
      fiscal_years <- tib %>% filter(account_number == account_num) %>% pull(fiscal_year) %>% unique() %>% paste(collapse = ", ")
      batches <- tib %>% filter(account_number == account_num) %>% pull(batch_id) %>% unique() %>% paste(collapse = ", ")
      reason <- tib %>% filter(account_number == account_num) %>% pull(reason) %>% unique()
      type <- tib %>% filter(account_number == account_num) %>% pull(type) %>% unique()
      transaction_type <- tib %>% filter(account_number == account_num) %>% pull(transaction_type) %>% unique()
      temp_tib <- tibble("fiscal_year(s)" = fiscal_years,
                         "batch_id(s)" = batches,
                         #"type" = type,
                         "transaction_type"=transaction_type,
                         "account_number" = account_num,
                         "reason" = reason)
      
      account_holder <- account_holder %>% bind_rows(temp_tib)
    }
    
    
    
    summary_uca <- summary_uca %>% bind_rows(account_holder)
  }
  
  summary_uca <- summary_uca %>% distinct()
  
  uca_problems[["summary_of_problems"]] <-
    summary_uca
  
  
  
  ## Create a blank workbook
  wb <- createWorkbook()
  
  header_st <-createStyle(fontSize = 13, fontColour = "#FFFFFF", halign = "center",
                          fgFill = "#34a8eb", border="TopBottom", borderColour = "#000000")
  
  bodyStyle <- createStyle(border="TopBottom", borderColour = "#4F81BD")
  
  ## Loop through the list of split tables as well as their names
  ##   and add each one as a sheet to the workbook
  Map(function(data, name) {
    addWorksheet(wb, name)
    
    writeData(wb,
              name,
              data,
              headerStyle = header_st)
    
    # addStyle(wb, sheet = name, header_st, rows = 1, cols = 1:ncol(data), gridExpand = TRUE)
    # 
    # addStyle(wb, sheet = name, bodyStyle, rows = 2:nrow(data), cols = 1:ncol(data), gridExpand = TRUE)
    
    setColWidths(wb, name, cols = 1:ncol(data), widths = "auto")
    
  }, uca_problems, names(uca_problems))
  
  
  ## Save workbook to working directory
  ## 
  #file_path <- paste0("E:/rprojects/Transparency-UCOA/bad-entity-files/",entity_name,"-admin.xlsx")
  file_path <- paste0("E:/reports/admin-uca/",entity_name,".xlsx")
  saveWorkbook(wb, file = file_path, overwrite = TRUE)
  
  
  wb_entity <- createWorkbook()
  addWorksheet(wb_entity, "summary of uca problems")
  
  writeData(wb_entity,
            "summary of uca problems",
            uca_problems[["summary_of_problems"]],
            headerStyle = header_st)
  setColWidths(wb_entity, "summary of uca problems", cols = 1:ncol(uca_problems[["summary_of_problems"]]), widths = "auto")
  
  file_path <- paste0("E:/reports/bad-uca/",entity_name,".xlsx")
  saveWorkbook(wb_entity, file = file_path, overwrite = TRUE)
}