dsn_aws        <- "transpAWS"
dsn_salesforce <- "salesforce"

options(scipen=6)

library(lubridate)
library(magrittr)
library(odbc)
library(readxl) 
library(tidyverse)
library(stringi) 
library(tidyr)
library(plotly)

odbc_aws <- dbConnect(odbc::odbc(), dsn_aws)
odbc_sf  <- dbConnect(odbc::odbc(), dsn_salesforce)

rm(dsn_aws, dsn_salesforce)

alexis_query <- 
  dbGetQuery(
    odbc_aws,
    "
SELECT *
FROM transaction
WHERE fund1 = 1446613 
  OR  fund2 = 1446613 
  OR  fund3 = 1446613 
  OR  fund4 = 1446613 
  OR  program1 = 1446613 
  OR  program2 = 1446613 
  OR  program3 = 1446613 
  OR  program4 = 1446613 
  OR  program5 = 1446613 
  OR  program6 = 1446613 
  OR  program7 = 1446613 
  OR  org1 = 1446613 
  OR  org2 = 1446613 
  OR  org3 = 1446613 
  OR  org4 = 1446613 
  OR  org5 = 1446613 
  OR  org6 = 1446613 
  OR  org7 = 1446613 
  OR  org8 = 1446613 
  OR  org9 = 1446613 
  OR  cat1 = 1446613 
  OR  cat2 = 1446613 
  OR  cat3 = 1446613 
  OR  cat4 = 1446613 
  OR  cat5 = 1446613 
  OR  cat6 = 1446613 
  OR  cat7 = 1446613 
  OR  function1 = 1446613 
  OR  function2 = 1446613 
  OR  function3 = 1446613 
  OR  function4 = 1446613 
  OR  function5 = 1446613 
  OR  function6 = 1446613 
  OR  function7 = 1446613 
LIMIT 1
    ") %>% 
  as_tibble()



tg_head <- 
  dbGetQuery(
    odbc_aws,
    "
    SELECT *
FROM transaction_group
WHERE id  = 1446613
LIMIT 10000


    ") %>% 
  as_tibble()


tg_head <- 
  dbGetQuery(
    odbc_aws,
    "
    SELECT *
FROM transaction_group
WHERE name LIKE '%Education%' AND name like '%utah%' AND name LIKe '%state%'
LIMIT 10000
    ") %>% 
  as_tibble()

v_head <- 
  dbGetQuery(
    odbc_aws,
    "
    SELECT *
FROM vendor
WHERE name LIKE '%Education%' AND name like '%utah%' AND name LIKe '%state%' AND name LIKE '%board%'
LIMIT 10000
    ") %>% 
  as_tibble()

vendor_ids_with_usbe_vendor <- 
  dbGetQuery(
    odbc_aws,
    "
    SELECT *
FROM vendor
WHERE name LIKE '%state board of education%' 
LIMIT 10000
    ") %>% 
  as_tibble()

ids_to_check <- vendor_ids_with_usbe_vendor %>% pull(id) #%>% paste(", ")



query_fy2018 <- function(vendor_id){
  print(paste("querying vendor_id: ", vendor_id))
  temp<- 
    dbGetQuery(
      odbc_aws,
      paste0(
        "SELECT 
    t.id,
    t.batch_id,
    t.fiscal_year,
    t.type,
    entity.name as entity_name,
    entity.govt_lvl, 
    transaction_group.name as fund1, 
    transaction_group2.name as fund2, 
    transaction_group3.name as fund3, 
    transaction_group4.name as fund4, 
    transaction_group5.name as org1, 
    transaction_group6.name as org2, 
    transaction_group7.name as org3, 
    transaction_group8.name as org4,
    transaction_group15.name as cat1, 
    transaction_group16.name as cat2, 
    transaction_group17.name as cat3, 
    transaction_group18.name as cat4, 
    vendor.name as vendor_name, 
    t.posting_date, 
    t.description, 
    t.entity_trans_id, 
    t.ref_id, 
    t.contract_name, 
    t.contract_number, 
    vendor.title, 
    vendor.hourly_rate,
    t.amount, 
    t.account_number, 
    transaction_group22.name as program1, 
    transaction_group23.name as program2, 
    transaction_group24.name as program3, 
    transaction_group25.name as program4
  FROM transaction t
  INNER JOIN batch ON batch_id = batch.id
  INNER JOIN entity ON entity_id = entity.id
  INNER JOIN vendor ON vendor_id = vendor.id
  LEFT JOIN transaction_group ON t.Fund1 = transaction_group.id
  LEFT JOIN transaction_group as transaction_group2 ON t.Fund2 = transaction_group2.id
  LEFT JOIN transaction_group as transaction_group3 ON t.Fund3 = transaction_group3.id
  LEFT JOIN transaction_group as transaction_group4 ON t.Fund4 = transaction_group4.id
  LEFT JOIN transaction_group as transaction_group5 ON t.Org1 = transaction_group5.id
  LEFT JOIN transaction_group as transaction_group6 ON t.Org2 = transaction_group6.id
  LEFT JOIN transaction_group as transaction_group7 ON t.Org3 = transaction_group7.id
  LEFT JOIN transaction_group as transaction_group8 ON t.Org4 = transaction_group8.id
  LEFT JOIN transaction_group as transaction_group9 ON t.Org5 = transaction_group9.id
  LEFT JOIN transaction_group as transaction_group10 ON t.Org6 = transaction_group10.id
  LEFT JOIN transaction_group as transaction_group11 ON t.Org7 = transaction_group11.id
  LEFT JOIN transaction_group as transaction_group12 ON t.Org8 = transaction_group12.id
  LEFT JOIN transaction_group as transaction_group13 ON t.Org9 = transaction_group13.id
  LEFT JOIN transaction_group as transaction_group14 ON t.Org10 = transaction_group14.id
  LEFT JOIN transaction_group as transaction_group15 ON t.cat1 = transaction_group15.id
  LEFT JOIN transaction_group as transaction_group16 ON t.cat2 = transaction_group16.id
  LEFT JOIN transaction_group as transaction_group17 ON t.cat3 = transaction_group17.id
  LEFT JOIN transaction_group as transaction_group18 ON t.cat4 = transaction_group18.id
  LEFT JOIN transaction_group as transaction_group19 ON t.cat5 = transaction_group19.id
  LEFT JOIN transaction_group as transaction_group20 ON t.cat6 = transaction_group20.id
  LEFT JOIN transaction_group as transaction_group21 ON t.cat7 = transaction_group21.id
  LEFT JOIN transaction_group as transaction_group22 ON t.Program1 = transaction_group22.id
  LEFT JOIN transaction_group as transaction_group23 ON t.Program2 = transaction_group23.id
  LEFT JOIN transaction_group as transaction_group24 ON t.Program3 = transaction_group24.id
  LEFT JOIN transaction_group as transaction_group25 ON t.Program4 = transaction_group25.id
  LEFT JOIN transaction_group as transaction_group26 ON t.Program5 = transaction_group26.id
  LEFT JOIN transaction_group as transaction_group27 ON t.Program6 = transaction_group27.id
  LEFT JOIN transaction_group as transaction_group28 ON t.Program7 = transaction_group28.id
  LEFT JOIN transaction_group as transaction_group29 ON t.Function1 = transaction_group29.id
  LEFT JOIN transaction_group as transaction_group30 ON t.Function2 = transaction_group30.id
  LEFT JOIN transaction_group as transaction_group31 ON t.Function3 = transaction_group31.id
  LEFT JOIN transaction_group as transaction_group32 ON t.Function4 = transaction_group32.id
  LEFT JOIN transaction_group as transaction_group33 ON t.Function5 = transaction_group33.id
  LEFT JOIN transaction_group as transaction_group34 ON t.Function6 = transaction_group34.id
  LEFT JOIN transaction_group as transaction_group35 ON t.Function7 = transaction_group35.id
  WHERE 
  # (t.Org1 = 1446613  
  #       OR t.Org2 = 1446613 
  #       OR t.Org3 = 1446613
  #       OR t.Org4 = 1446613
  #       OR t.Org5 = 1446613)
    #AND
    t.fiscal_year = 2018
    AND t.vendor_id = ",vendor_id,"
  "
      )) %>% 
    as_tibble() 
}

transactions_with_usbe_vendor <- ids_to_check %>% head() %>% map(query_fy2018)

lea_names<-
  dbGetQuery(
    odbc_aws,
    paste(
      "SELECT DISTINCT name
     FROM entity
     WHERE govt_lvl = 'K12 EDUCATION' 
      AND name != 'xk12' 
      AND name != 'xxk12'
    "
    )) %>% select(name) %>% arrange(name)%>%  unlist()

temp_names <- lea_names %>% head()

lea_usbe_data <- temp_names %>% map(query_fy2018)


temp<- lea_usbe_data %>% pluck(1) %>% head()
