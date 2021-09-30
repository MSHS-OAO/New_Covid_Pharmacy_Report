# Code for analyzing COVID Surge Med data
# Last Updated 2021_09_014

rm(list=ls())

# Import Libraries

library(readxl)
library(writexl)
library(stringr)
library(tidyverse)
library(dplyr)



# Work Directory
wrk.dir <- "J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/COVID Pharmacy/Data/Epic Latest Reports/Daily Reports"
setwd(wrk.dir)


# Import COVID Surge Med WINV data
new_inv_list <- list.files(path= paste0(wrk.dir,"/Med Inventory - Phase2"), full.names=TRUE, pattern = paste0("COVID Surge Med WINV Balance_", Sys.Date()) )
new_inventory_raw <-  read_excel(new_inv_list, col_names = TRUE, na = c("", "NA"))

#Add Date Column
new_inventory_raw <- new_inventory_raw %>% mutate(UpdateDate = as.Date(LAST_UPDATE_TIME, format = "%d-%b-%y"),
                                                  ReportDate =  as.Date(max(LAST_UPDATE_TIME),origin = "2020-01-01")-1)

                                                                      
# Add Site 
new_inventory_raw <- new_inventory_raw %>% mutate(Site = ifelse(str_detect("MSHS COVID 19 Stockpile", INV_NAME), "MSHS Stockpile",
                                                    str_replace(str_extract(INV_NAME, "[A-Z]+(\\s|\\-)"), "\\s|\\-", "")),
                                                    Site=ifelse(Site== "BI","MSBI" , Site))

  
# Extract inventory item concentration

# Pattern 1: x MCG/y ML
conc_pattern_1 <- "[0-9]+(\\.|\\,)*[0-9]*\\s(MCG/)[0-9]*(\\.|\\,)*[0-9]*\\s*(ML)"

# Pattern 2: x MG/y ML
conc_pattern_2 <- "[0-9]+(\\.|\\,)*[0-9]*\\s(MG/)[0-9]*(\\.|\\,)*[0-9]*\\s*(ML)"

# Pattern 3: x MG 
conc_pattern_3 <- "[0-9]+(\\.|\\,)*[0-9]*\\s(MG)\\s"

# Pattern 4: x UNIT/ y ML
conc_pattern_4 <- "[0-9]+(\\.|\\,)*[0-9]*\\s(UNIT/)[0-9]*(\\.|\\,)*[0-9]*\\s*(ML)"

# Extract inventory concentration from PRD_NAME
new_inventory_raw <- new_inventory_raw %>%
     mutate(ConcExtract = ifelse(str_detect(PRD_NAME, conc_pattern_1), str_extract(PRD_NAME, conc_pattern_1),
                              ifelse(str_detect(PRD_NAME, conc_pattern_2), str_extract(PRD_NAME, conc_pattern_2),
                                     ifelse(str_detect(PRD_NAME, conc_pattern_3), str_extract(PRD_NAME, conc_pattern_3),
                                            ifelse(str_detect(PRD_NAME, conc_pattern_4), str_extract(PRD_NAME, conc_pattern_4), NA)))))



# Fix concentrations with missing spaces or missing numeric values (ie, 200 MG/200ML, 10 MG/ML)
new_inventory_raw <- new_inventory_raw %>%
  mutate(ConcExtract = ifelse(str_detect(ConcExtract, "([0-9])(ML)"), str_replace(ConcExtract, "([0-9])(ML)", "\\1 \\2"),
                              ifelse(str_detect(ConcExtract, "(/ML)"), str_replace(ConcExtract, "(/ML)", "/1 ML"), ConcExtract)))



# Split concentrations on spaces
new_inventory_raw$ConcExt_split<- new_inventory_raw$ConcExtract
new_inventory_raw <- new_inventory_raw %>% separate(ConcExt_split, c("ConcDoseSize", "b", "ConcVolUnit"), extra = "merge", fill = "left", sep = " ")
new_inventory_raw <- new_inventory_raw %>% separate(b, c( "ConcDoseUnit", "ConcVolSize"), extra = "merge", fill = "left", sep = "/")
new_inventory_raw <- new_inventory_raw %>% mutate(ConcDoseUnit=ifelse(is.na(ConcDoseUnit), ConcVolSize, ConcDoseUnit),
                                                  ConcVolSize=ifelse(ConcVolSize=="MG", NA, ConcVolSize))



# Format concentration columns into numerics
new_inventory_raw <- new_inventory_raw %>% mutate(ConcDoseSize = as.numeric(str_replace(ConcDoseSize, "\\,", "")),
                                                    ConcVolSize =as.numeric(str_replace(ConcVolSize, "\\,", "")))


# Calculate normalize dose unit
new_inventory_raw <- new_inventory_raw %>% mutate(NormDoseSize = ifelse(is.na(ConcVolSize), ConcDoseSize, ConcDoseSize / ConcVolSize),
                                                   NormDoseUnit = paste0(ConcDoseUnit,"/",ConcVolUnit))



# Extract inventory item volume size --------------------------
# Strip out inventory sizes from Inventory Item Name
inv_size_pattern <- "\\,\\s[0-9]+(\\.|\\,)*[0-9]*\\s*(mL|g)"

# Extract inventory item size
new_inventory_raw <- new_inventory_raw %>% mutate(InvShortName = str_replace(PRD_NAME, inv_size_pattern, ""),
                                             InvSizeUnit = str_replace(str_extract(PRD_NAME, inv_size_pattern), "\\,\\s", ""))

# Replace NA with "1 Each" and fix inventory sizes with missing spaces (ie, 2mL) 
new_inventory_raw <- new_inventory_raw %>% mutate(InvSizeUnit = ifelse(is.na(InvSizeUnit), "1 Each", InvSizeUnit))

new_inventory_raw <- new_inventory_raw %>% mutate(InvSizeUnit = (str_replace(InvSizeUnit, "([0-9]+)(mL)", "\\1 \\2")))

# split InvSizeUnit into inventory item size and unit 
new_inventory_raw$InvSizeUnit_split <- new_inventory_raw$InvSizeUnit
new_inventory_raw <- new_inventory_raw  %>% separate(InvSizeUnit_split, c("InvSize", "InvUnit"), extra = "merge", fill = "left", sep = " ")

new_inventory_raw <- new_inventory_raw %>% mutate(InvSize=  as.numeric(as.character(InvSize)))



# Calculate total dose balance
new_inventory_raw <- new_inventory_raw %>% mutate(TotalDoseBalance = BALANCE * NormDoseSize * InvSize)

# Fix NDC ID and Code data alignment & NormDoseUnit = MG/ and /
new_inventory_raw <- new_inventory_raw %>% mutate(NDC_ID_ref = NDC_ID, NDC_CODE_ref = NDC_CODE) %>%
             mutate(NDC_ID = ifelse(str_detect(NDC_ID,"-"), NDC_CODE_ref, NDC_ID_ref), NDC_CODE = ifelse(str_detect(NDC_CODE,"-"), NDC_CODE_ref, NDC_ID_ref)) %>%
             mutate(NormDoseUnit = ifelse(NormDoseUnit == "/", NA, ifelse(NormDoseUnit == "MG/", "MG", NormDoseUnit)))

# Aggregate inventory data by site and NDC 
inv_site_summary <- new_inventory_raw  %>%
  group_by(ReportDate, Site, PRD_NAME, NDC_ID, NDC_CODE,
           ConcExtract, ConcDoseSize, ConcDoseUnit, ConcVolSize, ConcVolUnit,
           NormDoseSize, NormDoseUnit,
           InvShortName, InvSizeUnit, InvSize, InvUnit) %>%
  summarize(Balance = sum(BALANCE),
            TotalDoseBalance = sum(TotalDoseBalance)) %>%
  ungroup()


inv_site_summary$MedGroup <- toupper(gsub("([A-Za-z]+).*", "\\1", inv_site_summary$PRD_NAME))
inv_site_summary$MedGroup <- ifelse(inv_site_summary$MedGroup == "NOREPINEPHRINE",
                                    "NOREPINEPHRINE BITARTRATE", inv_site_summary$MedGroup)



## Inventory Repo File
inv_final_repo <- inv_site_summary


#### Import and pre-process Med Admin data ###

# Import COVID Surge Med WINV data
new_med_admin_list <- list.files(path= paste0(wrk.dir,"//Med Admin - Phase2"), full.names=TRUE, pattern = paste0("COVID Surge Med Admin_", Sys.Date()) )
new_med_admin_raw <-  read_excel(new_med_admin_list, col_names = TRUE, na = c("", "NA"))





