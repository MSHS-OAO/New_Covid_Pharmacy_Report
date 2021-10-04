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

# Import covid_med_groups data
covid_med_groups <- read_excel(paste0(wrk.dir, "/Medication Classification_COVID_Phase2.xlsx"), col_names = TRUE, na = c("", "NA"))
  

# Current week date
dates_pattern <-  seq(Sys.Date()-5, Sys.Date(), by='day')

#Import the latest Med Inv REPO file 
inv_repo <- file.info(list.files(path = paste0(wrk.dir,"/REPO/Inv_Repo"), full.names = T , pattern =paste0("Covid Surge Meds Inventory Repo-", dates_pattern, collapse = "|"  )))
repo_file <- rownames(inv_repo)[which.max(inv_repo$ctime)]
inv_repo <- read_excel(repo_file)

# Check the most recent ReportDate
max(inv_repo$ReportDate)


# Import COVID Surge Med WINV data
inv_list <- file.info(list.files(path= paste0(wrk.dir,"/Med Inventory - Phase2"), full.names=TRUE, pattern = paste0("COVID Surge Med WINV Balance_", dates_pattern, collapse = "|")))
inv_list = inv_list[with(inv_list, order(as.POSIXct(ctime), decreasing = TRUE)), ]

dif_time= difftime(max(inv_repo$ReportDate), Sys.Date()-1)
count = gsub('.*-([0-9]+).*','\\1', dif_time )

files = rownames(inv_list)[1:count]
new_inventory_raw <- lapply(files, function(filename){
  read_excel(filename, col_names = TRUE, na = c("", "NA"))})

#Add Date Column
new_inventory_raw <- lapply(new_inventory_raw,transform, UpdateDate = as.Date(LAST_UPDATE_TIME,  format = "%d-%b-%y"))
new_inventory_raw <- lapply(new_inventory_raw,transform,ReportDate =  as.Date(max(LAST_UPDATE_TIME),origin = "2020-01-01")-1)


inv_daily_df <-  do.call(rbind.data.frame,  new_inventory_raw )
rm(inv_list, new_inventory_raw)



                                                                      
# Add Site 
inv_daily_df <- inv_daily_df %>% mutate(Site = ifelse(str_detect("MSHS COVID 19 Stockpile", INV_NAME), "MSHS Stockpile",
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
inv_daily_df <- inv_daily_df %>% mutate(ConcExtract = ifelse(str_detect(PRD_NAME, conc_pattern_1), str_extract(PRD_NAME, conc_pattern_1),
                                    ifelse(str_detect(PRD_NAME, conc_pattern_2), str_extract(PRD_NAME, conc_pattern_2),
                                       ifelse(str_detect(PRD_NAME, conc_pattern_3), str_extract(PRD_NAME, conc_pattern_3),
                                            ifelse(str_detect(PRD_NAME, conc_pattern_4), str_extract(PRD_NAME, conc_pattern_4), NA)))))



# Fix concentrations with missing spaces or missing numeric values (ie, 200 MG/200ML, 10 MG/ML)
inv_daily_df <- inv_daily_df %>%  mutate(ConcExtract = ifelse(str_detect(ConcExtract, "([0-9])(ML)"), str_replace(ConcExtract, "([0-9])(ML)", "\\1 \\2"),
                                   ifelse(str_detect(ConcExtract, "(/ML)"), str_replace(ConcExtract, "(/ML)", "/1 ML"), ConcExtract)))

# Split concentrations on spaces
inv_daily_df$ConcExt_split<- inv_daily_df$ConcExtract
inv_daily_df <- inv_daily_df %>% separate(ConcExt_split, c("ConcDoseSize", "b", "ConcVolUnit"), extra = "merge", fill = "left", sep = " ")
inv_daily_df <- inv_daily_df %>% separate(b, c( "ConcDoseUnit", "ConcVolSize"), extra = "merge", fill = "left", sep = "/")
inv_daily_df <- inv_daily_df %>% mutate(ConcDoseUnit=ifelse(is.na(ConcDoseUnit), ConcVolSize, ConcDoseUnit),
                                                  ConcVolSize=ifelse(ConcVolSize=="MG", NA, ConcVolSize))



# Format concentration columns into numerics
inv_daily_df <- inv_daily_df %>% mutate(ConcDoseSize = as.numeric(str_replace(ConcDoseSize, "\\,", "")),
                                                    ConcVolSize =as.numeric(str_replace(ConcVolSize, "\\,", "")))


# Calculate normalize dose unit
inv_daily_df <- inv_daily_df %>% mutate(NormDoseSize = ifelse(is.na(ConcVolSize), ConcDoseSize, ConcDoseSize / ConcVolSize),
                                                   NormDoseUnit = paste0(ConcDoseUnit,"/",ConcVolUnit))


# Strip out inventory volume and sizes from Inventory Item Name
inv_size_pattern <- "\\,\\s[0-9]+(\\.|\\,)*[0-9]*\\s*(mL|g)"

# Extract inventory item size
inv_daily_df <- inv_daily_df %>% mutate(InvShortName = str_replace(PRD_NAME, inv_size_pattern, ""),
                                             InvSizeUnit = str_replace(str_extract(PRD_NAME, inv_size_pattern), "\\,\\s", ""))

# Replace NA with "1 Each" and fix inventory sizes with missing spaces (ie, 2mL) 
inv_daily_df <- inv_daily_df %>% mutate(InvSizeUnit = ifelse(is.na(InvSizeUnit), "1 Each", InvSizeUnit))

inv_daily_df <- inv_daily_df %>% mutate(InvSizeUnit = (str_replace(InvSizeUnit, "([0-9]+)(mL)", "\\1 \\2")))

# split InvSizeUnit into inventory item size and unit 
inv_daily_df$InvSizeUnit_split <- inv_daily_df$InvSizeUnit
inv_daily_df <- inv_daily_df  %>% separate(InvSizeUnit_split, c("InvSize", "InvUnit"), extra = "merge", fill = "left", sep = " ")

inv_daily_df <- inv_daily_df %>% mutate(InvSize=  as.numeric(as.character(InvSize)))



# Calculate total dose balance
inv_daily_df <- inv_daily_df %>% mutate(TotalDoseBalance = BALANCE * NormDoseSize * InvSize)

# Fix NDC ID and Code data alignment & NormDoseUnit = MG/ and /
inv_daily_df <- inv_daily_df %>% mutate(NDC_ID_ref = NDC_ID, NDC_CODE_ref = NDC_CODE) %>%
             mutate(NDC_ID = ifelse(str_detect(NDC_ID,"-"), NDC_CODE_ref, NDC_ID_ref), NDC_CODE = ifelse(str_detect(NDC_CODE,"-"), NDC_CODE_ref, NDC_ID_ref)) %>%
             mutate(NormDoseUnit = ifelse(NormDoseUnit == "/", NA, ifelse(NormDoseUnit == "MG/", "MG", NormDoseUnit)))

# Aggregate inventory data by site and NDC 
inv_site_summary <- inv_daily_df  %>%
  group_by(ReportDate, Site, PRD_NAME, NDC_ID, NDC_CODE,
           ConcExtract, ConcDoseSize, ConcDoseUnit, ConcVolSize, ConcVolUnit,
           NormDoseSize, NormDoseUnit,
           InvShortName, InvSizeUnit, InvSize, InvUnit) %>%
  summarize(Balance = sum(BALANCE),
            TotalDoseBalance = sum(TotalDoseBalance)) %>%
  ungroup()


inv_site_summary <- inv_site_summary %>% mutate(MedGroup= toupper(gsub("([A-Za-z]+).*", "\\1", PRD_NAME)),
                                                MedGroup= ifelse(MedGroup == "NOREPINEPHRINE",  "NOREPINEPHRINE BITARTRATE", MedGroup))
                                                                



inv_site_summary <- inv_site_summary [!duplicated(inv_site_summary), ]


# Bind today's data with repository
inv_final_repo <- rbind(inv_repo, inv_site_summary)

inv_final_repo$med_class <- covid_med_groups$Classification[match(inv_final_repo$MedGroup, covid_med_groups$`Medication Group`)]

inv_final_repo <- inv_final_repo %>% filter(med_class == "COVID")



# Save the new repo
write_xlsx(inv_final_repo, path = paste0(wrk.dir, "\\REPO\\Inv_Repo\\Covid Surge Meds Inventory Repo-", Sys.Date()-1, ".xlsx"))
rm(inv_repo, inv_site_summary, inv_daily_df)




#----------- Import and pre-process Med Admin data  --------------

#Import the latest REPO file 
med_repo <- file.info(list.files(path = paste0(wrk.dir,"/REPO/Med_Repo"), full.names = T , pattern =paste0("Covid Surge Meds Admin Repo-", dates_pattern, collapse = "|"  )))
repo_file <- rownames(med_repo)[which.max(med_repo$ctime)]
med_repo <- read_excel(repo_file)



# Import COVID Surge Med Admin data
new_med_admin_list <- file.info(list.files(path= paste0(wrk.dir,"/Med Admin - Phase2"), full.names=TRUE, pattern = paste0("COVID Surge Med Admin_", dates_pattern, collapse = "|")))
new_med_admin_list = new_med_admin_list[with(new_med_admin_list, order(as.POSIXct(ctime), decreasing = TRUE)), ]


files = rownames(new_med_admin_list)[1:count]
new_med_admin_raw <- lapply(files, function(filename){
  read_excel(filename, col_names = TRUE, na = c("", "NA"))})


new_med_admin <-  do.call(rbind.data.frame, new_med_admin_raw )

rm(new_med_admin_list, new_med_admin_raw)


new_med_admin <-  new_med_admin %>% mutate(MedGroup=toupper(gsub("([A-Za-z]+).*", "\\1", DISPINSABLE_MED_NAME)),
                                                  MedGroup=ifelse(MedGroup == "NOREPINEPHRINE",  "NOREPINEPHRINE BITARTRATE", MedGroup))

 
# Import COVID Surge Meds Administered Data
new_med_admin <- new_med_admin[!duplicated(new_med_admin), ]
new_med_admin$med_class <- covid_med_groups$Classification[match(new_med_admin$MedGroup, covid_med_groups$`Medication Group`)]

#Filter Covid data
new_med_admin <- new_med_admin %>% filter(med_class == "COVID")

##  Missing ndc id and code

common_ndc <- new_med_admin %>%
  filter(!is.na(NDC_ID)) %>%
  group_by(LOC_NAME, DISPINSABLE_MED_NAME, NDC_ID) %>%
  summarise(total = n()) %>%
  arrange(LOC_NAME, DISPINSABLE_MED_NAME, desc(total)) %>%
  mutate(count = 1:n()) %>% filter(count == 1)

colnames(common_ndc)[colnames(common_ndc) == "NDC_ID"] <- "ndc"

no_ndc <- new_med_admin %>% filter(is.na(NDC_ID))
no_ndc <- merge(no_ndc, common_ndc[,c("LOC_NAME","DISPINSABLE_MED_NAME","ndc")], 
                by.x = c("LOC_NAME","DISPINSABLE_MED_NAME"), by.y = c("LOC_NAME","DISPINSABLE_MED_NAME"))

no_ndc$NDC_ID <- no_ndc$ndc
no_ndc <- no_ndc[,1:26]

new_med_admin <-new_med_admin %>% filter(!is.na(NDC_ID))
new_med_admin <- rbind(new_med_admin, no_ndc)

rm(no_ndc)



# Calculate total daily usage by patient
# Data Exclusion Criteria: Exclude ADMIN_REASON = "Bolus from Infusion"
covid_meds_admin <- new_med_admin %>%
  filter(ADMIN_REASON != "Bolus from Infusion")

# Format columns
covid_meds_admin$Admin_Date <- as.Date(covid_meds_admin$TAKEN_DATETIME, format="%d-%m-%y")
covid_meds_admin$MAR_DOSE <- as.numeric(covid_meds_admin$MAR_DOSE)

# 2. Strip off the concentration from med name 
conc_pattern_1 <- "[0-9]+\\s(MCG/)[0-9]*\\s*(ML)" # str Pattern 1: x MCG/ML
conc_pattern_2 <- "[0-9]+\\s(MG/)[0-9]*\\s*(ML)" # str Pattern 2: x MG/ML
conc_pattern_3 <- "[0-9]+\\s(MG/)[0-9]+\\.*[0-9]*\\s*(ML)" # str Pattern 3: x MG/y ML 
conc_pattern_4 <- "[0-9]+\\s(MCG/)[0-9]+\\.*[0-9]*\\s*(ML)" # str Pattern 4: x MCG/y ML 
conc_pattern_5 <- "[0-9]+\\s(MG\\s)" # str Pattern 5: x MG
conc_pattern_6 <- "[0-9]*,*[0-9]+\\s(UNIT/)[0-9]*\\s*(ML)" # str Pattern 6: x UNITS/y ML

covid_meds_admin <- covid_meds_admin %>%
  mutate(StrExtract = ifelse(!is.na(str_extract(DISPINSABLE_MED_NAME, conc_pattern_1)), 
                             str_extract(DISPINSABLE_MED_NAME, conc_pattern_1),
                             ifelse(!is.na(str_extract(DISPINSABLE_MED_NAME, conc_pattern_2)),
                                    str_extract(DISPINSABLE_MED_NAME, conc_pattern_2),
                                    ifelse(!is.na(str_extract(DISPINSABLE_MED_NAME, conc_pattern_3)),
                                           str_extract(DISPINSABLE_MED_NAME, conc_pattern_3),
                                           ifelse(!is.na(str_extract(DISPINSABLE_MED_NAME, conc_pattern_4)),
                                                  str_extract(DISPINSABLE_MED_NAME, conc_pattern_4),
                                                  ifelse(!is.na(str_extract(DISPINSABLE_MED_NAME, conc_pattern_5)),
                                                         str_extract(DISPINSABLE_MED_NAME, conc_pattern_5), 
                                                         ifelse(!is.na(str_extract(DISPINSABLE_MED_NAME, conc_pattern_6)),
                                                                str_extract(DISPINSABLE_MED_NAME, conc_pattern_6), NA)))))))

# Replace /ML with /1 ML
covid_meds_admin <- covid_meds_admin %>%
  mutate(StrExtract = str_replace(StrExtract, "/ML", "/1 ML"))


# split StrExtract
covid_meds_admin$StrExtract_split <- covid_meds_admin$StrExtract
covid_meds_admin <- covid_meds_admin %>% separate(StrExtract_split, c('ConcDoseSize', 'ConcDoseUnit', 'ConcVolSize', 'ConcVolUnit' ), extra = "merge", fill = "right", sep = " ")


covid_meds_admin <- covid_meds_admin %>% mutate(ConcDoseSize=gsub(",","", ConcDoseSize),
                                                ConcVolSize= gsub(",","", ConcVolSize) )

covid_meds_admin <- covid_meds_admin %>% mutate(ConcDoseSize=  as.numeric(as.character(ConcDoseSize)),
                                                ConcVolSize=  as.numeric(as.character(ConcVolSize)))

# Capitalize all units
covid_meds_admin <- covid_meds_admin %>% mutate(MAR_DOSE_UNITS= toupper(MAR_DOSE_UNITS), ADMIN_UNIT= toupper(ADMIN_UNIT),
                                                 ORDER_VOLUME_UNIT= toupper(ORDER_VOLUME_UNIT), ConcDoseUnit=toupper(ConcDoseUnit),
                                                  ConcVolUnit = toupper(ConcVolUnit))

# Format to standardize units 
covid_meds_admin$ConcDoseUnit[which(covid_meds_admin$ConcDoseUnit == "UNIT")] <- "UNITS"

# Normalized doses 
covid_meds_admin <- covid_meds_admin %>% 
  mutate(NormDoseSize = ifelse(is.na(ConcVolSize), ConcDoseSize, ConcDoseSize / ConcVolSize),
         NormDoseUnit = paste0(ConcDoseUnit,"/",ConcVolUnit)) %>%
  mutate(NormDoseUnit = ifelse(NormDoseUnit == "/", NA,
                               ifelse(NormDoseUnit == "MG/", "MG", NormDoseUnit)))
covid_meds_admin$NormDoseSize <- as.numeric(covid_meds_admin$NormDoseSize)

# Normalized concentration per ml 
covid_meds_admin <- covid_meds_admin %>% mutate(NormConcPerMl = ifelse(ConcVolUnit == "ML", ConcDoseSize / ConcVolSize, ""))
covid_meds_admin$NormConcPerMl <- as.numeric(covid_meds_admin$NormConcPerMl)



# Calculate total doses administered
covid_meds_admin <- covid_meds_admin %>%
  mutate(total_doses = ifelse(MAR_DOSE_UNITS %in% c("MG","MCG","UNITS"), MAR_DOSE,
                              ifelse(ADMIN_UNIT %in% c("MG","MCG","UNITS"), ADMIN_AMOUNT, 
                                     ifelse(ADMIN_UNIT == "ML" & !is.na(ConcDoseUnit), ADMIN_AMOUNT*NormConcPerMl,""))))

covid_meds_admin$total_doses[is.na(covid_meds_admin$total_doses)] <- ""

covid_meds_admin <- covid_meds_admin %>%
  mutate(total_doses = ifelse(total_doses == "" & ORDER_VOLUME_UNIT == "ML" & ConcVolUnit == "ML" & !is.na(ConcDoseUnit),
                              ORDER_VOLUME*NormConcPerMl, total_doses))
covid_meds_admin$total_doses[is.na(covid_meds_admin$total_doses)] <- ""
covid_meds_admin$total_doses <- as.numeric(covid_meds_admin$total_doses)

# Get correct units for total doses administered 
covid_meds_admin <- covid_meds_admin %>%
  mutate(total_doses_unit = ifelse(MAR_DOSE_UNITS %in% c("MG","MCG","UNITS"), MAR_DOSE_UNITS,
                                   ifelse(ADMIN_UNIT %in% c("MG","MCG","UNITS"), ADMIN_UNIT, 
                                          ifelse(ADMIN_UNIT == "ML" & !is.na(ConcDoseUnit), ConcDoseUnit,""))))

covid_meds_admin$total_doses_unit[is.na(covid_meds_admin$total_doses_unit)] <- ""

covid_meds_admin <- covid_meds_admin %>%
  mutate(total_doses_unit = ifelse(total_doses_unit == "" & ORDER_VOLUME_UNIT == "ML" & ConcVolUnit == "ML" & !is.na(ConcDoseUnit),
                                   ConcDoseUnit, total_doses_unit))
covid_meds_admin$total_doses_unit[is.na(covid_meds_admin$total_doses_unit)] <- ""



# Site rolllup
covid_meds_admin$loc_rollup <- ""
covid_meds_admin$loc_rollup[which(covid_meds_admin$LOC_NAME %in% c("MOUNT SINAI BETH ISRAEL"))] <- "MSBI"
covid_meds_admin$loc_rollup[which(covid_meds_admin$LOC_NAME %in% c("MOUNT SINAI BROOKLYN"))] <- "MSB"
covid_meds_admin$loc_rollup[which(covid_meds_admin$LOC_NAME %in% c("MOUNT SINAI MORNINGSIDE"))] <- "MSM"
covid_meds_admin$loc_rollup[which(covid_meds_admin$LOC_NAME %in% c("MOUNT SINAI QUEENS"))] <- "MSQ"
covid_meds_admin$loc_rollup[which(covid_meds_admin$LOC_NAME %in% c("MOUNT SINAI WEST"))] <- "MSW"
covid_meds_admin$loc_rollup[which(covid_meds_admin$LOC_NAME %in% c("THE MOUNT SINAI HOSPITAL"))] <- "MSH"
covid_meds_admin$loc_rollup[which(covid_meds_admin$LOC_NAME %in% c("X_MOUNT SINAI BI BROOKLYN_DEACTIVATED"))] <- "MSB"
covid_meds_admin$loc_rollup[which(covid_meds_admin$LOC_NAME %in% c("X_MOUNT SINAI BI PETRIE_DEACTIVATED"))] <- "MSBI"
covid_meds_admin$loc_rollup[which(covid_meds_admin$LOC_NAME %in% c("X_MOUNT SINAI QUEENS HOSPITAL"))] <- "MSQ"
covid_meds_admin$loc_rollup[which(covid_meds_admin$LOC_NAME %in% c("X_MOUNT SINAI ST LUKE'S_DEACTIVATED"))] <- "MSM"
covid_meds_admin$loc_rollup[which(covid_meds_admin$LOC_NAME %in% c("X_MOUNT SINAI WEST_DEACTIVATED"))] <- "MSW"
covid_meds_admin$loc_rollup[which(covid_meds_admin$LOC_NAME %in% c("1440 MADISON AVE"))] <- "MSH"
covid_meds_admin$loc_rollup[which(covid_meds_admin$LOC_NAME %in% c("1470 MADISON AVE"))] <- "MSH"


covid_meds_admin <- covid_meds_admin %>% filter(loc_rollup != "")


# Bind today's data with repository
med_final_repo <- rbind(med_repo, covid_meds_admin)


# Save the new repo
write_xlsx(med_final_repo, path = paste0(wrk.dir, "\\REPO\\Med_Repo\\Covid Surge Meds Admin Repo-", Sys.Date()-1, ".xlsx"))




#----------- Map admin and inventory data using ndc  --------------

admin_aggregated <- med_final_repo
inventory_data <- inv_final_repo

admin_aggregated <- med_repo
inventory_data <- inv_repo



# Pull in inventory unit
admin_aggregated$PRD_NAME <- inventory_data$PRD_NAME[match(admin_aggregated$NDC_ID, inventory_data$NDC_ID)] 
admin_aggregated$ConcDoseSize_inv <- inventory_data$ConcDoseSize[match(admin_aggregated$NDC_ID, inventory_data$NDC_ID)] 
admin_aggregated$ConcDoseUnit_inv <- as.character(inventory_data$ConcDoseUnit[match(admin_aggregated$NDC_ID, inventory_data$NDC_ID)])
admin_aggregated$ConcVolSize_inv <- inventory_data$ConcVolSize[match(admin_aggregated$NDC_ID, inventory_data$NDC_ID)] 
admin_aggregated$ConcVolUnit_inv <- inventory_data$ConcVolUnit[match(admin_aggregated$NDC_ID, inventory_data$NDC_ID)] 
admin_aggregated$NormDoseSize_inv <- inventory_data$NormDoseSize[match(admin_aggregated$NDC_ID, inventory_data$NDC_ID)] 
admin_aggregated$NormDoseUnit_inv <- inventory_data$NormDoseUnit[match(admin_aggregated$NDC_ID, inventory_data$NDC_ID)] 
admin_aggregated$inv_size <- inventory_data$InvSize[match(admin_aggregated$NDC_ID, inventory_data$NDC_ID)] 
admin_aggregated$inv_unit <- toupper(inventory_data$InvUnit[match(admin_aggregated$NDC_ID, inventory_data$NDC_ID)])
admin_aggregated$TotalDosesPerInv <- admin_aggregated$NormDoseSize_inv*admin_aggregated$inv_size

admin_aggregated$NormDoseUnit_inv[which(admin_aggregated$NormDoseUnit_inv == "UNIT/ML")] <- "UNITS/ML" 
admin_aggregated$ConcDoseUnit_inv[which(admin_aggregated$ConcDoseUnit_inv == "UNIT")] <- "UNITS"

# Filter out administrations wihtout inventory mapping by ndc id
#meds_wo_inv_mapping <- admin_aggregated %>% filter(is.na(inv_unit))


admin_aggregated <- admin_aggregated %>% filter(!is.na(inv_unit))
# write_xlsx(admin_aggregated, "Admin Pre-processed Data.xlsx")





# Calculate total inventory used in count 
#units <- admin_inv_used %>% group_by(inv_unit, total_doses_unit, ConcDoseUnit_inv) %>% summarise(count = n())


# ML inventory used --------------------------------------- Based on same inventory NOT shared across multiple administrations
admin_inv_used_ml <- admin_aggregated %>% filter(inv_unit == "ML") %>%
  mutate(total_inv_used = ifelse(total_doses_unit == ConcDoseUnit_inv, ceiling(total_doses/TotalDosesPerInv),NA))

### Manual calculation of total inventory used 
admin_inv_used_ml <- admin_inv_used_ml %>%
  mutate(total_inv_used = ifelse(is.na(total_inv_used) & ADMIN_REASON == "New Bag", 1, total_inv_used)) %>%
  mutate(total_inv_used = ifelse(is.na(total_inv_used) & !is.na(ADMIN_AMOUNT), ceiling(ADMIN_AMOUNT/inv_size), total_inv_used)) %>%
  mutate(total_inv_used = ifelse(is.na(total_inv_used), 1, total_inv_used)) 


## Aggregate total inventory used by patient and date
admin_ml_aggregated <- admin_inv_used_ml %>%
  group_by(loc_rollup, Admin_Date, PAT_ID, PRD_NAME, inv_size, inv_unit) %>%
  summarise(total_inv_used = sum(total_inv_used)) %>%
  mutate(total_inv_count = ceiling(total_inv_used/inv_size))




# EACH inventory used/ Based on same inventory shared across multiple administrations
admin_aggregated$total_doses_unit[which(admin_aggregated$total_doses_unit == "")] <- NA

admin_inv_used_each <- admin_aggregated %>%
  filter(inv_unit == "EACH") %>%
  mutate(total_inv_used = ceiling(ifelse(is.na(total_doses_unit), ADMIN_AMOUNT,
                                         ifelse(total_doses_unit == ConcDoseUnit_inv, total_doses/TotalDosesPerInv, NA))))




### Manual mapping of concentration: DISPINSABLE_MED_NAME | PRD_NAME (inventory concentration)
### 1. ALTEPLASE FOR ARTERIAL AND/OR VENOUS THROMBOSIS | ALTEPLASE 50 MG INTRAVENOUS SOLUTION (50mg/50ml)
### 2. AZITHROMYCIN 500 MG IVPB PYXIS MINIBAG | AZITHROMYCIN 500 MG INTRAVENOUS SOLUTION (500mg/250ml)
### 3. remdesivir 100 mg intravenous (solution) and remdesivir 100 mg intravenous (powder) (100mg/20ml)

admin_inv_used_each <- admin_inv_used_each %>%
  mutate(total_inv_used = ifelse(is.na(total_inv_used) & DISPINSABLE_MED_NAME == "ALTEPLASE FOR ARTERIAL AND/OR VENOUS THROMBOSIS",
                                 ORDER_VOLUME/50, total_inv_used)) %>%
  mutate(total_inv_used = ifelse(is.na(total_inv_used) & DISPINSABLE_MED_NAME == "AZITHROMYCIN 500 MG IVPB PYXIS MINIBAG",
                                 ORDER_VOLUME/250, total_inv_used)) %>%
  mutate(total_inv_used = ifelse(is.na(total_inv_used) & PRD_NAME == "remdesivir 100 mg intravenous (solution)",
                                 ORDER_VOLUME/20, total_inv_used)) %>%
  mutate(total_inv_used = ifelse(is.na(total_inv_used) & PRD_NAME == "remdesivir 100 mg intravenous (powder)",
                                 ORDER_VOLUME/20, total_inv_used))

## Aggregate total inventory used by patient and date
admin_each_aggregated <- admin_inv_used_each %>%
  group_by(loc_rollup, Admin_Date, PAT_ID, PRD_NAME, inv_size, inv_unit) %>%
  summarise(total_inv_used = sum(total_inv_used)) %>%
  mutate(total_inv_count = ceiling(total_inv_used))


# ***NOT shared across administrations = 41728 vs. Shared across administrations = 41214*** 

# Merge ML and EACH datasets
inv_used_merged <- rbind(admin_ml_aggregated, admin_each_aggregated)

inv_used_site <- inv_used_merged %>%
  group_by(loc_rollup, Admin_Date, PRD_NAME) %>%
  summarise(total_inv_used = sum(total_inv_count))


#Inventory Data Pre-processing
inv_balance <- inventory_data %>%
  group_by(ReportDate, Site, MedGroup, InvShortName, InvSizeUnit, PRD_NAME) %>%
  summarise(total_balance = round(sum(Balance), 0))

inv_balance <- merge(inv_balance, inv_used_site, by.x = c("ReportDate","Site","PRD_NAME"), 
                     by.y = c("Admin_Date","loc_rollup","PRD_NAME"), all.x = TRUE)

# Format inv_balance to include all missing dates
inv_unique <- as.data.frame(unique(inv_balance[,c("Site","MedGroup","InvShortName","PRD_NAME","InvSizeUnit")]))
n <- length(seq(min(inv_balance$ReportDate), max(inv_balance$ReportDate), by = "1 day"))
inv_bal_df <- do.call("rbind", replicate(n, inv_unique, simplify = FALSE))


dates <- data.frame(ReportDate = rep(seq(min(inv_balance$ReportDate), max(inv_balance$ReportDate), by = "1 day"),
                                     each=nrow(inv_unique)))
inv_bal_df <- cbind(inv_bal_df, dates)
inv_bal_df <- inv_bal_df %>% mutate(uniqueId = paste0(ReportDate,Site,PRD_NAME))

inv_balance <- inv_balance %>%  mutate(uniqueId = paste0(ReportDate,Site,PRD_NAME))
inv_used_site <- inv_used_site %>%  mutate(uniqueId = paste0(Admin_Date,loc_rollup,PRD_NAME))

inv_bal_df$total_balance <- inv_balance$total_balance[match(inv_bal_df$uniqueId, inv_balance$uniqueId)]
inv_bal_df$total_inv_used <-inv_used_site$total_inv_used[match(inv_bal_df$uniqueId, inv_used_site$uniqueId)]

inv_bal_df[is.na(inv_bal_df)] <- 0 

# Filter our sites
inv_bal_df <- inv_bal_df %>% filter(Site %in% c("MSB","MSBI","MSH","MSHS Stockpile","MSM","MSQ","MSW"))


 
