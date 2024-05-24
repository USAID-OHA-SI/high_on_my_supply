#### Title
# PURPOSE: QAT, Artmis, and Commodities (Oh My!)
# AUTHOR: alerichardson | sch
# LICENSE: MIT
# DATE: 2024-03-25
# NOTES: 

#### LOCALS & SETUP ============================================================================

# Libraries
require(tidyverse)
require(gagglr)
require(here)
require(googledrive)
require(readxl)
library(zoo)
require(rvest)
#si_setup()

# Functions

downloader <- function(folder, file, path = here("Data")) {
  files_in_folder <- googledrive::drive_ls(googledrive::as_id(folder))
  glamr::import_drivefile(
    drive_folder = folder,
    filename = file,
    folderpath = path,
    zip = FALSE
  )
}

#### LOAD DATA ============================================================================  

drive_auth()

commodities_folder <- "1YqA0VutptWYs1_cvNeSacb9I7I2a3ovg"
commodities_filename <- "Commodities_Datasets_COP18-23_20240315.txt"
downloader(commodities_folder, commodities_filename)
commodities <- read_tsv(here("Data", commodities_filename))

artmis_folder <- "1h9SXDID1H2FSgWJffsfJbgi1Pyv1Em0L"
artmis_filename <- "Performance Dataset 2024.5.17.xlsx"
downloader(artmis_folder, artmis_filename)
artmis <- read_excel(here("Data", artmis_filename))

qat_folder <- "1Liuijz_f6sPhcRoSR0xjRw2qaNinSt39"
qat_filename <- "supply plan data ARVs 2024.4.3.xlsx"
downloader(qat_folder, qat_filename)
qat <- read_excel(here("Data", qat_filename))

mer <- getBigfoot::get_mer(path = here("C:/Users/arichardson/OneDrive - Credence Management Solutions LLC/Documents/Github/sch_misc/Data"))

sc_fact <- getBigfoot::get_scfact(download = T)
  
  
#### WRANGLE DATA ============================================================================  

### Part One (Distribution of Statuses) ===================================

qat_agg = qat %>%
  group_by(Country,
           Category,
           Product,
           `Funder / ProcAgent`,
           `Ship Status`,
           TransID) %>%
  summarize(Amount = sum(Amount, na.rm = T)) %>%
  ungroup()

qat_agg = qat_agg %>%
  mutate(productID = str_extract(Product, "\\((.*?)\\)")) %>%
  mutate(productID = str_remove_all(productID, "\\(")) %>%
  mutate(productID = str_remove_all(productID, "\\)"))

qat_matched = qat_agg %>%
  left_join(artmis, by = c("TransID" = "PO DO IO Number"))

qat_matched_ro = qt_agg %>%
  filter(str_detect(TransID, "^RO")) %>%
  left_join(artmis, by = c("TransID" = "PO DO IO Number",
                           ))

#write_csv(qat_matched, here("Dataout", "qat_matched.csv"))

artmis %>%
  filter(Fiscal_Year_Funding == "FY23",
         str_detect(`Item Tracer Category`, "ARV")) %>%
  group_by(Country,
           Fiscal_Year_Funding,
           `Status Name`) %>%
  summarize(n = n()) %>%
  #write_csv(here("Dataout", "artmis_status_distro.csv"))
  ggplot() +
  geom_bar(aes(x = Country, y = n, group = `Status Name`, fill = `Status Name`),
           stat = "identity",
           position = position_dodge()) + 
  theme(axis.text.x = element_text(angle = 45,  hjust=1))



### Part 2 (Compare QAT with commodities dataset) ====================================

country_codes = read_html("https://www.iban.com/country-codes") %>%
  html_node(xpath = "/html/body/div/div[2]/div/div/div/div/table") %>%
  html_table()

qat_year = qat %>%
  filter(`Funder / ProcAgent` %in% c("PSM", "USAID Local"),
         str_detect(Product, "Dolutegravir/Lamivudine/Tenofovir")) %>%
  mutate(
    smp = lubridate::quarter(x = `Receive Date`, with_year = TRUE, fiscal_start = 10),
    fiscal_year = paste0("FY", substr(smp, 3, 4)),
    productID = str_extract(Product, "\\((.*?)\\)"),
    productID = str_remove_all(productID, "\\("),
    productID = str_remove_all(productID, "\\)"),
    multiplier = case_when(
      str_detect(Product, "30 Tablets") ~ 1,
      str_detect(Product, "90 Tablets") ~ 3,
      str_detect(Product, "180 Tablets") ~ 6
    ),
    months_of_treatment = Amount * multiplier
  ) %>%
  group_by(Country, 
           fiscal_year, 
           Category) %>%
  summarize(months_of_treatment = sum(months_of_treatment, na.rm = T)) %>%
  ungroup() %>%
  select(country_code = Country,
         FY = fiscal_year,
         qat_category = Category,
         qat_months_of_treatment = months_of_treatment) %>%
  left_join(country_codes, by = c("country_code" = "Alpha-3 code")) %>%
  select(-country_code,
         -`Alpha-2 code`,
         Numeric)

qat_year$Country[qat_year$Country == "Congo (the Democratic Republic of the)"] <- "Congo DRC"
qat_year$Country[qat_year$Country == "Tanzania, United Republic of"] <- "Tanzania"

commodities_year = commodities %>%
  filter(str_detect(commodity_item, "Dolutegravir/Lamivudine/Tenofovir")) %>%
  left_join(read_csv(here("Data", "distinct_commodity_ARVs.csv"))) %>%
  mutate(multiplier = case_when(
    str_detect(commodity_item, "30 Tablets") ~ 1,
    str_detect(commodity_item, "90 Tablets") ~ 3,
    str_detect(commodity_item, "180 Tablets") ~ 6
  ),
  months_of_treatment = item_quantity * multiplier) %>%
  group_by(country,
           fundingagency,
           planning_cycle) %>%
  summarize(months_of_treatment = sum(months_of_treatment, na.rm = T)) %>%
  mutate(FY = paste0("FY", as.numeric(substr(planning_cycle, 4, 5)) + 1)) %>%
  select(Country = country,
         FundingAgency = fundingagency,
         FY,
         commodity_months_of_treatment = months_of_treatment)

artmis_year = artmis %>%
  mutate(productID = substr(`Item ID`, 1, 12)) %>%
  group_by(Country,
           Fiscal_Year_Funding,
           `Item Tracer Category`,
           productID,
           Product_Name) %>%
  summarize(`Ordered Quantity` = sum(`Ordered Quantity`, na.rm = T),
            `Shipped Quantity` = sum(`Shipped Quantity`, na.rm = T)) %>%
  ungroup() %>%
  filter(str_detect(`Item Tracer Category`, "ARV")) %>%
  select(Country,
         FY = Fiscal_Year_Funding,
         artmis_category = `Item Tracer Category`,
         productID,
         artmis_product = Product_Name,
         artmis_ordered_quantity = `Ordered Quantity`,
         artmis_shipped_quantity = `Shipped Quantity`)

qat_comm = qat_year %>%
  full_join(commodities_year) %>%
  mutate(Commodity = "TLD") %>%
  select(Country,
         FY,
         Commodity,
         qat_months_of_treatment,
         commodity_months_of_treatment) %>%
  arrange(Country, FY) %>%
  filter(FY %in% c("FY23", "FY24", "FY25")) %>%
  write_csv(here("Dataout", "FY_commodity_qat_TLD_mot.csv"))

qat_comm %>%
  pivot_longer(cols = c("qat_months_of_treatment", "commodity_months_of_treatment"),
               names_to = "dataset",
               values_to = "Months of Treatment") %>%
  mutate(dataset = str_remove_all(dataset, "_months_of_treatment")) %>%
  filter(FY != "FY25") %>%
  ggplot() +
  geom_bar(aes(x = Country, y = `Months of Treatment`, group = dataset, fill = dataset),
           stat = "identity",
           position = position_dodge()) +
  facet_wrap(~FY) +
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

#### Take 3 (Make a table of "Unapproved", "Not Procured", and "Approved - Undelivered") =====================================================================

# Three things! For prior fiscal years, lets have (ART) everything with a PO that 
# has not been delivered, (ART) everything without a PO (ie not approved), and 
# (QAT) without an RO or PO (planned but not procured)


artmis_nd = artmis %>%
  filter(Fiscal_Year_Funding %in% c("FY18", "FY19", "FY20", "FY21", "FY22", "FY23")) %>%
  filter((
    str_detect(ROPOLine, "PO") &
      `Status Name` != "Shipment Delivered") |
      (!str_detect(ROPOLine, "PO"))) %>%
  mutate(Category = case_when(
    str_detect(ROPOLine, "PO") ~ "Approved - Undelivered",
    TRUE ~ "Unapproved"
  )) %>%
  select(ROPOLine,
         `Task Order`,
         Country,
         `Status Name`,
         `Order Type`,
         `Transportation Mode`,
         `Item Tracer Category`,
         `Product Category`,
         `Item ID`,
         Product_Name,
         Fiscal_Year_Funding,
         `Ordered Quantity`,
         `Agreed Delivery Date`,
         `Revised Agreed Delivery Date`,
         `Latest Actual Delivery Date`) %>%
  filter(Fiscal_Year_Funding %in% c("FY18", "FY19", "FY20", "FY21", "FY22", "FY23")) %>%
  filter((
    str_detect(ROPOLine, "PO") &
      `Status Name` != "Shipment Delivered") |
      (!str_detect(ROPOLine, "PO"))) %>%
  mutate(Category = case_when(
    str_detect(ROPOLine, "PO") ~ "Approved - Undelivered",
    TRUE ~ "Unapproved"
  ))


artmis_nd_sum = artmis %>%
  group_by(Fiscal_Year_Funding, Category, Country) %>%
  summarize(n = n())

qat_nd = qat %>%
  filter(!str_detect(TransID, "RO"),
         !str_detect(TransID, "PO")) %>%
  mutate(
    smp = lubridate::quarter(x = `Receive Date`, with_year = TRUE, fiscal_start = 10),
    fiscal_year = paste0("FY", substr(smp, 3, 4))) %>%
  rename(country_code = Country) %>%
  left_join(country_codes, by = c("country_code" = "Alpha-3 code"))

qat_nd$Country[qat_nd$Country == "Congo (the Democratic Republic of the)"] <- "Congo DRC"
qat_nd$Country[qat_nd$Country == "Tanzania, United Republic of"] <- "Tanzania"
  
qat_nd = qat_nd %>%
  select(Country,
         `Status Name` = `Ship Status`,
         `Item Tracer Category` = Category,
         Product_Name = Product,
         Fiscal_Year_Funding = fiscal_year,
         `Ordered Quantity` = Amount,
         `Latest Actual Delivery Date` = `Receive Date`) %>%
  mutate(Category = "Not Procured")

qat_nd_sum = qat_nd %>%
  group_by(Fiscal_Year_Funding, Category, Country) %>%
  summarize(n = n())



artmis_nd %>%
  bind_rows(qat_nd) %>%
  write_csv(here("Dataout", "art_qat_undelivered.csv"))

artmis_nd_sum %>%
  bind_rows(qat_nd_sum) %>%
  write_csv(here("Dataout", "art_qat_undelivered_sums.csv"))
