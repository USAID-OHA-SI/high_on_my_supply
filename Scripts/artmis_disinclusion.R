#### Title
# PURPOSE: Artmis Disinclusion Criteria
# AUTHOR: alerichardson | sch
# LICENSE: MIT
# DATE: 2024-05-14
# NOTES: 

#### LOCALS & SETUP ============================================================================

# Libraries
require(tidyverse)
require(gagglr)
require(here)
require(googledrive)
require(readxl)
#si_setup()

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

# artmis_folder <- "1h9SXDID1H2FSgWJffsfJbgi1Pyv1Em0L"
# artmis_filename <- "Performance Dataset 2024.5.3.xlsx"
# downloader(artmis_folder, artmis_filename)
artmis <- read_excel(here("Data", artmis_filename))

#### ANALYSIS ============================================================================  

# Are there cases where ADD == 2099?
## Yes and many of them are marked delivered

artmis %>%
  filter(`Agreed Delivery Date`>=as.Date("2098-12-31")) %>%
  view()
  
# Are there RADDs >= 2026?
## No. There's one with an ADD in 2026 but it was revised to 2024

artmis %>%
  filter((`Agreed Delivery Date`<=as.Date("2098-12-31") &
         `Agreed Delivery Date`>=as.Date("2026-1-1")) | 
           (`Revised Agreed Delivery Date`<=as.Date("2098-12-31") &
            `Revised Agreed Delivery Date`>=as.Date("2026-1-1"))) %>%
  view()

# How many lines does this apply to (esp > 250 days out)?
## 79 in 2025 total, 37 more than 250 days out


artmis %>%
  filter((`Agreed Delivery Date`< as.Date("2026-1-1") &
            `Agreed Delivery Date`>= as.Date("2025-1-1")) | 
           (`Revised Agreed Delivery Date`< as.Date("2026-1-1") &
              `Revised Agreed Delivery Date`>= as.Date("2025-1-1"))) %>%
  view()

artmis %>%
  filter((`Agreed Delivery Date`< as.Date("2026-1-1") &
            `Agreed Delivery Date`>= as.Date("2025-1-29")) | 
           (`Revised Agreed Delivery Date`< as.Date("2026-1-1") &
              `Revised Agreed Delivery Date`>= as.Date("2025-1-29"))) %>%
  view()

# Looking only at TO1, how does this break down for categories?
## See tables

artmis %>%
  filter((`Agreed Delivery Date`< as.Date("2026-1-1") &
            `Agreed Delivery Date`>= as.Date("2025-1-29")) | 
           (`Revised Agreed Delivery Date`< as.Date("2026-1-1") &
              `Revised Agreed Delivery Date`>= as.Date("2025-1-29"))) %>%
  filter(`Task Order` == "TO1") %>%
  group_by(Country, `Transportation Mode`, `Item Tracer Category`, `Status Name`) %>%
  summarize(n = n()) %>%
  view()

artmis %>%
  filter((`Revised Agreed Delivery Date`< as.Date("2026-1-1") &
              `Revised Agreed Delivery Date`>= as.Date("2025-1-29"))) %>%
  filter(`Task Order` == "TO1") %>%
  group_by(Country, `Transportation Mode`, `Item Tracer Category`, `Status Name`) %>%
  summarize(n = n()) %>%
  view()


artmis %>%
  filter(`Task Order` == "TO1",
         !str_detect(`RO Number`, "Replenishment"),
         str_detect(ROPOLine, "PO"),
         `Revised Agreed Delivery Date`<= as.Date("2023-12-16"),
         str_detect(`Line Delivery Status`, "Undelivered")) %>%
  group_by(Country, `Transportation Mode`, `Item Tracer Category`, `Status Name`) %>%
  summarize(n = n()) %>%
  view()

#### OUTPUT ====================================================================

# RADD in 2099 and != ADD
## Note - it looks like some of these were delivered

artmis %>%
  filter(`Task Order` == "TO1",
         !str_detect(`RO Number`, "Replenishment"),
         `Revised Agreed Delivery Date`>=as.Date("2098-12-31") &
           `Revised Agreed Delivery Date` != `Agreed Delivery Date`) %>%
  write_csv(here("Dataout", "Artmis_RADD2099.csv"))

# "PO exists & RADD < Current Month - 6 & Status CONTAINS Undelivered"

artmis %>%
  filter(`Task Order` == "TO1",
         !str_detect(`RO Number`, "Replenishment"),
         str_detect(ROPOLine, "PO"),
         `Revised Agreed Delivery Date`<=as.Date("2023-12-03"),
         str_detect(`Line Delivery Status`, "Undelivered")) %>%
  write_csv(here("Dataout", "Artmis_PO_6MO_UND.csv"))

# "PO does not exist & RADD < Current Month - 1"

artmis %>%
  filter(`Task Order` == "TO1",
         !str_detect(`RO Number`, "Replenishment"),
         !str_detect(ROPOLine, "PO"),
         `Revised Agreed Delivery Date`<=as.Date("2024-4-03")) %>%
  write_csv(here("Dataout", "Artmis_NOPO_1MO.csv"))

# "RADD Blank & ADD Blank & Order # Starts w/ PO"

artmis %>%
  filter(`Task Order` == "TO1",
         !str_detect(`RO Number`, "Replenishment"),
         is.na(`Revised Agreed Delivery Date`),
         str_detect(ROPOLine, "PO")) %>%
  write_csv(here("Dataout", "Artmis_NOADD_PO.csv"))
