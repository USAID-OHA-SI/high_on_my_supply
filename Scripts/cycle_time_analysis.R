#### Title
# PURPOSE: Cycle Time Analysis
# AUTHOR: alerichardson | sch
# LICENSE: MIT
# DATE: 2024-04-24
# NOTES: 

#### LOCALS & SETUP ============================================================================

# Libraries
require(tidyverse)
require(gagglr)
require(here)
require(googledrive)
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

artmis_folder <- "1h9SXDID1H2FSgWJffsfJbgi1Pyv1Em0L"
artmis_filename <- "Performance Dataset 2024.3.25.xlsx"
downloader(artmis_folder, artmis_filename)
artmis <- read_excel(here("Data", artmis_filename))

#### Analysis ============================================================================  

# LIMIT TO DELIVERED
# Limit to things fulfilling an RO

# By product


artmis_cycle_times = artmis %>%
  filter(!is.na(`Latest Actual Delivery Date`),
         Fiscal_Year_Funding %in% c("FY22", "FY23")) %>% # Filter out anything that hasn't been delivered or befre FY22
  mutate(
    RO_Validation_End = case_when(
      !is.na(`RO Sent Sourcing RFX Event Date`) &
        `Order Type` == "Purchase Order" ~ `RO Sent Sourcing RFX Event Date`,!is.na(`RO Sent Plan Fulfillment Date`) &
        `Order Type` == "Distribution Order" ~ `RO Sent Plan Fulfillment Date`,
      TRUE ~ `RO Clarified Date`
    ),
    # Create RO_Validation_End, which follows the cycle algorithm in selecting the appropriate date based on whether the line is a PO or a DO
    RO_Validation_Time = difftime(RO_Validation_End, `Order Entry Date`, units = "days"),
    # Difference in time for segment 1
    Sourcing_Planning_Time = difftime(`Recipient Approval Date`, RO_Validation_End, units = "days"),
    # Difference in time for segment 2
    USAID_Approval_Time = difftime(`USAID Approval Date`, `Recipient Approval Date`, units = "days"),
    # Difference in time for segment 3
    Process_PO_DO_Time = difftime(`PO Released For Fulfillment Date`, `USAID Approval Date`, units = "days"),
    # Difference in time for segment 4
    Manufacture_Prepare_End = case_when(
      `Order Type` == "Distribution Order" ~ `Actual Cargo Ready Date`,
      `Order Type` == "Purchase Order" ~ `Actual Goods Available Date`
    ),
    # Create Manufacture_Prepare_End based on cycle algorithm to select appropriate date based on PO or DO
    Manufacture_Prepare_Time = difftime(
      Manufacture_Prepare_End,
      `PO Released For Fulfillment Date`,
      units = "days"
    ),
    # Difference in time for segment 5
    Pick_Up_Time = difftime(`Max Pick Up Date`, Manufacture_Prepare_End, units = "days"),
    # Difference in time for segment 6
    Deliver_Time = difftime(`Latest Actual Delivery Date`, `Max Pick Up Date`, units = "days"), 
    # Difference in time for segment 7
    Total_Cycle_Time = difftime(`Latest Actual Delivery Date`, `Order Entry Date`, units = "days") 
    # Difference in time between order entry and actual delivery date
  ) %>%
  select(
    ROPOLine,
    # RO and PO Numbers
    `Status Name`,
    # Status (should be delivered)
    `Transportation Mode`,
    # Air, lamd or sea
    `Order Type`,
    # PO or DO
    Country,
    # Country
    Fiscal_Year_Funding,
    # Fiscal year (FY22 or FY23)
    `Task Order`,
    # TO1 or TO2
    `Item Tracer Category`,
    # Category
    `Product Category`,
    # Finer Category
    `Item ID`,
    # ID Code
    Product_Name,
    # Name
    `Manufacturer Name`,
    # Manufacturer Name
    `Order Entry Date`,
    # Begin Segment 1
    `RO Sent Sourcing RFX Event Date`,
    # End Segment 1 PO
    `RO Sent Plan Fulfillment Date`,
    # End Segment 1 DO
    `RO Clarified Date`,
    # End Segment 1 Backup
    RO_Validation_End,
    # Calculated Segment 1 End
    RO_Validation_Time,
    # Segment 1 Length
    `RO Validation`,
    # Original Segment 1 Length
    `Recipient Approval Date`,
    # End Segment 2
    Sourcing_Planning_Time,
    # Segment 2 Length
    `Sourcing and Planning`,
    # Original Segment 2 Length
    `USAID Approval Date`,
    # Segment 3 End
    USAID_Approval_Time,
    # Segment 3 Length
    `USAID Approval`,
    # Original Segment 3 Length
    `PO Released For Fulfillment Date`,
    # Segment 4 End
    Process_PO_DO_Time,
    # Segment 4 Length
    `Process PO/DO`,
    # Original Segment 4 Length
    `Actual Goods Available Date`,
    # Segment 5 End PO
    `Actual Cargo Ready Date`,
    # Segment 5 End DO
    Manufacture_Prepare_End,
    # Calculated Segment 5 End
    Manufacture_Prepare_Time,
    # Segment 5 Length
    Manufacture,
    # Original Segment 5 Length
    `Max Pick Up Date`,
    # Segment 6 End
    Pick_Up_Time,
    # Segment 6 Length
    `Pick Up`,
    # Original Segment 6 Length
    `Latest Actual Delivery Date`,
    # Segment 7 End
    Deliver_Time,
    # Segment 7 Length
    Deliver,
    # Original Segment 7 Length
    Total_Cycle_Time
    # Total Cycle Time
  )

artmis_cycle_times %>% 
  write_csv(here("Dataout", "artmis_cycle_times.csv"))
