#### Title
# PURPOSE: DRC Procured vs. Delivered
# AUTHOR: alerichardson | sch
# LICENSE: MIT
# DATE: 2022-12-14
# NOTES: 

#### LOCALS & SETUP ============================================================================


# Libraries
require(tidyverse)
require(gagglr)
require(here)
library(googledrive)
require(lubridate)

library(glitr)
library(glamr)
library(gisr)
library(Wavelength)
library(gophr)
library(tidyverse)
library(scales)
library(sf)
library(extrafont)
library(tidytext)
library(patchwork)
library(ggtext)
library(here)



load_secrets()

# Set paths  
data <- here("Data")
dataout <- here("Dataout")


## function to download perf dataset

perf_dataset <- "1h9SXDID1H2FSgWJffsfJbgi1Pyv1Em0L"

get_perf <- function(path = data){
  
  file <- googledrive::drive_ls(googledrive::as_id(perf_dataset))
  
  filename <- file %>% 
    dplyr::pull(name)
  
  glamr::import_drivefile(drive_folder = perf_dataset,
                          filename = filename,
                          folderpath = path,
                          zip = F)
}
  


#### LOAD DATA ============================================================================ 

perf_raw <- readxl::read_xlsx("Data/Performance Dataset_12.14.22.xlsx")


#  performance dataset
df_arv_drc <- perf_raw %>%
  janitor::clean_names() %>%
  filter(country == "Congo DRC",
         str_detect(item_tracer_category, "ARV"),
         order_type %in% c("Purchase Order", "Distribution Order"),
         #d365_funding_source_detail %in% c("PEPFAR-COP-USAID","PEPFAR-Condom Fund"),
         d365_health_element == "HIV/AIDS"
         ) %>% 
  select(item_tracer_category,
         item_id,
         product_name,
         ordered_quantity,
         line_total,
         shipped_quantity,
         delivery_progress,
         latest_actual_delivery_date,
         po_released_for_fulfillment_date,
         order_type) %>%
  filter(order_type != "Replenishment Order")

drc_procured = df_arv_drc %>%
  mutate(month = floor_date(po_released_for_fulfillment_date, unit = "month")) %>%
  group_by(month, product_name) %>%
  summarize(ordered_quantity = sum(ordered_quantity, na.rm = T)) %>%
  filter(month >= as_datetime("2019-01-01")) %>%
  rename("Ordered Quantity" = ordered_quantity) %>%
  mutate(month = zoo::as.yearmon(month)) %>%
  pivot_longer(cols = c("Ordered Quantity"), names_to = "Category", values_to = "Value") %>%
  pivot_wider(id_cols = c("product_name", "Category"), names_from = "month", values_from = "Value")
  
drc_procured[is.na(drc_procured)]<-0

write_csv(drc_procured, here("Dataout", "drc_procured.csv"))

drc_delivered = df_arv_drc %>%
  filter(delivery_progress == 2) %>%
  mutate(month = floor_date(latest_actual_delivery_date, unit = "month")) %>%
  group_by(month, product_name) %>%
  summarize(shipped_quantity = sum(shipped_quantity, na.rm = T)) %>%
  filter(month >= as_datetime("2019-01-01") &
           month < as_datetime("2022-12-14")) %>%
  rename("Delivered Quantity" = shipped_quantity) %>%
  mutate(month = zoo::as.yearmon(month)) %>%
  pivot_longer(cols = c("Delivered Quantity"), names_to = "Category", values_to = "Value") %>%
  pivot_wider(id_cols = c("product_name", "Category"), names_from = "month", values_from = "Value")

drc_delivered[is.na(drc_delivered)]<-0

write_csv(drc_delivered, here("Dataout", "drc_delivered.csv"))
