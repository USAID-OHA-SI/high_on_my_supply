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

# Set paths  
data <- here("Data")
dataout <- here("Dataout")
  

#### LOAD DATA ============================================================================ 

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
         list_price,
         ordered_quantity,
         line_total,
         shipped_quantity,
         delivery_value,
         delivery_progress,
         latest_actual_delivery_date,
         po_released_for_fulfillment_date,
         order_type) %>%
  filter(order_type != "Replenishment Order")

drc_procured = df_arv_drc %>%
  mutate(month = floor_date(po_released_for_fulfillment_date, unit = "month")) %>%
  group_by(month, product_name) %>%
  summarize(ordered_quantity = sum(ordered_quantity, na.rm = T),
            line_total = sum(line_total, na.rm = T)) %>%
  filter(month >= as_datetime("2019-01-01")) %>%
  rename("Ordered Costs" = line_total,
         "Ordered Quantity" = ordered_quantity) %>%
  mutate(month = zoo::as.yearmon(month)) %>%
  pivot_longer(cols = c("Ordered Quantity", "Ordered Costs"), names_to = "Category", values_to = "Value") %>%
  pivot_wider(id_cols = c("product_name", "Category"), names_from = "month", values_from = "Value")
  
drc_procured[is.na(drc_procured)]<-0

write_csv(drc_procured, here("Dataout", "drc_procured.csv"))

drc_delivered = df_arv_drc %>%
  filter(delivery_progress == 2) %>%
  mutate(month = floor_date(latest_actual_delivery_date, unit = "month")) %>%
  group_by(month, product_name) %>%
  summarize(shipped_quantity = sum(shipped_quantity, na.rm = T),
            delivery_value = sum(delivery_value, na.rm = T)) %>%
  filter(month >= as_datetime("2019-01-01") &
           month < as_datetime("2022-12-14")) %>%
  rename("Delivered Value" = delivery_value,
         "Delivered Quantity" = shipped_quantity) %>%
  mutate(month = zoo::as.yearmon(month)) %>%
  pivot_longer(cols = c("Delivered Quantity", "Delivered Value"), names_to = "Category", values_to = "Value") %>%
  pivot_wider(id_cols = c("product_name", "Category"), names_from = "month", values_from = "Value")

drc_delivered[is.na(drc_delivered)]<-0

write_csv(drc_delivered, here("Dataout", "drc_delivered.csv"))
