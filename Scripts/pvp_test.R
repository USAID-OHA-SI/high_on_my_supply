#### Title
# PURPOSE: Test Joining of Planned and Procured
# AUTHOR: alerichardson | sch
# LICENSE: MIT
# DATE: 2022-11-08
# NOTES: 



#### DATA WRANGLING ============================================================================
  
# Getting df_performance, which has the variables I care about
df_performance = df_artmis %>%
  select(unit_price, 
         ordered_quantity, 
         line_total, 
         fiscal_year_funding, 
         d365_funding_source, 
         d365_health_element,
         product_category,
         item_tracer_category,
         country,
         product_name)

# Limiting to Adult ARVs and summing down - update this later when we functionalize
df_perf_arv = df_performance %>%
  filter(item_tracer_category == "Adult ARV") %>%
  group_by(product_name) %>%
  summarize(ordered_quantity = sum(ordered_quantity, na.rm = T),
            line_total = sum(line_total, na.rm = T))

# Check numbers
df_perf_arv %>%
  summarize(ordered_quantity = sum(ordered_quantity, na.rm = T),
            line_total = sum(line_total, na.rm = T))

# Creating df_commodity with the variables I care about
df_commodity = df_commod %>%
  select(country,
         fundingagency,
         major_category, 
         minor_category,
         planning_cycle,
         implementation_year,
         commodity_item,
         item_quantity,
         unit_price) %>%
  mutate(total_cost = item_quantity*unit_price)

# Limiting to Adult ARVs and summing down - update later when we functionalize
df_comm_arv = df_commodity %>%
  filter(minor_category == "ARVs for Adult Treatment") %>%
  group_by(commodity_item) %>%
  summarize(item_quantity = sum(item_quantity, na.rm = T),
            total_cost = sum(total_cost, na.rm = T))

# Checking numbers
df_comm_arv %>%
  summarize(item_quantity = sum(item_quantity, na.rm = T))

# Put them together
df_arv = df_perf_arv %>%
  full_join(match_arv, by = c("product_name" = "product_name")) %>%
  left_join(df_comm_arv, by = c("match_name" = "commodity_item"))

df_arv %>%
  summarize(ordered_quantity = sum(ordered_quantity, na.rm = T),
            line_total = sum(line_total, na.rm = T),
            item_quantity = sum(item_quantity, na.rm = T))
  
#### VIZ ============================================================================

  #  

#### SPINDOWN ============================================================================
