#### Title
# PURPOSE: Functionalized Joining of Planned and Procured
# AUTHOR: alerichardson | sch
# LICENSE: MIT
# DATE: 2022-11-09
# NOTES: 

#### Initial Wrangling ============================================================================

# Getting df_performance, which has the variables I care about
df_performance = df_artmis %>%
  select(unit_price, 
         ordered_quantity, 
         line_total, 
         fiscal_year_funding, 
         d365_funding_source_detail, 
         d365_health_element,
         product_category,
         item_tracer_category,
         country,
         product_name,
         task_order,
         order_type) %>%
  mutate(cop = (as.numeric(str_extract(fiscal_year_funding, "\\d{1,}"))-1)+2000) %>%
  mutate(product_name = case_when(
    str_detect(product_name, "Shang Ring Device") ~ "VMMC Device Shang Ring, pack of 200 devices per size",
    TRUE ~ product_name
  ))

# Creating df_commodity with the variables I care about
df_commodity = df_commod %>%
  mutate(minor_category = case_when(
    minor_category == "ARVs for Adult Treatment" ~ "Adult ARV",
    minor_category == "ARVs for PrEP" ~ "Adult ARV",
    minor_category == "ARVs for Pediatric Treatment" ~ "Pediatric ARV",
    minor_category == "ARVs for Infant Prophylaxis" ~ "Pediatric ARV",
    minor_category %in% c("Other Health Commodities VMMC",
                          "Surgical Kit",
                          "VMMC Device") ~ "VMMC",
    TRUE ~ minor_category
  )) %>%
  select(country,
         fundingagency,
         major_category, 
         minor_category,
         planning_cycle,
         implementation_year,
         commodity_item,
         item_quantity,
         unit_price,
         mech_name) %>%
  mutate(total_cost = item_quantity*unit_price,
         cop = as.numeric(str_extract(planning_cycle, "\\d{1,}"))+2000)

#### Define Function ============================================================================
  
pvp_join = function(planned, procured, mtch, cop_year, country_sel, category_sel){
  
  # Limiting by parameters and summing down
  df_perf = df_performance %>%
    filter(item_tracer_category == category_sel,
           cop == cop_year,
           country == country_sel,
           d365_funding_source_detail %in% c("PEPFAR-COP-USAID","PEPFAR-Condom Fund"),
           d365_health_element == "HIV/AIDS",
           task_order  == "TO1",
           order_type %in% c("Purchase Order","Distribution Order")) %>%
    group_by(product_name) %>%
    summarize(ordered_quantity = sum(ordered_quantity, na.rm = T),
              line_total = sum(line_total, na.rm = T))
  
  # Limiting by parameters and summing down
  df_comm = df_commodity %>%
    filter(minor_category == category_sel,
           cop == cop_year,
           country == country_sel,
           fundingagency %in% c("USAID/WCF","USAID"),
           mech_name %in% c("GHSC-PSM", "GHSC-RTK")) %>%
    group_by(commodity_item) %>%
    summarize(item_quantity = sum(item_quantity, na.rm = T),
              total_cost = sum(total_cost, na.rm = T))
  
  matches_local = matches %>%
    filter(category == category_sel)
  
  df_local = df_perf %>%
    full_join(matches_local, by = c("product_name" = "product_name")) %>%
    left_join(df_comm, by = c("match_name" = "commodity_item")) %>%
    mutate(country = country_sel,
           category = category_sel)
  
  return(df_local)
  
}

#### Using the function ============================================================================

categories = c("Adult ARV", "Pediatric ARV", "VMMC")
cop = 2021
df_pvp = data.frame()

for(country in unique(df_performance$country)){
  for(category in categories){
    temp = pvp_join(df_performance, df_commodity, mtch = matches, cop_year = 2021, country_sel = country, category_sel = category)
    df_pvp = df_pvp %>%
      bind_rows(temp)
  }
}

#write_csv(df_pvp, "df_pvp.csv")
