# PURPOSE: Munge and Analysis of artmis and SPT data
# AUTHOR: J.davis | OHA/SCH
# LICENSE: MIT
# DATE: 2022-01-25
# NOTES: Compare SPT and Artmis data

# LOCALS & SETUP ============================================================================

  # Libraries
    oha
  
  # Set paths  
    proj_paths
   
    si_paths 
    
  # Functions  
  

# LOAD DATA ============================================================================  

    #performance dataset
    df_artmisraw <- read_xlsx("C:/Users/Josh/Documents/data/artmis/performance_dataset/Performance Dataset.xlsx")
    
    #spt
    cop20_sptraw <- read_csv("C:/Users/Josh/Documents/GitHub/sch_misc/Data/spt/supply_plan_tool_ALL2020.04.23.csv")
    

# MUNGE ============================================================================
  
    #munge performance dataset for comparison with SPT
    # fields: condom_adjusted_task_order, item_tracer_category, latest_actual_delivery_date_fiscal_year
    # delivery_progress, fiscal_year_funding, order_type
    # po_released_for_fulfillment_date_fiscal_year == this means 'procured'
    #
    
    df_artmis %>% distinct(condom_adjusted_task_order)
    df_artmis %>% distinct(item_tracer_category) %>% prinf()
    df_artmis %>% distinct(latest_actual_delivery_date_fiscal_year)
    df_artmis %>% distinct(delivery_progress)
    df_artmis %>% distinct(fiscal_year_funding) 
    df_artmis %>% distinct(order_type)
    df_artmis %>% distinct(po_released_for_fulfillment_date_fiscal_year)
    df_artmis %>% distinct(po_released_for_fulfillment_date_fiscal_year, fiscal_year_funding)
    
    #munge down to obs of interest
    
    df_artmis <- df_artmisraw %>% 
      janitor::clean_names() %>%
      filter(condom_adjusted_task_order == "TO1",
             item_tracer_category == "Adult ARV",
             fiscal_year_funding == "FY21",
             order_type %in% c("Purchase Order", "Distribution Order")) %>% 
      mutate(country = case_when(country == "Côte d'Ivoire" ~ "Cote d'Ivoire",
                                 country == "Congo DRC" ~ "DRC",
                                 country == "Eswatini" ~ "eSwatini",
                                 TRUE ~ country))
    
    # #group and summarize
    # df_artmis <- df_artmis %>% 
    #   group_by(ropo_line, country, item_tracer_category, po_released_for_fulfillment_date, uom, product_name) %>% 
    #   summarise(order_total = sum(ordered_quantity, na.rm = T))
    
    #same but with no product
    df_artmis_sum <- df_artmis_tld %>%
      group_by(country, item_tracer_category) %>% 
      summarise(ordered_total = sum(ordered_quantity_converted, na.rm = T)) %>% 
      ungroup()
    
    ###munge spt
    cop20_spt <- cop20_sptraw %>% 
      janitor::clean_names() %>% 
      filter(orders>0) %>%
      mutate(cop_yr = "COP20",
             country = case_when(country %in% c("Uganda JMS", "Uganda MAUL") ~ "Uganda",
                                 TRUE ~ country)) %>%
      select(cop_yr, country, procuring_agent, item, orders, category)
    
    cop20_spt <- cop20_spt %>% 
      filter(category == "Adult ARVs",
             procuring_agent == "USAID") %>% 
      group_by(country) %>% 
      summarise(planned_orders = round(sum(orders_converted, na.rm = T,0))) %>% 
      ungroup()
    
    df_all <- df_artmis_sum %>% 
      left_join(cop20_spt) %>%
      select(-item_tracer_category) %>% 
      mutate(planned_executed = (order_total - planned_orders),
             `% diff` = round(order_total/planned_orders*100,0))
    
    df_all %>% write_csv("Dataout/planned_v_procured.csv")
    
    
    df_all %>% 
      gt() %>%
      fmt_number(
        columns = c(order_total, planned_orders, planned_executed),
        use_seps = TRUE,
        decimals = 0) %>% 
      gtsave("planned vs procured orders, COP20-FY21.png", path = "Dataout")
    
  ## take two, look at TLD only
    #spt first
    
    tld_spt <- c("Dolutegravir/Lamivudine/Tenofovir DF (TLD) 50/300/300 mg Tablet, 180 Tablets [OPTIMAL]",
                 "Dolutegravir/Lamivudine/Tenofovir DF (TLD) 50/300/300 mg Tablet, 30 Tablets",
                 "Dolutegravir/Lamivudine/Tenofovir DF (TLD) 50/300/300 mg Tablet, 90 Tablets [OPTIMAL]")
    
    cop20_spt <- cop20_sptraw %>% 
      janitor::clean_names() %>% 
      filter(orders>0,
             item %in% tld_spt) %>%
      mutate(cop_yr = "COP20",
             country = case_when(country %in% c("Uganda JMS", "Uganda MAUL") ~ "Uganda",
                                 TRUE ~ country),
             orders_converted = case_when(item == "Dolutegravir/Lamivudine/Tenofovir DF (TLD) 50/300/300 mg Tablet, 180 Tablets [OPTIMAL]" ~ (orders*6),
                                item == "Dolutegravir/Lamivudine/Tenofovir DF (TLD) 50/300/300 mg Tablet, 90 Tablets [OPTIMAL]" ~ (orders*3),
                                TRUE ~ orders)) %>%
      select(cop_yr, country, procuring_agent, item, orders, orders_converted, category)
    
    #now performance
    
    tld_art <- c("Dolutegravir/Lamivudine/Tenofovir DF 50/300/300 mg Tablet, 180 Tablets",
    "Dolutegravir/Lamivudine/Tenofovir DF 50/300/300 mg Tablet, 30 Tablets",
    "Dolutegravir/Lamivudine/Tenofovir DF 50/300/300 mg Tablet, 90 Tablets")
    
    df_artmis <- df_artmisraw %>% 
      janitor::clean_names() %>%
      filter(condom_adjusted_task_order == "TO1",
             item_tracer_category == "Adult ARV",
             fiscal_year_funding == "FY21",
             order_type %in% c("Purchase Order", "Distribution Order")) %>% 
      mutate(country = case_when(country == "Côte d'Ivoire" ~ "Cote d'Ivoire",
                                 country == "Congo DRC" ~ "DRC",
                                 country == "Eswatini" ~ "eSwatini",
                                 TRUE ~ country))
    
    #convert to 30ct equilivent
    df_artmis_tld <- df_artmis %>% 
      filter(product_name %in% tld_art) %>% 
      mutate(ordered_quantity_converted =
               case_when(product_name == "Dolutegravir/Lamivudine/Tenofovir DF 50/300/300 mg Tablet, 180 Tablets" ~ (ordered_quantity*6),
                         product_name == "Dolutegravir/Lamivudine/Tenofovir DF 50/300/300 mg Tablet, 90 Tablets" ~ (ordered_quantity*3),
                         TRUE ~ ordered_quantity))
    #summarise to country
    df_artmis_sum <- df_artmis_tld %>%
      group_by(country, item_tracer_category) %>% 
      summarise

    ## now join spt and ARTMIS
    
    df_all <- df_artmis_sum %>% 
      left_join(cop20_spt) %>%
      select(-item_tracer_category) %>% 
      mutate(planned_executed = (ordered_total - planned_orders),
             `% diff` = round(planned_executed/planned_orders*100,0))
    
    df_all %>% write_csv("Dataout/planned_v_procured.csv")
    
    ##look into SPT raw compared to comoodties dataest
    cop20_spt %>% 
      filter(procuring_agent == "USAID",
             category == "Adult ARVs",
             item == "Dolutegravir/Lamivudine/Tenofovir DF (TLD) 50/300/300 mg Tablet, 90 Tablets [OPTIMAL]") %>% 
      group_by(item) %>% 
      summarise(orders = sum(orders, na.rm = TRUE))
    
    
    df_artmisraw %>% 
      janitor::clean_names() %>%
      filter(condom_adjusted_task_order == "TO1",
             item_tracer_category == "Adult ARV",
             fiscal_year_funding == "FY21",
             order_type %in% c("Purchase Order", "Distribution Order"),
             product_name == "Dolutegravir/Lamivudine/Tenofovir DF 50/300/300 mg Tablet, 90 Tablets") %>%
      group_by(product_name) %>% 
      summarise(orders = sum(ordered_quantity, na.rm = TRUE))
      
    
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

