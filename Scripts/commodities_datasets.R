# PURPOSE: Munge and Analysis of Commodities datasets
# AUTHOR: J.davis | OHA/SCH
# LICENSE: MIT
# DATE: 2022-01-27
# NOTES: read in quarterly commodities datasets to plot trends in $ and amounts

# LOCALS & SETUP ============================================================================

  # Libraries
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
    library(readxl)
    library(webshot)
    library(gt)
    
    
  
  # Set paths  
    data   <- "Data"
    dataout <- "Dataout"
    images  <- "Images"
    graphs  <- "Graphics"
   
    merdata <- glamr::si_path("path_msd")
    rasdata <- glamr::si_path("path_raster")
    shpdata <- glamr::si_path("path_vector")
    datim   <- glamr::si_path("path_datim")
    commod  <- "C:/Users/Josh/Documents/data/money"
     
    
  # Functions  
    pal <- RColorBrewer::brewer.pal(5, "Spectral")[2:5] ## we'll come back to this
    

# LOAD DATA ============================================================================  

  #read in the five datasets seperately
    
    fy20q4_commod <- read_tsv(file.path(commod, "Commodities_Dataset_COP18-20_20201218_v2_1.txt")) %>% 
      mutate(source = "fY20Q4")
    
    fy21q1_commod <- read_tsv(file.path(commod, "Commodities_Dataset_COP18-20_20210319.txt")) %>% 
      mutate(source = "FY21Q1")
    
    fy21q2_commod <- read_tsv(file.path(commod, "Commodities_Dataset_COP18-20_20210618.txt")) %>% 
      mutate(source = "FY21Q2")
    
    fy21q3_commod <- read_tsv(file.path(commod, "Commodities_Datasets_COP18-21_20210917.txt")) %>% 
      mutate(source = "FY21Q3")
    
    fy21q4_commod <- read_tsv(file.path(commod, "Commodities_Datasets_COP18-21_20211112.txt")) %>% 
      mutate(source = "FY21Q4")

# MUNGE ============================================================================
  
  #  bind all the dfs together
    
    
  df_commd_all <- fy21q4_commod %>% 
      bind_rows(fy21q3_commod, fy21q2_commod, fy21q1_commod, fy20q4_commod)
    
    df_commd_all %>% distinct(source, planning_cycle, implementation_year) %>%
      arrange(source, planning_cycle, implementation_year)
    
    
  #some exploration
    
    df_commd_all %>% distinct(source, planning_cycle, implementation_year) %>%
      arrange(source, planning_cycle, implementation_year)
    
    tld <- c("Dolutegravir/Lamivudine/Tenofivir 50/300/300 mg Tablet 180 Tablets",                                
             "Dolutegravir/Lamivudine/Tenofivir 50/300/300 mg Tablet 30 Tablets",                                 
             "Dolutegravir/Lamivudine/Tenofivir 50/300/300 mg Tablet, 30 Tablets",                                
             "Dolutegravir/Lamivudine/Tenofovir DF (TLD) 50/300/300 mg Tablet, 180 Tablets",                      
             "Dolutegravir/Lamivudine/Tenofovir DF (TLD) 50/300/300 mg Tablet, 180 Tablets [OPTIMAL]",            
             "Dolutegravir/Lamivudine/Tenofovir DF (TLD) 50/300/300 mg Tablet, 90 Tablets",                       
             "Dolutegravir/Lamivudine/Tenofovir DF (TLD) 50/300/300 mg Tablet, 90 Tablets [OPTIMAL]",             
             "Dolutegravir/Lamivudine/Tenofovir DF 50/300/300 mg Tablet, 180 Tablets [OPTIMAL]",                  
             "Dolutegravir/Lamivudine/Tenofovir DF 50/300/300 mg Tablet, 30 Tablets [OPTIMAL]",                   
             "Dolutegravir/Lamivudine/Tenofovir DF 50/300/300 mg Tablet, 90 Tablets [OPTIMAL]")
    
    df_commd_all %>% 
      filter(fundingagency == "USAID",
             major_category == "ARV",
             commodity_item %in% tld) %>% 
      group_by(source, planning_cycle, country) %>% 
      summarise(orders = sum(item_quantity, na.rm = TRUE)) %>% 
      write_csv("Dataout/commod_rollup_ou.csv")
    
    #create tables for item and budget
    
    #items first
    df_commd_all %>% 
      filter(fundingagency == "USAID",
             major_category == "ARV",
             commodity_item %in% tld) %>%
      mutate(item_price = item_quantity*unit_price,
             freight = item_quantity*global_freight,
             modified_budget = (item_price + freight)) %>% 
      group_by(source, planning_cycle, country) %>% 
      summarise(across(c(item_quantity, modified_budget), sum, na.rm = T)) %>% 
      ungroup() %>%
      filter(planning_cycle == "COP20") %>%
      select(-planning_cycle, -modified_budget) %>% 
      pivot_wider(names_from = source,
                  values_from = c(item_quantity)) %>% 
      mutate(difference = (FY21Q4-fY20Q4)) %>% 
      filter(difference !=0) %>% 
      gt() %>%
      cols_align(align = c("center"),
                 columns = c(fY20Q4:difference)) %>% 
      fmt_number(columns = c(fY20Q4:difference),
                 use_seps = T,
                 decimals = 0) %>% 
      # tab_options(table.font.names = "Source Sans Pro") %>% 
      cols_width(c(country) ~ px(140),
                 everything() ~ px(80)) %>%
      tab_style(style = cell_fill(color = pal[1]),
                locations = cells_body(
                  columns = c(fY20Q4))) %>%   
      tab_style(style = cell_fill(color = pal[2]),
                locations = cells_body(
                  columns = c(FY21Q4))) %>% 
      tab_style(style = cell_fill(color = pal[3]),
                locations = cells_body(
                  columns = c(difference))) %>% 
      tab_header(title = "Comparison of budgeted quantities of TLD90 across time",
                 subtitle = "TLD90 item quantities across the versions of the Commodities Dataset, FY20Q4-FY21Q4") %>% 
  tab_source_note("source: Commodities datasets, downloaded from Panorama")
      
      
    #now budgets
    df_commd_all %>% 
      filter(fundingagency == "USAID",
             major_category == "ARV",
             commodity_item %in% tld) %>%
      mutate(item_price = item_quantity*unit_price,
             freight = item_quantity*global_freight,
             modified_budget = (item_price + freight)) %>% 
      group_by(source, planning_cycle, country) %>% 
      summarise(across(c(item_quantity, modified_budget), sum, na.rm = T)) %>% 
      ungroup() %>%
      filter(planning_cycle == "COP20") %>%
      select(-planning_cycle, -item_quantity) %>% 
      pivot_wider(names_from = source,
                  values_from = c(modified_budget)) %>% 
      mutate(difference = (FY21Q4-fY20Q4)) %>% 
      filter(difference !=0) %>% 
      gt() %>%
      cols_align(align = c("center"),
                 columns = c(fY20Q4:difference)) %>% 
      fmt_currency(columns = c(fY20Q4:difference),
                 use_seps = T,
                 decimals = 0,
                 accounting = T) %>% 
      # tab_options(table.font.names = "Source Sans Pro") %>% 
      cols_width(c(country) ~ px(140),
                 everything() ~ px(100)) %>%
      tab_style(style = cell_fill(color = pal[1]),
                locations = cells_body(
                  columns = c(fY20Q4))) %>%   
      tab_style(style = cell_fill(color = pal[2]),
                locations = cells_body(
                  columns = c(FY21Q4))) %>% 
      tab_style(style = cell_fill(color = pal[3]),
                locations = cells_body(
                  columns = c(difference))) %>% 
      tab_header(title = "Comparison of budgeted $ amounts for TLD90 across time",
                 subtitle = "TLD90 budgeted totals(item+freight) across the versions of the Commodities Dataset, FY20Q4-FY21Q4") %>% 
      tab_source_note("source: Commodities datasets, downloaded from Panorama")
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

