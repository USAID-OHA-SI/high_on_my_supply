# PURPOSE: Munge and Analysis of planned vs procured 
# AUTHOR: jdavis | sch
# LICENSE: MIT
# DATE: 2022-10-27
# NOTES: This is to download and read in and munge performance dataset

# LOCALS & SETUP ============================================================================

  # Libraries
    library(gagglr)
    gagglr::oha_sitrep()
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
    load_secrets()
      
  
  # Set paths  
    data   <- "Data"
    dataout <- "Dataout"
    images  <- "Images"
    graphs  <- "Graphics"
   
    merdata <- glamr::si_path("path_msd")
    rasdata <- glamr::si_path("path_raster")
    shpdata <- glamr::si_path("path_vector")
    datim   <- glamr::si_path("path_datim")  
     
    
  # Functions  
    
    perf_path <- "1h9SXDID1H2FSgWJffsfJbgi1Pyv1Em0L"
    commod_path <- "1YqA0VutptWYs1_cvNeSacb9I7I2a3ovg"
    

    ##function to download performance dataset
    
    down_performance <- function(perf_path){
      
      file <- googledrive::drive_ls(googledrive::as_id(perf_path))
      
      perf_filename <- file %>% 
        dplyr::filter(stringr::str_detect(name, pattern = "xlsx")) %>%
        dplyr::pull(name)
      
      glamr::import_drivefile(drive_folder = perf_path,
                              filename = filename,
                              folderpath = data,
                              zip = FALSE)
    }
    
    ##function to download commodities dataset
    down_commod <- function(commod_path) {
      
      file <- googledrive::drive_ls(googledrive::as_id(commod_path))
      
      commod_filename <- file %>% 
        dplyr::filter(stringr::str_detect(name, pattern = ".txt")) %>%
        dplyr::pull(name)
      
      glamr::import_drivefile(drive_folder = commod_path,
                              filename = filename,
                              folderpath = data,
                              zip = FALSE)
      
    }
      

# LOAD DATA ============================================================================  

  #
    perf_raw <- readxl::read_xlsx(file.path(data, filename))
    
    commod_raw <- readr::read_tsv(file.path(data, commod_filename))
    
    
      
# MUNGE ============================================================================
  
  #  performance dataset
    df_artmis <- df_raw %>%
        janitor::clean_names() %>%
        filter(condom_adjusted_task_order == "TO1",
               fiscal_year_funding == "FY22",
               order_type %in% c("Purchase Order", "Distribution Order")) %>% 
        mutate(country = case_when(country == "Côte d'Ivoire" ~ "Cote d'Ivoire",
                                   country == "Congo DRC" ~ "DRC",
                                   country == "Eswatini" ~ "eSwatini",
                                   TRUE ~ country))
    
    #export list of items and categories for comparison
    
    df_artmis %>% 
    distinct(item_tracer_category, product_category, product_name) %>% 
      arrange(item_tracer_category, product_category, product_name) %>% 
      googlesheets4::write_sheet(ss = googledrive::as_id("1BiYqRkYb9iGRGZVuMtUIeQ0mj2G7fgLMnX83MQf3KqA"),
                                 sheet = "perf dataset cats")
    
    #export lab-specific reagent tabs to a NEW tab
    df_artmis %>%
      filter(product_category == "Laboratory Reagents") %>% 
      distinct(item_tracer_category, product_category, product_name) %>% 
      arrange(item_tracer_category, product_category, product_name) %>% 
      googlesheets4::write_sheet(ss = googledrive::as_id("1BiYqRkYb9iGRGZVuMtUIeQ0mj2G7fgLMnX83MQf3KqA"),
                                 sheet = "lab reagent comp")
    
  #commodities dataset
    
    
    df_commod <- commod_raw %>%
      filter(implementation_year == 2022)
    
    
    #export list of items and categories for comparison
    
    df_commod %>% 
      distinct(major_category, minor_category, commodity_item) %>% 
      arrange(major_category, minor_category, commodity_item) %>% 
      googlesheets4::write_sheet(ss = googledrive::as_id("1BiYqRkYb9iGRGZVuMtUIeQ0mj2G7fgLMnX83MQf3KqA"),
                                 sheet = "commodities cats")
    
    #export list of lab reagents to an exsiting tab for editing
    
    df_commod %>%
      filter(minor_category %in% c(""))
      distinct(major_category, minor_category, commodity_item) %>% 
      arrange(major_category, minor_category, commodity_item) %>% 
      googlesheets4::write_sheet(ss = googledrive::as_id("1BiYqRkYb9iGRGZVuMtUIeQ0mj2G7fgLMnX83MQf3KqA"),
                                 sheet = "commodities cats")
    
    
    
    ###scratch
    #compare minor category from commod to product_category from perf
    
    df_artmis %>% 
      filter(item_tracer_category == "Laboratory") %>% 
      distinct(product_category) %>% prinf
    #operative category Laboratory Reagents
    
    df_commod %>% 
      filter(major_category == "Laboratory") %>% 
      distinct(minor_category)
    
    commod_raw %>% filter(major_category == "Laboratory" & minor_category == "VL Reagents And Consumables",
                          implementation_year == 2021) %>%
      distinct(commodity_item) %>%
      arrange(commodity_item) %>%
      prinf
    
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================
