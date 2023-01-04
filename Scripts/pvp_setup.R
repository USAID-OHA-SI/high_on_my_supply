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
    data   <- here("Data")
    dataout <- here("Dataout")
    images  <- here("Images")
    graphs  <- here("Graphics")
   
    merdata <- glamr::si_path("path_msd")
    rasdata <- glamr::si_path("path_raster")
    shpdata <- glamr::si_path("path_vector")
    datim   <- glamr::si_path("path_datim")  
     
    
  # Functions  
    
    perf_path <- "1h9SXDID1H2FSgWJffsfJbgi1Pyv1Em0L"
    commod_path <- "1YqA0VutptWYs1_cvNeSacb9I7I2a3ovg"
    match_path <-  "1bZateKAvx5j8Y5MYs3h27n9TqY0eVFiN"
    rtk_path <- "1GNl2b046QBPBxw0o4z1YpKz54uCcYl4w"

    ##function to download performance dataset
    
    down_performance <- function(perf_path){
      
      file <- googledrive::drive_ls(googledrive::as_id(perf_path))
      
      perf_filename <- file %>% 
        dplyr::filter(stringr::str_detect(name, pattern = "xlsx")) %>%
        dplyr::pull(name)
      
      glamr::import_drivefile(drive_folder = perf_path,
                              filename = perf_filename,
                              folderpath = data,
                              zip = FALSE)
      return(perf_filename)
    }
    
    ##function to download commodities dataset
    down_commod <- function(commod_path) {
      
      file <- googledrive::drive_ls(googledrive::as_id(commod_path))
      
      commod_filename <- file %>% 
        dplyr::filter(stringr::str_detect(name, pattern = ".txt")) %>%
        dplyr::pull(name)
      
      glamr::import_drivefile(drive_folder = commod_path,
                              filename = commod_filename,
                              folderpath = data,
                              zip = FALSE)
      return(commod_filename)
    }
      
    # Read in matched dataset
    
    down_match <- function(match_path){
      
      file <- googledrive::drive_ls(googledrive::as_id(match_path))
      
      match_filename <- file %>% 
        dplyr::filter(stringr::str_detect(name, pattern = "planned v procured")) %>%
        dplyr::pull(name)
      
      glamr::import_drivefile(drive_folder = match_path,
                              filename = match_filename,
                              folderpath = data,
                              zip = FALSE)
      return(match_filename)
    }
    
    down_rtk <- function(rtk_path){
      
      file <- googledrive::drive_ls(googledrive::as_id(rtk_path))
      
      rtk_filename <- file %>% 
        dplyr::filter(stringr::str_detect(name, pattern = "xlsx")) %>%
        dplyr::pull(name)
      
      glamr::import_drivefile(drive_folder = rtk_path,
                              filename = rtk_filename[1],
                              folderpath = data,
                              zip = FALSE)
      return(rtk_filename[1])
    }

# LOAD DATA ============================================================================  

  perf_filename = down_performance(perf_path)
  commod_filename = down_commod(commod_path)
  match_filename = down_match(match_path)
  rtk_filename = down_rtk(rtk_path)
    
    
  perf_raw <- readxl::read_xlsx(file.path(data, perf_filename))
    
  commod_raw <- readr::read_tsv(file.path(data, commod_filename))
    
  match_arv = readxl::read_xlsx(file.path(data, paste0(match_filename,".xlsx")), sheet = "Copy of adult arv comp") %>%
    mutate(category = "Adult ARV")
  match_arv[match_arv == "NA"]<-NA

  match_ped = readxl::read_xlsx(file.path(data, paste0(match_filename,".xlsx")), sheet = "Copy of peds arv comp") %>%
    mutate(category = "Pediatric ARV")
  match_ped[match_ped == "NA"]<-NA
  
  match_vmmc = readxl::read_xlsx(file.path(data, paste0(match_filename,".xlsx")), sheet = "Copy of VMMC") %>%
    mutate(category = "VMMC")
  match_vmmc[match_vmmc == "NA"]<-NA
  
  match_test = readxl::read_xlsx(file.path(data, paste0(match_filename,".xlsx")), sheet = "Copy of Testing") %>%
    mutate(category = "RTK")
  match_test[match_test == "NA"]<-NA
  
  match_tb = readxl::read_xlsx(file.path(data, paste0(match_filename,".xlsx")), sheet = "Copy of Tuburculosis") %>%
    mutate(category = "TB")
  match_tb[match_tb == "NA"]<-NA
  
  match_condoms = readxl::read_xlsx(file.path(data, paste0(match_filename,".xlsx")), sheet = "Condoms") %>%
    mutate(category = "Condoms")
  match_condoms[match_condoms == "NA"]<-NA
  
  matches = match_arv %>%
    bind_rows(match_ped) %>%
    bind_rows(match_vmmc) %>%
    bind_rows(match_test) %>%
    bind_rows(match_tb)
  
  rtk_raw = readxl::read_xlsx(file.path(data, rtk_filename), sheet = "GHSCTransactionStatusallTransa") %>%
    select(ordered_quantity = `Quantity (kits)`,
           line_total = `Grand Total incl CR`,
           fiscal_year_funding = COP,
           Class,
           country = `Ship-To Country`,
           product_name = Description) %>%
    mutate(cop = as.numeric(str_extract(fiscal_year_funding, "\\d{1,}"))+2000) %>%
    filter(Class == "Products") %>%
    mutate(item_tracer_category = "RTK",
           d365_funding_source_detail = "PEPFAR-COP-USAID",
           d365_health_element = "HIV/AIDS",
           task_order  = "TO1",
           order_type = "Purchase Order") 
  rtk_raw$country[rtk_raw$country=="Cote D'Ivoire"]<-"Cote d'Ivoire"
  rtk_raw$country[rtk_raw$country=="Congo, DRC"]<-"DRC"
  
       
# MUNGE ============================================================================
  
  #  performance dataset
    df_artmis <- perf_raw %>%
        janitor::clean_names() %>%
        filter(condom_adjusted_task_order == "TO1",
               fiscal_year_funding == "FY22",
               order_type %in% c("Purchase Order", "Distribution Order")) %>% 
        mutate(country = case_when(country == "Côte d'Ivoire" ~ "Cote d'Ivoire",
                                   country == "Congo DRC" ~ "DRC",
                                   country == "Eswatini" ~ "eSwatini",
                                   TRUE ~ country))
    # 
    # #export list of items and categories for comparison
    # 
    # df_artmis %>% 
    # distinct(item_tracer_category, product_category, product_name) %>% 
    #   arrange(item_tracer_category, product_category, product_name) %>% 
    #   googlesheets4::write_sheet(ss = googledrive::as_id("1BiYqRkYb9iGRGZVuMtUIeQ0mj2G7fgLMnX83MQf3KqA"),
    #                              sheet = "perf dataset cats")
    # 
    # #export lab-specific reagent tabs to a NEW tab
    # df_artmis %>%
    #   filter(product_category == "Laboratory Reagents") %>% 
    #   distinct(item_tracer_category, product_category, product_name) %>% 
    #   arrange(item_tracer_category, product_category, product_name) %>% 
    #   googlesheets4::write_sheet(ss = googledrive::as_id("1BiYqRkYb9iGRGZVuMtUIeQ0mj2G7fgLMnX83MQf3KqA"),
    #                              sheet = "lab reagent comp")
    # 
    # 
  #commodities dataset
    
    
    df_commod <- commod_raw %>%
      filter(implementation_year == 2022)
    
    
    # 
    # #export list of items and categories for comparison
    # 
    # df_commod %>% 
    #   distinct(major_category, minor_category, commodity_item) %>% 
    #   arrange(major_category, minor_category, commodity_item) %>% 
    #   googlesheets4::write_sheet(ss = googledrive::as_id("1BiYqRkYb9iGRGZVuMtUIeQ0mj2G7fgLMnX83MQf3KqA"),
    #                              sheet = "commodities cats")
    # 
    # #export list of lab reagents to an existing tab for editing
    # 
    # df_commod %>%
    #   filter(minor_category %in% c("")) %>%
    #   distinct(major_category, minor_category, commodity_item) %>% 
    #   arrange(major_category, minor_category, commodity_item) %>% 
    #   googlesheets4::write_sheet(ss = googledrive::as_id("1BiYqRkYb9iGRGZVuMtUIeQ0mj2G7fgLMnX83MQf3KqA"),
    #                              sheet = "commodities cats")
    # 
    # 
    # 
    # ###scratch
    # #compare minor category from commod to product_category from perf
    # 
    # df_artmis %>% 
    #   filter(item_tracer_category == "Laboratory") %>% 
    #   distinct(product_category) %>% prinf
    # #operative category Laboratory Reagents
    # 
    # df_commod %>% 
    #   filter(major_category == "Laboratory") %>% 
    #   distinct(minor_category)
    # 
    # commod_raw %>% filter(major_category == "Laboratory" & minor_category == "VL Reagents And Consumables",
    #                       implementation_year == 2021) %>%
    #   distinct(commodity_item) %>%
    #   arrange(commodity_item) %>%
    #   prinf
    
  
