
artmisDL <- function(down_path = here::here("Data"), redownload = T, perf_filename = "", rtk_filename = ""){
  
  if(redownload == T){
    
    # Download the Artmis dataset
    perf_path <- "1h9SXDID1H2FSgWJffsfJbgi1Pyv1Em0L"
    file <- googledrive::drive_ls(googledrive::as_id(perf_path))
    perf_filename <- file %>%
      dplyr::filter(stringr::str_detect(name, pattern = "xlsx")) %>%
      dplyr::pull(name)
    glamr::import_drivefile(
      drive_folder = perf_path,
      filename = perf_filename,
      folderpath = down_path,
      zip = FALSE
    )
    
    # Download RTKs
    rtk_path <- "1GNl2b046QBPBxw0o4z1YpKz54uCcYl4w"
    file <- googledrive::drive_ls(googledrive::as_id(rtk_path))
    rtk_filename <- file %>%
      dplyr::filter(stringr::str_detect(name, pattern = "xlsx")) %>%
      dplyr::pull(name)
    glamr::import_drivefile(
      drive_folder = rtk_path,
      filename = rtk_filename[1],
      folderpath = down_path,
      zip = FALSE
    )
    rtk_filename = rtk_filename[1]
  }
  
  # Read in Artmis
  perf_raw <- readxl::read_xlsx(here::here(down_path, perf_filename))
  
  df_artmis <- perf_raw %>%
    janitor::clean_names() %>%
    mutate(country = case_when(country == "Côte d'Ivoire" ~ "Cote d'Ivoire",
                               country == "Congo DRC" ~ "Democratic Republic of the Congo",
                               country == "DRC" ~ "Democratic Republic of the Congo",
                               country == "Eswatini" ~ "eSwatini",
                               TRUE ~ country)) %>%
    readr::write_csv(here::here(down_path, "df_artmis.csv"))
  
  return(df_artmis)
}


artmisWrangle <- function(rtk_filename = NA, RTKs = F, down_path = here::here("Data")){
  
  df_artmis = readr::read_csv(here::here(down_path, "df_artmis.csv"))
  
  df_artmis = df_artmis %>%
    filter(condom_adjusted_task_order == "TO1",
           order_type %in% c("Purchase Order", "Distribution Order")
    ) %>% 
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
           order_type,
           base_unit_multiplier,
           total_actual_freight_costs) %>%
    mutate(cop = (as.numeric(str_extract(fiscal_year_funding, "\\d{1,}"))-1)+2000) %>%
    mutate(item_tracer_category = case_when(
      item_tracer_category == "TB HIV" ~ "TB",
      TRUE ~ item_tracer_category
    )) 
  
  if(RTKs == T){
    
    rtk_raw = readxl::read_xlsx(file.path(data, rtk_filename), sheet = "GHSCTransactionStatusallTransa") %>%
      select(ordered_quantity = `Quantity (kits)`,
             cost = CPT,
             fiscal_year_funding = COP,
             Class,
             country = `Ship-To Country`,
             product_name = Description) %>%
      mutate(cop = as.numeric(str_extract(fiscal_year_funding, "\\d{1,}"))+2000,
             line_total = ordered_quantity*cost) %>%
      filter(Class == "Products") %>%
      mutate(item_tracer_category = "RTK",
             d365_funding_source_detail = "PEPFAR-COP-USAID",
             d365_health_element = "HIV/AIDS",
             task_order  = "TO1",
             order_type = "Purchase Order") 
    rtk_raw$country[rtk_raw$country=="Cote D'Ivoire"]<-"Cote d'Ivoire"
    rtk_raw$country[rtk_raw$country=="Congo, DRC"]<-"DRC"
    
    df_artmis = df_artmis %>%
      bind_rows(rtk_raw)
  }
  
  return(df_artmis)
  
}
