# PURPOSE: This is a script that compares the planned vs procured commodities.
# AUTHOR: M.Kalnoky | OHA/SCH
# LICENSE: MIT
# DATE: 2022-09-02
# NOTES: 

# LOCALS & SETUP ============================================================================

  # Libraries
  
  library(tidyverse)
  library(readxl)
  library(googledrive)
  library(googlesheets4)
  library(purrr)

  # function to combine different sized matrix to create
  # side by side tables in Google Sheets
  
  combine.mat<-function(m1,m2,by="column"){
    nrow1<-nrow(m1);ncol1<-ncol(m1)
    nrow2<-nrow(m2);ncol2<-ncol(m2)
    if(by=="column"){
      combine<-matrix(NA,max(nrow1,nrow2),ncol1+ncol2)
      combine[1:nrow1,1:ncol1]<-m1
      combine[1:nrow2,(ncol1+1):(ncol1+ncol2)]<-m2
    }
    if(by=="row"){
      combine<-matrix(NA,nrow1+nrow2,max(ncol1,ncol2))
      combine[1:nrow1,1:ncol1]<-m1
      combine[(nrow1+1):(nrow1+nrow2),1:ncol2]<-m2
    }
    return(combine)
  }

  # Authorizations
  # request authorization from Google. If you are connecting to Google drive form R using the googledrive package for the first time
  # you can find instruction on how to gain Authorization here https://googledrive.tidyverse.org/.  
  
  drive_auth()

# LOAD DATA ============================================================================  
  
  Year = 2022
  country_name  = "Mozambique"
  
  # create temp folder to contain file downloads.  Temp files will be deleted after a normal shutdown of an R session
  # or can be deleted manually using file.remove()
  tmp_file <- tempfile()
  
  # download and import and process commodities, performance data sets using the 
  #commodities name match files from Google Drive
  
  # commodities data 
  # directory: SIKM/data/commodities
  
  
  temp_path <- drive_download(file = as_id("1C16pxCNsGR2yKxj9Md1glIQntVdtkYJz"),
                              path = tmp_file,
                              type = NULL,
                              overwrite = TRUE)
  
  df_comm <- read_delim(temp_path$local_path, 
                        delim = "\t",
                        skip_empty_rows = TRUE,
                        col_names = TRUE,
                        trim_ws = TRUE,
                        show_col_types = FALSE) 
    
  # performance data 
  # directory: SIKM/data/artmis
  
  temp_path <- drive_download(file = as_id("1i_p2n5e78oEEebEu4210JJy-czcuvDuM"),
                              path = tmp_file,
                              type = NULL,
                              overwrite = TRUE)
  
  df_perf <- read_csv(temp_path$local_path, show_col_types = FALSE) 
  
  # rtk purchased data 
  # directory: SIKM/data/RTK
  
  temp_path <- drive_download(file = as_id("1ucUZ1aydLmwFt_IDrET4oTvvkJs7X6mr"),
                              path = tmp_file,
                              type = NULL,
                              overwrite = TRUE)
  
  
  df_rtk <- read_xlsx(temp_path$local_path, sheet = "GHSCTransactionStatusallTransa") 


# MUNGE ============================================================================
  
  # product totals ordered by category
  
  df_comm_products <- df_comm %>%
    filter(implementation_year %in% Year,
           fundingagency %in% c("USAID/WCF","USAID"),
           mech_name %in% c("GHSC-PSM", "GHSC-RTK"),
           country == country_name) %>%
    mutate(`Direct Costs` = unit_price*item_quantity) %>% 
    rename(`Product Category` = major_category) %>% 
    rename(Quantity = item_quantity) %>% 
    rename(`Commodity Item` = commodity_item) %>% 
    group_by(`Product Category`, `Commodity Item`) %>%
    summarise_at(c("Direct Costs", "Quantity"), sum, na.rm = TRUE) %>%
    mutate(Category = "Planned") 
  
  
  df_perf_products <-  df_perf %>% 
    mutate(`Product Category`= recode(`Product Category`, `Laboratory Consumables`="Laboratory",
                                      `Laboratory Reagents`= "Laboratory",
                                      `Laboratory Equipment`= "Laboratory",
                                      `Voluntary Male Circumcision (VMMC) Kits` = "VMMC",
                                      `Voluntary Male Circumcision (VMMC) Supplies` = "VMMC",
                                      `HIV/AIDS Pharmaceuticals` = "ARV",
                                      `HIV Rapid Test Kits (RTKs)` = "RTK",
                                      `Female Condoms` = "Condoms and Lubricant",
                                      `Male Condoms` = "Condoms and Lubricant" ,
                                      `Personal Lubricants` = "Condoms and Lubricant", 
                                      `Essential Medicines` = "Essential Meds"
                                      )) %>%
    filter(`PO Released For Fulfillment Date Fiscal Year` %in% Year,
         Country == country_name,
         `D365 Funding Source Detail` %in% c("PEPFAR-COP-USAID","PEPFAR-Condom Fund"),
         `D365 Health Element` == "HIV/AIDS",
         `Task Order` == "TO1",
         `Order Type` %in% c("Purchase Order","Distribution Order")) %>%
    mutate(`Direct Costs` = `Unit Price`*`Ordered Quantity`) %>%
    rename(`Product Category` = `Product Category`) %>% 
    rename(`Commodity Item` = Product_Name) %>% 
    rename(Quantity = `Ordered Quantity`) %>%
    group_by(`Product Category`, `Commodity Item`) %>%  
    summarise_at(c("Direct Costs", "Quantity"), sum, na.rm = TRUE) %>%
    mutate(Category = "Procured") 
  
  df_rtk_products <-  df_rtk %>% 
    filter(`Ship-To Country` == country_name,
           Class == "Products",
           `Actual Delivery date FY` == paste0("FY",Year),
           CPT != 0) %>%
    mutate(`Direct Costs` = CPT*`Quantity (individual tests, RTKs only)`) %>% 
    rename(Quantity = `Quantity (individual tests, RTKs only)`) %>% 
    rename(`Commodity Item` = Description) %>% 
    mutate(`Product Category` = "RTKs") %>% 
    group_by(`Product Category`, `Commodity Item`) %>%  
    summarise_at(c("Direct Costs", "Quantity"), sum, na.rm = TRUE) %>%
    mutate(Category = "Procured")              
                          
  # Add RTK purchased data to performance data.
  
  df_perf_products <- rbind(df_perf_products, df_rtk_products)
  
  # combine data frames to create the machine readable file for upload
  
  products <- rbind(df_perf_products, df_comm_products)

# VIZ ============================================================================
  
  # Make pretty tables 
  
  # total costs by category remove decimals and add commas for legibility
  
  df_comm_category <- df_comm_products %>% 
    group_by(`Product Category`) %>% 
    summarise(Planned = paste0("$",formatC(sum(`Direct Costs`, na.rm = TRUE), format="f", digits=2, big.mark=",")))
  
  df_perf_category <- df_perf_products %>% 
    group_by(`Product Category`) %>% 
    summarise(Procured = paste0("$",formatC(sum(`Direct Costs`, na.rm = TRUE), format="f", digits=2, big.mark=",")))
  
  df_category_pretty <- full_join(df_comm_category, 
                                  df_perf_category, 
                                  by = "Product Category")
  
  
  # make a nice table to view products level quantities and costs.
  # add splits in between product categories for readability.
  
  products_pretty <- products %>% 
    mutate(`Direct Costs` = paste0("$",formatC(`Direct Costs`, format="f", digits=2, big.mark=","))) %>% 
    mutate(Quantity = formatC(Quantity, format="f", digits=0, big.mark=","))
    
  # get list of product categories
  
  product_categories <- unique(products$`Product Category`)
  
  # create table of planned vs procured for each category left and right respectivly
  
  products_pretty_to_split <- map(1:length(product_categories), function(x){
     
     planned <- products_pretty %>% 
      filter(`Product Category` == product_categories[x], Category == "Planned") 
      
     procured <- products_pretty %>% 
       filter(`Product Category` == product_categories[x], Category == "Procured") 
     
     # will add a space below the dataframe chunch for the final table
     
     planned[nrow(planned) + 1, ] <- NA
     procured[nrow(procured) + 1, ] <- NA
     
     planned <- planned[, c("Commodity Item", "Direct Costs", "Quantity")]
     procured <- procured[, c("Commodity Item", "Direct Costs", "Quantity")]
     
     m1 <- combine.mat(as.matrix(planned), matrix(NA, 1, 1), by = "column")
     m2 <- as.data.frame(combine.mat(m1, as.matrix(procured), by = "column"))
     m3 <- cbind(c(rep(product_categories[x], nrow(m2)-1),NA), m2)
     colnames(m3) <- c("Product Category", paste0("Planned ",colnames(planned)), 
                       "", paste0("Procured ",colnames(procured)))
     m3
  })
  
  # unlist to a dataframe
  
  df_products_pretty <- do.call(bind_rows, products_pretty_to_split)
  colnames(df_products_pretty)[5] <- ""
  
# SPINDOWN ============================================================================

  # Create a google sheets doc and upload different tables to different sheets.
  
  new_google_sheet <- gs4_create(name = paste0("planned_vs_procurred_",country_name,"_FY",Year), 
                                 sheets = c("Filters",
                                            "Total Spent By Product Category",
                                            "Total Spent By Product",
                                            "Data in Machine Readable Format"))
  
  # Put the data.frame in the spreadsheet and provide the sheet_id so it can be found.
  
  sheet_write(data=df_category_pretty, ss = new_google_sheet, sheet = "Total Spent By Product Category")
  sheet_write(data=df_products_pretty, ss = new_google_sheet, sheet = "Total Spent By Product")
  sheet_write(data=products, ss = new_google_sheet, sheet = "Data in Machine Readable Format")
  
  # Move your spreadsheet to the desired location
  
  drive_mv(file = new_google_sheet, 
           path = as_id("1bZateKAvx5j8Y5MYs3h27n9TqY0eVFiN"),
           overwrite = TRUE)
  
  
  # delete temp file
  
  file.remove(tmp_file)
  
  

