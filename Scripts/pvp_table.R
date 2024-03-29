#### Title
# PURPOSE: Presentation of PvP
# AUTHOR: alerichardson | sch
# LICENSE: MIT
# DATE: 2022-11-10
# NOTES: 

#### Packages ============================================================================

require(xlsx)

#### Tabular Presentation ============================================================================
filepath = here("Dataout", "plannedVsProcured.xlsx")

pvp_table = function(df_pvp, categories, cop, filepath) {
  # Initialize Workbook
  wb = xlsx::createWorkbook()
  
  # Basic Setup
  titleStyle = CellStyle(wb) +
    Font(wb, isBold = T)
  columnStyle = CellStyle(wb) +
    Font(wb, isBold = T) +
    Fill(backgroundColor = "#ABD6DC")
  dataStyle = CellStyle(wb) +
    Alignment(h = "ALIGN_RIGHT")
  dataDiffStyle = CellStyle(wb) +
    Alignment(h = "ALIGN_RIGHT") +
    Font(wb, color = "#FF0000")
  
  sumHolster = data.frame()
  
  # Loop though countries
  for (country_sel in sort(unique(df_pvp$country))) {
    # Limit to Country
    df_local = df_pvp %>%
      filter(country == country_sel,!is.na(line_total) |
               !is.na(total_cost)) %>%
      # mutate(product_name = case_when(
      #   is.na(product_name) ~ match_name,!is.na(product_name) ~ product_name
      # )) %>%
      mutate(
        quantity_difference = item_quantity - ordered_quantity,
        cost_difference = total_cost - line_total
      ) %>%
      select(
        product_name,
        ordered_quantity,
        item_quantity,
        quantity_difference,
        line_total,
        total_cost,
        cost_difference,
        category
      )
    
    
    df_sum = df_local %>%
      group_by(category) %>%
      summarize(ordered_quantity = sum(ordered_quantity, na.rm = T),
                item_quantity = sum(item_quantity, na.rm = T),
                line_total = sum(line_total, na.rm = T),
                total_cost = sum(total_cost, na.rm = T)) %>%
      mutate(
        quantity_difference = item_quantity - ordered_quantity,
        cost_difference = total_cost - line_total
      ) %>%
        select(
          ordered_quantity,
          item_quantity,
          quantity_difference,
          line_total,
          total_cost,
          cost_difference,
          category)
    
    temp = df_sum %>%
      mutate(country = country_sel) %>%
      select(Country = country, 
             Category = category,
             `Planned Quantity` = ordered_quantity,
             `Procured Quantity` = item_quantity,
             `Quantity Difference` = quantity_difference,
             `Planned Cost` = line_total,
             `Procured Cost` = total_cost,
             `Cost Difference` = cost_difference)
    
    sumHolster = sumHolster %>%
      bind_rows(temp)
    
    
    df_local = df_local %>%
      mutate(ordered_quantity = prettyNum(round(ordered_quantity, digits = 0), big.mark = ","),
             item_quantity = prettyNum(round(item_quantity, digits = 0), big.mark = ","),
             quantity_difference = prettyNum(round(quantity_difference, digits = 0), big.mark = ","),
             line_total = paste0("$",prettyNum(round(line_total, digits = 0), big.mark = ",")),
             total_cost = paste0("$",prettyNum(round(total_cost, digits = 0), big.mark = ",")),
             cost_difference = paste0("$",prettyNum(round(cost_difference, digits = 0), big.mark = ",")))
    
    df_local[df_local == "NA" | df_local == "$NA"]<-""

    # Tab Setup
    rowCounter = 1
    sheet = xlsx::createSheet(wb, sheetName = country_sel)
    rows <- createRow(sheet, 1:100)
    cells <- createCell(rows, colIndex = 1:12)
    
    setCellValue(cells[[rowCounter, 1]], paste0("Data from ARTMIS database [Jan 17, 2023], Panorama.com FAST database [Sept 16, 2022], and GHSC-RTK [Jan 3, 2023]"))
    
    rowCounter = rowCounter + 1
    
    setCellValue(cells[[rowCounter, 1]], paste0("COP ", cop))
    
    rowCounter = rowCounter + 2
    
    # Loop through Categories
    for (category_sel in categories) {
      
      df_cat = df_local %>%
        filter(category == category_sel) %>%
        select(-category) %>%
        arrange(desc(product_name))
      
      df_sum_cat = df_sum %>%
        filter(category == category_sel) %>%
        select(-category)
      df_sum_cat[is.na(df_sum_cat)]<-""
      
      # Header
      setCellValue(cells[[rowCounter, 1]], category_sel)
      setCellStyle(cells[[rowCounter, 1]], titleStyle)
      if(length(df_sum_cat$ordered_quantity)>0) {
        setCellValue(cells[[rowCounter, 2]], prettyNum(round(df_sum_cat$ordered_quantity, digits = 0), big.mark = ","))
        setCellValue(cells[[rowCounter, 3]], prettyNum(round(df_sum_cat$item_quantity, digits = 0), big.mark = ","))
        setCellValue(cells[[rowCounter, 4]], prettyNum(round(df_sum_cat$quantity_difference, digits = 0), big.mark = ","))
        setCellValue(cells[[rowCounter, 5]], paste0("$", prettyNum(round(df_sum_cat$line_total, digits = 0), big.mark = ",")))
        setCellValue(cells[[rowCounter, 6]], paste0("$", prettyNum(round(df_sum_cat$total_cost, digits = 0), big.mark = ",")))
        setCellValue(cells[[rowCounter, 7]], paste0("$", prettyNum(round(df_sum_cat$cost_difference, digits = 0), big.mark = ",")))
        setCellStyle(cells[[rowCounter, 2]], dataStyle)
        setCellStyle(cells[[rowCounter, 3]], dataStyle)
        setCellStyle(cells[[rowCounter, 4]], dataDiffStyle)
        setCellStyle(cells[[rowCounter, 5]], dataStyle)
        setCellStyle(cells[[rowCounter, 6]], dataStyle)
        setCellStyle(cells[[rowCounter, 7]], dataDiffStyle)
      }
      rowCounter = rowCounter + 1
      
      # Columns
      setCellValue(cells[[rowCounter, 1]], "Product Name")
      setCellValue(cells[[rowCounter, 2]], "Planned Quantity")
      setCellValue(cells[[rowCounter, 3]], "Procured Quantity")
      setCellValue(cells[[rowCounter, 4]], "Quantity Difference")
      setCellValue(cells[[rowCounter, 5]], "Planned Cost")
      setCellValue(cells[[rowCounter, 6]], "Procured Cost")
      setCellValue(cells[[rowCounter, 7]], "Cost Difference")
      setCellStyle(cells[[rowCounter, 1]], columnStyle)
      setCellStyle(cells[[rowCounter, 2]], columnStyle)
      setCellStyle(cells[[rowCounter, 3]], columnStyle)
      setCellStyle(cells[[rowCounter, 4]], columnStyle)
      setCellStyle(cells[[rowCounter, 5]], columnStyle)
      setCellStyle(cells[[rowCounter, 6]], columnStyle)
      setCellStyle(cells[[rowCounter, 7]], columnStyle)
      rowCounter = rowCounter + 1
      
      # Data
      
      addDataFrame(
        df_cat,
        sheet,
        col.names = F,
        row.names = F,
        startRow = rowCounter,
        colStyle = dataStyle
      )
      for(c in 1:length(df_cat)){
        if(names(df_cat)[c] %in% c("quantity_difference", "cost_difference")){
          for(n in rowCounter:(rowCounter+length(df_cat$product_name))){
            setCellStyle(cells[[n, c]], dataDiffStyle)
          }
        }
      }
      
      rowCounter = rowCounter + length(df_cat$product_name) + 2
      
    }
  }
  
  # Tab Setup
  sheet = xlsx::createSheet(wb, sheetName = "Summary")
  rows <- createRow(sheet, 1:100)
  cells <- createCell(rows, colIndex = 1:12)
  
  sumHolster = sumHolster %>%
    mutate(`Planned Quantity` = prettyNum(round(`Planned Quantity`, digits = 0), big.mark = ","),
         `Procured Quantity` = prettyNum(round(`Procured Quantity`, digits = 0), big.mark = ","),
         `Quantity Difference` = prettyNum(round(`Quantity Difference`, digits = 0), big.mark = ","),
         `Planned Cost` = paste0("$",prettyNum(round(`Planned Cost`, digits = 0), big.mark = ",")),
         `Procured Cost` = paste0("$",prettyNum(round(`Procured Cost`, digits = 0), big.mark = ",")),
         `Cost Difference` = paste0("$",prettyNum(round(`Cost Difference`, digits = 0), big.mark = ",")))
  
  addDataFrame(
    sumHolster,
    sheet,
    col.names = T,
    row.names = F,
    startRow = 1,
    colStyle = dataStyle
  )
  
  # Save
  saveWorkbook(wb, filepath)
}

pvp_table(df_pvp, categories, cop = 2021, filepath)
