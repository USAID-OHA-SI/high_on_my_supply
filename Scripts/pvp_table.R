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

pvp_table = function(df_pvp, categories, filepath) {
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
  
  # Loop though countries
  for (country_sel in unique(df_pvp$country)) {
    # Limit to Country
    df_local = df_pvp %>%
      filter(country == country_sel,!is.na(line_total) |
               !is.na(total_cost)) %>%
      mutate(product_name = case_when(
        is.na(product_name) ~ match_name,!is.na(product_name) ~ product_name
      )) %>%
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
    
    # Tab Setup
    rowCounter = 1
    sheet = xlsx::createSheet(wb, sheetName = country_sel)
    rows <- createRow(sheet, 1:100)
    cells <- createCell(rows, colIndex = 1:12)
    
    # Loop through Categories
    for (category_sel in categories) {
      # Header
      setCellValue(cells[[rowCounter, 1]], category_sel)
      setCellStyle(cells[[rowCounter, 1]], titleStyle)
      rowCounter = rowCounter + 1
      
      # Columns
      setCellValue(cells[[rowCounter, 1]], "Product Name")
      setCellValue(cells[[rowCounter, 2]], "Planned Quantity")
      setCellValue(cells[[rowCounter, 3]], "Quantity Difference")
      setCellValue(cells[[rowCounter, 4]], "Procured Quantity")
      setCellValue(cells[[rowCounter, 5]], "Procured Cost")
      setCellValue(cells[[rowCounter, 6]], "Planned Cost")
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
      df_cat = df_local %>%
        filter(category == category_sel) %>%
        select(-category)
      addDataFrame(
        df_cat,
        sheet,
        col.names = F,
        row.names = F,
        startRow = rowCounter,
        colStyle = dataStyle
      )
      
      rowCounter = rowCounter + length(df_cat$product_name) + 2
      
    }
  }
  
  # Save
  saveWorkbook(wb, filepath)
}

