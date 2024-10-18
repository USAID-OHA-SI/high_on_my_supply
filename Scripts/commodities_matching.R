require(tidyverse)
require(here)
require(readxl)
require(gagglr)
require(googledrive)
require(janitor)

#--------------------------------------------------------------------------------

# Download Performance Dataset
artmis_folder <- "1h9SXDID1H2FSgWJffsfJbgi1Pyv1Em0L"
files_in_folder <- googledrive::drive_ls(path = as_id(artmis_folder))
drive_download(file = as_id(files_in_folder$id[1]),
               path = here("Data", files_in_folder$name[1]),
               overwrite = T)

perf <- read_excel(here("Data", files_in_folder$name[1]))

# Download Commodities Dataset
comm_folder <- "1-zz5r_Pf83ktnFDCjjWRXE1bBeKqagMf"
files_in_folder <- googledrive::drive_ls(path = as_id(comm_folder))
drive_download(file = as_id(files_in_folder$id[str_detect(files_in_folder$name, "Commodities_Dataset")]),
               path = here("Data", files_in_folder$name[str_detect(files_in_folder$name, "Commodities_Dataset")]),
               overwrite = T)

comm <- read_excel(here("Data", files_in_folder$name[str_detect(files_in_folder$name, "Commodities_Dataset")]))

#-------------------------------------------------------------------------------

perf_unique = perf %>%
  clean_names() %>%
  filter(
    condom_adjusted_task_order == "TO1",
    fiscal_year_funding %in% c("FY22", "FY23", "FY24"),
    !item_tracer_category %in% c("Laboratory", "COVID19")
  ) %>%
  group_by(product_category,
           product_name) %>%
  summarize()

perf_unique %>%
  write_csv(here("Dataout", "perf_distinct_products.csv"))

comm_unique = comm %>%
  filter(planning_cycle == "COP24") %>%
  group_by(major_category, commodity_item) %>%
  summarize() %>%
  filter(major_category != "Laboratory")

comm_unique %>%
  write_csv(here("Dataout", "comm_distinct_products.csv"))

matcher = perf_unique %>%
  mutate(product_name = trimws(product_name),
         performance_name = product_name,
         Performance = TRUE) %>%
  full_join(comm_unique %>%
              ungroup() %>%
              select(commodity_item) %>%
              mutate(Commodity = TRUE,
                     commodity_item = trimws(commodity_item),
                     commodity_name = commodity_item),
            by = c("product_name" = "commodity_item")) %>%
  select(-product_name)

matcher %>%
  write_csv(here("Dataout", "pvp_matches.csv"))

# 
# matcher2 = perf_dropped %>%
#   mutate(product_name = trimws(product_name),
#          performance_name = product_name,
#          Performance = TRUE) %>%
#   full_join(comm_unique %>%
#               ungroup() %>%
#               select(commodity_item) %>%
#               mutate(Commodity = TRUE,
#                      commodity_item = trimws(commodity_item),
#                      commodity_name = commodity_item),
#             by = c("product_name" = "commodity_item")) %>%
#   select(-product_name) %>%
#   filter(Performance == T)
# 
# matcher2 %>%
#   write_csv(here("Dataout", "matcher_addendum.csv"))

#-------------------------------------------------------------------------------

matches <- read_excel(here("Data", "pvp_matches.xlsx"), sheet = "pvp_matches") %>%
  select(performance_name, commodity_name)

matched_df = perf %>%
  clean_names() %>%
  filter(
    condom_adjusted_task_order == "TO1",
    fiscal_year_funding %in% c("FY22", "FY23", "FY24"),!item_tracer_category %in% c("Laboratory", "COVID19")
  ) %>%
  select(country,
         fiscal_year_funding,
         status_name,
         order_type,
         item_tracer_category,
         product_category,
         product_name,
         ordered_quantity,
         line_total
         ) %>%
  full_join(matches,
            by = c("product_name" = "performance_name"),
            na_matches = "never") %>%
  left_join(
    comm %>%
      filter(planning_cycle == "COP24") %>%
      mutate(commodity_item = trimws(commodity_item)) %>%
      select(country,
             planning_cycle,
             major_category,
             minor_category,
             commodity_item,
             other,
             total_budget,
             item_quantity
             ),
    by = c("commodity_name" = "commodity_item"),
    na_matches = "never"
  )

