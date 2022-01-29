# PURPOSE: Munge and Analysis of PSM transactional data
# AUTHOR: J.davis | OHA/SCH
# LICENSE: MIT
# DATE: 2022-01-25
# NOTES: 

# LOCALS & SETUP ============================================================================

  # Libraries
    library(glitr)
    library(glamr)
    # library(gisr)
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

    
    
  
  # Set paths  
    data   <- "Data"
    dataout <- "Dataout"
    images  <- "Images"
    graphs  <- "Graphics"
   
    merdata <- glamr::si_path("path_msd")
    rasdata <- glamr::si_path("path_raster")
    shpdata <- glamr::si_path("path_vector")
    datim   <- glamr::si_path("path_datim")
    artmis  <- "C:/Users/Josh/Documents/data/artmis"
      

  # Functions  
  

# LOAD DATA ============================================================================  


