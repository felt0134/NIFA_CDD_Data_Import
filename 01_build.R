#setup workspace

#clearworkspace
rm(list=ls())

pkgs <- c("raster",'tidyverse','data.table',
          'rstudioapi') #sp and rgdal deleted 1/4/2022
lapply(pkgs, library, character.only = TRUE) 

# Set working directory to local directory
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))

#load functions
source('02_Functions.R')

