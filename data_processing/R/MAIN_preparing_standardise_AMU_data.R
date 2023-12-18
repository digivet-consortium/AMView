# MAIN PREPARING STANDARDISED AMU DATA 

# Script used for the example of preparing Norwegian raw data for AMU
# into standardised data that can be used as input for analyses
# by standardise scripts, apps etc. 

# The scripts are a part of deliverable 2.3  for DgiVet project.

# SETTING UP THE R ENVIRONMENT ----
rm(list = ls())

### Activate the required packages----
library(rio)
library(dplyr)
library(janitor)
library(lubridate)
library(stringr)
library(rstudioapi)
library(readxl)
library(openssl)

### Global variables----

# Set the working directory to VetReg folder in NVI database
setwd(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/.."))

# For the purpose of reporting, we only need the csv file for that particular year but we might want to combine the
# VetReg data for a few years for other purposes, so we allow for this by giving an option to enter the start and end years
# Enter the range of years for start and end but if you only want data for one year, keep the start and end as the same
start_year <- 2023
end_year <- 2023

# SOURCE THE VARIOUS SCRIPTS ----

# Import raw data for AMU 
# First we set variable to store the path of the file that needs to be imported
file <- ("data/VetReg_2023.csv")
# The path and name of the final csv that will be exported (This name is generally VetRegXXXX.csv, where XXXX is the year)
new_file <- ("transformed/VetReg_2023.csv")
# Now we need the historic data to correct the class of the new import and match the column names.
# historic_file <- ("data/standard_file.csv")
source(file = "R/import_and_standardize_raw_VetReg_data.R", encoding = "UTF-8")

# Merge raw data with medicine information (FEST data)
# data prepared from FEST data and others as described in report on deliverable 2.3.
# Script for preparation of data not included as it will always be 
#     different for different countries.
load("data/antall_Varenr_Virkestoff.rds")
load("data/Varenr_Virkestoff_unique.rds")

# Enter the name and path of the rds file you want to save the merged VetReg. 
merged_file <- "transformed/VetReg_merged_2023.rds"
# Enter the name and path of the rds file you want to save the records not in FEST (for checking) 
check_file_1 <- "transformed/Records_not_in_Fest_2023.rds"
# perform merging
source(file = "R/merging_VetReg_and_FEST.R", encoding = "UTF-8")


# Get and format AMU data from VetReg data
source(file = "R/getting_and_formatting_antibiotic_specific_data_from_VetReg.R", encoding = "UTF-8")


# Standardise AMU data for reporting
antibiotics <- readRDS("transformed/antibiotics_2023.rds")

source(file = "R/format_antibiotic_data_for_digivet.R", encoding = "UTF-8")
