# SDD-GIS-Scripts
Collection of scripts used by the Statistics for Development Divison (SDD) for manipulating GIS data.

The following R scripts have been designed monitor and process spatial data collected through World Bank's Survey Solutions (SS) CAPI system. These scripts automate when possible the GIS data treatment. 

## Scripts for data monitoring.
This script ensures that the Statistical Unit (SU) code entered in the CAPI questionnaire corresponds with the dwelling location, given by the GPS coordinates (from the tablet - CAPI GPS question), within the Statistical Units framework. These scripts can be embedded in a data monitoring system to inform the Monitoring team about data quality issues related to the SU code entered.
In a nutshell the geographic validation works as a combination of a STATA do file that process the data retrieved from the server and an R script that compares the SU code entered by the interviewer with the SU code spatially determined (GPS plot vs SU framework). Apart from these, the system also detects interviews where SU codes or GPS plots are missing.

## Scripts for data cleaning and processing.
This group of scripts have been designed to process and correct SU codes and Dwelling locations, once the field work has finished. This ensures that the data collected is correctly aggregated at SU level and upwards (district, province...).

The data cleaning and processing includes the following phases:
1. Data extraction and processing from SS server using STATA do file to extract the information required.
2. Automatic processing using R script that cleans SU codes, GPS coordinates when the automatic processing is possible (this is explained in the script and is related to the tablet built in GPS accuracy ~ 30m)
3. Manual processing using QGIS for these interviews where is necessary human interpretation of the data to assess and correct the errors.
4. Last processing steps using R script to keep and rename fields (interview_key, SU names and codes, Lat, Long. This table will be merged with the final dataset.


