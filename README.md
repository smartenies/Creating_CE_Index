# Creating_CE_Index
Code used to generate the ENV, SOC, and CE indices used in the ECHO-wide study of associations between neighborhood-level environmental and social determinants of health and perinatal outcomes

**Date created:** January 11, 2019

**Author:** Sheena Martenies

**Contact:** Sheena.Martenies@colostate.edu

#### Acknowledgements

### Analytical Notes
The CEI combines data from several different sources. 
For spatial data, the most recently available data were used.

The file name suffix _AEA means that spatial data within that file (stored as WKT) has been projected to Albers Equal Area conic and have units of meters

Except for scripts numbered 00, scripts should be run in order.

Data inputs are listed in the Data folder. All raw data are organized into folders within this directory. CSV files within the Data folder have been cleaned and are used in other scripts. 

Script 03_Crime Statistics will need some work. When I did this analysis for Denver, I had point data from crimes and could aggregate to any spatial unit I liked. Crime data from the FBI is aggregated at the jurisdiction level. I have currently summarized to county level. There may be a way to get the jurisdiction shapefiles (typically municipality, but not always), however the current partial government shutdown is limiting my ability to find data on federal sites. I'll look into this further.


### Script Dictionary

- 01_Spatial Units of Analysis.R    Formats all the spatial objects needed
- 02_ACS Variables.R                Summarizes 5-year ACS variables
- 03_Crime Statistics.R             Summarizes ICPSR crime data at the county level
- 04_Hazardous Land Uses.R          Summarizes NLCD data
- 05_AQS Data Scrape.R              Scrapes AQS Data Mart
- 06_Emissions Inventory.R          Summarizes the emissions inventory from CDPHE
- 07_Toxic Releases.R               Summarizes the Toxic Release Inventory data
- 08_Traffic Variables.R            Summarizes AADT from the NHPMS dataset
- 09_Health outcome Rates.R         Calculates population-weighted hospitalziation rates
- 10_CT Environmental Exposures.R   Summarizes the built environment variables at the census tract level
- 11_CT Social Exposures.R          Summarizes social determinants of health at the census tract level
- 12_CT Air Pollution Exposures.R   Uses kriging to estimate biweekly air pollution metrics at CT centroids
- 13_Exposure Indices.R             Calculates the ENV, SOC, and CEI index values for each participant


### Data Sources
Sources for input datasets are found in the "EC0206 Data Sources for DAC.xlsx" file in the /Data directory