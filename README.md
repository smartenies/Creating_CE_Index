# Creating_CE_Index
Code uesd to generate the ENV, SOC, and CE indices used in the ECHO-wide study of associations between neighborhood-level environmental and social determinants of health and perinatal outcomes

**Date created:** January 11, 2019

**Author:** Sheena Martenies

**Contact:** Sheena.Martenies@colostate.edu

#### Acknowledgements


### To Do


### Analytical Notes
The CEI combines data from several different sources. 
For spatial data, the most recently available data were used.

The file name suffix _AEA means that spatial data within that file (stored as 
WKT) has been projected to Albers Equal Area conic and have units of meters

Except for scripts numbered 00, scripts should be run in order.

Data inputs are listed in the Data folder. All raw data are organized into
folders within this directory. CSV files within the Data folder have been cleaned
and are used in other scripts. 

### Script Dictionary

- 01_Spatial Units of Analysis.R    Formats all the spatial objects needed
- 02_ACS Variables.R                Summarizes 5-year ACS variables
- 03_Crime Statistics.R             Summarizes ICPSR and Denver crime data
- 04_Hazardous Land Uses.R          Summarizes data from Ben's geodatabase
- 05_AQS Data Scrape.R              Scrapes AQS Data Mart
- 06_Monitor Concentrations.R       Daily metrics at each of the monitors in CO
- 07_Emissions Inventory.R          Summarizes the emissions inventory from CDPHE
- 08_Toxic Releases.R               Summarizes the Toxic Release Inventory data
- 09_Traffic Variables.R            Summarizes AADT from the NHPMS dataset
- 10_Health outcome Rates.R         Calculates population-weighted hospitalziation rates
- 11_CT Environmental Exposures.R   Summarizes the built environment variables at the census tract level
- 12_CT Social Exposures.R          Summarizes social determinants of health at the census tract level
- 13_CT Air Pollution Exposures.R   Uses kriging to estimate biweekly air pollution metrics at CT centroids
- 14_Exposure Indices.R             Calculates the ENV, SOC, and CEI index values for each participant


### Data Sources
Sources for input datasets are found in the "EC0206 Data Sources for DAC.xlsx" file in the /Data directory