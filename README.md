# AWM_WUDR
Data and codes used for WUDR project

DEQ Data:

The DEQ data folder contains user-reported DEQ irrigation withdrawals and scripts to download data from DEQ and process it in the correct format.

Function and Scripts:
1. fn_Coefficent: contains two functions that are designed to download USDA census data for any census year. The function performs further actions to fill in the missing data in the USDA census data. 
2. fn_Coeffiecnt also contains another function that divides the farms into Large and Small farms. In this study, 10 Acres and below were considered small farms based on reporting regulations. 

Small and Large Farm unreported: These scripts use the previously mentioned functions along with meteorological data from PRISM and Crop water demands to calculate irrigation withdrawals and compare it with DEQ reported data.

PRISM data: Scripts are used to download and process PPT data at the county level for Virginia and Calculate the effective precipitation.
dat_load: is the saved data in Rdata type for an easy load.

If you have any questions or  queries, please feel free to reach me at:  
laljeetstudy@gmail.com or 
laljeet@vt.edu

