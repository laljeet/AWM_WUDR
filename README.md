# AWM_WUDR
Data and codes used for WUDR project

DEQ Data:

DEQ data folder containes user reported DEQ irrigation withdrawals and scripts to download data from DEQ and process it in the correct format.

Function and Scripts:
1. fn_Coefficent: containes two functions which are designed to download USDA census data for any census year. The function performes further actions to fill the missing data in the USDA census data. 
2. fn_Coeffiecnt also contations another function which divides the farms to Large and Small farms. In this study 10 Acres and below were considered small farms based on reporting regualtions. 

Small and Large Farm unreported: These scripts use the previous mentioned functions along with meterological data from PRISM and Crop water demands to calcute irrigation withdrawls and compare it with DEQ reported data.

PRISM data: Scripts are used to download and process PPT data at county level for virginia and Calculate the effective precipitation.
dat_load: is the save datas in Rdata type for easy load.

Any questions and queries please feel free to reach me at:  
laljeetstudy@gmail.com
laljeet@vt.edu
(315-975-3413)
