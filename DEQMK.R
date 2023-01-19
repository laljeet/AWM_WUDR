# This code is used to generate the unreported withdrawals and corresponding coefficients for small farmers.
#  1. Firstly the deficit irrigation is used to calculate the irrigation amount for area under the threshold.
#  2. Only variation is in calculation of coefficient.


WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR"
setwd(WUDR_github)

library(tidyverse) 
library(tmap)
library(rgdal)
library(stringr)
library(Kendall)
library(plotly)
library("gridExtra")
options(scipen = 9999)


VA_counties<-readOGR(paste0(WUDR_github, "/VA_counties_sp"), layer="VA_counties")
county.codes <- read.csv(paste0(WUDR_github, "/csv_files/county_codes_census.csv")) 
county.codes_deq <- VA_counties@data[,c(1,3)]


#Load funtions and Census data
source(paste0(WUDR_github,"/Small_farmers_coeff/Functions/fn_Coefficent1.R"))
rm(absent, Year)

load(paste0(WUDR_github, "/dat_load/DEQ_data_total_nd_irr.Rdata"))

Mk_counties <- c("APPOMATTOX", "JAMES CITY", "AMELIA"
, "CARROLL"
, "CHARLOTTE"
, "FREDERICK"
, "GLOUCESTER"
, "LUNENBURG"
, "MONTGOMERY"
, "NORTHAMPTON"
, "SCOTT"
, "SUFFOLK CITY"
, "WARREN"
, "WASHINGTON"
)

County_codes_Mk <- county.codes %>% 
  filter(County_Name %in% Mk_counties)
 

DEQ_MK <- Total_deq_county %>% 
  filter(COUNTYFP %in% County_codes_Mk$County_Code)



