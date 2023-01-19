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

###############################################################################
# C_tot Coefficient as function of total withdrawals
# Crop water demand (20 inches)
small_counties_coefficient3 <- function(Year){
  QS_data(Year) 
  fn_Area_TH(10, Year)
  
  # rm(bin.char, binned_irrigated_area, binned_operations,nass_binned,nass.ag.data, nass.ops.data, opeartions_list) # Not required for further calculations.
  
  
  Non.Reported_Coefficient1 <- Non.Reported
  
  Non.Reported_Coefficient1 <- left_join(Non.Reported_Coefficient1, county.codes, by = c("County" = "County_Name"))
  
  ##################################################################################################################################
  # Load DEQ data   
  
  load(paste0(WUDR_github, "/dat_load/DEQ_data_total_nd_irr.Rdata"))
  
  
  
  # Total_deq_counties are the sum of  NON-POWER withdrawals for each census year at county level
  # Irri_deq_counties are the sum of  Irrigation withdrawals for each census year at county level
  
  
  IRR_DEQ_withdarwals <- Irri_deq_county %>% 
    filter(YEAR == Year) %>% 
    filter(Facility_withdrawal_mg >0)
  
  Total_DEQ_withdarwals <- Total_deq_county %>% 
    filter(YEAR == Year) %>% 
    filter(Facility_withdrawal_mg >0)
  Unreported_Under_TH <- left_join(Non.Reported_Coefficient1,Total_DEQ_withdarwals[, c("COUNTYFP","Facility_withdrawal_mg")], by = c("County_Code"= "COUNTYFP"))
  
  Unreported_Under_TH <- Unreported_Under_TH %>%
    drop_na(Facility_withdrawal_mg)
  
  
  load(paste0(WUDR_github,"/dat_load/June_August_Effective precipitation.RData"))
  
  if (Year == 2002) {
    i = 1}else if (Year == 2007){
      i = 2} else if (Year == 2012){
        i = 3} else if (Year == 2017){
          i =4
        }
  
  
  Irr_deficit <- ppt_list_yearly[[i]][,c(2,3)]
  Irr_deficit$Irrigation <- round(508 - Irr_deficit$PPT,0) # considering 20 inches as crop water demand
  
  Unreported_Under_TH <- left_join(Unreported_Under_TH, Irr_deficit, by = c("County"= "name"))
  
  Unreported_Under_TH$Vol_Unreported <- round(Unreported_Under_TH$Irr.Area.Under.TH*(Unreported_Under_TH$Irrigation/25.5)*27154/1000000,2)
  
  Unreported_Under_TH$C_tot = Unreported_Under_TH$Vol_Unreported / Unreported_Under_TH$Facility_withdrawal_mg 
  
  Unreported_Under_TH$Method1_Unreported <- Unreported_Under_TH$C_tot * Unreported_Under_TH$Facility_withdrawal_mg
  
  Unreported_Under_TH$Method1_Unreported_AF <- Unreported_Under_TH$Method1_Unreported*3.068
  Unreported_Under_TH <- Unreported_Under_TH[,c(1,8,2,4,9,10:15)]
 
  return(Unreported_Under_TH)
}

Tdeq_coef_2002 <- small_counties_coefficient3(2002)
Tdeq_coef_2007 <- small_counties_coefficient3(2007)
Tdeq_coef_2012 <- small_counties_coefficient3(2012)
Tdeq_coef_2017 <- small_counties_coefficient3(2017)


###############################################################
SF_plot_fn <- function(Year){
  QS_data(Year) 
  fn_Area_TH(10, Year)
  
  # rm(bin.char, binned_irrigated_area, binned_operations,nass_binned,nass.ag.data, nass.ops.data, opeartions_list) # Not required for further calculations.
  
  
  Non.Reported_Coefficient1 <- Non.Reported
  
  Non.Reported_Coefficient1 <- left_join(Non.Reported_Coefficient1, county.codes, by = c("County" = "County_Name"))
  
  ##################################################################################################################################
  # Load DEQ data   
  
  load(paste0(WUDR_github, "/dat_load/DEQ_data_total_nd_irr.Rdata"))
  
  
  
  # Total_deq_counties are the sum of  NON-POWER withdrawals for each census year at county level
  # Irri_deq_counties are the sum of  Irrigation withdrawals for each census year at county level
  
  
  IRR_DEQ_withdarwals <- Irri_deq_county %>% 
    filter(YEAR == Year) %>% 
    filter(Facility_withdrawal_mg >0)
  
  Total_DEQ_withdarwals <- Total_deq_county %>% 
    filter(YEAR == Year) %>% 
    filter(Facility_withdrawal_mg >0)
  Unreported_Under_TH <- left_join(Non.Reported_Coefficient1,Total_DEQ_withdarwals[, c("COUNTYFP","Facility_withdrawal_mg")], by = c("County_Code"= "COUNTYFP"))
  
  Unreported_Under_TH <- Unreported_Under_TH %>%
    drop_na(Facility_withdrawal_mg)
  
  
  load(paste0(WUDR_github,"/dat_load/June_August_Effective precipitation.RData"))
  
  if (Year == 2002) {
    i = 1}else if (Year == 2007){
      i = 2} else if (Year == 2012){
        i = 3} else if (Year == 2017){
          i =4
        }
  
  
  Irr_deficit <- ppt_list_yearly[[i]][,c(2,3)]
  Irr_deficit$Irrigation <- round(508 - Irr_deficit$PPT,0) # considering 20 inches as crop water demand
  
  Unreported_Under_TH <- left_join(Unreported_Under_TH, Irr_deficit, by = c("County"= "name"))
  
  Unreported_Under_TH$Vol_Unreported <- round(Unreported_Under_TH$Irr.Area.Under.TH*(Unreported_Under_TH$Irrigation/25.5)*27154/1000000,2)
  
  Unreported_Under_TH$C_tot = Unreported_Under_TH$Vol_Unreported / Unreported_Under_TH$Facility_withdrawal_mg 
  
  Unreported_Under_TH$Method1_Unreported <- Unreported_Under_TH$C_tot * Unreported_Under_TH$Facility_withdrawal_mg
  
  Unreported_Under_TH$Method1_Unreported <- Unreported_Under_TH$Method1_Unreported * 3.068
  
  # Unreported_Under_TH <- Unreported_Under_TH %>% 
  #   filter(C_tot >= 0)
  
  # Get the DEQ irrigation withdrawals missing counties
  
  MISSING_withdrawals <- left_join(Non.Reported_Coefficient1,IRR_DEQ_withdarwals[, c("COUNTYFP","Facility_withdrawal_mg")], by = c("County_Code"= "COUNTYFP"))
  
  MISSING_withdrawals <- MISSING_withdrawals[is.na(MISSING_withdrawals$Facility_withdrawal_mg),]
  
  MISSING_withdrawals <- MISSING_withdrawals[,c(1,8)]
  
  # Unreported_Under_TH <- left_join(MISSING_withdrawals, Unreported_Under_TH, by = c("County_Code"))
  
  
  Unreported_Under_TH_Irrigation_Missing <- Unreported_Under_TH %>% 
    filter(County_Code %in% MISSING_withdrawals$County_Code)
  
  plotdat<-sp::merge(VA_counties,Unreported_Under_TH, by.x = "COUNTYFP", by.y = "County_Code")
  
  # Check if merge was correct
  sum(Unreported_Under_TH$Irr.Area.above.TH, na.rm = TRUE)
  sum(plotdat@data$Irr.Area.above.TH, na.rm=TRUE)
  
  p3<-tm_shape(plotdat)+
    tm_polygons("C_tot", title = "Unreported Coefficient",
                breaks = c(0,0.05,0.1,0.2,0.5,0.75,Inf),
                # n=5,style="jenks",
                textNA = "Missing Census data / No Irrigation deficit",
                id="NAMELSAD")+
    tm_layout(main.title = paste0(Year," Small farm unreported\n(as a percentage of VDEQ total nonenergy withdrawal)"),
              legend.outside = FALSE,
              legend.title.size = 1.2,
              legend.text.size = 0.8,
              legend.position = c("left","top"),
              legend.bg.alpha = 1)
  p3
  
  # tmap_save(p3, paste0(WUDR_github,"/plots/Coefficient1/",Year, "_SF_Coeff_DEQ_Missing_counties.png"),  width = 8, height = 5, units = 'in')
  
  ##### Plot for unreported Volume
  
  p4<-tm_shape(plotdat)+
    tm_polygons("Method1_Unreported", title = "Unreported volume (Acre-feet)",
                breaks = c(0,15,30,60,120,240,310),
                # n=5,style="jenks",
                textNA = "No DEQ Irrigation Withdrawals or Census data",
                id="NAMELSAD")+
    # tm_text("NAME", size = 0.3)+
    tm_layout(main.title = paste0(Year," Small farm unreported withdrawals"),
              legend.outside = FALSE,
              legend.title.size = 1,
              legend.text.size = 0.8,
              legend.position = c("left","top"),
              legend.bg.alpha = 1)
  p4
  
  
   # tmap_save(p4, paste0("F:/My Drive/WUDR/WUDR Manuscript/Plots/",Year, "_SF_Vol_DEQ_Missing_countiesAF.png"),  width = 8.5, height = 5, units = 'in')
  
  
  Unreported_Under_TH <- Unreported_Under_TH[,c(1,8,2,4,9,10:14)]
  # write.csv(Unreported_Under_TH, paste0(WUDR_github,"/Output_Tables/",Year, "_SF_Coeff_DEQ_Missing_counties.csv"))
  return(p4)
}

plot_2002 <- SF_plot_fn(2002)
plot_2007 <- SF_plot_fn(2007)
plot_2012 <- SF_plot_fn(2012)
plot_2017<- SF_plot_fn(2017)

# tmap_mode("view")
  tmap_mode("plot")
p_both <-  tmap_arrange(plot_2002,plot_2007,plot_2012,plot_2017,nrow=2, ncol=2)
  p_both  
  

 tmap_save(p_both, paste0("F:/My Drive/WUDR/WUDR Manuscript/Plots/", "_SF_ALL_AF.png"),  width = 10, height = 6, units = 'in')

 
# save(DEQ_2002,DEQ_2007,DEQ_2012,DEQ_2017,Tdeq_coef_2002,Tdeq_coef_2007,Tdeq_coef_2012,Tdeq_coef_2017,file= paste0(WUDR_github,"/dat_load/SF_Coeff.RData"))



















