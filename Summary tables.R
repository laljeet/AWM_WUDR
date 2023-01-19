
#################################################################
# summary of SF unreported if we consider census years

WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR"
setwd(WUDR_github)

library(tidyverse) 
library(rgdal)
library(vegan)
library(DescTools)
options(scipen = 9999)

####################################################################
load(paste0(WUDR_github,"/dat_load/SF_Coeff.RData"))

# Figure 3 presents county-level estimates of SM withdrawals across all four census years. 
# In counties where small farm withdrawals were present, these volumes ranged from 0.30 to 99.76  MG per year.
# To put these quantities in context, this ranges from 0.0015 to 164 percent of reported irrigation withdrawals in counties
# with DEQ reported data. As the threshold for reporting is withdrawals greater than 1 million gallons in any month, 
# X percent of counties have a cumulative volume of small farm withdrawals that would likely meet reporting thresholds
# if the volume were from a single user.  


SF_all_withdarwals <- rbind.data.frame(Tdeq_coef_2002,Tdeq_coef_2007,Tdeq_coef_2012,Tdeq_coef_2017)
min(SF_all_withdarwals$Vol_Unreported[SF_all_withdarwals$Vol_Unreported>0])
max(SF_all_withdarwals$Vol_Unreported[SF_all_withdarwals$Vol_Unreported>0])

SF_DEQ_withdarwals <- rbind.data.frame(DEQ_2002,DEQ_2007,DEQ_2012,DEQ_2017)
min(SF_DEQ_withdarwals$C_irr[SF_DEQ_withdarwals$C_irr>0])
max(SF_DEQ_withdarwals$C_irr[SF_DEQ_withdarwals$C_irr>0])


PCT_counties_reported <- round(100*sum(SF_DEQ_withdarwals$Vol_Unreported > 3) / nrow(SF_DEQ_withdarwals),0)
PCT_counties_reported

####################################################################
load(paste0(WUDR_github,"/dat_load/LF_Coeff_both.RData"))

names <- paste0("d_", names(Large_DEQ_Tot_area))
names(Large_DEQ_Tot_area) <- names

list2env(Large_DEQ_Tot_area,envir=.GlobalEnv)



LF_all_withdarwals <- rbind.data.frame(d_2002,d_2007,d_2012,d_2017)


min(LF_all_withdarwals$Large_Farm_unreported[LF_all_withdarwals$Large_Farm_unreported>0])
max(LF_all_withdarwals$Large_Farm_unreported[LF_all_withdarwals$Large_Farm_unreported>0])

######################################################################

names <- paste0("d_", names(Large_DEQ_IRR_Coeff))
names(Large_DEQ_IRR_Coeff) <- names

list2env(Large_DEQ_IRR_Coeff,envir=.GlobalEnv)

LF_DEQ_withdarwals <- rbind.data.frame(d_2002,d_2007,d_2012,d_2017)


min(LF_DEQ_withdarwals$C_irr_lg[LF_DEQ_withdarwals$Large_Farm_unreported>0])
max(LF_DEQ_withdarwals$C_irr_lg[LF_DEQ_withdarwals$Large_Farm_unreported>0])












####################################################
nrow(DEQ_2002)-nrow(Tdeq_coef_2002)
nrow(DEQ_2007)-nrow(Tdeq_coef_2007)
nrow(DEQ_2012)-nrow(Tdeq_coef_2012)
nrow(DEQ_2017)-nrow(Tdeq_coef_2017)


100*sum(Tdeq_coef_2002$Vol_Unreported)/sum(DEQ_2002$Facility_withdrawal_mg)
100*sum(Tdeq_coef_2007$Vol_Unreported)/sum(DEQ_2007$Facility_withdrawal_mg)
100*sum(Tdeq_coef_2012$Vol_Unreported)/sum(DEQ_2012$Facility_withdrawal_mg)
100*sum(Tdeq_coef_2017$Vol_Unreported)/sum(DEQ_2017$Facility_withdrawal_mg)


100*sum(Tdeq_coef_2002$Vol_Unreported)/sum(Tdeq_coef_2002$Facility_withdrawal_mg)
100*sum(Tdeq_coef_2007$Vol_Unreported)/sum(Tdeq_coef_2007$Facility_withdrawal_mg)
100*sum(Tdeq_coef_2012$Vol_Unreported)/sum(Tdeq_coef_2012$Facility_withdrawal_mg)
100*sum(Tdeq_coef_2017$Vol_Unreported)/sum(Tdeq_coef_2017$Facility_withdrawal_mg)

#################################################################################################
# Summary of small farm unreported based on times series of all years. # This is used in presentation

WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR"
setwd(WUDR_github)

library(tidyverse) 
library(rgdal)
library(vegan)
library(DescTools)
options(scipen = 9999)
# Load VA Shapefile to be used later
VA_counties<-readOGR(paste0(WUDR_github, "/VA_counties_sp"), layer="VA_counties")

county.codes <- read.csv(paste0(WUDR_github, "/csv_files/county_codes_census.csv")) 
####################################################################


load(paste0(WUDR_github,"/dat_load/SF_times_series_Median.RData")) # Small Farm time series

load(paste0(WUDR_github, "/dat_load/All_Years_DEQ_data_Total_nd_irr.Rdata"))  #DEQ reported data


load(paste0(WUDR_github,"/dat_load/PRECIP_JUNE_AUGUST_ALL_YEARS.RData")) # PRISM PPT data

load(paste0(WUDR_github,"/dat_load/PRISM_county_codes.RData")) # County Codes for PRISM

load(paste0(WUDR_github,"/dat_load/LF_All_times_series.RData")) # Large Farm time series


SF_unreported_summary <- TS_LF_Unreported_Median_Area %>% 
  group_by(Year) %>% 
  summarise(SF_unreported = sum(Small_Farm_Unreported),
            DEQ_irr_reported =sum(IRR_DEQ_withdrawals))

SF_unreported_summary$PCT <- (round(100*SF_unreported_summary$SF_unreported/SF_unreported_summary$DEQ_irr_reported,0))
mean(SF_unreported_summary$PCT)


LF_unreported_summary <- TS_LF_Unreported_Median_Area %>% 
  group_by(Year) %>% 
  summarise(LF_unreported = sum(Unreported_Deficit_Irr_based[Unreported_Deficit_Irr_based>0]),
            DEQ_irr_reported =sum(IRR_DEQ_withdrawals))

LF_unreported_summary$PCT <- (round(100*LF_unreported_summary$LF_unreported/LF_unreported_summary$DEQ_irr_reported,0))
mean(LF_unreported_summary$PCT)

##################################################################################
load(paste0(WUDR_github,"/dat_load/LF_Coeff_both.RData"))
load(paste0(WUDR_github, "/dat_load/All_Years_DEQ_data_Total_nd_irr.Rdata")) 

list2env(Large_DEQ_Tot_area,envir=.GlobalEnv)
######################################################################################
Tdeq_coef_2002$Year <- 2002
Tdeq_coef_2007$Year <- 2007
Tdeq_coef_2012$Year <- 2012
Tdeq_coef_2017$Year <- 2017


SF_all_withdarwals <- rbind.data.frame(Tdeq_coef_2002,Tdeq_coef_2007,Tdeq_coef_2012,Tdeq_coef_2017)

Arranges <- SF_all_withdarwals %>% arrange(desc(Irrigation), desc(Irr.Area.Under.TH))


Top_high <- SF_all_withdarwals %>%                                      # Top N highest values by group
  arrange(desc(Vol_Unreported)) %>% 
  group_by(Year) %>%
  slice(1:5) 

Top_low <- SF_all_withdarwals %>%                                      # Top N highest values by group
  arrange(Vol_Unreported) %>% 
  group_by(Year) %>%
  slice(1:10) 


##################################################################################
# Last TAbles
load(paste0(WUDR_github,"/dat_load/LF_Coeff_both.RData"))

y <- names(Large_DEQ_Tot_area)

names <- paste0("d_", names(Large_DEQ_Tot_area))
names(Large_DEQ_Tot_area) <- names

for (i in 1:4) {
  Large_DEQ_Tot_area[[i]]$Year = y[i] 
}



list2env(Large_DEQ_Tot_area,envir=.GlobalEnv)


LF_all_withdarwals <- rbind.data.frame(d_2002,d_2007,d_2012,d_2017)

summary <- LF_all_withdarwals %>% 
  group_by(Year) %>% 
  summarise(IA = sum(Total.Irri.Area),
            Idemadn = round(mean(Irrigation)/25.5,3))
