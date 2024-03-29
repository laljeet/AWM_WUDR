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

DEQ_Irr<- Irri_deq_county %>%
  dplyr::filter(YEAR <2018) %>% 
 dplyr:: group_by(YEAR) %>% 
  dplyr::summarise(`VDEQ Irrigation` = sum(Facility_withdrawal_mg))

Unreported_LF <- TS_LF_Unreported_Median_Area %>% 
  dplyr::filter(Unreported_Deficit_Irr_based >0 ) %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise("Large Farm Unreported" = sum(Unreported_Deficit_Irr_based))

Unreported_SF <- SF_Unreported_Median_UnderTh %>% 
  dplyr::filter(Unreported_Irrigation_based >0 ) %>% 
  dplyr::group_by(Year) %>% 
 dplyr:: summarise("Small Farm Unreported" = sum(Unreported_Irrigation_based))


plot_dat_median <- cbind.data.frame(DEQ_Irr,Unreported_LF,Unreported_SF)
plot_dat_median <- plot_dat_median[,c(1,2,4,6)]
plot_dat_median$PCTSF_DEQ <- plot_dat_median$`Small Farm Unreported`/plot_dat_median$`VDEQ Irrigation`
plot_dat_median$PCTLF_DEQ <- plot_dat_median$`Large Farm Unreported`/plot_dat_median$`VDEQ Irrigation`
mean(plot_dat_median$PCTSF_DEQ)
mean(plot_dat_median$PCTLF_DEQ)

plot_dat <- pivot_longer(plot_dat_median, cols = c(2:4), names_to = "Type" ,values_to = "Unreported Wth")

plot_dat$`Unreported Wth` <- plot_dat$`Unreported Wth`*3785/1000
p1 <- ggplot(plot_dat, aes(x=YEAR, y=`Unreported Wth`, group = Type ))+
  geom_line(aes(color=Type))+
  geom_point(aes(color=Type))+
  labs(title= " " ,
       x="Year", y = expression(paste("Unreported Withdrawals ", "(", 10^{3}, " x ", m^3, ")")))+
  scale_y_continuous(limits = c(0, 55000),  breaks = seq(0, 55000, by = 5000))+
  scale_x_continuous(limits = c(2002, 2017),  breaks = seq(2002, 2017, by = 2))

p1<- p1 + theme_bw()
p1 <- p1+theme(axis.text.x=element_text(angle = 0, hjust = 0),
               legend.position="top",
               legend.title=element_blank(),
               legend.box = "horizontal",
               legend.background = element_rect(fill="white",
                                                size=0.5, linetype="solid",
                                                colour ="white"),
               legend.text=element_text(size=12),
               axis.text=element_text(size=12, colour="black"),
               axis.title=element_text(size=14, colour="black"),
               axis.line = element_line(colour = "black",
                                        size = 0.5, linetype = "solid"),
               axis.ticks = element_line(colour="black"),
               panel.grid.major=element_line(colour = "light grey"),
               panel.grid.minor=element_blank())
p1
# ggplotly(p1)

ggsave(paste0("F:/My Drive/WUDR/WUDR Manuscript/Plots/","State_Summary_unreported_WTHSI.png"), plot = p1, width = 9.5, height = 6, units = "in")

