WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR"
setwd(WUDR_github)

library(tidyverse) 
library(rgdal)
options(scipen = 9999)
# Load VA Shapefile to be used later
VA_counties<-readOGR(paste0(WUDR_github, "/VA_counties_sp"), layer="VA_counties")

county.codes <- read.csv(paste0(WUDR_github, "/csv_files/county_codes_census.csv")) 
load(paste0(WUDR_github,"/dat_load/SF_times_series_Median.RData"))
rm(SF_Unreported_Median_Irr_Coeff)


load(paste0(WUDR_github,"/dat_load/LF_All_times_series.RData")) # Large Farm time series
rm(TS_LF_Coeff_Unreported_median)
#####Make selection
select <- TS_LF_Unreported_Median_Area %>% 
  filter(IRR_DEQ_withdrawals >0) %>% 
  filter(Small_Farm_Unreported >0) %>% 
  filter(Unreported_Deficit_Irr_based >0)

Select_counties <- c("131", "171", "145")

LF_TS_select_counties <- TS_LF_Unreported_Median_Area %>% 
  filter(County_Code %in% Select_counties)

LF_TS_select_counties$Unreported_Deficit_Irr_based <- ifelse(LF_TS_select_counties$Unreported_Deficit_Irr_based<0,
                                                             0,LF_TS_select_counties$Unreported_Deficit_Irr_based)

yearly_max_LF <- TS_LF_Unreported_Median_Area %>% 
  group_by(Year) %>% 
  slice_max(Unreported_Deficit_Irr_based)

yearly_min_LF <- TS_LF_Unreported_Median_Area %>% 
  group_by(Year) %>% 
  slice_min(Unreported_Deficit_Irr_based)
###############################################################################



SF_TS_select_counties <- SF_Unreported_Median_UnderTh %>% 
   filter(County_Code %in% Select_counties)

SF_TS_select_counties$Unreported_Irrigation_based <- ifelse(SF_TS_select_counties$Unreported_Irrigation_based<0,
                                                            0,SF_TS_select_counties$Unreported_Irrigation_based)


quantile(SF_Unreported_Median_UnderTh$Unreported_Irrigation_based)

small_counties_SF <-  SF_Unreported_Median_UnderTh %>% 
  filter(Unreported_Irrigation_based < 3.5)

large_counties_SF <-  SF_Unreported_Median_UnderTh %>% 
  filter(Unreported_Irrigation_based >13)

# hist(small_counties_SF$Year)
# hist(large_counties_SF$Year)






#####################################################################################################
load(paste0(WUDR_github, "/dat_load/All_Years_DEQ_data_Total_nd_irr.Rdata"))

Select_codes <- c("131", "171", "145")


DEQ_select_counties <- Irri_deq_county %>% 
  filter(COUNTYFP %in% Select_codes) %>% 
  filter(YEAR< 2018)


rm(Irri_deq_county,large_counties_SF,SF_Unreported_Median_UnderTh,Total_deq_county,TS_LF_Unreported_Median_Area, yearly_max_LF,yearly_min_LF,small_counties_SF)


###############################################

North <- LF_TS_select_counties %>% 
  filter(County_Code == 131)

Pow <- LF_TS_select_counties %>% 
  filter(County_Code == 145)

Shenan <- LF_TS_select_counties %>% 
  filter(County_Code == 171)

#########################################################Shenandoah digin
load(paste0(WUDR_github,"/dat_load/Monthly_PPT_Growing_Season.Rdata"))
Shenan_ppt <- Monthly_PPT_Growing_Season %>% 
  select(starts_with('Shenan'))
Shenan_ppt$Shenandoah_in <- Shenan_ppt$Shenandoah/25.5

library(plyr)
 library(scales)

##################################################################
  plot_dat <- Shenan[,c(1:3,8,10,11)]
  plot_dat[4:6] <- plot_dat[4:6]*3.068
  max_lim <- max(plot_dat$Unreported_Deficit_Irr_based)
  max_lim <- round(max_lim,-2)+100
  sc = 2
  # p1 <- ggplot(plot_dat, aes(x = Year, y = Unreported_Deficit_Irr_based/sc)) +
     p1 <- ggplot(plot_dat, aes(x = Year, y = Unreported_Deficit_Irr_based  )) +
  geom_line(aes(color = "LF unreported "), lwd=1) +
  geom_line(aes(y = IRR_DEQ_withdrawals, color = "VDEQ reported "), lwd=1)+
  geom_line(aes(y = Small_Farm_Unreported, color = "SF Unreported "), lwd=1)+
   scale_x_continuous(limits = c(2002, 2017),  breaks = seq(2002, 2017, by = 1))+
  # scale_y_continuous(limits = c(0,2000), breaks = seq(0,2000, by = 500))+
    scale_y_continuous(limits = c(0,max_lim ), breaks = seq(0,max_lim, by = 100))+
                       # ,sec.axis = sec_axis(~.*sc, name="LF unreported (MG)")) +
    
  labs(title= "Shenandoah" , x = "Year", y = "Withdrawals \n(Acre-feet)", color = "") +
         # labs(title= "Shenandoah" , x = "Year", y = bquote('Withdrawals (' ~ 10^'4' ~ 'meter cu'~')'), color = "") +
  scale_color_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb"))
    p1<- p1 + theme_bw()
  p1 <- p1+theme(plot.title = element_text(size=20),
                 axis.text.x=element_text(angle = 90, hjust = 0),
                 legend.position="top",
                 legend.title=element_blank(),
                 legend.box = "horizontal",
                 legend.background = element_rect(fill="white",
                                                  size=0.5, linetype="solid",
                                                  colour ="white"),
                 legend.text=element_text(size=14),
                 axis.text=element_text(size=14, colour="black"),
                 axis.title=element_text(size=16, colour="black"),
                 axis.line = element_line(colour = "black",
                                          size = 0.5, linetype = "solid"),
                 axis.ticks = element_line(colour="black"),
                 panel.grid.major=element_line(colour = "light grey"),
                 panel.grid.minor=element_blank())
  p1

 ggsave(paste0("F:/My Drive/WUDR/WUDR Manuscript/Plots/shenanAF.png"), plot = p1, width = 9.5, height = 6, units = "in")



#   
  plot_dat <- North[,c(1:3,8,10,11)]
   plot_dat[4:6] <- plot_dat[4:6]*3.068
   
     max_lim <- max(plot_dat$Unreported_Deficit_Irr_based)
  max_lim <- round(max_lim,-2)+100
  sc = 20
  p2 <- ggplot(plot_dat, aes(x = Year, y = Unreported_Deficit_Irr_based )) +
  geom_line(aes(color = "LF unreported "), lwd=1) +
  geom_line(aes(y = IRR_DEQ_withdrawals , color = "VDEQ reported "),lwd=1)+
  geom_line(aes(y = Small_Farm_Unreported*sc, color = "SF Unreported "), lwd=1)+
   scale_x_continuous(limits = c(2002, 2017),  breaks = seq(2002, 2017, by = 1))+
  # scale_y_continuous(limits = c(0,2000), breaks = seq(0,2000, by = 500))+
    scale_y_continuous(limits = c(0,6000), breaks = seq(0,6000, by = 1000), 
                       sec.axis = sec_axis(~./sc, name="SF Unreported\n(Acre-feet)")) +
    
  labs(title= "Northampton" , x = "Year", y = "LF unreported / VDEQ reported \n (Acre-feet)" , color = "") +
  scale_color_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb"))
    p2<- p2 + theme_bw()
  p2 <- p2+theme(plot.title = element_text(size=20),
                 axis.text.x=element_text(angle = 90, hjust = 0),
                 legend.position="top",
                 legend.title=element_blank(),
                 legend.box = "horizontal",
                 legend.background = element_rect(fill="white",
                                                  size=0.5, linetype="solid",
                                                  colour ="white"),
                 legend.text=element_text(size=14),
                 axis.text=element_text(size=14, colour="black"),
                 axis.title=element_text(size=16, colour="black"),
                 axis.line = element_line(colour = "black",
                                          size = 0.5, linetype = "solid"),
                 axis.ticks = element_line(colour="black"),
                 panel.grid.major=element_line(colour = "light grey"),
                 panel.grid.minor=element_blank())
  p2

  # plotly::ggplotly(p2)
 ggsave(paste0("F:/My Drive/WUDR/WUDR Manuscript/Plots/NORTHAMPTONAF.png"), plot = p2, width = 9.5, height = 6, units = "in")

 plot_dat <- Pow[,c(1:3,8,10,11)]
 plot_dat[4:6] <- plot_dat[4:6]*3.068
 max_lim <- max(plot_dat$Unreported_Deficit_Irr_based)
  max_lim <- round(max_lim,-2)
 p3 <- ggplot(plot_dat, aes(x = Year, y = Unreported_Deficit_Irr_based )) +
  geom_line(aes(color = "LF unreported"), lwd=1) +
  geom_line(aes(y = IRR_DEQ_withdrawals , color = "VDEQ reported "), lwd=1)+
  geom_line(aes(y = Small_Farm_Unreported , color = "SF Unreported"), lwd=1)+
   scale_x_continuous(limits = c(2002, 2017),  breaks = seq(2002, 2017, by = 1))+
  # scale_y_continuous(limits = c(0,2000), breaks = seq(0,2000, by = 500))+
    scale_y_continuous(limits = c(0,max_lim ), breaks = seq(0,max_lim, by = 20)) +
    
  labs(title= "Powhatan" , x = "Year", y = "Withdrawals \n(Acre-feet)", color = "") +
  scale_color_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb"))
    p3<- p3 + theme_bw()
  p3 <- p3+theme(plot.title = element_text(size=20),
                 axis.text.x=element_text(angle = 90, hjust = 0),
                 legend.position="top",
                 legend.title=element_blank(),
                 legend.box = "horizontal",
                 legend.background = element_rect(fill="white",
                                                  size=0.5, linetype="solid",
                                                  colour ="white"),
                 legend.text=element_text(size=14),
                 axis.text=element_text(size=14, colour="black"),
                 axis.title=element_text(size=16, colour="black"),
                 axis.line = element_line(colour = "black",
                                          size = 0.5, linetype = "solid"),
                 axis.ticks = element_line(colour="black"),
                 panel.grid.major=element_line(colour = "light grey"),
                 panel.grid.minor=element_blank())
  p3

 ggsave(paste0("F:/My Drive/WUDR/WUDR Manuscript/Plots/Powhatancu.png"), plot = p3, width = 9.5, height = 6, units = "in")

  library(ggpubr)
   p_both <-  ggarrange(p1+ rremove("xlab"), p2+ rremove("xlab"), p3 , 
          nrow = 3, common.legend = TRUE,legend="bottom")
  p_both  
  
  ggsave("F:/My Drive/WUDR/WUDR Manuscript/Plots/secondaryAxis_countiesAF.png", plot = p_both, width = 8, height =12, units = "in")
  
  