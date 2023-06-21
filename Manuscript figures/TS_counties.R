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

hist(small_counties_SF$Year)
hist(large_counties_SF$Year)



load(paste0(WUDR_github,"/dat_load/LF_All_times_series.RData")) # Large Farm time series
rm(TS_LF_Coeff_Unreported_median)



#####################################################################################################
load(paste0(WUDR_github, "/dat_load/All_Years_DEQ_data_Total_nd_irr.Rdata"))

Select_codes <- c("131", "171", "145")


DEQ_select_counties <- Irri_deq_county %>% 
  filter(COUNTYFP %in% Select_codes) %>% 
  filter(YEAR< 2018)
     
     
     
#######################################################################################################
# PLOTS
     
#####################################################################################################


# Small Farms

plot_dat_median <- split(SF_TS_select_counties , f = SF_TS_select_counties$name)

i=1  
p1 <- ggplot(plot_dat_median[[i]], aes(x=Year, y=`Unreported_Irrigation_based`, group = 1))+
    geom_line()+
    geom_point()+
    labs(title= paste0("Northampton" ) ,
         x="Year", y = " Withdarwals (MG)")+
    scale_x_continuous(limits = c(2002, 2017),  breaks = seq(2002, 2017, by = 3))+
  scale_y_continuous(limits = c(0,20), breaks = seq(0,20, by = 5))
  p1<- p1 + theme_bw()
  p1 <- p1+theme(axis.text.x=element_text(angle = 90, hjust = 0),
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
  
  nam = paste0("/", plot_dat_median[[i]][1,3],"SF")

  #ggsave(paste0(WUDR_github, nam,".png"), plot = p1, width = 9.5, height = 6, units = "in")
  
  i = 3
  p2 <- ggplot(plot_dat_median[[i]], aes(x=Year, y=`Unreported_Irrigation_based`, group = 1))+
    geom_line()+
    geom_point()+
    labs(title= paste0("Shenandoah" ) ,
         x="Year", y = " Withdarwals (MG)")+
    scale_x_continuous(limits = c(2002, 2017),  breaks = seq(2002, 2017, by = 3))+
  scale_y_continuous(limits = c(0,50), breaks = seq(0,50, by = 10))
  
  p2<- p2 + theme_bw()
  p2 <- p2+theme(axis.text.x=element_text(angle = 90, hjust = 0),
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
  p2
  
  nam = paste0( "/",plot_dat_median[[i]][1,3],"SF")
  
  #ggsave(paste0(WUDR_github, nam,".png"), plot = p2, width = 9.5, height = 6, units = "in")
  
  
  i = 2
  p3 <- ggplot(plot_dat_median[[i]], aes(x=Year, y=`Unreported_Irrigation_based`, group = 1))+
    geom_line()+
    geom_point()+
    labs(title= paste0("Powhatan" ) ,
         x="Year", y = " Withdarwals (MG)")+
    scale_x_continuous(limits = c(2002, 2017),  breaks = seq(2002, 2017, by = 3))+
    scale_y_continuous(limits = c(0,8), breaks = seq(0,8, by = 2))
  
  p3<- p3 + theme_bw()
  p3 <- p3+theme(axis.text.x=element_text(angle = 90, hjust = 0),
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
  p3
  
  nam = paste0( "/",plot_dat_median[[i]][1,3],"SF")
  
  #ggsave(paste0(WUDR_github, nam,".png"), plot = p3, width = 9.5, height = 6, units = "in")

  
  library("ggpubr")
 p<-  ggarrange(p1+ rremove("xlab"), p2+ rremove("xlab"), p3 , 
          nrow = 3)
  
p<-annotate_figure(p,
                top = text_grob("SF Unreported Withdrawals", face = "bold", size = 16)
                )
p
#ggsave("F:/My Drive/WUDR/WUDR Manuscript/PLots/SF_TS-counties.png", plot = p, width = 8, height = 12, units = "in")



############################################################################################################
  # large Farm
  plot_dat_median <- split(LF_TS_select_counties , f = LF_TS_select_counties$name)
  
  i=1  
  p4 <- ggplot(plot_dat_median[[i]], aes(x=Year, y=`Unreported_Deficit_Irr_based`, group = 1))+
    geom_line()+
    geom_point()+
    labs(title= paste0("Northampton" ) ,
         x="Year", y = " Withdarwals (MG)")+
    scale_x_continuous(limits = c(2002, 2017),  breaks = seq(2002, 2017, by = 3))+
    scale_y_continuous(limits = c(0,2000), breaks = seq(0,2000, by = 500))
  p4<- p4 + theme_bw()
  p4 <- p4+theme(axis.text.x=element_text(angle = 90, hjust = 0),
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
  p4
  
  nam = paste0("/", plot_dat_median[[i]][1,3],"LF")
  
  #ggsave(paste0(WUDR_github, nam,".png"), plot = p4, width = 9.5, height = 6, units = "in")
  
  i = 3
  p5 <- ggplot(plot_dat_median[[i]], aes(x=Year, y=`Unreported_Deficit_Irr_based`, group = 1))+
    geom_line()+
    geom_point()+
    labs(title= paste0("Shenandoah" ) ,
         x="Year", y = " Withdarwals (MG)")+
    scale_x_continuous(limits = c(2002, 2017),  breaks = seq(2002, 2017, by = 3))+
    scale_y_continuous(limits = c(0,220), breaks = seq(0,200, by = 50))
  
  p5<- p5 + theme_bw()
  p5 <- p5+theme(axis.text.x=element_text(angle = 90, hjust = 0),
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
  p5
  
  nam = paste0( "/",plot_dat_median[[i]][1,3],"LF")
  
  #ggsave(paste0(WUDR_github, nam,".png"), plot = p5, width = 9.5, height = 6, units = "in")
  
  
  i = 2
  p6 <- ggplot(plot_dat_median[[i]], aes(x=Year, y=`Unreported_Deficit_Irr_based`, group = 1))+
    geom_line()+
    geom_point()+
    labs(title= paste0("Powhatan" ) ,
         x="Year", y = " Withdarwals (MG)")+
    scale_x_continuous(limits = c(2002, 2017),  breaks = seq(2002, 2017, by = 3))+
    scale_y_continuous(limits = c(0,25), breaks = seq(0,25, by = 5))
  
  p6<- p6 + theme_bw()
  p6 <- p6+theme(axis.text.x=element_text(angle = 90, hjust = 0),
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
  p6
  
  nam = paste0( "/",plot_dat_median[[i]][1,3],"LF")
  
  #ggsave(paste0(WUDR_github, nam,".png"), plot = p6, width = 9.5, height = 6, units = "in")
  
  library("ggpubr")
  p_1<-  ggarrange(p4+ rremove("xlab"), p5+ rremove("xlab"), p6 , 
                   nrow = 3)
  
  p_1 <- annotate_figure(p_1,
                         top = text_grob("     LF Unreported Withdrawals", face = "bold", size = 16)
  )
p_1


  #################################################################################################################
  plot_dat_median <- split(DEQ_select_counties , f = DEQ_select_counties$NAMELSAD)
  
  i=1  
  p7 <- ggplot(plot_dat_median[[i]], aes(x=YEAR, y=`Facility_withdrawal_mg`, group = 1))+
    geom_line()+
    geom_point()+
    labs(title= paste0("Northampton" ) ,
         x="Year", y = "Withdarwals (MG)")+
    scale_x_continuous(limits = c(2002, 2017),  breaks = seq(2002, 2017, by = 3))+
    scale_y_continuous(limits = c(0,1500), breaks = seq(0,1500, by = 500))
  p7<- p7 + theme_bw()
  p7 <- p7+theme(axis.text.x=element_text(angle = 90, hjust = 0),
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
  p7
  
  nam = paste0("/", plot_dat_median[[i]][1,3],"SF")
  
  #ggsave(paste0(WUDR_github, nam,".png"), plot = p1, width = 9.5, height = 6, units = "in")
  
  i = 3
  p8 <- ggplot(plot_dat_median[[i]], aes(x=YEAR, y=`Facility_withdrawal_mg`, group = 1))+
    geom_line()+
    geom_point()+
    labs(title= paste0("Shenandoah" ) ,
         x="Year", y = " Withdarwals (MG)")+
    scale_x_continuous(limits = c(2002, 2017),  breaks = seq(2002, 2017, by = 3))+
    scale_y_continuous(limits = c(0,100), breaks = seq(0,100, by = 20))
  
  p8<- p8 + theme_bw()
  p8 <- p8+theme(axis.text.x=element_text(angle = 90, hjust = 0),
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
  p8
  
  nam = paste0( "/",plot_dat_median[[i]][1,3],"SF")
  
  #ggsave(paste0(WUDR_github, nam,".png"), plot = p2, width = 9.5, height = 6, units = "in")
  
  
  i = 2
  p9 <- ggplot(plot_dat_median[[i]], aes(x=YEAR, y=`Facility_withdrawal_mg`, group = 1))+
    geom_line()+
    geom_point()+
    labs(title= paste0("Powhatan" ) ,
         x="Year", y = " Withdarwals (MG)")+
    scale_x_continuous(limits = c(2002, 2017),  breaks = seq(2002, 2017, by = 3))+
    scale_y_continuous(limits = c(0,8), breaks = seq(0,8, by = 2))
  
  p9<- p9 + theme_bw()
  p9 <- p9+theme(axis.text.x=element_text(angle = 90, hjust = 0),
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
  p9
  
  nam = paste0( "/",plot_dat_median[[i]][1,3],"SF")
  
  p_2<-  ggarrange(p7+ rremove("xlab"), p8+ rremove("xlab"), p9 , 
                   nrow = 3)
  
  p_2 <- annotate_figure(p_2,
                         top = text_grob("DEQ reported Withdrawals", face = "bold", size = 16)
  )
  p_2
  
  
  
  
  ##All Three
  p_both <-  ggarrange(p, p_1,p_2, ncol = 3)
  p_both  
  
  ggsave("F:/My Drive/WUDR/WUDR Manuscript/PLots/threeplots-counties.png", plot = p_both, width = 10, height = 8, units = "in")
  
  
  
  
  
  

   

# 
# ggsave("F:/My Drive/WUDR/WUDR Manuscript/PLots/SF_TS-counties.png", plot = p, width = 8, height = 8, units = "in")
# ggsave("F:/My Drive/WUDR/WUDR Manuscript/PLots/LF_TS-counties.png", plot = p_1, width = 8, height = 8, units = "in")
# ggsave("F:/My Drive/WUDR/WUDR Manuscript/PLots/DEQ_TS-counties.png", plot = p_2, width = 8, height = 8, units = "in")

 p_both <-  ggarrange(p, p_1, ncol = 2)
p_both  

ggsave("F:/My Drive/WUDR/WUDR Manuscript/PLots/both-counties.png", plot = p_both, width = 10, height = 8, units = "in")



###############################################