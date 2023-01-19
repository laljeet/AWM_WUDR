WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR"
setwd(WUDR_github)

library(tidyverse) 
library(rgdal)
options(scipen = 9999)
# Load VA Shapefile to be used later
VA_counties<-readOGR(paste0(WUDR_github, "/VA_counties_sp"), layer="VA_counties")

county.codes <- read.csv(paste0(WUDR_github, "/csv_files/county_codes_census.csv"))
load(paste0(WUDR_github,"/dat_load/LF_All_times_series.RData")) # Large Farm time series

LF_comparison_summary <- TS_LF_Unreported_Median_Area %>% 
  group_by(Year,County_Code) %>% 
  summarise(Irrigation_withdrawals = sum(All_Irrigation_mg[All_Irrigation_mg>0]),
            DEQ_reported =sum(IRR_DEQ_withdrawals))
plot_dat <- LF_comparison_summary

max <- max(c(max(plot_dat$DEQ_reported), na.rm = TRUE))

min <- min(c(min(plot_dat$DEQ_reported), na.rm = TRUE))


library(scales)

plot_dat <- plot_dat %>% 
  filter(Irrigation_withdrawals >0 ) %>% 
  filter(DEQ_reported >0)

plot_dat$status <- NA

# 
# plot_dat$status[plot_dat$Irrigation_withdrawals < 10 & plot_dat$DEQ_reported <10] <- "Similar trend"
# plot_dat$status[between(plot_dat$Irrigation_withdrawals,10,100) &  between(plot_dat$DEQ_reported,10,100)] <- "Similar trend"
# plot_dat$status[between(plot_dat$Irrigation_withdrawals,100,1000) &  between(plot_dat$DEQ_reported,100,1000)] <- "Similar trend"
# plot_dat$status[plot_dat$Irrigation_withdrawals > 1000 & plot_dat$DEQ_reported >1000] <- "Similar trend"
# 
# plot_dat$status[plot_dat$Irrigation_withdrawals > 10 & plot_dat$DEQ_reported <10] <- "Lower DEQ reported"
# plot_dat$status[plot_dat$Irrigation_withdrawals < 10 & plot_dat$DEQ_reported >10] <- "Lower Irrigation estimated"
# 
# plot_dat$status[between(plot_dat$Irrigation_withdrawals,10,100) &  plot_dat$DEQ_reported <10] <- "Lower DEQ reported"
# 
# plot_dat$status[between(plot_dat$Irrigation_withdrawals,100,1000) &  plot_dat$DEQ_reported <100] <- "Lower DEQ reported"
# plot_dat$status[plot_dat$Irrigation_withdrawals > 1000 & plot_dat$DEQ_reported <1000] <- "Lower DEQ reported"
# 
# 
# 
# 
# plot_dat$status[between(plot_dat$DEQ_reported,10,100) &  plot_dat$Irrigation_withdrawals <10] <- "Lower Irrigation estimated"
# 
# plot_dat$status[between(plot_dat$DEQ_reported,100,1000) &  plot_dat$Irrigation_withdrawals <100] <- "Lower Irrigation estimated"
# plot_dat$status[plot_dat$DEQ_reported > 1000 & plot_dat$Irrigation_withdrawals <1000] <- "Lower Irrigation estimated"
# 
# 
# sum(plot_dat$status == "Similar trend")
# 
# sum(plot_dat$status == "Lower Irrigation estimated")
# 
# sum(plot_dat$status == "Lower DEQ reported")
# 
# plot_dat$status[plot_dat$status == "Similar trend"] <- c("")

plot_dat$status[plot_dat$Irrigation_withdrawals < 1000 & plot_dat$DEQ_reported <1] <- "Outlier"
plot_dat$status[plot_dat$Irrigation_withdrawals < 10 & plot_dat$DEQ_reported >10] <- "Outlier"

plot_dat[3:4] <- plot_dat[3:4]*3.068
max <- max(c(max(plot_dat$DEQ_reported), na.rm = TRUE))

min <- min(c(min(plot_dat$DEQ_reported), na.rm = TRUE))


p1 <- ggplot(plot_dat, aes(x=Irrigation_withdrawals, y=DEQ_reported, color= status ))+
  geom_point()+
  # geom_abline()+
  # geom_smooth(method=lm)+
  labs(title = "",
       x="Irrigation withdrawals \n(Acre-feet)", y = "VDEQ Reported\n (Acre-feet)")+
  scale_x_continuous(trans =log10_trans(), 
                     breaks = c(0.1,1, 10, 100, 1000,10000,1000000), 
                     limits=c(min,max)) +
  scale_y_continuous(trans = log10_trans(), 
                     breaks = c(0.1,1, 10, 100, 1000,10000,1000000), 
                     limits=c(min,max))
p1
p1<- p1 + theme_bw()
p1 <- p1+theme( plot.title = element_text(size=18),
                axis.text.x=element_text(angle = 0, hjust = 0),
               legend.position="none", 
               legend.title=element_blank(),
               legend.box = "horizontal", 
               legend.background = element_rect(fill="white",
                                                    size=0.5, linetype="solid", 
                                                colour ="white"),
               legend.text=element_text(size = 16),
               axis.text=element_text(size = 16, colour="black"),
               axis.title=element_text(size = 16, colour="black"),
               axis.line = element_line(colour = "black", 
                                        size = 0.5, linetype = "solid"),
               axis.ticks = element_line(colour="black"),
               panel.grid.major=element_line(colour = "light grey"), 
               panel.grid.minor=element_blank())
p1


library(plotly)
ggplotly(p1)

ggsave("F:/My Drive/WUDR/WUDR Manuscript/Plots/Scatter_colorAF1.png", plot = p1, width =12, height =6, units = "in")


