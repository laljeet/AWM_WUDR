WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR_local"
setwd(WUDR_github)

pacman::p_load(dplyr, rgdal, tmap,tidyverse, gridExtra)
year=2017

deq_dat<-read.csv(paste0(WUDR_github,"/csv_files/", year,"Summary_DEQ_withdarwals.csv"))

VA_counties<-readOGR("F:/My Drive/WUDR/WUDR_Github/WUDR/VA_counties_sp", layer="VA_counties")

data_deq<-deq_dat[complete.cases(deq_dat),]

census_dat<-read.csv(paste0(WUDR_github,"/csv_files/",year, "County_data_NASS.csv"))


census_dat<-merge.data.frame(census_dat,deq_dat[,c("GEOID","COUNTYFP")], by.x = "County_Code", by.y ="COUNTYFP", all.x = TRUE )

data_census<-census_dat[complete.cases(census_dat),]

data_deq$data_status<- data_deq$GEOID %in% data_census$GEOID
data_deq$data_status<-ifelse(data_deq$data_status == TRUE, "Both datasets", "VDEQ only")

data_census$data_status<-data_census$GEOID%in% data_deq$GEOID
data_census$data_status<-ifelse(data_census$data_status == TRUE, "Both datasets", "USDA only")

#############################
#Histogrmas for counties  ##
############################
p1<-data_census%>%
  filter(data_status=="Both datasets")%>%
  ggplot(aes(x=Irrigated_Acreage))+
  geom_histogram()+
  labs(subtitle = "Irrigated Acreage for data in both VDEQ and Census datsets", 
       x= "Irrigated Acreage", y="Count")+
  theme_light()


p1
# ggsave(paste0(WUDR_github,"/plots/Hist Irrigated acreage both datesets.png"),p1, width = 8, height = 5, units="in")


p2<-data_census%>%
  filter(data_status=="USDA only")%>%
  ggplot(aes(x=Irrigated_Acreage))+
  geom_histogram()+
  labs(subtitle = "Irrigated Acreage for data in only Census datset", 
       x= "Irrigated Acreage", y="Count")+
  theme_light()


p2
# ggsave(paste0(WUDR_github,"/plots/Hist Irrigated acreage only Census dateset.png"),p2, width = 8, height = 5, units="in")
p3<-grid.arrange(p1, p2, ncol=1)

# ggsave(paste0(WUDR_github,"/plots/Hist Irrigated acreage side by side.png"),p3, width = 8, height = 5, units="in")
plyr::count(data_census$Irrigated_Acreage)

dat1<-merge.data.frame(data_census[,c("GEOID", "data_status")],
                       data_deq[,c("GEOID", "data_status")], by.x="GEOID", by.y="GEOID", all.x = TRUE, all.y=TRUE)

dat1$data_status.x <- ifelse(is.na(dat1$data_status.x), dat1$data_status.y, dat1$data_status.x)


deq_dat$status<-deq_dat$GEOID %in% dat1$GEOID
deq_dat_missing<-filter(deq_dat, status== FALSE)
deq_dat_missing$status[deq_dat_missing$status==FALSE] <- c("No irrigation reported")

dat_final<-as.data.frame(mapply(c,dat1[,c("GEOID", "data_status.x")],deq_dat_missing[,c("GEOID","status")]))
colnames(dat_final)[2]<- "Data Status"

VA_counties<-readOGR(paste0(WUDR_github,"/VA_shapefile_updated"), layer="VA_counties_new")

plotdat<-sp::merge(VA_counties,dat_final, 
                   by.x = "GEOID", by.y = "GEOID", all.x=TRUE)

summary<-plotdat@data[,c(1,3,10,11)]
# write.csv(summary,paste0(WUDR_github,"/csv_files/deqdat_avaliability_summary", year,".csv"))

########################################################
dev.off()

plotdat@data$`Data Status`[plotdat@data$`Data Status` == "No irrigation reported"] <- NA
MYpal<-c("#1c9099","#fdae6b","#756bb1")
tmap_mode("plot")
p1<-tm_shape(plotdat)+
  tm_polygons("Data Status", title = "Status",
              # breaks = c(0,1,5,10,20),
              #n=5,style="jenks",
               palette = MYpal,
              id="NAMELSAD",
              textNA = "No irrigation reported", 
              colorNA = "grey",
              legend.hist = FALSE)+
  tm_layout(main.title = paste0(year," Data availability in USDA-CA and VDEQ reported withdrawals"),
            legend.outside = FALSE,
            legend.title.size = 1.2,
            legend.text.size = 1,
            legend.position = c("left","top"),
            legend.bg.alpha = 1)
p1

tmap_save(p1, "F:/My Drive/WUDR/WUDR Manuscript/Plots/Data avaliability1.png",  width = 10, height = 5, units = 'in')
# tmap_save(p1, paste0(WUDR_github,"/plots/DEQ_irrigated_facilites-1.html"),  width = 10, height = 6.5, units = 'in')


#######################################
#Summary
###

total <- data_census %>% 
  summarise(Acreage_sum=sum(Irrigated_Acreage[Irrigated_Acreage >0]),
            Operation_sum=sum(Irrigated_Operations),
            Count = n())
  
USDA_Both <- data_census%>%
  filter(data_status == "Both datasets")%>%
  summarise(Acreage_sum=sum(Irrigated_Acreage[Irrigated_Acreage >0]),
            Operation_sum=sum(Irrigated_Operations),
            Count = n())

USDA <- data_census%>%
  filter(data_status == "USDA only")%>%
  summarise(Acreage_sum=sum(Irrigated_Acreage[Irrigated_Acreage >0]),
            Operation_sum=sum(Irrigated_Operations),
            Count = n())

DEQ <- data_deq%>%
  filter(data_status == "DEQ only")%>%
  summarise(withdrawal_sum=sum(Facility_withdrawal_mg),
            Count = n(),
            Faciltiy_sum=sum(Count))

DEQ_Both<- data_deq%>%
  filter(data_status == "Both datasets")%>%
  summarise(withdrawal_sum=sum(Facility_withdrawal_mg),
            Count = n(),
            Faciltiy_sum=sum(Count))

DEQ_Total <- data_deq%>%
  summarise(withdrawal_sum=sum(Facility_withdrawal_mg),
            Count = n(),
            Faciltiy_sum=sum(Count))
