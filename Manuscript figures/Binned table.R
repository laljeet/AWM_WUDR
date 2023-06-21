WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR"
setwd(WUDR_github)
library(tidyverse)

load(paste0(WUDR_github, "/dat_load/Quickstats_All_years_data.Rdata"))

##QS_data function takes Year as an input and gives three files required. 
#binned_operations- The no of operations in each bin size for a census year
#binned_irrigated_area - Irrigated area in each bin size for a census year
#County_Summary - Summary Census data in each county 

YEAR = 2017

  County_summary <- subset(nass_binned, Domain == "TOTAL") #Total gives county Totals for irrigated area and Irrigated operations
  # County_summary <- County_summary %>% 
  #   dplyr::select(Year = "Year", County = "County" ,County_Code = "County_Code", 
  #                 Irr.Ops = "Value.y", Irr.Area.Acres = "Irrigated_Area")
  
  dat_YEAR_QS <-   filter(County_summary, Year == YEAR ) # County Totals for a particular year
  
  QS_year <- filter(nass_binned, Year == YEAR )  # This gives irrigated acreage and operations for each size bin in a county for a particular year
  
  ## Count number of counties with irrigation data by farm size
  size.id <- grep(pattern="AREA OPERATED", QS_year$Size.bins) 
  farm.size <- QS_year[size.id ,]              # Remove the total values
  
  n.counties <- length(unique(farm.size$County_Code))
  n.sizebins <- length(unique(farm.size$Size.bins))
  
  farm.size.ops <- matrix(data=NA, ncol=n.sizebins, nrow=n.counties)
  colnames(farm.size.ops) <- unique(farm.size$Size.bins)
  rownames(farm.size.ops) <- unique(farm.size$County_Name)
  
  farm.size.size <- farm.size.ops
  
  for (c in 1:n.counties){
    county.data <- subset(farm.size, County_Code == unique(farm.size$County_Code)[c])
    county.ops <- county.data[, -c(7,8)]
    county.sizes <- county.data[, -c(7,9)]
    for (s in 1:n.sizebins){
      size.ops<-subset(county.ops, Size.bins == unique(farm.size$Size.bins)[s])
      size.sizes<-subset(county.sizes, Size.bins == unique(farm.size$Size.bins)[s])
      if (dim(size.ops)[1] == 1) {farm.size.ops[c,s] <- paste(size.ops$Irr.Ops)}
      if (dim(size.sizes)[1] == 1) {farm.size.size[c,s] <- paste(size.sizes$Irr.Area.Acres)}
    }
  }
  rm(county.ops, county.sizes, county.data)
  
  
  # Summary Table 
  data.summary <- data.frame(County_Name = unique(farm.size$County_Name),
                             County_Code = rep(NA,n.counties),
                             Irrigated_Operations = rep(NA, n.counties),
                             Irrigated_Acreage = rep(NA, n.counties),
                             Size_Bins = rep(NA, n.counties),
                             Ops_Bins = rep(NA, n.counties)
  )
  data.summary$County_Name <- as.character(paste(data.summary$County_Name))
  
  
  for (c in 1:n.counties){
    County_Name<-data.summary$County_Name[c]
    data.summary$County_Code[c] <- as.character(paste(dat_YEAR_QS$County_Code[
      as.character(paste(dat_YEAR_QS$County_Name)) == County_Name]))
    data.summary$Irrigated_Operations[c] <- dat_YEAR_QS$Irr.Ops[
      as.character(paste(dat_YEAR_QS$County_Name)) == County_Name]
    data.summary$Irrigated_Acreage[c] <- dat_YEAR_QS$Irr.Area.Acres[
      as.character(paste(dat_YEAR_QS$County_Name)) == County_Name]
    
    # Binned data
    county.ops<-farm.size.ops[rownames(farm.size.ops) == County_Name,]
    data.summary$Ops_Bins[c] <-  sum(!is.na(county.ops))
    county.sizes<-farm.size.size[rownames(farm.size.size) == County_Name,]
    data.summary$Size_Bins[c]<- sum(!is.na(county.sizes) & county.sizes != -9999)
  }
  
  binned_operations <<- as.data.frame(farm.size.ops[, c(2,3,7,9,10,11,12,5,6,8,1,4)]) # No of operations for each size bin in a county
  binned_irrigated_area  <<- as.data.frame(farm.size.size[, c(2,3,7,9,10,11,12,5,6,8,1,4)]) # Irrigated Acreage for each size bin in a county
  County_Summary <<- data.summary   # Totals Summary
  
  ##write.csv(County_Summary, paste0(WUDR_github,"/csv_files/", YEAR, "County_data_NASS.csv")) 
  
  
  
  ####################################################Bin dat summary
  
  ################################################################################
  ####Bin dat summary ###########################################################
  
  data.ops <- binned_operations
  data.ops <-cbind(rownames(data.ops),data.ops)
  data.size <- binned_irrigated_area
  data.size <-cbind(rownames(data.size),data.size)
  
  # Create bin.char to summarize information for each size bin
  n.sizes <-ncol(data.ops) - 1
  bin.char<-data.frame(size=colnames(data.ops)[2:(n.sizes+1)], 
                       minsize = rep(NA,n.sizes), maxsize = rep(NA,n.sizes),
                       avgsize = rep(NA,n.sizes))
  bin.char$minsize <- c(1, 10, 50, 70, 100, 140, 180, 220, 260, 500, 1000, 2000)
  bin.char$maxsize <- c(9.9, 49.9, 69.9, 99.9, 139, 179, 219, 259, 499, 999, 1999, 5000)
  bin.char$avgsize <- apply(bin.char[,2:3],1,"mean")
  bin.char$size<-as.character(bin.char$size)
  bin.char$cntys_w_data <- NA
  bin.char$cntys_w_Ds <- NA
  bin.char$avg_perc_irr <- NA
  
  # Create an object that lists size groupings in a nice format for plotting
  size.bins <-gsub(pattern="AREA.OPERATED...",replacement="",x=bin.char$size)
  size.bins <-gsub(pattern=".TO.", replacement=" to ", x=size.bins)
  size.bins <-gsub(pattern=".ACRES.", replacement=" acres", x=size.bins)
  size.bins <-gsub(pattern=".OR.MORE.", replacement=" or more ", x=size.bins)
  
  
  
  # Create a table for each bin size to store all county info for farms of that size
  # Store in a list of length 12 
  bin.table <- data.frame(County=as.character(data.ops[,1]), 
                          irr.ops=rep(NA,dim(data.ops)[1]), 
                          irr.acres=rep(NA,dim(data.ops)[1]),
                          irr.AcresperOp=rep(NA,dim(data.ops)[1]),
                          irr.perc=rep(NA,dim(data.ops)[1]))
  bin.list <- rep(list(bin.table),length(size.bins))
  
  for (i in 1:length(size.bins)){
    bin.table <- bin.list[[i]]
    size <- bin.char$size[i]
    bin.table$irr.ops <- data.ops[,colnames(data.ops) == size]
    bin.table$irr.acres <- data.size[,colnames(data.size) == size]
    
    # remove any commas in irr.acres
    bin.table$irr.acres <- gsub(pattern=",",replacement="",x=bin.table$irr.acres)
    
    # How many counties have operations and acreage info
    has.ops <- !is.na(bin.table$irr.ops)
    has.acres <- !is.na(bin.table$irr.ops) & bin.table$irr.acres != "-9999"
    bin.table$irr.AcresperOp[has.acres] <- as.numeric(paste(bin.table$irr.acres[has.acres]))/
      as.numeric(paste(bin.table$irr.ops[has.acres]))
    size.avg <-bin.char$avgsize[i]
    bin.table$irr.perc <- bin.table$irr.AcresperOp/size.avg
    
    # Save info to bin.char
    bin.char$cntys_w_data[i] <- sum(has.acres)
    bin.char$avg_perc_irr[i] <- mean(bin.table$irr.perc,na.rm=TRUE)
    bin.char$cntys_w_Ds[i] <- sum(has.ops) - sum(has.acres)
    
    # create and save figure
    
    # ppi<-300
    # filename<-paste0(WUDR_github,"/plots/Census_data_bin.char/",YEAR,"_",size.bins[i],".png")
    # png(file=filename,width=4*ppi,height=4*ppi,res=ppi)
    # hist(bin.table$irr.perc, main=size.bins[i], xlab="Percent of Farm Irrigated")
    # abline(v=mean(bin.table$irr.perc,na.rm=TRUE), col="red",lwd=2)
    # dev.off()
    
    # Save size category table to list
    bin.list[[i]] <- bin.table
    
    bin.char <<- bin.char
  }
  
  
write.csv(bin.char, "Binned_average,csv")




#####################################################
NB <- nass_binned %>% 
  filter(Year > 2001) %>% 
  filter(Domain != "TOTAL")

NB_NA <- NB %>% 
  filter( Irr.Area.Acres == "-9999")
  