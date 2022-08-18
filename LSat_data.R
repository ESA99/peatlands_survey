{# install.packages("devtools")
     # library(devtools)
     # 
     # install.packages("greenbrown", repos="http://R-Forge.R-project.org")
     # library(greenbrown)
     # 
     # install.packages("gdalUtils")
     # 
     # # Dependencis in Error message need to be installed first
     #   # 'raster', 'zoo', 'Kendall', 'bfast', 'phenopix', 'quantreg' 'forecast'
     # 
     # # install.packages("greenbrown_2.5.0.tar.gz", repos = NULL, type="source")
     # 
     # 
     # # library(installr)
     # # install.Rtools()
     # # print(R.Version())
     # # updateR()
     
     
     # install.packages("bfast", repos="http://R-Forge.R-project.org")
}
# Load packages
{    library(sf)
     library(bfast)
     library(raster)
     library(mapview)
     library(lubridate)
     library(ggplot2)
     library(corrplot)
     library(ggpubr)
     library(dplyr)
}

setwd("~/R/Projects/Landsat_AV/data")

#________Load and prepare data_______________

A <- read.csv("AV_L_clip.csv")
B <- read.csv("L_AV_Bog_new.csv")
C <- read.csv("L_AV_bog_plotR_new.csv")

# AV <- st_read("C:/Users/esanc/Desktop/Uni/6_Semester/Bachelorarbeit/3.QGIS/AV_dissolved.shp") # Shape of Amtsvenn

A <- subset (A, select = -c(3,5,6))


data.prep <- function(a){
     
     # coordinates
     a$coordinates <- substr(a$.geo, 49, nchar(a$.geo)-2)
     s <- strsplit(a$coordinates, split=',', fixed=TRUE)
     
     cdf <- data.frame(t(sapply(s,c)))
     names(cdf) <- c("E", "N")
     
     a$N <- as.numeric(cdf$N)
     a$E <- as.numeric(cdf$E)
     
     rm(list = "cdf")
     
     # dates
     end7 <- length(which(substr(a$system.index, 1, 1) == 1))
     dates <- c(substr(a$system.index[1:end7], 17, 24), substr(a$system.index[(end7+1):nrow(a)], 15, 22))
     a$date <- ymd(dates) 
     a$year <- year(a$date)
     a$month <- month(a$date)
     
     a$season <- ifelse(month(a$date) >= 3 & month(a$date) <= 5, "spring", 
                        ifelse(month(a$date) >= 6 & month(a$date) <= 8, "summer",
                               ifelse(month(a$date) >= 9 & month(a$date) <= 11, "autumn", "winter")))
     
     return(a)     
}

A <- data.prep(A)
B <- data.prep(B)
C <- data.prep(C)


# Dates and coordinates
Adates <- unique(A$date)
Acoordinates <- unique(A$coordinates)

Bdates <- unique(B$date)
Bcoordinates <- unique(B$coordinates)

Cdates <- unique(C$date)
Ccoordinates <- unique(C$coordinates)



# Aggregate
# plot_agg <- aggregate(C, by = list(C$date), FUN = median)            ## aggregate function different !
A_agg <- aggregate.data.frame(A, by = list(A$date), FUN = median)
B_agg <- aggregate.data.frame(B, by = list(B$date), FUN = mean)
C_agg <- aggregate.data.frame(C, by = list(C$date), FUN = mean)


# Timeseries preperation
timesprep <- function(u){

     u$monthname <-      ifelse(u$month == 1, "Jan",
                                ifelse(u$month == 2, "Feb",
                                       ifelse(u$month == 3, "Mar",
                                              ifelse(u$month == 4, "Apr",
                                                     ifelse(u$month == 5, "May",
                                                            ifelse(u$month == 6, "Jun",
                                                                   ifelse(u$month == 7, "Jul",
                                                                          ifelse(u$month == 8, "Aug",
                                                                                 ifelse(u$month == 9, "Sep",
                                                                                        ifelse(u$month == 10, "Oct",
                                                                                               ifelse(u$month == 11, "Nov",
                                                                                                      ifelse(u$month == 12, "Dec", NA))))))))))))


     u$monthname <- factor(u$monthname, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

     u$season <- ifelse(u$month >= 3 & u$month <= 5, "1", #spring
                        ifelse(u$month >= 6 & u$month <= 8, "2", #summer
                               ifelse(u$month >= 9 & u$month <= 11, "3", "4")))

     u$season <- as.numeric(paste0(u$year, ".", u$season))

     u <- u[,-c(1,2,3,4,5,6,7,8,14,15)]

     return(u)

}

# Agg <- timesprep(A_agg)
# Bgg <- timesprep(B_agg)
# Cgg <- timesprep(C_agg)

timesprep2 <- function(u){
     
     u$monthname <-      ifelse(u$month == 1, "Jan", 
                                ifelse(u$month == 2, "Feb",
                                       ifelse(u$month == 3, "Mar", 
                                              ifelse(u$month == 4, "Apr",
                                                     ifelse(u$month == 5, "May",
                                                            ifelse(u$month == 6, "Jun",
                                                                   ifelse(u$month == 7, "Jul",
                                                                          ifelse(u$month == 8, "Aug",
                                                                                 ifelse(u$month == 9, "Sep",
                                                                                        ifelse(u$month == 10, "Oct",
                                                                                               ifelse(u$month == 11, "Nov",
                                                                                                      ifelse(u$month == 12, "Dec", NA))))))))))))
     
     
     u$monthname <- factor(u$monthname, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
     
     u$season <- ifelse(u$month >= 1 & u$month <= 3, "1", #spring
                        ifelse(u$month >= 4 & u$month <= 6, "2", #summer
                               ifelse(u$month >= 7 & u$month <= 9, "3", "4")))
     
     u$season <- as.numeric(paste0(u$year, ".", u$season))
     
     u <- u[,-c(1,2,3,4,5,6,7,8,14,15)]
     
     return(u) 
     
}

Agg <- timesprep2(A_agg)
Bgg <- timesprep2(B_agg)
Cgg <- timesprep2(C_agg)


#________sf objects________#
a <- st_as_sf(a, coords = c("E", "N"), crs = 4326)


# Singel point
point10 <- B[B$coordinates == bogcoordinates[868],]
droplevels(point10)

point10sf <- st_as_sf(point10, coords = c("E", "N"), crs = 4326)
mapview(point10sf)


june <- st_as_sf(jun, coords = c("E", "N"), crs = 4326) #UTM zone 32U -> WGS84


Point1 <- subset(A[A$coordinates == A$coordinates[14500],])
plot(Point1$ndvi, type = "l")
Point1sf <- st_as_sf(Point1, coords = c("E", "N"), crs = 4326)


summer <- A[A$season == "summer",]
summersf <- st_as_sf(summer, coords = c("E", "N"), crs = 4326)




# export
# write.table(B, file = "C:/Users/esanc/Desktop/B.txt", sep = ";", dec = ".", row.names = F, col.names = T)

resultpath <- "C:/Users/esanc/Desktop/Uni/6_Semester/Bachelorarbeit/2.1Results/"

stop()
# monthly subsets
dset <- B_agg

     jan <- subset(dset, month == 1)[,c(12,13,18,19)]
     feb <- subset(dset, month == 2)[,c(12,13,18,19)]
     mar <- subset(dset, month == 3)[,c(12,13,18,19)]
     apr <- subset(dset, month == 4)[,c(12,13,18,19)]
     may <- subset(dset, month == 5)[,c(12,13,18,19)]
     jun <- subset(dset, month == 6)[,c(12,13,18,19)]
     jul <- subset(dset, month == 7)[,c(12,13,18,19)]
     aug <- subset(dset, month == 8)[,c(12,13,18,19)]
     sep <- subset(dset, month == 9)[,c(12,13,18,19)]
     oct <- subset(dset, month == 10)[,c(12,13,18,19)]
     nov <- subset(dset, month == 11)[,c(12,13,18,19)]
     dec <- subset(dset, month == 12)[,c(12,13,18,19)]
     
{    
     plot(jan$date, jan$ndvi, ylim = c(0,1))
     abline(lm(jan$ndvi ~ jan$date), col = "blue")
     abline(lm(feb$ndvi ~ feb$date), col = "lightblue")
     abline(lm(mar$ndvi ~ mar$date), col = "yellow")
     abline(lm(apr$ndvi ~ apr$date), col = "purple")
     abline(lm(may$ndvi ~ may$date), col = "lightgreen")
     abline(lm(jun$ndvi ~ jun$date), col = "darkgreen")
     abline(lm(jul$ndvi ~ jul$date), col = "darkred")
     abline(lm(aug$ndvi ~ aug$date), col = "brown")
     abline(lm(sep$ndvi ~ sep$date), col = "brown4")
     abline(lm(oct$ndvi ~ oct$date), col = "orange")
     abline(lm(nov$ndvi ~ nov$date), col = "grey")
     abline(lm(dec$ndvi ~ dec$date), col = "navy")}




