
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
     
     u$season <- ifelse(u$month >= 1 & u$month <= 3, "1", #spring
                        ifelse(u$month >= 4 & u$month <= 6, "2", #summer
                               ifelse(u$month >= 7 & u$month <= 9, "3", "4")))
     
     u$season <- as.numeric(paste0(u$year, ".", u$season))
     
     u <- u[,-c(1,2,3,4,5,6,7,8,14,15)]
     
     return(u) 
     
}

Agg <- timesprep(A_agg)
Bgg <- timesprep(B_agg)
Cgg <- timesprep(C_agg)


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

     
# Time steps
days <- seq(as.Date("1984-03-16"), as.Date("2022-05-01"), by="days")
months <- seq(as.Date("1984-03-01"), as.Date("2022-04-01"), by="months")
quarter <- seq(as.Date("1984-03-01"), as.Date("2022-04-01"), by="quarter")
years <- seq(as.Date("1984-01-01"), as.Date("2022-01-01"), by="year")



resultpath <- "C:/Users/esanc/Desktop/Uni/6_Semester/Bachelorarbeit/2.1Results/"

