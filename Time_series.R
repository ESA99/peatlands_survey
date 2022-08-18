
#____Mean by date and time series for bfast______#

# Time steps
days <- seq(as.Date("1984-03-16"), as.Date("2022-05-01"), by="days")
months <- seq(as.Date("1984-03-01"), as.Date("2022-04-01"), by="months")
quarter <- seq(as.Date("1984-03-01"), as.Date("2022-04-01"), by="quarter")
years <- seq(as.Date("1984-01-01"), as.Date("2022-01-01"), by="year")




# _____Quartal time series_______

Q <- Cgg
i <- "NDVI"

quarterdf <- as.data.frame(quarter)
names(quarterdf) <- c("date")
quarterdf$season <- c(rep(c("1","2","3","4"), 38), "1")
quarterdf$season <- paste0(year(quarterdf$date),".", quarterdf$season)

Q <- aggregate.data.frame(Q[,c(1:11)], by = list(Q$season), FUN = median)  ###'*this is important*
Q <- Q[,c(5,6,12)]

quarterdf <- merge(Q, quarterdf, by = c("season"), all.y = T)
       quarterdf <- quarterdf[!duplicated(quarterdf$season),]

quarterdf$intndmi <-  approxfun(quarterdf$season, quarterdf$ndmi)(quarterdf$season)
quarterdf$intndvi <-  approxfun(quarterdf$season, quarterdf$ndvi)(quarterdf$season)
ifelse(length(which(is.na(quarterdf$intndmi))) == 0, "No NA", quarterdf <- quarterdf[-c(which(is.na(quarterdf$intndvi))),] )

     # Bfast application of quarterly data
     qutr_ts <- ts(quarterdf[,ifelse(i == "NDVI", ncol(quarterdf), ncol(quarterdf)-1)], deltat = 1/4, start = 1984)
     tsp(qutr_ts) <- tsp(qutr_ts)
     plot(qutr_ts)
     
     fit <- bfast(qutr_ts, h = 0.05, season = "harmonic", max.iter = 200)
     plot(fit, sim = NULL, xlab = "Time [years]",
          main = paste0("Quarterly time series | plot: ", ifelse(nrow(Q) <= 127, "C", ifelse(nrow(Q) >= 135, "A", "B")) ,
                                         " | median ", i))
fit


# slope
sum(diff(fit$output[[1]]$Tt[1:length(fit$output[[1]]$Tt)])/
         diff(time(fit$output[[1]]$Tt)[1:length(fit$output[[1]]$Tt)]))*1/4 

sum(diff(fit$output[[1]]$Tt[86:length(fit$output[[1]]$Tt)])/
         diff(time(fit$output[[1]]$Tt)[86:length(fit$output[[1]]$Tt)]))*1/4 

     


#_______Monthly time series__________
     
M <- Agg
i <- "NDMI"

monthsdf <- as.data.frame(months)
names(monthsdf) <- c("date")
                                                       ######'*aggregate by month needed*
M$date <- ym(paste0(year(M$date),"-",month(M$date)))
M <- aggregate.data.frame(M, by = list(M$date), FUN = mean)

monthsdf <- merge(M, monthsdf, by = c("date"), all.y = T)
monthsdf <- monthsdf[!duplicated(monthsdf$date),]


monthsdf$intndmi <-  approxfun(monthsdf$date, monthsdf$ndmi)(monthsdf$date)
monthsdf$intndvi <-  approxfun(monthsdf$date, monthsdf$ndvi)(monthsdf$date)
ifelse(length(which(is.na(monthsdf$intndmi))) == 0, "No NA", monthsdf <- monthsdf[-c(which(is.na(monthsdf$intndvi))),] )

     # Bfast application of monthly data
     mdf_ts <- ts(monthsdf[,ifelse(i == "NDVI", ncol(monthsdf), ncol(monthsdf)-1)], deltat = 1/12, start = 1984)
     tsp(mdf_ts) <- tsp(mdf_ts)
     plot(mdf_ts)
     
     fit <- bfast(mdf_ts, h = 0.1, season = "harmonic", max.iter = 10)
     plot(fit, sim = NULL, main = paste0("Monthly time series | plot: ", ifelse(nrow(M) <= 224, "C", ifelse(nrow(M) >= 228, "A", "B")) ,
                                         " | median ", i))
     # Plot aggregated by date (median) -> df with all months -> merged -> NA´s interpolated (210/458)
fit

# slope
sum(diff(fit$output[[1]]$Tt[1:length(fit$output[[1]]$Tt)])/
         diff(time(fit$output[[1]]$Tt)[1:length(fit$output[[1]]$Tt)]))*1/12 

     
     

#_________Time series for one month__________

M <- may
i <- "NDVI"
     
yearsdf <- data.frame(year = year(years))
yearsdf <-  merge(aggregate.data.frame(M, by = list(M$year), FUN = mean), yearsdf, by = c("year"), all.y = T)
     
yearsdf$intndmi <-  approxfun(yearsdf$year, yearsdf$ndmi)(yearsdf$year)
yearsdf$intndvi <-  approxfun(yearsdf$year, yearsdf$ndvi)(yearsdf$year)

ifelse(length(which(is.na(yearsdf$intndmi))) == 0, "No NA", yearsdf <- yearsdf[-c(which(is.na(yearsdf$intndmi))),] )

     # Bfast application of monthly data
     ydf_ts <- ts(yearsdf[,ifelse(i == "NDVI", ncol(yearsdf), ncol(yearsdf)-1)], deltat = 12/12, start = 1984)
     tsp(ydf_ts) <- tsp(ydf_ts)
     plot(ydf_ts)
     
     fit <- bfast(ydf_ts, h = 0.3, season = "none", max.iter = 20)
     eplot(fit, sim = NULL, main = "April | B | mean ndvi", sub = "h = 0.3",
          type = "trend", xlab = "Time [years]")
     # Plot aggregated by date (median) -> df with all months -> merged -> NA´s interpolated (210/458)
     fit


     
     


# Slope for the whole time series
sum(diff(fit$output[[1]]$Tt[1:length(fit$output[[1]]$Tt)])/
         diff(time(fit$output[[1]]$Tt)[1:length(fit$output[[1]]$Tt)])) * 1/4      # Quarterly: 1/4    Monthly: 1/12   Y: 1

# slope between first two points
diff(fit$output[[1]]$Tt[1:2])/diff(time(fit$output[[1]]$Tt)[1:2])
# slope at every time step
diff(fit$output[[1]]$Tt[1:length(fit$output[[1]]$Tt)])/
     diff(time(fit$output[[1]]$Tt)[1:length(fit$output[[1]]$Tt)])
# slope at every time step -> scaled to length of time series
diff(fit$output[[1]]$Tt[1:length(fit$output[[1]]$Tt)])/
     diff(time(fit$output[[1]]$Tt)[1:length(fit$output[[1]]$Tt)]) * length(fit$output[[1]]$Tt)



#diagonal line in plot
# segments(1984,0.23,2020,0.422565)