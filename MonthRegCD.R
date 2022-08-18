
#'*Monthly trend analysis*


# Slope by month

dat <- Bgg
ind <- "NDMI"

slope <- c()
n <- c()
ms <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
# years <- seq(as.Date("1984-01-01"), as.Date("2022-01-01"), by="year")

for (i in 1:12) {
     v <- subset(dat, month == i)
     
     yearsdf <- data.frame(year = year(years))
     v <- aggregate.data.frame(v, by = list(v$year), FUN = mean)
     yearsdf <-  merge(v, yearsdf, by = c("year"), all.y = T)
     
     yearsdf$intndmi <-  approxfun(yearsdf$year, yearsdf$ndmi)(yearsdf$year)
     yearsdf$intndvi <-  approxfun(yearsdf$year, yearsdf$ndvi)(yearsdf$year)
     ifelse(length(which(is.na(yearsdf$intndmi))) == 0, "No NA", yearsdf <- yearsdf[-c(which(is.na(yearsdf$intndmi))),] )
     
     ydf_ts <- ts(yearsdf[,ifelse(ind == "NDVI", ncol(yearsdf), ncol(yearsdf)-1)], deltat = 12/12, start = 1984)
     tsp(ydf_ts) <- tsp(ydf_ts)
     
     fit <- bfast(ydf_ts, h = 0.3, season = "none", max.iter = 10)
     
     
     slope <- cbind(slope, sum(diff(fit$output[[1]]$Tt[1:length(fit$output[[1]]$Tt)])/
                                    diff(time(fit$output[[1]]$Tt)[1:length(fit$output[[1]]$Tt)])))
     
     n <- c(n,nrow(v[v$month == i,]))
     
}

slope <- slope[1:12]
s <- data.frame(ms, slope, n)
s$ms <- factor(s$ms, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
s$pos <- ifelse(s$slope >=0, TRUE, FALSE)
# s$slope[12] <- 0
# s$slope[1] <- 0



#'*________________Change intensities by month________________*

ggplot(s, aes(x = ms, y = slope)) +             
     geom_bar(stat = "identity", aes(fill = n))+
     scale_fill_gradientn(colors = colorRampPalette(c("aliceblue","lightskyblue","royalblue","navy"))(200))+
     coord_polar()+
     theme_minimal()+
     geom_vline(xintercept=2.5)+
     geom_vline(xintercept=6.5)+
     geom_vline(xintercept=10.5)+
     theme(axis.title.x = element_text(size=18),
           axis.title.y = element_text(size=18, vjust = 10, hjust = 0.76),
           axis.text=element_text(size=14),
           legend.key.size = unit(1.5, 'cm'),
           legend.title = element_text(size=14),
           legend.text = element_text(size=12),
           plot.tag.position = "bottomleft")+
     labs(fill="Number of \nobservations",
          tag = ifelse(nrow(dat) <= 315, "C", ifelse(nrow(dat) >= 400, "A", "B")))+
     ylab("NDVI increase over 38 years")


# Plot for NDMI change
ggplot(s, aes(x = ms, y = slope)) +             
     geom_bar(stat = "identity", fill = "grey73")+
     theme_minimal()+
     geom_hline(yintercept=0)+
     theme(plot.tag.position = "topleft",
           axis.title.x=element_blank())+
     geom_text(aes(label = n), colour = "black", size = 5, vjust = -1)+
     labs(#fill="Change",
          # tag = ifelse(nrow(dat) <= 315, "C", ifelse(nrow(dat) >= 400, "A", "B")),
          caption = "Count = Number of observations",
          tag = ifelse(nrow(dat) <= 315, "C", ifelse(nrow(dat) >= 400, "A", "B")))+
     ylab("NDMI change over time series")+
     ylim(-0.32,0.32)

# coloured version
ggplot(s, aes(x = ms, y = slope, fill = pos)) +             
     geom_bar(stat = "identity")+
     theme_minimal()+
     geom_hline(yintercept=0)+
     theme(plot.tag.position = "topleft",
           axis.title.x=element_blank(),
           legend.position = "none")+
     geom_text(aes(label = n), colour = "black", size = 5, vjust = -1)+
     labs(#fill="Change",
          # tag = ifelse(nrow(dat) <= 315, "C", ifelse(nrow(dat) >= 400, "A", "B")),
          caption = "Count = Number of observations",
          tag = ifelse(nrow(dat) <= 315, "C", ifelse(nrow(dat) >= 400, "A", "B")))+
     ylab("NDMI change over time series")
     # ylim(-0.32,0.32)
     # ylim(-0.0000002,0.0000002)



ggarrange(a, b, c, ncol = 1, nrow = 3, common.legend = T)
ggsave("NDMIchange_months.jpg", width = 18 , height = 27, units = "cm", dpi = 350, path = resultpath)



