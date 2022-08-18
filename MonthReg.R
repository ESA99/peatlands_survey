
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



stop()

#'@Other_plots_and_ideas...



# N of months with slope
ggplot(s, aes(x = ms, y = slope)) +
     geom_bar(stat = "identity", aes(fill = n))+
     scale_fill_gradientn(colours=c("khaki1","orange3","yellow","lawngreen", "green2", "green4"))+
     coord_polar()+
     theme_minimal()+
     geom_vline(xintercept=2.5)+
     geom_vline(xintercept=6.5)+
     geom_vline(xintercept=10.5)+
     geom_text(aes(label = n), colour = "blue4", size = 5)+
     theme(axis.title.x = element_text(size=18),
           axis.title.y = element_text(size=18),
           axis.text=element_text(size=14),
           legend.position = "none")+
     labs(tag = ifelse(nrow(dat) <= 315, "C", ifelse(nrow(dat) >= 400, "A", "B")))+
     ylab("NDVI increase over 38 years")


# 
ggplot(s, aes(x = ms, y = slope)) +
     geom_bar(stat = "identity", aes(fill = slope))+
     coord_polar()+
     scale_fill_gradientn(colours=c("orange3","khaki1","yellow","lawngreen", "green2"))+
     theme_minimal()+
     theme(axis.title.x=element_blank(),
           legend.position = "none",
           axis.title.y = element_text(size=14))+
     labs(tag = ifelse(nrow(dat) <= 315, "C", ifelse(nrow(dat) >= 400, "A", "B")))+
     ylab("NDVI increase over 38 years")





ggsave("NDMIchange_months.jpg", width = 32 , height = 18, units = "cm", dpi = 400, path = resultpath)




#___________________ Linear models | circular plots ___________________#
ggplot(Cgg, aes(x=date, y=ndmi, color=monthname))+
     geom_smooth(method="lm", se = F,lwd=1)+
     coord_polar()+
     scale_fill_discrete(breaks = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
     scale_color_manual(values=c("blue4","cadetblue1", "cornflowerblue","yellow1","greenyellow","chartreuse3","chartreuse4","chocolate","burlywood2","goldenrod","darkorchid3","deeppink1"))+
     theme_minimal()+
     xlab("Year")+
     ylab("NDVI")+
     theme(legend.title=element_blank(),
           legend.text = element_text(size=14),
           legend.key.size = unit(1.5, 'cm'),
           axis.title.x = element_text(size=18),
           axis.title.y = element_text(size=18),
           axis.text = element_text(size=12),
           axis.text.y = element_text(hjust = 7.4))
     





# bog -> lm
ggplot(Bgg, aes(x=date, y=ndvi, color=monthname))+
     geom_smooth(method="lm", se = F,lwd=1)+
     coord_polar()+
     scale_fill_discrete(breaks = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
     scale_color_manual(values=c("cadetblue1","blue4", "cornflowerblue","yellow1","greenyellow","chartreuse3","chartreuse4","chocolate","burlywood2","goldenrod","darkorchid3","gray85"))+
     theme_minimal()+
     xlab("Year")+
     ylab("NDVI")+
     theme(legend.title=element_blank(),
           axis.title.x = element_text(size=18),
           axis.title.y = element_text(size=18),
           legend.text = element_text(size=14),
           legend.key.size = unit(1.5, 'cm'),
           axis.text=element_text(size=12))+
     labs(title = "Change of the NDVI over time",
          subtitle = "Linear model by month",
          caption = "This is the caption",
          tag = "B")



# bog -> loess (smooth)
ggplot(Bgg, aes(x=date, y=ndvi, color=monthname))+
     geom_smooth(se = F)+
     coord_polar()+
     scale_color_manual(values=c("cadetblue1","blue4", "cornflowerblue","yellow1","greenyellow","chartreuse3","chartreuse4","chocolate","burlywood2","goldenrod","darkorchid3","gray85"))+
     theme_minimal()+
     xlab("Year")+
     ylab("NDVI")+
     ylim(0.1,0.8)+
     theme(legend.title=element_blank(),
           axis.title.x = element_text(size=18),
           axis.title.y = element_text(size=18),
           legend.text = element_text(size=14),
           legend.key.size = unit(1.5, 'cm'),
           axis.text=element_text(size=12))


# L_agg -> lm
ggplot(Agg, aes(x=date, y=ndvi, color=monthname))+
     geom_smooth(method="lm", se = F,lwd=1)+
     coord_polar()+
     scale_fill_discrete(breaks = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
     scale_color_manual(values=c("cadetblue1","blue4", "cornflowerblue","yellow1","greenyellow","chartreuse3","chartreuse4","chocolate","burlywood2","goldenrod","darkorchid3","gray85"))+
     theme_minimal()+
     xlab("Year")+
     ylab("NDVI")+
     theme(legend.title=element_blank(),
           axis.title.x = element_text(size=18),
           axis.title.y = element_text(size=18),
           legend.text = element_text(size=14),
           legend.key.size = unit(1.5, 'cm'),
           axis.text=element_text(size=12))





     





# smooth monthly model
ggplot(Bgg, aes(x = date, y = ndvi, color = monthname))+
     geom_smooth(se=F)


ggsave("name.jpg", width = 32 , height = 18, units = "cm", dpi = 340, path = "C:/Users/esanc/Desktop/Uni/6_Semester/Bachelorarbeit/2.1Results")



#Radar
stop()
{library(ggradar)

years <- data.frame(year = year(seq(as.Date("1984-01-01"), as.Date("2022-01-01"), by="year")))
merge(jan, years, by = "year", all.y = T)[,2]

r <- data.frame(matrix(, nrow = 39, ncol = 1)) # ndmi
names(r) <- c("Year")

r$Year <- year(seq(as.Date("1984-01-01"), as.Date("2022-01-01"), by="year"))
r$jan <- merge(jan, years, by = "year", all.y = T)[,3]
r$feb <- merge(aggregate(feb, by = list(feb$year), FUN = mean), years, by = "year", all.y = T)[,5]
r$mar <- merge(aggregate(mar, by = list(mar$year), FUN = mean), years, by = "year", all.y = T)[,4]
r$apr <- merge(aggregate(apr, by = list(apr$year), FUN = mean), years, by = "year", all.y = T)[,4]
r$may <- merge(aggregate(may, by = list(may$year), FUN = mean), years, by = "year", all.y = T)[,4]
r$jun <- merge(aggregate(jun, by = list(jun$year), FUN = mean), years, by = "year", all.y = T)[,4]
r$jul <- merge(aggregate(jul, by = list(jul$year), FUN = mean), years, by = "year", all.y = T)[,4]
r$aug <- merge(aggregate(aug, by = list(aug$year), FUN = mean), years, by = "year", all.y = T)[,4]
r$sep <- merge(aggregate(sep, by = list(sep$year), FUN = mean), years, by = "year", all.y = T)[,4]
r$oct <- merge(aggregate(oct, by = list(oct$year), FUN = mean), years, by = "year", all.y = T)[,4]
r$nov <- merge(aggregate(nov, by = list(nov$year), FUN = mean), years, by = "year", all.y = T)[,4]
r$dec <- merge(aggregate(dec, by = list(dec$year), FUN = mean), years, by = "year", all.y = T)[,4]

r <- t(r)
r <- as.data.frame(r)
names(r) <- r[1,]
r <- r[-1,]
r <- merge(data.frame(c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")),r)
names(r[,1]) <- "month"

ggradar(r, values.radar = c(0, 0.5, 1))
ggradar(r)

}

# barplot(s$slope, names.arg = s$ms, ylim = c(0,0.015)) # base r barplot



{
     # for (i in 1:12) {
     #      v <- subset(dat, month == i)
     #      mod <- lm(v$ndvi ~ v$year)
     #      
     #      n <- c(n,nrow(v[v$month == i,]))
     #      
     #      slope <- cbind(slope, c(mod$coefficients[2]))
     # } # slope calculation
} # old slope calculation





#__________________________________________________________________________________________________


# R^2 p-value ...

v <- mar # based of plot_agg
m1 <- glm(v$ndvi ~ v$year)
m2 <- lm(v$ndmi ~ v$year)
summary(m1) 
plot(m1)
# plot(m1)
plot(v$year, v$ndvi, col = "green3")
plot(v$year, v$ndmi, col = "blue3")
abline(m1, col = "green3")
abline(m2, col = "blue3")

#january: 10 years but only few points
#march: R2 = 0.8 & p = 6e-11 -> + 0.1 in 20 years
#april: R2 = 0.6 & p = 3e-9 -> + 0.1 in 24 years








# normal plot by month
mar_plot <- ggplot(mar, aes(x = date, y = ndvi)) +
     geom_line()+
     ylim(0.1,0.8)+
     geom_smooth(method = "lm", col = "red", se = F)

jan_plot <- ggplot(jan, aes(x = date, y = ndvi)) +
     geom_line()+
     ylim(0,1)+
     geom_smooth(method = "lm", col = "red", se = F)

# With coord_polar
mar_plot + coord_polar()

