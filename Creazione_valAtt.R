library(tmap) 
library(sf)
library(ggplot2)
library(dplyr)
library(lmtest)
library(tidyverse)

  nyc_map <- sf::read_sf("DatiNewYork/nycdta2020_24a/nycdta2020.shp") %>%
                        st_transform(4326) %>%
                        subset( ,select = c("geometry", "CDTA2020"))

  nyc_Network <- sf::read_sf("DatiNewYork/NYC_roadNetwork/DCM_StreetCenterLine.shp") %>%
                                                              st_transform(4326)  
# dati presi da https://www.nyc.gov/site/planning/data-maps/open-data/dwn-digital-city-map.page
tot_road_m <- sum(st_length(nyc_Network))
nyc_Network <- subset(nyc_Network, select = c("Borough","Street_NM", "geometry", "Streetwidt"))



    nyc_map %>%  count(CDTA2020) %>% filter(n > 1) 
#CDTA2020 è univoco
  
  nyc_Network %>%  group_by(Street_NM) %>% filter(n() > 1)
#View(nyc_Network)

tm_shape(nyc_Network, color = "red") +
  tm_lines(col = "red") +
  tm_shape(nyc_map) +
  tm_borders() 

data_mt <- data.frame(matrix(nrow = nrow(nyc_map), ncol = 2))
 colnames(data_mt) <- c("PrimaryKey", "Mt Strada")
 
for(i in 1:length(WorkData$PrimaryKey)){
  result <- st_intersection(nyc_Network$geometry, nyc_map$geometry[i])
    data_mt[i, 1] <- nyc_map$PrimaryKey[i]
    data_mt[i,2] <- sum(st_length(result))
}
 
head(data_mt)
paste("Riportati il (%)",sum(data_mt[2])/tot_road_m * 100, "dei kilometri")

 
WorkData <- full_join(WorkData, data_mt, by = "PrimaryKey") 

plot(y = WorkData$count, x = WorkData$`Mt Strada`) +
  abline(a = 0, b = 0)                       



tot_ev <- sum(MapThis$count) #totale eventi, as in, totale eventi (no numero di pedoni feriti/ persone coinvolte ecc)
 tot_ev
tot_mt <- sum(WorkData$`Mt Strada`)
rapp_ev_mt <- tot_ev / tot_mt
  rapp_ev_mt

  
WorkData$ei_mts <- WorkData$`Mt Strada` * rapp_ev_mt

  WorkData$count <- as.double(WorkData$count)
WorkData$diff <- as.numeric(WorkData$count) - as.numeric(WorkData$ei_mts)  #as.nmeric() per--->Error in Ops.units(WorkData$ei_mt, WorkData$count) : both operands of the expression should be "units" objects 

plot(y = WorkData$diff, x = WorkData$ei_mts) +
  abline(a = 0, b = 0)

 #direi abbastanza omoschedastico 
# provo regressione, aggingo intercetta 

 #dev.off()

obj <- lm(WorkData, formula = count ~ ei_mts)
 summary(obj) 
 
WorkData$ei_reg1 <- obj$coefficients[1] +  as.numeric(WorkData$ei_mts) * obj$coefficients[2] 
 
WorkData$diff_reg1 <- as.numeric(WorkData$count) - as.numeric(WorkData$ei_reg1)  #as.numeric() per--->Error in Ops.units(WorkData$ei_mt, WorkData$count) : both operands of the expression should be "units" objects 

plot(x = WorkData$ei_reg1, y = WorkData$diff_reg1) +
  abline(a = 0, b = 0) 

bptest(obj)


#meglio
 
path <- "DatiNewYork/DatiModello.csv"
write.csv(WorkData, file = path, row.names = FALSE)

rm(list =ls())


