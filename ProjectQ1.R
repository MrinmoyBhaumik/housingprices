library(ggplot2)
library(dplyr)
library(GGally)
library(tidyverse)
library(stringr)
library(MASS)
library(olsrr)
Df = read.csv("C:/Users/Mrinmoy/Documents/School/Applied Statistics/Project 1/modelingData.csv")
sum(is.na(Df))
head(Df)
#Convert timestamp to date type
Df$timestamp <- do.call("c",lapply(Df$timestamp, function(x) as.Date(x, origin = "1899-12-30")))
#Remove id columns
Df$id <- NULL
Df$ID_railroad_station_walk <- NULL
#Checking which columns contain NA values
colnames(Df)[colSums(is.na(Df)) > 0]
#Mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
#Mode when number of NA values is the greatest
Mode <- function(x) {
  ux <- na.omit(unique(x) )
  tab <- tabulate(match(x, ux)); ux[tab == max(tab) ]
}
#Replace NA with mean for column
Df$life_sq[is.na(Df$life_sq)] <- mean(Df$life_sq, na.rm = TRUE)
Df$kitch_sq[is.na(Df$kitch_sq)] <- round(mean(Df$kitch_sq, na.rm = TRUE))
Df$preschool_quota[is.na(Df$preschool_quota)] <- round(mean(Df$preschool_quota, na.rm = TRUE))
Df$hospital_beds_raion[is.na(Df$hospital_beds_raion)] <- round(mean(Df$hospital_beds_raion, na.rm = TRUE))
Df$build_count_brick[is.na(Df$build_count_brick)] <- round(mean(Df$build_count_brick, na.rm = TRUE))
Df$build_count_wood[is.na(Df$build_count_wood)] <- round(mean(Df$build_count_wood, na.rm = TRUE))
Df$build_count_frame[is.na(Df$build_count_frame)] <- round(mean(Df$build_count_frame, na.rm = TRUE))
Df$build_count_block[is.na(Df$build_count_block)] <- round(mean(Df$build_count_block, na.rm = TRUE))
Df$build_count_before_1920[is.na(Df$build_count_before_1920)] <- round(mean(Df$build_count_before_1920, na.rm = TRUE))
Df$build_count_1921.1945[is.na(Df$build_count_1921.1945)] <- round(mean(Df$build_count_1921.1945, na.rm = TRUE))
Df$build_count_1946.1970[is.na(Df$build_count_1946.1970)] <- round(mean(Df$build_count_1946.1970, na.rm = TRUE))
Df$build_count_1971.1995[is.na(Df$build_count_1971.1995)] <- round(mean(Df$build_count_1971.1995, na.rm = TRUE))
Df$build_count_after_1995[is.na(Df$build_count_after_1995)] <- round(mean(Df$build_count_after_1995, na.rm = TRUE))
Df$metro_min_walk[is.na(Df$metro_min_walk)] <- mean(Df$metro_min_walk, na.rm = TRUE)
Df$metro_km_walk[is.na(Df$metro_km_walk)] <- mean(Df$metro_km_walk, na.rm = TRUE)
Df$railroad_station_walk_km[is.na(Df$railroad_station_walk_km)] <- mean(Df$railroad_station_walk_km, na.rm = TRUE)
Df$railroad_station_walk_min[is.na(Df$railroad_station_walk_min)] <- mean(Df$railroad_station_walk_min, na.rm = TRUE)
#Replace NA with mode for column
Df$floor[is.na(Df$floor)] <- getmode(Df$floor)
Df$max_floor[is.na(Df$max_floor)] <- Mode(Df$max_floor)
Df$material[is.na(Df$material)] <- getmode(Df$material)
Df$build_year[is.na(Df$build_year)] <- Mode(Df$build_year)
Df$num_room[is.na(Df$num_room)] <- Mode(Df$num_room)
#Checking which columns again for NA values
colnames(Df)[colSums(is.na(Df)) > 0]
#Using step-wise variable selection to decrease number of variables
full.model <- lm(price_doc~.,data=Df)
step.model <- stepAIC(full.model,direction = "both",trace = FALSE)
summary(step.model)
#Checking assumptions of OLS Stepwise Model
par(mfrow=c(2,2))
plot(step.model)

#Data set created to check multi-colinearity between explanatory variables
Df2 = Df %>% dplyr::select(price_doc,timestamp,full_sq,life_sq,floor,max_floor,material,num_room,product_type,raion_popul,green_zone_part,
                           indust_part,preschool_quota,children_school,hospital_beds_raion,healthcare_centers_raion,university_top_20_raion,
                           shopping_centers_raion,office_raion,railroad_terminal_raion,full_all,X16_29_all,build_count_block,build_count_wood,build_count_frame,
                           build_count_brick,build_count_before_1920,build_count_1921.1945,build_count_1946.1970,build_count_after_1995,
                           build_count_1921.1945,build_count_after_1995,metro_min_avto,metro_km_avto,metro_min_walk,park_km,
                           green_zone_km,industrial_km,railroad_station_walk_km,railroad_station_avto_km,public_transport_station_km,kremlin_km,
                           big_road1_km,big_road2_km,railroad_km,bus_terminal_avto_km,big_market_km,fitness_km,
                           ice_rink_km,stadium_km,basketball_km,public_healthcare_km,university_km,office_km)
write.csv(Df2,"C:/Users/Mrinmoy/Documents/School/Applied Statistics/Project 1/Df2.csv")

# Data set created after removing correlated explanatory variables
Df3 = Df %>% dplyr::select(price_doc,timestamp,full_sq,life_sq,floor,max_floor,material,num_room,product_type,raion_popul,green_zone_part,
                           indust_part,preschool_quota,hospital_beds_raion,healthcare_centers_raion,university_top_20_raion,
                           shopping_centers_raion,office_raion,railroad_terminal_raion,full_all,X16_29_all,build_count_block,build_count_frame,
                           build_count_before_1920,build_count_1946.1970,build_count_after_1995,
                          build_count_after_1995,metro_min_avto,park_km,
                           green_zone_km,industrial_km,railroad_station_avto_km,public_transport_station_km,
                           big_road1_km,big_road2_km,railroad_km,bus_terminal_avto_km,big_market_km,fitness_km,
                           ice_rink_km,university_km,office_km)
write.csv(Df3,"C:/Users/Mrinmoy/Documents/School/Applied Statistics/Project 1/Df3.csv")
#Clean the projection data as well as remove NA values
projectionData = read.csv("C:/Users/Mrinmoy/Documents/School/Applied Statistics/Project 1/projectionData.csv")
projectionData$timestamp <- do.call("c",lapply(projectionData$timestamp, function(x) as.Date(x, origin = "1899-12-30")))
colnames(projectionData)[colSums(is.na(projectionData)) > 0]
projectionData$ID_railroad_station_walk <- NULL
projectionData$life_sq[is.na(projectionData$life_sq)] <- mean(projectionData$life_sq, na.rm = TRUE)
projectionData$kitch_sq[is.na(projectionData$kitch_sq)] <- round(mean(projectionData$kitch_sq, na.rm = TRUE))
projectionData$preschool_quota[is.na(projectionData$preschool_quota)] <- round(mean(projectionData$preschool_quota, na.rm = TRUE))
projectionData$hospital_beds_raion[is.na(projectionData$hospital_beds_raion)] <- round(mean(projectionData$hospital_beds_raion, na.rm = TRUE))
projectionData$build_count_brick[is.na(projectionData$build_count_brick)] <- round(mean(projectionData$build_count_brick, na.rm = TRUE))
projectionData$build_count_wood[is.na(projectionData$build_count_wood)] <- round(mean(projectionData$build_count_wood, na.rm = TRUE))
projectionData$build_count_frame[is.na(projectionData$build_count_frame)] <- round(mean(projectionData$build_count_frame, na.rm = TRUE))
projectionData$build_count_block[is.na(projectionData$build_count_block)] <- round(mean(projectionData$build_count_block, na.rm = TRUE))
projectionData$build_count_before_1920[is.na(projectionData$build_count_before_1920)] <- round(mean(projectionData$build_count_before_1920, na.rm = TRUE))
projectionData$build_count_1921.1945[is.na(projectionData$build_count_1921.1945)] <- round(mean(projectionData$build_count_1921.1945, na.rm = TRUE))
projectionData$build_count_1946.1970[is.na(projectionData$build_count_1946.1970)] <- round(mean(projectionData$build_count_1946.1970, na.rm = TRUE))
projectionData$build_count_1971.1995[is.na(projectionData$build_count_1971.1995)] <- round(mean(projectionData$build_count_1971.1995, na.rm = TRUE))
projectionData$build_count_after_1995[is.na(projectionData$build_count_after_1995)] <- round(mean(projectionData$build_count_after_1995, na.rm = TRUE))
projectionData$metro_min_walk[is.na(projectionData$metro_min_walk)] <- mean(projectionData$metro_min_walk, na.rm = TRUE)
projectionData$metro_km_walk[is.na(projectionData$metro_km_walk)] <- mean(projectionData$metro_km_walk, na.rm = TRUE)
projectionData$railroad_station_walk_km[is.na(projectionData$railroad_station_walk_km)] <- mean(projectionData$railroad_station_walk_km, na.rm = TRUE)
projectionData$railroad_station_walk_min[is.na(projectionData$railroad_station_walk_min)] <- mean(projectionData$railroad_station_walk_min, na.rm = TRUE)
#Replace NA with mode for column
projectionData$floor[is.na(projectionData$floor)] <- getmode(projectionData$floor)
projectionData$max_floor[is.na(projectionData$max_floor)] <- Mode(projectionData$max_floor)
projectionData$material[is.na(projectionData$material)] <- getmode(projectionData$material)
projectionData$build_year[is.na(projectionData$build_year)] <- Mode(projectionData$build_year)
projectionData$num_room[is.na(projectionData$num_room)] <- Mode(projectionData$num_room)
colnames(projectionData)[colSums(is.na(projectionData)) > 0]
write.csv(projectionData,"C:/Users/Mrinmoy/Documents/School/Applied Statistics/Project 1/projectionData1.csv")
#Calculate prediction with LASSO Model using R
df4 <- read.csv("C:/Users/Mrinmoy/Documents/School/Applied Statistics/Project 1/df4.csv", header = T)
LASSOModel <- lm(logprice_doc~ full_sq+num_room+university_km+office_km+metro_min_avto +
              raion_popul+ fitness_km+healthcare_centers_raion+railroad_station_avto_km+ big_road1_km +
                 life_sq + indust_part+floor+hospital_beds_raion+green_zone_km+ big_market_km+
                 green_zone_part+build_count_after_1995+office_raion+build_count_frame+max_floor+ ice_rink_km+
                 industrial_km+build_count_block+railroad_km, data = df4)
pred<-predict(LASSOModel,projectionData,se.fit = TRUE)
pred$fit <- exp(pred$fit)
pred$fit
submission <- as.data.frame(pred$fit)

write.csv(submission,"C:/Users/Mrinmoy/Documents/School/Applied Statistics/Project 1/submission.csv")


options(scipen=999)  # turn-off scientific notation like 1e+48
theme_set(theme_bw())  # pre-set the bw theme.
# midwest <- read.csv("http://goo.gl/G1K41K")  # bkup data source

# Scatterplot
gg <- ggplot(df4, aes(x=railroad_station_avto_km, y=logprice_doc)) + 
  geom_point(aes(col=product_type)) + 
  geom_smooth(method="lm", se=F) + 
  xlim(c(0, 25)) + 
  ylim(c(13, 18.5)) + 
  labs(subtitle="Distance From Railroad Station Vs log(Sale Price)", 
       y="log (Sale Price)", 
       x="Distance (km)", 
       title="Sale Price vs Railroad Station Distance", 
       caption = "Source: Housing Data")

plot(gg)

gg1 <- ggplot(df4, aes(x=green_zone_part, y=logprice_doc)) + 
  geom_point(aes(col=product_type, size=num_room)) + 
  geom_smooth(method="lm", se=F) + 
  xlim(c(0, 1)) + 
  ylim(c(13, 18.5)) + 
  labs(subtitle="Green Zone Proportions Vs log(Sale Price)", 
       y="log (Sale Price)", 
       x="Green Zone Proportions", 
       title="Sale Price vs Green Zone", 
       caption = "Source: Housing Data")

plot(gg1)


gg2 <- ggplot(df4, aes(x=green_zone_part, y=logprice_doc)) + 
  geom_point(aes(col=product_type, size=num_room)) + 
  geom_smooth(method="lm", se=F) + 
  xlim(c(0, 1)) + 
  ylim(c(13, 18.5)) + 
  labs(subtitle="Green Zone Proportions Vs log(Sale Price)", 
       y="log (Sale Price)", 
       x="Green Zone Proportions", 
       title="Sale Price vs Green Zone", 
       caption = "Source: Housing Data")

plot(gg2)



gg3 <- ggplot(df4, aes(x=office_km, y=logprice_doc)) + 
  geom_point(aes(col=product_type)) + 
  geom_smooth(method="lm", se=F) + 
  xlim(c(0, 19)) + 
  ylim(c(13, 18.5)) + 
  labs(subtitle="Distance To Office Vs log(Sale Price)", 
       y="log (Sale Price)", 
       x="Distance (km)", 
       title="Office Distance vs Green Zone", 
       caption = "Source: Housing Data")

plot(gg3)
