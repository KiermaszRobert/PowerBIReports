setwd("C://Users//cengishan//Documents//skrypty//raportyPowerBI//5_midocean//")

options(digits=3)
x1<-read.csv("sales_margin.csv", encoding="UTF-8", header = TRUE)
#library(dplyr)
x1$rok<-substr(x1$DateKey,0,4)
x1$miesiac<-substr(x1$DateKey,6,7)
x1$DateKey<-as.Date(x1$DateKey)
x1$UnitCost<-sub("$", "", x1$UnitCost, fixed = TRUE)
x1$UnitCost<-as.numeric(sub(",", ".", x1$UnitCost, fixed = TRUE))

x1$UnitPrice<-sub("$", "", x1$UnitPrice, fixed = TRUE)
x1$UnitPrice<-as.numeric(sub(",", ".", x1$UnitPrice, fixed = TRUE))



x1<-x1 %>% select(rok, miesiac,UnitCost, UnitPrice, SalesQuantity)
x1<-x1%>%filter(is.na(x1$UnitPrice)==FALSE)#usuniecie pustych
summary(x1)

# > summary(x1)
# rok              miesiac             UnitCost        UnitPrice     
# Length:1014320     Length:1014320     Min.   :  0.48   Min.   :  0.95  
# Class :character   Class :character   1st Qu.: 29.01   1st Qu.: 59.00  
# Mode  :character   Mode  :character   Median : 86.45   Median :199.00  
# Mean   :110.27   Mean   :251.60  
# 3rd Qu.:152.68   3rd Qu.:329.00  
# Max.   :459.40   Max.   :999.00  
# SalesQuantity        margin      margin_quantity  
# Min.   :   4.0   Min.   :0.489   Min.   :   1.96  
# 1st Qu.:   9.0   1st Qu.:0.490   1st Qu.:   4.86  
# Median :  10.0   Median :0.540   Median :   5.88  
# Mean   :  16.3   Mean   :0.543   Mean   :   8.64  
# 3rd Qu.:  13.0   3rd Qu.:0.540   3rd Qu.:   7.02  
# Max.   :2880.0   Max.   :0.669   Max.   :1412.61  




margin_mean<-mean(x1$margin)
#> margin_mean
#[1] 0.542619

x1$margin_quantity<-x1$margin*x1$SalesQuantity

margin_quantity_mean<-sum(x1$margin*x1$SalesQuantity)/sum(x1$SalesQuantity)
# margin_quantity_mean
#[1] 0.529483


x2<- x1 %>% group_by(rok,miesiac) %>% summarise(average_margin = mean(margin))

x3<-x1 %>% filter(rok=="2012" & miesiac=="01") %>% summarise(margin=mean(margin))
# margin
#   0.54

x4<-x1 %>% filter(rok=="2012" & miesiac=="02") %>% summarise(margin=mean(margin))
# margin
#   0.542

x5<-x1 %>% filter(rok=="2012" & miesiac=="03") %>% summarise(margin=mean(margin))
# margin
#   0.541


#write.csv(x1,"dane.csv", na = "",quote = FALSE,row.names = FALSE)
