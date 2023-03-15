setwd("C://Users//cengishan//Documents//skrypty//raportyPowerBI//5_midocean//")

options(digits=3)
x1<-read.csv("sales_margin.csv", encoding="UTF-8", header = TRUE)
#library(dplyr)
x1$rok<-substr(x1$DateKey,0,4)
x1$miesiac<-substr(x1$DateKey,6,7)
x1$DateKey<-as.Date(x1$DateKey)


x1$UnitPrice<-sub(",", ".", x1$UnitPrice, fixed = TRUE)
x1$UnitPrice<-sub("$", "", x1$UnitPrice, fixed = TRUE)
x1$UnitPrice<-sub(" ", "", x1$UnitPrice, fixed = TRUE)
x1$UnitPrice<-sub(" ", "", x1$UnitPrice, fixed = TRUE)
x1$UnitPrice<- as.numeric(x1$UnitPrice)

x1$TotalCost<-sub(",", ".", x1$TotalCost, fixed = TRUE)
x1$TotalCost<-sub("$", "", x1$TotalCost, fixed = TRUE)
x1$TotalCost<-sub(" ", "", x1$TotalCost, fixed = TRUE)
x1$TotalCost<-sub(" ", "", x1$TotalCost, fixed = TRUE)
x1$TotalCost<- as.numeric(x1$TotalCost)

x1$UnitCost<-sub(",", ".", x1$UnitCost, fixed = TRUE)
x1$UnitCost<-sub("$", "", x1$UnitCost, fixed = TRUE)
x1$UnitCost<-sub(" ", "", x1$UnitCost, fixed = TRUE)
x1$UnitCost<-sub(" ", "", x1$UnitCost, fixed = TRUE)
x1$UnitCost<- as.numeric(x1$UnitCost)



x1<-x1 %>% select(rok, miesiac,UnitCost, UnitPrice, SalesQuantity,TotalCost,SalesAmount)
#nie bylo pustych, był jakiś specjalny znak spacjopodobny
#x1<-x1%>%filter(is.na(x1$UnitPrice)==FALSE)#usuniecie pustych
summary(x1)

# summary(x1)
# rok              miesiac             UnitCost     UnitPrice   
# Length:1048575     Length:1048575     Min.   :  0   Min.   :   1  
# Class :character   Class :character   1st Qu.: 31   1st Qu.:  60  
# Mode  :character   Mode  :character   Median : 87   Median : 200  
# Mean   :124   Mean   : 297  
# 3rd Qu.:163   3rd Qu.: 358  
# Max.   :961   Max.   :2900  
# SalesQuantity    TotalCost      SalesAmount    
# Min.   :   4   Min.   :    2   Min.   :     3  
# 1st Qu.:   9   1st Qu.:  319   1st Qu.:   652  
# Median :  10   Median :  986   Median :  2294  
# Mean   :  16   Mean   : 1554   Mean   :  3649  
# 3rd Qu.:  13   3rd Qu.: 2064   3rd Qu.:  4696  
# Max.   :2880   Max.   :79080   Max.   :235467  

x1$margin<-(x1$UnitPrice-x1$UnitCost)/x1$UnitPrice
margin_mean<-mean(x1$margin)
#> margin_mean
#[1] 0.547

x1$margin_quantity<-x1$margin*x1$SalesQuantity

margin_quantity_mean<-sum(x1$margin*x1$SalesQuantity)/sum(x1$SalesQuantity)
# margin_quantity_mean
#[1] 0.533

x1$margin2<-(x1$SalesAmount-x1$TotalCost)/x1$SalesAmount
mean(x1$margin2)
# > mean(x1$margin2)
# [1] 0.542

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
