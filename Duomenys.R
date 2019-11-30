if(!require(psych)) install.packages("psych")
require(psych)
if(!require(eurostat)) install.packages("eurostat")
require(eurostat)
if(!require(tidyverse)) install.packages("tidyverse")
require(tidyverse)
BVP_crrntmrktprice <- get_eurostat("tec00001", stringsAsFactors=FALSE) ## GDP at market prices
BVP_crrntmrktprice <- filter(BVP_crrntmrktprice,
                             geo=="FR",
                             unit=="CP_MEUR") ## Market prices in EUR
population_males<- get_eurostat("demo_pjan", stringsAsFactors=FALSE) ## Population on 1 January by age groups and sex - cities and greater cities
population_males <- filter(population_males,
                           time>="2007-01-01",
                           age=="TOTAL",
                           sex=="M",
                           geo=="FR")
unemployment <- get_eurostat("une_rt_a", stringsAsFactors = FALSE)
unemployment <- filter(unemployment, 
                       time>="2007-01-01",
                       unit=="PC_POP", ##Percentage of total population
                       sex=="M",
                       geo=="FR",
                       age=="Y25-74")
mean_median_income <- get_eurostat("ilc_di13", stringsAsFactors= FALSE) ## Mean and median income before social transfers (pensions included in social transfers) by household type
mean_median_income <- filter(mean_median_income, 
                             time>="2007-01-01",
                             geo=="FR",
                             unit=="EUR",
                             age=="Y16-64")


Data <- right_join(unemployment, BVP_crrntmrktprice, by="time")
Data <- right_join(Data, mean_median_income, by="time")
Data <- right_join(Data, population_males, by="time")
Data <- Data %>% rename(unemployment=values.x, bvp=values.y, unemploymentage=age.x, mean_median_income=values.x.x, population_males=values.y.y)
Data <- select(Data, time, unemployment, bvp, indic_il, mean_median_income, population_males)
Mean<- Data %>% filter(indic_il=="MEI_E") %>%
        select(mean_median_income, time) %>% rename(meanincome=mean_median_income)
Median <-Data %>% filter(indic_il=="MED_E") %>%
        select(mean_median_income,time) %>% rename(medianincome=mean_median_income)
Data <- right_join(Data, Mean, by="time")
Data <- right_join(Data, Median, by="time")
Data <- select(Data, -mean_median_income, -indic_il)
Data <- distinct(Data)
Data$time <- format(as.Date(Data$time, format="%d/%m/%Y"),"%Y")
Data$time <- as.numeric(Data$time)

## Sklaidos charakteristikos.

summary(Data[,-1])
summarise_all(Data[,-1],list(StdNuok = sd, Dispersija = var))
library(psych)
describe(Data[,-1],na.rm=T)

## Liniju grafikai ir histogramos
ggplot(Data, aes(x=time, y=bvp)) +
        geom_line() +
        geom_point()+
        theme(axis.text.x = element_text(face="bold",  size=8, angle=90))+
        ggtitle("BVP 2007-2018m.")+
        scale_x_continuous(breaks = seq(min(Data$time), max(Data$time), by = 1))+
        labs(x="Metai", y="BVP, mln.eur.", subtitle = "Duomenų šaltinis:Eurostat[tec00001]")

ggplot(Data, aes(x = time)) + 
        geom_line(aes(y = bvp, colour = "BVP")) + 
        geom_line(aes(y = unemployment*100000, colour = "Vyrų nedarbas")) + 
        scale_y_continuous(sec.axis = sec_axis(~.*0.00001, name = "Vyrų nedarbas [%]")) + 
        labs(y = "BVP [mln. eur.]", x = "Metai",
             colour = "Parametrai") + 
        scale_x_continuous(breaks = seq(min(Data$time), max(Data$time), by = 1))+
        theme(legend.position = c(0.1, 0.9),
              axis.text.x = element_text(face="bold",  size=8, angle=90))+
        ggtitle("BVP ir vyrų nedarbo lygis 2007-2018m.")

ggplot(Data, aes(x = time)) + 
        geom_line(aes(y = bvp, colour = "BVP")) + 
        geom_line(aes(y = unemployment*100000, colour = "Vyrų nedarbas")) + 
        scale_y_continuous(sec.axis = sec_axis(~.*0.00001, name = "Vyrų nedarbas [%]")) + 
        labs(y = "BVP [mln. eur.]", x = "Metai",
             colour = "Parametrai",
             subtitle="Šaltinis:Eurostat [une_rt_a]") + 
        scale_x_continuous(breaks = seq(min(Data$time), max(Data$time), by = 1))+
        theme(legend.position = c(0.1, 0.9),
              axis.text.x = element_text(face="bold",  size=8, angle=90))+
        ggtitle("BVP ir vyrų nedarbo lygis 2007-2018m.")

ggplot(Data, aes(x = time)) + 
        geom_line(aes(y = bvp, colour = "BVP")) + 
        geom_line(aes(y = meanincome*100, colour = "Pajamų vidurkis")) + 
        scale_y_continuous(sec.axis = sec_axis(~.*0.01, name = "Pajamų vidurkis (tūkst.eur)")) + 
        labs(y = "BVP [mln. eur.]", x = "Metai",
             colour = "Parametrai", subtitle="Šaltinis:Eurostat[ilc_di13]") + 
             theme(axis.text.x = element_text(face="bold",  size=8, angle=90))+
             theme(legend.position="bottom")+
        scale_x_continuous(breaks = seq(min(Data$time), max(Data$time), by = 1))+
        ggtitle("BVP ir vyrų vidutinių pajamų kitimas 2007-2018m.")

ggplot(Data, aes(x = time)) + 
        geom_line(aes(y = meanincome, colour = "Vidutinės pajamos")) + 
        geom_line(aes(y = unemployment*1000, colour = "Nedarbo lygis")) + 
        scale_y_continuous(sec.axis = sec_axis(~.*0.001, name = "Nedarbo lygis")) + 
        labs(y = "Pajamų vidurkis (tūkst.eur)", x = "Metai",
             colour = "Parametrai", subtitle="Šaltinis:Eurostat") + 
        theme(axis.text.x = element_text(face="bold",  size=8, angle=90))+
        theme(legend.position="bottom")+
        scale_x_continuous(breaks = seq(min(Data$time), max(Data$time), by = 1))+
        ggtitle("Vyrų vidutinių pajamų ir nedarbo lygio kitimas 2007-2018m.")


ggplot(data=Data, aes(x=bvp)) + 
        geom_histogram(binwidth=100000)+
        ylab("Dažnis")+
        ggtitle("BVP histograma")


### 8 uzduotis,
cols.num <- c("unemployment","bvp","population_males",
              "meanincome","medianincome")
DF<-Data
DF[cols.num] <- sapply(Data[cols.num],as.numeric)
DF$time <- as.integer(DF$time)


trnd<-lm(bvp~time, data=DF)
summary(trnd)


ggplot(DF, aes(time, bvp)) +
        scale_x_continuous(breaks=DF$time,labels=DF$time)+
        geom_line(stat="identity") +
        geom_point(size=1) +
        stat_smooth(method = "lm", se = FALSE)

ggplotRegression <- function(fit,df){
        
        ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
                geom_line() +
                geom_point(size=3)+
                scale_x_continuous(breaks=DF$time,labels=DF$time)+
                stat_smooth(method = "lm", col = "red") +
                labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                                   "Intercept =",signif(fit$coef[[1]],5 ),
                                   " Slope =",signif(fit$coef[[2]], 5),
                                   " P =",signif(summary(fit)$coef[2,4], 5)))+
                theme(axis.text.x = element_text(face="bold",  size=8, angle=45))
}


ggplotRegression(lm(bvp ~ time, data = DF),DF)


## koreliacijos testas
cor(DF[,-1])
cr <- corr.test(DF[,-1], ci=TRUE)
cr
cr$r
cr$p
cr$ci.adj

install.packages("car")
library(car)

## regresija
regr<-lm(meanincome~bvp+population_males+unemployment,data=DF)
summary(regr)
vif(regr)

regr2<-lm(bvp~population_males, data=DF)
summary(regr2)

regr3<-lm(meanincome~bvp, data=DF)
summary(regr3)

ggplotRegression(regr3,DF)

## prognoze meanincome nuo BVP
new.time <- data.frame(Metai = c(DF$time, 2018:2020))
new.bvp <- data.frame(bvp=predict(trnd, new.time, se.fit=TRUE)$fit)
