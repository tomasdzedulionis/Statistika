## 2-a uzduotis. Duomenu importas.
if(!require(psych)) install.packages("psych")
require(psych)
if(!require(eurostat)) install.packages("eurostat")
require(eurostat)
if(!require(tidyverse)) install.packages("tidyverse")
require(tidyverse)
if(!require(car)) install.packages("car")
require(car)
if(!require(reshape2)) install.packages("reshape2")
require(reshape2)
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
                             age=="Y16-64",
                             sex=="T")


Data <- right_join(unemployment, BVP_crrntmrktprice, by="time")
Data <- right_join(Data, mean_median_income, by="time")
Data <- right_join(Data, population_males, by="time")
Data <- Data %>% rename(unemployment=values.x, bvp=values.y, unemploymentage=age.x, mean_median_income=values.x.x, population_males=values.y.y)
Data <- select(Data, time, unemployment, bvp, indic_il, mean_median_income, population_males)
Median <-Data %>% filter(indic_il=="MED_E") %>%
        select(mean_median_income,time) %>% rename(medianincome=mean_median_income)
Data <- right_join(Data, Median, by="time")
Data <- select(Data, -mean_median_income, -indic_il)
Data <- distinct(Data)
Data$time <- format(as.Date(Data$time, format="%d/%m/%Y"),"%Y")
Data$time <- as.numeric(Data$time)


## 4-a uzduotis. 
##Apibūdinkite „macro“ kintamuosius; apskaičiuokite padėties, sklaidos bei formos skaitines charakteristikas;
##pateikite gautų charakteristikų interpretaciją bei ekonominį pagrindimą; 
##Išbrėžkite pasirinktų rodiklių linijos grafiką bei histogramą.

## Sklaidos charakteristikos.

summary(Data[,-1])
summarise_all(Data[,-1],list(StdNuok = sd, Dispersija = var))
describe(Data[,-1],na.rm=T)

## Grafikai ir histogramos.

ggplot(Data, aes(x=time, y=bvp)) +
        geom_line() +
        geom_point()+
        theme(axis.text.x = element_text(face="bold",  size=8, angle=90))+
        ggtitle("BVP 2007-2018m.")+
        scale_x_continuous(breaks = seq(min(Data$time), max(Data$time), by = 1))+
        labs(x="Metai", y="BVP, mln.eur.", subtitle = "Duomenu saltinis:Eurostat[tec00001]")

ggplot(Data, aes(x = time)) + 
        geom_line(aes(y = bvp, colour = "BVP")) + 
        geom_line(aes(y = unemployment*100000, colour = "Vyru nedarbas")) + 
        scale_y_continuous(sec.axis = sec_axis(~.*0.00001, name = "Vyru nedarbas [%]")) + 
        labs(y = "BVP [mln. eur.]", x = "Metai",
             colour = "Parametrai") + 
        scale_x_continuous(breaks = seq(min(Data$time), max(Data$time), by = 1))+
        theme(legend.position = c(0.1, 0.9),
              axis.text.x = element_text(face="bold",  size=8, angle=90))+
        ggtitle("BVP ir vyru nedarbo lygis 2007-2018m.")

ggplot(Data, aes(x = time)) + 
        geom_line(aes(y = bvp, colour = "BVP")) + 
        geom_line(aes(y = medianincome*100, colour = "Pajamu mediana")) + 
        scale_y_continuous(sec.axis = sec_axis(~.*0.01, name = "Pajamu mediana")) + 
        labs(y = "BVP [mln. eur.]", x = "Metai",
             colour = "Parametrai", subtitle="Saltinis:Eurostat[ilc_di13]") + 
        theme(axis.text.x = element_text(face="bold",  size=8, angle=90))+
        theme(legend.position="bottom")+
        scale_x_continuous(breaks = seq(min(Data$time), max(Data$time), by = 1))+
        ggtitle("BVP ir pajamu medianos kitimas 2007-2018m.")

ggplot(Data, aes(x = time)) + 
        geom_line(aes(y = medianincome, colour = "Pajamu mediana")) + 
        geom_line(aes(y = unemployment*1000, colour = "Nedarbo lygis")) + 
        scale_y_continuous(sec.axis = sec_axis(~.*0.001, name = "Nedarbo lygis")) + 
        labs(y = "Pajamu mediana", x = "Metai",
             colour = "Parametrai", subtitle="Saltinis:Eurostat") + 
        theme(axis.text.x = element_text(face="bold",  size=8, angle=90))+
        theme(legend.position="bottom")+
        scale_x_continuous(breaks = seq(min(Data$time), max(Data$time), by = 1))+
        ggtitle("Pajamu medianos ir vyru nedarbo lygio kitimas 2007-2018m.")

hist(Data$bvp,
     main="BVP histograma",
     xlab="BVP",
     ylab="Daznis")

hist(Data$medianincome,
     main="Pajamu medianos histograma",
     xlab="Pajamu mediana",
     ylab="Daznis")

hist(Data$unemployment,
     main="Nedarbo lygio histograma",
     xlab="Nedarbo lygis",
     ylab="Daznis")

hist(Data$population_males,
     main="Vyru populiacijos histograma",
     xlab="Vyru populiacija",
     ylab="Daznis")


### 8 uzduotis,
##Rinkmenos „macro“ kintamiesiems išskirkite tiesinius trendus. Pavaizduokite duomenų ir trendų grafikus.
cols.num <- c("unemployment","bvp","population_males",
              "medianincome")
DF<-Data
DF[cols.num] <- sapply(Data[cols.num],as.numeric)
DF$time <- as.integer(DF$time)

trnd<-lm(bvp~time, data=DF)
summary(trnd)
trnd2<-lm(population_males~time, data=DF)
summary(trnd2)
trnd3<-lm(medianincome~time, data=DF)
summary(trnd3)
trnd4<-lm(unemployment~time, data=DF)
summary(trnd4)

ggplotRegression <- function(fit,DF){
        
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
ggplotRegression(lm(population_males~time, data=DF),DF)
ggplotRegression(lm(medianincome~time, data=DF),DF)
ggplotRegression(lm(unemployment~time, data=DF),DF)

##9Uzduotis
#Pasirinkite kelis arba visus „macro“ duomenų kintamuosius ir
##sudarykite tiesinę regresiją ir atlikite prognozę.

cor(DF[,-1])
cr <- corr.test(DF[,-1], ci=TRUE)
cr
cr$r
cr$p
cr$ci.adj

regr2<-lm(bvp~population_males, data=DF)
summary(regr2)
regr3<-lm(medianincome~bvp, data=DF)
summary(regr3)

ggplotRegression(regr3,DF)
ggplotRegression(regr2,DF)

new.time <- data.frame(time = c(Data$time, 2018:2020))
new.bvp <- data.frame(bvp=predict(trnd, new.time, se.fit=TRUE)$fit)
prognoze1 <- predict(regr3, new.bvp, se.fit = TRUE, 
                     interval = "confidence")
prognoze <- predict(regr3, new.bvp, se.fit = TRUE, 
                    interval = "prediction")

g.df <- data.frame(Metai = new.time, 
                   medianincome = c(DF$medianincome, rep(NA, 3)),
                   fitMediannincome = prognoze$fit[,1],
                   opMedianincome = prognoze$fit[,3],
                   pesMedianincome = prognoze$fit[,2])
mg.df <- melt(g.df, id="time")


ggplot(mg.df, aes(time, value, color=variable, shape=variable,
                  linetype=variable)) +
        geom_line()+
        geom_point()+
        labs(x="Metai", y="Pajamu mediana", title="Pajamu mediana ir pajamu medianos kitimo prognoze")+
        theme(legend.position="bottom")+
        scale_colour_manual(name = "Pajamu mediana ir prognoze",
                            labels = c("Pajamu mediana", "Taskine prognoze", "Optimistine prognoze", 
                                       "Pesimistine prognoze"),
                            values = 1:4) +   
        scale_shape_manual(name = "Pajamu mediana ir prognoze",
                           labels = c("Pajamu mediana", "Taskine prognoze", "Optimistine prognoze", 
                                      "Pesimistine prognoze"),
                           values = 1:4) +
        scale_linetype_manual(name = "Pajamu mediana ir prognoze",
                              labels = c("Pajamu mediana", "Taskine prognoze", "Optimistine prognoze", 
                                         "Pesimistine prognoze"),
                              values = 1:4)

g1.dt <- data.frame(Metai = new.time, 
                    Medianincome = c(DF$medianincome, rep(NA, 3)),
                    fitMedianincome = prognoze1$fit[,1],
                    opMedianincome = prognoze1$fit[,3],
                    pesMedianincome = prognoze1$fit[,2])

ggplot(g1.dt, aes(time, fitMedianincome)) +
        geom_line(color="blue") +
        geom_line(aes(time, opMedianincome), color=2) +
        geom_line(aes(time, pesMedianincome), color=2) +
        geom_line(aes(time, Medianincome))+
        labs(x="Metai", y="Pajamu mediana", title="Pajamu mediana ir pajamu medianos prognoze su \npasikliautinais intervalais")


new.populationmales <- data.frame(population_males=predict(trnd2, new.time, se.fit=TRUE)$fit)
prognoze2 <- predict(regr2, new.populationmales, se.fit = TRUE, 
                     interval = "confidence")
prognoze22 <- predict(regr2, new.populationmales, se.fit = TRUE, 
                      interval = "prediction")

g.df2 <- data.frame(Metai = new.time, 
                    bvp = c(DF$bvp, rep(NA, 3)),
                    fitbvp = prognoze2$fit[,1],
                    opbvp = prognoze2$fit[,3],
                    pesbvp = prognoze2$fit[,2])
mg.df2 <- melt(g.df, id="time")


ggplot(mg.df2, aes(time, value, color=variable, shape=variable,
                   linetype=variable)) +
        geom_line()+
        geom_point()+
        labs(x="Metai", y="BVP", title="BVP ir BVP kitimo prognoze")+
        theme(legend.position="bottom")+
        scale_colour_manual(name = "BVP ir prognoze",
                            labels = c("BVP", "Taskine prognoze", "Optimistinė prognozė", 
                                       "Pesimistinė prognoze"),
                            values = 1:4) +   
        scale_shape_manual(name = "BVP ir prognoze",
                           labels = c("BVP", "Taskine prognoze", "Optimistinė prognozė", 
                                      "Pesimistinė prognoze"),
                           values = 1:4) +
        scale_linetype_manual(name = "BVP ir prognoze",
                              labels = c("BVP", "Taskine prognoze", "Optimistinė prognozė", 
                                         "Pesimistinė prognoze"),
                              values = 1:4)

g1.dt2 <- data.frame(Metai = new.time, 
                     bvp = c(DF$bvp, rep(NA, 3)),
                     fitbvp = prognoze22$fit[,1],
                     opbvp = prognoze22$fit[,3],
                     pesbvp = prognoze22$fit[,2])

ggplot(g1.dt2, aes(time, fitbvp)) +
        geom_line(color="blue") +
        geom_line(aes(time, opbvp), color=2) +
        geom_line(aes(time, pesbvp), color=2) +
        geom_line(aes(time, bvp))+
        labs(x="Metai", y="BVP", title="BVP ir BVP prognoze su pasikliautinais intervalais")
