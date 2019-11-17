if(!require(tidyverse)) install.packages("tidyverse")
require(tidyverse)
if(!require(httr)) install.packages("httr")
require(httr)
if(!require(psych)) install.packages("psych")
require(psych)
url<-"https://drive.google.com/uc?export=download&id=1S5K0ii6_5bUlPUzGbmw0E-Y0N2rUXs_W"
GET(url, write_disk(tf <- tempfile(fileext = ".csv")))
data<- read.csv(tf, sep = ",", header=TRUE)
## Kintamieji: Lytis(kategorinis), Išsilavinimas(kategorinis), Sektorius(kategorinis),
## Darbo stažas įmonėje(intervalinis), Bruto atlyginimas(Intervalinis). įmonės dydis(kategorinis)
data <- select(data, B21, B25, A14, B26, B42, A12)
colnames(data) <- c("lytis", "issilavinimas", "sektorius", "stazas", "bruto", "imonesdydis")
data <- filter(data, bruto>=100) ## Nezinau ar reikia, taciau logiskiau gaunasi (BENT 0.25 etato)
data$lytis <- factor(data$lytis)
data$issilavinimas <- factor (data$issilavinimas)
data$sektorius <- factor (data$sektorius)
data$imonesdydis <- factor (data$imonesdydis)

## Dazniai ir santykiniai dazniai

table(data$lytis)
prop.table(table(data$lytis))
round(prop.table(table(data$lytis))*100, 2)

table(data$issilavinimas)
prop.table(table(data$issilavinimas))
round(prop.table(table(data$issilavinimas))*100, 2)

table(data$sektorius)
prop.table(table(data$sektorius))
round(prop.table(table(data$sektorius))*100, 2)

table(data$imonesdydis)
prop.table(table(data$imonesdydis))
round(prop.table(table(data$imonesdydis))*100, 2)

## Grafikai kategoriniu

LytisGrafikas <- ggplot(data, aes(lytis)) + 
        geom_bar(aes(y = (..count..)/sum(..count..))) + 
        scale_y_continuous(labels=scales::percent) +
        labs(x="Lytis",y="Procentai")+
        ggtitle("Lytis (procentai)")+
        theme(axis.text.x = element_text(face="bold",  size=8, angle=90))
LytisGrafikas


IssilavinimasGrafikas <- ggplot(data, aes(issilavinimas)) + 
        geom_bar(aes(y = (..count..)/sum(..count..))) + 
        scale_y_continuous(labels=scales::percent) +
        labs(x="Issilavinimas",y="Procentai")+
        ggtitle("Issilavinimas (procentai)")+
        theme(axis.text.x = element_text(face="bold",  size=8, angle=90))
IssilavinimasGrafikas

SektoriusGrafikas <- ggplot(data, aes(sektorius)) + 
        geom_bar(aes(y = (..count..)/sum(..count..))) + 
        scale_y_continuous(labels=scales::percent) +
        labs(x="Sektorius",y="Procentai")+
        ggtitle("Sektorius (procentai)")+
        theme(axis.text.x = element_text(face="bold",  size=8, angle=90))
SektoriusGrafikas

ImonesdydisGrafikas <- ggplot(data, aes(imonesdydis)) + 
        geom_bar(aes(y = (..count..)/sum(..count..))) + 
        scale_y_continuous(labels=scales::percent) +
        labs(x="Imones Dydis",y="Procentai")+
        ggtitle("Imones dydis (procentai)")+
        theme(axis.text.x = element_text(face="bold",  size=8, angle=90))
ImonesdydisGrafikas

## Charakteristikos

dat<-select(data,stazas,bruto)
summary(dat)
summarise_all(dat,list(StdNuok = sd, Dispersija = var, MAD = mad),na.rm=T)
describe(dat,na.rm=T)

## Pjuvis 

dat1 <- select(data, lytis, bruto, stazas)
dat1 %>% group_by(lytis)%>% summarise_all(list(StdNuok = sd, Dispersija = var),na.rm=T)

dat2 <- select(data, issilavinimas, bruto, stazas)
dat2 %>% group_by(issilavinimas)%>% summarise_all(list(StdNuok = sd, Dispersija = var),na.rm=T)

dat3 <- select(data, sektorius, bruto, stazas)
dat3 %>% group_by(sektorius)%>% summarise_all(list(StdNuok = sd, Dispersija = var),na.rm=T)

dat4 <- select(data, imonesdydis, bruto, stazas)
dat4 %>% group_by(imonesdydis)%>% summarise_all(list(StdNuok = sd, Dispersija = var),na.rm=T)

## Staciakampes diagramos ir histogramos
### Bruto pagal lyti
a <- ggplot(data, aes(x=lytis, y=bruto,color=as.factor(lytis))) +
        labs(x="Lytis",y="Bruto atlyginimas",color="Atlyginimas")+
        geom_boxplot()+ggtitle("Bruto atlyginimo pagal lytitačiakampė diagrama")+
        scale_x_discrete(labels = c('Moterys','Vyrai'))+
        scale_y_continuous(breaks = seq(0, max(data$bruto), by = 2000))
a

levels(dat1$lytis)<-c("Moteris","Vyras")
ggplot(dat1, aes(x=bruto, group=lytis, color=lytis)) +
        geom_histogram(fill="white",binwidth=500, alpha=0.5, position="identity")+
        labs(x="Bruto atlyginimas",y="Dažnis",group="Lytis",color="Lytis")+
        ggtitle("Bruto atlyginimo pagal lyti histograma")+
        theme(axis.text.x = element_text(face="bold",  size=8, angle=90))+
        scale_x_continuous(breaks = seq(min(dat1$bruto), max(dat1$bruto), by = 2500))


###Bruto pagal issilavinima
b <- ggplot(data, aes(x=issilavinimas, y=bruto,color=as.factor(issilavinimas))) +
        labs(x="Issilavinimas",y="Bruto atlyginimas",color="Issilavinimas")+
        geom_boxplot()+ggtitle("Bruto atlyginimo pagal issilavinima Stačiakampė diagrama")+
        scale_y_continuous(breaks = seq(0, max(data$bruto), by = 2000))
b

ggplot(dat2, aes(x=bruto, group=issilavinimas, color=issilavinimas)) +
        geom_histogram(fill="white",binwidth=500, alpha=0.5, position="identity")+
        labs(x="Bruto atlyginimas",y="Dažnis",group="Issilavinimas",color="Issilavinimas")+
        ggtitle("Bruto atlyginimo pagal lyti histograma")+
        theme(axis.text.x = element_text(face="bold",  size=8, angle=90))+
        scale_x_continuous(breaks = seq(min(dat1$bruto), max(dat1$bruto), by = 2500))


###Stazas pagal lyti
a <- ggplot(data, aes(x=lytis, y=bruto,color=as.factor(lytis))) +
        labs(x="Lytis",y="Bruto atlyginimas",color="Atlyginimas")+
        geom_boxplot()+ggtitle("Bruto atlyginimo pagal lytitačiakampė diagrama")+
        scale_x_discrete(labels = c('Moterys','Vyrai'))+
        scale_y_continuous(breaks = seq(0, max(data$bruto), by = 2000))
a

ggplot(dat1, aes(x=as.numeric(stazas), group=lytis, color=lytis)) +
        geom_histogram(fill="white",binwidth=1, alpha=0.5, position="identity")+
        labs(x="Darbo stazas",y="Dažnis",group="Lytis",color="Lytis")+
        ggtitle("Darbo stazo pagal lyti histograma")+
        theme(axis.text.x = element_text(face="bold",  size=8, angle=90))






