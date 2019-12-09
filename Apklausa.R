##1-a uzduotis.
if(!require(tidyverse)) install.packages("tidyverse")
require(tidyverse)
if(!require(httr)) install.packages("httr")
require(httr)
if(!require(psych)) install.packages("psych")
require(psych)
url<-"https://drive.google.com/uc?export=download&id=12PNPliiWwR8L1u8s-Q02kvtzg5YyUjND"
GET(url, write_disk(tf <- tempfile(fileext = ".csv")))
data<- read.csv(tf, sep = ";", header=TRUE)
data <- select(data, HH070, HS040, HS060, HS120, HY010, M_K)
colnames(data) <- c("BustoIslaikymoIslaidos", "GalimybeAtostogauti", "GalimybeApmoketiIslaidas", "KaipVerciasi", "Pajamos", "M_K")
levels(data$M_K) <- c(levels(data$M_K), "Miestas", "Kaimas") 
data$M_K[data$M_K=="1"]  <- "Miestas"
data$M_K[data$M_K=="2"]  <- "Kaimas"
levels(data$GalimybeAtostogauti) <- c(levels(data$GalimybeAtostogauti), "Taip", "Ne")
data$GalimybeAtostogauti[data$GalimybeAtostogauti=="1"] <- "Taip"
data$GalimybeAtostogauti[data$GalimybeAtostogauti=="2"] <- "Ne"
data$GalimybeAtostogauti <- factor(data$GalimybeAtostogauti)
data$GalimybeApmoketiIslaidas <- factor (data$GalimybeApmoketiIslaidas)
data$KaipVerciasi <- factor (data$KaipVerciasi)
data$M_K <- factor (data$M_K)
data<-na.omit(data)
data$BustoIslaikymoIslaidos <- as.numeric(sub(",", ".", data$BustoIslaikymoIslaidos, fixed=T))
data$Pajamos <- as.numeric(sub(",", ".", data$Pajamos, fixed=T))

##3-ia uzduotis

#a) Dazniai ir santykiniai dazniai

table(data$GalimybeAtostogauti)
prop.table(table(data$GalimybeAtostogauti))
round(prop.table(table(data$GalimybeAtostogauti))*100, 2)

table(data$GalimybeApmoketiIslaidas)
prop.table(table(data$GalimybeApmoketiIslaidas))
round(prop.table(table(data$GalimybeApmoketiIslaidas))*100, 2)

table(data$KaipVerciasi)
prop.table(table(data$KaipVerciasi))
round(prop.table(table(data$KaipVerciasi))*100, 2)

table(data$M_K)
prop.table(table(data$M_K))
round(prop.table(table(data$M_K))*100, 2)

# Stulpeliu diagramos

ggplot(data, aes(GalimybeAtostogauti)) + 
        geom_bar(aes(y = (..count..)/sum(..count..))) + 
        scale_y_continuous(labels=scales::percent) +
        labs(x="Galimybe karta per metus atostogauti",y="Procentai")+
        ggtitle("Galimybe karta per metus atostogauti proc.")
ggplot(data, aes(GalimybeApmoketiIslaidas)) + 
        geom_bar(aes(y = (..count..)/sum(..count..))) + 
        scale_y_continuous(labels=scales::percent) +
        labs(x="Galimybe apmoketi islaidas",y="Procentai")+
        ggtitle("Galimybe apmoketi nenumatytas 700lt islaidas proc. (1-Taip, 2-Ne)")
ggplot(data, aes(KaipVerciasi)) + 
        geom_bar(aes(y = (..count..)/sum(..count..))) + 
        scale_y_continuous(labels=scales::percent) +
        labs(x="Kaip namu ukis verciasi",y="Procentai")+
        ggtitle("Kaip verciasi namu ukis? (1-labai sunkiai...6-labai lengvai)")
ggplot(data, aes(M_K)) + 
        geom_bar(aes(y = (..count..)/sum(..count..))) + 
        scale_y_continuous(labels=scales::percent) +
        labs(x="Vietove",y="Procentai")+
        ggtitle("Vietoves pasiskirstymas proc.")


###b) Intervaliniams kintamiesiems apskaičiuokite padėties, 
##    sklaidos bei formos skaitines charakteristikas pagal kategorinio kintamojo(-ųjų) pjūvį(-ius).
## Charakteristikos

dat<-select(data,BustoIslaikymoIslaidos,Pajamos)
summary(dat)
summarise_all(dat,list(StdNuok = sd, Dispersija = var, MAD = mad),na.rm=T)
describe(dat,na.rm=T)

## Pjuvis 
dat<-select(data,BustoIslaikymoIslaidos,Pajamos)
summary(dat)
summarise_all(dat,list(StdNuok = sd, Dispersija = var, MAD = mad),na.rm=T)
describe(dat,na.rm=T)
dat1 <- select(data, GalimybeAtostogauti, BustoIslaikymoIslaidos, Pajamos)
dat1 %>% group_by(GalimybeAtostogauti)%>% summarise_all(list(StdNuok = sd, Dispersija = var),na.rm=T)
dat2 <- select(data, GalimybeApmoketiIslaidas, BustoIslaikymoIslaidos, Pajamos)
dat2 %>% group_by(GalimybeApmoketiIslaidas)%>% summarise_all(list(StdNuok = sd, Dispersija = var),na.rm=T)
dat3 <- select(data, KaipVerciasi, BustoIslaikymoIslaidos, Pajamos)
dat3 %>% group_by(KaipVerciasi)%>% summarise_all(list(StdNuok = sd, Dispersija = var),na.rm=T)
dat4 <- select(data, M_K, BustoIslaikymoIslaidos, Pajamos)
dat4 %>% group_by(M_K)%>% summarise_all(list(StdNuok = sd, Dispersija = var),na.rm=T)

##c) Išbrėžkite pasirinktų rodiklių 
##stačiakampę diagramą bei histogramą pagal kategorinio kintamojo(-ųjų) pjūvį(-ius). 

ggplot(data, aes(x=M_K, y=Pajamos,color=as.factor(M_K))) +
        labs(x="Vietove",y="Namu ukio pajamos",color="Pajamos")+
        geom_boxplot()+ggtitle("Namu ukio pajamu pagal vietove staciakampe diagrama")+
        scale_y_continuous(breaks = seq(0, max(data$Pajamos), by = 10000))
ggplot(dat4, aes(x=Pajamos, group=M_K, color=M_K)) +
        geom_histogram(fill="white",binwidth=10000, alpha=0.5, position="identity")+
        labs(x="Namu ukio pajamos",y="Dažnis",group="M_K",color="M_K")+
        ggtitle("Namu ukio pajamu pagal vietove histograma")+
        theme(axis.text.x = element_text(face="bold",  size=8, angle=90))+
        scale_x_continuous(breaks = seq(min(dat1$Pajamos), max(dat1$Pajamos), by = 10000))
ggplot(data, aes(x=GalimybeAtostogauti, y=Pajamos,color=as.factor(GalimybeAtostogauti))) +
        labs(x="Galimybe atostogauti",y="Pajamos",color="GalimybeAtostogauti")+
        geom_boxplot()+ggtitle("Galimybes atostogauti pagal pajamas staciakampe diagrama")+
        scale_y_continuous(breaks = seq(0, max(data$Pajamos), by = 10000))
ggplot(dat1, aes(x=Pajamos, group=GalimybeAtostogauti, color=GalimybeAtostogauti)) +
        geom_histogram(fill="white",binwidth=10000, alpha=0.5, position="identity")+
        labs(x="Pajamos",y="Dažnis",group="GalimybeAtostogauti",color="GalimybeAtostogauti")+
        ggtitle("Galimybes atostogauti pagal pajamas histograma")+
        theme(axis.text.x = element_text(face="bold",  size=8, angle=90))+
        scale_x_continuous(breaks = seq(min(dat1$Pajamos), max(dat1$Pajamos), by = 10000))
ggplot(data, aes(x=M_K, y=BustoIslaikymoIslaidos,color=as.factor(M_K))) +
        labs(x="Vietove",y="Busto Islaikymo islaidos",color="Vietove")+
        geom_boxplot()+
        ggtitle("Busto islaikymo islaidu pagal vietove staciakampe diagrama")+
        scale_y_continuous(breaks = seq(0, 10000, by = 100))
ggplot(dat4, aes(x=as.numeric(BustoIslaikymoIslaidos), group=M_K, color=M_K)) +
        geom_histogram(fill="white",binwidth=100, alpha=0.5, position="identity")+
        labs(x="Vietove",y="Dažnis",group="M_K",color="M_K")+
        ggtitle("Busto islaikymo islaidu pagal vietove histograma")+
        theme(axis.text.x = element_text(face="bold",  size=8, angle=90))+
        scale_x_continuous(breaks = seq(0, 2500, by = 100))


#Charakteristikos, penkiaskaite sistema
summary(Data[,-1])
#Papildomos sklaidos charakteristikos
summarise_all(Data[,-1],list(StdNuok = sd, Dispersija = var))
#Papildomos charakteristikos
describe(Data[,-1],na.rm=T)


## 5-a uzduotis. 
##Rinkmenoje „apklausa“ pasirinkite du kintamuosius ir atlikite t-testą
        
### T testas
t.test(data$Pajamos ~ data$M_K)

### 6 uzduotis
##Rinkmenoje „apklausa“ pasirinkite intervalinį kintamajį bei suskaičiuokite vidurkio ir dispersijos
##taškinius įverčius bei vidurkio pasikliautinąjį intervalą.
rez<-summarise(data,vid=mean(Pajamos), std=sd(Pajamos),disp=var(Pajamos)) 
error <- qt(0.975,df=length(data$Pajamos)-1)*sd(data$Pajamos)/sqrt(length(data$Pajamos)) 
left <- mean(data$Pajamos)-error
right <- mean(data$Pajamos)+error
left
right


### 7 uzduotis.
##6-oje dalyje gautus taškinius įverčius panaudokite atsitiktinių normaliųjų dydžių generavimui.
##Porai (vidurkis, dispersija) sugeneruokite 100 normaliųjų atsitiktinių dydžių imčių
##(imties dydis 500 stebėjimų). Apskaičiuokite gautų imčių taškinius vidurkio bei dispersijos įverčius.
##Gautiems vidurkių taškiniams įverčiams išbrėžkite histogramą bei stačiakampę diagramą. 

observations <- matrix(rnorm(50000, mean=rez$vid, sd=rez$std), 100, 500) 
means<- apply(observations,1,mean)
variance<-apply(observations,1,var)##Dispersija
deviation<-apply(observations,1,sd)##Std. nuokrypis

ggplot(as.data.frame(means),aes(x=means))+
        geom_histogram(binwidth=300,color="white")+
        scale_x_continuous(breaks=seq(30900,37280, 200),labels = seq(30900,37280, 200))+
        theme(axis.text.x = element_text(face="bold",  size=8, angle=90))+
        labs(x="vidurkiai",y="Dazniai")+
        ggtitle("Vidurkiu iverciu histograma")

ggplot(as.data.frame(means),aes(x="",y=means))+
        geom_boxplot()+
        ylab("Vidurkiai")+
        ggtitle("Staciakampe diagrama")
