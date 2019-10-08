###############################################################################
####                    Klausimų pavyzdžiai spalio 8 d.                    ####
###############################################################################

# Įvykdykite komandą
B<-2:7
# Klausimai:
# Koks objekto B tipas?
str(B)
class(B)
mode(B)
# Kokie objekto matavimai (elementų skaičius)?
str(B)
# Parašykite komandų (kurias įvykdžius gaunami atsakymai į klausimus) 
# tekstą ir atsakymus.
#
#Įvykdykite komandą

library(car)
#      Klausimai apie duomenų rinkinį UN
# 
#
###########################################################
#
#   Klausimai:
#
# 1)	Kiek eilučių ir stulpelių turi duomenų aibė
# (parašykit atitinkamą komandą ir atsakymą)?
nrow(UN) ## 213 eiluciu
ncol(UN) ## 7 stulpeliai
# 2)	Parašykit komandą, kurią įvykdžius, kintamojo “ppgdp”  
# pavadinimas pasikeis į “ppbvp”.
UN<-UN
newpav<-rename(UN, ppbvp=ppgdp)
# 3) Parašykite komandas, kurias įvykdžius, iš duomenų aibės bus išrinktos:
#    a) eilutės (šalių duomenys), kuriose tikėtina moterų gyvenimo trukmė didesnė nei 45 metai ir 
#        ne mažesnė nei 70,
library(tidyverse)
filter(UN, lifeExpF>45 & lifeExpF<=70)
#    b) eilutės su gimstamumo rodikliu (fertility) didesniu nei 2 arba kūdikių 
#        mirtingumu (infantMortality) mažesniu nei 50,
filter(UN, fertility>2, infantMortality<50)
#    c) eilutes, kuriose kintamasis pctUrban neturi praleistų reikšmių.
UN[!is.na(UN$pctUrban),]
# 4)	Parašykit komandą (ir jos atsakymą), kurias įvydžius, apskaičiuosime  
# vidutinį kūdikių mirštamumo rodiklį šalims, kurių ppbvp>1000.
c <- UN[!is.na(UN$infantMortality),]
mean(c$infantMortality)
#####################################################
#
#   Įvykdykite žemiau parašytas komandas
#
####################################################
library(ggplot2)
UNN<-UN
UNN$country<-rownames(UN)
UNN<-na.omit(sample_n(UNN,10))
ggplot(UNN,aes(x=ppgdp,y=infantMortality,color=country))+geom_point()
##########################################################################
#
# 1) Papildykite ggplot komandą taip, kad x ašies pavadinimas būtų “PPbvp”, 
#    o y ašies “kūdikių mirtingumas”.
# 2) Papildykite komandą, kad legendos pavadinimas būtų "šalis".
# 3) Papildykite  komandą, kad būtų pateiktas grafiko pavadinimas: 
# “Kūdikių mirtingumo priklausomybė nuo PPbvp”.
# 4) Perrašykite komandą taip, kad x ašies žymelės būtų pateiktos vertikaliai.
# 5) Papildykite komandą taip, kad būtų nubrėžta horizontali atskaitos linija y=20.
UNN$country<-rownames(UN)
UNN<-na.omit(sample_n(UNN,10))
ggplot(UNN,aes(x=ppgdp,y=infantMortality,color=country))+geom_point()+
  labs(x="PPbvp", y="kudikiumirtingumas")+
  labs(col="ssalis")+
  ggtitle("Mirtingumas nuo BVP")+
  geom_hline(yintercept=20)+
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
