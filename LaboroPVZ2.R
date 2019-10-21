###############################################################################
####                    Klausimų pavyzdžiai spalio 15 d.                   ####
###############################################################################


###############################################################################
####                    Pradžia: Duomenų importas                          ####
###############################################################################

# Įvykdykite komandas, kurios nuskaito duomenis iš interneto
# duomenų rinkinys vadinasi socdata
library(readxl)

# Duomenų nuskaitymas
fileURL <- "http://web.vu.lt/mif/j.markeviciute/files/2019/08/socialdata.xlsx"
tempf <- tempfile()
download.file(fileURL, tempf, method = "curl")
socdata <- read_excel(tempf)
unlink(tempf)
###############################################################################
####                Klausimai                                              ####
###############################################################################

# 1) Atspausdinkite pirmas 5 duomenų socdata rinkinio eilutes:
socdata[1:5,]

# 2) Parašykite komandą, kuri nurodo duomenų dimensiją (matavimus). Komentare parašykite atsakymą
dim(socdata)

# 3) Parašykite komandas, kurios duomenų rinkinyje
#    socdata kintamuosius city bei sex pavers faktoriais
socdata$city <- as.factor(socdata$city)
socdata$sex <- as.factor(socdata$sex)
# 4) Parašykite komandą, kuri atspausdina duomenų rinkinio padėties charakteristikas
summary(socdata)
# 5) Parašykite komandas, kurios pašalins (išims) vartotojo ID bei lytį iš duomenų rinkinio
#    Naująjį duomenų rinkinį pavadinkite df_miestai
library(dplyr)
df_miestai<-select(socdata, -sex,-ID)
# 6) Parašykite komandas, kurios atrenka kintamuosius city, messinger, posts, 
#    user.negative, bei friend.positive bei suskaičiuoja vidurkius 
#   visiems skaitiniams kintamiesiems sugrupuotiems pagal miestą 
#  (naudokite duomenų rinkinį df_miestai)
miestai<-select(df_miestai, city, messinger, posts, user.negative, friend.positive)
miestai_grp<-group_by(miestai,city)
vid_miestai<-summarise(miestai_grp, mean(messinger), mean(posts), mean(user.negative),mean(friend.positive))

# Parašykite atsakymą, kokia yra vidutinė posts reikšmė Akmenėje?
akmene<-subset(vid_miestai, city=="Akmene")

# 7) Parašykite komandas, kurios išbrėžia sklaidos diagramą kintamiesiems 
#    lon bei lat. Ką šie kintamieji reiškia?
#  (naudokite duomenų rinkinį df_miestai)
library(ggplot2)
ggplot(df_miestai, aes(x=lon, y = lat)) +
        geom_point()
x<-c("t","k")

# 8) Parašykite komandą, kuri atrenka penkis miestus: Ukmerge, Druskininkai,
#    Vilnius, Kaunas, Klaipeda (naudokite duomenų rinkinį df_miestai)
#    Naująjį duomenų rinkinį pavadinkite df2
df2<-filter(df_miestai, city%in%c("Ukmerge","Druskininkai","Vilnius","Kaunas","Klaipeda"))

# 9) Parašykite komandą, kuri išbrėžia sklaidos diagramą kintamiesiems
#    posts ir likes. Miestus pažymėkite atskira spalva
#    Naudokite duomenų rinkinį df2
#    Kurie miestai panašiausi vienas į kitą?
ggplot(df2, aes(posts, likes,color=city))+
geom_point()

# 10) Papildykite 9) klausimo ggplot komandą taip, kad 
#     a) x ašies pavadinimas būtų “Žinučių skaičius ant naudotojo sienos per savaitę”, 
#        o y ašies "Patiktukų skaičius per savaitę, kuriuos vartotojas paspaudė".
#     b) legendos pavadinimas būtų "Miestas".
#     c) būtų pateiktas grafiko pavadinimas: "Sklaidos diagrama".
