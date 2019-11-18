if(!require(eurostat)) install.packages("eurostat")
require(eurostat)
if(!require(tidyverse)) install.packages("tidyverse")
require(tidyverse)
BVP_crrntmrktprice <- get_eurostat("tec00001", stringsAsFactors=FALSE) ## GDP at market prices
BVP_crrntmrktprice <- filter(BVP_crrntmrktprice,
              geo=="FR",
              unit=="CP_MEUR") ## Market prices in EUR
population_males<- get_eurostat("urb_cpop1", stringsAsFactors=FALSE) ## Population on 1 January by age groups and sex - cities and greater cities
population_males <- filter(population_males,
               time>="2007-01-01",
               indic_ur=="DE1002V", ## Population on the 1st of January, male
               cities=="FR")
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
                             sex=="M",
                             age=="Y16-64")
                             

Data <- right_join(unemployment, BVP_crrntmrktprice, by="time")
Data <- right_join(Data, mean_median_income, by="time")
Data <- right_join(Data, population_males, by="time")

Data <- Data %>% rename(unemployment=values.x, bvp=values.y, unemploymentage=age.x, mean_median_income=values.x.x, population_males=values.y.y)
Data <- select(Data, time, unemployment, bvp, indic_il, mean_median_income, population_males)
