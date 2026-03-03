#Elaborato Data Mining - Marco Pozzoli , Emiliano Farina
#https://www.kaggle.com/datasets/shreyasur965/recent-earthquakes

# Column Descriptors:
#   id: Unique identifier for each earthquake event.
# magnitude: The strength of the earthquake on the Richter scale.
# type: Type of seismic event (earthquake, explosion, etc.).
# title: Title of the earthquake event (place and magnitude).
# date: Date when the earthquake occurred.
# time: Time when the earthquake occurred.
# updated: Last updated timestamp for the event.
# url: Link to the earthquake event's details.
# detailUrl: Additional details URL.
# felt: Number of people who reported feeling the earthquake.
# cdi: Community Determined Intensity, how strongly the event was felt.
# mmi: Modified Mercalli Intensity, scale used to measure earthquake intensity.
# alert: Alert level (green, yellow, orange, red).
# status: Status of the event (reviewed, automatic).
# tsunami: Tsunami risk flag (0 = no risk, 1 = risk).
# sig: Significance of the earthquake, based on magnitude and impact.
# net: Network that detected the earthquake.
# code: Code assigned by the network.
# ids: IDs of other events related to the same earthquake.
# sources: Data sources for the event.
# types: Event types associated with the earthquake.
# nst: Number of seismic stations that recorded the event.
# dmin: Minimum distance to the earthquake event.
# rms: Root mean square of signal, used to measure earthquake intensity.
# gap: Data gap between stations detecting the earthquake.
# magType: Type of magnitude (e.g., mb, Ms).
# geometryType: Type of geometry for the location (usually "Point").
# depth: Depth of the earthquake in kilometers.
# latitude: Latitude of the earthquake's epicenter.
# longitude: Longitude of the earthquake's epicenter.
# place: General location description of the earthquake.
# distanceKM: Distance from the nearest populated place in kilometers.
# placeOnly: Flag indicating if only place information is available.
# location: Full address or description of the earthquake's location.
# continent: Continent where the earthquake occurred.
# country: Country where the earthquake occurred.
# subnational: Subnational region where the earthquake occurred.
# city: City closest to the earthquake.
# locality: Locality or neighborhood name.
# postcode: Postal code of the affected area.
# what3words: Unique 3-word code representing the location.
# timezone: Timezone of the earthquake.
# locationDetails: Additional details about the earthquake’s location.

#NB il tempo è indicato con il formato standard, che indica i millisecondi passati dal 01/01/1970.

# Carichiamo i dati -------------------------------------------------------

rm(list=ls())

#setwd("C:/Users/Università/Documents/Università/2024-25/Data Mining e Machine Learning/Elaborato")
#Carichiamo i dati:
#data <- read.csv("earthquakes.csv", stringsAsFactors = T, na.strings = "")
load("Pozzoli_Farina.RData")
data$tsunami = as.factor(data$tsunami)
data$mmi = as.factor(data$mmi)
data$cdi = as.factor(data$cdi)
#trattiamo tsunami, mmi e cdi come factors

#Visualizziamo la mappa, ogni punto corrisponde a un terremoto, il colore ne indica la profondità del sisma

library(ggplot2)
library(ggmap)

world_map <- map_data("world")

ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
               fill = "gray", color = "white") +
  geom_point(data = data, aes(x = longitude, y = latitude, color = depth),
             size = 3, alpha = 0.7) +
  scale_color_gradient(low = "green", high = "red") +
  labs(title = "Mappa dei Terremoti",
       x = "Longitudine", y = "Latitudine", color = "Profondità") +
  theme_minimal()


summary(data)

#controlliamo i valori mancanti
sapply(data, function(x)(sum(is.na(x))))

#Ho valori mancanti in alert (373), postcode (940), continent (270), country (338), subnational (421), city (463)

library(VIM)
missingness<- aggr(data[,c("continent","country","subnational","city","postcode","alert")], col=c('navyblue','yellow'), numbers=TRUE, sortVars=TRUE, cex.axis=.7,gap=3)

#solo il 5 percento delle righe ha tutti i valori.

#Abbiamo già un modello in mente, droppiamo alcune covariate che non ci interessano:

library(dplyr)
dp <- data %>% select(-id,-type,-title,-date,-url,-detailUrl,-status,-sig,-magType,-geometryType,-place,-placeOnly,-location,-subnational,-city,-locality,-postcode,-what3words,-locationDetails,-types,-ids,-code, -sources, -timezone)

#vediamo un sommario dei nostri dati che useremo in regressione
summary(dp)

#errore di spelling per l'oceania insolare
dp$continent[dp$continent == "Insluar Oceania"] <- "Insular Oceania"

# 1)Missing Values --------------------------------------------------------

#vediamo ora i missing values delle var rimanenti
sapply(dp, function(x)(sum(is.na(x))))

#nel caso di alert un missing value indica una mancanza dell'emanazione dell'allerta, la definiamo come allerta bianca,
#in quanto indica un terremoto con conseguenze impercettibili
#facciamo imputazione.
library(Hmisc)
dp$alert=impute(dp$alert, "white")

#vediamo i pattern in cui si presentano i missing values di country e continent
missingness<- aggr(dp[,c("continent","country")], col=c('navyblue','yellow'), numbers=TRUE, sortVars=TRUE, cex.axis=.7,gap=3)

# il 2.3% delle righe ha la nazione ma non il continente
# il 8.3% ha il continente ma non la nazione
# al 21.5% mancano entrambi
# il 68% ha entrambi

datasicountry <- dp[is.na(dp$continent) & !is.na(dp$country) , ]
nrow(datasicountry)

#Ho 26 ossrvazioni dove ho la nazione ma non il continente. Le correggiamo a mano.
unique(dp[is.na(dp$continent) & !is.na(dp$country) , ]$country)
dp <- dp %>%
  mutate(continent = case_when(
    country == "Chile" & is.na(continent) ~ factor("South America", levels = levels(continent)),  # Nuova categoria
    TRUE ~ continent  # Mantieni i valori esistenti
  ))
dp <- dp %>%
  mutate(continent = case_when(
    country == "Peru" & is.na(continent) ~ factor("South America", levels = levels(continent)),  # Nuova categoria
    TRUE ~ continent  # Mantieni i valori esistenti
  ))
dp <- dp %>%
  mutate(continent = case_when(
    country == "Japan" & is.na(continent) ~ factor("Asia", levels = levels(continent)),  # Nuova categoria
    TRUE ~ continent  # Mantieni i valori esistenti
  ))
dp <- dp %>%
  mutate(continent = case_when(
    country == "Taiwan (Province of China)" & is.na(continent) ~ factor("Asia", levels = levels(continent)),  # Nuova categoria
    TRUE ~ continent  # Mantieni i valori esistenti
  ))
dp <- dp %>%
  mutate(continent = case_when(
    country == "United States of America (the)" & is.na(continent) ~ factor("North America", levels = levels(continent)),  # Nuova categoria
    TRUE ~ continent  # Mantieni i valori esistenti
  ))
dp <- dp %>%
  mutate(continent = case_when(
    country == "Indonesia" & is.na(continent) ~ factor("Insular Oceania", levels = levels(continent)),  # Nuova categoria
    TRUE ~ continent  # Mantieni i valori esistenti
  ))

#Ho così risolto le righe in cui ho la nazione ma non il continente.

#Le osservazioni dove non ho nessuno dei due valori fanno riferimento a terremoti oceanici.
#gli do il valore "Ocean"
dataness <- dp[is.na(dp$continent) & is.na(dp$country) , ]
nrow(dataness)

dp$continent <- factor(dp$continent, levels = c(levels(dp$continent), "Ocean"))

dp <- dp %>%
  mutate(continent = case_when(
    is.na(country) & is.na(continent) ~ "Ocean",  # Sostituzione con "Ocean"
    TRUE ~ as.factor(continent)  # Mantieni i valori esistenti come caratteri
  ))

#A questo punto tutte le osservazioni hanno il continente, che è ciò che ci interessa, country non ci serve.
dp <- dp %>% select(-country)
colnames(dp)
dp$continent <- factor(dp$continent)
#+++++++++++++++++++++++++++++++++++++++
#world_map <- map_data("world")
#
# Crea la mappa con i punti colorati per magnitudo
# ggplot() +
#   geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
#                fill = "gray", color = "white") +
#   geom_point(data = datanocc, aes(x = longitude, y = latitude, color = magnitude),
#              size = 3, alpha = 0.7) +
#   scale_color_gradient(low = "green", high = "red") +
#   labs(title = "Mappa dei Terremoti",
#        x = "Longitudine", y = "Latitudine", color = "Magnitudo") +
#   theme_minimal()

#OSSERVO CHE LE OSSERVAZIONI CON MISSING VALUE IN COUNTRY E CONTINENT SONO TERREMOTI MARITTIMI
#OSSERVAZIONI CON COUNTRY MA NON CONTINENT SONO TERREMOTI MOLTO VICINI ALLA COSTA
#++++++++++++++++++++++++++++++++++++++++++


# 1.b)Collinearità --------------------------------------------------------

#creiamo il nostro primo modello (starting model)
lm <- lm(depth ~ .,dp)
summary(lm)

drop1(lm,test="F")

par(mfrow=c(2,2)) 
plot(lm)
par(mfrow=c(1,1))

num<-unlist(lapply(dp,is.numeric))

Cor=round(cor(dp[ , num]),3)
Cor

library(corrplot)
corrplot(Cor)



#Valutiamo VIF e TOL
#usiamo target fittizio
y = as.numeric(row.names(dp))
#selezioniamo covariate.
cov=attr(terms(lm), "term.labels") 
d_numeric <- dp[,cov]%>% dplyr::select_if(is.numeric)
X<-d_numeric
X=as.matrix(X)

library(mctest)
#vediamo vif e tol
fit=lm(y ~ X)
imcdiag(fit)
#rimuovo time
X <- as.data.frame(X) %>% select(-time) %>% as.matrix()

fit=lm(y ~ X)
imcdiag(fit)
#Ora ho ridotto la collinearità a valori accettabili.

lm2 <-lm(depth~ .-time, dp)
summary(lm2)
drop1(lm2,test="F")

#vediamo ora la collinearità per i valori categorici

d_fac <- dp[,cov]%>% dplyr::select_if(is.factor)

library(plyr)

combos <- combn(ncol(d_fac),2)
adply(combos, 2, function(x) {
  test <- chisq.test(d_fac[, x[1]],d_fac[, x[2]])
  tab  <- table(d_fac[, x[1]], d_fac[, x[2]])
  out <- data.frame("Row" = colnames(d_fac)[x[1]]
                    , "Column" = colnames(d_fac[x[2]])
                    , "Chi.Square" = round(test$statistic,3)
                    , "df"= test$parameter
                    , "p.value" = round(test$p.value, 3)
                    , "n" = sum(table(d_fac[,x[1]], d_fac[,x[2]]))
                    , "u1" =length(unique(d_fac[,x[1]]))-1
                    , "u2" =length(unique(d_fac[,x[2]]))-1
                    , "nMinu1u2" =sum(table(d_fac[,x[1]], d_fac[,x[2]]))* min(length(unique(d_fac[,x[1]]))-1 , length(unique(d_fac[,x[2]]))-1) 
                    , "Chi.Square norm"  =test$statistic/(sum(table(d_fac[,x[1]], d_fac[,x[2]]))* min(length(unique(d_fac[,x[1]]))-1 , length(unique(d_fac[,x[2]]))-1)) 
  )
  
  
  return(out)
  
}) 

#Ho un chi quadro normalizzato maggiore di 0.3 tra mmi e alert.
#tolgo alert in quanto ha chi quadri piuttosto elevati con tutte le altre var qualitative
dp <- dp %>% select(-alert)

d_fac <- dp[,c("cdi","mmi","tsunami","net","continent")]

library(plyr)

combos <- combn(ncol(d_fac),2)
adply(combos, 2, function(x) {
  test <- chisq.test(d_fac[, x[1]],d_fac[, x[2]])
  tab  <- table(d_fac[, x[1]], d_fac[, x[2]])
  out <- data.frame("Row" = colnames(d_fac)[x[1]]
                    , "Column" = colnames(d_fac[x[2]])
                    , "Chi.Square" = round(test$statistic,3)
                    , "df"= test$parameter
                    , "p.value" = round(test$p.value, 3)
                    , "n" = sum(table(d_fac[,x[1]], d_fac[,x[2]]))
                    , "u1" =length(unique(d_fac[,x[1]]))-1
                    , "u2" =length(unique(d_fac[,x[2]]))-1
                    , "nMinu1u2" =sum(table(d_fac[,x[1]], d_fac[,x[2]]))* min(length(unique(d_fac[,x[1]]))-1 , length(unique(d_fac[,x[2]]))-1) 
                    , "Chi.Square norm"  =test$statistic/(sum(table(d_fac[,x[1]], d_fac[,x[2]]))* min(length(unique(d_fac[,x[1]]))-1 , length(unique(d_fac[,x[2]]))-1)) 
  )
  
  
  return(out)
  
}) 

#non ho più chi quadri normalizzati maggiori di 0.3
#otteniamo il seguente modello senza collinearità:
lm2 <-lm(depth~ magnitude + updated + felt + cdi + mmi + tsunami + net + nst + dmin + rms + gap + latitude + distanceKM +  continent + longitude, dp)
summary(lm2)
drop1(lm2,test="F")

#Guardiamo il diagnostic plot
par(mfrow=c(2,2)) 
plot(lm2)
par(mfrow=c(1,1))
#molto male:
# primo plot: ipotesi linearità -> RIFIUTO
# secondo: hp normalità residui -> RIFIUTO
# terzo: hp omoschedasticità -> RIFIUTO
# quarto: outliers -> ci sono outliers da sistemare.


# 2)Analisi eteroschedasticità e Linearità --------
#  Trasformazioni, Additive models (loess o splines)

#trasformazione Box-Cox
library(MASS)
#boxcoxreg1<-boxcox(lm2)
#abbiamo bisogno di un target positivo, modifichiamo il target.
#depth ha minimo -0.25
dp$depth = dp$depth + 0.5

lm3 <- lm(depth ~ magnitude + updated + felt + cdi + mmi + tsunami + net + nst + dmin + rms + gap + latitude + distanceKM +  continent + longitude, dp)
summary(lm3)

boxcoxreg1<-boxcox(lm3)

title("Lambda")
lambda=boxcoxreg1$x[which.max(boxcoxreg1$y)]
lambda

#la lambda ha valore -0.06060606, suggerisce una trasformazione logaritmica in quanto vicina allo zero.
lm3_log <- lm(log(depth) ~ magnitude + updated + felt + cdi + mmi + tsunami + net + nst + dmin + rms + gap + latitude + distanceKM +  continent + longitude , dp)
summary(lm3_log)
#passo da un R2adj=0.5689 a uno con R2adj=0.5887
#c'è un miglioramento.

par(mfrow=c(2,2))
plot(lm3_log)
par(mfrow=c(1,1))
#la situazione è migliorata, ma non è ancora buona.

#Proviamo a usare i Generalized Additive Models
library(gam)

#riscriviamo lm3_log con questa libreria, in modo da poter fare comparazioni
lm3_log <- gam(log(depth) ~ magnitude + updated + felt + cdi + mmi  + tsunami + net + nst + dmin + rms + gap + latitude + distanceKM +  continent + longitude , data=dp)


lm3_log_gam <- gam(log(depth) ~ s(magnitude) + s(updated) + s(felt) + cdi + mmi  + tsunami + net + s(nst) + s(dmin) + s(rms) + s(gap) + s(latitude) + s(distanceKM) + continent + s(longitude) , data=dp)
summary(lm3_log_gam)

lm3_log_gamlo <- gam(log(depth) ~ lo(magnitude) + lo(updated) + lo(felt) + cdi + mmi + tsunami + net + lo(nst) + lo(dmin) + lo(rms) + lo(gap) + lo(latitude) + lo(distanceKM) + continent + lo(longitude) , data=dp)
summary(lm3_log_gamlo)

anova(lm3_log, lm3_log_gam , lm3_log_gamlo , test="F")

#il GAM usando splines è SIGNIFICATIVAMENTE migliore del modello log-lineare.
#il GAM usando splines è anche migliore del gam usando loess.

par(mfrow=c(3,3))
plot(lm3_log_gam)
par(mfrow=c(1,1))

#Trasformiamo le covariate linearizzandole.
library(lmtest)
dp2 <- dp
dp2$gap = dp2$gap + 1 #in modo da poter usare il log di gap
dp2$longitude = dp2$longitude + 200

lm3_lin <- lm(log(depth) ~ magnitude + I(updated^3) + felt + cdi + mmi + tsunami 
                    + net + I(nst^3 + nst^2) + I(dmin^3) + I(rms^2) + I(log(gap)) + I(latitude^2) 
                    + I(distanceKM^3) +  continent + I(log(longitude)) , data=dp2)
summary(lm3_lin)#R2 = 0.6023
resettest(lm3_lin, power = 3, type = "fitted",  data = dp2)#0.002679

#Ho ottenuto il miglior R2 e reset test possibile per ora trasformando.

par(mfrow=c(2,2))
plot(lm3_lin)
par(mfrow=c(1,1))


# 2.b)Optimal Grouping ----------------------------------------------------

#Proviamo a unire livelli delle variabili categoriche del nostro modello e osserviamo i risultati.
#le nostre variabili categoriche sono: "cdi","mmi","net","continent"
#vediamo quanti livelli hanno ciascuna:
sapply(dp2[ , c("cdi","mmi","net","continent")], function(x) length(unique(x)))
library(factorMerger)

#Iniziamo con cdi
liv <- mergeFactors(response = dp2$depth, factor = dp2$cdi)
plot(liv , panel = "GIC",title = "Optimal grouping cdi", panelGrid = FALSE )

#Inseriamo i nuovi livelli nel dataset
cdiop=cutTree(liv)
dp2$optimal_group=cdiop

dp2$cdi_group =as.factor(as.numeric(dp2$optimal_group))
plot(dp2$cdi,dp2$depth)
table(dp2$cdi_group,dp2$cdi)
plot(dp2$cdi_group,dp2$depth)
#Compariamo i modelli con e senza optimal grouping
lm3_merge1 <- lm(log(depth) ~ magnitude + I(updated^3) + felt + cdi + mmi + tsunami 
                 + net + I(nst^3 + nst^2) + I(dmin^3) + I(rms^2) + I(log(gap)) + I(latitude^2) 
                 + I(distanceKM^3) +  continent + I(log(longitude)) , data=dp2)
summary(lm3_merge1)#R2 = 0.6023
resettest(lm3_merge1, power = 3, type = "fitted",  data = dp2)#0.002679

lm3_merge2 <- lm(log(depth) ~ magnitude + I(updated^3) + felt + cdi_group + mmi + tsunami 
                 + net + I(nst^3 + nst^2) + I(dmin^3) + I(rms^2) + I(log(gap)) + I(latitude^2) 
                 + I(distanceKM^3) +  continent + I(log(longitude)) , data=dp2)
summary(lm3_merge2)#R2 = 0.6033
resettest(lm3_merge2, power = 3, type = "fitted",  data = dp2)#0.003335

drop1(lm3_merge1,test="F")
drop1(lm3_merge2,test="F")
AIC(lm3_merge1,lm3_merge2)
#Migliorano sia R2 che linearità. L'AIC migliora. teniamo i valori raggruppati di cdi

#Facciamo lo stesso con MMI
liv <- mergeFactors(response = dp2$depth, factor = dp2$mmi)
plot(liv , panel = "GIC",title = "Optimal grouping mmi", panelGrid = FALSE )

#Inseriamo i nuovi livelli nel dataset
mmiop=cutTree(liv)
dp2$optimal_group=mmiop

dp2$mmi_group =as.factor(as.numeric(dp2$optimal_group))
table(dp2$mmi_group,dp2$mmi)
plot(dp2$mmi_group,dp2$depth)
#Compariamo i modelli con e senza optimal grouping
lm3_merge3 <- lm(log(depth) ~ magnitude + I(updated^3) + felt + cdi_group + mmi_group + tsunami 
                 + net + I(nst^3 + nst^2) + I(dmin^3) + I(rms^2) + I(log(gap)) + I(latitude^2) 
                 + I(distanceKM^3) +  continent + I(log(longitude)) , data=dp2)
summary(lm3_merge3)#R2 = 0.571
resettest(lm3_merge3, power = 3, type = "fitted",  data = dp2)#0.01029

drop1(lm3_merge2,test="F")
drop1(lm3_merge3,test="F")
AIC(lm3_merge2,lm3_merge3)
#Tengo mmi originale, AIC peggiora sensibilmente raggruppandolo.

#Ora lo facciamo su NET
liv <- mergeFactors(response = dp2$depth, factor = dp2$net)
plot(liv , panel = "GIC",title = "Optimal grouping net", panelGrid = FALSE )

#Inseriamo i nuovi livelli nel dataset
netop=cutTree(liv)
dp2$optimal_group=netop

dp2$net_group =as.factor(as.numeric(dp2$optimal_group))
table(dp2$net_group,dp2$net)
plot(dp2$net_group,dp2$depth)
#vediamo i modelli
lm3_merge4 <- lm(log(depth) ~ magnitude + I(updated^3) + felt + cdi_group + mmi + tsunami 
                 + net_group + I(nst^3 + nst^2) + I(dmin^3) + I(rms^2) + I(log(gap)) + I(latitude^2) 
                 + I(distanceKM^3) +  continent + I(log(longitude)) , data=dp2)
summary(lm3_merge4)#R2 = 0.5944
resettest(lm3_merge4, power = 3, type = "fitted",  data = dp2)#0.0124

drop1(lm3_merge2,test="F")
drop1(lm3_merge4,test="F")
AIC(lm3_merge2,lm3_merge4)
#tengo net sciolto, AIC peggiora.

#Ora lo facciamo su continent
liv <- mergeFactors(response = dp2$depth, factor = dp2$continent)
plot(liv , panel = "GIC",title = "Optimal grouping continent", panelGrid = FALSE )

#Inseriamo i nuovi livelli nel dataset
contop=cutTree(liv)
dp2$optimal_group=contop

dp2$cont_group =as.factor(as.numeric(dp2$optimal_group))
table(dp2$cont_group,dp2$continent)
plot(dp2$cont_group,dp2$depth)
#vediamo i modelli
lm3_merge5 <- lm(log(depth) ~ magnitude + I(updated^3) + felt + cdi_group + mmi + tsunami 
                 + net + I(nst^3 + nst^2) + I(dmin^3) + I(rms^2) + I(log(gap)) + I(latitude^2) 
                 + I(distanceKM^3) +  cont_group + I(log(longitude)) , data=dp2)
summary(lm3_merge5)#R2 = 0.5141
resettest(lm3_merge5, power = 3, type = "fitted",  data = dp2)#03e-13

drop1(lm3_merge2,test="F")
drop1(lm3_merge5,test="F")
AIC(lm3_lin, lm3_merge2,lm3_merge3,lm3_merge4,lm3_merge5)
#Quello di cont è il peggior raggruppamento finora

#Dunque l'optimal grouping va fatto solo su cdi, sulle altre variabili non è utile.
#tengo lm3_merge2 come modello.
par(mfrow=c(2,2))
plot(lm3_merge2)
par(mfrow=c(1,1))


# 3)Analisi outliers e leverage: Diagnostiche e Robust regression ---------

library(car)
influencePlot(lm3_merge2,  main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

stzed <- rstudent(lm3_merge2)
lever <- hat(model.matrix(lm3_merge2))
dffits1 <- dffits(lm3_merge2)
cooksd <- cooks.distance(lm3_merge2)
#noto che la osservazione 476 ha valori anomali -> è l'unica osservazione con contintent = "Oceania"!!!!
#Nel togliere gli outliers probabilmente perderò le categorie con poche osservazioni.
cutoffhv = (2*length(lm3_merge2$coefficients) + 2)/nrow(dp2) # 0.0703606
cutoffck = 4/length(lm3_merge2$coefficients) #0.1025641
cutoffdf = 2*sqrt(length(lm3_merge2$coefficients)/nrow(dp2)) #0.3704095

stzfiltr <- dp2[abs(stzed)<2.5 & !is.na(stzed), ]
levfiltr <- dp2[abs(lever)<cutoffhv & !is.na(lever), ]
dfsfiltr <- dp2[abs(dffits1)<cutoffdf & !is.na(dffits1), ]
cokfiltr <- dp2[abs(cooksd)<cutoffck & !is.na(cooksd), ]
nrow(dp2) - nrow(stzfiltr) #37 oss. droppate
nrow(dp2) - nrow(levfiltr) #92 oss. droppate
nrow(dp2) - nrow(dfsfiltr) #79 oss. droppate
nrow(dp2) - nrow(cokfiltr) #3 oss.droppate

lm4 <- lm(log(depth) ~ magnitude + I(updated^3) + felt + cdi_group + mmi + tsunami 
                 + net + I(nst^3 + nst^2) + I(dmin^3) + I(rms^2) + I(log(gap)) + I(latitude^2) 
                 + I(distanceKM^3) +  continent + I(log(longitude)) , data=dfsfiltr)
summary(lm4)#R2 = 0.7088 
resettest(lm4, power = 3, type = "fitted",  data = dfsfiltr)#0.03969
#ottengo i valori migliori togliendo i dfitts
#tolgo ora i punti di leva

influencePlot(lm4,  main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

par(mfrow=c(2,2))
plot(lm4)
par(mfrow=c(1,1))

lever2 <- hat(model.matrix(lm4))
cutoffhv2 = (2*length(lm4$coefficients) + 2)/nrow(dfsfiltr)#
levfiltr2 <- dfsfiltr[abs(lever2)<cutoffhv2 & !is.na(lever2), ]
nrow(dfsfiltr) - nrow(levfiltr2) #79 oss droppate

lm4b <- lm(log(depth) ~ magnitude + I(updated^3) + felt + cdi_group + mmi + tsunami 
          + net + I(nst^3 + nst^2) + I(dmin^3) + I(rms^2) + I(log(gap)) + I(latitude^2) 
          + I(distanceKM^3) +  continent + I(log(longitude)) , data=levfiltr2)
summary(lm4b)#R2 = 0.6864
resettest(lm4b, power = 3, type = "fitted",  data = levfiltr2)#0.7064
influencePlot(lm4b,  main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

par(mfrow=c(2,2))
plot(lm4b)
par(mfrow=c(1,1))
#Perdo leggermente in quanto a R2, guadagno moltissimo in linearità: di fatto ho eliminato le oss.
#che hanno come osservazioni qualitative "rare", cioè quelle che hanno valori unici o quasi.
#Giusto eliminarle: costruire il modello su pochi casi rari non ha senso, non sono sufficienti
#per avere un modello robusto.
#Questo spiega anche la perdita in R2.

# 3.b)Robust Regression ---------------------------------------------------
#vediamo cosa otteniamo usando la robust regression.
library(robustbase)
control <- lmrob.control(maxit = 500, tuning.psi = 3.0)
lmrobfit <- lmrob(log(depth) ~ magnitude + cdi_group + mmi  + tsunami +
                   net + I(nst^3 + nst^2) + I(rms^2) + I(log(gap)) +
                   continent + I(log(longitude)),
                 data = levfiltr2, control = control)

summary(lmrobfit)
par(mfrow=c(3,2))
plot(lmrobfit)
par(mfrow=c(1,1))

#doesn't work well
# 4)Model Selection -------------------------------------------------------

step <- stepAIC(lm4b, direction="both")
step
step$anova

lm5 <- lm(log(depth) ~ magnitude + cdi_group + mmi + tsunami 
           + net + I(nst^3 + nst^2) + I(dmin^3) + I(rms^2) + I(log(gap)) + 
            + continent + I(log(longitude)) , data=levfiltr2)
summary(lm5)#R2 = 0.6874
resettest(lm5, power = 3, type = "fitted",  data = levfiltr2)#0.6304
drop1(lm5,test="F")#dmin^3 non è significativo, lo tolgo.

lm5 <- lm(log(depth) ~ magnitude + cdi_group + mmi + tsunami 
          + net + I(nst^3 + nst^2) + I(rms^2) + I(log(gap)) + 
            + continent + I(log(longitude)) , data=levfiltr2)
summary(lm5)#R2 = 0.687
resettest(lm5, power=3, type = "fitted",  data = levfiltr2)#0.5588

#Vediamo con BIC
step2 <- stepAIC(lm4b, direction="both", k=log(nrow(levfiltr2)))
step2
step2$anova
#Mi fa togliere molte più covariate! (BIC preferisce modelli più semplici)
lm5b <- lm(log(depth) ~ magnitude + cdi_group + mmi + I(rms^2) + I(log(gap)) +  continent + I(log(longitude)) , data=levfiltr2)
summary(lm5b)#R2 = 0.6801
resettest(lm5b,power = 3,  type = "fitted",  data = levfiltr2)#0.5628

#il primo ha AIC minore, il secondo BIC minore. (il secondo è più semplice, cosa che BIC apprezza)
AIC(lm5,lm5b)
BIC(lm5,lm5b)
length(lm5$coefficients)#23
length(lm5b$coefficients)#17
bptest(lm5)
bptest(lm5b)





# 5)Interpretazione modello: Effetti e significatività parametri ----------
#Confrontiamo i plot del modello iniziale con quello finale
par(mfrow=c(2,2))
plot(lm)
par(mfrow=c(1,1))
par(mfrow=c(2,2))
plot(lm5b)
par(mfrow=c(1,1))
#grandissimo miglioramento
library(gvlma)
gvlma(lm5b)

library(sandwich)
coeftest(lm5b, vcov=vcovHC(lm5b)) 

library(lsmeans)
avPlots(lm5b)
drop1(lm5b,  test="F")

lm5b$coefficients

library(forestmodel)
print(forest_model(lm5b))

library(coefplot)
coefplot(lm5b, intercept=FALSE)
#effetti categoriche. Uso lsmeans, vedo le medie marginali dei valori previsti con una certa categoria.
#vedo anche come cambiano in base alla categoria nei contrast.

ls=lsmeans(lm5b,pairwise ~ cdi_group, adjust="tukey")
ls
plot(ls$lsmeans, alpha = .05)

ls2=lsmeans(lm5b,pairwise ~ mmi)
ls2
plot(ls2$lsmeans, alpha = .05)

ls3=lsmeans(lm5b,pairwise ~ continent, adjust="tukey")
ls3
plot(ls3$lsmeans, alpha = .05)

#confronto a coppie:
ls$contrasts#cdi_group
ls2$contrasts#mmi
ls3$contrasts#continent


# 6)Robust Inference ------------------------------------------------------

BOOT.MOD=Boot(lm5b, R=1999)
summary(BOOT.MOD, high.moments=TRUE)

Confint(BOOT.MOD, level=c(.95), type="perc")

hist(BOOT.MOD, legend="separate")

#Tutti i coefficienti del nostro modello sono all'interno dei rispettivi intervalli di confidenza.
#per alcuni valori di mmi ci sono beta che potrebbero avere valore 0 (compreso nell'intervallo di confidenza),
#tuttavia lo sapevamo già, guardando i test.

#Tutti i coefficienti del nostro modello corrispondono, o sono molto vicini, alla media. Hanno bias ristretto.






# Parte 2: Modellazione variabile binaria ---------------------------------

rm(list=ls())

#setwd("C:/Users/Università/Documents/Università/2024-25/Data Mining e Machine Learning/Elaborato")
#Carichiamo i dati:
#data <- read.csv("earthquakes.csv", stringsAsFactors = T, na.strings = "")
#trattiamo tsunami, mmi e cdi come factors
load("Pozzoli_Farina.RData")
data$tsunami = as.factor(data$tsunami)
data$mmi = as.factor(data$mmi)
data$cdi = as.factor(data$cdi)

#trasformiamo la variabile target (depth) in binaria, discretizzandola.
hist(data$depth)
#Usiamo la soglia di 70 km di profondità.
#al di sotto troviamo i terremoti poco profondi ("shallow"), valore 0
#al di sopra abbiamo i terremoti profondi ("deep"), valore 1
data$dbin <- ifelse(data$depth > 70, 1, 0)


#Sistemiamo i missing data come abbiamo fatto precedentemente
library(dplyr)
b <- data %>% select(-id,-depth,-type,-title,-date,-url,-detailUrl,-status,-sig,-magType,-geometryType,-place,-placeOnly,-location,-subnational,-city,-locality,-postcode,-what3words,-locationDetails,-types,-ids,-code, -sources, -timezone)
b$continent[b$continent == "Insluar Oceania"] <- "Insular Oceania"
library(Hmisc)
b$alert=impute(b$alert, "white")

b <- b %>%
  mutate(continent = case_when(
    country == "Chile" & is.na(continent) ~ factor("South America", levels = levels(continent)),  # Nuova categoria
    TRUE ~ continent  # Mantieni i valori esistenti
  ))
b <- b %>%
  mutate(continent = case_when(
    country == "Peru" & is.na(continent) ~ factor("South America", levels = levels(continent)),  # Nuova categoria
    TRUE ~ continent  # Mantieni i valori esistenti
  ))
b <- b %>%
  mutate(continent = case_when(
    country == "Japan" & is.na(continent) ~ factor("Asia", levels = levels(continent)),  # Nuova categoria
    TRUE ~ continent  # Mantieni i valori esistenti
  ))
b <- b %>%
  mutate(continent = case_when(
    country == "Taiwan (Province of China)" & is.na(continent) ~ factor("Asia", levels = levels(continent)),  # Nuova categoria
    TRUE ~ continent  # Mantieni i valori esistenti
  ))
b <- b %>%
  mutate(continent = case_when(
    country == "United States of America (the)" & is.na(continent) ~ factor("North America", levels = levels(continent)),  # Nuova categoria
    TRUE ~ continent  # Mantieni i valori esistenti
  ))
b <- b %>%
  mutate(continent = case_when(
    country == "Indonesia" & is.na(continent) ~ factor("Insular Oceania", levels = levels(continent)),  # Nuova categoria
    TRUE ~ continent  # Mantieni i valori esistenti
  ))
b$continent <- factor(b$continent, levels = c(levels(b$continent), "Ocean"))
b <- b %>%
  mutate(continent = case_when(
    is.na(country) & is.na(continent) ~ "Ocean",  # Sostituzione con "Ocean"
    TRUE ~ as.factor(continent)  # Mantieni i valori esistenti come caratteri
  ))

b <- b %>% select(-country)
colnames(b)
b$continent <- factor(b$continent)

#Abbiamo così sistemato i missing values.


#Diamo un'occhiata ai dati
table(b$dbin)
prop.table(table(b$dbin))



# Modello -----------------------------------------------------------------
#Costruiamo il nostro modello
logi2 = glm(dbin ~ magnitude + tsunami + rms + longitude, data=b,family = binomial)
summary(logi2)


#Osserviamo i valori dei coefficienti
library(coefplot)
coefplot(logi2, intercept=FALSE)
library(forestmodel)
print(forest_model(logi2))

#Verifichiamo la Zero Variance
library(funModeling)
status=df_status( b[ ,c("magnitude" , "tsunami" , "rms" , "longitude") ], print_results = F)
status
#Non ho zero variance, tutte le variabili hanno diversi valori diversi. 
#non ho nemmeno Na in quanot li ho gestiti prima.

#Controlliamo la collinearità
#Valutiamo VIF e TOL
library(mctest)
#usiamo target fittizio
y = as.numeric(row.names( b))
#selezioniamo covariate.
cov=attr(terms(logi2), "term.labels") 
d_numeric <- b[,cov]%>% dplyr::select_if(is.numeric)
X<-d_numeric
X=as.matrix(X)

fit=lm(y ~ X)
imcdiag(fit)

#Non è presente collinearità: ho VIF contenuti e TOL maggiori di 0.3

#Odds Ratio (quanto il valore 1 è più probabile dello 0 date le x_i)
exp(logi2$coefficients)
exp(confint(logi2))

#Osserviamo
a=exp(cbind(OR=coef(logi2), confint(logi2)))
round(a, digits = 4)


#Guardiamo come cambia la devianza del modello se escludiamo una variabile.
drop1(logi2, test="LRT")
#controlliamo se c'è separation tra tsunami e dbin
table(b$dbin,b$tsunami)
# non c'è 

#Le variabili sono tutte significative, il modello peggiora se escludiamo una delle var
par(mfrow=c(2,2))
plot(logi2)
par(mfrow=c(1,1))


#Guardiamo quanto è accurato nelle prediction il nostro modello
predicted_p <- predict(logi2, b, type="response") 
head(predicted_p)

b$predicted_p <- predict(logi2, b, type="response") 
tail(b)

#Vediamo le previsioni
b$predicted_y <- ifelse(b$predicted_p > 0.5,1,0)
tail(b)

#Classificazione:
table(observed=b$dbin, predicted=b$predicted_y)

#in percentuale
t=table(observed=b$dbin,predicted=b$predicted_y)
t
t2=t/sum(t)
t2

#accuratezza
acc=t2[1,1]+t2[2,2]
acc
#Error rate
1-acc
#buono nel predire gli 0, male per gli 1.