###### Projet de série temporelle ######


# Packages utilisés ####

library(zoo)
library(tseries)
library(lmtest)
library(forecast)
library(ggplot2)
library(urca)
library(tidyverse)
library(lubridate)
library(xts)

# Importation et structuration des données ####
datafile <- "projet-serietemp/valeurs_mensuelles_2.csv"
data <- read.csv(datafile, sep = ";") %>% arrange(Periode)
ts_data = ts(data$Valeur, start = c(1985,01), freq = 12)
plot(ts_data, type = "l")
# On observe une tendance à la hausse
# Données : 1985 - 2000

# Partie 1 ####

acf(ts_data) # Il semble que la série représente un AR pure

## Question 1
# Saisonnalité ? 
decomposition_saison = data %>% 
  mutate(year = str_sub(Periode, end = 4)) %>% 
  mutate(month = str_sub(Periode, -2)) %>% 
  group_by(month) %>% 
  summarise(moy_mens = mean(Valeur))

plot(decomposition_saison$moy_mens)
#On observe une très légère saisonnalité
(max(decomposition_saison$moy_mens) - min(decomposition_saison$moy_mens))/min(decomposition_saison$moy_mens)
# Les valeurs de décembre sont en moyenne 1,5% supérieur à celle de février...

# Tendance ?
data_tendance = data %>% 
  mutate(index = index(data))
library("car")

scatterplot(Valeur ~ index, data = data_tendance, 
            smoother = FALSE, grid = FALSE, frame = FALSE)

# Une tendance à la hausse semble se dégager
diff_ts = diff(ts_data, 1)

plot(diff_ts)

# ADF test
fUnitRoots::adfTest(diff_ts)
fUnitRoots::adfTest(desaison_ts, type = "c")

# PP test
tseries::pp.test(diff_ts) # test utilisé dans corrigé
summary(urca::ur.pp(diff_ts, type="Z-tau"))
summary(urca::ur.pp(diff_ts, type="Z-tau",model = "trend"))

# KPSS
tseries::kpss.test(diff_ts)

# On rejette l'hypothèse de racine unitaire donc la série différencié est stationnaire

# Représentation de la série avant / après 
library(patchwork)

# Partie 2 ####

# Question 4 : Estimation un ARMA(p,q)
acf(desaison_ts)
# valeur max p est de 2

pacf(desaison_ts)
# valeur max q est de 3

# Testons cette hypothèses
model_maxi <- arima(desaison_ts, order = c(2,0,3))
residus_maxi <- residuals(model_maxi)
# Parait bon :
ggAcf(residus_maxi) + ggPacf(residus_maxi)

portes::LjungBox(model_maxi, order = 6)


# Partie 3 ####



