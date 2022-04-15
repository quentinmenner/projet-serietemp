###### Projet de série temporelle ######


# Packages utilisés ####

require(zoo)
require(tseries)
library(tidyverse)

# Importation et structuration des données ####
datafile <- "projet-serietemp/valeurs_mensuelles_2.csv"
data <- read.csv(datafile, sep = ";") %>% arrange(Periode)
ts_data = ts(data$Valeur, start = c(1985,01), freq = 12)
plot(ts_data, type = "l")

# Partie 1 ####

acf(ts_data) # Il semble que la série représente un AR pure

# Question 1

# Testons la stationnarité avec un test ADF
m <- fUnitRoots::adfTest(ts_data, type = "ct")
m
# On rejette fortement l'hypothèse de stationnarité
# Une tendance à la hausse semble se dégager
diff_ts = diff(ts_data, 1)

plot(diff_ts)

#Par ailleurs une saisonnalité semble s'observer (forte hausse à la fin de l'année)
desaison_ts = diff(diff_ts,12)
plot(desaison_ts)

fUnitRoots::adfTest(desaison_ts, type = "c")
# On rejette l'hypothèse de racine unitaire à 99% donc la série est stationnaire

# Représentation de la série avant / après 
library(patchwork)
(plot.ts(ts_data))+
  (plot.ts(diff_ts))

# Partie 2 ####

# Question 4 : Estimation un ARMA(p,q)
acf(desaison_ts)
# valeur max p est de 2

pacf(desaison_ts)
# valeur max q est de 3


# Partie 3 ####





#axis(1,                                                   # Add dates to x-axis
#     data$Periode,
#     format(data$Periode, "%Y-%m-%d"))
#     labels = c("Some text", "Other text"))


