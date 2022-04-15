###### Projet de s�rie temporelle ######


# Packages utilis�s ####

require(zoo)
require(tseries)
library(tidyverse)

# Importation et structuration des donn�es ####
datafile <- "projet-serietemp/valeurs_mensuelles_2.csv"
data <- read.csv(datafile, sep = ";") %>% arrange(Periode)
ts_data = ts(data$Valeur, start = c(1985,01), freq = 12)
plot(ts_data, type = "l")

# Partie 1 ####

acf(ts_data) # Il semble que la s�rie repr�sente un AR pure

# Question 1

# Testons la stationnarit� avec un test ADF
m <- fUnitRoots::adfTest(ts_data, type = "ct")
m
# On rejette fortement l'hypoth�se de stationnarit�
# Une tendance � la hausse semble se d�gager
diff_ts = diff(ts_data, 1)

plot(diff_ts)

#Par ailleurs une saisonnalit� semble s'observer (forte hausse � la fin de l'ann�e)
desaison_ts = diff(diff_ts,12)
plot(desaison_ts)

fUnitRoots::adfTest(desaison_ts, type = "c")
# On rejette l'hypoth�se de racine unitaire � 99% donc la s�rie est stationnaire

# Repr�sentation de la s�rie avant / apr�s 
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


