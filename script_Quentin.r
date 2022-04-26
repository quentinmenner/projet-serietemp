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
library(patchwork)


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
test_adf = urca::ur.df(diff_ts)
summary(test_adf)
# On rejette l'hypothèse nulle

# PP test
test_pp = tseries::pp.test(diff_ts) # test utilisé dans corrigé
summary(test_pp)
# On rejette l'hypothèse nulle

# KPSS
test_kpss = urca::ur.kpss(diff_ts)
summary(test_kpss)
# On ne rejette pas l'hypothèse nulle 

# La série différenciée est donc stationnaire.

# Représentation de la série avant / après 
plot(diff_ts)

# Partie 2 ####

# Question 4 : Estimation un ARMA(p,q)
acf(diff_ts)
# valeur max p est de 2 (pour le MA)

pacf(diff_ts)
# valeur max q est de 3 (pour le AR) en regardant assez largement

# Testons cette hypothèses
model_maxi <- arima(diff_ts, order = c(2,0,1))
residus_maxi <- residuals(model_maxi)
# Parait bon :
ggAcf(residus_maxi) + ggPacf(residus_maxi)

portes::LjungBox(model_maxi, order = 6)
# Le modèle maximal parait passer le test d'autocorrélation des résidus.

#Test des valeurs possibles de p et de q : test de tous les modèles

lmtest::coeftest(model_maxi) # aucun coefficient n'est significatif

evaluation_model <- function(order, x = diff_ts, lags = 24,...){
  model <- forecast::Arima(x, order = order,...)
  residus <- residuals(model)
  # Test d'autocorrélation des résidus
  lbtest <- t(sapply(1:lags,function(l){
    if(l <=  length(coef(model))){
      b <- list(statistic = NA, p.value = NA)
    }else{
      b <- Box.test(residus,"Ljung-Box",lag = l,
                    fitdf = length(coef(model))
      )
    }
    data.frame(lag = l,
               b$statistic,
               b$p.value
    )
  }))
  # on ajoute un tryCatch pour éviter les erreurs
  ttest <- tryCatch(lmtest::coeftest(model), error = NULL)
  qualite <- c(AIC(model), BIC(model), accuracy(model))
  names(qualite) <- c("AIC", "BIC", colnames(accuracy(model)))
  list(model = model,
       ttest = ttest,
       lbtest = lbtest,
       qualite = qualite)
  
}

models_possibles <- expand.grid(p = c(0, 1, 2), d = 0, q = c(0, 1, 2, 3))

models_evalues <- apply(models_possibles,1, evaluation_model)

names(models_evalues) <- sprintf("ARIMA(%i,%i,%i)", models_possibles[,"p"],
                                 models_possibles[,"d"], models_possibles[,"q"])

models_evalues$`ARIMA(0,0,0)`
# Résidus non corrélés mais coefficient pas significatif

models_evalues$`ARIMA(1,0,0)`
# Résidus non corrélés globalement et coefficients significatif modèle retenu

models_evalues$`ARIMA(2,0,0)`
# Résidus non corrélés globalement mais coefficient ar2 pas significatif

models_evalues$`ARIMA(0,0,1)`
# Résidus non corrélés globalement et coefficients significatifs

models_evalues$`ARIMA(1,0,1)`
# Résidus non corrélés globalement mais coefficient ma1 pas significatif

models_evalues$`ARIMA(2,0,1)`
# Résidus non corrélés globalement et coefficients significatifs à 5% 

models_evalues$`ARIMA(0,0,2)`
# Résidus corrélés mais coefficients significatifs

models_evalues$`ARIMA(1,0,2)`
# Résidus non corrélés globalement mais coefficients pas significatifs

models_evalues$`ARIMA(2,0,2)`
# Très peu d'autocorrélation mais coefficients ar1 et ma1 non significatifs

models_evalues$`ARIMA(0,0,3)`
# Très peu d'autocorrélation mais coefficient ma3 non significatif

models_evalues$`ARIMA(1,0,3)`
# Coefficients pas significatifs

models_evalues$`ARIMA(2,0,3)`
# Très peu d'autocorrélation mais coefficient ar1 non significatif
 
nom_modeles_retenus = c('ARIMA(1,0,0)', 'ARIMA(0,0,1)', 'ARIMA(2,0,1)')

modeles_retenus <- models_evalues[nom_modeles_retenus]

qualite_modeles_retenus <- sapply(modeles_retenus, function(x) x$qualite)

round(qualite_modeles_retenus,4)

apply(qualite_modeles_retenus,1,function(x) colnames(qualite_modeles_retenus)[which.min(x)])
# Sur les critère d'informations un AR(1) semble le meilleur choix : ce résultat 
# correspond aussi à l'hypothèse de départ étant l'ACF.

# Question 5

# Finalement on opte pour un ARIMA(1,1,0)
model <- Arima(ts_data, order = c(1,1,0))
model
checkresiduals(model)

# Partie 3 : Prédiction####

# Question 6 :..
lapply(modeles_retenus,function(x) forecast(x$model, h = 4))

# Question 7 : On suppose que les résidus suivent une loi normal centrée-réduite.

# Question 8:
autoplot(forecast(model))

predict(model, 5)
# Ce genre de modèle est difficile à prédire
