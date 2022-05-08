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

# Lieu exportation graphiques
setwd(dir = "~/projet-serietemp")
lien_graph = "Graphiques"

# Importation et structuration des données ####

# en object data.frame
datafile <- "valeurs_mensuelles_2.csv"
data <- read.csv(datafile, sep = ";") %>% 
  arrange(Periode) %>% 
  mutate(Periode = ym(Periode))

#Passage en en object ts
ts_data <- ts(data$Valeur, start = c(1985,01), freq = 12)

# On observe une tendance à la hausse

# Partie 1 ####

## Question 2 ####

# Y-a-t-il une saisonnalité ? 
decomposition_saison <- data %>% 
  mutate(year = year(Periode)) %>% 
  mutate(month = month(Periode, label = TRUE, abbr = FALSE,locale = "French")) %>% 
  select(month, Valeur)

gg3 = ggplot(decomposition_saison, aes(group = month, x = month, y =Valeur)) +
  geom_boxplot()+
  ggthemes::theme_stata()+
  theme(
    plot.title   = element_text(lineheight = 0.8, face = "bold", hjust = 0.5, size = 15),
    axis.text.x  = element_text(angle = 45,hjust = 1),
    axis.text.y  = element_text(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())
gg3  

saveRDS(gg3, file.path(lien_graph,"boxplot.rds"))
# On observe une très légère saisonnalité mais rien de très marqué au point de réaliser 
# une différenciation saisonnière

# Y-a-t-il une tendance ?
data_tendance <- data %>% 
  mutate(index = index(data))

# Régression linéaire sur le temps pour observer s'il y a une tendance
lm(data_tendance$Valeur ~ data_tendance$index)

# Une hausse moyenne de 8.7% par ans. Il y a donc une tendance nette à la hausse.
# Il faut donc réaliser une différenciation première

# Différenciation première
diff_ts = diff(ts_data, 1)

# Test de stationnarité

# ADF test
test_adf = urca::ur.df(diff_ts)
summary(test_adf)
# Résultat : "p-value: < 2.2e-16"
# On rejette l'hypothése nulle (la série n'est pas stationnaire)

# PP test
test_pp = tseries::pp.test(diff_ts) # test utilisé dans corrigé
test_pp
# Résultat : "p-value = 0.01"
# On rejette l'hypothése nulle (la série n'est pas stationnaire)

# KPSS
test_kpss = urca::ur.kpss(diff_ts)
summary(test_kpss)
# Résultat : "test-statistic is 0.207" ce qui est bien inférieur aux valeurs critiques usuelles
# On ne rejette pas l'hypothése nulle (la série est stationnaire)

# La série différenciée est donc stationnaire.


## Quesion 3 ####

#Représentation de la série avant :

#Graphique de la série en niveau
graph_niveau = ggplot(data = data, aes(x = Periode, y = Valeur))+
  geom_line()+
  scale_x_date(expand = c(0.01, 0.01)) +
  ggtitle("Evolution de l'indice de la production mensuelle de biens manufacturés 
          \nen France entre janvier 1985 et janvier 2000")+
  labs(caption = "Indice en base 100 en 1990")+
  ggthemes::theme_stata()+
  theme(
    plot.title   = element_text(lineheight = 0.8, face = "bold", hjust = 0.5, size = 15),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    axis.text.y  = element_text(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

# Sauvegarde du graphique
saveRDS(graph_niveau, file.path(lien_graph,"courbe.rds"))

# Réprésentation de la série après :

# Passage en object data.frame
df_serie_choisi = data.frame(Periode = time(diff_ts),
                             Valeur = as_tibble(diff_ts)$x)

# Construction du graphique
graph_diff = ggplot(df_serie_choisi, aes(x = Periode, y = Valeur))+
  geom_line()+
  ggtitle("Représentation de la série temporelle choisie") +
  ggthemes::theme_stata()+
  theme(
    plot.title   = element_text(lineheight = 0.8, face = "bold", hjust = 0.5, size = 15),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    axis.text.y  = element_text(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

# Sauvegarde du graphique
saveRDS(graph_diff, file.path(lien_graph, "ts_choisie.rds"))

graph_niveau /
  graph_diff

# Partie 2 ####

# Question 4 : Estimation un ARMA(p,q)
ggAcf(diff_ts)
# valeur max p est de 1 (pour le MA)

ggPacf(diff_ts)
# valeur max q est de 3 (pour le AR) en regardant assez largement

# Testons cette hypoth�ses
model_maxi <- arima(diff_ts, order = c(1,0,3))
residus_maxi <- residuals(model_maxi)
# Parait bon :
ggAcf(residus_maxi) + ggPacf(residus_maxi)
# On observe un pique important pour un lag 12

portes::LjungBox(model_maxi, order = 6)
# Le mod�le maximal parait passer le test d'autocorr�lation des r�sidus.

#Test des valeurs possibles de p et de q : test de tous les mod�les

lmtest::coeftest(model_maxi) # Coefficient ar1 non significatif --> pas forc?ment utilisable

evaluation_model <- function(order, x = diff_ts, lags = 24,...){
  model <- forecast::Arima(x, order = order,...)
  residus <- residuals(model)
  # Test d'autocorr�lation des r�sidus
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
  # on ajoute un tryCatch pour �viter les erreurs
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
# R�sidus non corr�l�s mais coefficient pas significatif

models_evalues$`ARIMA(1,0,0)`
# R�sidus non corr�l�s globalement et coefficients significatif mod�le retenu

models_evalues$`ARIMA(2,0,0)`
# R�sidus non corr�l�s globalement mais coefficient ar2 pas significatif

models_evalues$`ARIMA(0,0,1)`
# R�sidus non corr�l�s globalement et coefficients significatifs

models_evalues$`ARIMA(1,0,1)`
# R�sidus non corr�l�s globalement mais coefficient ma1 pas significatif

models_evalues$`ARIMA(2,0,1)`
# R�sidus non corr�l�s globalement et coefficients significatifs � 5% 

models_evalues$`ARIMA(0,0,2)`
# R�sidus corr�l�s mais coefficients significatifs

models_evalues$`ARIMA(1,0,2)`
# R�sidus non corr�l�s globalement mais coefficients pas significatifs

models_evalues$`ARIMA(2,0,2)`
# Tr�s peu d'autocorr�lation mais coefficients ar1 et ma1 non significatifs

models_evalues$`ARIMA(0,0,3)`
# Tr�s peu d'autocorr�lation mais coefficient ma3 non significatif

models_evalues$`ARIMA(1,0,3)`
# Coefficients pas significatifs

models_evalues$`ARIMA(2,0,3)`
# Tr�s peu d'autocorr�lation mais coefficient ar1 non significatif
 
nom_modeles_retenus = c('ARIMA(1,0,0)', 'ARIMA(0,0,1)', 'ARIMA(2,0,1)')

modeles_retenus <- models_evalues[nom_modeles_retenus]

qualite_modeles_retenus <- sapply(modeles_retenus, function(x) x$qualite)

round(qualite_modeles_retenus,4)

apply(qualite_modeles_retenus,1,function(x) colnames(qualite_modeles_retenus)[which.min(x)])
# Sur les crit�re d'informations un AR(1) semble le meilleur choix : ce r�sultat 
# correspond aussi � l'hypoth�se de d�part �tant l'ACF.

# Question 5

# Finalement on opte pour un ARIMA(1,1,0)
model <- Arima(ts_data, order = c(1,1,0))
model
checkresiduals(model)

# Partie 3 : Pr�diction####

# Question 6 :..
lapply(modeles_retenus,function(x) forecast(x$model, h = 4))

# Question 7 : On suppose que les r�sidus suivent une loi normal centr�e-r�duite.

# Question 8:
autoplot(forecast(model, 2))

predict(model, 2)
# Ce genre de mod�le est difficile � pr�dire
