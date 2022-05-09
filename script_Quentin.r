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
library(ggpmisc)


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
slot(test_adf, 'teststat')
slot(test_adf, "cval")
# Résultat : "p-value: < 2.2e-16"
# On rejette l'hypothése nulle (la série n'est pas stationnaire)

# PP test
test_pp = urca::ur.pp(diff_ts, type = "Z-tau") # test utilisé dans corrigé
slot(test_pp, 'teststat')
slot(test_pp, "cval")
test_pp
summary(test_pp)
# Résultat : "p-value = 0.01"
# On rejette l'hypothése nulle (la série n'est pas stationnaire)

# KPSS
test_kpss = kpss.test(diff_ts, null = "Level")
test_kpss
slot(test_kpss, 'teststat')
slot(test_kpss, "cval")
# Résultat : "test-statistic is 0.207" ce qui est bien inférieur aux valeurs critiques usuelles
# On ne rejette pas l'hypothése nulle (la série est stationnaire)

# Rassemblement des données dans un tableau



# La série différenciée est donc stationnaire.


# Question 3 ####

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

# Construction du graphique
graph_diff = ggplot(diff_ts, as.numeric = FALSE)+
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

# Question 4 : Choix ARMA(p,q) ####

# Identification des valeurs maximales
acf = ggAcf(diff_ts)+
  ggthemes::theme_stata()+
  labs(title = "ACF")+
  theme(
    plot.title   = element_text(lineheight = 0.8, face = "bold", hjust = 0.5, size = 15),
    axis.text.x  = element_text(),
    axis.text.y  = element_text(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

# valeur max p est de 1 (pour le MA)

pacf = ggPacf(diff_ts)+
  ggthemes::theme_stata()+
  labs(title = "PACF")+
  theme(
    plot.title   = element_text(lineheight = 0.8, face = "bold", hjust = 0.5, size = 15),
    axis.text.x  = element_text(),
    axis.text.y  = element_text(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

# Sauvegarde 
saveRDS(acf, file.path(lien_graph,"ACF_ts_choix.rds"))
saveRDS(pacf, file.path(lien_graph,"PACF_ts_choix.rds"))

acf /
  pacf
# valeur maximale de q est 1 (pour le AR) 

# Estimation des modèles : test de tous les modèles

evaluation_model <- function(order, x = diff_ts, lags = 24,...){
  # On estime les paramètres
  model = forecast::Arima(x, order = order,...)
  
  residus <- residuals(model)
  
  # Test d'autocorrélation des résidus avec lags retards testés
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
  
  # On regarde les informations de validité
  qualite <- c(AIC(model), BIC(model), accuracy(model))
  names(qualite) <- c("AIC", "BIC", colnames(accuracy(model)))
  list(model = model,
       ttest = ttest,
       lbtest = lbtest,
       qualite = qualite)
  
}


models_possibles = expand.grid(p = c(0, 1), d = 0, q = c(0, 1))

# On applique la fonction au 4 modèles possibles
models_evalues = apply(models_possibles,1, evaluation_model)

saveRDS(models_evalues,"eval_modele.rds")
#ARIMA(0,0,0)
models_evalues[1]
# R�sidus non corr�l�s mais coefficient pas significatif

#ARIMA(1,0,0)
models_evalues[2]
# R�sidus non corr�l�s globalement et coefficients significatif mod�le retenu

#ARIMA(0,0,1)
models_evalues[3]
# R�sidus non corr�l�s globalement et coefficients significatifs

#ARIMA(1,0,1)
models_evalues[4]

# R�sidus non corr�l�s globalement mais coefficient ma1 pas significatif

# Modèles retenues selon les critères d'autocorrélation des résidues et 
# significativité des coeffcicients
num_modeles_retenus = c(2,3)

modeles_retenus = models_evalues[num_modeles_retenus]

# Selection par la qualité (AIC, BIC)
qualite_modeles_retenus <- sapply(modeles_retenus, function(x) x$qualite)
round(qualite_modeles_retenus,4)
apply(qualite_modeles_retenus,1,function(x) colnames(qualite_modeles_retenus)[which.min(x)])
# Sur les critère d'informations un AR(1) semble le meilleur choix : ce résultat 
# correspond aussi à l'hypothèse de départ avec l'ACF.

# Question 5

# Finalement on opte pour un ARIMA(1,1,0)
model <- Arima(diff_ts, order = c(1,1,0))
saveRDS(model, "model_est.rds")

rest = checkresiduals(model)
rest
saveRDS(rest, "residues.rds")
# Les résidues sont bien gaussiens

# Partie 3 : Prédiction####

# Question 6 :..
lapply(modeles_retenus,function(x) forecast(x$model, h = 4))

# Question 7 : On suppose que les r�sidus suivent une loi normal centr�e-r�duite.

# Question 8:
autoplot(forecast(model, 2))

predict(model, 2)
# Ce genre de mod�le est difficile � pr�dire
