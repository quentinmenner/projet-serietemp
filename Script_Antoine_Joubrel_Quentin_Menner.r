###### Projet de série temporelle ######

# Encodage : UTF-8

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
# Pas de saisonnalité apparente

# Y-a-t-il une tendance ?
data_tendance <- data %>% 
  mutate(index = index(data))

# Régression linéaire sur le temps pour observer s'il y a une tendance
lm(data_tendance$Valeur ~ data_tendance$index)

# Resultat Une hausse moyenne de 8.7% par ans. Il y a donc une tendance nette à la hausse.
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
test_pp = urca::ur.pp(diff_ts, type = "Z-tau") 
summary(test_pp)
# Résultat : "p-value = 0.01"
# On rejette l'hypothése nulle (la série n'est pas stationnaire)

# KPSS
test_kpss = kpss.test(diff_ts, null = "Level")
test_kpss

# Résultat : "test-statistic is 0.207" ce qui est bien inférieur aux valeurs critiques usuelles
# On ne rejette pas l'hypothése nulle (la série est stationnaire)

# La série différenciée est donc stationnaire.

# Question 3 ####

# Représentation de la série avant :

# Graphique de la série en niveau

graph_niveau = ggplot(data = data, aes(x = Periode, y = Valeur))+
  geom_line()+
  scale_x_date(expand = c(0.01, 0.01)) +
  ggtitle("Evolution de l'indice de la production mensuelle de biens manufacturés 
          \nen France entre janvier 1985 et janvier 2000")+
  labs(caption = "Indice en base 100 en 1990")+
  ggthemes::theme_stata()+
  theme(
    plot.title   = element_text(lineheight = 0.8, face = "bold", hjust = 0.5, size = 10),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    axis.text.y  = element_text(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

graph_niveau

# Sauvegarde du graphique
saveRDS(graph_niveau, file.path(lien_graph,"courbe.rds"))

# Réprésentation de la série après :

# Construction du graphique
graph_diff = ggplot(diff_ts, as.numeric = FALSE)+
  geom_line()+
  ggtitle("Représentation de la série temporelle choisie (stationnaire)") +
  ggthemes::theme_stata()+
  theme(
    plot.title   = element_text(lineheight = 0.8, face = "bold", hjust = 0.5, size = 10),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    axis.text.y  = element_text(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())
graph_diff

# Sauvegarde du graphique
saveRDS(graph_diff, file.path(lien_graph, "ts_choisie.rds"))

graph_niveau /
  graph_diff

# Partie 2 ####

# Question 4 : Choix ARMA(p,q) ####

# ACF de la série non stationnarisé 
acf_non_stationnaire = ggAcf(ts_data)+
  ggthemes::theme_stata()+
  labs(title = "ACF de la série non stationnarisée")+
  theme(
    plot.title   = element_text(lineheight = 0.8, face = "bold", hjust = 0.5, size = 15),
    axis.text.x  = element_text(),
    axis.text.y  = element_text(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())
acf_non_stationnaire

saveRDS(acf_non_stationnaire, file.path(lien_graph,"ACF_1.rds"))

# Identification des valeurs maximales
acf = ggAcf(diff_ts)+
  ggthemes::theme_stata()+
  labs(title = "ACF de la série stationnarisée")+
  theme(
    plot.title   = element_text(lineheight = 0.8, face = "bold", hjust = 0.5, size = 15),
    axis.text.x  = element_text(),
    axis.text.y  = element_text(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

# La valeur maximale de p est 1 (pour le MA)

pacf = ggPacf(diff_ts)+
  ggthemes::theme_stata()+
  labs(title = "PACF de la série stationnarisée")+
  theme(
    plot.title   = element_text(lineheight = 0.8, face = "bold", hjust = 0.5, size = 15),
    axis.text.x  = element_text(),
    axis.text.y  = element_text(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())
# La valeur maximale de q est 1 (pour le AR)

# Sauvegarde 
saveRDS(acf, file.path(lien_graph,"ACF_ts_choix.rds"))
saveRDS(pacf, file.path(lien_graph,"PACF_ts_choix.rds"))

acf /
  pacf


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
# Résidus non corrélés mais coefficient pas significatif : modèle non retenu

#ARIMA(1,0,0)
models_evalues[2]
# Résidus non corrélés globalement et coefficients significatif : modèle retenu

#ARIMA(0,0,1)
models_evalues[3]
# Résidus non corrélés globalement et coefficients significatifs : modèle retenu

#ARIMA(1,0,1)
models_evalues[4]
# Coefficients pas tous significatif : modèle non retenu

# Les modèles retenues selon les critères d'autocorrélation des résidues et 
# de significativité des coeffcicients

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
model <- Arima(diff_ts, order = c(1,0,0))
model
saveRDS(model, "model_est.rds")

plot(checkresiduals(model))

# Les résidues sont gaussiens.

# Partie 3 : Prédiction####

# Question 8:

# Tableau des valeurs de la région de confiance de niveau 95%
x_serie <- seq(110.4564,114.3764,0.01)
funct_1 <- function(x) 45.5324 - 0.612*x
funct_2 <- function(x) 41.6124 - 0.612*x
colonne_2 <- funct_1(x_serie)
colonne_3 <- funct_2(x_serie)

df <- data.frame(x_serie, colonne_2, colonne_3)

# Graphique de la région de confiance

gg_RC = ggplot(data = df,aes(x=x_serie))+
  labs(title = "Région de confiance (X_(t+1), X_(t+2))")+
  geom_ribbon(aes(x=x_serie, ymax=colonne_2, ymin=colonne_3), fill="pink", alpha=.5) +
  geom_line(aes(y = colonne_2, colour = "Max.")) +
  geom_line(aes(y = colonne_3, colour = "Min.")) +
  scale_colour_manual("", 
                      breaks = c("Max.", "Min."),
                      values = c("Max."="red", "Min."="blue"))+
  ggthemes::theme_stata()+
  theme(
    plot.title   = element_text(lineheight = 0.8, face = "bold", hjust = 0.5, size = 15),
    axis.text.x  = element_text(),
    axis.text.y  = element_text(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())
gg_RC

#Sauvegarde
saveRDS(gg_RC, file.path(lien_graph,"region_confiance.rds"))
