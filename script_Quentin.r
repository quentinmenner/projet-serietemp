###### Projet de sï¿½rie temporelle ######

# Testlol
# Packages utilisï¿½s ####

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
library(tsbox)

# Lieu exportation graphiques
setwd(dir = "~/projet-serietemp")
lien_graph = "Graphiques"

# Importation et structuration des donnï¿½es ####
datafile <- "valeurs_mensuelles_2.csv"
data <- read.csv(datafile, sep = ";") %>% 
  arrange(Periode) %>% 
  mutate(Periode = ym(Periode))
ts_data <- ts(data$Valeur, start = c(1985,01), freq = 12)
gg = ggplot(data = data, aes(x = Periode, y = Valeur))+
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

saveRDS(gg, file.path(lien_graph,"courbe.rds"))

# On observe une tendance ï¿½ la hausse
# Donnï¿½es : 1985 - 2000

# Partie 1 ####

gg2 = ggAcf(ts_data)+
  labs(title = "ACF")+
  ggthemes::theme_stata()+
  theme(
    plot.title   = element_text(lineheight = 0.8, face = "bold", hjust = 0.5, size = 15),
    axis.text.x  = element_text(hjust = 1),
    axis.text.y  = element_text(),
    axis.title.x = element_text(),
    axis.title.y = element_blank())
 
saveRDS(gg2, file.path(lien_graph,"ACF_1.rds"))

# Il semble que la sï¿½rie reprï¿½sente un AR pure

## Question 1
# Saisonnalitï¿½ ? 
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
#On observe une trï¿½s lï¿½gï¿½re saisonnalitï¿½

ts_data2 = diff(ts_data, 12)


# Tendance ?
data_tendance <- data %>% 
  mutate(index = index(data))
library("car")

# Régression linéaire sur le temps
scatterplot(Valeur ~ index, data = data_tendance, 
            smoother = FALSE, grid = FALSE, frame = FALSE)

# Une tendance ï¿½ la hausse semble se dï¿½gager
diff_ts = diff(ts_data, 1)

# ADF test
test_adf = urca::ur.df(diff_ts)
summary(test_adf)
# On rejette l'hypothï¿½se nulle

# PP test
test_pp = tseries::pp.test(diff_ts) # test utilisï¿½ dans corrigï¿½
test_pp
# On rejette l'hypothï¿½se nulle

# KPSS
test_kpss = urca::ur.kpss(diff_ts)
summary(test_kpss)
# On ne rejette pas l'hypothï¿½se nulle 

# La sï¿½rie diffï¿½renciï¿½e est donc stationnaire.

# Reprï¿½sentation de la sï¿½rie avant / aprï¿½s 

df_serie_choisi = data.frame(Periode = time(diff_ts),
                             Valeur = as_tibble(diff_ts)$x)
df_serie_choisi$Periode


graph_serie_choisi = ggplot(df_serie_choisi, aes(x = Periode, y = Valeur))+
  geom_line()+
  ggtitle("Série temporelle choisie (différenciation saisonnière et
          \n différenciation première)") +
  ggthemes::theme_stata()+
  theme(
    plot.title   = element_text(lineheight = 0.8, face = "bold", hjust = 0.5, size = 15),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    axis.text.y  = element_text(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

saveRDS(graph_serie_choisi, file.path(lien_graph, "ts_choisie.rds"))

# Partie 2 ####

# Question 4 : Estimation un ARMA(p,q)
ggAcf(diff_ts)
# valeur max p est de 1 (pour le MA)

ggPacf(diff_ts)
# valeur max q est de 3 (pour le AR) en regardant assez largement

# Testons cette hypothï¿½ses
model_maxi <- arima(diff_ts, order = c(1,0,3))
residus_maxi <- residuals(model_maxi)
# Parait bon :
ggAcf(residus_maxi) + ggPacf(residus_maxi)
# On observe un pique important pour un lag 12

portes::LjungBox(model_maxi, order = 6)
# Le modï¿½le maximal parait passer le test d'autocorrï¿½lation des rï¿½sidus.

#Test des valeurs possibles de p et de q : test de tous les modï¿½les

lmtest::coeftest(model_maxi) # Coefficient ar1 non significatif --> pas forcèment utilisable

evaluation_model <- function(order, x = diff_ts, lags = 24,...){
  model <- forecast::Arima(x, order = order,...)
  residus <- residuals(model)
  # Test d'autocorrï¿½lation des rï¿½sidus
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
  # on ajoute un tryCatch pour ï¿½viter les erreurs
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
# Rï¿½sidus non corrï¿½lï¿½s mais coefficient pas significatif

models_evalues$`ARIMA(1,0,0)`
# Rï¿½sidus non corrï¿½lï¿½s globalement et coefficients significatif modï¿½le retenu

models_evalues$`ARIMA(2,0,0)`
# Rï¿½sidus non corrï¿½lï¿½s globalement mais coefficient ar2 pas significatif

models_evalues$`ARIMA(0,0,1)`
# Rï¿½sidus non corrï¿½lï¿½s globalement et coefficients significatifs

models_evalues$`ARIMA(1,0,1)`
# Rï¿½sidus non corrï¿½lï¿½s globalement mais coefficient ma1 pas significatif

models_evalues$`ARIMA(2,0,1)`
# Rï¿½sidus non corrï¿½lï¿½s globalement et coefficients significatifs ï¿½ 5% 

models_evalues$`ARIMA(0,0,2)`
# Rï¿½sidus corrï¿½lï¿½s mais coefficients significatifs

models_evalues$`ARIMA(1,0,2)`
# Rï¿½sidus non corrï¿½lï¿½s globalement mais coefficients pas significatifs

models_evalues$`ARIMA(2,0,2)`
# Trï¿½s peu d'autocorrï¿½lation mais coefficients ar1 et ma1 non significatifs

models_evalues$`ARIMA(0,0,3)`
# Trï¿½s peu d'autocorrï¿½lation mais coefficient ma3 non significatif

models_evalues$`ARIMA(1,0,3)`
# Coefficients pas significatifs

models_evalues$`ARIMA(2,0,3)`
# Trï¿½s peu d'autocorrï¿½lation mais coefficient ar1 non significatif
 
nom_modeles_retenus = c('ARIMA(1,0,0)', 'ARIMA(0,0,1)', 'ARIMA(2,0,1)')

modeles_retenus <- models_evalues[nom_modeles_retenus]

qualite_modeles_retenus <- sapply(modeles_retenus, function(x) x$qualite)

round(qualite_modeles_retenus,4)

apply(qualite_modeles_retenus,1,function(x) colnames(qualite_modeles_retenus)[which.min(x)])
# Sur les critï¿½re d'informations un AR(1) semble le meilleur choix : ce rï¿½sultat 
# correspond aussi ï¿½ l'hypothï¿½se de dï¿½part ï¿½tant l'ACF.

# Question 5

# Finalement on opte pour un ARIMA(1,1,0)
model <- Arima(ts_data, order = c(1,1,0))
model
checkresiduals(model)

# Partie 3 : Prï¿½diction####

# Question 6 :..
lapply(modeles_retenus,function(x) forecast(x$model, h = 4))

# Question 7 : On suppose que les rï¿½sidus suivent une loi normal centrï¿½e-rï¿½duite.

# Question 8:
autoplot(forecast(model, 2))

predict(model, 2)
# Ce genre de modï¿½le est difficile ï¿½ prï¿½dire
