# Packages

library(tidyverse)
library(zoo)
library(tseries)
library(lmtest)
library(forecast)
library(ggplot2)
library(patchwork)
library(urca)

data = read.csv("C:/Users/antoi/OneDrive - GENES/Documents/Donnees_TD_series_temp/data_tp5.csv", sep = ";")

data$dates[1] 
spread <- ts(data$spread, start = c(1986, 3), frequency = 12)
dspread <- diff(spread,1) # difference premiere


##########################
####### Question 2 #######
##########################
plot(cbind(spread,dspread))
# La s�rie en niveau semble avoir une tendance d�terministe (ou deux tendances)
# La s�rie diff�renci�e pourrait �tre stationnaire

##########################
####### Question 3 #######
##########################

# rmq : tester la pr�sence d'une tendance par r�gression n'a pas de sens
# car si on a une racine unitaire on est dans le cas d'une spurious r�gression
y = cumsum(rnorm(n=100))
summary(lm(y ~ seq_along(y)))

library(urca)
library(fUnitRoots)
# Ici on teste la pr�sence de racine unitaire
# adf invalid si on ne rajoute pas de variable explicative
# dans le doute c'est toujours mieux de rajouter la tendance et constante
adf <- adfTest(spread, type = "ct",lags = 0)

Qtests <- function(series, k = 24, fitdf=0) {
  t(sapply(1:k,function(l){
    if(l <= fitdf){
      b <- list(statistic = NA, p.value = NA)
    }else{
      b <- Box.test(series,"Ljung-Box",lag = l,
                    fitdf = fitdf
      )
    }
    data.frame(lag = l,
               b$statistic,
               b$p.value
    )
  }))
}
adfTest_valid <- function(series,kmax,type){
  k <- 0
  # On test ADF jusqu'� ce que les r�sidus ne soient pas autocorr�l�s
  noautocorr <- 0
  while (noautocorr==0){
    cat(paste0("ADF with ",k, " lags: residuals OK? "))
    adf <- adfTest(series,lags=k,type=type)
    pvals <- Qtests(adf@test$lm$residuals,
                    24,
                    fitdf=length(adf@test$lm$coefficients))[,3]
    if (sum(pvals<0.05,na.rm=T) == 0) {
      noautocorr <- 1; cat("OK \n")
    }else {
      cat("nope \n")
    }
    k <- k + 1
  }
  return(adf)
}

# On ne fait que le test � l'ordre 24
adfTest_valid2 <- function(series,kmax,type){
  k <- 0
  # On test ADF jusqu'� ce que les r�sidus ne soient pas autocorr�l�s
  noautocorr <- 0
  while (noautocorr==0){
    cat(paste0("ADF with ",k, " lags: residuals OK? "))
    adf <- adfTest(series,lags=k,type=type)
    pvals <- Qtests(adf@test$lm$residuals,
                    24,
                    fitdf=length(adf@test$lm$coefficients))[,3]
    if (sum(pvals[24]<0.05,na.rm=T) == 0) {
      noautocorr <- 1; cat("OK \n")
    }else {
      cat("nope \n")
    }
    k <- k + 1
  }
  return(adf)
}

df1 <- adfTest_valid(spread,24,"ct")
df2 <- adfTest_valid2(spread,24,"ct")

# On teste ici le mod�le
# ???y_t = a + bt + ?? y_t-1 +e_t
# tau3 correspond au test ?? = 0
# phi2 correspond au test a = b = ?? = 0
# phi3 correspond au test b = ?? = 0
# voir https://new.mmf.lnu.edu.ua/wp-content/uploads/2018/03/enders_applied_econometric_time_series.pdf
summary(
  urca::ur.df(spread, type  = "trend",lags = 12,
              selectlags = "AIC")
) # 5 lags retenus par AIC

# Dans tous cas on ne rejette pas H0 (mais a priori pas de tendance lin�aire)

# PP n'a pas besoin de beaucoup d'hypoth�ses parce que la stat calcul�e
# sur beta est construite de fa�on non param�trique et qui corrige
# toute forme d'autocorr�lation. Mais pas tr�s bon quand peu de donn�es
tseries::pp.test(spread) # on rejette pas hypoth�se de pr�sence de racine unitaire
tseries::kpss.test(spread, null = "Trend") # on rejette hypoth�se de stationnarit�

autoplot(dspread) / 
  (ggAcf(dspread) + labs(title = "ACF")  +
     ggPacf(dspread) + labs(title = "PACF") )
tseries::pp.test(dspread) # on rejette pas hypoth�se de pr�sence de racine unitaire
tseries::kpss.test(dspread, null = "Level")
df1 <- adfTest_valid(dspread,24,"c")
df2 <- adfTest_valid2(dspread,24,"c")
summary(
  urca::ur.df(dspread, type  = "none",lags = 12,
              selectlags = "AIC")
)

acf(dspread)# possibilit� de ma = 3
pacf(dspread)# possibilit� de AR = 3

model_maxi <- arima(dspread, order = c(3,0,3))
residus_maxi <- residuals(model_maxi)

ggAcf(residus_maxi) + ggPacf(residus_maxi)# Fonctionne

# Pour l'application du test de Ljung-Box sur les r�sidus du mod�le ARIMA
# il est sugg�r� de consid�r� p+q degr�s de libert�s
lbtest <- t(sapply(1:24,function(l){
  if(l <=  length(coef(model_maxi))){
    b <- list(statistic = NA, p.value = NA)
  }else{
    b <- Box.test(residus_maxi,"Ljung-Box",lag = l,
                  # il faut ajuster du degr� de libert�, i.e. : nombre de coefficients estim�s
                  fitdf = length(coef(model_maxi))
    )
  }
  
  data.frame(lag = l,
             b$statistic,
             b$p.value
  )
}))
lbtest # Il n'y a pas autocorr�lation des r�sidus
# Remarque:portes n'utilise pas le bon nombre de degr� de libert� (devrait �tre �gal � lags-6)
# Le probl�me est qu'il n'utilise pas la constante
portes::LjungBox(model_maxi)
# Autre exemple, si l'on rajoute une variable suppl�mentaire (par exemple une indicatrice choisit au hasard)
# alors c'est encore le m�me nombre de degr� de libert�s :
# faire attention en utilisant ce package
portes::LjungBox(arima(dspread, order = c(3,0,3))

# Utiliser plut�t
portes::LjungBox(residuals(model_maxi),order = 6)

#Evalution des mod�les possibles
models_possibles <- expand.grid(p = c(0,1,2,3), d = 0, q = c(0, 1, 2, 3))

lmtest::coeftest(model_maxi) # aucun coefficient n'est significatif � part la constante
evaluation_model <- function(order, x = dspread, lags = 24,...){
  # ici on utilise Arima plut�t que arma pour la fonction accuracy
  model <- forecast::Arima(x, order = order,...)
  residus <- residuals(model)
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
  #ttest <- tryCatch(lmtest::coeftest(model), error = \(e) NULL)
  qualite <- c(AIC(model), BIC(model), accuracy(model))
  names(qualite) <- c("AIC", "BIC", colnames(accuracy(model)))
  list(model = model,
       lbtest = lbtest,
       qualite = qualite)
  
}

models_evalues <- apply(models_possibles,1, evaluation_model)

names(models_evalues) <- sprintf("ARIMA(%i,%i,%i)", models_possibles[,"p"],
                                 models_possibles[,"d"], models_possibles[,"q"])

cat(paste(sprintf("models_evalues$`%s`",names(models_evalues)),collapse = "\n"))
models_evalues$`ARIMA(0,0,0)`
models_evalues$`ARIMA(1,0,0)`
models_evalues$`ARIMA(2,0,0)`
models_evalues$`ARIMA(3,0,0)`
models_evalues$`ARIMA(0,0,1)`
models_evalues$`ARIMA(1,0,1)`
models_evalues$`ARIMA(2,0,1)`
models_evalues$`ARIMA(3,0,1)`
models_evalues$`ARIMA(0,0,2)`
models_evalues$`ARIMA(1,0,2)`
models_evalues$`ARIMA(2,0,2)`
models_evalues$`ARIMA(3,0,2)`
models_evalues$`ARIMA(0,0,3)`
models_evalues$`ARIMA(1,0,3)`
models_evalues$`ARIMA(2,0,3)`
models_evalues$`ARIMA(3,0,3)`


auto.arima(spread, max.D = 0, max.P = 0, max.Q = 0)
