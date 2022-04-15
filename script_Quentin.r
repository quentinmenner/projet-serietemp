require(zoo)
require(tseries)

datafile <- "valeurs_mensuelles_2.csv"
data <- read.csv(datafile,sep=";")
data <- apply(data, 2, rev)
data <- as.data.frame(data)
data$Valeur <- as.numeric(data$Valeur)
print ("Modified Dataframe")
print (data)

'x <- data[,c("Periode")]
y <- data[,c("Valeur")]
x <- c(x)
plot(y)'
#new <- as.Date(x, format="%Y-%m")
#data$Periode <- as.Date(data$Periode, format="%Y-%m")
data$Periode <- as.yearmon(data$Periode, "%Y-%m")
#data$Periode <- as.Date(paste0(data$Periode, '01'), format="%Y-%m-%d")

plot(data$Periode,                                      # Draw plot without x-axis
     data$Valeur,
     type = "l",
     xaxt = "n")
#axis(1,                                                   # Add dates to x-axis
#     data$Periode,
#     format(data$Periode, "%Y-%m-%d"))
axis(1, at = data$Periode[seq(1, length(data$Periode), 12)])
#     labels = c("Some text", "Other text"))

acf(data$Valeur)