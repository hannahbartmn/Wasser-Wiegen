###### Kapitel 5 Versuchsplanung

library(readxl)
data <- read_excel("C:/Users/bartm/Documents/Uni/Sommersemester 2023/Grundlagen der Versuchsplanung/Umfrageergebnisse_Gruppe_A.xlsx")
data <- data[-c(4,15,10,22,16,20),] #entfernen der Versuchsleitenden
data$Alter <- as.numeric(data$Alter)
median(data$Alter) #20.5
data$Alter <- cut(data$Alter, breaks = c(18,20.5,53), levels = c("[19,20]", "[21,53]"))
data <- data[order(data$Alter), ]

#Erstellen des randomisierten Versuchsplan
library(agricolae) 
trt <- c("Schmales Glas", "Breites Glas")
design.crd(trt = trt, c(4,4), seed = 8)$book
design.crd(trt = trt, c(4,4), seed = 9)$book

#Berechnung des theoretischen Stichprobenumfangs
stichprobenumfang <- function(n, alpha = 0.05, delta = 1.5){
  return(1 - pt(-qt(1 - alpha, df = 2*n-2), df = 2*n-2, ncp = -sqrt(n/2)*delta))
}

plot(2:20, stichprobenumfang(2:20), xlab = "Stichprobenumfang n", 
     ylab = "Fehlerwahrscheinlichkeit 2. Art", type = "b")
abline(h = 0.05)

stichprobenumfang(10) <= 0.05
stichprobenumfang(11) <= 0.05