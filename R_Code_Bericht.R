#####
#Zweiter Versuch

fliegen <- data.frame(matrix(nrow = 16, ncol = 5))
colnames(fliegen) <- c("weite", "papier", "untergrund", "fingerdruck", "groesse")
fliegen$papier <- as.factor(rep(rep(c(0,1), each = 4), 2))
fliegen$untergrund <- as.factor(rep(c(0,1,1,0,1,0,0,1), 2))
fliegen$fingerdruck <- as.factor(rep(c(0,1), 8))
fliegen$groesse <- as.factor(rep(rep(c(0,1), each = 2), 4))

#Papier
#0 duennes Papier, 1 dickes Papier
#Untergrund
#0 ohne, 1 mit
#fingerdruck
#0 unten, #1 oben 
#groesse 
#0 klein, 1 gross

#Einheit in mm 
fliegen$weite <- c(33, 375, 0, 240, 106, 500, 127, 648, 14, 390, 0, 160, 54, 1179, 179, 514)

#hinterster Punkt wird gemessen 
#3 Versuche, davon das maximum fuer eine beobachtung


# Screening
limo <- lm(weite ~ papier + untergrund + fingerdruck + groesse, data = fliegen)
summary(limo)

fliegen2 <- fliegen[fliegen$fingerdruck == 1,]
limo2 <- lm(weite[fingerdruck == 1] ~ papier + untergrund + groesse, data = fliegen2)
summary(limo2)












################################################################################
###### Kapitel 5 Versuchsplanung

# Laden der Pakete
library(ggplot2)
library(ggpubr)
library(car)
library(readxl)

################################################################################
data <- read_excel("C:/Users/bartm/Documents/Uni/Sommersemester 2023/Grundlagen der Versuchsplanung/Umfrageergebnisse_Gruppe_A.xlsx")
data <- data[-c(4,15,10,22,16,20),] #entfernen der Versuchsleitenden
data$Alter <- as.numeric(data$Alter)
median(data$Alter) #20.5
data$Alter <- cut(data$Alter, breaks = c(18,20.5,53), levels = c("[19,20]", "[21,53]"))
data <- data[order(data$Alter), ]

# Erstellen des randomisierten Versuchsplan ------------------------------------
library(agricolae) 
trt <- c("Schmales Glas", "Breites Glas")
design.crd(trt = trt, c(4,4), seed = 8)$book
design.crd(trt = trt, c(4,4), seed = 9)$book

# Berechnung des theoretischen Stichprobenumfangs ------------------------------
stichprobenumfang <- function(n, alpha = 0.05, delta = 1.5){
  return(1 - pt(-qt(1 - alpha, df = 2*n-2), df = 2*n-2, ncp = -sqrt(n/2)*delta))
}

plot(2:20, stichprobenumfang(2:20), xlab = "Stichprobenumfang n", 
     ylab = "Fehlerwahrscheinlichkeit 2. Art", type = "b")
abline(h = 0.05)

stichprobenumfang(10) <= 0.05
stichprobenumfang(11) <= 0.05





################################################################################
# ------------------------------------------------------------------------------
# Auswertroutine 
# Erstellung Dataframe
Daten <- data.frame(Proband = c("Lukas Pape", "Serhat Aydin", "Maike Brochtrup", "Justin Pixner",
                                "Larissa Schoeneich", "Mia Macarena Bedarf", "Tobias Huebner", 
                                "Nicole Hofmann", "Simon Kutzner", "Jonas Molsbeck", 
                                "Julia Nadine Pohl", "Piet Wilhelm Cornils", "Dylan Wagner",
                                "Max Kuebler", "Nils Huelpuesch", "Ashtar Hashmin", 
                                "Mika Lowak", "Onur Guel"), 
                    Glas = c("s", "s", "s", "b", "b", "b", "b", "s", "s", "s", "b", 
                             "s", "b", "b", "s", "b", "b", "s"), 
                    Versuchseinheit = c(1,9,2,10,3,11,4,12,5,13,6,14,7,15,8,16,17,18),
                    Geschlecht = c("m", "m", "w", "m", "w", "w", "m", "w", "m", "m", "w", 
                                   "m", "m", "m", "m", "w", "m", "m"), 
                    Beginn = c("9:13", "9:19", "9:28", "9:32", "9:42", "9:46", "10:18", 
                               "10:54", "10:38", "10:26", "10:35", "10:23", "10:47", "10:04", "11:04", "11:07", 
                               "11:17", "11:00"), 
                    Ende = c("9:15", "9:21", "9:31", "9:34", "9:44", "9:48", "10:20", "NA", "10:40", 
                             "10:27", "10:37", "10:25", "10:48", "10:06", "11:06", "11:09", "11:20", 
                             "11:01"),
                    Vorwissen = c(0,1,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,2), 
                    Stress = c(2,1,2,3,2,2,1,3,2,1,4,2,1,1,1,3,2,1), 
                    Kurzsichtig = c(0,1,0,1,0,0,0,1,0,0,1,0,1,1,1,0,0,0), 
                    Weitsichtig = c(0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0), 
                    Hornhautverkruemmung = c(0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0), 
                    Messergebnis = c(193, 198, 184, 212, 191, 225, 225, 210, 172, 198, 
                                     223, 179, 191, 189, 179, 211, 212, 183))


# ------------------------------------------------------------------------------
# Erste Betrachtungen 
# Messergebnisse ohne Gewicht vom Glas
B <- Daten$Messergebnis[Daten$Glas == "b"] - 165
S <- Daten$Messergebnis[Daten$Glas == "s"] - 132


# Transdormation der Daten
B1 <- abs(B-50)
S1 <- abs(S-50)

# Zusammenfassung deskrip. Stat.
des_stat_B <- summary(B)
des_stat_S <- summary(S)
summary(B1)
summary(S1)

# Dataframe erstellen
data <- data.frame(Glas = c(rep("B", length(B)), rep("S", length(S))), Value = c(B, S))

# Dataframe fuer transformierte Daten
data2 <- data.frame(Glas = c(rep("B", length(B1)), rep("S", length(S1))), Value = c(B1, S1))


# ------------------------------------------------------------------------------
# Betrachtung Normalverteilung
#
# qq-Plot für einzelne Messung der nicht transdormierten Daten
#
# Layout fuer drei Unterplots
par(mfrow = c(1, 2))

qqnorm(B, main = "Q-Q Plot \nBreites Glas", xlab = "Theoretische Quantile", ylab = "Beobachtete Quantile")
qqline(B)

qqnorm(S, main = "Q-Q Plot \nSchmales Glas", xlab = "Theoretische Quantile", ylab = "Beobachtete Quantile")
qqline(S)

# Zusammengefasster qq-Plot
qq_plot <- ggplot(data, aes(sample = Value)) +
    geom_qq() +
    geom_qq_line(color = "red") +
    xlab("Theoretische Quantile") + 
    ylab("Beobachtete Quantile") +  
    ggtitle("Q-Q-Plot der Daten") +
    theme_bw()

print(qq_plot)


#
# qq-Plot für einzelne Messung der transformierten Daten
#
# Layout fuer drei Unterplots
par(mfrow = c(1, 2))

qqnorm(B1, main = "Q-Q Plot \nBreites Glas", xlab = "Theoretische Quantile", ylab = "Beobachtete Quantile")
qqline(B1)

qqnorm(S1, main = "Q-Q Plot \nSchmales Glas", xlab = "Theoretische Quantile", ylab = "Beobachtete Quantile")
qqline(S1)

# Zusammengefasster qq-Plot
qq_plot <- ggplot(data2, aes(sample = Value)) +
    geom_qq() +
    geom_qq_line(color = "red") +
    xlab("Theoretische Quantile") + 
    ylab("Beobachtete Quantile") +  
    ggtitle("Q-Q-Plot der Daten") +
    theme_bw()

print(qq_plot)


# ------------------------------------------------------------------------------
# Grafische Darstellung
#

# Violinplot erstellen der nichttransformierten Daten
violin_plot <- ggplot(data, aes(x = Glas, y = Value, fill = Glas)) +
    geom_violin(scale = "width", trim = TRUE) +
    geom_boxplot(width = 0.1, fill = "white", color = "black") +
    #stat_qq(aes(sample = Value), color = "red") +
    #facet_wrap(~Group, scales = "free") +
    ylim(20, 80) + 
    ylab("Menge in ml") +
    geom_hline(yintercept = 50, linetype = "dashed", color = "black") +
    ggtitle("Violinplot der Daten") +
    theme_bw()

print(violin_plot)


# Violinplot erstellen der transformierten Daten
violin_plot <- ggplot(data2, aes(x = Glas, y = Value, fill = Glas)) +
    geom_violin(scale = "width", trim = TRUE) +
    geom_boxplot(width = 0.1, fill = "white", color = "black") +
    #stat_qq(aes(sample = Value), color = "red") +
    #facet_wrap(~Group, scales = "free") +
    ylim(0, 30) + 
    ylab("Abweichung in ml") +
    geom_hline(yintercept = 50, linetype = "dashed", color = "black") +
    ggtitle("Violinplot der Daten") +
    theme_bw()

print(violin_plot)


# Betrachtung nur Boxplot
par(mfrow = c(1, 1))
boxplot(Value ~ Glas, data = data2)
boxplot(data2$Value)




# ------------------------------------------------------------------------------
# Teststatistik
#
# Vortest auf Normalverteilung (Kolmogorov-Smirnov-Test und Shapiro-Wilk-Test)

# schmales Glas
ks_S <- ks.test(S1,"pnorm",mean=mean(S1), sd=sd(S1)) #p-Wert 0.74 => keine Ablehnung der Normalverteilungsannahme 
sh_S <- shapiro.test(S1)  # p-Wert 0.1684 => keine Ablehnung der Normalverteilungsannahme                                       

#breites Glas
ks_B <- ks.test(B1,"pnorm",mean=mean(B1), sd=sd(B1)) #p-Wert 0.54 => keine Ablehnung der Normalverteilungsannahme 
sh_B <- shapiro.test(B1)  # p-Wert 0.0344 => Ablehnung der Normalverteilungsannahme        


#-------------------------------------------------------------------------------
# einseitiger Zwei-Stichproben T-test (f?r normalverteilte Stichprobe mit gleicher Varianz)

t.test(S1,B1,var.equal = TRUE, alternative = "less") ## p-Wert 0.2916 => kein signifikanter Unterschied

#-------------------------------------------------------------------------------
# Varianzgleichheit

leveneTest(data2$Value, data2$Glas) # p-Wert von 0.8701 => Varianzgleichheit wird nicht abgelehnt


#-------------------------------------------------------------------------------
#Problem der Normalverteilungsannahme im Intervall (0,50)

#Berechnen der vernachl?ssigten Fl?che

#schmales Glas

Fehler_S <- pnorm(0, mean=mean(S1), sd=sd(S1))+(1-(pnorm(50, mean=mean(S1), sd=sd(S1)))) ## 13% der Verteilung wird nicht ber?cksichtigt
Fehler_B <- pnorm(0, mean=mean(B1), sd=sd(B1))+(1-(pnorm(50, mean=mean(B1), sd=sd(B1)))) ## 9%  der Verteilung wird nicht ber?cksichtigt 










