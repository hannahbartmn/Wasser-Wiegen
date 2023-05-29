# Grundlagen der Versuchsplanung 



#
# Laden der Pakete
library(ggplot2)
library(ggpubr)


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

# Messergebniss ohne Gewicht vom Glas
B <- Daten$Messergebnis[Daten$Glas == "b"] - 165
S <- Daten$Messergebnis[Daten$Glas == "s"] - 132

mean(B)
mean(S)


# Zusammenfassung deskrip. Stat.
des_stat_B <- summary(B)
des_stat_S <- summary(S)


# ------------------------------------------------------------------------------
# Betrachtung Normalverteilung
#
# Dataframe erstellen
data <- data.frame(Group = c(rep("B", length(B)), rep("S", length(S))), Value = c(B, S))

# qq-Plot für einzelne Messung
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
  ggtitle("Q-Q-Plot der Daten")
theme_bw()

print(qq_plot)

# ------------------------------------------------------------------------------
# t-Test
t.test(B,S)


# ------------------------------------------------------------------------------
# Grafische Darstellung
#

# Violinplot erstellen
violin_plot <- ggplot(data, aes(x = Group, y = Value, fill = Group)) +
  geom_violin(scale = "width", trim = TRUE) +
  geom_boxplot(width = 0.1, fill = "white", color = "black") +
  #stat_qq(aes(sample = Value), color = "red") +
  facet_wrap(~ Group, scales = "fixed") +
  ggtitle("Violinplot der Daten") +
  theme_bw()

print(violin_plot)
