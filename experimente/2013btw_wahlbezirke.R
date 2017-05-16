#######################
# PAKETE INSTALLIEREN #
#######################
#install.packages('needs')
#library(needs)
needs(reshape, dplyr)

###########################
# WAHLERGEBNISSE EINLESEN #
###########################

# Pfad setzen
setwd("Projekte/machine-learning/experimente/")

# Zweitstimmen Wahlbezirke Bundestagswahl 2013
btw13 <- read.csv('BTW13_Zweitstimmen_Wahlbezirke.txt', sep=';', encoding='UTF-8', colClasses='character')

# AGS (Amtlicher Gemeindeschlüssel), RS (Regionalschlüssel), KRS (Kreisschlüssel) der Gemeinden generieren
btw13$id <- paste0(btw13$Land, btw13$RB, btw13$Kreis, btw13$Gemeinde, btw13$Wbz)

# Stimmen in Zahlen umwandeln
btw13[10:47] <- sapply(btw13[10:47], as.numeric) 

# Prozentwerte der Parteistimmen ausrechnen, runden und relativ speichern
btw13[18:47] <- round(btw13[18:47] / btw13$Gültige * 100, digits = 1)

# AGS an den Anfang der Tabelle
btw13 <- btw13 %>% select(id, everything())

# Wahlbeteiligung sowie Rechtsextreme und Rechtspopulisten berechnen
btw13$Wahlbeteiligung <- round(btw13$B / btw13$A * 100, digits = 1)

# Unnötige Spalten löschen und umbenennen
btw13 <- subset(btw13, select = -c(RB, VG, A, B, A1, A2, A3, B1, BW.Bez, Gültige, Ungültige))
names(btw13)[names(btw13) == 'A'] <- 'Wahlberechtigte'
names(btw13)[names(btw13) == 'B'] <- 'Wähler'


### MDS
#d <- btw13[, !(colnames(btw13) %in% c("KRS","Art","ost_west"))]
d <- na.omit(btw13)

d_dist <- dist(d) # euclidean distances between the rows
fit <- cmdscale(d_dist,eig=TRUE, k=2) # k is the number of dim
fit # view results

# plot solution 
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
     main="Metric	MDS")
text(x, y, labels = d$Name, cex=.7)

# # NA und NaN-Werte rausfiltern
# btw13 <- na.omit(btw13)
# 
# # Shape laden und mit Daten mergen
# shp13 <- readOGR("2013_Gemeinden.shp", "2013_Gemeinden", stringsAsFactors=FALSE, encoding="UTF-8")
# #plot(shp13, axes=TRUE, border="gray")
# shp13 <- shp13[, "RS"] 
# shp13 <- merge(shp13, btw13, by="RS") 
# 
# # GeoJSON mit Daten schreiben
# writeOGR(shp13, dsn='2013btw', layer='2013btw', driver='GeoJSON', encoding="UTF-8", overwrite_layer = TRUE)
# 
# # TO-DO: ENDUNG .geojson ANHÄNGEN
# 
# # Daten in CSV und Excel-Tabelle schreiben
# write.csv(btw13, "2013btw.csv")
# WriteXLS(btw13, "2013btw.xls", Encoding="UTF-8")
