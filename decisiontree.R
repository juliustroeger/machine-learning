#######################
# PAKETE INSTALLIEREN #
#######################

#install.packages('needs')
library(needs)
needs(rpart, rpart.plot, rattle, caret, e1071)

################################################################################
# FEATURE ENGINEERING: DATEN EINLESEN, MERGEN, NORMALISIEREN UND FAKTORISIEREN #
################################################################################

setwd("/Users/juliustroeger/Projekte/") # Startverzeichnis einrichten

d_election <- read.csv('machine-learning/btw2013_landkreise.csv') # Zweitstimmen der Bundestagswahl 2013 laden
d_socio <- read.csv('machine-learning/soziodemographisch_landkreise.csv') # Soziodemographische Daten laden
d <- merge(d_election, d_socio, by = "KRS") # Daten anhand des Kreisschlüssels KRS mergen

# TO-DO: Ein Datensatz daraus machen oder Dopplungen wie Einwohner.x raus. Ist verwirrend

# Lineare Regression
plot(d$GRÜNE_rel ~ d$Mietpreis)
fit_lin <- lm(d$Grüne ~ d$Mietpreis)
abline(fit)
summary(fit)

# Multiple Regression
fit_lin_multi <- lm(d$GRÜNE_rel ~ d$Mietpreis + d$Medianeinkommen + d$Haus..und.Sperrmüll)
summary(fit_lin_multi)
# TO-DO: Kollinearität checken / wie damit umgehen

# Spalte mit Parteien einfügen, die Landkreise gewonnen haben
# which(colnames(d)=="Die.PARTEI_rel")
z <- apply(d[37:66],1,which.max) 
d$Gewinner <- names(d[37:66])[z]

# Parteistimmen faktorisieren (Ist Partei X stärkste Kraft im Briefwahlbezirk? Dummyvariable ja = 1, nein = 0)
for(i in 1:length(unique(d$Gewinner))){
newcol <- ncol(d)+1
  for(k in 1:nrow(d)){
    paste0("factor_", unique(d$Gewinner)[i])
      if(d$Gewinner[k] == unique(d$Gewinner)[i]){
      d[k,newcol] <- 1
      } else{d[k,newcol] <- 0} 
  }
  d[,newcol] <- as.factor(d[,newcol])
  colnames(d)[ncol(d)] <- paste0("factor_", unique(d$Gewinner)[i])
}

# TO-DO: Checken, wo Rechte/AfD 5-Prozent-Hürde geschafft haben

table(d$Gewinner) # Check: In so vielen Briefwahlbezirken konnten Parteien gewinnen
prop.table(table(d$Gewinner))*100 # Ausgabe in Prozent

#######################################################################################
# MODEL BUILDING: DECISION TREE MIT CARET/RPART UND 10-FOLD-CROSSVALIDATION ERSTELLEN #
#######################################################################################

variables <- Gewinner ~ Medianeinkommen + Katholisch + Mietpreis + PKW + Evangelisch + Temperatur + Aldi_Nord

#d_clean <- na.omit(d) # NA-Werte löschen (führt zu kleinerem Datensatz. Besser Imputation?)

tc <- trainControl(method = "repeatedcv", number = 10, repeats = 10) # Daten zufällig in zehn ähnlich große Blöcke aufteilen
train.rpart <- train(variables, data=d, method="rpart", tuneLength=10, metric="Accuracy", trControl=tc) # Metric "Accuracy" für Classification, "Rsquared" für Regression

plot(train.rpart$finalModel, uniform=TRUE) # Baum plotten
text(train.rpart$finalModel, all=TRUE) # Text plotten

###############
# EVALUIERUNG #
###############

train.rpart # Accuracy der Crossvalidation überprüfen (Hier nur 76 Prozent)

fancyRpartPlot(model = train.rpart$finalModel, sub = "") # Baum detaillierter plotten

# Caret-Dokumentation: http://topepo.github.io/caret/index.html
# Caret-Methods: https://topepo.github.io/caret/modelList.html
