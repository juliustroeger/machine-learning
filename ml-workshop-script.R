#######################
# PAKETE INSTALLIEREN #
#######################

#install.packages('needs')
library(needs)
needs(caret)

################################################################################
# FEATURE ENGINEERING: DATEN EINLESEN, MERGEN, NORMALISIEREN UND FAKTORISIEREN #
################################################################################

# Arbeitsverzeichnis einrichten
setwd("/Users/juliustroeger/Projekte/")

# Zweitstimmen der Bundestagswahl 2013 und soziodemographische Daten laden
d <- read.csv('machine-learning/ml-workshop-daten.csv') 


# Spalte mit Parteien einfügen, die Landkreise gewonnen haben
which(colnames(d)=="factor_CDU")
z <- apply(d[5:34],1,which.max) 
d$Gewinner <- names(d[5:34])[z]
rm(z)

# Parteistimmen faktorisieren: Weniger oder mehr als x Prozent
factor_party <- d[5:34] 
colnames(factor_party) <- paste("factor", colnames(factor_party), sep = "_")
d <- cbind(d, factor_party)
rm(factor_party)

# Parteistimmen faktorisieren (Ist Partei X stärkste Kraft im Briefwahlbezirk? Dummyvariable ja = 1, nein = 0)
# Codierung ja = 1 nein = 0, wenn: Fünf-Prozent-Hürde für FDP und NPD, Zweistellig für AfD, d$Gewinner = SPD, CDU, Grüne, Linke
for(i in 68:ncol(d)){
  for(k in 1:nrow(d)){
    if(colnames(d[i]) == "factor_AfD"){
      if(d[k,i] >= 5){
        d[k,i] <- 1
      } else{d[k,i] <- 0}
    } else{
      if(colnames(d[i]) == "factor_SPD" | colnames(d[i]) == "factor_CDU" | colnames(d[i]) == "factor_CSU"){
        x <- unlist(strsplit(colnames(d[i]), "_"))[2]
        if(d$Gewinner[k] == x){
          d[k,i] <- 1
        } else{d[k,i] <- 0} 
      } else{
        if(d[k,i] >= 5){
          d[k,i] <- 1
        } else{d[k,i] <- 0}  
      }
    }
  }
}

d$factor_AfD <- as.factor(d$factor_AfD)

table(d$factor_AfD) # Check: In so vielen Briefwahlbezirken konnten Parteien gewinnen / Über die 5-Prozent-Hürde springen
prop.table(table(d$factor_AfD))*100 # Ausgabe in Prozent

#######################################################################################
# MODEL BUILDING: DECISION TREE MIT CARET/RPART UND 10-FOLD-CROSSVALIDATION ERSTELLEN #
#######################################################################################

# # Lineare Regression
# plot(d$GRÜNE ~ d$Art)
# fit_lin <- lm(d$GRÜNE ~ d$Medianeinkommen)
# abline(fit_lin)
# summary(fit_lin)
# 
# # Multiple Regression
# fit_lin_multi <- lm(d$GRÜNE_rel ~ d$Mietpreis + d$Medianeinkommen + d$Haus..und.Sperrmüll)
# summary(fit_lin_multi)
# # TO-DO: Kollinearität checken / wie damit umgehen

variables <- factor_AfD ~ Wahlbeteiligung + ost_west + Medianeinkommen + Arbeitslosenquote + Ausländeranteil + Gesamtwanderungssaldo + Geborene + Gestorbene + Schulabgänger.mit.Hauptschulabschluss + Schulabgänger.mit.Hochschulreife + Schulabgänger.ohne.Abschluss + Alle_Anzeigen + Anzeigen_Ausländerquote + Ladendiebstahl + Wohnungseinbruch + Autoklau + Fahrradklau + Betrug + Beförderungserschleichung + Widerstand_Polizei + Brandstiftung + Rauschgift + Mord_Totschlag + Mietpreis + Mietsteigerung + Kaufpreis + PKW + Krafträder + Cabrios + Elektroautos + Sportwagen + Kirchenmitglieder

d_clean <- na.omit(d) # NA-Werte löschen (führt zu kleinerem Datensatz. Besser Imputation?)

tc <- trainControl(method = "repeatedcv", number = 10, repeats = 10) # Daten zufällig in zehn ähnlich große Blöcke aufteilen
train.rpart <- train(variables, data=d_clean, method="rpart", tuneLength=10, metric="Accuracy", trControl=tc) # Metric "Accuracy" für Classification, "Rsquared" für Regression

plot(train.rpart$finalModel, uniform=TRUE) # Baum plotten
text(train.rpart$finalModel, all=TRUE) # Text plotten

###############
# EVALUIERUNG #
###############

train.rpart # Accuracy der Crossvalidation überprüfen (Hier nur 76 Prozent)

# TO-DO: Diesen Punkt detaillierter. Wie Modell tunen? Andere Algorithmen testen und vergleichen? RSME nötig?

