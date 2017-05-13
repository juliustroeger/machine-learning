#######################
# PAKETE INSTALLIEREN #
#######################

load_packages <- function(x){
  for(i in 1:length(x)){  
    if(!require(x[i], character.only = TRUE)) {
      install.packages(x[i], repos="http://cran.us.r-project.org")
      require(x[i], character.only = TRUE)
    }
  }
}

load_packages(c("rpart", "rpart.plot", "rattle", "caret", "e1071"))

###########################################################
# DATEN EINLESEN, MERGEN, NORMALISIEREN UND FAKTORISIEREN #
###########################################################

setwd("/Users/juliustroeger/Projekte/r-scripts/wahl/")

d_election <- read.csv('wahl-zweitstimmen.tsv', sep='\t') # Stimmen der Briefwahlbezirke laden
d_socio <- read.csv('wahl-soziodemographisch.tsv', sep='\t') # Soziodemographische Daten laden
d <- merge(d_election, d_socio, by = "id") # Daten mergen

d[10:35] <- d[10:35] / d$Gültige.Stimmen * 100 # Abhängige Variablen Parteistimmen normalisieren
# Unabhängige Variablen normalisieren
d$Hartz4 = d$Einwohner.unter.65.in.SGB.II.2014 / d$Einwohner.unter.65.Jahren.2014 * 100
d$Ausländeranteil = d$Ausländer / d$Einwohner * 100
d$Gebürtige_Berliner_P = d$Gebürtige.Berliner / d$Einwohner * 100                           
d$Rentner = d$Deutsche.65.und.älter / d$Deutsche * 100
d$Migrationshintergund = d$Deutsche.18...Migrationshintergrund / d$Deutsche.18.. * 100
d$Einfache_Wohnlage = d$Deutsche.18...Wohnlage.einfach / d$Deutsche.18.. * 100
d$Flüchtlinge = d$Flüchtlinge / d$Einwohner * 100
d$Kaufkraft = d$Kaufkraft / 1000

# Spalte mit Parteien einfügen, die Briefwahlbezirke gewonnen haben
z <- apply(d[10:35],1,which.max) 
d$Gewinner <- names(d[10:35])[z]

# Parteistimmen faktorisieren (Ist Partei X stärkste Kraft im Briefwahlbezirk? ja = 1, nein = 0)
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

table(d$Gewinner) # Check: In so vielen Briefwahlbezirken konnten Parteien gewinnen
prop.table(table(d$Gewinner))*100 # Ausgabe in Prozent

#####################################
# DECISION TREE MIT RPART ERSTELLEN # 
#####################################

variables = Gewinner ~ Rentner + OstWest + Hartz4 + Migrationshintergund + Gebürtige_Berliner_P + Ausländeranteil + Miete + Kaufkraft + Einfache_Wohnlage + Flüchtlinge

# Decision Tree mit Trainings- und Testdaten
#set.seed(100) 
#dt <- sample(1:nrow(d), .7*nrow(d))
#train <- d[dt,] # 457 Briefwahlbezirke als Trainingsdatensatz
#test <- d[-dt,] # 196 Briefwahlbezirke als Testdatensatz

tree <- rpart(variables, method="class", data=d) # Decision Tree mit Gesamtdatensatz

tree_pruned <- prune(tree, cp=tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]) # Baum prunen (cp = complexity parameter)

plot(tree, uniform=TRUE) # Baum plotten
text(tree, all=TRUE) # Text plotten

# Baum mit Testdaten überprüfen
#p <- predict(tree_pruned, type="class", newdata=test)
#table(p, test$Gewinner)
#prop.table(table(p, test$Gewinner))
#sum(test$Gewinner==p)/length(p)

#################################################################
# DECISION TREE MIT CARET UND 10-FOLD-CROSSVALIDATION ERSTELLEN #
#################################################################

d_clean <- na.omit(d) # NA-Werte löschen (führt zu kleinerem Datensatz. Besser Imputation?)

tc <- trainControl(method = "repeatedcv", number = 10, repeats = 10) # Daten zufällig in zehn ähnlich große Blöcke aufteilen
train.rpart <- train(variables, data=d_clean, method="rpart", tuneLength=10, metric="Accuracy", trControl=tc) # Metric "Accuracy" für Classification, "Rsquared" für Regression

train.rpart # Accuracy der Crossvalidation überprügen

plot(train.rpart$finalModel, uniform=TRUE) # Baum plotten
text(train.rpart$finalModel, all=TRUE) # Text plotten

fancyRpartPlot(model = train.rpart$finalModel, sub = "") # Baum detaillierter plotten

# Caret-Dokumentation: http://topepo.github.io/caret/index.html
# Caret-Methods: https://topepo.github.io/caret/modelList.html
