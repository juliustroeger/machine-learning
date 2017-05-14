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

# Parteistimmen faktorisieren: Hat z.B. die AfD die Fünf-Prozent-Hürde geschafft: Dummyvariable ja = 1, nein = 0)
d$factor_party <- d$AfD
d$factor_party <- factor(with(d,ifelse((factor_party <= 5),0,1)))

table(d$factor_party) # Check: In so vielen Briefwahlbezirken konnten Parteien gewinnen / Über die 5-Prozent-Hürde springen
prop.table(table(d$factor_party))*100 # Ausgabe in Prozent

#######################################################################################
# MODEL BUILDING: DECISION TREE MIT CARET/RPART UND 10-FOLD-CROSSVALIDATION ERSTELLEN #
#######################################################################################

# Erklärung: Interpretierbar vs. Genauigkeit je nach Anwendungsfall

variables <- factor_party ~ Wahlbeteiligung + ost_west + Medianeinkommen + Arbeitslosenquote + Ausländeranteil + Mietpreis

d_clean <- na.omit(d) # NA-Werte löschen (führt zu kleinerem Datensatz. Besser Imputation?)

tc <- trainControl(method = "repeatedcv", number = 10, repeats = 10) # Daten zufällig in zehn ähnlich große Blöcke aufteilen
train.rpart <- train(variables, data=d_clean, method="rpart", tuneLength=10, metric="Accuracy", trControl=tc) # Metric "Accuracy" für Classification, "Rsquared" für Regression

# OHNE CROSSVALIDATION TESTEN

plot(train.rpart$finalModel, uniform=TRUE) # Baum plotten
text(train.rpart$finalModel, use.n=TRUE, all=TRUE, cex=.7) # Text plotten

###############
# EVALUIERUNG #
###############

train.rpart # Accuracy der Crossvalidation überprüfen (Hier nur 76 Prozent)
# Fällt prinzipiell weg hier

