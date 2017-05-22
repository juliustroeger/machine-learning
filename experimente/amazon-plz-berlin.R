#install.packages('needs')
#library(needs)
needs(caret,car)

d <- read.csv('plz-daten.csv') 

fit <- lm(Mietpreis ~ Linke.Wähler + Grüne.Wähler + FDP.Wähler + Autobesitzer + Realschulabschluss + Einfache.Wohnlage + Mittlere.Wohnlage + Gute.Wohnlage + Migrationshintergrund + Neugeborene + Schüler + Airbnb.Ferienwohnungen, data=d)
summary(fit)
#alias(fit)
vif(fit)

variables <- Mietpreis ~ Linke.Wähler + Grüne.Wähler + FDP.Wähler + Autobesitzer + Realschulabschluss + Einfache.Wohnlage + Mittlere.Wohnlage + Gute.Wohnlage + Migrationshintergrund + Neugeborene + Schüler + Airbnb.Ferienwohnungen

d_clean <- na.omit(d) # NA-Werte löschen

tc <- trainControl(method = "repeatedcv", number = 10, repeats = 10) # Daten zufällig in zehn ähnlich große Blöcke aufteilen
train.rpart <- train(variables, data=d_clean, method="rf", tuneLength=10, metric="Rsquared", trControl=tc) # Metric "Accuracy" für Classification, "Rsquared" für Regression

plot(train.rpart$finalModel, uniform=TRUE) # Baum plotten
text(train.rpart$finalModel, use.n=TRUE, all=TRUE, cex=.7) # Text plotten

varImp(train.rpart$finalModel)

###############
# EVALUIERUNG #
###############

train.rpart # Accuracy der Crossvalidation überprüfen 

