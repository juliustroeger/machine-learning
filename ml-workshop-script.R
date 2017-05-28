# Pakete installieren
#install.packages('needs')
library(needs)
needs(caret, car)

#######################
# FEATURE ENGINEERING #
#######################

# Zweitstimmen der Bundestagswahl 2013 mit soziodemographische Daten laden
d <- read.csv('ml-workshop-daten.csv') 

# Parteistimmen faktorisieren: Hat z.B. die AfD die Fünf-Prozent-Hürde geschafft: Dummyvariable ja = 1, nein = 0)
d$factor_party <- d$AfD
d$factor_party <- factor(with(d,ifelse((factor_party <= 10),0,1)))

##################
# MODEL BUILDING #
##################

variables <- factor_party ~ Wahlbeteiligung + Medianeinkommen + Arbeitslosenquote + Ausländeranteil + Mietpreis + Ländlichkeit

fit <- lm(AfD ~ Wahlbeteiligung + Medianeinkommen + Arbeitslosenquote + Ausländeranteil + Mietpreis + Ländlichkeit, data=d)
plot(d$GRÜNE~d$Ländlichkeit)
summary(fit)
vif(fit) # TO-DO: Wie kann man das interpretieren?

set.seed(123)
tc <- trainControl(method = "repeatedcv", number = 10, repeats = 10) # Daten zufällig in zehn ähnlich große Blöcke aufteilen
train.rpart <- train(variables, data=d, method="rpart", tuneLength=10, metric="Accuracy", trControl=tc) # Metric "Accuracy" für Classification, "Rsquared" für Regression

plot(train.rpart$finalModel, uniform=TRUE, margin=0.2) # Baum plotten
text(train.rpart$finalModel, use.n=TRUE, all=TRUE, cex=.7) # Text plotten

varImp(train.rpart) # TO-DO: Warum in umgekehrter Reihenfolge?
#plot(varImp(train.rpart))

###############
# EVALUIERUNG #
###############

train.rpart # Accuracy des Modells überprüfen
# TO-DO: Interpretierbar vs. Genauigkeit je nach Anwendungsfall / Ohne Crossvalidation gegenchecken?

