#######################
# PAKETE INSTALLIEREN #
#######################

#install.packages('needs')
library(needs)
needs(rpart, rpart.plot)

###########################################################
# DATEN EINLESEN, MERGEN, NORMALISIEREN UND FAKTORISIEREN #
###########################################################

d <- read.csv('Projekte/machine-learning/ml-workshop.csv') # Daten laden

plot(d$Grüne ~ d$Mietpreis)
fit <- lm(d$Grüne ~ d$Mietpreis)
abline(fit)
summary(fit)
#which(colnames(d)=="CDU.CSU")
# Spalte mit Parteien einfügen, die Briefwahlbezirke gewonnen haben
z <- apply(d[142:147],1,which.max) 
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

##########################################
# Trainings- und Testdatensatz erstellen # 
##########################################

set.seed(100)
dt <- sample(1:nrow(d), .7 * nrow(d))
train <- d[dt,] # 457 Briefwahlbezirke als Trainingsdatensatz
test <- d[-dt,] # 196 Briefwahlbezirke als Testdatensatz

#####################################
# Decision Tree mit Rpart erstellen # 
#####################################
variables = Gewinner ~ Rentner + OstWest + Hartz4 + Migrationshintergund + Gebürtige_Berliner_P + Ausländeranteil + Miete + Kaufkraft + Einfache_Wohnlage

# Baum erstellen und prunen
tree <- rpart(variables, method="class", data=train)
tree_pruned <- prune(tree, cp=tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"])

# Baum plotten
plot(tree, uniform=TRUE)
text(tree, all=TRUE, cex=.8)

# Baum mit Testdaten überprüfen
p <- predict(tree_pruned, type="class", newdata=test)
table(p, test$Gewinner)
prop.table(table(p, test$Gewinner))
sum(test$Gewinner==p)/length(p)