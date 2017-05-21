#######################
# PAKETE INSTALLIEREN #
#######################

#install.packages('needs')
#library(needs)
needs(caret,corrplot)

################################################################################
# FEATURE ENGINEERING: DATEN EINLESEN, MERGEN, NORMALISIEREN UND FAKTORISIEREN #
################################################################################

# Arbeitsverzeichnis einrichten
setwd("/Users/juliustroeger/")

# Zweitstimmen der Bundestagswahl 2013 und soziodemographische Daten laden
d <- read.csv('Desktop/amazon/plz-daten.csv') 
d$Amazon <- as.factor(d$Amazon)

table(d$Amazon) # Check: In so vielen Briefwahlbezirken konnten Parteien gewinnen / Über die 5-Prozent-Hürde springen
prop.table(table(d$Amazon))*100 # Ausgabe in Prozent


M<-cor(d) # compute correlation matrix
corrplot(M, method="circle")

#######################################################################################
# MODEL BUILDING: DECISION TREE MIT CARET/RPART UND 10-FOLD-CROSSVALIDATION ERSTELLEN #
#######################################################################################

variables <- Amazon ~ Einwohner + Kaufkraft + Mietpreis + Mietsteigerung + Arbeitslosenquote + CDU.Wähler + Linke.Wähler + SPD.Wähler + Grüne.Wähler + FDP.Wähler + Afd.Wähler + Autobesitzer + Hauptschulabschluss + Realschulabschluss + Abitur + Konservativ.Etabliertes.Milieu + Liberal.Intellektuelles.Milieu + Milieu.der.Performer + Expeditives.Milieu + Bürgerliche.Mitte + Adaptiv.Pragmatisches.Milieu + Sozialökologisches.Milieu + Traditionelles.Milieu + Prekäres.Milieu + Hedonistisches.Milieu + Junge.Singles + Junge.Paare + Junge.Familien.mit.Kind + Singles + Paare + Familien.mit.Kind + Alleinstehende.Senioren + Ältere.Paare + Ältere.Mehrpersonenhaushalte + Einwohner_Amt + Gebürtige.Berliner + Zugezogene + Einfache.Wohnlage + Einfache.Wohnlage.mit.Lärm + Mittlere.Wohnlage + Mittlere.Wohnlage.mit.Lärm + Gute.Wohnlage + Gute.Wohnlage.mit.Lärm + Migrationshintergrund + Ausländeranteil + Alter + Neugeborene + Kita.Kinder + Schüler + Rentner + Airbnb.Ferienwohnungen + Internet.Ferne + Internet.Laien + Selektivorientierte + Sicherheitsorientierte + Effizienzorientierte + Unterhaltungsorientierte + Digital.Souveräne + Werbeverweigerer + Statushohe.Großstädter + Gutsituierte.in.stadtnahen.Umlandgemeinden + Gute.Wohngebiete.in.mittelgroßen.Städten + Städtische.Problemgebiete + Hochhäuser.und.einfache.Mietwohnungen + Rentner.in.einfachen.Nachkriegsbauten + Alte.Häuser.auf.dem.Land + Arbeiter.in.kleinen.Städten + Ältere.Leute.in.Umlandgemeinden + Landbevölkerung + Attraktive.innerstädtische.Wohnlagen + Wohlhabende.Akademiker.in.Villenvierteln + Gutverdienende.Familien.in.neueren.Eigenheimen.im.Umland + Gediegene.ältere.Einzelhäuser + Speckgürtel..Gute.neuere.Einzelhäuser + Gute.neue.Einzelhäuser + Alte.Ortskerne + Neue.Reihenhäuser.im.ländlichen.Raum + Einfache.Häuser.im.Grünen + Ältere.Mehrfamilienhäuser + Aufsteiger..Gehobene.Berufe.in.Außenbezirken + Mittelstand.in.ländlichen.Gemeinden + Sozialer.Wohnungsbau.und.einfache.Mehrfamilienhäuser + Nicht.modernisierter.Altbau + Blockbebauung.geringen.Standards + Multi.kulturelle.Innenstadtbereiche + Hochhäuser.einfachen.Standards + Älterer.sozialer.Wohnungsbau + Einfache.städtische.Zeilenbau.Siedlungen + Soziale.Brennpunkte + Jüngere.Leute.in.älteren.Mietwohnungen + Mittelstand.in.älteren.Quartieren + Einfache.Leute.in.Mietwohnungen + Einfache.alleinstehende.Rentner + Jüngere.Dorfbewohner + Einfache.Berufe.auf.dem.Land + Gering.qualifizierte.Arbeiter + Selbständige.in.neueren.Häusern + Handwerker.im.ländlichen.Raum + Sozial.schwache.Kleinstädter + Peripherie..Dörfer.in.Randlagen + Senioren.im.Umland + Ältere.Familien.am.Stadtrand + Solide.Rentner.in.Zweifamilienhäusern + Ältere.Leute.in.älteren.Häusern + Gutsituierte.Senioren.in.Vororten + Ältere.Landbevölkerung + Landbevölkerung + Umzugsvolumen + Umzugssaldo + Fluktuation + Nahumzugsquote + Fernumzugsquote

d_clean <- na.omit(d) # NA-Werte löschen (führt zu kleinerem Datensatz. Besser Imputation?)

tc <- trainControl(method = "repeatedcv", number = 10, repeats = 10) # Daten zufällig in zehn ähnlich große Blöcke aufteilen
train.rpart <- train(variables, data=d_clean, method="rpart", tuneLength=10, metric="Accuracy", trControl=tc) # Metric "Accuracy" für Classification, "Rsquared" für Regression

plot(train.rpart$finalModel, uniform=TRUE) # Baum plotten
text(train.rpart$finalModel, use.n=TRUE, all=TRUE, cex=.7) # Text plotten

###############
# EVALUIERUNG #
###############

train.rpart # Accuracy der Crossvalidation überprüfen (Hier nur 76 Prozent)

# TO-DO: Diesen Punkt detaillierter. Wie Modell tunen? Andere Algorithmen testen und vergleichen? RSME nötig? Was sind die wichtigsten Einflussfaktoren? Was sagt uns Decision Tree jetzt genau? Doch lieber Gemeinden?
# Naive Bayes oder Neuronales Netz mit Tensorflow spannender?

