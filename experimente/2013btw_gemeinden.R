#######################
# PAKETE INSTALLIEREN #
#######################
#install.packages('needs')
#library(needs)
needs(dplyr)

###########################
# WAHLERGEBNISSE EINLESEN #
###########################

# Pfad setzen
setwd("/Users/juliustroeger/Projekte/machine-learning/experimente/")

# Zweitstimmen Wahlbezirke Bundestagswahl 2013
btw13 <- read.csv('2013gemeinden.csv', encoding='UTF-8')

btw13$Union_rel <- btw13$CDU_rel + btw13$CSU_rel

btw13 <- subset(btw13, select = c(Name,Union_rel,SPD_rel,FDP_rel,DIE.LINKE_rel,GRÜNE_rel,AfD_rel)) 

d <- na.omit(btw13)

#MDA

d_dist <- dist(d) # euclidean distances between the rows
fit <- cmdscale(d_dist,eig=TRUE, k=1) # k is the number of dim
fit # view results
save(fit, file = "mda_gemeinden_2013_klein.rda")

# plot solution 
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
     main="Metric	MDS")
text(x, y, labels = d$, cex=.7)

#KMEAN
d_scale <- scale(d[2:ncol(d)]) # standardize variables
is.na(d_scale)
# K-Means Cluster Analysis
fit_k <- kmeans(d_scale, 3) # 5 cluster solution

plot(d_scale, col = fit_k$cluster)
points(fit_k$centers, col = 1:2, pch = 8, cex = 2)
text(d_scale, labels=d$Name, cex = 0.5)
