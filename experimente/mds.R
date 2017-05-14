## MDS

d <- read.csv('machine-learning/ml-workshop-daten.csv') 
#d <- d[, !(colnames(d) %in% c("Name", "Landbevölkerung", "Landbevölkerung.1", "Ältere.Landbevölkerung", "Jüngere.Dorfbewohner" ))]
d <- d[, !(colnames(d) %in% c("KRS","Art","ost_west"))]
d <- na.omit(d)

d_dist <- dist(d) # euclidean distances between the rows
fit <- cmdscale(d_dist,eig=TRUE, k=2) # k is the number of dim
fit # view results

# plot solution 
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
     main="Metric	MDS")
text(x, y, labels = d$Name, cex=.7)

## KMEANS CLUSTER

d <- read.csv('machine-learning/ml-workshop-daten.csv')
#d <- d[, !(colnames(d) %in% c("Name", "Landbevölkerung", "Landbevölkerung.1", "Ältere.Landbevölkerung", "Jüngere.Dorfbewohner" ))]
d <- d[, !(colnames(d) %in% c("KRS","Art","ost_west"))]
d <- na.omit(d)
d_scale <- scale(d[3:32]) # standardize variables
is.na(d_scale)
# K-Means Cluster Analysis
fit <- kmeans(d_scale, 5) # 5 cluster solution


plot(d_scale, col = fit$cluster)
points(fit$centers, col = 1:2, pch = 8, cex = 2)
text(d_scale, labels=d$Name, cex = 0.5)
