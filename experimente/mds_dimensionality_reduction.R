## MDS

d <- read.csv('rohdaten_hochburgen_advanced.csv')
d_selection <- d[,12:18]
d_dist <- dist(d_selection) # euclidean distances between the rows

fit <- cmdscale(d_dist,eig=TRUE, k=2) # k is the number of dim
fit # view results

# plot solution 
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
     main="Metric	MDS", col = d$Farbe)
text(x, y, labels = d$Name, cex=.7, col = 'grey')
