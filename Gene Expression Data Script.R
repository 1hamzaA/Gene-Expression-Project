# Import Dataset

install.packages("ISLR")
library("ISLR")

# Data Setup
labelsnci=NCI60$labs
datanci=NCI60$data

# Overview of Cell Line GRoups
table(labelsnci)

# Create 'Principal Component' Scores
pcacomp=prcomp(datanci, scale=TRUE)

# Plot 'Principal Component' Scores (First 3 Components)
Cols = function(vec){
     cols = rainbow(length(unique(vec)))
     return(cols[as.numeric(as.factor(vec))])
     }

par(mfrow = c(2,3))

plot(pcacomp$x[,1:2], col = Cols(labelsnci), pch = 17, xlab = "PC1", ylab = "PC2")
plot(pcacomp$x[,c(2:3)], col = Cols(labelsnci), pch = 17, xlab = "PC2", ylab = "PC3")


# Plot 'PVE' Scores by Principal Component
prvar = 100*pcacomp$sdev^2/sum(pcacomp$sdev^2)
plot(prvar, type = "o", xlab = "Principal Component", ylab = "PVE", col = "blue2")

# Plot Cumulative 'PVE' Scores by Principal Component
plot(cumsum(prvar), type = "o", xlab = "Principal Component", ylab = "Cumulative PVE",  col = "green2")


# Scale Data For Clustering
datanci = NCI60$data
scaled = scale(datanci)
dist = dist(scaled)

# Plot Linkage Clustering Outcomes
plot(hclust(dist, method = "single"), labels = labelsnci, main = "Single Linkage", xlab = "", sub = "", ylab = "")
plot(hclust(dist, method = "average"), labels = labelsnci, main = "Average Linkage", xlab = "", sub = "", ylab = "")
plot(hclust(dist), labels = labelsnci, main = "Complete Linkage", xlab = "", sub = "", ylab = "")

# Tabulate 'Complete Linkage' Cluster Splits
hclust = hclust(dist(scaled))

clusters=cutree(hclust,3)
table(clusters, labelsnci)

clusters=cutree(hclust,4)
table(clusters, labelsnci)


# K-means Clustering

km3.result = kmeans(scaled, 3, nstart = 10)
km4.result = kmeans(scaled, 3, nstart = 10)

fviz_cluster(km3.result, datanci, ellipse.type = "norm")
fviz_cluster(km4.result, datanc

# Hierarchical Clustering on First 7 PC Scores
plot(hclust,labels = labelsnci, main = "Cluster Dendrogram", xlab = "", sub = "", ylab = "", cex = ".5", col="25", cex = ".5")
abline(h = 140, col = "red")

hclust7 = hclust(dist(pcacomp[$x [,1:7]) )
plot(hclust7 , labels = labelsnci , main = "Hierarchical Clustering on First 7 PC Scores")
