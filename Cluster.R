#Load libraries
library(cluster)
library(fpc)
library(ggplot2)
library(dendextend)
library(dplyr)
library(datasets)
library(NbClust)
library(heatmaply)

getwd()
setwd("C:/Users/USER/Desktop/MS in Business Analytics/Data Mining/Assignment 3")
# Read product data file. Data on product nutrition from openfoodfacts.org
# Note colClasses specifies the variable types - first 2 are character, the rest (48) are numberic
# Interpret undefines as NA's

ProductData <- read.csv("C:/Users/USER/Desktop/MS in Business Analytics/Data Mining/Assignment 3/products2.csv", na.strings = "undefined",
                        colClasses = c("character", "character", rep("numeric", 48)))
head(ProductData)
# Delete first column
ProductData$prodid <-NULL

# Select Product Data Subset that has less than 26 NA's
ProdDataSubset <- ProductData[rowSums(is.na(ProductData))<26,]
summary(ProdDataSubset)
head(ProdDataSubset)
nrow(ProdDataSubset)

# Remove duplicates
CleanProductData <- ProdDataSubset %>% distinct(prodname, .keep_all = TRUE)
head(CleanProductData)

# Set NA's to zero
CleanProductData[is.na(CleanProductData)] <- 0
head(CleanProductData)

# Set productname as row names
rownames(CleanProductData) <- CleanProductData[,1]
CleanProductData$prodname <- NULL
summary(CleanProductData)

head(CleanProductData)
nrow(CleanProductData)
ncol(CleanProductData)

# Scale the data
ScaledProdData <- scale(CleanProductData)
is.na(ScaledProdData)
ScaledProdData[is.na(ScaledProdData)] <- 0

head(ScaledProdData)

nrow(ScaledProdData)
ncol(ScaledProdData)

# Calculate distance matrix. Many clustering algorithms need dist matrix as input
proddist <- dist(ScaledProdData, method = "euclidean")

proddist

##################################################################################
# Create hierachical cluster, agnes automatically creates dist matrix so feed raw data
prodclusters <- agnes(CleanProductData, method = "complete", metric = "euclidean")

prodclusters

# Dendogram of product data
plot(prodclusters, which.plots=2, cex = 0.5)

# Create hierarchial cluster on scaled data
prodclusters1 <- agnes(ScaledProdData, method = "complete", metric = "euclidean")

# Dendogram on scaled product data
plot(prodclusters1, which.plots=2, cex = 0.5)

##############################################################

# Optimal Cluster Number

bestK1 <- NbClust(ScaledProdData, min.nc=2, max.nc = 15, method="kmeans",
                  index= c("kl"))
bestK1
bestK1$All.index
bestK1$Best.nc
bestK1$Best.partition

bestK2 <- NbClust(ScaledProdData, min.nc=2, max.nc = 15, method="kmeans",
                  index= c("gap"))
bestK2
bestK2$All.index
bestK2$Best.nc
bestK2$Best.partition

bestK3 <- NbClust(ScaledProdData, min.nc=2, max.nc = 15, method="kmeans",
                  index= c("silhouette"))
bestK3
bestK3$All.index
bestK3$Best.nc
bestK3$Best.partition

########################################################
#####The knee bend is at 2, 8 and 10
k.max <- 12 # Maximal number of clusters
data <- ScaledProdData
wss <- sapply(2:k.max, 
              function(k){kmeans(data, k, nstart=25 )$tot.withinss})
plot(2:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

abline(v = 2, lty =3)
abline(v = 8, lty =9)
abline(v = 10,lty= 11)
###########################################################

# K-Means Clustering using 2 clusters
prodclusterk <- kmeans(ScaledProdData,2, nstart = 25)

prodclusterk$cluster
plot(ScaledProdData, col = prodclusterk$cluster)
plot(ScaledProdData[,c(3,4)], col = prodclusterk$cluster)
plot(ScaledProdData[,c(27,28)], col = prodclusterk$cluster)
plotcluster(ScaledProdData,prodclusterk$cluster)

# Get sum of squares within clusters and between clusters
prodclusterk
prodclusterk$withinss
prodclusterk$tot.withinss
prodclusterk$betweenss

####################################################

# K-Means Clustering using 8 clusters
prodclusterk1 <- kmeans(ScaledProdData,8, nstart = 25)

prodclusterk1$cluster
plot(ScaledProdData, col = prodclusterk1$cluster)
plot(ScaledProdData[,c(3,4)], col = prodclusterk1$cluster)
plot(ScaledProdData[,c(27,28)], col = prodclusterk1$cluster)
plotcluster(ScaledProdData,prodclusterk1$cluster)


# Get sum of squares within clusters and between clusters
prodclusterk1
prodclusterk1$withinss
prodclusterk1$tot.withinss
prodclusterk1$betweenss

####################################################

# K-Means Clustering using 10 clusters
prodclusterk2 <- kmeans(ScaledProdData,10, nstart = 25)

prodclusterk2$cluster
plot(ScaledProdData, col = prodclusterk2$cluster)
plot(ScaledProdData[,c(3,4)], col = prodclusterk2$cluster)
plot(ScaledProdData[,c(27,28)], col = prodclusterk2$cluster)
plotcluster(ScaledProdData,prodclusterk2$cluster)


# Get sum of squares within clusters and between clusters
prodclusterk2
prodclusterk2$withinss
prodclusterk2$tot.withinss
prodclusterk2$betweenss

####################################################

# Kohonen Self Organizing Map
library(kohonen)

# Create Kohonen SOM
prodsom <- som(data=ScaledProdData, grid = somgrid(5, 4, "hexagonal"))

# Plot map
plot(prodsom, type="mapping", labels = rownames(ScaledProdData))

# Look at codes
plot(prodsom)

######################################
##########Extra Credit################
########### Heat Map##################

# scale data to mean=0, standard deviation=1 and convert it to matrix
prodscaled <- as.matrix(scale(ScaledProdData))

## replacing inf, na, nan to 0
is.na(prodscaled) <- sapply(prodscaled, is.infinite)
prodscaled[is.na(prodscaled)] <- 0
prodscaled[is.nan(prodscaled)] <- 0

# create heatmap and don't reorder columns
heatmaply(prodscaled, Colv=F, scale='none')
heatmap(prodscaled, Colv=F, scale='none')

# clustering rows
hc.rows <- hclust(dist(prodscaled))
plot(hc.rows)

# transpose the matrix and clustering columns
hc.cols <- hclust(dist(t(prodscaled)))
plot(hc.cols)

# Heatmap for first cluster
heatmaply(prodscaled[cutree(hc.rows,k=2)==1,], Colv=as.dendrogram(hc.cols), scale='none',margins = c(60, 150))

# Heatmap for second cluster
heatmaply(prodscaled[cutree(hc.rows,k=2)==2,], Colv=as.dendrogram(hc.cols), scale='none')



