# read the data to R

# data.csv must be located into your working directory, to know your current working directory, type getwd()
data = read.csv("data.csv")


# Kmeans Clustering

km = kmeans(data, 3)

par(mfrow = c(1, 2))

plot(data$x, data$y, pch=16, xlab = "X", ylab = "Y", xlim=c(0,10), ylim=c(0,10), main="Original Data",)
plot(data$x, data$y, pch=16, xlab = "X", ylab = "Y", xlim=c(0,10), ylim=c(0,10), main="K-means Clustering", col=km$cluster)


# Hierarchical Clustering

par(mfrow=c(2, 2))
hc_complete = hclust(dist(data), method= "complete")
plot(hc_complete, hang=-1, main="Complete Linkage", xlab="", ylab="", sub="")
rect.hclust(hc_complete , k = 3, border = 2:6)

hc_single = hclust(dist(data), method= "single")
plot(hc_single, hang=-1, main="Single Linkage", xlab="", ylab="", sub="")
rect.hclust(hc_single , k = 3, border = 2:6)

hc_average = hclust(dist(data), method= "average")
plot(hc_average, hang=-1, main="Average Linkage", xlab="", ylab="", sub="")
rect.hclust(hc_average , k = 3, border = 2:6)

hc_centroid = hclust(dist(data), method= "centroid")
plot(hc_centroid, hang=-1, main="Centroid Linkage", xlab="", ylab="", sub="")
rect.hclust(hc_centroid , k = 3, border = 2:6)
