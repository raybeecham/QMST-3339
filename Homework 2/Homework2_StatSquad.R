data(wine, package="rattle")

wine

# 1.  Determine the numerical variables and scale them using the default scale() function in R. 0.6 pts
sapply(wine, is.numeric) # Which Variables are numeric using logical operator. Type is False.

scWine <- scale(wine[-1]) # I am scaling the variables minus the first variable 'Type' since it is not numerical.

# 2. Using the scaled data set, utilize k-means clustering analysis for 25 randomly selected starting points, with 
# initial seed as 1234. Answer the questions below. 2.4 pts,0.6 pts each.
set.seed(1234)

# (a) Using WSS method, determine the number of clusters.
#WSS: Within cluster sum of squares
wss <- numeric(15) 
for (k in 1:15) wss[k] <- sum(kmeans(scWine, centers=k, nstart=25)$withinss)

#Screen Plot
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within Sum of Squares")
# There are 15 clusters, but the best number of clusters is 3 looking at the plot.
# WSS would equal 0 because there is no ob that is different from the center.
# You can see after 3 that it starts becoming linear and you could draw a line. Elbow Method.

print(wss) 
graphics.off()



# (b) Report the within cluster sum of squares by cluster.  What is the ratio of between sum of squares and total 
# sum of squares?   Comment on its meaning, does it correspond to having high quality clusters?
kWine <- kmeans(scWine, 3, nstart=25) #Checks clusters 25 times. Will converge to the correct amount
# We are not sampling just different starting points.

# Find the within cluster sum of squares
# Each observation would be its own cluster. 2 data points may be in each cluster.
# Would have diff sum of squares value.

kWine$withinss
kWine$tot.withinss
kWine$betweenss
kWine$betweenss/(kWine$tot.withinss+kWine$betweenss)
# This is the ratio of between sum of squares and total sum of squares
# Still a distance from the centroid value. The reason you would get a lower value..
# lower ratio's would correspond to lower quality clusters.

?withinss


# I would say according to the 44.8% the quality of clusters is average or does not correspond to having
# high quality clusters primary because being closer to 100% is the perfect cluster.

# (c) What is the size of each cluster? Share the centroid means of each cluster.
set.seed(1234)
kWine <- kmeans(scWine, 3, nstart=25)                        
kWine$size

# centroid means of each cluster
aggregate(wine[-1], by=list(cluster=kWine$cluster), mean)

# d)  Report the counts of wine types in each cluster. What type of wine do you see most in the first cluster?
table(kWine$cluster, wine$Type)

# The first type is seen the most. 59 out of 62.







library(datasets)
data(iris)
attach(iris)

# 1  Share the R code to draw 40 randomly selected records from the iris data set thatonly includes the numerical 
# variables of interest. Use the seed as 123. 0.5 pts
set.seed(123)
myD <- sample(1:dim(iris)[1], 40)
with40 <- iris[myD,1:5]
comp40 <- iris[myD,1:4]

# 2. Use Ward's method to create dendogram. Share the dendogram that is labelled with respect to species
myClust<-hclust(dist(comp40),method="ward.D")
graphics.off()
plot(myClust)
plot(myClust, labels=with40$Species)
plot(myClust, hang = -1, labels=with40$Species, xlab="Species")


# Height of 6 would give you 4 clusters.

# 3. Share the plot of dendrogram with three clusters. 0.5 pts
rect.hclust(myClust, k=3)
groups <- cutree(myClust, k=3)
graphics.off()



q()
