#HW 1
###summary stats and basic visualization

#http://rstudio-pubs-static.s3.amazonaws.com/20480_ed16ede2f1ca46beb2 c0e95dbfb1d5a1.html
# Annual Average Pollution in the US
pollution <- read.csv(file="avgpm25.csv")
attach(pollution)
#pollution <- read.csv(file="avgpm25.csv",colClasses = c("numeric", "character", "factor", "numeric", "numeric"))
# First five observations
head(pollution,5)

firstFiveObs <- head(pollution,5)
firstFiveObs

#Five Number Summary
fivenum(pm25) #-------------------------------------------------->
##The five-number summary refers to a collection of five numbers that represent the basic statistical characteristics
#of a data set. The "Minimum" and "Maximum" are smallest and largest number in the data set. The
#"Mean" is the average of all values in the data set. The "Upper-Quartile" and "Lower-Quartile" refer
#to the median value of the upper and lower halves of the data set, respectively. It is important to note that
#it a specific set of data must be specified in order to perform an insightful five-number analysis. 
#In the case of avgpm25, there are 4 numerical variables (columns) that could be assessed. 
#The pollution level (pm25) was chosen.

fiveNumSum <- fivenum(pm25)
fiveNumSum
sum(is.na(pollution)) # how many nulls in our dataset = 0
md.pattern(pollution) # shows us table of obs and na's
aggr(pollution, prop=T, numbers=T) # numbers and proportions of missing data
#min, Q1, median, Q3, max

# Boxplots
boxplot(pm25, col = "red",main="Boxplot for Particle Pollution" ,ylim=c(0,20))

###Density plots
#plot(density(pollution$pm25), col="green")

#Multiple Boxplots
boxplot(pm25 ~ region, data = pollution, col = "red")

# Histograms
hist(pollution$pm25, col = "green", breaks = 100)
library(Hmisc)
minor.tick(nx=0, ny=2, tick.ratio=0.5) # Adds minor tick
rug(pollution$pm25)
#abline(v = 12,lwd = 2)
abline(v = median(pollution$pm25), col = "magenta", lwd = 4)
#abline(v = mean(pollution$pm25), col = "red", lwd = 4)

#Multiple Histograms
par(mfrow = c(2, 1), mar = c(4, 3, 3, 1)) 

hist(subset(pollution, region == "east")$pm25, col = "green", main ="Histogram of Pollution East (pm25)", xlab ="Pollution Level (pm25)")
hist(subset(pollution, region == "west")$pm25, col = "green", main ="Histogram of Pollution West (pm25)", xlab ="Pollution Level (pm25)")
graphics.off()
#Barplots

table(region)

barplot(table(region), col = "#8d734a", main = "Number of Counties in Each Region")
abline(h = 442, lwd = 2, lty = 2)
abline(h = 134, lwd = 2, lty = 2)
graphics.off()

# Conditional Plots
boxplot(pm25 ~ region, data = pollution, col = "blue")

with(pollution,boxplot(pm25 ~ region, col = "blue"))

par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
hist(subset(pollution, region == "east")$pm25, col = "green")
hist(subset(pollution, region == "west")$pm25, col = "green")
par(mfrow=c(1,1))

with(pollution, plot(latitude, pm25))
abline(h = 12, lwd = 2, lty = 2)

# Pollution by Latitude and Region
with(pollution, plot(latitude, pm25, col = region))
abline(h = 12, lwd = 2, lty = 2)
#legend(50,18,legend=c("East", "West"), pch=c(1,1), 
#       col=c(1,2))

#par(mfrow = c(1, 1))
#Scatterplot
with(pollution, plot(latitude, pm25))
abline(h = 12, lwd = 2, lty = 2)

#Scatterplot - Using Color
with(pollution, plot(latitude, pm25, col = region))
abline(h = 12, lwd = 2, lty = 2)

#Multiple Scatterplots
par(mfrow = c(1, 2))
par(mfrow = c(1, 2), mar = c(5, 4, 2, 1))
with(subset(pollution, region == "west"), plot(latitude, pm25, main = "West",ylim=c(0,20)))
with(subset(pollution, region == "east"), plot(latitude, pm25, main = "East",ylim=c(0,20)))

#Multiple Scatterplots on the same frame

graphics.off()
with(pollution, plot(latitude, pm25, main = "Pollution level for west and east", type = "n", ylim=c(0,20)))
with(subset(pollution, region == "west"), plot(latitude, pm25,col = "blue", pch = 2))
with(subset(pollution, region == "east"), points(latitude, pm25,col = "red"))
legend("topright", pch = c(2,1), col = c("blue", "red"), legend = c("West", "East"))



##t test--- Independent
west=subset(pollution, region == "west")
east=subset(pollution, region == "east")
#var.test(s1[1:134,]$pm25,s2[1:134,]$pm25) # Reject the equality of variances; p-value = 1.658e-10
t.test(west[1:134,]$pm25,east[1:134,]$pm25, alternative ="two.sided", var.equal = FALSE, mu=0)
# p-value = 2.127e-14, reject the equality of means.
#t.test(west[1:134],east[1:134], alternative ="two.sided", var.equal = FALSE, mu=0)

t.test(west[1:134,1],east[1:134,1], alternative ="two.sided", var.equal = FALSE, mu=0)

# T Test--- Dependent
t.test(west[1:134,],east[1:134,], alternative ="two.sided", var.equal = FALSE, mu=0)

t.test(west[1:134,1],east[1:134,1], alternative ="two.sided", var.equal = FALSE, mu=0, paired = T)
westM <- mean(west[1:134,]$pm25)
westM
eastM <- mean(east[1:134,]$pm25)
eastM

meandiff <- eastM - westM
meandiff # Difference of means

graphics.off()
library(lattice)
xyplot(pm25 ~ longitude | region, data = pollution, layout = c(1, 2))
xyplot(pm25 ~ longitude | region, data = pollution, layout = c(2, 1))


#lattice
#used for panels
# Lattice functions generally take a formula for their first argument, usually of the form
#xyplot($y \sim x | f * g$, data)\\
# Lattice functions have a panel function which controls what happens inside each panel of the plot:
#It depends on the variable of focus. Second one can be argued to focus on comparison of pollution level more, since it provides one unified index.
detach(pollution)


