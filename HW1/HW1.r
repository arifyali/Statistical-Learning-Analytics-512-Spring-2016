
library("ISLR")
data(Auto)
Auto = na.omit(Auto)
Auto = Auto[!is.null(Auto),]
summary(Auto)


quantitative = c("mpg", "displacement", "horsepower", "weight", "acceleration")
for(i in quantitative){
    print(i)
    print(range(Auto[, i]))
    }

for(i in quantitative){
    print(i)
    print(mean(Auto[, i]))
    print(sd(Auto[, i]))    
    }

Auto.minus10.to85 = Auto[,-(10:85)]
for(i in quantitative){
    print(i)
    print(range(Auto.minus10.to85[, i]))
    print(mean(Auto.minus10.to85[, i]))
    print(sd(Auto.minus10.to85[, i]))    
    }

pairs(Auto[,quantitative])

library(MASS)
head(Boston)

dim(Boston)
names(Boston)

jpeg(filename = "ex10pb.jpeg")
pairs(Boston)
dev.off()

par(mfrow = c(4,4))
for(i in names(Boston)){
    plot(Boston$crim, Boston[, i], xlab="crim", ylab = i)
    }

quantile(Boston$crim, seq(from = 0, to = 1, by = .1))
hist(Boston$crim, breaks = 25)

quantile(Boston$tax)
hist(Boston$tax, breaks = 25)

quantile(Boston$ptratio)
hist(Boston$ptratio, breaks = 10)

sum(Boston$chas == 1)

median(Boston$ptratio)

Boston[Boston$medv == min(Boston$medv),]

sum(Boston$rm>7)
sum(Boston$rm>8)
summary(Boston[Boston$rm>8,])
