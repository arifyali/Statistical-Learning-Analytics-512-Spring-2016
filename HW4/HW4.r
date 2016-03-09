
library(ISLR)
attach(Default)

glm.default = glm(default~balance, data = Default, family = binomial)

train = sample(nrow(Default), .5*nrow(Default), replace = F)
training = Default[train,]
validation = Default[-train,]

glm.default = glm(default~., data = training, family = binomial)

pred = predict(glm.default, validation, type = "response")
pred.default = rep(x = "No", times = length(pred))
pred.default[pred>0.5] = "Yes"

mean(pred.default != validation$default)

glm.default = glm(default~income+balance, data = Default, family = binomial)
cost <- function(r, pi = 0){
    mean(abs(r-pi) > 0.5)
    }
library(boot)
cv.default = cv.glm(Default, glm.default, cost, K = 3)
cv.default$delta

glm.default = glm(default~income+balance+student, data = Default, family = binomial)
cost <- function(r, pi = 0){
    mean(abs(r-pi) > 0.5)
    }
library(boot)
cv.default = cv.glm(Default, glm.default, cost, K = 3)
cv.default$delta

X = rnorm(100)
e = rnorm(100)

Y = 21 +45*X+38*X^2+12*X^3+e

XY = data.frame(Y,X)
names(XY)
library(leaps)
partc = regsubsets(Y~X+poly(X, 9), data = XY)


