#utf-8
# A multinomial response recoded to use clogit
#  The revised data set has one copy per possible outcome level, with new
#  variable tocc = target occupation for this copy, and case = whether
#  that is the actual outcome for each subject.
# See the reference below for the data.
setwd("~/rwd/m04")
rm(list = ls())
gc()
#library(readxl)
library(survival)
myd <- read.csv('data1.csv')
myd1 <- myd[,1:16]
summary(myd1)
str(myd1)
myd1$注册资本 <- as.numeric(myd1$注册资本)
myd1$是否国家级开发区 <- factor(myd1$是否国家级开发区)
myd1$是否红岛新区 <- factor(myd1$是否红岛新区)
myd1$是否蓝色硅谷 <- factor(myd1$是否蓝色硅谷)
myd1$企业选址 <- factor(myd1$企业选址)
resp <- levels(myd1$企业选址)
n <- nrow(myd1)
indx <- rep(1:n, length(resp))
logan <- data.frame(myd1[indx,],
                     id = indx,
                     tocc = factor(rep(resp, each=n)))
logan$case <- (logan$企业选址 == logan$tocc)
str(logan)
#fit1 <- clogit(case ~ tocc + tocc:education,data = logan,iter.max = 10)



fit <- clogit(logan$case ~ logan$X2.km.缓冲区半径的企业总数 + logan$X2014年之前企业数量 + 
                logan$X2014之后新入驻企业数量 + logan$人员平均工资 + logan$注册资本 +
                logan$土地均价 + logan$距市中心距离 + logan$距机场距离 + logan$距火车站距离 +
                logan$距最近主干道的距离 + logan$一般科研院所距离 + logan$重点科研院所距离 +
                logan$是否国家级开发区 + logan$是否红岛新区 + logan$是否蓝色硅谷,iter.max = 1000)


clogit(logan$case ~ logan$X2.km.缓冲区半径的企业总数 + logan$X2014之后新入驻企业数量 + 
         logan$X2014之后新入驻企业数量 + logan$人员平均工资 + logan$注册资本 ,iter.max = 100)


#####2
myd <- read.csv('data2.csv')
myd1 <- myd[,1:16]
summary(myd1)
str(myd1)
myd1$注册资本 <- as.numeric(myd1$注册资本)
myd1$是否国家级开发区 <- factor(myd1$是否国家级开发区)
myd1$是否红岛新区 <- factor(myd1$是否红岛新区)
myd1$是否蓝色硅谷 <- factor(myd1$是否蓝色硅谷)
myd1$企业选址 <- factor(myd1$企业选址)
resp <- levels(myd1$企业选址)
n <- nrow(myd1)
indx <- rep(1:n, length(resp))
logan <- data.frame(myd1[indx,],
                    id = indx,
                    tocc = factor(rep(resp, each=n)))
logan$case <- (logan$企业选址 == logan$tocc)
str(logan)
#fit1 <- clogit(case ~ tocc + tocc:education,data = logan,iter.max = 10)



fit <- clogit(logan$case ~ logan$X2.km.缓冲区半径的企业总数 + logan$X2014年之前企业数量 + 
                logan$X2014之后新入驻企业数量 + logan$人员平均工资 + logan$注册资本 +
                logan$土地均价 + logan$距市中心距离 + logan$距机场距离 + logan$距火车站距离 +
                logan$距最近主干道的距离 + logan$一般科研院所距离 + logan$重点科研院所距离 +
                logan$是否国家级开发区 + logan$是否红岛新区 + logan$是否蓝色硅谷,iter.max = 1000)


clogit(logan$case ~ logan$X2.km.缓冲区半径的企业总数 + logan$X2014之后新入驻企业数量 + 
         logan$X2014之后新入驻企业数量 + logan$人员平均工资 + logan$注册资本 ,iter.max = 100)

#####
myd <- read.csv('data3.csv')
myd1 <- myd[,1:16]
summary(myd1)
str(myd1)
myd1$注册资本 <- as.numeric(myd1$注册资本)
myd1$是否国家级开发区 <- factor(myd1$是否国家级开发区)
myd1$是否红岛新区 <- factor(myd1$是否红岛新区)
myd1$是否蓝色硅谷 <- factor(myd1$是否蓝色硅谷)
myd1$企业选址 <- factor(myd1$企业选址)
resp <- levels(myd1$企业选址)
n <- nrow(myd1)
indx <- rep(1:n, length(resp))
logan <- data.frame(myd1[indx,],
                    id = indx,
                    tocc = factor(rep(resp, each=n)))
logan$case <- (logan$企业选址 == logan$tocc)
str(logan)
#fit1 <- clogit(case ~ tocc + tocc:education,data = logan,iter.max = 10)



fit <- clogit(logan$case ~ logan$X2.km.缓冲区半径的企业总数 + logan$X2014年之前企业数量 + 
                logan$X2014之后新入驻企业数量 + logan$人员平均工资 + logan$注册资本 +
                logan$土地均价 + logan$距市中心距离 + logan$距机场距离 + logan$距火车站距离 +
                logan$距最近主干道的距离 + logan$一般科研院所距离 + logan$重点科研院所距离 +
                logan$是否国家级开发区 + logan$是否红岛新区 + logan$是否蓝色硅谷,iter.max = 1000)


clogit(logan$case ~ logan$X2.km.缓冲区半径的企业总数 + logan$X2014之后新入驻企业数量 + 
         logan$X2014之后新入驻企业数量 + logan$人员平均工资 + logan$注册资本 ,iter.max = 100)