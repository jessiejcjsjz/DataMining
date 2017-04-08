#######################################################
#Homework 3
#Name:  Jiani Shen  
#andrew ID: jianis1
#email: jianis1@andrew.cmu.edu
#######################################################

#Problem 1

#####################################
#code to download yahoo finance data#
####################################
# source('yahoo_finance_data_loading.R')
# ticker.list <- read.csv('SP500_ticker.csv',header=TRUE)[,1]
# first.day <- "2011-01-01"
# last.day <-"2014-12-31"
# 
# 
# 
# download.data <- NULL
# for (ticker in ticker.list){
#     print(ticker)
#     dataset <- NULL
#     try(dataset<- data.loading(ticker, first.day, last.day)$close)
#     download.data <-cbind(download.data, dataset)
# }
# 
# date <- row.names(download.data)
# download.data <- data.frame(date=date,download.data)
# write.table(download.data,'SP500_close_price_raw.csv',quote=FALSE,sep=",",row.names=FALSE)
# 
# #remove stocks with more than na.thresh missing values
# na.thresh <- 60
# stay.col <- colnames(download.data)[which(colSums(1*(is.na(download.data)))<na.thresh)]
# download.data2 <- download.data[,stay.col]
# write.csv(download.data2, 'SP500_close_price.csv')

#reference for zoo package
#http://cran.r-project.org/web/packages/zoo/vignettes/zoo-quickref.pdf
rm(list=ls())
library(zoo)
library(plyr)
library(MASS)
library(leaps)



import.csv <- function(filename){
  #return(read.csv(filename,row.names=1,sep="," ,header=TRUE))
  return(read.csv(filename,sep="," ,header=TRUE)) 
}

write.csv <- function(ob,filename){
  write.table(ob, filename, quote=FALSE, sep=",", row.names=FALSE)
}

##############################
#prepare data for PCA analysis
##############################
mydata <-import.csv('SP500_close_price.csv')
date <- as.Date(as.character(mydata[,1]), format="%Y-%m-%d")
myzoo <- zoo(mydata[,-1], date )
myzoo <- na.locf(myzoo) #impute missing values
prices2returns <- function(x) 100*diff(log(x)) #function to covert from price to return
log.return.zoo <- prices2returns(myzoo)
log.return.data <- coredata(log.return.zoo) #data
log.return.date <- time(log.return.zoo) #date


###############################
#problem 1 a
###############################
#turn the data into data frame
log.return.data<-as.data.frame(log.return.data)
#run pca on log.return.data
#obj<-prcomp(log.return.data)
obj<-prcomp(log.return.data,retx = TRUE, center = TRUE, scale = TRUE)
#screeplot
screeplot(obj, npcs = min(10,ncol(log.return.data)), type = 'line', main = "PCA Varance by Eigenvalue")
#Accumulative 
plot((summary(obj)$importance)[3,], main = "Accumulate % Variance Retained by PCA", 
     ylab = "Accumulate % Variance", 
     col=ifelse(summary(obj)$importance[3,] > 0.8, "red", "black"),
     xlab = "PCA")+ abline(h=0.8) + abline(v=127)

#how many principle should be contained in order to capture at least 80%???
which((summary(obj)$importance)[3,]>0.8)  # start from 2, so much retain at least 2 
#What is the magnitude of the estimated reconstruction error if we only retain top two of the PCA components?
#get all the rest STD besides the first 2, power them 2, then sum is the total variance we lose
lose<-obj$sdev[-1:-2]
var.lose<-sum(lose^2)
var.lose
##############################
#Problem 1 b
##############################

#get CP1
cp1<-obj$x[,1,drop = FALSE]
#plot the cp1, show the lowest date
plot(log.return.date,cp1,pch = 20, type = "b",
     col=ifelse(log.return.date==log.return.date[which.min(obj$x[, 1])], "red", "black"), 
     xlab = "Date", ylab = "PCA1", main = "PCA1 Value vs Time")
text( log.return.date[which.min(obj$x[, 1])],obj$x[, 1][which.min(obj$x[, 1])], 
     labels=log.return.date[which.min(obj$x[, 1])], cex= 2, adj = 0)


# check which date when cp1 is the lowest
log.return.date[which(cp1 == min(cp1))]

#get weights from CP1 and CP2
weights<-obj$rotation[,1:2]

#merge data , need sector information
weights<- data.frame(ticker = rownames(weights), PC1.W = weights[,1], PC2.W = weights[,2])
#remove the row names 
rownames(weights)<-c()

ticker<-import.csv("SP500_ticker.csv")
merged.weights<-merge(weights,ticker, by = "ticker")

#remove space " "from the sector name to avoid duplicated sector name
levels(merged.weights$sector)[levels(merged.weights$sector) == "Information Technology "]<-"Information Technology"
levels(merged.weights$sector)[levels(merged.weights$sector) == "Materials "]<-"Materials"
levels(merged.weights$sector)[levels(merged.weights$sector) == "Utilities "]<-"Utilities"
levels(merged.weights$sector)[levels(merged.weights$sector) == "Industrials "]<-"Industrials"
levels(merged.weights$sector)[levels(merged.weights$sector) == "Health Care "]<-"Health Care"
levels(merged.weights$sector)[levels(merged.weights$sector) == "Financials "]<-"Financials"
levels(merged.weights$sector)[levels(merged.weights$sector) == "Energy "]<-"Energy"
levels(merged.weights$sector)[levels(merged.weights$sector) == "Consumer Staples "]<-"Consumer Staples"
levels(merged.weights$sector)[levels(merged.weights$sector) == "Consumer Discretionary "]<-"Consumer Discretionary"

# calculate pc1 and pc2 mean by sector
pca.mean.sector<-ddply(merged.weights,"sector",summarise, pc1.w.m = mean(PC1.W), pc2.w.m = mean(PC2.W))
# plot bar, pc1 mean by weights
barplot(pca.mean.sector$pc1.w.m,
        names.arg = pca.mean.sector$sector,
        main = "PC1 Mean Weights by sector",
        cex.names = 0.7,las = 2)
# plot bar, pc2 mean by weights
barplot(pca.mean.sector$pc2.w.m,
        names.arg = pca.mean.sector$sector,
        main = "PC2 Mean Weights by sector",
        cex.names = 0.7,las = 2)


pca.mean.sector

##########################################
#Problem 2 a
##########################################
#assume the last column of the dataframe is the desised value
filter.features.by.cor<-function(df){
  #find the correlation of variables
  cor.result<-cor(test)
  #use only the last row and without theh last column
  cor.result<-cor.result[nrow(cor.result),-ncol(cor.result)]
  #turn the result into a data frame
  my.cor<-as.data.frame(cor.result)
  #order the result with the 
  my.cor<-my.cor[order(-my.cor$cor.result), , drop = FALSE]
  final.cor<-my.cor[1:3,,drop = FALSE]
  return(final.cor)
}

#test
test<-import.csv("BMI.csv")
filter.features.by.cor(test)

##########################################
#Problem 2 b
##########################################
test<-import.csv("BMI.csv")
leaps<-regsubsets(fatpctg~., data = test, nbest = 1, nvmax = 3)
#view resutls
summary(leaps)
# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
plot(leaps,scale="r2", main = "exhaustive search selected features")
# size =1 , abdomen
# size =2 , weight, abdomen
# size =3 , weight, abdomen, Wrist

##########################################
#Problem 2 c
##########################################
test<-import.csv("BMI.csv")
fit<-lm(fatpctg~., data = test )
#using the backward stepwise regression
step <- stepAIC(fit, direction="backward")
step$anova # display results


