#Author: jianis1 Jiani Shen
#required liabrary for this homework 1

library("gplots")
library("ggplot2")
library("FNN")
library("plyr")


#########################################hw1 Problem1 part a#######################################

#import data to be data frame
# Utility function for importing data from a csv (comma-separated values, flat table) file
import.csv <- function(filename) {
  return(read.csv(filename, sep = ",", header = TRUE))
}
# Utility function for exporting data to csv file
# Note that csv files are very portable, practically all tools understand them,
# including Microsoft Excel
write.csv <- function(ob, filename) {
  write.table(ob, filename, quote = FALSE, sep = ",", row.names = FALSE)
}

#brief take into a data frame
#returns to each column name, type, missing value numbers and statistic facts
brief<-function(my.data){
  #get name of the input data frame
  my.data.name<-deparse(substitute(my.data))
  #result1 to store numeric variable result
  result1<-data.frame()
  #result4 to store symbolic variable result
  result4<-data.frame()
  #loop to print out each columns' information
  #for each column, check if the column is numeric or is factor
  #if it is numeric, create a dataframe that store column information,
  #NA is removed before any calculation
  #if it is symbolic(factor), remove the those with no value first, then count words frequency
  i<-1
  for(i in 1:ncol(my.data)){
    my.data.col<-my.data[,i]
    if(is.numeric(my.data.col)){
      df<-data.frame(Attribute_ID = c(i),
                     Attribute_Name = c(colnames(my.data)[i]),
                     Missing = c(sum(is.na(my.data[,i]))),
                     Mean = c(mean(my.data[,i],na.rm = T)),
                     median = c(median(my.data[,i],na.rm = T)),
                     Min = c(min(my.data[,i],na.rm = T)),
                     Max = c(max(my.data[,i],na.rm = T))
      )
      #row bind df to result1
      result1<- rbind(result1,df)
    }else if(is.factor(my.data.col) || is.character(my.data.col)){
      #change no matter what data type inot factor
      ct<-as.factor(my.data[,i])
      #count the missing value numbers
      m<-sum(ct=="")
      #if there's missing value, remove them
      if(m!=0){
        ct<-ct[-which(ct=="")]
      }
      #count the words frequency and rank them in descending order
      d<-count(ct)
      d<-d[order(-d$freq),]
      #decide how much information to show in MVCs_count, at maxinum 3
      if(length(unique(ct))==1){
        a<-c(paste(d[1,1],"(",d[1,2],")"))
      }
      if(length(unique(ct))==2){
        a<-c(paste(d[1,1],"(",d[1,2],")"," ",
                   d[2,1],"(",d[2,2],")",
                   sep = ""))
      }
      if(length(unique(ct))>=3){
        a<-c(paste(d[1,1],"(",d[1,2],")"," ",
                   d[2,1],"(",d[2,2],")"," ",
                   d[3,1],"(",d[3,2],")",
                   sep = ""))
      }
      #create the data frame for factor variable
      df2<-data.frame(Attribute_ID = c(i),
                      Attribute_Name = c(colnames(my.data)[i]),
                      Missing = c(m),
                      Arity = c(length(unique(ct))),
                      MCVs_counts = c(a)
      )
      #row bind result to ressult 4
      result4<-rbind(result4,df2)
    }
  }
  #round the numeric numbers in to 2 digit
  result2<-result1[,1:3]
  result3<-round(result1[,4:7],2)
  #combine rounded value into the final result of numric variables information
  result<-cbind(result2,result3) 
  #print the result out
  cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
  cat("brief introduction for",my.data.name,"\n")
  cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
  cat("This dataset has",nrow(my.data),"Rows",ncol(my.data),"Attributes\n\n")
  cat("real valued atttributes\n")
  cat("-----------------------\n")
  print(result)
  cat("symbolic atttributes\n")
  cat("--------------------\n")
  print(result4)
}


#example 1: with non missing value
house_no_missing.csv<-import.csv("house_no_missing.csv")
brief(house_no_missing.csv)

#exmaple 2: with missing value
house_with_missing.csv<-import.csv("house_with_missing.csv")
brief(house_with_missing.csv)


#########################################hw1 Problem1 part b#######################################
############below are the charts in presentation###############

library(ggplot2)
df<-house_no_missing.csv

#distribution of house value - chart 1
qplot(house_value,data = df) + ggtitle("Distribution of House Value")

#simple house value distribution by river_bound - chart 2
qplot(house_value,data = df,fill = Charles_river_bound)



#variable correlation vs house value - chart 3 - chart 9
qplot(Crime_Rate,house_value,data = df,geom=c("point","smooth"),method = "lm") + ggtitle("HouseValue vs CrimeRate")
qplot(accessiblity_to_highway,house_value,data = df,geom=c("point","smooth"),method = "lm") + ggtitle("HouseValue vs AcccessHW")
qplot(num_of_rooms,house_value,data = df,geom=c("point","smooth"),method = "lm") + ggtitle("HouseValue vs RoomNumbers")
qplot(dist_to_employment_center,house_value,data = df,geom=c("point","smooth"),method = "lm") + ggtitle("HouseValue vs Disttocenter")
qplot(property_tax_rate,house_value,data = df,geom=c("point","smooth"),method = "lm") + ggtitle("HouseValue vs TaxRate")
qplot(student_teacher_ratio,house_value,data = df,geom=c("point","smooth"),method = "lm") + ggtitle("HouseValue vs STRatio")
qplot(Nitric_Oxides,house_value,data = df,geom=c("point","smooth"),method = "lm") + ggtitle("HouseValue vs NtricOxides")



#variable correlation vs house value BY River Bound  - slides 5 - chart 10 - chart 12
qplot(Crime_Rate,house_value,data = df,geom=c("point","smooth"),method = "lm",facets = .~Charles_river_bound) + ggtitle("HouseValue vs CrimeRate")
qplot(num_of_rooms,house_value,data = df,geom=c("point","smooth"),method = "lm",facets = .~Charles_river_bound) + ggtitle("HouseValue vs RoomNumbers")
qplot(dist_to_employment_center,house_value,data = df,geom=c("point","smooth"),method = "lm",facets = .~Charles_river_bound) + ggtitle("HouseValue vs Disttocenter")
qplot(property_tax_rate,house_value,data = df,geom=c("point","smooth"),method = "lm",facets = .~Charles_river_bound) + ggtitle("HouseValue vs TaxRate")
qplot(student_teacher_ratio,house_value,data = df,geom=c("point","smooth"),method = "lm",facets = .~Charles_river_bound) + ggtitle("HouseValue vs STRatio")
qplot(accessiblity_to_highway,house_value,data = df,geom=c("point","smooth"),method = "lm",facets = .~Charles_river_bound) + ggtitle("HouseValue vs AcccessHW")
qplot(Nitric_Oxides,house_value,data = df,geom=c("point","smooth"),method = "lm",facets = .~Charles_river_bound) + ggtitle("HouseValue vs NtricOxides")


#########################################hw1 Problem2 part a#######################################

# You need to install package 'FNN'
library("FNN")
library("gplots")
library("ggplot2")
library("FNN")
library("plyr")

# utility function for import from csv file
import.csv <- function(filename) {
  return(read.csv(filename, sep = ",", header = TRUE))
}

# utility function for export to csv file
write.csv <- function(ob, filename) {
  write.table(ob, filename, quote = FALSE, sep = ",", row.names = FALSE)
}

# Connect-the-dots model that learns from train set and is being tested using test set
# Assumes the last column of data is the output dimension
get_pred_dots <- function(train,test){
  nf <- ncol(train)
  input <- train[,-nf]
  query <- test[,-nf]
  my.knn <- get.knnx(input,query,k=2) # Get two nearest neighbors
  nn.index <- my.knn$nn.index
  pred <- rep(NA,nrow(test))
  for (ii in 1:nrow(test)){
    y1 <- train[nn.index[ii,1],nf]
    y2 <- train[nn.index[ii,2],nf]
    pred[ii] = (y1+y2)/2
  }
  return(pred)  
}

# Linear model
# Assumes the last column of data is the output dimension
# note: shoud the test data has the las column as output value, need to delete if before
# using this function
get_pred_lr <- function(train,test){
  nf <- ncol(train)
  y<-train[,nf]
  lmodel<-lm(y~.,train)
  pred<-predict(lmodel,test)
  return(pred)
}

# Default predictor model : average training data
# Assumes the last column of data is the output dimension
get_pred_default <- function(train,test){
  nf<-ncol(train)
  p<-mean(train[,nf])
  pred<-c(rep(p,nrow(test)))
  return(pred)
}

# do a cross validation
do_cv<-function(df,output,k,model){
  #put the output variable to the last column of the data frame
  i<-which(colnames(df)== output)
  df1<-df[,-i]
  df2<-df[,i]
  df<-cbind(df1,df2) 
  # store the final result
  result<-c()
  # prepare for the random test data chosene
  nr<-nrow(df)
  base.index<-c(1:nr)
  # here come's the cross validation
  for(i in 1:k){
    #random select K times as test data and the rest as train data
    testrow<-sample(base.index, size = round(nr/k,0))
    base.index<-base.index[-testrow]
    test<-as.data.frame(df[testrow,])
    train<-as.data.frame(df[-testrow,])
    
    #chech which model is called
    if(model == 'get_pred_default'){
      predict<-get_pred_default(train,test)
    }
    if(model == 'get_pred_lr'){
      predict<- get_pred_lr(train,test)
    }
    if(model == 'get_pred_dots'){
      predict<- get_pred_dots(train,test)
    }
    mse<-mean((test[,ncol(test)]-predict)^2)
    result[i]<-mse
  }
  return(result)
}


#########################################hw1 Problem2 part b#######################################

########################################################################################
#Problem 2: B) i ) Applying  leave-one-out cross-validation for the three different model
#########################################################################################
cat("Problem2 b i)\n")
#construct a dataframe with log(crime_rate) and house value only
df<-import.csv("house_no_missing.csv")
df1<-log(df$Crime_Rate)
df2<-df$house_value
df.final<-cbind(df1,df2)

#random shuffle the dataset
df.final<-df.final[sample(nrow(df.final)),]

#pass it to all three 
default.result<-do_cv(df.final,"df2",nrow(df.final),"get_pred_default")
linear.result<-do_cv(df.final,"df2",nrow(df.final),"get_pred_lr")
dots.result<-do_cv(df.final,"df2",nrow(df.final),"get_pred_dots")

########################################################################################
#Problem 2: B) ii ) Compute 95% confidence intervals of each score
#########################################################################################
cat("Problem2 b ii)\n")
#create a data frame by model and their MSE
df1<-data.frame(Model = "default", MSE.Result = default.result)
df2<-data.frame(Model = "linear", MSE.Result = linear.result)
df3<-data.frame(Model = "dots",MSE.Result = dots.result)
total.result <- rbind(df1,df2,df3)

#compute 95% of interval of each score
CI<-function(vector){
  lower<-mean(vector)-qnorm(.95)*sd(vector)/sqrt(length(vector))
  upper<-mean(vector)+qnorm(.95)*sd(vector)/sqrt(length(vector))  
  ci<-c(lower,upper)
  return(ci)
}

#combine MEAN MSE and CI by model into a data frame
mean.result.byModel<-ddply(total.result,"Model",summarize, MeanMSE = mean(MSE.Result),
                           Upper = CI(MSE.Result)[2],
                           Lower = CI(MSE.Result)[1])


# The result and the 95% of confidence level, chart
ggplot(mean.result.byModel,aes(Model,MeanMSE)) + 
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymax = Upper,ymin = Lower),
                position = position_dodge(0.9),
                data = mean.result.byModel)+
  ggtitle("Three models meanMSE and 95% Confidence Interval")

#it turns out the linear predicted relatively good so will use linear to build the model
finalmodel<-lm(df2~df1,data.frame(df.final))
#draw the final model out
ggplot(data.frame(df.final),aes(df1,df2)) + geom_point() + 
  geom_abline(intercept = finalmodel$coefficients[1], 
              slope = finalmodel$coefficients[2],
              colour="#0072B2",size = 1.8)+
  ggtitle("Model vs Actual house value")+labs(x = "log(crime_rate)") + labs(y = "housevalue")
  

########################################################################################
#Problem 2: B) iii ) Present, interpret, and discuss the results in your presentation slide
#########################################################################################
cat("Problem2 b iii)\nPlease find from slides")









