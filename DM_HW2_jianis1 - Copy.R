library("e1071")
library("class")
library("ggplot2")
library("dplyr")

#HW2 - DATA MINING
#Author : Jiani Shen (AndrewID : jianis1)

# utility function for import from csv file
import.csv <- function(filename) {
  return(read.csv(filename, sep = ",", header = TRUE))
}

# utility function for export to csv file
write.csv <- function(ob, filename) {
  write.table(ob, filename, quote = FALSE, sep = ",", row.names = FALSE)
}

#=======================
#Part 1 with R functions
#=======================

#logistic regression
#Assume the last column of dat is the output dimension, assume last column is {1,0}
#will assign cut of later, here with no cut off
get_pred_logreg<-function(train,test){
  y<-train[,ncol(train)]
  x<-train[,-ncol(train)]
  my.logit<-glm(y~., family = binomial(link = "logit"), data = data.frame(x,y))
  pred<-predict(my.logit,test,type = "response")
  #pred<-ifelse(pred>0.5,1,0)
  result<-data.frame(prediction = pred,actual = test[,ncol(test)])
  return(result)
}

#SVM
#Assume the last column of dat is the output dimension, assume last column is {1,0}
#why no matter type = response or not , the value is not 100% within [0,1]
#will assign cut of later, here with no cut off
get_pred_svm<-function(train,test){
  y<-train[,ncol(train)]
  x<-train[,-ncol(train)]
  my.svm<-svm(x,y)
  pred<-predict(my.svm,test[,-ncol(test)], type = "response")
  #pred<-ifelse(pred>0.5,1,0)
  result<-data.frame(prediction = pred,actual = test[,ncol(test)])
}

#Naive Bayes
#Assume the last column of dat is the output dimension, assume last column is {1,0}
#will assign cut of later, here with no cut off
get_pred_nb<-function(train,test){
  y<-train[,ncol(train)]
  x<-train[,-ncol(train)]
  my.nb<-naiveBayes(y~., data = data.frame(x,y))
  pred<-predict(my.nb,test[,-ncol(test)],type = "raw")
  #pred1<-ifelse(pred[,2]>0.5,1,0)
  pred1<-pred[,2]
  result<-data.frame(prediction = pred1,actual = test[,ncol(test)])
}

#KNN
#Assume the last column of dat is the output dimension, assume last column is {1,0}
get_pred_knn<-function(train,test,k){
  label<-train[,ncol(train)]
  test1<-test[,-ncol(test)]
  train1<-train[,-ncol(train)]
  my.knn<-knn(train1,test1,label,k)
  #check if the prediction output should be numeric or symbolic
  if(is.numeric(label)){
    pred<-as.numeric(as.character(my.knn)) 
  }
  if(!is.numeric(label)){
    pred<-my.knn
  }
  result<-data.frame(prediction = pred,actual = test[,ncol(test)])
}


#=======================
#Part 2 with CV
#=======================
df<-ele.whole.shuffle
num_folds<-10
model_name<-"get_pred_logreg"

#model name = svm,nb,knn,logreg
do_cv_class<-function(df,num_folds,model_name){
  #store k fold output result
  final.result<-data.frame()
  # prepare for the random test data chosene
  nr<-nrow(df)
  eachtime<-round(nr/num_folds,0)
  a<-1
  b<-eachtime
  
  # here come's the cross validation
  for(i in 1:num_folds){
    #random select num_folds times as test data and the rest as train data
    
    if(i != num_folds){
      test<-as.data.frame(df[a:b,])
      train<-as.data.frame(df[-a:-b,])
      a<-a+eachtime
      b<-b+eachtime
    }
    if(i == num_folds){
      test<-as.data.frame(df[a:nr,])
      train<-as.data.frame(df[-a:-nr,])
    }
    
    name<-as.character(model_name)
    #chech which model is called
    if(grepl(name,"get_pred_logreg")){
      result<-get_pred_logreg(train,test)
    }
    if(grepl(name,"get_pred_svm")){
      result<- get_pred_svm(train,test)
    }
    if(grepl(name,"get_pred_nb")){
      result<- get_pred_nb(train,test)
    }
    if(grepl(substring(name,nchar(name)-1,nchar(name)),"get_pred_knn")){
      k<-as.numeric(substring(name,1,nchar(name)-2))
      result<- get_pred_knn(train,test,k)
    }
    final.result<-rbind(final.result,result)
  }
  return(final.result)
}


#=======================
#Part 3 with metrics
#=======================


get_metrics<-function(df,cutoff){
  df[,1]<-ifelse(df[,1]>cutoff,1,0)
  df[,1]<-as.numeric(df[,1])
  df[,2]<-as.numeric(df[,2])
  df$sum<-df[,1] + df[,2]
  df$minus<-df[,1]-df[,2]
  nrecords<-nrow(df)
  #predict as positive
  Ppos<-sum(df[,1])
  Pneg<-length(df$prediction[which(df$prediction == 0)])
  #true positive
  pos<-sum(df[,2])
  neg<-length(df$actual[which(df$actual == 0)])
  #where predict = 1, and actual = 1, their sum  = 2, count those true positive
  tpr<-length(df$sum[which(df$sum == 2)])/pos
  #where predict = 1, and actual = 0, their different  = -1, count those false positive
  fpr<-length(df$minus[which(df$minus == -1)])/neg
  #acc
  acc<-length(df$minus[which(df$minus == 0)])/nrecords
  #precision
  precision<-length(df$sum[which(df$sum == 2)])/Ppos
  #recall
  recall<-length(df$sum[which(df$sum == 2)])/pos
  #final output
  output<-data.frame(tpr = tpr,fpr = fpr,acc = acc,precision = precision,recall = recall)
  return(output)
}



#=======================
#Part 4 Output with wine.data
#=======================

wine.data<-import.csv("wine.csv")

#random shuffle the dataset
wine.data<-wine.data[sample(nrow(wine.data)),]

#change the class int to 1, 0, "high"-1,"low"-0
levels(wine.data$type)[1]<-1
levels(wine.data$type)[2]<-0
wine.data$type <- as.numeric(as.character(wine.data$type))

#question a
#create a data frame to store result
knn.test<-data.frame()
#test k from k = 1 to k = 100
for(i in 1 : 100){
  knn<-paste0(i,"nn")
  cv.r<-do_cv_class(wine.data,10,knn)
  r.r<-get_metrics(cv.r,0.5)
  r1<-data.frame(num = i, acc = r.r$acc)
  knn.test<-rbind(knn.test,r1)
}
knn.test

#find out k number when it has the highest accuracy
ideak<-knn.test[which(knn.test$acc == max(knn.test$acc)),]

#question a slides
knn.graph.smooth <- ggplot(knn.test,mapping = aes( x = num, y= acc))+
stat_smooth(method = "lm", formula = y ~ poly(x, 8))+ geom_line(size = 2)+
  scale_x_continuous(breaks = seq(0,100,5))+
  labs(x = "Number of Nearest Neighbours",y = "Accuracy", title = "Generalization vs number of Neighbours")

#question b
#test all the three parametric models, the defualt cutoff is 0.5, using 10 cross valiation
logreg<-do_cv_class(wine.data,10,"logreg")
logreg.result<-get_metrics(logreg,0.5)

nb<-do_cv_class(wine.data,10,"nb")
nb.result<-get_metrics(nb,0.5)

svm<-do_cv_class(wine.data,10,"svm")
svm.result<-get_metrics(svm,0.5)

result.combine<-rbind(logreg.result,nb.result,svm.result)
result.combine<-cbind(Model = c("logreg","nb","svm"),result.combine)
result.combine
#the defualt class prediction
defualt.acc<-max(table(wine.data$type)/nrow(wine.data))


#question c
#combine all the results from diffrent model into a dataframe
logreg<-do_cv_class(wine.data,10,"logreg")
logreg.result<-get_metrics(logreg,0.5)

nb<-do_cv_class(wine.data,10,"nb")
nb.result<-get_metrics(nb,0.5)

svm<-do_cv_class(wine.data,10,"svm")
svm.result<-get_metrics(svm,0.5)

sixnn<-do_cv_class(wine.data,10,"6nn")
sixnn.result<-get_metrics(sixnn,0.5)

result.combine<-rbind(logreg.result,nb.result,svm.result,sixnn.result)
result.combine<-cbind(Model = c("logreg","nb","svm","6nn"),result.combine)
result.combine

##result is sligtly different on accuracy as it was randomized
#follow the 95% CI 
#n stands for data points
CI_class<-function(accuracy,n){
  lower<- (accuracy + (1.96)*(1.96)/n - 1.96*sqrt(accuracy/n-(accuracy)^2/n+(accuracy)^2/4/((n)^2)))/(1+(accuracy)^2/n)
  upper<- (accuracy + (1.96)*(1.96)/n + 1.96*sqrt(accuracy/n-(accuracy)^2/n+(accuracy)^2/4/((n)^2)))/(1+(accuracy)^2/n)
  ci<-c(lower,upper)
  return(ci)
}

#combine all the result , the accuracy with 95% standard deviation

sixnn.acc<-result.combine$acc[which(result.combine$Model == "6nn")]
logreg.acc<-result.combine$acc[which(result.combine$Model == "logreg")]
svm.acc<-result.combine$acc[which(result.combine$Model == "svm")]
nb.acc<-result.combine$acc[which(result.combine$Model == "nb")]
defualt.acc<-max(table(wine.data$type))/nrow(wine.data)

six.ci<-data.frame(Model = "6nn", ACC = sixnn.acc, 
          Lower = CI_class(sixnn.acc,nrow(wine.data))[1],
          Upper = CI_class(sixnn.acc,nrow(wine.data))[2])

log.ci<-data.frame(Model = "logreg", ACC = logreg.acc, 
          Lower = CI_class(logreg.acc,nrow(wine.data))[1],
          Upper = CI_class(logreg.acc,nrow(wine.data))[2])

svm.ci<-data.frame(Model = "svm", ACC = svm.acc, 
          Lower = CI_class(svm.acc,nrow(wine.data))[1],
          Upper = CI_class(svm.acc,nrow(wine.data))[2])

nb.ci<-data.frame(Model = "nb", ACC = nb.acc, 
          Lower = CI_class(nb.acc,nrow(wine.data))[1],
          Upper = CI_class(nb.acc,nrow(wine.data))[2])
def.ci<-data.frame(Model = "default", ACC = defualt.acc, 
          Lower = CI_class(defualt.acc,nrow(wine.data))[1],
          Upper = CI_class(defualt.acc,nrow(wine.data))[2])

model.accuracy<-rbind(six.ci,log.ci,svm.ci,nb.ci,def.ci)

#final model comparison slides
ggplot(model.accuracy,aes(x = Model,ymin = Lower,ymax = Upper))+
  geom_errorbar(width = 0.3) + geom_point( mapping = aes(y = ACC),size = 4)+
  labs( x = "model", y = "Accuracy with CI", title = "Model Performance")

