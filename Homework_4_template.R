################################
#HW4 Jiani Shen jianis1
######################


library(arules)
library(rpart)
library(ROCR)
library(party)

import.csv <- function(filename) {
  return(read.csv(filename, sep = ",", header = TRUE))
}

write.csv <- function(ob, filename) {
  write.table(ob, filename, quote = FALSE, sep = ",", row.names = FALSE)
}

my.data <- import.csv('cars.csv')

#######################################################
# Problem 1
# Entropy, Information gain, Feature selection
#######################################################

# Compute entropy for a vector of symbolic values
# Inputs: Vector of symbolic values
# Output: Entropy (in bits)
entropy <- function(x) {
  #@@@@@@@@ Your function goes here @@@@@@@@@@@@@@@@
  f<-as.data.frame(table(x))
  f$prior<-f[,2]/sum(f[,2])
  f$IC<- -log2(f[,3])
  f$e<- f$prior*f$IC
  entr<-sum(f$e)
  return(entr)
}

# Unit test for function entropy
x <- c(rep('A', 3),rep('B', 2),rep('C', 5))
print(ifelse(abs(entropy(x) - 1.485475 ) < 1.0e-05, 'entropy function has passed this test!', 'entropy function has failed this test'))


# Compute information gain IG(x,y)
# Inputs: x, y: vectors of symbolic values of equal length
# Output: information gain IG(x,y)
info.gain <- function(x,y){
  #@@@@@@@@@@@@@@@Your function goes here@@@@@@@@
  df<-as.data.frame(cbind(x,y))
  df<-df[order(x),]
  
  #get X class value and it's P(x = Xj)
  f<-as.data.frame(table(df[,1]))
  f$prior<-f[,2]/sum(f[,2])
  
  #split according X's class value
  split.df<-split(df,df$x)
  
  #a vector to store the H(y|x=xj)
  f$en<-c()
  
  for(i in 1:length(split.df)){
    y.s<-as.character(split.df[[i]][,2])
    e<-entropy(y.s)
    f$en[i]<-e
  }
  #H(y|x=xj) column
  f$H<-f$prior*f$en
  #H(y|x)
  H.y.x<-sum(f$H)
  H.y<-entropy(y)
  info.gain<-H.y-H.y.x
  return(info.gain)
}


# Unit test for function info.gain
x <- c(rep('A',3),rep('B',2),rep('C',5))
y <- c(rep('X',4),rep('Y',6))
print(ifelse(abs(info.gain(x,y) - 0.7709506 ) < 1.0e-05, 'Info.gain function has passed this test!', 'info.gain function has failed this test'))

# Information-gain-based feature selection: exhaustive search
# Input: df is a data frame with last column being the output attribute
#        m: size of feature set, default is 1
# Output: data frame with name(s) of selected feature(s), information gain, relative information gain, sorted by the value of information gain
features <- function(df, m = 1){
  nf <- ncol(df) -1 # number of input features
  idx <- 1: nf  # column indices of input features
  output <- df[, ncol(df)]  # output column
  outputH <- entropy(output) # entropy for output
  idx.list <- combn(idx, m) # matrix storing all combinations of size m from idx
  IG.res <-NULL # output data frame
  # iterate through all combinations of index 
  for (ii in 1:ncol(idx.list)){
    this.idx <- idx.list[, ii]  
    input.df <- data.frame(df[,this.idx]) 
    # create a vector where each element is a concatenation of all values of a row of a data frame
    this.input <- apply(input.df, 1, paste, collapse='') 
    # create a new feature name which is a concatenation of feature names in a feature set
    this.input.names <- paste(names(df)[this.idx], collapse=' ')    
    this.IG <-info.gain(this.input,output) # information gain
    this.RIG <- this.IG / outputH # relative information gain
    this.res <- data.frame(feature = this.input.names, IG = this.IG, RIG = this.RIG) #assemble a df
    IG.res <- rbind(IG.res, this.res) # concatenate the results    
  }
  sorted <- IG.res[order(-IG.res$IG), ] # sort the result by information gain in a descending order
  return (sorted)
}


#problem1 a
car.class.entropy<-entropy(my.data$class)
car.class.entropy

#problem1 b
ig.class.price<-info.gain(my.data$class,my.data$price)
ig.class.price
#relative information gain
ig.price.class<- ig.class.price/car.class.entropy
ig.price.class

#problem1 c

#put class as the last column
class<-my.data$class
df<-my.data[,-5]
df<-data.frame(df,class = class)

#different size of features
size.set1<-features(df,1)
size.set2<-features(df,2)
size.set3<-features(df,3)

size.set1
size.set2
size.set3

#plot bar chart 1 with ig
#http://stackoverflow.com/questions/16121903/r-barplot-y-axis-scale-too-short
barplot(height = size.set1[,2], names.arg = size.set1[,1],
        ylim=c(0,0.3),
        col = rainbow(20),
        main="Class vs Feature Information Gain", 
        xlab="Feature Name",
        ylab="Information Gain")

#plot bar chart 2 with Rig
barplot(height = size.set1[,3], names.arg = size.set1[,1],
        ylim=c(0,0.3),
        col = rainbow(20),
        main="Class vs Feature Relative Information Gain", 
        xlab="Feature Name",
        ylab="Relative Information Gain")




##################
# Problem 2
# Association Rules
#################

#a.
my.data.rl<-apriori(my.data, 
                    parameter = list(support = 0.1 , confidence = 0.5, minlen = 2),
                    appearance = list(rhs = c("price=high"),default = "lhs")
)

#number of rules
nrow(my.data.rl@quality)


#top 5 rules sorted by support
inspect(head(my.data.rl,5,by = "support"))

#top 5 rules sorted by 
inspect(head(my.data.rl,5,by = "confidence"))


#b.
# Inputs: rules is the object returned by the apriori function
#         df is a data frame from which the rules are learned
# Output: a rule object with extra metrics (95% CI of score)
expand_rule_metrics <- function(rules,df){
  rule.df <- interestMeasure(rules, c('support', 'confidence'), df) # extract metrics into a data frame
  nn <- nrow(df)
  ci.low <-  rule.df$confidence - 1.96*sqrt(rule.df$confidence*(1-rule.df$confidence))/sqrt(nn*rule.df$support)
  ci.high <- rule.df$confidence + 1.96*sqrt(rule.df$confidence*(1-rule.df$confidence))/sqrt(nn*rule.df$support)
  quality(rules) <-cbind(quality(rules), ci.low, ci.high) # update the quality slot of the rules object
  return(rules)
}

#c.
my.data.rl.expand<-expand_rule_metrics(my.data.rl,my.data)
my.ci.quality<-my.data.rl.expand@quality
my.rl.result<-cbind(inspect(my.data.rl),my.ci.quality[,4:5] )

#sort by lower CI DESC
my.rl.result<-my.rl.result[order(-my.rl.result$ci.low),]
my.rl.result

##################
# Problem 3
# Build a tree
#################

# grow tree 
fit <- rpart(price ~ .,method="class", data=my.data)
# prune the tree
pfit<- prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
#score the prediction
pred.tree1<-predict(pfit, my.data[,-ncol(my.data)])
tree1.score<-prediction(pred.tree1[,2], my.data$price)

#get AUC from tree1
auc.tree1<-performance(tree1.score,"auc")
auc.tree1@y.values

# plot tree
par(mar = c(7,3,3,5))
plot(pfit, uniform=TRUE, 
     main="Classification Tree for rpart Tree")
text(fit, use.n=TRUE, all=TRUE, cex=.8, pretty = TRUE)


#grow another tree
fit2<-ctree(price ~., data = my.data)
#score teh prediction
pred.tree2<-predict(fit2, my.data[,-ncol(my.data)], type = "prob", simply = FALSE)
#get the prediction value
pred.value<-sapply(pred.tree2, '[[',2)
tree2.score<-prediction(pred.value, my.data$price)

#get AUC from tree1
auc.tree2<-performance(tree2.score,"auc")
auc.tree2@y.values

# plot ctree 
plot(fit2,main="Classification Tree for ctree Tree")
