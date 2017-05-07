#case challenge
#@Jiani Shen
#May-06-2017

#Y: Sample * 1: Price[0,1] represent if the sample pay $1 or not
#X: Sample * Document[0,1] represent if the sample read the document in the specific period(first 30 days)
 
#Set equation Y = XB. 
#B[document * 1: price] represent how much each sample willing to pay per document
 
#To solve the equation we need to minimize
#Model error: (Y-XB)^2
#L2 penalty: ||B|| = sqrt(sum(Bi^2)
#Also for the actual case, Yi >= 0
 
#It comes to be a prox-prox problem and we can have:
#Y = positive(inverse(X’X) * X’A – lambda), where if x>0, positive(x) = x else positive(x) = 0




##########################
#Part 1
##########################
rm(list = ls())

user<-read.csv('Data_challenge_conversions_v2.csv')
doc<-read.csv('Data_challenge_original_documents.csv')
tra<-read.csv('Data_challenge_traffic_v2.csv')

colnames(user)[6]<-'session_id'
tra<-tra[,1:3]

#covert date 
tra$access_date<-as.Date(as.character(tra$access_date))
user$creation_date<-as.Date(as.character(user$creation_date))

#JOin the traffic and con to see user and doc visit situation, outer JOIN , 110297 for those without USER ID but with sessionid
user_doc<-merge(tra,user,by = 'session_id',all = TRUE)
#remove those without user_id since only want to study user vs doc
user_doc<-user_doc[which(is.na(user_doc$user_id) == FALSE),]
user_doc$checkday<-as.numeric(user_doc$access_date - user_doc$creation_date) #average 4.87

#those who upload their document may access their own document so need to exclude those
user_sub<-user[which(as.character(user$Uploaded.Content.ID)!= '--'),]
user_upload<-paste0(user_sub$user_id,user_sub$Uploaded.Content.ID,'_')

user_doc_sub<-user_doc[which(!paste0(user_doc$user_id,user_doc$Uploaded.Content.ID.x,'_') %in% user_upload),] #all false, good


#assumption 1 : only 10days before and 10days after conversiondate is useful
#assumption 2: those who didn't pay bring 0 value to business revenue
user_doc_sub<-user_doc_sub[which(user_doc_sub$checkday >= -10 & user_doc_sub$checkday <= 10),]  #from 10247 to 10012


#input and convert into binary to avoid unfair to , checked there's no sum = 0 columns
x<-table(user_doc_sub$user_id,user_doc_sub$Uploaded.Content.ID.x)
x1<-as.matrix((x>0)+0)  #binary
x<-data.frame(user_id = rownames(x1),x1[,1:ncol(x1)])
colnames(x)[2:ncol(x)]<-colnames(x1)
x$user_id<-as.character(x$user_id)

#output y
y<-data.frame(user_id = user$user_id, price = user$price)
levels(y$price)[1]<-0
levels(y$price)[2]<-1
y$price<-as.numeric(as.character(y$price))


#dataframe with input and output, inner join
df<-merge(x,y, by ='user_id')
df<-df[,-1] #discard user_id


#model
my.model<-lm(df$price~., data = df)
co.model<-coef(my.model)
co.model[co.model<0]<-0
co.model[is.na(co.model)]<-0
col.f<-apply(df,2,sum)
col.f<-col.f[1:(length(col.f)-1)] #remove price column sum
coeff<-co.model[-1]
v<-round(coeff*col.f,1)

#10 folds cross validation to see model performance
#shuffle df
my.df<-df[sample(nrow(df)),]
size<-nrow(df)/10
pool<-seq(1:nrow(df))
result.mse<-c() #store 10 cv result mse


# here come's the cross validation
for(k in 1:10) {
  if(k != 10){
    sample.id<-sample(pool,size = size, replace = FALSE)
    pool<-pool[-sample.id]
  }
  if(k == 10){
    sample.id<-pool #when k = 10, sample the rest of pool
  }
  
  #testset and trainset
  test<-df[sample.id,]
  train<-df[-sample.id,]
  
  train.m<-lm(train$price~.,data = train)
  actual<-test$price
  test<-test[,-ncol(test)]
  predict.m<-predict(train.m, test) #train model on train, test model on test
  
  #train.m<-glm(train$price~.,family=binomial(link='logit'),data = train) tried logistic regression
  #predict.m<-predict(train.m,test)
  
  mse<-mean((actual-predict.m)^2)
  #R2 <- 1 - (sum((actual-predict.m)^2)/sum((actual-mean(actual))^2))
  result.mse[k]<-mse
}

result.lm<-result.mse
sqrt(mean(result.lm))

#below is double check
sum(v,na.rm = TRUE)# sum total = 6412
#length(df)  #1986 documents in model
#sum(as.numeric(v == 0), na.rm = TRUE) #= 451, # 451 doc regards as no value

#all together 6412 docs
#doc that has been visited 5284
#doc that has been visited near user conversation date, which convert visitor to user is 1986


############################
#PART 2
############################
doc.val<-as.data.frame(v)
doc.val<-data.frame(Content_id = rownames(doc.val), value = doc.val$v)
doc.val$db_filename<-gsub('`*`',"",doc.val$Content_id)

#match , left join
doc_val_cos<-merge(doc,doc.val, by = 'db_filename', all.x= TRUE)  #part of the contentid cannot be matched(eg, 8615840), for traffic table same happen
doc_val_cos<-doc_val_cos[,-5]
doc_val_cos$value[is.na(doc_val_cos$value)]<-0
doc_val_cos$cost<-0
doc_val_cos$cost[grep('Business',doc_val_cos$course_type)]<-0.51
doc_val_cos$cost[grep('Stem',doc_val_cos$course_type)]<-0.45
doc_val_cos$cost[grep('Other',doc_val_cos$course_type)]<-0.62

re.agg<-aggregate(doc_val_cos$value, by = list(Category = doc_val_cos$course_type), sum)
co.agg<-aggregate(doc_val_cos$cost, by = list(Category = doc_val_cos$course_type),sum)
fre.agg<-table(doc_val_cos$course_type)

cate.agg<-data.frame(category = re.agg$Category, 
                     freq = head(fre.agg),
                     revenue = re.agg$x, 
                     cost = round(co.agg$x,0), 
                     ROI = round(re.agg$x/co.agg$x,2))
cate.agg<-cate.agg[,-2]

#1. more business oriented
#2. 


#what kind of document user more interesed in?

head(user_doc)
k<-user_doc[which(user_doc$checkday>=0),]
d<-doc
colnames(d)[1]<-'Uploaded.Content.ID.x'
k<-merge(k,d,by = 'Uploaded.Content.ID.x')

table(k$course_type)

unpay<-k[which(as.character(k$price) == '--'),]
table(unpay$course_type)

pay<-k[which(!as.character(k$price) == '--'),]
table(pay$course_type)

