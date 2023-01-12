df<- data(iris)

head(iris)
set.seed(42)
ran <- sample(1:nrow(iris), 0.9 * nrow(iris)) 

iris_train <- iris[ran,] 

y_train <- iris_train[,5]
x_train <- iris_train[,-5]


iris_test <- iris[-ran,] 
y_test <- iris_test[,5]
x_test <- iris_test[,-5]


library(class)

pr<- knn(train = x_train, test = x_test, cl = y_train,k = 20)

table_contingence<-table(pr, y_test)
accuracy<- function(table_contingence){
round(sum(diag(table_contingence))/
sum(table_contingence)*100,1)
}

accuracy(table(pr, y_test))

res_k<-res_acc<-c()

for(k in c(1:20)){
  
  pr<- knn(train = x_train, test = x_test, cl = y_train,k = k)
  
  res_k<-c(res_k, k)
  
  res_acc<-c(res_acc, accuracy(table(pr, y_test)))
  
}

set.seed(42)
quantiles<- quantile(1:nrow(iris), c(0,1:10/10))
samples<- sample(1:nrow(iris), nrow(iris))

res_k_global<- res_acc_global<-c()

for(sample_i in 1:10){
  x_test_id<- samples[c(quantiles[sample_i]:quantiles[sample_i+1])] 
  iris_train <- iris[-x_test_id,] 
  y_train <- iris_train[,5]
  x_train <- iris_train[,-5]
  iris_test <- iris[x_test_id,] 
  y_test <- iris_test[,5]
  x_test <- iris_test[,-5]
  
  res_k<-res_acc<-c()
  for(k in c(1:20)){
    
    pr<- knn(train = x_train, test = x_test, cl = y_train,k = k)
    
    res_k<-c(res_k, k)
    
    res_acc<-c(res_acc, accuracy(table(pr, y_test)))
    
  }
  res_k_global<- c(res_k_global,res_k )
  res_acc_global <- c(res_acc_global,res_acc )
}

  res_tot<- data.frame(k= res_k_global, acc=res_acc_global)
library(dplyr)
res_cv<-res_tot%>%group_by(k)%>%summarise(acc_mean=mean(acc))

res_cv%>%ggplot(aes(x = k, y = acc_mean))+geom_point()


folds <- cut(seq(1,nrow(iris)),breaks=10,labels=FALSE)
folds
x_train[!folds==i,]
x_test[folds==i,]