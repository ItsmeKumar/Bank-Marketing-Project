setwd("~/Downloads/ML/bank-additional")
bank <- read.table("bank-additional-full.csv",header=TRUE,sep=";") 
str(bank)

set.seed(121)
train_rows <- sample.int(n=nrow(bank),size = floor(0.70*nrow(bank)), replace= F)
train <- bank[train_rows, ]
test <- bank[-train_rows, ]

library(ggplot2)

ggplot(data=bank, aes(x=age))+
    geom_histogram()

ggplot(data=bank, aes(x=duration))+
    geom_histogram(bins=10)

ggplot(data=bank, aes(x=duration))+
    geom_bar(bins=10)

boxplot(duration ~ y, data = bank)

bartable = table(bank$nr.employed, bank$y)
barplot(bartable, beside = TRUE, legend = levels(unique(bank$nr.employed)))
barplot(prop.table(bartable,2)*100, legend = levels(unique(bank$nr.employed)))

plot(table(bank$pdays, bank$y))

plot(bank$previous, col=bank$y)
ggplot(data= bank, aes(previous,y))+geom_point()
boxplot(euribor3m ~ y, data = bank)


boxplot(cons.price.idx ~ y, data = bank)
factors_index <- which(sapply(bank, is.factor))
factors_index
lev <- lapply(factors_index, function(fac) {levels(bank[,fac])} )
lev
bank[,factors_index] <- lapply(factors_index, function(fac) {as.numeric(bank[,fac])})
str(bank)

methods <-  c("pearson","spearman")
cors <- lapply(methods, function(m) { cor(bank, method=m)})
str(cors)

library("lattice")
plot_cors <- function(x, labs) {
  plot(levelplot(x, main=paste(labs), scales = list(x=list(rot=90),cex=1.0, xlim=c(0,1),ylim=c(0,1))))
}

Map(plot_cors, cors, methods)

library(corrplot)
corrplot(cor(bank, method="pearson"))


nam <- names((bank[sapply(bank, is.factor)]))
nam
lapply(nam, function(nam){ levels(bank$nam)})
library('gmodels')
joint <- CrossTable(bank$job, bank$y, chisq=T)



boxplot(bank$duration)

boxplot(bank$pdays)

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

factors_index <- c()
j <- 1
for(i in 1:ncol(bank)) {
    if (is.factor(bank[,i])) {
        factors_index[j] <- i
        j <- j+1
    }
}
factors_index

bank_n <- as.data.frame(lapply(bank[ ,-factors_index], normalize))
names(bank_n)
bank[names(bank_n)] <- bank_n[names(bank_n)]
str(bank)
corrplot(cor(bank, method="pearson"))

library(irr)
library(ROCR)
library(caret)
train_ind <- createDataPartition(bank$y, p=0.75, list=FALSE)
train_data <- bank[train_ind, ]
test_data <- bank[-train_ind, ]
prop.table(table(train_data$y))
prop.table(table(test_data$y))


library(C50)
decision_tree <- C5.0(train_data[-21], train_data$y, trails=20)
tree_pred <- predict(decision_tree, test_data[-21])
library(gmodels)
CrossTable(tree_pred, test_data$y)
kappa2(data.frame(tree_pred, test_data$y))$value
confusionMatrix(tree_pred,test_data$y, positive = "yes")
pred_dt <- prediction(predictions = as.numeric(tree_pred), labels = as.numeric(test_data$y))
perf_dt <- performance(pred_dt,measure = "tpr", x.measure = "fpr")
plot(perf_dt, main="decision tree")


for(x in colnames(bank)) {
    y <- paste(y,x,"+")
}
y
library(nnet)
nnet_model <- nnet(y~age + job + marital + education + 
                       default + housing + loan + contact + 
                       month + day_of_week + duration + campaign + 
                       pdays + previous + poutcome + emp.var.rate + 
                       cons.price.idx + cons.conf.idx + euribor3m + 
                       nr.employed, data=train_data, size=17, 
                   maxit = 200)
nnet_pred <- predict(nnet_model, test_data[-21], type="class")
CrossTable(nnet_pred, test_data$y)
kappa2(data.frame(nnet_pred, test_data$y))$value
confusionMatrix(nnet_pred,test_data$y, positive = "yes")
str(nnet_pred)
nnet_pred_fac <- as.factor(nnet_pred)
str(nnet_pred_fac)
pred_nn <- prediction(predictions = as.numeric(nnet_pred_fac), labels = as.numeric(test_data$y))
perf_nn <- performance(pred_nn,measure = "tpr", x.measure = "fpr")
plot(perf_nn, main="neural net")

library(kernlab)
svm_model <- ksvm(y~., data=train_data, kernel = "rbfdot")
summary(svm_model)
svm_pred <- predict(svm_model, test_data[-21])
str(svm_pred)
CrossTable(svm_pred, test_data$y)
kappa2(data.frame(svm_pred, test_data$y))$value
confusionMatrix(svm_pred,test_data$y, positive = "yes")
pred_svm <- prediction(predictions = as.numeric(svm_pred), labels = as.numeric(test_data$y))
perf_svm <- performance(pred_svm,measure = "tpr", x.measure = "fpr")
plot(perf_svm, main="SVM")

library(e1071)
bayes_model <- naiveBayes(train_data[-21], train_data$y, laplace = 1)
bayes_pred <- predict(bayes_model, test_data[-21])
CrossTable(bayes_pred, test_data$y)
kappa2(data.frame(bayes_pred, test_data$y))$value
confusionMatrix(bayes_pred,test_data$y, positive = "yes")
pred_nb <- prediction(predictions = as.numeric(bayes_pred), labels = as.numeric(test_data$y))
perf_nb <- performance(pred_nb,measure = "tpr", x.measure = "fpr")
plot(perf_nb, main="Naive Bayes")


library(randomForest)
rf_model <- randomForest(y~., ntree=100, data=train_data)
plot(rf_model)
varImpPlot(rf_model,  
           sort = T,
           n.var=20,
           main="Top 20 - Variable Importance")
rf_pred <- predict(rf_model, test_data[-21])
CrossTable(rf_pred, test_data$y)
kappa2(data.frame(rf_pred, test_data$y))$value
confusionMatrix(rf_pred,test_data$y, positive = "yes")
pred_rf <- prediction(predictions = as.numeric(rf_pred), labels = as.numeric(test_data$y))
perf_rf <- performance(pred_rf,measure = "tpr", x.measure = "fpr")
plot(perf_rf, main="Random Forest")

