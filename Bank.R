
### Importing Data ###

setwd("~/Downloads/ML/bank-additional")

# Importing the csv file and keeping stringsAsFactors= T for automatically converting all
# the string variables into factors
bank <- read.table("bank-additional-full.csv",header=TRUE,sep=";")
str(bank)



### Exploratory Data Analysis ###

# Visualizing different variables in the data set and their relations with the dependent 
# variable y

library(ggplot2)

## The variable age shows that most of the people are between 25 and 60 as expected and it 
# is slightly right skewed. And the distribution of people who subscribed is almost evenly
# distributed with repspect to the number of people in that age group. ##
ggplot(data=bank, aes(x=age, col=y))+
    geom_histogram()+
    ggtitle("Age distribution based on subscription")+
    theme(plot.title = element_text(hjust = 0.5))
# A boxplot of "age vs y" also reflects the same information
ggplot(bank, aes(x=y, y=age, col=y))+
    geom_boxplot()+
    ggtitle("Age vs y")+
    theme(plot.title = element_text(hjust = 0.5))

## Creating a table to visualize the relationship between the job role of a person and 
# variable y indicating whether a person subscribed for the plan or not
table(bank$job, bank$y)
# Lets look at the proportion of people subscribing with repect to their job roles
prop.table(table(bank$job, bank$y), 1)
# Plotting the proportions of each category shows that students and retired people have
# very high probability of saying "yes" to the subscription compared to all other 
# categories. And blue-collar, entrepreneur andservices are the least probable categories 
# for saying "yes". ##
ggplot(bank, aes(x=job, col=y))+
    geom_histogram(stat="count")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    ggtitle("Histogram of job in relation with y")+
    theme(plot.title = element_text(hjust = 0.5))
ggplot(bank, aes(x=job, col=y))+
    geom_histogram(stat="count", position = "fill")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    ggtitle("Histogram of job in relation with y")+
    theme(plot.title = element_text(hjust = 0.5))

## A similar plot of proportions for marital status vs y shows the spread of y is almost 
# evenly distributed and there is very little to no insight ##
prop.table(table(bank$marital, bank$y), 1)
plot((prop.table(table(bank$marital, bank$y), 1)), 
     main="Marital status vs y", col=c("black","grey"))


## The plot of proportions shows that people who are illiterate, people who has a
# university degree and the unknown category has more chance of taking the subscription
prop.table(table(bank$education, bank$y), 1)
ggplot(bank, aes(x=education, col=y))+
    geom_histogram(stat="count", position = "fill")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    ggtitle("Histogram of education in relation with y")+
    theme(plot.title = element_text(hjust = 0.5))
# But histogram of education shows that concentration of people who are illiterate is very
# small compared to other categories. So people having a university degree are more
# reasonable target.
ggplot(bank, aes(x=education, col=y))+
    geom_histogram(stat="count")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    ggtitle("Histogram of education in relation with y")+
    theme(plot.title = element_text(hjust = 0.5))

# The default variable is very less informative as it has only 3 values in the category of
# people who have defaulted and also large number of unkown values. So we can't get any
# understanding of its relation with y
table(bank$default)


# The proportion plots show that the variable y is almost uniformly distributed in all the
# 3 categories of both housing and loan variables which indicates that these variables has
# very less correlation with variable y
ggplot(bank, aes(x=housing, col=y))+
    geom_histogram(stat="count")+
    ggtitle("Histogram of housing in relation with y")+
    theme(plot.title = element_text(hjust = 0.5))
ggplot(bank, aes(x=housing, col=y))+
    geom_histogram(stat="count", position = "fill")+
    ggtitle("Histogram of housing in relation with y")+
    theme(plot.title = element_text(hjust = 0.5))

ggplot(bank, aes(x=loan, col=y))+
    geom_histogram(stat="count")+
    ggtitle("Histogram of loan in relation with y")+
    theme(plot.title = element_text(hjust = 0.5))
ggplot(bank, aes(x=loan, col=y))+
    geom_histogram(stat="count", position = "fill")+
    ggtitle("Histogram of loan in relation with y")+
    theme(plot.title = element_text(hjust = 0.5))

# The people who were contacted through cellular phone have slighlty high probability of 
# taking the plan compared to the people who were contacted through a telephone
ggplot(bank, aes(x=contact, col=y))+
    geom_bar()+
    ggtitle("Histogram of contact in relation with y")+
    theme(plot.title = element_text(hjust = 0.5))
ggplot(bank, aes(x=contact, col=y))+
    geom_bar(position = "fill")+
    ggtitle("Histogram of contact in relation with y")+
    theme(plot.title = element_text(hjust = 0.5))


# The plot of proportion table and scatterplot of month and day of the week on which
# people were contacted shows that the months december, march, october and september
# have a very high probability of people taking the plan compared to other months.
# But the histogram of month in accordance with y shows that very less number of people
# were actually contacted in those months.
ggplot(bank, aes(x=month, col=y))+
    geom_histogram(stat="count")+
    ggtitle("Histogram of month in relation with y")+
    theme(plot.title = element_text(hjust = 0.5))
prop.table(table(bank$month, bank$y),1)
ggplot(bank, aes(x=month, col=y))+
    geom_histogram(stat="count", position = "fill")+
    ggtitle("Histogram of month in relation with y")+
    theme(plot.title = element_text(hjust = 0.5))
ggplot(bank, aes(x=month, y=day_of_week, col=y)) +
    geom_jitter()+
    ggtitle("Scatter plot of month and day of week wrt y")+
    theme(plot.title = element_text(hjust = 0.5))


# The boxplot of call duration and whether the customer took the subscription clearly 
# indicates that people who took subscription spend significanlty more amount of time
# speaking to the representative. The variable duration can be highly useful while
# predicting y. We can acutally predict that y="no" whenever call duration is 0 which 
# means that if a customer didnt attend the call then he/she didnt subscribe. And the
# variable duration should be left out while building a realistic predictive model, as we
# only get to know call duration after speaking to the customer, but we would anyway
# know if a person subscribed or not (y="yes" or "no") by the end of the call.So this 
# variable while building a true predictive model which will be used by the organization
# in future
ggplot(bank, aes(x=y, y=duration, col=y))+
    geom_boxplot()+
    ggtitle("duration vs y")+
    theme(plot.title = element_text(hjust = 0.5))


# The proportion table of campaign(number of times client was contacted during this
# campaign) shows that it is very unlikely that client will say "yes" after contacting
# the client more than 15 times with a probability of 1.4%
ggplot(bank, aes(x=y, y=campaign, col=y))+
    geom_boxplot()+
    ggtitle("campaign vs y")+
    theme(plot.title = element_text(hjust = 0.5))
table(bank$campaign, bank$y)
prop.table(table(bank[bank$campaign>15, ]$y))
prop.table(table(bank[bank$campaign<15, ]$y))


# The proportion table of pdays (number of days that passed by after the client was last
# contacted from a previous campaign. If a client was not contacted previously pdays will
# be 999) of people who were contacted previously and people who weren't shows that
# previously contacted clients have 63.8% chance of taking the subscription while people
# who weren't contacted previously only have 9% chance of taking the plan.
ggplot(bank[bank$pdays!=999,], aes(x=y, col=y))+
    geom_bar()+
    ggtitle("Histogram of y for people who were contacted after previous campaign")+
    theme(plot.title = element_text(hjust = 0.5))
prop.table(table(bank[bank$pdays!=999,]$y))
ggplot(bank[bank$pdays==999,], aes(x=y, col=y))+
    geom_bar()+
    ggtitle("Histogram of y for people who were not contacted after previous campaign")+
    theme(plot.title = element_text(hjust = 0.5))
prop.table(table(bank[bank$pdays==999,]$y))

# The previous variable (number of contacts performed before this campaign and for this
# client) is very similar to campaign variable, and indicates that people who were
# contacted previously have higher chance of taking the subscription compared to the
# people who were not contacted before this campaign. By examining the relationship
# between pdays and previous using subsets of population, it is evident that, of those
# people who were contacted at least once during this campaign were all contacted at least
# once before this campaign. And complimentarily, the people who were not contacted
# even once before this campaign are also not contacted even once in this campaign. This
# indicates that the variable previous is highly dependent on the variable pdays and
# doesn't play significant role in giving extra information
ggplot(bank[bank$previous!=0,], aes(x=y, col=y))+
    geom_bar()+
    ggtitle("Histogram of y for people who were conatacted before this campaign")+
    theme(plot.title = element_text(hjust = 0.5))
prop.table(table(bank[bank$previous!=0,]$y))
ggplot(bank[bank$previous==0,], aes(x=y, col=y))+
    geom_bar()+
    ggtitle("Histogram of y for people who were not conatacted before this campaign")+
    theme(plot.title = element_text(hjust = 0.5))
prop.table(table(bank[bank$previous==0,]$y))

table(bank[bank$pdays!=999,]$previous)
table(bank[bank$previous==0,]$pdays)


# Bar plots of each category of the variable poutcome(outcome of the previous marketing
# campaign) indicates that people whose outcome of the previous campaign is a success
# have very high probability of taking the plan followed by people who rejected the offer
# in previous campaign. The people who were not contacted previously at all have very low
# chance of taking the plan.
ggplot(bank[bank$poutcome=="nonexistent",], aes(x=y, col=y))+
    geom_bar()+
    ggtitle("Histogram of y for people in nonexistent category")+
    theme(plot.title = element_text(hjust = 0.5))
prop.table(table(bank[bank$poutcome=="nonexistent",]$y))
ggplot(bank[bank$poutcome=="failure",], aes(x=y, col=y))+
    geom_bar()+
    ggtitle("Histogram of y for people in failure category")+
    theme(plot.title = element_text(hjust = 0.5))
prop.table(table(bank[bank$poutcome=="failure",]$y))
ggplot(bank[bank$poutcome=="success",], aes(x=y, col=y))+
    geom_bar()+
    ggtitle("Histogram of y for people in success category")+
    theme(plot.title = element_text(hjust = 0.5))
prop.table(table(bank[bank$poutcome=="success",]$y))

# It is interesting to note that people who were non-existent in poutcome are the
# same people with pdays=999
table(bank[bank$poutcome=="nonexistent",]$y)
table(bank[bank$poutcome=="nonexistent"&bank$pdays==999,]$y)


# The probability taking the subscription significantly increases when employment
# variance rate is less than -1.
ggplot(data=bank, aes(x=emp.var.rate, col=y))+
    geom_histogram()+
    ggtitle("employment variance rate based on subscription")+
    theme(plot.title = element_text(hjust = 0.5))
ggplot(data=bank, aes(x=emp.var.rate, col=y))+
    geom_histogram(position = "fill")+
    ggtitle("employment variance rate based on subscription")+
    theme(plot.title = element_text(hjust = 0.5))

# A histogram of proportions of the variable consumer price index shows that the
# concentration of people taking the plan is evenly spread on both higher and lower
# sides of cpi, so we can't really make any generalizations about a particular group
# being more favourable for saying "yes"
ggplot(data=bank, aes(x=cons.price.idx, col=y))+
    geom_histogram(position = "fill")+
    ggtitle("consumer price index based on subscription")+
    theme(plot.title = element_text(hjust = 0.5))


# The scatterplot and histogram of consumer confidence index indicates that the amount of
# people who are taking the subscription mostly depends on the amount of people present in
# that particular range of values and not actually on the consumer confidence index itself
ggplot(data=bank, aes(x=y, y=cons.conf.idx, col=y))+
    geom_jitter(shape=46)+
    ggtitle("Consumer confidence index vs y")+
    theme(plot.title = element_text(hjust = 0.5))
ggplot(data=bank, aes(x=cons.conf.idx, col=y))+
    geom_histogram(position = "fill")+
    ggtitle("Consumer confidence index based on subscription")+
    theme(plot.title = element_text(hjust = 0.5))


# The histograms and proportion tables of euribor(Euro Interbank Offered Rate) shows that,
# as euribor decreases the probability of people taking the plan increases significantly.
# And when euribor drops below 1, there is very high chance of people taking the
# subscription.
ggplot(data=bank, aes(x=euribor3m, col=y))+
    geom_histogram()+
    ggtitle("Histogram of euribor rate based on subscription")+
    theme(plot.title = element_text(hjust = 0.5))
ggplot(data=bank, aes(x=euribor3m, col=y))+
    geom_histogram(position = "fill")+
    ggtitle("Histogram of euribor rate based on subscription")+
    theme(plot.title = element_text(hjust = 0.5))
ggplot(bank, aes(x=y, y=euribor3m, col=y))+
    geom_boxplot()+
    ggtitle("Euribor vs y")+
    theme(plot.title = element_text(hjust = 0.5))

prop.table(table(bank[bank$euribor3m<5,]$y))
prop.table(table(bank[bank$euribor3m<3,]$y))
prop.table(table(bank[bank$euribor3m<1,]$y))


# The barplot, histogram and proportion tables of the nr.employed shows that, as the
# number of employees increases the efficiency of the campaign decreases. When the number
# of employees are less than 5000 the probability of a client accepting the offer
# increases significantly.
bartable <- table(bank$nr.employed, bank$y)
barplot(bartable, beside = TRUE, legend = levels(unique(bank$nr.employed)), 
        main="employed vs y")
ggplot(data=bank, aes(x=nr.employed, col=y))+
    geom_histogram()+
    ggtitle("Histogram of nr.employed in relation with subscription")+
    theme(plot.title = element_text(hjust = 0.5))
ggplot(data=bank, aes(x=nr.employed, col=y))+
    geom_histogram(position="fill")+
    ggtitle("Histogram of nr.employed in relation with subscription")+
    theme(plot.title = element_text(hjust = 0.5))

prop.table(table(bank[bank$nr.employed<5228,]$y))
prop.table(table(bank[bank$nr.employed<5100,]$y))
prop.table(table(bank[bank$nr.employed<5000,]$y))



# Getting the indexes of factor columns from bank data set, to convert them into
# numeric for creating a correlation plot
bank_dup <- bank
factors_index <- which(sapply(bank_dup, is.factor))
factors_index
# Converting factor columns to numeric 
bank_dup[,factors_index] <- lapply(factors_index, function(fac) 
    {as.numeric(bank_dup[,fac])})
str(bank_dup)
# Correlation plot of bank explains the correlation between different columns of bank
library(corrplot)
C <- cor(bank_dup)
corrplot(C, tl.col="black")
corrplot(C, method = "number",number.cex=0.6, tl.col="black")



### Model building and evaluation ###


# Normalizing the numeric features in bank to reduce the bias towards features with
# comparitively high numeric values
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
factors_index <- which(sapply(bank, is.factor))
factors_index
bank_n <- as.data.frame(lapply(bank[ ,-factors_index], normalize))
names(bank_n)
bank[names(bank_n)] <- bank_n[names(bank_n)]
str(bank)

# Splitting the bank data set into training and testing sets
library(irr)
library(ROCR)
library(caret)
library(gmodels)
set.seed(141)
train_ind <- createDataPartition(bank$y, p=0.75, list=FALSE)
train_data <- bank[train_ind, ]
test_data <- bank[-train_ind, ]

# The target variable y is uniformly distributed among both train and test sets
prop.table(table(train_data$y))
prop.table(table(test_data$y))


### Naive Bayes ###

library(e1071)

set.seed(141)
# Building naive bayes model using train data
bayes_model <- naiveBayes(train_data[-21], train_data$y, laplace = 3)
bayes_model

# Applying the model on the test data to predict the dependent variable
bayes_pred <- predict(bayes_model, test_data[-21])
str(bayes_pred)

# The model has accuracy of around 86% which is good. The kappa value of  0.42 suggests
# a moderate agreement between the true and predicted values
CrossTable(bayes_pred, test_data$y)
confusionMatrix(bayes_pred,test_data$y, positive = "yes")
nb_accuracy <- confusionMatrix(bayes_pred,test_data$y, positive = "yes")$overall[[1]]
nb_accuracy <- nb_accuracy*100
nb_accuracy

# Evaluation of the model using ROC curve and AUC(area under the curve) shows the model
# is performing fine in predicting the false positives and false negatives with auc value
# of 0.75, which has value of 1 for ideal case.
pred_nb <- prediction(predictions = as.numeric(bayes_pred), labels = as.numeric(test_data$y))
perf_nb <- performance(pred_nb,measure = "tpr", x.measure = "fpr")
plot(perf_nb, main="Naive Bayes")
perf.auc_nb <- performance(pred_nb, measure = "auc")
nb_auc <- unlist(perf.auc_nb@y.values)
nb_auc



### Decision tree ###

library(C50)

set.seed(141)
# Building a decision tree classification model using training set data
decision_tree <- C5.0(train_data[-21], train_data$y, trails=20)
# The summary of decision tree model shows that the variables poutcome, duration,
# nr.employed, month, age and emp.var.rate are the most important variables in predicting
# the target variable y
summary(decision_tree)
# Applying model to the training set which classifies all the observations in training set
# as either "yes" or "no"
tree_pred <- predict(decision_tree, test_data[-21])
str(tree_pred)

# Evaluating model performance by comparing the predicted variable with true labels

# The model has an accuracy of 91.32% and p-value is < 2.2e-16 which implies that the model
# is performing well. The kappa value is 0.533 which indicates a moderate agreement between
# true and predicted values. The sensitivity and specificity of the model are 0.53 and 0.96,
# which are the false negative and false positive rates respectively
CrossTable(tree_pred, test_data$y)
confusionMatrix(tree_pred,test_data$y, positive = "yes")
dt_accuracy <- confusionMatrix(tree_pred,test_data$y, positive = "yes")$overall[[1]]
dt_accuracy <- dt_accuracy*100
dt_accuracy

# The ROC curve suggests that the model is doing a fine job in predicting the true negatives
# and false positives with a area under the curve value of 0.74
pred_dt <- prediction(predictions = as.numeric(tree_pred), labels = as.numeric(test_data$y))
perf_dt <- performance(pred_dt,measure = "tpr", x.measure = "fpr")
plot(perf_dt, main="Decision Tree 1")
perf.auc <- performance(pred_dt, measure = "auc")
dt_auc <- unlist(perf.auc@y.values)
dt_auc


# We can improve performance of the model in classifying FP and FN and thereby increasing
# the value of auc by assigning cost or penalty for the making a FP or FN mistake. Here we
# assignmed more penalty for FN than FP because it is better to make few extra calls for
# the people who won't actually take the plan, than classifying the person who would
# actually take the subscription as "no" and avoid contacting him altogether

# Creating cost error matrix 
matrix_dimensions <- list(c("no", "yes"), c("no", "yes"))
names(matrix_dimensions) <- c("predicted", "actual")
matrix_dimensions
error_cost <- matrix(c(0, 1, 5, 0), nrow = 2, dimnames = matrix_dimensions)
error_cost

# Training a decision tree model using the error_cost matrix
set.seed(141)
decision_tree_2 <- C5.0(train_data[-21], train_data$y, trails=20, costs=error_cost)
summary(decision_tree)

# Applying the new model to test set
tree_pred_2 <- predict(decision_tree_2, test_data[-21])
str(tree_pred_2)

# The accuracy of the model was slightly reduced to 88.06%. But the value of sensitivity
# is very significantly improved
confusionMatrix(tree_pred_2,test_data$y, positive = "yes")
dt2_accuracy <- confusionMatrix(tree_pred_2,test_data$y, positive = "yes")$overall[[1]]
dt2_accuracy <- dt2_accuracy*100
dt2_accuracy

# The ROC curve also reflects improvement in the prediction of true positives with auc of 0.8845
pred_dt_2 <- prediction(predictions = as.numeric(tree_pred_2), labels = as.numeric(test_data$y))
perf_dt_2 <- performance(pred_dt_2,measure = "tpr", x.measure = "fpr")
plot(perf_dt_2, main="Decision Tree 2")
perf.auc_dt_2 <- performance(pred_dt_2, measure = "auc")
dt2_auc <- unlist(perf.auc_dt_2@y.values)
dt2_auc



### Neural Networks ###

library(nnet)

set.seed(141)
# creating a neural network model using training set
nnet_model <- nnet(y~age + job + marital + education + 
                       default + housing + loan + contact + 
                       month + day_of_week + duration + campaign + 
                       pdays + previous + poutcome + emp.var.rate + 
                       cons.price.idx + cons.conf.idx + euribor3m + 
                       nr.employed, data=train_data, size=9, decay=0.1)
# The model has 53 input nodes, 9 hidden nodes and 1 output node
nnet_model$n

# applying the neural network model to test set
nnet_pred <- predict(nnet_model, test_data[-21], type="class")
str(nnet_pred)

# The accuracy of the model is around 91% which is very good. And the kappa value is
# indicating a moderate agreement between predicted and true values.
CrossTable(nnet_pred, test_data$y)
confusionMatrix(nnet_pred,test_data$y, positive = "yes")
nnet_accuracy <- confusionMatrix(nnet_pred,test_data$y, positive = "yes")$overall[[1]]
nn_accuracy <- nnet_accuracy*100
nn_accuracy

# Building ROC curve and calculating AUC of the predicted and true values indicating the
# relationship between true positive rate and false positive rate.
nnet_pred_fac <- as.factor(nnet_pred) 
pred_nn <- prediction(predictions = as.numeric(nnet_pred_fac), labels = as.numeric(test_data$y))
perf_nn <- performance(pred_nn,measure = "tpr", x.measure = "fpr")
plot(perf_nn, main="Neural Net 1")
perf.auc_nn <- performance(pred_nn, measure = "auc")
nn_auc <- unlist(perf.auc_nn@y.values)
nn_auc


# Lets try to improve the model peformance by using the function pcaNNet which applies
# principal component analysis to the variables before building a neural network model.
set.seed(141)
nnet_model_2 <- pcaNNet(y~age + job + marital + education + 
            default + housing + loan + contact + 
            month + day_of_week + duration + campaign + 
            pdays + previous + poutcome + emp.var.rate + 
            cons.price.idx + cons.conf.idx + euribor3m + 
            nr.employed, data=train_data, size=8, decay=0.1)

# predicting the target variable of the training set using the model
nnet_pred_2 <- predict(nnet_model_2, test_data[,-21], type="class")
str(nnet_pred_2)

# The sensitivity of the model fairly increased but it is still less efficient compared to
# the decision tree model
confusionMatrix(nnet_pred_2,test_data$y, positive = "yes")
nn2_accuracy <- confusionMatrix(nnet_pred_2,test_data$y, positive = "yes")$overall[[1]]
nn2_accuracy <- nn2_accuracy*100
nn2_accuracy

# Plotting the ROC curve using the true and predicted values of target variable and
# computing area under the ROC curve
nnet_pred_fac_2 <- as.factor(nnet_pred_2) 
pred_nn_2 <- prediction(predictions = as.numeric(nnet_pred_fac_2), labels = as.numeric(test_data$y))
perf_nn_2 <- performance(pred_nn_2,measure = "tpr", x.measure = "fpr")
plot(perf_nn_2, main="Neural Net 2")
perf.auc_nn_2 <- performance(pred_nn_2, measure = "auc")
nn2_auc <- unlist(perf.auc_nn_2@y.values)
nn2_auc


### Support Vector Machine ###

library(kernlab)

# Building a SVM model using training set
set.seed(141)
svm_model <- ksvm(y~., data=train_data, kernel = "rbfdot", C=9)
svm_model

# Predicting the target variable by supplying test data for the model
svm_pred <- predict(svm_model, test_data[-21])
str(svm_pred)

# The accuracy of the SVM model is around 91% and a kappa value of 0.46
CrossTable(svm_pred, test_data$y)
confusionMatrix(svm_pred,test_data$y, positive = "yes")
svm_accuracy <- confusionMatrix(svm_pred,test_data$y, positive = "yes")$overall[[1]]
svm_accuracy <- svm_accuracy*100
svm_accuracy

# ROC curve and AUC
pred_svm <- prediction(predictions = as.numeric(svm_pred), labels = as.numeric(test_data$y))
perf_svm <- performance(pred_svm,measure = "tpr", x.measure = "fpr")
plot(perf_svm, main="SVM")
perf.auc_svm <- performance(pred_svm, measure = "auc")
svm_auc <- unlist(perf.auc_svm@y.values)
svm_auc



### Random Forest ###

library(randomForest)

# Building a random forest model using training data
set.seed(141)
rf_model <- randomForest(y~., ntree=80, data=train_data)
rf_model

# Generating a variable importance graph using the random forest model we built. The
# variables with high Gini index are the most important variables. So, the variables
# at the top of the y-axis are more important in building a model than variables at
# the bottom
varImpPlot(rf_model,  
           sort = T,
           n.var=20,
           main="Top 20 - Variable Importance")

# Applying our model to test data for predicting the target variable y
rf_pred <- predict(rf_model, test_data[-21])
str(rf_pred)

# The accuracy of the model is around 91%
CrossTable(rf_pred, test_data$y)
confusionMatrix(rf_pred,test_data$y, positive = "yes")
rf_accuracy <- confusionMatrix(rf_pred,test_data$y, positive = "yes")$overall[[1]]
rf_accuracy <- rf_accuracy*100
rf_accuracy

# ROC curve and AUC for the Random Forest model
pred_rf <- prediction(predictions = as.numeric(rf_pred), labels = as.numeric(test_data$y))
perf_rf <- performance(pred_rf,measure = "tpr", x.measure = "fpr")
plot(perf_rf, main="Random Forest")
perf.auc_rf <- performance(pred_rf, measure = "auc")
rf_auc <- unlist(perf.auc_rf@y.values)
rf_auc


### Comparision of models ###

# Creating a matrix containing accuracy and AUC values of all the models
compare <- matrix(c(nb_accuracy, nb_auc, dt_accuracy, dt_auc, dt2_accuracy, dt2_auc,
                       nn_accuracy, nn_auc, nn2_accuracy, nn2_auc,
                       svm_accuracy, svm_auc, rf_accuracy, rf_auc),ncol=2, byrow = T)

compare <- as.data.frame(compare)

rownames(compare) <- c("NaiveBayes", "DecisionTree1", "DecisionTree2", "NeuralNetwork1",
                       "NeuralNetwork2", "SVM", "RandomForest")

names(compare) <- c("Accuracy", "AUC")
compare

# Adding a third column for the comparision matrix which serves as final evaluation metric
compare$evaluation <- (compare$Accuracy^2)*(compare$AUC)
compare



### If FN and FP rates are considered significant in addition to the accuracy, we need to
# select the model with highest evaluation metric. The 2nd Decision Tree model is a clear
# standout in this case ###
compare[order(compare$evaluation, decreasing = T), ]

### If model accuracy is the only metric to be considered, then we can select any of
# Random Forest, Neural network 1, Decision tree 1 or Support Vector Machines, as all
# these models have almost the same accuracy ###
compare[order(compare$Accuracy, decreasing = T), ]



### K-fold cross validation ###


# Performing K-fold cross validation on the selected models to get a better estimation 
# of its future performance

# Random Forest #
# 10-fold cross validation of the random forest model
set.seed(141)
folds <- createFolds(bank$y, k=10)
cv_results <- lapply(folds, function (x) {
    bank_train <- bank[-x, ]
    bank_test <- bank[x, ]
    bank_model <- randomForest(y~., ntree=80, data=bank_train)
    bank_predict <- predict(bank_model, bank_test[-21])
    accuracy <- confusionMatrix(bank_predict, bank_test$y, positive = "yes")$overall[[1]]
    accuracy <- accuracy*100
    return(accuracy)
})

# The average accuracy of our random forest models is 91.5% which is impressive
cv_rf <- mean(unlist(cv_results))
cv_rf


# Neural network #
# 10-fold cross validation of the Neural network 1 model #
set.seed(141)
folds <- createFolds(bank$y, k=10)
cv_results <- lapply(folds, function (x) {
    bank_train <- bank[-x, ]
    bank_test <- bank[x, ]
    bank_model <- nnet(y~age + job + marital + education + 
                           default + housing + loan + contact + 
                           month + day_of_week + duration + campaign + 
                           pdays + previous + poutcome + emp.var.rate + 
                           cons.price.idx + cons.conf.idx + euribor3m + 
                           nr.employed,data=bank_train, size=9, decay=0.1)
    bank_predict <- predict(bank_model, bank_test[-21], type="class")
    accuracy <- confusionMatrix(bank_predict, bank_test$y, positive = "yes")$overall[[1]]
    accuracy <- accuracy*100
    return(accuracy)
})

# The average accuracy of the neural network model is 91.3%
cv_nn <- mean(unlist(cv_results))
cv_nn


# Decision tree #
# 10-fold cross validation of the Decision tree 1 model
set.seed(141)
folds <- createFolds(bank$y, k=10)
cv_results <- lapply(folds, function (x) {
    bank_train <- bank[-x, ]
    bank_test <- bank[x, ]
    bank_model <- C5.0(bank_train[-21],bank_train$y, trails=20)
    bank_predict <- predict(bank_model, bank_test[-21], type="class")
    accuracy <- confusionMatrix(bank_predict, bank_test$y, positive = "yes")$overall[[1]]
    accuracy <- accuracy*100
    return(accuracy)
})

# The decision tree model has an average accuracy of 91.3%
cv_dt <- mean(unlist(cv_results))
cv_dt

# 
set.seed(141)
folds <- createFolds(bank$y, k=10)
cv_results <- lapply(folds, function (x) {
    bank_train <- bank[-x, ]
    bank_test <- bank[x, ]
    bank_model <- ksvm(y~., data=bank_train, kernel = "rbfdot", C=9)
    bank_predict <- predict(bank_model, bank_test[-21])
    accuracy <- confusionMatrix(bank_predict, bank_test$y, positive = "yes")$overall[[1]]
    accuracy <- accuracy*100
    return(accuracy)
})

# The SVM model has an average accuracy of 90.9%
cv_svm <- mean(unlist(cv_results))
cv_svm


### Random forest model is more robust and stable in predicting future outcomes and it is
# the best model to use if accuracy is the only criterion ###
