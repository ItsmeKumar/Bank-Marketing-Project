setwd("~/Downloads/ML/bank-additional")

### Importing Data ###

# Importing the csv file and keeping stringsAsFactors= T for automatically converting all
# the string variables into factors
bank <- read.table("bank-additional-full.csv",header=TRUE,sep=";")
str(bank)



### Exploratory Data Analysis ###

# Visualizing different variables in the data set and their relations with the dependent variable y

library(ggplot2)

# The variable age shows that most of the people are between 25 and 60 as expected
# and it is slightly right skewed. And the distribution of people who subscribed is almost evenly
# distributed with repspect to the number of people in that age group.
ggplot(data=bank, aes(x=age, col=y))+
    geom_histogram()+
    ggtitle("Age distribution based on subscription")+
    theme(plot.title = element_text(hjust = 0.5))
# A boxplot of "age vs y" also reflects the same information
boxplot(age ~ y, data = bank, main= "age vs y")


# Creating a table to visualize the relationship between the job a person is doing and 
# whether he subscribed for the plan or not
table(bank$job, bank$y)
# Lets look at the proportion of people subscribing in accordance with their job role
prop.table(table(bank$job, bank$y), 1)
# Plotting the proportion table shows that students and retired people have very high probability
# of saying "yes" to the subscription compared to all other categories. And blue-collar, entrepreneur and 
# services are least probable for "yes".
plot((prop.table(table(bank$job, bank$y), 1)), main="job vs y")


# A similar plot of proportions for marital status vs y has little to no information
prop.table(table(bank$marital, bank$y), 1)
plot((prop.table(table(bank$marital, bank$y), 1)), main="marital Status vs y")


# The plot of proportion table shows that people who are illiterate, or having university degree and 
# the unknown category has more chance of taking the subscription
prop.table(table(bank$education, bank$y), 1)
plot((prop.table(table(bank$education, bank$y), 1)), main="education vs y")
# But histogram of education shows that concentration of people who are illiterate is very small compared to other categories.
# So people having a university degree is more reasonable target.
ggplot(bank, aes(x=education, col=y))+
    geom_histogram(stat="count")

# The default variable is very less informative as it has only 3 values in the category of people
# who have defaulted and large number of unkown values, making it worthless for the analysis.
table(bank$default)


# The proportion table shows that the variable y is almost uniformly distributed over all the 3 categories
# of housing and loan variables which indicates that these variables are very little correlation with y
ggplot(bank, aes(x=housing, col=y))+
    geom_histogram(stat="count")
plot((prop.table(table(bank$housing, bank$y), 1)), main="housing vs y")

ggplot(bank, aes(x=loan, col=y))+
    geom_histogram(stat="count")
plot((prop.table(table(bank$loan, bank$y), 1)), main="loan vs y")


# The people who were contacted through cellular phone have slighlty high probability of taking the 
# plan but a scatter plot involving consumer price index, y and contact shows that most of the people 
# who were contacted through telephone are people with cpi above 94 which means the variable is skewed
# and highly dependent on the consumer price index, so it has very little information by itself
plot((prop.table(table(bank$contact, bank$y), 1)), main="contact vs y")
ggplot(data=bank, aes(x=cons.price.idx,y=y, col=contact))+
    geom_jitter()+
    ggtitle("consumer price index vs y based on medium of contact")+
    theme(plot.title = element_text(hjust = 0.5))

# The plot of proportion table and scatterplot of month and day of the week on which people were contacted
# shows that the months december, march, october and september have a very high probability of people taking 
# the plan compared to other months. But the histogram of month in accordance with y shows that very less
# number of people were actually contacted in those months.
ggplot(bank, aes(x=month, col=y))+
    geom_histogram(stat="count")
prop.table(table(bank$month, bank$y),1)
plot((prop.table(table(bank$month, bank$y), 1)), main="month vs y")
ggplot(bank, aes(x=month, y=day_of_week, col=y)) +
    geom_jitter()


# The boxplot of call duration and whether the customer took the subscription clearly indicates that people
# who took subscription spend significanlty more amount of time speaking to the representative.
# The variable duration can be highly useful while predicting y. We can acutally predict that y="no" whenever
# call duration is 0 which means that if a customer didnt attend the call then he/she didnt subscribe. And the variable
# duration should be left out while building a true predictive model, as we only get to know call duration after speaking
# to the customer, but we would automatically know if a person subscribed or not (y="yes" or "no") by the end of the call
# So this variable can't be used in reality
ggplot(bank, aes(x=y, y=duration))+
    geom_boxplot()


# The boxplot and table of campaign(number of times client was contacted during this campaign) shows that it is very 
# unlikely that client will say "yes" after contacting the client more than 10 times.
ggplot(bank, aes(x=y, y=campaign))+
    geom_boxplot()
table(bank$campaign, bank$y)
plot(table(bank$campaign, bank$y), main="campaign vs y")


# The proportion table of pdays (number of days that passed by after the client was last contacted from a 
# previous campaign. If a client was not contacted previously pdays will be 999) of people who were contacted 
# previously and people who weren't shows that, previously contacted clients have 63.8% chance of taking the subscription
# while people who weren't contacted previously only have 9% chance of taking the plan.
ggplot(bank[bank$pdays!=999,], aes(x=y))+
    geom_bar()
prop.table(table(bank[bank$pdays!=999,]$y))
ggplot(bank[bank$pdays==999,], aes(x=y))+
    geom_bar()
prop.table(table(bank[bank$pdays==999,]$y))

# The previous variable (number of contacts performed before this campaign and for this client) is very similar to
# to campaign variable, and indicates that people who were contacted previously have higher chance of taking the subscription
# compared to the people who were not contacted before this campaign. By examining the relationship between pdays and previous using
# subsets of population, it is evident that, of those people who were contacted at least once during this campaign were all contacted
# at least once before this campaign. And complimentarily, the people who were not contacted even once before this campaign are also not
# contacted even once in this campaign. This indicates that the variable previous is highly dependent on the variable pdays and doesn't play
# significant role in giving extra information
ggplot(bank[bank$previous!=0,], aes(x=y))+
    geom_bar()
prop.table(table(bank[bank$previous!=0,]$y))
ggplot(bank[bank$previous==0,], aes(x=y))+
    geom_bar()
prop.table(table(bank[bank$previous==0,]$y))

table(bank[bank$pdays!=999,]$previous)
table(bank[bank$previous==0,]$pdays)


# Bar plots of each category in the variable poutcome(outcome of the previous marketing campaign) indicates that people whose outcome of the previous 
# campaign is a success have very high probability of taking the plan followed by people who rejected the offer in previous campaign. The people who were
# not contacted previously at all have very low chance of taking the plan. But looking at the people who are nonexistent tells that they are the same group
# of people whose pdays=999 or those who were not contacted during this campaign at all, which inevitably means that they all have y=0. So the variable although
# provides an interesting insight, it is highly dependedent on pdays variable.
ggplot(bank[bank$poutcome=="nonexistent",], aes(x=y))+
    geom_bar()
prop.table(table(bank[bank$poutcome=="nonexistent",]$y))
ggplot(bank[bank$poutcome=="failure",], aes(x=y))+
    geom_bar()
prop.table(table(bank[bank$poutcome=="failure",]$y))
ggplot(bank[bank$poutcome=="success",], aes(x=y))+
    geom_bar()
prop.table(table(bank[bank$poutcome=="success",]$y))

table(bank[bank$poutcome=="nonexistent",]$pdays)
table(bank[bank$poutcome=="nonexistent"&bank$pdays==999,]$pdays)


# The probability taking the subscription significantly increases when employment variance rate is less than -1.
ggplot(data=bank, aes(x=emp.var.rate, col=y))+
    geom_histogram()+
    ggtitle("employment variance rate based on subscription")+
    theme(plot.title = element_text(hjust = 0.5))
ggplot(data=bank, aes(x=emp.var.rate, col=y))+
    geom_histogram(position = "fill")+
    ggtitle("employment variance rate based on subscription")+
    theme(plot.title = element_text(hjust = 0.5))

# A histogram of proportions of the variable consumer price index shows that the concentration of people
# taking the plan is evenly spread on both higher and lower sides of cpi, so we can't really make any
# generalizations about a particular group being more favourable for saying "yes"
ggplot(data=bank, aes(x=cons.price.idx, col=y))+
    geom_histogram(position = "fill")+
    ggtitle("consumer price index based on subscription")+
    theme(plot.title = element_text(hjust = 0.5))


# The scatterplot and histogram of consumer confidence index indicates that the amount of people who are taking
# the subscription mostly depends on the amount of people present in that particular range of values and not actually
# on the consumer confidence index itself
ggplot(data=bank, aes(x=y, y=cons.conf.idx, col=y))+
    geom_jitter(shape=46)
ggplot(data=bank, aes(x=cons.conf.idx, col=y))+
    geom_histogram(position = "fill")+
    ggtitle("consumer confidence index based on subscription")+
    theme(plot.title = element_text(hjust = 0.5))


# The histograms and proportion tables of euribor (Euro Interbank Offered Rate) shows that, as euribor decreases
# the probability of people taking the plan increases significantly. And when euribor drops below 1, there is very high
# chance of people taking the subscription.
ggplot(data=bank, aes(x=euribor3m, col=y))+
    geom_histogram()+
    ggtitle("consumer confidence index based on subscription")+
    theme(plot.title = element_text(hjust = 0.5))
ggplot(data=bank, aes(x=euribor3m, col=y))+
    geom_histogram(position = "fill")+
    ggtitle("consumer confidence index based on subscription")+
    theme(plot.title = element_text(hjust = 0.5))
boxplot(euribor3m ~ y, data = bank)

prop.table(table(bank[bank$euribor3m<5,]$y))
prop.table(table(bank[bank$euribor3m<3,]$y))
prop.table(table(bank[bank$euribor3m<1,]$y))


# The barplot, histogram and proportion tables of the nr.employed shows that, as the number of employees increases
# the efficiency of the campaign decreases. When the number of employees are less than 5000 the probability of a client
# accepting the offer increases significantly.
bartable <- table(bank$nr.employed, bank$y)
barplot(bartable, beside = TRUE, legend = levels(unique(bank$nr.employed)))
ggplot(data=bank, aes(x=nr.employed, col=y))+
    geom_histogram()+
    ggtitle("consumer confidence index based on subscription")+
    theme(plot.title = element_text(hjust = 0.5))
ggplot(data=bank, aes(x=nr.employed, col=y))+
    geom_histogram(position="fill")+
    ggtitle("consumer confidence index based on subscription")+
    theme(plot.title = element_text(hjust = 0.5))

prop.table(table(bank[bank$nr.employed<5228,]$y))
prop.table(table(bank[bank$nr.employed<5100,]$y))
prop.table(table(bank[bank$nr.employed<5000,]$y))



# Getting the indexes of factor columns from bank data set, to convert them into numeric for creating 
# a correlation plot
bank_dup <- bank
factors_index <- which(sapply(bank_dup, is.factor))
factors_index
# Converting factor columns to numeric 
bank_dup[,factors_index] <- lapply(factors_index, function(fac) {as.numeric(bank_dup[,fac])})
str(bank_dup)
# Correlation plot of bank explains the correlation between different columns of bank
library(corrplot)
C <- cor(bank_dup)
corrplot(C, tl.col="black")
corrplot(C, method = "number",number.cex=0.6, tl.col="black")



### Model building and evaluation ###


# Normalizing the numeric features in bank to reduce the bias towards features with comparitively high numeric values
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
train_ind <- createDataPartition(bank$y, p=0.75, list=FALSE)
train_data <- bank[train_ind, ]
test_data <- bank[-train_ind, ]

# The target variable y is uniformly distributed among both train and test sets
prop.table(table(train_data$y))
prop.table(table(test_data$y))



### Decision tree ###

library(C50)

set.seed(141)
# Building a decision tree classification model using training set data
decision_tree <- C5.0(train_data[-21], train_data$y, trails=20)
# The summary of decision tree model shows that the variables poutcome, duration, nr.employed,
# month, age and emp.var.rate are the most important variables in predicting the target variable y
summary(decision_tree)
# Applying model to the training set which classifies all the observations in training set as either "yes" or "no"
tree_pred <- predict(decision_tree, test_data[-21])
str(tree_pred)

# Evaluating model performance by comparing the predicted variable with true labels

# The model has an accuracy of 91.36% and p-value is < 2.2e-16 which implies that the model is good. 
# The kappa value is 0.534 which indicates a moderate agreement between true and predicted values
#  The sensitivity and specificity of the model are 0.5336 and 0.9618, which implies that the model is good at
# prediciting the negative cases (y="no") more accurately than positive cases(y="yes")
CrossTable(tree_pred, test_data$y)
confusionMatrix(tree_pred,test_data$y, positive = "yes")

# The ROC curve suggests that the model is doing a fine job in predicting the true negatives and false positives
# with a area under the curve value of 0.7477
pred_dt <- prediction(predictions = as.numeric(tree_pred), labels = as.numeric(test_data$y))
perf_dt <- performance(pred_dt,measure = "tpr", x.measure = "fpr")
plot(perf_dt, main="decision tree")
perf.auc <- performance(pred_dt, measure = "auc")
unlist(perf.auc@y.values)

# The decision tree we built has very good accuracy, but while using it in reality we don't want our model to
# classify a huge number of true positives as false negatives, which means classifying a person who is actually
# going to take the subscription as "no" which is a big mistake. So, we assign cost for each of errors and
# train a model that penalises false negatives cases more than false positive cases. This results in model classifying
# the people who are at the edge of being classified as FN or FP into FP's. It increases the false positive rate but
# making few extra calls is better than missing a positive client.

# Creating cost error matrix 
matrix_dimensions <- list(c("no", "yes"), c("no", "yes"))
names(matrix_dimensions) <- c("predicted", "actual")
matrix_dimensions
error_cost <- matrix(c(0, 1, 4, 0), nrow = 2, dimnames = matrix_dimensions)
error_cost

# Training a decision tree model using the error_cost matrix
set.seed(141)
decision_tree <- C5.0(train_data[-21], train_data$y, trails=20, costs=error_cost)
summary(decision_tree)

# Applying the new model to test set
tree_pred <- predict(decision_tree, test_data[-21])

# The accuracy of the model was slightly reduced to 88.19%. But the value of sensitivity is very significantly improved
confusionMatrix(tree_pred,test_data$y, positive = "yes")

# The ROC curve also reflects improvement in the prediction of true positives
pred_dt <- prediction(predictions = as.numeric(tree_pred), labels = as.numeric(test_data$y))
perf_dt <- performance(pred_dt,measure = "tpr", x.measure = "fpr")
plot(perf_dt, main="decision tree")
perf.auc_dt <- performance(pred_dt, measure = "auc")
unlist(perf.auc_dt@y.values)



### Neural Networks ###

set.seed(141)
library(nnet)
# creating a neural network model using training set
nnet_model <- nnet(y~age + job + marital + education + 
                       default + housing + loan + contact + 
                       month + day_of_week + duration + campaign + 
                       pdays + previous + poutcome + emp.var.rate + 
                       cons.price.idx + cons.conf.idx + euribor3m + 
                       nr.employed, data=train_data, size=3, decay=0.1)
# The model has 53 input nodes, 3 hidden nodes and 1 output node
nnet_model$n

# applying the neural network model to test set
nnet_pred <- predict(nnet_model, test_data[-21], type="class")
str(nnet_pred)

# The accuracy of the model is 91.11% and p-value is 1.707e-15 which is very good. kappa is 0.51 
# which indicates a moderate agreement between predicted and true values. But the sensitivity of the
# model is bit low
CrossTable(nnet_pred, test_data$y)
confusionMatrix(nnet_pred,test_data$y, positive = "yes")

# ROC curve of the predicted and true values indicating the relationship between true positive rate and
# false positive rate. The area under the curve for the plot is 0.7386739
nnet_pred_fac <- as.factor(nnet_pred) 
pred_nn <- prediction(predictions = as.numeric(nnet_pred_fac), labels = as.numeric(test_data$y))
perf_nn <- performance(pred_nn,measure = "tpr", x.measure = "fpr")
plot(perf_nn, main="neural net")
perf.auc_nn <- performance(pred_nn, measure = "auc")
unlist(perf.auc_nn@y.values)

# Lets try to improve the model peformance by using the function pcaNNet which applies principal component analysis to
# the variables before building a neural network model. And also size of the hidden units were reduced to 2 for the model to
# generalize more on future data and to avoid overfitting
set.seed(150)
nnet_model_2 <- pcaNNet(y~age + job + marital + education + 
            default + housing + loan + contact + 
            month + day_of_week + duration + campaign + 
            pdays + previous + poutcome + emp.var.rate + 
            cons.price.idx + cons.conf.idx + euribor3m + 
            nr.employed, data=train_data, size=2, decay=0.1)
# predicting the target variable of the training set using the model
nnet_pred_2 <- predict(nnet_model_2, test_data[,-21], type="class")
str(nnet_pred_2)

# The sensitivity of the model fairly increased but it is still less efficient compared to the decision tree model
confusionMatrix(nnet_pred_2,test_data$y, positive = "yes")


### Support Vector Machine ###

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




