setwd(dir = "D:\\IIM - K\\CAPSTONE PROJECT")

data<-read.csv("TVS.csv")

------------------------------------------------------------------------------------
#Feature Selection: Removed the following 4 variables that are not important for analysis: 
#CustomerID, 
#Dealer codes, 
#DOB, 
#Number of new loans (all zeroes) in last 3 months

data<-data[,-c(1,9,16,22)]

-----------------------------------------------------------------------------------

#Missing values: there are missing values from row 85049 to 119528, 
#hence we selected rows 1:85048 for our analysis. 
#Out of total 119528 rows, we will drop 34480 records
#119528-85048

colSums(is.na(data))

data<-data[1:85048,]

--------------------------------------------------------------------------------
  
#there are variables with missing values, we will replace it with "0". 
#We did try imputation however there is no difference in the analysis results

colSums(is.na(data))

data$Maximum.amount.sanctioned.in.the.Live.loans[which(is.na(data$Maximum.amount.sanctioned.in.the.Live.loans))]<-0
data$Total.sanctioned.amount.in.the.secured.Loans.which.are.Live[which(is.na(data$Total.sanctioned.amount.in.the.secured.Loans.which.are.Live))]<-0
data$Total.sanctioned.amount.in.the.unsecured.Loans.which.are.Live[which(is.na(data$Total.sanctioned.amount.in.the.unsecured.Loans.which.are.Live))]<-0
data$Maximum.amount.sanctioned.for.any.Two.wheeler.loan[which(is.na(data$Maximum.amount.sanctioned.for.any.Two.wheeler.loan))]<-0
data$Time.since.last.Personal.loan.taken..in.months.[which(is.na(data$Time.since.last.Personal.loan.taken..in.months.))]<-0
data$Time.since.first.consumer.durables.loan.taken..in.months.[which(is.na(data$Time.since.first.consumer.durables.loan.taken..in.months.))]<-0

--------------------------------------------------------------------------------
#Renaming variables names

data.frame(colnames(data))
names(data)<-c("Customer_bounced_1stEMI", "No_of_times_bounced_last_12mons", 
               "Maximum_Month_of_business", "No_of_times_bounced_while_repayingloan",
               "EMI","Loan_amt", "Tenure","Product_code_2wheeler", "No_of_advanceEMIpaid",
               "Rate_of_Interest", "Gender","Emp_type","Res_type", "CustomerAge_duringloan",
               "No._of_loans", "No._of_secured_loans", "No._of_unsecured_loans",
               "Max_amt_sanctioned_liveloan","Total_sanctioned_amt_live_securedloans",
               "Total_sanctioned_amt_live_unsecuredloans","Max_amt_sanctioned_any2wheelerloan", 
               "Time_since_last_personal_loan_taken", "Time_since_first_consumer_durables_loan_taken",
               "No_of_times_30days_pastdue_last6months", "No_of_times_60days_pastdue_last6months", 
               "No_of_times_90days_pastdue_last3months", "Customers_location", "Target_Variable")

-------------------------------------------------------------------------------------

#Chi-Square Test: Evaluating the relationship between categorical data and target variable using chi-square test

#null hypothesis: there is no association between IVs and Target variable
#alternate hypothesis: there is association between IVs and Target variable
#if p-value is greater than 0.05 then we accept null hypothesis and drop those IVs

str(data)

chisq.test(data$Product_code_2wheeler, data$Target_Variable, simulate.p.value = TRUE)
chisq.test(data$Gender, data$Target_Variable, simulate.p.value = TRUE)
chisq.test(data$Emp_type, data$Target_Variable, simulate.p.value = TRUE)
chisq.test(data$Res_type, data$Target_Variable, simulate.p.value = TRUE)
chisq.test(data$Customers_location, data$Target_Variable, simulate.p.value = TRUE)


#from the above chi-square test, we can drop Resident_type_of_customer as p-value>alpha i.e., 0.06>0.05

data.frame(colnames(data))
data<-data[,-c(13)]

----------------------------------------------------------------------------------

#Converting categorical data into dummies

str(data)
table(data$Customer_bounced_1stEMI)
table(data$Target_Variable)

install.packages("fastDummies",dependencies = TRUE)
library(fastDummies)


data<-dummy_cols(data, select_columns = "Product_code_2wheeler", remove_first_dummy = TRUE)
data<-dummy_cols(data, select_columns = "Gender", remove_first_dummy = TRUE)
data<-dummy_cols(data, select_columns = "Emp_type", remove_first_dummy = TRUE)
data<-dummy_cols(data, select_columns = "Customers_location", remove_first_dummy = TRUE)


#Removing the original categorical columns 

data.frame(colnames(data))

data<-data[,-c(8,11,12,26)]


#Converting dummies to factor

data$Product_code_2wheeler_MO<-factor(data$Product_code_2wheeler_MO)
data$Product_code_2wheeler_RETOP<-factor(data$Product_code_2wheeler_RETOP)
data$Product_code_2wheeler_SC<-factor(data$Product_code_2wheeler_SC)
data$Product_code_2wheeler_TL<-factor(data$Product_code_2wheeler_TL)

data$Gender_MALE<-factor(data$Gender_MALE)

data$Emp_type_PENS<-factor(data$Emp_type_PENS)
data$Emp_type_SAL<-factor(data$Emp_type_SAL)
data$Emp_type_SELF<-factor(data$Emp_type_SELF)
data$Emp_type_STUDENT<-factor(data$Emp_type_STUDENT)

data$`Customers_location_TIER 2`<-factor(data$`Customers_location_TIER 2`)
data$`Customers_location_TIER 3`<-factor(data$`Customers_location_TIER 3`)
data$`Customers_location_TIER 4`<-factor(data$`Customers_location_TIER 4`)


#converting categorical data with 2 levels into factor

data$Customer_bounced_1stEMI<-factor(data$Customer_bounced_1stEMI)
data$Target_Variable<-factor(data$Target_Variable)

---------------------------------------------------------------------------------

#Filtered the number of loans and removed all "0" records from the dataset

data<-subset(data, No._of_loans>=1)

--------------------------------------------------------------------------------
  
#checking the status of positive and negative class in the below categorical or dummies

data.frame(colnames(data))

str(data)

table(data$Customer_bounced_1stEMI)
table(data$Product_code_2wheeler_MO)
table(data$Product_code_2wheeler_RETOP)
table(data$Product_code_2wheeler_SC)
table(data$Product_code_2wheeler_TL)
table(data$Gender_MALE)
table(data$Emp_type_PENS)
table(data$Emp_type_SAL)
table(data$Emp_type_SELF)
table(data$Emp_type_STUDENT)
table(data$`Customers_location_TIER 2`)
table(data$`Customers_location_TIER 3`)
table(data$`Customers_location_TIER 4`)

#we found that there were missing values in positive class (1) in the following variables:

data.frame(colnames(data))
data<-data[,-c(25,27)]

-----------------------------------------------------------------------------------------
  
#Split the data into Training & Test
  
install.packages("caret", dependencies = TRUE)
library(caret)

?createDataPartition

set.seed(123)
index<-createDataPartition(data$Target_Variable, p = 0.7, list = FALSE)

train<-data[index,]
test<-data[-index,]


#to format variables or column names 

install.packages("janitor")
library(janitor)

train<-clean_names(train)
test<-clean_names(test)

----------------------------------------------------------------------------------
  
#Logisitic Regression using Train data 
  
reg1<-glm(target_variable~., family = binomial, data = train)

#view regression results 

options(scipen = 10)
summary(reg1)

#variable importance plot

varImp(reg1)


#to check VIF of the model for multi-collinearity 

install.packages("car", dependencies = TRUE)
library(car)

vif(reg1)

#Stepwise backward Regression

reg2<-step(reg1, direction = "backward", trace = 0)

#results of stepwise reg

options(scipen = 10)
summary(reg2)
vif(reg2)

#variable importance plot
varImp(reg2)

#Predict the model and build confusion matrix with 0.50 probability
  
p1<-predict(reg1, newdata = test, type = "response")
p1<-ifelse(p1>0.50, "1", "0")
p1<-factor(p1)
confusionMatrix(p1, test$target_variable, positive = "1")

--------------------------------------------------------------------------------
#Data Balancing techniques (Oversampling, Undersampling & Both)

install.packages("ROSE")
library(ROSE)

#Over-Sampling the train data

table(train$target_variable)
50877*2

set.seed(123)
over<-ovun.sample(target_variable~., data = train, method="over", N=101754)$data

table(over$target_variable)

#Perform Logistic regression on Over sampled data

reg4<-glm(target_variable~., family = binomial, data = over)
options(scipen = 10)
summary(reg4)
exp(coef(reg4))
vif(reg4)
varImp(reg4)

#Predict the model & Build the confusion matrix with 0.50 probability

p3<-predict(reg4, newdata = test, type = "response")
p3<-ifelse(p3>0.50, "1", "0")
p3<-as.factor(p3)
confusionMatrix(p3, test$target_variable, positive = "1")

----------------------------------------------------------------------------------
#Under-Sampling the train data

table(train$target_variable)
1147*2

set.seed(123)
under<-ovun.sample(target_variable~., train, method="under", N = 2294)$data


#Perform Logistic regression on under sampled data

reg5<-glm(target_variable~., family = binomial, data = under)
options(scipen = 10)
summary(reg5)
exp(coef(reg5))
vif(reg5)


#Predict the model & Build the confusion matrix with 0.50 probability

p4<-predict(reg5, newdata = test, type = "response")
p4<-ifelse(p4>0.50, "1", "0")
p4<-as.factor(p4)
confusionMatrix(p4, test$target_variable, positive = "1" )

------------------------------------------------------------------------------------------

#Both (Over & Under) Sampling using train as sample size

set.seed(123)
both<-ovun.sample(target_variable~., train, method="both", N = 52024)$data

#Perform Logistic regression on both sampled data

reg6<-glm(target_variable~., family = binomial, data = both)
options(scipen = 10)
summary(reg6)
exp(coef(reg6))
vif(reg6)

#Predict the model & Build the confusion matrix with 0.50 probability

p5<-predict(reg6, newdata = test, type = "response")
p5<-ifelse(p5>0.50, "1", "0")
p5<-as.factor(p5)
confusionMatrix(p5, test$target_variable, positive = "1")

-------------------------------------------------------------------------------------

#Decision Tree on Over sampled data  
  
install.packages("rpart", dependencies = TRUE)
library(rpart)

install.packages("rpart.plot")
library(rpart.plot)

library(RColorBrewer)
library(rattle)

?ctree
tree1<-rpart(target_variable~., data = over)

#view results of decision tree
tree1
summary(tree1)

#visualization of tree
plot(tree1)
rpart.plot(tree1, box.palette = "RdBu", shadow.col = "gray", nn = TRUE)

#or

fancyRpartPlot(tree1, cex = .90)
varImp(tree1)

#Predict the model & Build the confusion matrix

p6<-predict(tree1, newdata = test, type="class")
confusionMatrix(p6, test$target_variable, positive = "1")

--------------------------------------------------------------------------------

#Decision Tree via 10-fold Cross validation on over sampled data 

custom<-trainControl(method = "repeatedcv", number = 10, repeats = 5)

set.seed(123)
tree2<-train(target_variable~., data = over, method = "rpart", 
             trControl = custom, tuneLength = 10)

plot(tree2)
tree2
tree2$results
plot(varImp(tree2))

#Predict the model & Build the confusion matrix

p7<-predict(tree2, newdata = test)
confusionMatrix(p7, test$target_variable, positive = "1")

---------------------------------------------------------------------------------
#Random Forest on over-sampled data
  
install.packages("randomForest")
library(randomForest)

#build random forest 

rf1<-randomForest(target_variable~., data = over)
summary(rf1)
rf1

#variable importance plot
varImpPlot(rf1)

#to visualize the number of trees (ntree) and OOB error rate
plot(rf1)

#Predict the model & Build the confusion matrix
p8<-predict(rf1, test)
confusionMatrix(p8, test$target_variable, positive = "1")


#Tuning RF with number of trees (refer plot(rf)) and give mtry as random number
#ntree=sqrt(16)=4

tuneRF(over[,-23], over[,23], stepFactor = 2, plot = TRUE, 
       ntreeTry = 430, mtry = 8, improve = 0.05)


#Rebuild Random forest with mtry obtained from tuneRF

rf2<-randomForest(target_variable~., data = over, ntree = 430, mtry = 8)
summary(rf2)
rf2
varImpPlot(rf2)
plot(rf2)

#Predict the model & Build the confusion matrix
p9<-predict(rf2, test)
confusionMatrix(p9, test$target_variable, positive = "1")

----------------------------------------------------------------------------------------

#Boosting 
  
str(train)
train1<-dummy_cols(train, remove_first_dummy = TRUE, 
                   remove_selected_columns = TRUE)

test1<-dummy_cols(test, remove_first_dummy = TRUE, 
                  remove_selected_columns = TRUE)


train_label<-as.numeric(train1[,23])
test_label<-as.numeric(test1[,23])

train_features<-train1[,-c(23)]
test_features<-test1[,-c(23)]


## convert both test and train data-sets (IVs) into matrix

train_features<-as.matrix(train_features)
test_features<-as.matrix(test_features)



install.packages("xgboost")
library(xgboost)

?xgboost

#state the parameters
parameters <- list(eta = 0.3,
                   max_depth = 2,
                   subsample = 1,
                   colsample_bytree = 1,
                   min_child_weight = 1,
                   gamma = 0,
                   eval_metric = "auc",
                   objective = "binary:logistic",
                   booster = "gbtree",
                   scale_pos_weight = 44)

#scale_pos_weight, [default=1] 
#It control the balance of positive and negative weights, #useful for 
#unbalanced classes. 
#A typical value to consider: sum(negative cases)/sum(positive cases) 
#for example, here in our training data: 50879/1145 = 44.4 ~ 44       

table(train1$target_variable_1)
50877/1147

#Running XGBoost 

#label is dv, data is only ivs, nrounds = number of boosting iterations

model1 <- xgboost(data = train_features, label = train_label, set.seed(1234),
                  nthread = 6, nround = 200, params = parameters,
                  print_every_n = 50, early_stopping_rounds = 20, verbose = 1)

#Predictions
predictions<-predict(model1, newdata = test_features)
predictions1<-ifelse(predictions> 0.5, 1, 0)

#Checking accuracy
confusionMatrix(table(predictions1, test_label), positive = "1")
--------------------------------------------------------------------------------

#using cross validation method

library(caret)
tune_control <- trainControl(method = "cv",
                             number = 5,
                             allowParallel = TRUE)
#setting the parameter grid
tune_grid <- expand.grid(nrounds = seq(200, to = 1800, by = 200),
                         eta = c(0.05, 0.3),
                         max_depth = c(2,4,6,8),
                         subsample = c(0.9, 1),
                         colsample_bytree = 1,
                         min_child_weight = 1,
                         gamma = 0)

#cross validation and parameter tuning start
start <- Sys.time()
xgb_tune <- train(x = train_features,
                  y = factor(train_label),
                  method = "xgbTree",
                  trControl = tune_control,
                  tuneGrid = tune_grid)
end <- Sys.time()

#retrieve the results
xgb_tune$bestTune
View(xgb_tune$results)

predictions2<-predict(xgb_tune, newdata = test_features)
confusionMatrix(table(predictions2, test_label), positive = "1")
