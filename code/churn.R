library(readr)
churn=read.csv(file.choose())
View(churn)
attach(churn)
churn$RowNumber=NULL
churn$CustomerId=NULL
churn$Surname=NULL
churn$Geography
for(unique_value in unique(churn$Gender)){
  
  
  churn[paste("Gender", unique_value, sep = ".")] <- ifelse(churn$Gender== unique_value, 1, 0)
  
}

for(unique_value in unique(churn$Geography)){
  
  
  churn[paste("country", unique_value, sep = ".")] <- ifelse(churn$Geography== unique_value, 1, 0)
  
}
churn$Gender=NULL
churn$Geography=NULL
churn$Gender.Female=NULL
churn$country.France=NULL

target_Variable <- churn$Exited
independent_Variables <-  subset(churn, select = -c(Exited))


library(vegan)
# Note: To standardize the data using 'Range' method
independent_Variables <-  decostand(independent_Variables,"range")

churn<-  data.frame(independent_Variables, Exited = target_Variable)
summary(churn)
rm(independent_Variables, target_Variable)
set.seed(123)
num_Records <-  nrow(churn)

train_Index <-  sample(1:num_Records, round(num_Records * 0.7, digits = 0))
train_Data <-  churn[train_Index,] 
test_Data <-  churn[-train_Index,] 
rm(train_Index, num_Records)



table(churn$Exited)
prop.table(table(churn$Exited))

table(train_Data$Exited)
prop.table(table(train_Data$Exited))

table(test_Data$Exited)
prop.table(table(test_Data$Exited))
colnames(churn)
library(neuralnet)
nn=neuralnet(Exited~ CreditScore + Age+Tenure+Balance+NumOfProducts+HasCrCard+IsActiveMember+EstimatedSalary+Gender.Male+country.Spain+country.Germany,data=train_Data, hidden=4,linear.output = F,stepmax=1e6)
plot(nn)



test_Data_No_Target <-  subset(test_Data, select=-c(Exited))

# Predict 
nn_predict <- compute(nn, covariate = test_Data_No_Target)
rm(test_Data_No_Target)

# View the predicted values
nn_predict$net.result

# Compute confusion matrix and accuracy
predicted <-  factor(ifelse(nn_predict$net.result > 0.5, 1, 0))

conf_Matrix <-  table(test_Data$Exited, predicted)
conf_Matrix

# Accuracy
sum(diag(conf_Matrix))/sum(conf_Matrix)*100

# Error = 1 - Accuracy
100 - sum(diag(conf_Matrix))/sum(conf_Matrix)*100

############################################################################

test_Data_No_Target <-  subset(train_Data, select=-c(Exited))

# Predict 
nn_predict <- compute(nn, covariate = test_Data_No_Target)
rm(test_Data_No_Target)

# View the predicted values
nn_predict$net.result

# Compute confusion matrix and accuracy
predicted <-  factor(ifelse(nn_predict$net.result > 0.5, 1, 0))

conf_Matrix <-  table(train_Data$Exited, predicted)
conf_Matrix

# Accuracy
sum(diag(conf_Matrix))/sum(conf_Matrix)*100

# Error = 1 - Accuracy
100 - sum(diag(conf_Matrix))/sum(conf_Matrix)*100
