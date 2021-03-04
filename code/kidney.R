library(readr)
kidney=read.csv(file.choose())
View(kidney)
summary(kidney)
library(vegan)
attach(kidney)
target_Variable <- kidney$Outcome
independent_Variables <-  subset(kidney, select = -c(Outcome))

independent_Variables <-  decostand(independent_Variables,"range")

kidney<-  data.frame(independent_Variables, Exited = target_Variable)
summary(kidney)
View(kidney)

rm(independent_Variables, target_Variable)
set.seed(123)
num_Records <-  nrow(kidney)

train_Index <-  sample(1:num_Records, round(num_Records * 0.7, digits = 0))
train_Data <-  kidney[train_Index,] 
test_Data <-  kidney[-train_Index,] 
rm(train_Index, num_Records)

table(kidney$Exited)
prop.table(table(kidney$Exited))

table(train_Data$Exited)
prop.table(table(train_Data$Exited))

table(test_Data$Exited)
prop.table(table(test_Data$Exited))
colnames(kidney)

library(neuralnet)
nn <-  neuralnet(Exited~ Pregnancies+Glucose+BloodPressure+SkinThickness+           
                        Insulin+BMI+DiabetesPedigreeFunction+Age,
                        data=train_Data, hidden=3,linear.output = F,stepmax=1e6)

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
