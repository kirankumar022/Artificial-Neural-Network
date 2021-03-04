library(readr)
cancer=read.csv(file.choose())
summary(cancer)
attach(cancer)
cancer$id=NULL
library(dummies)
ed <-  dummy(cancer$diagnosis)
cancer <-  subset(cancer, select=-c(diagnosis)) 
cancer <-  cbind(cancer, ed)
rm(ed)
cancer$X=NULL
cancer$diagnosisM=NULL
str(cancer)
target_Variable <-  cancer$diagnosisB
independent_Variables <-  subset(cancer, select = -c(diagnosisB))

library(vegan)
# Note: To standardize the data using 'Range' method
independent_Variables <-  decostand(independent_Variables,"range")

cancer<-  data.frame(independent_Variables, diagnosisB = target_Variable)
summary(cancer)
rm(independent_Variables, target_Variable)
set.seed(123)
num_Records <-  nrow(cancer)
train_Index <-  sample(1:num_Records, round(num_Records * 0.7, digits = 0))
train_Data <-  cancer[train_Index,] 
test_Data <-  cancer[-train_Index,] 
rm(train_Index, num_Records)

table(cancer$diagnosisB)
prop.table(table(cancer$diagnosisB))

table(train_Data$diagnosisB)
prop.table(table(train_Data$diagnosisB))

table(test_Data$diagnosisB)
prop.table(table(test_Data$diagnosisB))
colnames(cancer)
library(neuralnet)
nn=neuralnet(diagnosisB~ radius_mean+texture_mean+perimeter_mean+area_mean+
                         smoothness_mean+compactness_mean+concavity_mean+concave.points_mean+symmetry_mean+fractal_dimension_mean+radius_se+texture_se+
                         perimeter_se+area_se+smoothness_se+compactness_se+concavity_se+concave.points_se+symmetry_se+fractal_dimension_se+
                         radius_worst+texture_worst+perimeter_worst+area_worst+smoothness_worst+compactness_worst+concavity_worst+concave.points_worst+
                         symmetry_worst+fractal_dimension_worst,data=train_Data, hidden=4,linear.output = F,stepmax=1e6)
plot(nn)



test_Data_No_Target <-  subset(test_Data, select=-c(diagnosisB))

# Predict 
nn_predict <- compute(nn, covariate = test_Data_No_Target)
rm(test_Data_No_Target)

# View the predicted values
nn_predict$net.result

# Compute confusion matrix and accuracy
predicted <-  factor(ifelse(nn_predict$net.result > 0.5, 1, 0))

conf_Matrix <-  table(test_Data$diagnosisB, predicted)
conf_Matrix

# Accuracy
sum(diag(conf_Matrix))/sum(conf_Matrix)*100

# Error = 1 - Accuracy
100 - sum(diag(conf_Matrix))/sum(conf_Matrix)*100

