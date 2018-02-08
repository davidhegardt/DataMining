############# Cross Validation ###############
# Read the data from file
# Download "Heart Disease" dataset from UCI Machine Learning Repository
# URL https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data 
# Find details of data set in https://archive.ics.uci.edu/ml/datasets/Heart+Disease 

heart.disease.db <- read.table(file = "https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data",
                               header = FALSE, sep = ",", na.strings = "?", strip.white = TRUE, 
                               stringsAsFactors = FALSE)

head(heart.disease.db)
# details of the dataset features
# -------------------------------- #
# sex       = (0 is female, 1 is male)
# cp        = chest pain type (1 -> typical angina,  2 -> atypical angina,  3 -> non-anginal, 4 -> asymptomatic)
# trestbps  = resting blood pressure
# chol      = serum cholestral in mg/dl
# fbs       = fasting blood sugar > 120 mg/dl is 1 otherwise 0
# restecg   = resting electrocardiographic result, 0 -> normal, 1 -> St-T wave abnormality, 2 -> probable or definite hypertropy
# thalach   = maximum heart rate achieved
# exang     = exercise induced angina (1 = yes, 0 = no)
# oldpeak   = ST depression induced by exercise relative to rest
# slope     = the slope of the peak exercise ST segment (1 -> upslopping, 2 -> flat, 3 -> downslopping)
# ca        = number of major vessels (0-3) covered by flourosopy
# thal      = (3 -> normal, 6 -> fixed defect, 7 -> reversible defect)
# class     = diagnosis of heart disease
# ---------------------------------- #

colnames(heart.disease.db) <- c("age", "sex", "cp", "trestbps", "chol",
                                "fbs", "restecg", "thalach", "exang",
                                "oldpeak", "slope", "ca", "thal", "class")


# Check for missing values in each column and remove the examples with missing values
apply(heart.disease.db, 2, function(x) sum(is.na(x)))
heart.disease.db <- na.omit(heart.disease.db)

# Format data properly
heart.disease.db$sex <- ifelse(heart.disease.db$sex == 1, "Male", "Female")
heart.disease.db$sex <- as.factor(heart.disease.db$sex)

heart.disease.db$cp <- as.factor(heart.disease.db$cp)

heart.disease.db$fbs <- ifelse(heart.disease.db$fbs == 1, ">120", "<120")
heart.disease.db$fbs <- as.factor(heart.disease.db$fbs)

heart.disease.db$exang <- ifelse(heart.disease.db$exang == 1, "yes", "no")
heart.disease.db$exang <- as.factor(heart.disease.db$exang)

heart.disease.db$slope <- as.factor(heart.disease.db$slope)

heart.disease.db$thal <- as.factor(heart.disease.db$thal)

# for all the patients with heart diseases, change the class label to 1 (combine 1,2,3 and 4 to 1)
# 1 = Heart Disease
# 0 = No heart Disease
index.heart.disease <- (heart.disease.db$class != 0) 
heart.disease.db$class[index.heart.disease] <- 1
heart.disease.db$class <- as.factor(heart.disease.db$class)


# Training and Testing model
# --------------------------
# Train and test SVM on this data, use "radial" kernel
# Change cost parameter from 0.1, 1, 10 and 100
# Perform 10 fold cross validation and compute average accuracy for each value of cost.
# Decide what value of cost parameter gives the best SVM model 

library("caret")
library("e1071")

set.seed(105)
num.folds <- 10

# create 10 fold using createFolds() function from caret package

k.fold.data.idx <- createFolds(heart.disease.db$class, k = num.folds)

# cost parameters to vary
cost.parameter <- c(0.1, 1, 10, 100)
# average cross validation accuracy for each cost value
cv.accuracy.cost <- numeric(length(cost.parameter))

for(i in 1:length(cost.parameter)){
  
  # vector to hold accuracy for each fold
  fold.accuracy <- numeric(num.folds)
  for(j in 1:num.folds){
    
    test.set.idx <- k.fold.data.idx[[j]]
    test.set = heart.disease.db[test.set.idx,]
    train.set = heart.disease.db[-test.set.idx,]
    
    # use svm() function in e1071 package to train SVM
    
    svm.model <- svm(class ~., data = train.set, kernel = "radial", cost = cost.parameter[i], probability = TRUE)
    
    svm.pred <- predict(svm.model, newdata = test.set)
    # accuracy on a test fold
    fold.accuracy[j] <- mean(svm.pred == test.set$class)
  }
  
  # average accuracy for a given cost value
  cv.accuracy.cost[i] <- mean(fold.accuracy)
}

# print average accuracy for varying cost parameter
print(cbind("cost.par" = cost.parameter, "avg.accuracy" = cv.accuracy.cost))


# ANSWER : ################################################################
# The best cost value to use is 1.0 and gives an avg.accuracy of 0.83528474


# For this part, randomly select 80% of data for training and test on remaining data
# Plot ROC curve with AUC value for positive class (i.e. 1)

set.seed(113)
train.idx <- sample(1:nrow(heart.disease.db), size = round(0.8 * nrow(heart.disease.db)), replace = FALSE)

train.set <- heart.disease.db[train.idx,]
test.set <- heart.disease.db[-train.idx,]

svm.model <- svm(formula = class ~ ., data = train.set, kernel = "radial", cost = 1, probability = TRUE)

# plotting ROC
library("ROCR")
# predict on test examples using trained SVM model

svm.pred <- predict(svm.model, newdata = test.set, probability = TRUE)

class.probs <- attr(svm.pred, "probabilities")
pred2 <- prediction(class.probs[,2], test.set$class)

roc.curve <- performance(pred2, "tpr", "fpr")
plot(roc.curve)
lines(x = c(0,1), y = c(0,1))

# AUC (Area Under ROC)
auc <- unlist(attr(performance(pred2, "auc"), "y.values"))
print(auc)
legend("bottomright", sprintf("%.3f",auc), title = "AUC")