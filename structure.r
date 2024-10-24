# Load necessary libraries
library(caret)
library(e1071)  # For SVM
library(randomForest)  # For Random Forest

# Load dataset (assuming a dataset named 'protein_data' is available)
# The dataset should have features representing protein sequences and a target variable for secondary structure
data(protein_data)

# Split the dataset into training and testing sets
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(protein_data$secondary_structure, p = .8, 
                                  list = FALSE, 
                                  times = 1)
proteinTrain <- protein_data[trainIndex, ]
proteinTest  <- protein_data[-trainIndex, ]

# Train a Random Forest model
rf_model <- train(secondary_structure ~ ., data = proteinTrain, method = "rf")

# Train a Support Vector Machine model
svm_model <- train(secondary_structure ~ ., data = proteinTrain, method = "svmRadial")

# Make predictions on the test set using Random Forest
rf_predictions <- predict(rf_model, newdata = proteinTest)

# Make predictions on the test set using SVM
svm_predictions <- predict(svm_model, newdata = proteinTest)

# Evaluate the models
rf_confusion <- confusionMatrix(rf_predictions, proteinTest$secondary_structure)
svm_confusion <- confusionMatrix(svm_predictions, proteinTest$secondary_structure)

# Print evaluation results
print(rf_confusion)
print(svm_confusion)
