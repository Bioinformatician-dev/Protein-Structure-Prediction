# Install and load required packages
install.packages("caret")           # Uncomment if not installed
install.packages("randomForest")    # Uncomment if not installed
install.packages("stringr")         # Uncomment if not installed
library(caret)
library(randomForest)
library(stringr)

# Step 1: Load and prepare your data
# Simulated example: replace with actual data loading
set.seed(42)  # For reproducibility

# Sample data creation (replace with actual loading)
df <- data.frame(
  sequence = c("HHEECC", "HHEEHC", "CCHEEH", "ECCEHC", "HCCEEH", "EHHCCG"),
  structure = c("H", "H", "E", "C", "C", "H")  # H = Helix, E = Strand, C = Coil
)

# Step 2: Encode sequences into features
encode_sequence <- function(seq) {
  feature_vector <- table(factor(unlist(str_split(seq, "")), levels = c("A", "C", "D", "E", "F", "G", "H", "I", "K", "L", "M", "N", "P", "Q", "R", "S", "T", "V", "W", "Y")))
  return(as.numeric(feature_vector))
}

# Encode all sequences into a feature matrix
features <- do.call(rbind, lapply(df$sequence, encode_sequence))

# Combine features with the labels
feature_df <- data.frame(features)
feature_df$structure <- df$structure

# Step 3: Data splitting
set.seed(42)  # For reproducibility
train_index <- createDataPartition(feature_df$structure, p = 0.8, list = FALSE)
train_data <- feature_df[train_index, ]
test_data <- feature_df[-train_index, ]

# Step 4: Model training
model <- train(structure ~ ., data = train_data, method = "rf", 
               trControl = trainControl(method = "cv", number = 10))

# Step 5: Model evaluation
predictions <- predict(model, newdata = test_data)

# Confusion Matrix
confusion_matrix <- confusionMatrix(predictions, test_data$structure)
print(confusion_matrix)

# Step 6: Feature importance
importance <- varImp(model, scale = FALSE)
print(importance)

# Optional: Plot the feature importance
plot(importance, main = "Feature Importance")
