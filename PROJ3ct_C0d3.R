#Loading the data
library(readr)
articles <- read_csv("~/PROJ3Ct/DATAS3T/articles.csv/articles.csv")
View(articles)

customers <- read_csv("~/PROJ3Ct/DATAS3T/customers.csv/customers.csv")
View(customers)

transactions_train <- read_csv("~/PROJ3Ct/DATAS3T/transactions_train.csv/transactions_train.csv")

#loading packages for cleaning data
install.packages("dplyr")
install.packages("tidyr")
install.packages("lubridate")  

library(dplyr)
library(tidyr)
library(lubridate)

# Function to count NA values in each column
check_na <- function(data) {
  sapply(data, function(x) sum(is.na(x)))
}

# Use the function on the dataframes
na_transactions_train <- check_na(transactions_train)
na_customers <- check_na(customers)
na_articles <- check_na(articles)

# Printing the results
print("NA counts in transactions_train:")
print(na_transactions_train)

print("NA counts in customers:")
print(na_customers)

print("NA counts in articles:")
print(na_articles)


# Removing null values
# For customers, impute missing ages with median or mode as appropriate
customers$age <- ifelse(is.na(customers$age), median(customers$age, na.rm = TRUE), customers$age)
customers <- customers %>%
  filter(!is.na(Active))

customers <- customers %>%
  filter(!is.na(fashion_news_frequency))
         
customers <- customers %>%
  filter(!is.na(club_member_status))

customers <- customers %>%
  filter(!is.na(FN)

# For articles remove rows where essential information is missing
articles <- articles %>%
  filter(!is.na(detail_desc))

# Enhanced function to include percentages
check_na_detailed <- function(data) {
  na_count <- sapply(data, function(x) sum(is.na(x)))
  na_percentage <- sapply(data, function(x) mean(is.na(x)) * 100)
  data.frame(Count = na_count, Percentage = na_percentage)
}

# Apply this detailed check
detailed_na_transactions_train <- check_na_detailed(transactions_train)
detailed_na_customers <- check_na_detailed(customers)
detailed_na_articles <- check_na_detailed(articles)

# Print detailed results
print("Detailed NA counts in transactions_train:")
print(detailed_na_transactions_train)

print("Detailed NA counts in customers:")
print(detailed_na_customers)

print("Detailed NA counts in articles:")
print(detailed_na_articles)

# Convert date columns
transactions_train$date <- as.Date(transactions_train$t_dat, format = "%Y-%m-%d")

# Convert categorical data
customers$club_member_status <- factor(customers$club_member_status)
articles$product_type_name <- factor(articles$product_type_name)

#Filtering out some rows
transactions_train <- transactions_train %>%
  filter(year(t_dat) >= 2020)

articles <- articles %>%
  filter(index_group_name %in% c("Ladieswear", "Menswear"))

# Check unique values to ensure only Ladieswear and Menswear are present
unique(articles$index_group_name)

# You may want to ensure the key columns are of the same type and have no trailing whitespaces
transactions_train$customer_id <- as.character(transactions_train$customer_id)
articles$article_id <- as.character(articles$article_id)
customers$customer_id <- as.character(customers$customer_id)

transactions_train$customer_id <- trimws(transactions_train$customer_id)
articles$article_id <- trimws(articles$article_id)
customers$customer_id <- trimws(customers$customer_id)

# Ensure the keys are unique
if(!all(unique(nrow(customers) == nrow(unique(customers$customer_id))))) {
  stop("customer_id in customers is not unique")
}

if(!all(unique(nrow(articles) == nrow(unique(articles$article_id))))) {
  stop("article_id in articles is not unique")
}

# Check for matching key values
unique_customers_in_transactions <- unique(transactions_train$customer_id)
if(!all(unique_customers_in_transactions %in% customers$customer_id)) {
  stop("Not all customer_id values in transactions_train have a match in customers")
}

unique_articles_in_transactions <- unique(transactions_train$article_id)
if(!all(unique_articles_in_transactions %in% articles$article_id)) {
  stop("Not all article_id values in transactions_train have a match in articles")
}

# After running these checks, proceed with the merge if all checks pass
# Add leading zeros to article_id in articles dataset to match the format in transactions_train
articles$article_id <- sprintf("%010d", as.numeric(articles$article_id))

#Identifying any discrepancies
# Look at some sample IDs from transactions_train
head(transactions_train$article_id)

# Compare with some sample IDs from articles
head(articles$article_id)

# Add leading zeros to article_id in articles dataset to match the format in transactions_train
articles$article_id <- sprintf("%010d", as.numeric(articles$article_id))

# Check the head of articles to ensure it's been done correctly
head(articles$article_id)

# Verify the intersection again with the reformatted article_id
common_article_ids <- intersect(unique(transactions_train$article_id), unique(articles$article_id))
length(common_article_ids)

# If common_article_ids is now greater than 0, proceed with the merge
if(length(common_article_ids) > 0) {
  full_data <- transactions_train %>%
    full_join(customers, by = "customer_id") %>%
    full_join(articles, by = "article_id")
}

# Remove rows with NA in any column
full_data <- full_data[complete.cases(full_data), ]

#Creating new variables
# Function to map month to season
get_season <- function(month) {
  if (month %in% c(12, 1, 2)) {
    return("Winter")
  } else if (month %in% 3:5) {
    return("Spring")
  } else if (month %in% 6:8) {
    return("Summer")
  } else if (month %in% 9:11) {
    return("Autumn")
  } else {
    return(NA) # In case of invalid data
  }
}

# Apply the function to the transaction date to create a new season column
full_data <- full_data %>%
  mutate(
    month = month(ymd(t_dat)), # Convert transaction date to month number
    season = sapply(month, get_season) # Apply the get_season function to each month
  )

# Assuming full_data already contains the merged information
full_data <- full_data %>%
  mutate(
    # Calculate average spend per transaction for each customer
    avg_spend = ave(price, customer_id, FUN = mean),
    
    # Calculate the number of unique product types each customer has bought
    prod_diversity = ave(product_type_no, customer_id, FUN = function(x) length(unique(x))),
    
    # Create a variable for the preferred colour for each customer
    preferred_colour = colour_group_name[which.max(table(colour_group_name))]
  )

# Product Affinity Score based on product_type_no
affinity_score <- full_data %>%
  group_by(customer_id, product_type_no) %>%
  summarise(affinity = n()) %>%
  ungroup()

# Customer Segments based on age and club member status
# This is a placeholder; actual segmentation would typically use a clustering algorithm
full_data <- full_data %>%
  mutate(segment = case_when(
    age >= 18 & age <= 25 & club_member_status == "ACTIVE" ~ "Young_Active",
    age > 25 & club_member_status == "ACTIVE" ~ "Adult_Active",
    TRUE ~ "Other"
  ))

#Get the most recent date in your dataset as a reference point for recency calculations
reference_date <- max(full_data$t_dat) + days(1) # Add one day to ensure recency is >= 1

# Calculate RFM metrics for each customer
rfm_data <- full_data %>%
  group_by(customer_id) %>%
  summarise(
    Recency = as.numeric(difftime(reference_date, max(t_dat), units = "days")),
    Frequency = n_distinct(t_dat), # Count of unique transaction dates for frequency
    Monetary = sum(price) # Total spend for monetary value
  ) %>%
  ungroup()

# Assign quartiles to each metric
rfm_data <- rfm_data %>%
  mutate(
    R_Quartile = ntile(Recency, 4),
    F_Quartile = ntile(Frequency, 4),
    M_Quartile = ntile(Monetary, 4)
  )

# Invert the R_Quartile since lower recency is better
rfm_data$R_Quartile <- 5 - rfm_data$R_Quartile

# Combine the quartiles in a single score
rfm_data <- rfm_data %>%
  mutate(RFM_Score = R_Quartile * 100 + F_Quartile * 10 + M_Quartile)

rfm_data <- rfm_data %>%
  mutate(
    Segment = case_when(
      R_Quartile >= 3 & F_Quartile >= 3 & M_Quartile >= 3 ~ "Top Customers",
      F_Quartile >= 3 ~ "Loyal Customers",
      M_Quartile >= 3 ~ "Big Spenders",
      R_Quartile >= 3 & F_Quartile <= 2 & M_Quartile <= 2 ~ "Almost Lost",
      R_Quartile <= 2 & F_Quartile <= 2 & M_Quartile <= 2 ~ "Lost Customers",
      TRUE ~ "Others"
    )
  )

# Load necessary library
library(randomForest)

# Assuming 'full_name' is your dataset and 'sales' is the target variable
# Ensure that any necessary pre-processing like handling of NA values, factor conversion has been done

# Split the data into training and testing sets
set.seed(123)  # for reproducibility
indices <- sample(1:nrow(full_data), size = 0.7 * nrow(full_data))  # 70% for training
train_data <- full_name[indices, ]
test_data <- full_name[-indices, ]

# Train the Random Forest model
rf_model <- randomForest(sales ~ ., data = train_data, ntree = 500, mtry = sqrt(ncol(train_data) - 1), importance = TRUE)

# Summary of the model
print(rf_model)

# Predicting on the test set
predictions <- predict(rf_model, newdata = test_data)

# Evaluating the model
actual <- test_data$sales
mse <- mean((predictions - actual)^2)
print(paste("Mean Squared Error:", mse))

# Calculate other performance metrics as needed, such as RMSE, MAE, or R-squared
rmse <- sqrt(mse)
mae <- mean(abs(predictions - actual))
r_squared <- 1 - sum((predictions - actual)^2) / sum((actual - mean(actual))^2)

print(paste("Root Mean Squared Error:", rmse))
print(paste("Mean Absolute Error:", mae))
print(paste("R-squared:", r_squared))

# Feature importance
importance <- importance(rf_model)
varImpPlot(rf_model)

#Creating a trend column
install.packages("zoo")
library(zoo)

library(dplyr)
library(lubridate)

# Aggregate sales by day for the entire dataset
daily_sales <- full_data %>%
  group_by(Days = as.numeric(difftime(ymd(t_dat), min(ymd(t_dat)), units = "days"))) %>%
  summarise(total_sales = sum(price), .groups = 'drop')

# Fit a linear model to these aggregated daily sales
trend_model <- lm(total_sales ~ Days, data = daily_sales)

# Add the trend values to the daily_sales dataframe
daily_sales <- daily_sales %>%
  mutate(Trend = predict(trend_model, newdata = daily_sales))

# Plot the trend with ggplot2
library(ggplot2)

ggplot(daily_sales, aes(x = Days, y = total_sales)) +
  geom_point() +
  geom_line(aes(y = Trend), color = "blue")

#Review the model summary
summary(trend_model)

# Plot residuals
ggplot(data = daily_sales, aes(x = Days, y = residuals(trend_model))) + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_point() +
  labs(title = "Residuals of Linear Model", x = "Days", y = "Residuals")

install.packages("caret")
install.packages("e1071")
library(caret)
library(e1071)

# Assuming full_data is already preprocessed and ready for modeling
# Assuming 'article_purchased' is the binary target variable

# Split the data into training and test sets
set.seed(123) # For reproducibility
index <- createDataPartition(full_data$article_id, p = 0.8, list = FALSE)
train_data <- full_data[index, ]
test_data <- full_data[-index, ]

length(unique(train_data$article_id))
train_data$article_id <- as.factor(train_data$article_id)

install.packages("mnet")
library(mnet)

# Fit a multinomial logistic regression model
multinom_model <- multinom(article_id ~ ., data = train_data)

install.packages("randomForest")

library(dplyr)

class_distribution <- train_data %>%
  group_by(product_group_name) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100) %>%
  arrange(desc(Count))

# Create .Renviron file in the user's home directory if it doesn't exist
path_to_renviron <- file.path(Sys.getenv("HOME"), ".Renviron")
if (!file.exists(path_to_renviron)) {
  file.create(path_to_renviron)
}

# Write the RTools path to .Renviron
writeLines('PATH="C:\\rtools44\\usr\\bin;${PATH}"', con = path_to_renviron)

library(DMwR)
install.packages("smotefamily")

# Convert product_group_name to factor
train_data$product_group_name <- as.factor(train_data$product_group_name)

# Check to ensure conversion was successful
is.factor(train_data$product_group_name)  # This should return TRUE

#Use down-sampling within caret
down_trainData <- downSample(x = train_data[, -which(names(train_data) == "product_group_name")],
                             y = train_data$product_group_name)

# Check results
table(down_trainData$product_group_name)  # This should show more balanced class frequencies

# Setting up training control
fitControl <- trainControl(
  method = "repeatedcv",  # Using repeated cross-validation
  number = 10,            # Number of folds in the cross-validation
  repeats = 3,            # Number of repeats
  savePredictions = "final",
  classProbs = TRUE       # If you need probability estimates
)

#Train  model with the balanced dataset
gbmModel <- train(
  product_group_name ~ .,
  data = down_trainData,
  method = "gbm",
  trControl = fitControl,
  verbose = FALSE
)




# Assuming 'frequency' is your data frame with the article_id, Count, and Percentage columns
cumulative_percentage <- sum(frequency$Percentage[1:600])

# This gives you the cumulative percentage of the top 600 article IDs
print(cumulative_percentage)

cutoff_percentage <- 100 - cumulative_percentage

# Creating Features
# First, create the binary indicators in the train_data
train_data <- train_data %>%
  mutate(
    IsLadieswear = as.integer(index_group_name == "Ladieswear"),
    IsMenswear = as.integer(index_group_name == "Menswear"),
    IsYoungMember = as.integer(segment == "Young"),
    IsOldMember = as.integer(segment == "Old")
  )

# Now, create the article_profiles with the aggregated binary features
article_profiles <- train_data %>%
  group_by(article_id) %>%
  summarise(
    TotalSales = sum(price),
    AverageSales = mean(price),
    PurchaseFrequency = n_distinct(prod_diversity),
    ActiveStatus = n_distinct(segment), 
    .groups = 'drop'
  )

# Normalize the features
article_profiles <- as.data.frame(scale(article_profiles[, -1]))

# Check for NA, NaN, or Inf values
summary(article_profiles)
any(is.na(article_profiles))
any(is.nan(article_profiles))
any(is.infinite(article_profiles))

# Remove or impute NA values
# Simple example: remove rows with any NAs
article_profiles <- na.omit(article_profiles)

# Scaling the data can sometimes help
article_profiles[, -1] <- scale(article_profiles[, -1])

install.packages("factoextra")
library(factoextra)

#gap statistics method
set.seed(123)  # For reproducibility
gap_stat <- clusGap(article_profiles[, -1], FUNcluster = kmeans, K.max = 10, B = 100)

#plot gap stats
fviz_gap_stat(gap_stat)


#silhouette method
sil_width <- sapply(2:10, function(k) {
  km.res <- kmeans(article_profiles[, -1], centers = k, nstart = 25)
  silhouette_width <- silhouette(km.res$cluster, dist(article_profiles[, -1]))
  mean(silhouette_width[, "sil_width"])
})

# Determine the optimal number of clusters based on the maximum silhouette width
optimal_clusters <- which.max(sil_width)

plot(2:10, sil_width, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters k", 
     ylab = "Average silhouette width")

set.seed(123)  # For reproducibility
kmeans_result <- kmeans(article_profiles[, -1], centers = 5)
article_profiles$Cluster <- kmeans_result$cluster

# Adding product group name for prediction
article_to_product_group <- train_data %>%
  select(article_id, product_group_name) %>%
  distinct()

article_profiles <- article_profiles %>%
  left_join(article_to_product_group, by = "article_id")

article_profiles$product_group_name <- as.factor(article_profiles$product_group_name)


# Converting categorical variables to factors
article_profiles$Cluster <- as.factor(article_profiles$Cluster)

# Making product_group_name the target variable
article_profiles$product_group_name <- as.factor(article_profiles$product_group_name)

library(caret)
set.seed(123)  # For reproducibility


# Assuming 'article_profiles' includes 'product_group_name' as a column
# We exclude it from the data used for clustering
clustering_data <- article_profiles[, !names(article_profiles) %in% 'product_group_name']

kmeans_result <- kmeans(clustering_data, centers = 4)

article_profiles$Cluster <- as.factor(kmeans_result$cluster)

# Exclude non-predictive variables such as 'article_id' if present
predictive_data <- article_profiles[, !names(article_profiles) %in% c('article_id')]

# Convert the target variable to a factor for classification
predictive_data$product_group_name <- as.factor(predictive_data$product_group_name)

set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(predictive_data$product_group_name, p = 0.8, list = FALSE)

trainSet <- predictive_data[trainIndex, ]
testSet <- predictive_data[-trainIndex, ]

rf_model <- randomForest(product_group_name ~ ., data = trainSet, ntree = 500)

# Predicting
predictions <- predict(rf_model, testSet)

# Evaluating model performance
confusionMatrix(predictions, testSet$product_group_name)

install.packages("ROSE")
library(ROSE)

#Gbm Model
install.packages("gbm")
library(gbm)
install.packages("MLmetrics")
library(MLmetrics)

# Ensure product_group_name is in the dataset and is a factor
if(!"product_group_name" %in% names(train_data)) {
  stop("product_group_name is not in the dataset")
}
if(!is.factor(train_data$product_group_name)) {
  train_data$product_group_name <- as.factor(train_data$product_group_name)
}

set.seed(123)  # Ensure reproducibility
down_trainData <- downSample(x = train_data[, setdiff(names(train_data), "product_group_name")],
                             y = train_data$product_group_name)

# Check to ensure the target variable is included and check its distribution
if(!"product_group_name" %in% names(down_trainData)) {
  stop("product_group_name was not included in the output of downSample")
}
table(down_trainData$product_group_name)

# Ensure target variable is a factor and check the number of levels
train_data$product_group_name <- as.factor(train_data$product_group_name)
print(length(levels(train_data$product_group_name)))  # Must be 2 for downSample

# If more than 2, you will need a different method for balancing or must focus on binary classes

# Manual undersampling for multiclass
set.seed(123)
sample_sizes <- table(train_data$product_group_name)  # Current sizes
target_size <- min(sample_sizes)  # Size of the smallest class

# Create a balanced dataset
balanced_data <- do.call(rbind,
                         lapply(split(train_data, train_data$product_group_name), function(subdata) {
                           subdata[sample(nrow(subdata), new_target_size),]
                         }))

# Check the new distribution
table(balanced_data$product_group_name)

# Load the necessary library
library(caret)

# Set up cross-validation
set.seed(123)  # for reproducibility
fitControl <- trainControl(
  method = "cv",       # for cross-validation
  number = 3,         # number of folds
  savePredictions = "final",  # saves predictions for the optimal model
  classProbs = TRUE,   # set TRUE if you want class probabilities
  summaryFunction = multiClassSummary
)

# Check levels of product_group_name
if(!is.factor(balanced_data$product_group_name)) {
  balanced_data$product_group_name <- factor(balanced_data$product_group_name)
}
# Ensure there are multiple levels
levels(balanced_data$product_group_name)

# Identify categorical predictors with only one level
single_level_predictors <- sapply(balanced_data[, sapply(balanced_data, is.factor)], function(x) {
  if(length(unique(x)) < 2) {
    return(names(x))
  }
})

# Report variables with single levels
single_level_predictors <- single_level_predictors[!sapply(single_level_predictors, is.null)]
print(single_level_predictors)

# Remove specific non-predictive variables
vars_to_remove <- c("customer_id", "article_id", "detail_desc", "t_dat", "date")
balanced_data <- balanced_data[, !(names(balanced_data) %in% vars_to_remove)]

# View the structure of the updated dataset
str(balanced_data)

# List all predictor variables, excluding 'FN'
predictor_columns <- setdiff(names(balanced_data), c("product_group_name", "FN"))

# Create the formula without 'FN'
formula <- as.formula(paste("product_group_name ~", paste(predictor_columns, collapse = " + ")))

# Make factor levels valid R variable names
levels(balanced_data$product_group_name) <- make.names(levels(balanced_data$product_group_name))

# Optionally, print out the new levels to check them
print(levels(balanced_data$product_group_name))
# Identify factor variables with a single level
single_level_factors <- sapply(balanced_data, function(x) {
  if(is.factor(x) || is.character(x)) {
    return(length(unique(na.omit(x))) < 2)
  } else {
    return(FALSE)
  }
})

# List factor variables with a single level
single_level_vars <- names(single_level_factors[single_level_factors])

# Print the variables
print(single_level_vars)

# Remove these variables from the data
balanced_data <- balanced_data[, !names(balanced_data) %in% single_level_vars]

# Now balanced_data should be ready for the model without any single-level factor issues.

# Removing preferred color
predictor_columns <- setdiff(names(balanced_data), c("product_group_name", "preferred_colour"))

# Create the formula without 'preferred_colour'
formula <- as.formula(paste("product_group_name ~", paste(predictor_columns, collapse = " + ")))

fitControl <- trainControl(method = "repeatedcv",
                           number = 1,
                           repeats = 1,
                           verboseIter = TRUE,
                           classProbs = TRUE, # Only set to TRUE if you need class probabilities
                           summaryFunction = multiClassSummary) # This is for multi-class problems

#run the model
set.seed(123)
gbmModel <- train(formula,
                  data = balanced_data,
                  method = "gbm",
                  trControl = fitControl,
                  verbose = FALSE,
                  metric = "Accuracy")
  
new_target_size <- 500

library(caret)
library(gbm)

# Assuming train_data is your training dataset
# and product_group_name is your target variable

set.seed(123)  # for reproducibility

# Check the current size of the dataset
print(nrow(balanced_data))

# Define a smaller subsampling rate if necessary
bag_fraction <- if(nrow(balanced_data) < 100) 0.5 else 1.0  # Adjust based on your dataset size

# Define the control using a smaller bag fraction
# and a smaller n.minobsinnode if needed
fitControl <- trainControl(
  method = "repeatedcv",
  number = 1,
  repeats = 3,
  verboseIter = TRUE,
  classProbs = TRUE,
  summaryFunction = multiClassSummary
)

# Set up the tuning grid for the GBM
  Tunegrid <- expand.grid(
  interaction.depth = c(1, 2),
  n.trees = c(100, 150,  # Example values: 50, 100, 150
  shrinkage = c(0.01, 0.05),
  n.minobsinnode = c(5, 10),
  .weight = c(0.5, 0.6)
))

# Train the GBM model
gbmModel <- train(
  product_group_name ~ .,
  data = balanced_data,
  method = "gbm",
  trControl = fitControl,
  verbose = FALSE,
  metric = "Accuracy",
  tuneGrid = grid,
  bag.fraction = bag_fraction
  # Adding the bag.fraction argument
)

# Output the results
print(gbmModel)

# Convert all character variables to factors where appropriate
balanced_data[sapply(balanced_data, is.character)] <- lapply(balanced_data[sapply(balanced_data, is.character)], factor)

# Identify and remove factors with less than two levels
factor_vars <- sapply(train_data, is.factor)
for (var in names(factor_vars)[factor_vars]) {
  if (length(unique(na.omit(train_data[[var]]))) < 2) {
    train_data[[var]] <- NULL  # Remove the variable with less than two levels
    cat(sprintf("Removed variable '%s' for having less than two levels.\n", var))
  }
}

# Update the formula if needed
predictor_vars <- setdiff(names(balanced_data), "product_group_name")
formula <- as.formula(paste("product_group_name ~", paste(predictor_vars, collapse = " + ")))

# Retry training the GBM model
gbmModel <- train(
  formula,
  data = train_data,
  method = "gbm",
  trControl = fitControl,
  verbose = FALSE,
  metric = "Accuracy",
  bag.fraction = bag_fraction
)

# Manually confirm that all factors have at least two levels
all_factors <- sapply(train_data, is.factor)
factor_levels <- sapply(train_data[, all_factors], levels)

# Print out the number of levels for each factor
sapply(names(factor_levels), function(x) {
  cat(x, "has", length(factor_levels[[x]]), "levels\n")
})

# Check for any levels in the test set not present in the training set if you have split your data
if("test_data" %in% ls()) {
  levels_in_test_not_in_train <- setdiff(levels(test_data$product_group_name), levels(train_data$product_group_name))
  if (length(levels_in_test_not_in_train) > 0) {
    cat("Levels in test not in train:", paste(levels_in_test_not_in_train, collapse=", "), "\n")
  }
}

# Ensure that the formula is correct
print(formula)

# Check for constant features
if(any(sapply(balanced_data, function(x) length(unique(x)) < 2))) {
  cat("There are constant features in the dataset.\n")
}

# Check for NA values
if(any(sapply(balanced_data, function(x) any(is.na(x))))) {
  cat("There are NA values in the dataset.\n")
}

# Identify constant features
constant_features <- sapply(balanced_data, function(x) length(unique(x)) < 2)

# Print the names of constant features to verify
constant_feature_names <- names(balanced_data)[constant_features]
print(constant_feature_names)


# Remove constant features from the dataset
balanced_data <- balanced_data[, !constant_feature_n]

# Print the structure of the updated dataset
str(balanced_data)

gbmModel <- train(
  product_group_name ~ .,
  data = balanced_data,
  method = "gbm",
  trControl = fitControl,
  verbose = TRUE,
  metric = "Accuracy",
  tuneGrid = tuneGrid
)

# Check the results
print(gbmModel)


# Assuming 'article_id' uniquely identifies each item and 'product_type_name', 'colour_group_name', 'index_group_name' are features
article_profiles <- train_data %>%
  select(article_id, product_type_name, colour_group_name, index_group_name) %>%
  distinct() %>%
  group_by(article_id) %>%
  summarise_all(funs(list))

# Convert categorical variables to a binary format using one-hot encoding
article_profiles <- dummy_cols(article_profiles, select_columns = c("product_type_name", "colour_group_name", "index_group_name"))

install.packages("lsa")
library(lsa)
# Assuming article_profiles now only contains numeric values post one-hot encoding
cosine_matrix <- cosine(article_profiles[-1])  # Exclude the article_id from similarity calculation

# Function to recommend items
recommend_items <- function(article_id, cosine_matrix, top_n = 5) {
  article_index <- which(article_profiles$article_id == article_id)
  similarities <- cosine_matrix[article_index, ]
  similar_articles <- order(similarities, decreasing = TRUE)[1:top_n]
  return(article_profiles$article_id[similar_articles])
}

recommend_items <- function(article_id, sim_matrix, top_n = 5) {
  # Find the index of the article_id in the row names of the similarity matrix
  article_index <- which(rownames(sim_matrix) == article_id)
  
  # If the article_id is not found, return NA
  if (length(article_index) == 0) {
    return(rep(NA, top_n))
  }
  
  # Get the similarity scores for the given article
  similarity_scores <- sim_matrix[article_index, ]
  
  # Sort and get the top_n indices of the highest scores
  top_indices <- order(similarity_scores, decreasing = TRUE)[1:top_n]
  
  # Return the article_ids corresponding to these indices
  return(rownames(sim_matrix)[top_indices])
}
