library(tidyverse)
library(randomForest)
library(caret)
library(pdp)
library(corrplot)
library(caTools)
library(glmnet)

#Load dataset
car_sales_data <- read_csv("/Users/cagoodwin/Desktop/CG-AAP/carsdataset.csv")

#PreProcessing
car_sales_data <- na.omit(car_sales_data) %>%
  mutate(Vehicle_type = as.factor(Vehicle_type))

car_sales_data <- car_sales_data %>%
  select(Sales_in_thousands, year_resale_value, Price_in_thousands, Engine_size, 
         Horsepower, Wheelbase, Width, Length, Curb_weight, Fuel_capacity, 
         Fuel_efficiency, Passenger, Vehicle_type) %>%
  mutate(Price_to_Horsepower_Ratio = Price_in_thousands / Horsepower)

#Train test splot
set.seed(123)
split <- sample.split(car_sales_data$Sales_in_thousands, SplitRatio = 0.75)
train_data <- subset(car_sales_data, split == TRUE)
test_data <- subset(car_sales_data, split == FALSE)

#Random Forest
control <- trainControl(method="cv", number=10)
tuned_rf_model <- train(Sales_in_thousands ~ ., data = train_data, method="rf", 
                        metric="RMSE", trControl=control)

rf_predictions <- predict(tuned_rf_model, test_data)
rf_rmse <- sqrt(mean((rf_predictions - test_data$Sales_in_thousands)^2))
rf_r2 <- 1 - sum((rf_predictions - test_data$Sales_in_thousands)^2) / sum((test_data$Sales_in_thousands - mean(test_data$Sales_in_thousands))^2)
print(paste("Random Forest RMSE:", rf_rmse))
print(paste("Random Forest R^2:", rf_r2))

#Preprocessing for glmnet for lasso and ridge reg model
x_train <- model.matrix(Sales_in_thousands ~ ., train_data)[,-1]
y_train <- train_data$Sales_in_thousands
x_test <- model.matrix(Sales_in_thousands ~ ., test_data)[,-1]
y_test <- test_data$Sales_in_thousands

#Cross validation lasso model
cv_lasso_model <- cv.glmnet(x_train, y_train, alpha = 1)
lasso_pred <- predict(cv_lasso_model, s = "lambda.min", newx = x_test)
lasso_rmse <- sqrt(mean((lasso_pred - y_test)^2))
lasso_r2 <- 1 - sum((lasso_pred - y_test)^2) / sum((y_test - mean(y_test))^2)
print(paste("Lasso RMSE:", lasso_rmse))
print(paste("Lasso R^2:", lasso_r2))

#Cross validation ridge model
cv_ridge_model <- cv.glmnet(x_train, y_train, alpha = 0)
ridge_pred <- predict(cv_ridge_model, s = "lambda.min", newx = x_test)
ridge_rmse <- sqrt(mean((ridge_pred - y_test)^2))
ridge_r2 <- 1 - sum((ridge_pred - y_test)^2) / sum((y_test - mean(y_test))^2)
print(paste("Ridge RMSE:", ridge_rmse))
print(paste("Ridge R^2:", ridge_r2))


#Plots

#Create corr matrix for variables
numeric_vars <- sapply(car_sales_data, is.numeric)
cor_matrix <- cor(car_sales_data[, numeric_vars])
#Plot corr matrix
corrplot(cor_matrix, method = "circle")

#Scatter plots
ggplot(car_sales_data, aes(x = Price_in_thousands, y = Sales_in_thousands)) +
  geom_point() + geom_smooth(method = "lm") +
  labs(title = "Sales vs. Price", x = "Price in Thousands", y = "Sales in Thousands")

ggplot(car_sales_data, aes(x = Fuel_efficiency, y = Sales_in_thousands)) +
  geom_point() + geom_smooth(method = "lm") +
  labs(title = "Sales vs. Fuel Efficiency", x = "Fuel Efficiency (mpg)", y = "Sales in Thousands")


#Partial Dependence Plots
features_of_interest <- c("Price_in_thousands", "Fuel_efficiency", "Horsepower", "Curb_weight", "Fuel_capacity")
pdp_data_list <- lapply(features_of_interest, function(feature) {
  pdp_obj <- partial(tuned_rf_model, pred.var = feature, grid.resolution = 20, train = train_data)
  pdp_df <- as.data.frame(pdp_obj)
  colnames(pdp_df) <- c("x", "yhat")
  pdp_df$Feature <- feature
  return(pdp_df)
})
#combining them so its just one output
pdp_data_combined <- bind_rows(pdp_data_list)
pdp_plot <- ggplot(pdp_data_combined, aes(x = x, y = yhat)) +
  geom_line() +
  facet_wrap(~ Feature, scales = "free_y") +
  labs(x = "Feature Value", y = "Partial Dependence", title = "Partial Dependence Plots for Each Feature") +
  theme_minimal() +
  theme(legend.position = "none")
print(pdp_plot)

#Boxplots for Vehicle Type Comparison (car vs passenger)
#Car is a regular car and passenger is a passenger car (more seats)
filtered_data <- car_sales_data %>%
  filter(Vehicle_type %in% c('Car', 'Passenger')) %>%
  select(Price_in_thousands, Sales_in_thousands, Vehicle_type)
par(mfrow=c(1,2))
boxplot(Price_in_thousands ~ Vehicle_type, data = filtered_data,
        main = "Price vs Vehicle Type", xlab = "Vehicle Type", ylab = "Price in Thousands",
        col = c("lightblue", "lightgreen"))
boxplot(Sales_in_thousands ~ Vehicle_type, data = filtered_data,
        main = "Sales vs Vehicle Type", xlab = "Vehicle Type", ylab = "Sales in Thousands",
        col = c("lightcoral", "lightyellow"))
par(mfrow=c(1,1))
