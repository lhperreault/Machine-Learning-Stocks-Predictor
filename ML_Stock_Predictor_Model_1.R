
# This is a Machine Learning Model for predicting the Stock Market.
# I learned how to use the xgboost model thanks to Kerill Eremenko's Udemy Course.

# The objective is to make a model that can predict what will happen in the snp 500.

# upload libraries and download the snp500
# install.packages("quantmod")
library(quantmod)
library(readr)
library(readxl)
library(readr)
library(tidyverse)
library(lubridate)
library(xgboost)

getSymbols("^GSPC")
head(GSPC)
dataset <- GSPC
dataset <- as_tibble(dataset, rownames = "Date")
colnames(dataset) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")

# deal with date 
dataset$Date <- as.Date(dataset$Date)  # Convert the column to Date format if it's not already

dataset$Year <- year(dataset$Date)  # Extract the year
dataset$Month <- month(dataset$Date)  # Extract the month
dataset$Day <- day(dataset$Date)  # Extract the da

# Encoding the categorical variables as numeric 
dataset$Date = as.numeric(dataset$Date)
dataset <- dataset %>% select(Year, Month, Day, Close, Open)
dataset <- na.omit(dataset)

# We must adjust the scope of time to what we want to predict. 
# Do this by looking at the appropriate sequence number that is matched up with the dates desired.
df <- dataset
df$SequenceColumn <- 1:nrow(dataset)
training_set <- df %>% filter(SequenceColumn < 3198) # this should be one number higher then below.
test_set <- df %>% filter(SequenceColumn > 3197 & SequenceColumn < 3225) # last number is where you want it to end.
training_set <- select(training_set, Year, Month, Day, Close, SequenceColumn)
test_set <- select(test_set, Year, Month, Day, Close, SequenceColumn)

################################################################

# Train the model
classifier = xgboost(data = as.matrix(training_set[-4]), label = training_set$Close, nrounds = 400)

# Predicting the Test set results
y_pred = predict(classifier, newdata = as.matrix(test_set[-4]))
y_pred = data.frame(y_pred)
y_pred
# results as mean square
mean_square_e <- mean((y_pred$y_pred - test_set$Close)^2)
mean_square_e
# accuracy from the predicted to real. 
accuracy = (mean((test_set$Close - abs(y_pred$y_pred - test_set$Close))/test_set$Close))
accuracy
### neither mean square nor accuracy are great indicators if the model is useful. 

######## This code below is for the manipulation and graph of the stock price to time. 
# We must add Date to y_pred
date1 <- function(x){
  x <- cbind(x,test_set)
  x$Date <- as.Date(paste(x$Year, x$Month, x$Day, sep = "-"))
  return(x)
}
### insert the db you want to use in the parenthesis below.

df <- date1(y_pred)

# Creating a plot of the predicted column.
ggplot(df, aes(x = Date, y = y_pred)) +
  geom_line() +  
  scale_x_date(
    date_breaks = "1 month",  # Set tick intervals for months
    date_labels = "%b %Y"     # Format for month-year labels
  ) +
  labs(x = "Date", y = "Close", title = "SnP500 Prediction") +
  theme_minimal()

# compare real to predicted. 

date1 <- function(x){
  x$Date <- as.Date(paste(x$Year, x$Month, x$Day, sep = "-"))
  return(x)
}
### insert the db you want to use in the parenthesis below.
test_set <- date1(test_set)

ggplot() +
  geom_line(data = test_set, aes(x = Date, y = Close, color = "real")) +
  geom_line(data = df, aes(x = Date, y = y_pred, color = "predicted")) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y"
  ) +
  labs(x = "Date", y = "Closing", title = "SnP500 ML Model") +
  theme_minimal() 

### When looking at the results the trades we wouldve made in this time period wouldve been rewarding.