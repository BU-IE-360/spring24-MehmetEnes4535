# Load Library
require(forecast)
require(data.table)
require(lubridate)
library(reshape2)
library(tidyr)
library(data.table)

tday=today("Turkey")
# Dosya yolu ve adı

file_path1 <- "C:/Users/Lenovo/OneDrive/Masaüstü/weather_info.csv"
file_path2 <- "C:/Users/Lenovo/OneDrive/Masaüstü/production.csv"

# Load CVSe
weather_data <- read.csv(file_path1)
View(weather_data)
production_data  <- read.csv(file_path2)
View(production_data)

# Getting full weather date and hours as a template
template_dt <- unique(weather_data[, c("date", "hour")])
View(template_dt)

template_dt = merge(template_dt,production_data,by=c('date','hour'),all.x=T)
View(template_dt)

# Incrementing "tday" by one day
tday_plus_1 <- tday + days(1)
tday_plus_1

# Filtering "template_dt" based on "tday_plus_1"
template_dt <- template_dt[template_dt$date <= tday, ]

# Installing and loading the "reshape2" package
install.packages("reshape2")
library(reshape2)

# Melting "weather_data" to a long Format
long_weather <- melt(weather_data, id.vars = c(1:4))
View(long_weather)

# Calculating hourly region averages
hourly_region_averages = dcast(long_weather, date+hour~variable,fun.aggregate=mean)
View(hourly_region_averages )

# Merging "template_dt" with hourly weather data
template_dt_with_weather = merge(template_dt,hourly_region_averages,by=c('date','hour'),all.x=T)
View(template_dt_with_weather)

# Merging data frames
template_dt_with_weather <- merge(template_dt, hourly_region_averages, by = c('date', 'hour'), all.x = TRUE)

# Ordering the merged data frame
template_dt_with_weather <- template_dt_with_weather[order(template_dt_with_weather$date, template_dt_with_weather$hour), ]
View(template_dt_with_weather)

# Filtering non-missing data
available_data <- na.omit(template_dt_with_weather)
View(available_data)


# Identifying data to be forecasted
to_be_forecasted <- template_dt_with_weather[is.na(template_dt_with_weather$production), ]
View(to_be_forecasted)

# Linear Regression Modeling
do_not_use <- c('date', 'hour')
lr_model <- lm(production ~ ., data = available_data[, !names(available_data) %in% do_not_use])
lr_model

# Prediction
forecasted <- predict(lr_model, newdata = to_be_forecasted)
View(forecasted)

# Handling negative predictions and data format conversion
forecasted[forecasted<0] = 0
library(data.table)
to_be_forecasted <- as.data.table(to_be_forecasted)

# Creating Forecast Table and Adding Forecasted Values
forecast_table <- to_be_forecasted[, .(date, hour)]
forecast_table[, lr_forecast := forecasted]

# Determining Tomorrow's Date
library(lubridate)
tomorrow <- tday + days(1)

# Obtaining hourly forecasts for tomorrow and displaying results
day_ahead_forecast <- forecast_table[date == tday]
print(day_ahead_forecast)

# Merging data and converting to Data.Table format and sorting
merged_data <- merge(weather_data, production_data, by = c('date', 'hour'), all = TRUE)
View(merged_data)
merged_data <- as.data.table(merged_data)
merged_data <- merged_data[order(date, hour)]

#  Converting date column and filtering training data and test data
merged_data$date <- ymd(merged_data$date)
train_data <- merged_data[date(merged_data$date) < ymd("2022-07-01")]
test_data <- merged_data[date(merged_data$date) >= ymd("2022-07-01")]

# Treating date column as factor
merged_data$date <- as.factor(merged_data$date)

# Training Model and prediction
model <- lm(production ~ ., data = train_data)
model
predicted <- predict(model, newdata = test_data)
View(predicted)

# Computing Errors and sumamrizing 
errors <- test_data$production - predicted
head(errors)

summary(errors)
errors <- errors[!is.na(errors)]

# Calculating Mean Squared Error (MSE)
mse <- mean(errors^2)
mse

# Calculating Root Mean Squared Error (RMSE)
rmse <- sqrt(mse)
rmse

# Displaying Errors
cat("Ortalama Kare Hata (MSE):", mse, "\n")
cat("Kök Ortalama Kare Hata (RMSE):", rmse, "\n")

