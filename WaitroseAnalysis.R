#### Price Optimisation Initiative

# 1. Simulate the sample data with normally distributed WTPs
set.seed(123)

num_consumers <- 1000
days_of_week <- c('Weekdays', 'Weekends')

consumer_data <- expand.grid(Consumer_Number = 1:num_consumers, Day_of_Week = days_of_week)
tail(consumer_data)
# Assigning normally distributed WTPs for non-peak hours, differentiating between weekdays and weekends
# Specify the mean and standard deviation for weekdays and weekends
mean_wtp_non_peak_weekdays <- 2.9  # Example mean for weekdays
sd_wtp_non_peak_weekdays <- 0.6    # Example standard deviation for weekdays

mean_wtp_non_peak_weekends <- 3.4   # Example mean for weekends
sd_wtp_non_peak_weekends <- 0.6    # Example standard deviation for weekends

consumer_data$WTP_Non_Peak <- ifelse(consumer_data$Day_of_Week == 'Weekdays', 
                                     round(rnorm(nrow(consumer_data)/2, mean = mean_wtp_non_peak_weekdays, sd = sd_wtp_non_peak_weekdays), 2), 
                                     round(rnorm(nrow(consumer_data)/2, mean = mean_wtp_non_peak_weekends, sd = sd_wtp_non_peak_weekends), 2))

# Assigning WTPs for peak hours
consumer_data$WTP_Peak <- ifelse(consumer_data$Day_of_Week == 'Weekdays', 
                                 round(consumer_data$WTP_Non_Peak + runif(nrow(consumer_data), min = 1, max = 2.5), 2), 
                                 round(consumer_data$WTP_Non_Peak + runif(nrow(consumer_data), min = 1.5, max = 3), 2))

# 2. Check the distribution of sample data
library(ggplot2)
library(dplyr)
#install.packages("tidyverse")
library(tidyverse)
# Create a combined 'WTP' column and a 'Period' column to facilitate plotting
consumer_data_long <- consumer_data %>%
  pivot_longer(cols = c("WTP_Non_Peak", "WTP_Peak"), 
               names_to = "Period", 
               values_to = "WTP") %>%
  mutate(Period = ifelse(Period == "WTP_Non_Peak", "Non-Peak", "Peak"))

# Filter data for weekdays and weekends
consumer_data_weekdays <- consumer_data_long %>% filter(Day_of_Week == "Weekdays")
consumer_data_weekends <- consumer_data_long %>% filter(Day_of_Week == "Weekends")

# Plot histogram for Weekdays
ggplot(consumer_data_weekdays, aes(x = WTP, fill = Period)) + 
  geom_histogram(binwidth = 0.5, position = "dodge") +
  labs(title = "Weekdays",
       x = "Willingness to Pay",
       y = "Frequency") +
  scale_fill_brewer(palette = "Set1", name = "Period") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot histogram for Weekends
ggplot(consumer_data_weekends, aes(x = WTP, fill = Period)) + 
  geom_histogram(binwidth = 0.5, position = "dodge") +
  labs(title = "Weekends",
       x = "Willingness to Pay",
       y = "Frequency") +
  scale_fill_brewer(palette = "Set1", name = "Period") +
  theme_minimal() +
  theme(legend.position = "bottom")

# 3. Estimate sample demands and corresponding optimal prices
# Build a non-linear demand function
demand_function <- function(price, wtp_data) {
  exp(-(price - mean(wtp_data)) ^ 2 / (2 * sd(wtp_data) ^ 2))
}

# Build a function to calculate total revenue
calculate_revenue <- function(price_non_peak, price_peak, data) {
  # Non-linear demand estimation
  demand_non_peak <- demand_function(price_non_peak, data$WTP_Non_Peak) * length(data$WTP_Non_Peak)
  demand_peak <- demand_function(price_peak, data$WTP_Peak) * length(data$WTP_Peak)
  
  # Calculate revenue
  revenue_non_peak <- demand_non_peak * price_non_peak
  revenue_peak <- demand_peak * price_peak
  
  # Return total revenue
  total_revenue <- revenue_non_peak + revenue_peak
  return(total_revenue)
}

# Build a function to calculate optimize prices
optimize_prices <- function(data) {
  # Define a sequence of possible prices to test
  prices_non_peak <- seq(min(consumer_data$WTP_Non_Peak), max(consumer_data$WTP_Non_Peak), by = 0.1)
  prices_peak <- seq(min(consumer_data$WTP_Peak), max(consumer_data$WTP_Peak), by = 0.1)
  
  max_revenue <- 0
  optimal_price_non_peak <- 0
  optimal_price_peak <- 0
  
  # Iterate over non-peak prices
  for (price_np in prices_non_peak) {
    for (price_p in prices_peak) {
      # Calculate revenue for current price combination
      current_revenue <- calculate_revenue(price_np, price_p, data)
      # Check if this is the maximum revenue so far
      if (current_revenue > max_revenue) {
        max_revenue <- current_revenue
        optimal_price_non_peak <- price_np
        optimal_price_peak <- price_p
      }
    }
  }
  
  return(list(optimal_price_non_peak = optimal_price_non_peak, optimal_price_peak = optimal_price_peak, max_revenue = max_revenue))
}

# Separate data into weekdays and weekends
data_weekdays <- subset(consumer_data, Day_of_Week == 'Weekdays')
data_weekends <- subset(consumer_data, Day_of_Week == 'Weekends')

# Find optimal prices for weekdays
optimal_prices_weekdays <- optimize_prices(data_weekdays)
print(optimal_prices_weekdays)

# Find optimal prices for weekends
optimal_prices_weekends <- optimize_prices(data_weekends)
print(optimal_prices_weekends)

# 4. Scale the sample results to the population
population_size <- 13700000  # Total number of customers
sample_size <- nrow(consumer_data)

scaling_factor <- population_size / sample_size

# Build a function to calculate scaled revenue
scale_revenue_to_population <- function(sample_revenue) {
  population_revenue <- sample_revenue * scaling_factor
  return(population_revenue)
}

sample_revenue_weekdays <- optimal_prices_weekdays$max_revenue
sample_revenue_weekends <- optimal_prices_weekends$max_revenue

# Scale the sample revenue to population revenue
population_revenue_weekdays <- scale_revenue_to_population(sample_revenue_weekdays)
population_revenue_weekends <- scale_revenue_to_population(sample_revenue_weekends)

# Print the scaled revenues
print(paste("Estimated daily total revenue for the entire population on weekdays:", population_revenue_weekdays))
print(paste("Estimated daily total revenue for the entire population on weekends:", population_revenue_weekends))



#### Delivery Pass for 1 Month

# Load necessary library
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# Number of consumers
num_consumers <- 1000

# Days of the week
# days_of_week <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')

# Generate data
consumer_data <- expand.grid(Consumer_Number = 1:num_consumers)

# Simulate WTP for peak and non-peak periods

# peak perid  - any day / time of the week - 1 delivery per day (7 days)
# non-peak period - midweek - tue/wedn/thur - 1 delivery per day (3 days)

peak_average <- mean(8, 7.5, 8.99, 7.99)
non_peak_average <- mean(5,4,3.99)

consumer_data$WTP_Peak_Period <- runif(nrow(consumer_data), min = peak_average, max = peak_average*2)  # WTP for peak periods between £5 and £15
consumer_data$WTP_Non_Peak_Period <- runif(nrow(consumer_data), min = non_peak_average, max = non_peak_average*1.5)  # WTP for non-peak periods between £2 and £10

# View the first few rows of the dataset
head(consumer_data)

N_peak <- (peak_average*2-peak_average)*2
N_non_peak <- (non_peak_average*1.5-non_peak_average)*2
combinations <- N_non_peak*N_peak

surplusNonPeak<-rep(0,num_consumers)
surplusPeak<-rep(0,num_consumers)
demandNonPeak<-rep(0,combinations)
demandPeak<-rep(0,combinations)

index=1
for (basePrice in seq(from = non_peak_average, to = non_peak_average*1.5, by = 0.5)){
  for (peakPrice in seq(from = peak_average, to = peak_average*2, by = 0.5)){
    for (i in 1:num_consumers){
      surplusNonPeak[i]=max(consumer_data[i,3]-basePrice)
      surplusPeak[i]=consumer_data[i,2]-peakPrice
    }
    demandNonPeak[index]=sum((surplusNonPeak>surplusPeak)*(surplusNonPeak>=0))
    demandPeak[index]=sum((surplusPeak>=surplusNonPeak)*(surplusPeak>=0))
    index=index+1
  }
}


# Create a data table which we will use to run the two regressions:
newdata<-data.frame(matrix(nrow=102,ncol = 5))
colnames(newdata)=c("index","basePrice","peakPrice","NonPeakDemand", "PeakDemand")
index=1
for (basePrice in seq(from = non_peak_average, to = non_peak_average*1.5, by = 0.5)){
  for (peakPrice in seq(from = peak_average, to = peak_average*2, by = 0.5)){
    newdata[index,1]=index
    newdata[index,2]=basePrice
    newdata[index,3]=peakPrice
    newdata[index,4]=demandNonPeak[index]
    newdata[index,5]=demandPeak[index]
    index=index+1
  }
}
# Visualizing Revenue as a Function of Base and Peak Price
#newdata$revenue=newdata$basePrice*newdata$NonPeakDemand+newdata$peakPrice*newdata$PeakDemand
#install.packages("lattice")
#library(lattice)
#wireframe(revenue ~ basePrice * peakPrice, data=newdata)

# Run Regressions:
# Regression for the dependent variable NonPeakDemand
fit2NonPeak <-lm(NonPeakDemand ~ basePrice+peakPrice, data=newdata)
summary(fit2NonPeak)

a1=coef(fit2NonPeak)[1]
b11=coef(fit2NonPeak)[2]
b12=coef(fit2NonPeak)[3]
# Regression for the dependent variable NonPeakDemand
fit2Peak <-lm(PeakDemand ~ basePrice+peakPrice, data=newdata)
summary(fit2Peak)
a2=coef(fit2Peak)[1]
b21=coef(fit2Peak)[2]
b22=coef(fit2Peak)[3]
library(stargazer)
stargazer(fit2NonPeak,fit2Peak, type="text")


#stargazer(fit2NonPeak,fit2Peak, type="latex", out="Outputexample.tex")
# Finding optimal revenue by optimization
library("nloptr")
# Differentiated Prices
?round
eval_f <- function(x){
  basePrice <- round(x[1], 0) - 0.01
  peakPrice <- round(x[2], 0) - 0.01
  NonPeakDemand=max(0,a1+b11*basePrice+b12*peakPrice)
  PeakDemand=max(0,a2+b21*basePrice+b22*peakPrice)
  revenue=basePrice*NonPeakDemand+peakPrice*PeakDemand
  objfunction=-revenue
  return(objfunction)
}
eval_g_ineq <- function(x) {
  basePrice <- round(x[1], 0) - 0.01
  peakPrice <- round(x[2], 0) - 0.01
  NonPeakDemand=max(0,a1+b11*basePrice+b12*peakPrice)
  PeakDemand=max(0,a2+b21*basePrice+b22*peakPrice)
  constraint <- c(-NonPeakDemand,
                  -PeakDemand,
                  x[1]-x[2])
  return(constraint)
}
# initial values
x0 <- c(6, 8)
# lower and upper bounds of control
lb <- c(5,5)
max(consumer_data$WTP_Peak_Period)
ub <- c(30,30)
opts <- list( "algorithm" = "NLOPT_LN_COBYLA",
              "xtol_rel" = 1.0e-9,
              "maxeval" = 1000)

result <- nloptr(x0=x0,eval_f=eval_f,lb=lb,ub=ub,
                 eval_g_ineq=eval_g_ineq,opts=opts)
# print(result)
priceOpt<-result$solution
RevenueOpt<- -result$objective
print(paste("Optimal Base Price:",round(priceOpt[1], 0) - 0.01))

## [1] "Optimal Base Price: 80.1121164184752"
print(paste("Optimal Peak Price:",round(priceOpt[2], 0) - 0.01))
## [1] "Optimal Peak Price: 84.6597690673213"
print(paste("Optimal Revenue:",RevenueOpt))

RevenueOpt*(13700000*0.14*0.3*0.5/1000)/4  

print(paste("optinal weekly revenue for 1-month pass:", RevenueOpt*(13700000*0.14*0.3*0.5/1000)/4 ))



#### Delivery Pass for 6 Months

# Load necessary library
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# Number of consumers
num_consumers_6 <- 1000

# Days of the week
# days_of_week <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')

# Generate data
consumer_data_6 <- expand.grid(Consumer_Number = 1:num_consumers_6)

# Simulate WTP for peak and non-peak periods

# peak perid  - any day / time of the week - 1 delivery per day (7 days)
# non-peak period - midweek - tue/wedn/thur - 1 delivery per day (3 days)

peak_average_6 <- mean(49.99, 45, 47.94,43)
non_peak_average_6 <- mean(22.49, 25)

consumer_data_6$WTP_Peak_Period_6 <- runif(nrow(consumer_data_6), min =peak_average_6 , max =peak_average_6*2)  # WTP for peak periods between ??
consumer_data_6$WTP_Non_Peak_Period_6 <- runif(nrow(consumer_data_6), min = non_peak_average_6, max =non_peak_average_6*1.5 )  # WTP for non-peak periods between ??

# View the first few rows of the dataset
head(consumer_data_6)

N_peak_6 <- (peak_average_6*2-peak_average_6)*2
N_non_peak_6 <- (non_peak_average_6*1.5-non_peak_average_6)*2
combinations_6 <- N_non_peak_6*N_peak_6

surplusNonPeak_6<-rep(0,num_consumers_6)
surplusPeak_6<-rep(0,num_consumers_6)
demandNonPeak_6<-rep(0,combinations_6)
demandPeak_6<-rep(0,combinations_6)

index=1
for (basePrice_6 in seq(from = non_peak_average_6, to = non_peak_average_6*1.5, by = 0.5)){
  for (peakPrice_6 in seq(from = peak_average_6, to = peak_average_6*2, by = 0.5)){
    for (i in 1:num_consumers_6){
      surplusNonPeak_6[i]=max(consumer_data_6[i,c(3)]-basePrice_6)
      surplusPeak_6[i]=consumer_data_6[i,2]-peakPrice_6
    }
    demandNonPeak_6[index]=sum((surplusNonPeak_6>surplusPeak_6)*(surplusNonPeak_6>=0))
    demandPeak_6[index]=sum((surplusPeak_6>=surplusNonPeak_6)*(surplusPeak_6>=0))
    index=index+1
  }
}


# Create a data table which we will use to run the two regressions:
newdata_6<-data.frame(matrix(nrow=2300,ncol = 5))
colnames(newdata_6)=c("index","basePrice_6","peakPrice_6","NonPeakDemand_6", "PeakDemand_6")
index=1
for (basePrice_6 in seq(from = non_peak_average_6, to = non_peak_average_6*1.5, by = 0.5)){
  for (peakPrice_6 in seq(from = peak_average_6, to = peak_average_6*2, by = 0.5)){
    newdata_6[index,1]=index
    newdata_6[index,2]=basePrice_6
    newdata_6[index,3]=peakPrice_6
    newdata_6[index,4]=demandNonPeak_6[index]
    newdata_6[index,5]=demandPeak_6[index]
    index=index+1
  }
}
# Visualizing Revenue as a Function of Base and Peak Price
#newdata$revenue=newdata$basePrice*newdata$NonPeakDemand+newdata$peakPrice*newdata$PeakDemand
#install.packages("lattice")
#library(lattice)
#wireframe(revenue ~ basePrice * peakPrice, data=newdata)

# Run Regressions:
# Regression for the dependent variable NonPeakDemand
fit2NonPeak_6 <-lm(NonPeakDemand_6 ~ basePrice_6+peakPrice_6, data=newdata_6)
summary(fit2NonPeak_6)

a1_6=coef(fit2NonPeak_6)[1]
b11_6=coef(fit2NonPeak_6)[2]
b12_6=coef(fit2NonPeak_6)[3]
# Regression for the dependent variable NonPeakDemand
fit2Peak_6 <-lm(PeakDemand_6 ~ basePrice_6+peakPrice_6, data=newdata_6)
summary(fit2Peak_6)
a2_6=coef(fit2Peak_6)[1]
b21_6=coef(fit2Peak_6)[2]
b22_6=coef(fit2Peak_6)[3]
library(stargazer)
stargazer(fit2NonPeak_6,fit2Peak_6, type="text")


# Finding optimal revenue by optimization
library("nloptr")
# Differentiated Prices
eval_f_6 <- function(x){
  basePrice_6=round(x[1], 0)-0.01
  peakPrice_6=round(x[2],0)-0/01
  NonPeakDemand_6=max(0,a1_6+b11_6*basePrice_6+b12_6*peakPrice_6)
  PeakDemand_6=max(0,a2_6+b21_6*basePrice_6+b22_6*peakPrice_6)
  revenue_6=basePrice_6*NonPeakDemand_6+peakPrice_6*PeakDemand_6
  objfunction=-revenue_6
  return(objfunction)
}
eval_g_ineq_6 <- function(x) {
  basePrice_6=round(x[1], 0)-0.01
  peakPrice_6=round(x[1], 0)-0.01
  NonPeakDemand_6=max(0,a1_6+b11_6*basePrice_6+b12_6*peakPrice_6)
  PeakDemand_6=max(0,a2_6+b21_6*basePrice_6+b22_6*peakPrice_6)
  constraint_6 <- c(-NonPeakDemand_6,
                    -PeakDemand_6,
                    x[1]-x[2])
  return(constraint_6)
}
# initial values
x0_6 <- c(23,49)
# lower and upper bounds of control
lb_6 <- c(23,23)

ub_6 <- c(65,65)
opts <- list( "algorithm" = "NLOPT_LN_COBYLA",
              "xtol_rel" = 1.0e-9,
              "maxeval" = 1000)

result <- nloptr(x0=x0_6,eval_f=eval_f_6,lb=lb_6,ub=ub_6,
                 eval_g_ineq=eval_g_ineq_6,opts=opts)
# print(result)
priceOpt_6<-result$solution
RevenueOpt_6<- -result$objective

print(paste("Optimal Base Price:",round(priceOpt_6[1],0)-0.01))
## [1] "Optimal Base Price: 

print(paste("Optimal Peak Price:",round(priceOpt_6[2], 0)- 0.01))
## [1] "Optimal Peak Price: 

print(paste("Optimal Revenue:",RevenueOpt_6))

RevenueOpt_6*(13700000*0.14*0.3*0.25/1000)/(6*4)

print(paste("optinal weekly revenue for 6-months pass:", RevenueOpt_6*(13700000*0.14*0.3*0.25/1000)/(6*4)))



#### Delivery Pass for 12 Months

# Load necessary library
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# Number of consumers
num_consumers_12 <- 1000

# Days of the week
# days_of_week <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')

# Generate data
consumer_data_12 <- expand.grid(Consumer_Number = 1:num_consumers_12)

# Simulate WTP for peak and non-peak periods

# peak perid  - any day / time of the week - 1 delivery per day (7 days)
# non-peak period - midweek - tue/wedn/thur - 1 delivery per day (3 days)
peak_average_12 <- mean(70, 80, 89.99)
non_peak_average_12 <- mean(40,40,39.99)

consumer_data_12$WTP_Peak_Period_12 <- runif(nrow(consumer_data_12), min =peak_average_12 , max =peak_average_12*2)  # WTP for peak periods between ??
consumer_data_12$WTP_Non_Peak_Period_12 <- runif(nrow(consumer_data_12), min = non_peak_average_12, max =non_peak_average_12*1.5 )  # WTP for non-peak periods between ??

# View the first few rows of the dataset
head(consumer_data_12)

N_peak_12 <- (peak_average_12*2-peak_average_12)*2
N_non_peak_12 <- (non_peak_average_12*1.5-non_peak_average_12)*2
combinations_12 <- N_non_peak_12*N_peak_12

surplusNonPeak_12<-rep(0,num_consumers_12)
surplusPeak_12<-rep(0,num_consumers_12)
demandNonPeak_12<-rep(0,combinations_12)
demandPeak_12<-rep(0,combinations_12)

index=1
for (basePrice_12 in seq(from = non_peak_average_12, to = non_peak_average_12*1.5, by = 0.5)){
  for (peakPrice_12 in seq(from = peak_average_12, to = peak_average_12*2, by = 0.5)){
    for (i in 1:num_consumers_12){
      surplusNonPeak_12[i]=max(consumer_data_12[i,c(3)]-basePrice_12)
      surplusPeak_12[i]=consumer_data_12[i,2]-peakPrice_12
    }
    demandNonPeak_12[index]=sum((surplusNonPeak_12>surplusPeak_12)*(surplusNonPeak_12>=0))
    demandPeak_12[index]=sum((surplusPeak_12>=surplusNonPeak_12)*(surplusPeak_12>=0))
    index=index+1
  }
}


# Create a data table which we will use to run the two regressions:
newdata_12<-data.frame(matrix(nrow=2300,ncol = 5))
colnames(newdata_12)=c("index","basePrice_12","peakPrice_12","NonPeakDemand_12", "PeakDemand_12")
index=1
for (basePrice_12 in seq(from = non_peak_average_12, to = non_peak_average_12*1.5, by = 0.5)){
  for (peakPrice_12 in seq(from = peak_average_12, to = peak_average_12*2, by = 0.5)){
    newdata_12[index,1]=index
    newdata_12[index,2]=basePrice_12
    newdata_12[index,3]=peakPrice_12
    newdata_12[index,4]=demandNonPeak_12[index]
    newdata_12[index,5]=demandPeak_12[index]
    index=index+1
  }
}
# Visualizing Revenue as a Function of Base and Peak Price
#newdata$revenue=newdata$basePrice*newdata$NonPeakDemand+newdata$peakPrice*newdata$PeakDemand
#install.packages("lattice")
#library(lattice)
#wireframe(revenue ~ basePrice * peakPrice, data=newdata)

# Run Regressions:
# Regression for the dependent variable NonPeakDemand
fit2NonPeak_12 <-lm(NonPeakDemand_12 ~ basePrice_12+peakPrice_12, data=newdata_12)
summary(fit2NonPeak_12)

a1_12=coef(fit2NonPeak_12)[1]
b11_12=coef(fit2NonPeak_12)[2]
b12_12=coef(fit2NonPeak_12)[3]
# Regression for the dependent variable NonPeakDemand
fit2Peak_12 <-lm(PeakDemand_12 ~ basePrice_12+peakPrice_12, data=newdata_12)
summary(fit2Peak_12)
a2_12=coef(fit2Peak_12)[1]
b21_12=coef(fit2Peak_12)[2]
b22_12=coef(fit2Peak_12)[3]
library(stargazer)
stargazer(fit2NonPeak_12,fit2Peak_12, type="text")


# Finding optimal revenue by optimization
library("nloptr")
# Differentiated Prices
eval_f_12 <- function(x){
  basePrice_12=round(x[1], 0)-0.01
  peakPrice_12=round(x[2],0)-0/01
  NonPeakDemand_12=max(0,a1_12+b11_12*basePrice_12+b12_12*peakPrice_12)
  PeakDemand_12=max(0,a2_12+b21_12*basePrice_12+b22_12*peakPrice_12)
  revenue_12=basePrice_12*NonPeakDemand_12+peakPrice_12*PeakDemand_12
  objfunction=-revenue_12
  return(objfunction)
}
eval_g_ineq_12 <- function(x) {
  basePrice_12=round(x[1], 0)-0.01
  peakPrice_12=round(x[1], 0)-0.01
  NonPeakDemand_12=max(0,a1_12+b11_12*basePrice_12+b12_12*peakPrice_12)
  PeakDemand_12=max(0,a2_12+b21_12*basePrice_12+b22_12*peakPrice_12)
  constraint_12 <- c(-NonPeakDemand_12,
                     -PeakDemand_12,
                     x[1]-x[2])
  return(constraint_12)
}
# initial values
x0_12 <- c(40,70)
# lower and upper bounds of control
lb_12 <- c(40,40)

ub_12 <- c(90,90)
opts <- list( "algorithm" = "NLOPT_LN_COBYLA",
              "xtol_rel" = 1.0e-9,
              "maxeval" = 1000)

result <- nloptr(x0=x0_12,eval_f=eval_f_12,lb=lb_12,ub=ub_12,
                 eval_g_ineq=eval_g_ineq_12,opts=opts)
# print(result)
priceOpt_12<-result$solution
RevenueOpt_12<- -result$objective

print(paste("Optimal Base Price:",round(priceOpt_12[1],0)-0.01))
## [1] "Optimal Base Price: 

print(paste("Optimal Peak Price:",round(priceOpt_12[2], 0)- 0.01))
## [1] "Optimal Peak Price: 

print(paste("Optimal Revenue:",RevenueOpt_12*0.05))

RevenueOpt_12*(13700000*0.14*0.3*0.25/1000)/(12*4)  

print(paste("optinal weekly revenue for annual pass:", RevenueOpt_12*(13700000*0.14*0.3*0.25/1000)/(12*4)))



