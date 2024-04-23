setwd("C:/Users/EKArc/OneDrive/Documents/2024 Spring Classes/Regression Modeling/Data")

library(caret)
library(Rcmdr)
library(olsrr)
library(dummy)
library(fastDummies)

data = read.csv("Data.csv", header = TRUE)

names(data)
attach(data)

#### Clean Data ####
apply(is.na(data), 2, mean)

data1 = subset(data)

for(i in 1:nrow(data1)){
  if(data1$sector[i] == "#N/A"){
    data1$sector[i] = "Other"
  }
}

aggregate(data1$sector, list(data1$sector), length)

# use for financial ratios
data1 = subset(data1, select = -c(X)) 
data1 = na.omit(data1)

for(i in 1:nrow(data1)){
  if(data1$sales[i] < 0){
    data1$sales[i] = NA
  }
  if(data1$assets[i] <= 0){
    data1$assets[i] = NA
  }
  if(data1$cost_goods_sold[i] < 0){
    data1$cost_goods_sold[i] = NA
  }
  if(data1$equity[i] <= 0){
    data1$equity[i] = NA
  }
  if(data1$revenue[i] < 0){
    data1$revenue[i] = NA
  }
  if(data1$inventory[i] <= 0){
    data1$inventory[i] = NA
  }
  #if(data1$working_capital[i] <= 0){
  #  data1$working_capital[i] = NA
  #}
  #if(data1$fixed_assets[i] <= 0){
  #  data1$fixed_assets[i] = NA
  #}
}

data1 = na.omit(data1)

# use for regression
data2 = subset(data1) 
apply(is.na(data), 2, mean)

data2 = subset(data2, select = -c(earnings_per_share, net_income))


#### Financial Ratios ####
ratios = subset(data1, select = c(1,2))

ratios$inventory_turnover = data1$cost_goods_sold/data1$inventory
ratios$asset_turnover = data1$revenue/data1$sales
ratios$fixed_asset_turnover = data1$revenue/data1$fixed_assets
ratios$working_capital_turnover = data1$revenue/data1$working_capital
ratios$current_ratio = data1$current_assets/data1$current_liabilities
ratios$quick_ratio = (data1$current_assets - data1$inventory)/data1$current_liabilities
ratios$debt_to_equity = (data1$debt_short_term+data1$debt_long_term)/data1$equity
ratios$debt_to_assets = (data1$debt_short_term+data1$debt_long_term)/data1$assets
ratios$financial_leverage = data1$assets/data1$equity
ratios$interest_coverage = data1$ebit/data1$interest_expense
ratios$net_profit_margin = data1$net_income/data1$revenue
ratios$gross_profit_margin = (data1$sales - data1$cost_goods_sold)/data1$revenue
ratios$return_on_assets = data1$net_income/data1$assets
ratios$return_on_equity = data1$net_income/data1$equity

summary(ratios[c(3:14)])

#### Dummy Variables ####
data2 = dummy_cols(data2, select_columns = c("sub_region", "sector"), remove_first_dummy = TRUE)


#VIF
vif_values <- vif(model)
barplot(vif_values, col = "skyblue", main = "Variance Inflation Factor (VIF)")
