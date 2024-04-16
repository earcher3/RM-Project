setwd("C:/Users/EKArc/OneDrive/Documents/2024 Spring Classes/Regression Modeling/Data")
Data=read.csv("Data.csv",header=TRUE,sep=",")
names(Data)
attach(Data)

Data=subset(Data,select= -c(X))
Data=na.omit(Data)

names(data)
attach(data)

#### Financial Ratios ####
fin.rat = subset(data, select = c(1,2))

fin.rat$inventory_turnover = cost_goods_sold/inventory
fin.rat$asset_turnover = revenue/sales
fin.rat$fixed_asset_turnover = revenue/fixed_assets
fin.rat$working_capital_turnover = revenue/working_capital
fin.rat$current_ratio = current_assets/current_liabilities
fin.rat$quick_ratio = (current_assets - inventory)/current_liabilities
fin.rat$debt_to_equity = (debt_short_term+debt_long_term)/equity
fin.rat$debt_to_assets = (debt_short_term+debt_long_term)/assets
fin.rat$financial_leverage = assets/equity
fin.rat$interest_coverage = ebit/interest_expense
fin.rat$net_profit_margin = net_income/revenue
fin.rat$gross_profit_margin = (sales - cost_goods_sold)/revenue
fin.rat$return_on_assets = net_income/assets
fin.rat$return_on_equity = net_income/equity
