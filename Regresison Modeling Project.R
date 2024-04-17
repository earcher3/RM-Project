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

fin.rat[is.na(fin.rat)] = 0
fin.rat[sapply(fin.rat, is.infinite)] = 0

summary(fin.rat[c(3:14)])

#### Dummy Variables ####
numrow = nrow(data)

region_dummy = matrix(nrow = numrow, ncol = 1)

for(i in 1:numrow){
  if(sub_region[i] == "Western_Asia"){
    region_dummy[i,] = 1
  } else if(sub_region[i] == "Southern_Asia"){
    region_dummy[i,] = 2
  } else if(sub_region[i] == "South_eastern_Asia"){
    region_dummy[i,] = 3
  } else if(sub_region[i] == "Micronesia"){
    region_dummy[i,] = 4
  } else if(sub_region[i] == "Melanesia"){
    region_dummy[i,] = 5
  } else if(sub_region[i] == "Eastern_Asia"){
    region_dummy[i,] = 6
  } else if(sub_region[i] == "Central_Asia"){
    region_dummy[i,] = 7
  } else {
    region_dummy[i,] = 0
  }
}

head(region_dummy)
colnames(region_dummy) = "region_dummy"

sector_dummy = matrix(nrow = numrow, ncol = 1)

for(i in 1:numrow){
  if(sector[i] == "com_services"){
    sector_dummy[i,] = 1
  } else if(sector[i] == "cons_disc"){
    sector_dummy[i,] = 2
  } else if(sector[i] == "cons_staples"){
    sector_dummy[i,] = 3
  } else if(sector[i] == "energy"){
    sector_dummy[i,] = 4
  } else if(sector[i] == "health_care"){
    sector_dummy[i,] = 5
  } else if(sector[i] == "industrials"){
    sector_dummy[i,] = 6
  } else if(sector[i] == "inf_tech"){
    sector_dummy[i,] = 7
  } else if(sector[i] == "materials"){
    sector_dummy[i,] = 8
  } else {
    sector_dummy[i,] = 0
  }
}

head(sector_dummy)
colnames(sector_dummy) = "sector_dummy"
