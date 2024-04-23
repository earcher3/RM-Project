
setwd("/Users/miwi/Documents/r/data")

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

data1 = subset(data1, select = -c(20)) 
data1 = na.omit(data1)

for(i in 1:nrow(data1)){
  if(data1$sales[i] < 0){
    data1$sales[i] = abs(data1$sales[i])
  }
  if(data1$assets[i] < 0){
    data1$assets[i] = abs(data1$assets[i])
  }
  if(data1$cost_goods_sold[i] < 0){
    data1$cost_goods_sold[i] = abs(data1$cost_goods_sold[i])
  }
  if(data1$equity[i] < 0){
    data1$equity[i] = abs(data1$equity[i])
  }
  if(data1$revenue[i] < 0){
    data1$revenue[i] = abs(data1$revenue[i])
  }
  if(data1$inventory[i] < 0){
    data1$inventory[i] = abs(data1$inventory[i])
  }
  if(data1$working_capital[i] < 0){
    data1$working_capital[i] = abs(data1$working_capital[i])
  }
  if(data1$fixed_assets[i] < 0){
    data1$fixed_assets[i] = abs(data1$fixed_assets[i])
  }
  if(data1$debt_short_term[i] < 0){
    data1$debt_short_term[i] = abs(data1$debt_short_term[i])
  }
  if(data1$debt_long_term[i] < 0){
    data1$debt_long_term[i] = abs(data1$debt_long_term[i])
  }
}

for(i in 1:nrow(data1)){
  if(data1$sales[i] == 0){
    data1$sales[i] = NA
  }
  if(data1$assets[i] == 0){
    data1$assets[i] = NA
  }
  if(data1$cost_goods_sold[i] == 0){
    data1$cost_goods_sold[i] = NA
  }
  if(data1$equity[i] == 0){
    data1$equity[i] = NA
  }
  if(data1$revenue[i] == 0){
    data1$revenue[i] = NA
  }
  if(data1$inventory[i] == 0){
    data1$inventory[i] = NA
  }
  if(data1$working_capital[i] == 0){
    data1$working_capital[i] = NA
  }
  if(data1$fixed_assets[i] == 0){
    data1$fixed_assets[i] = NA
  }
  if(data1$debt_short_term[i] == 0){
    data1$debt_short_term[i] = NA
  }
  if(data1$debt_long_term[i] == 0){
    data1$debt_long_term[i] = NA
  }
  if(data1$interest_expense[i] == 0){
    data1$interest_expense[i] = NA
  }
}

data1 = na.omit(data1)

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
data1 = dummy_cols(data1, select_columns = c("sub_region", "sector"), remove_first_dummy = TRUE)

#### Train & Test Data ####
train_split = 0.8
train = sample(c(TRUE,FALSE), nrow(data1), rep = TRUE, prob = c(train_split, 1-train_split))
train_subsample = data1[train,]
test_subsample = data1[!train,]

#### VIF ####
vif_max = 10

fullmod = glm("merger~.-firm_id-company-date-sub_region-sector", family = "binomial", data = train_subsample) # Full model
fullmod_linear = lm("merger~.-firm_id-company-date-sub_region-sector-sales", data = train_subsample)

vif(fullmod_linear)
fullmod_linear = lm("merger~.-firm_id-company-date-sub_region-sector-sales-revenue", data = train_subsample)
vif(fullmod_linear)
fullmod_linear = lm("merger~.-firm_id-company-date-sub_region-sector-sales-revenue-assets", data = train_subsample)
vif(fullmod_linear)
fullmod_linear = lm("merger~.-firm_id-company-date-sub_region-sector-sales-revenue-assets-current_liabilities", data = train_subsample)
vif(fullmod_linear)
fullmod_linear = lm("merger~.-firm_id-company-date-sub_region-sector-sales-revenue-assets-current_liabilities-equity", data = train_subsample)
vif(fullmod_linear)
fullmod_linear = lm("merger~.-firm_id-company-date-sub_region-sector-sales-revenue-assets-current_liabilities-equity-current_assets", data = train_subsample)
vif(fullmod_linear)
fullmod_linear = lm("merger~.-firm_id-company-date-sub_region-sector-sales-revenue-assets-current_liabilities-equity-current_assets-inventory", data = train_subsample)
vif(fullmod_linear)

#### Stepwise Selection ####
stepwise(fullmod_linear, direction = "backward", criterion = "AIC")
stepwise(fullmod_linear, direction = "forward", criterion = "AIC")
stepwise(fullmod_linear, direction = "backward/forward", criterion = "AIC")

#### Performance ####
fitmodel = glm(merger~current_assets + assets + ebit + inventory + 
                 fixed_assets + revenue + sub_region_Central_Asia + sub_region_Eastern_Asia + 
                 sub_region_Melanesia + sub_region_Micronesia + sub_region_South_eastern_Asia + 
                 sub_region_Southern_Asia + sub_region_Western_Asia + sector_cons_disc + 
                 sector_cons_staples + sector_energy + sector_healh_care + 
                 sector_industrials + sector_inf_tech + sector_materials + 
                 sector_Other + sector_real_estate, family = "binomial", data = train_subsample)

fitmodel = glm("merger~.-firm_id-company-date-sub_region-sector-sales-revenue-assets-current_liabilities-equity-current_assets-inventory", family = "binomial", data = train_subsample)

predmod_test = predict(fitmodel, family = "binomial", newdata = test_subsample, type = "response")

thresh = mean(train_subsample$merger)
yes_or_no_pred = ifelse(predmod_test > thresh, 1, 0)
yes_or_no_actual= test_subsample$merger
confusionMatrix(as.factor(yes_or_no_pred), as.factor(yes_or_no_actual))

#### Performance ####
fitmodel = glm(merger~current_assets + assets + ebit + inventory + 
                 fixed_assets + revenue + sub_region_Central_Asia + sub_region_Eastern_Asia + 
                 sub_region_Melanesia + sub_region_Micronesia + sub_region_South_eastern_Asia + 
                 sub_region_Southern_Asia + sub_region_Western_Asia + sector_cons_disc + 
                 sector_cons_staples + sector_energy + sector_healh_care + 
                 sector_industrials + sector_inf_tech + sector_materials + 
                 sector_Other + sector_real_estate, family = "binomial", data = train_subsample)


fitmodel = glm(merger ~ current_assets + assets + cost_goods_sold + 
                 debt_long_term + inventory + fixed_assets + revenue + interest_expense + 
                 net_income + sub_region + sector, family = "binomial", data = train_subsample)

predmod_test = predict(fitmodel, family = "binomial", newdata = test_subsample, type = "response")

thresh = mean(train_subsample$merger)
yes_or_no_pred = ifelse(predmod_test > thresh, 1, 0)
yes_or_no_actual= test_subsample$merger
confusionMatrix(as.factor(yes_or_no_pred), as.factor(yes_or_no_actual))

