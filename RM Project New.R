
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
data1 = subset(data)

for(i in 1:nrow(data1)){
  if(data1$sector[i] == "#N/A"){
    data1$sector[i] = "Other"
  }
}

aggregate(data1$sector, list(data1$sector), length)

data1 = subset(data1, select = -c(20)) 
apply(is.na(data1), 2, mean)

data1 = na.omit(data1)

data1 = data1[data1$sales > 0 & data1$assets > 0 & data1$cost_goods_sold > 0 & data1$equity > 0 & data1$revenue > 0 & data1$inventory > 0 & data1$working_capital > 0 & data1$fixed_assets > 0 & data1$debt_short_term > 0 & data1$debt_long_term > 0 & data1$interest_expense > 0,]

#### Financial Ratios ####
ratios = subset(data1, select = c(1:4,21,22))

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

#### Dummy Variables ####
ratios = dummy_cols(ratios, select_columns = c("sub_region", "sector"), remove_first_dummy = TRUE, remove_selected_columns = TRUE)

#### Train & Test Data ####
train_split = 0.8
train = sample(c(TRUE,FALSE), nrow(ratios), rep = TRUE, prob = c(train_split, 1-train_split))
train_subsample = ratios[train,]
test_subsample = ratios[!train,]

#### VIF ####
fullmod = glm("merger~.-firm_id-company-date", family = "binomial", data = train_subsample) # Full model
summary(fullmod)

fullmod_linear = lm("merger~.-firm_id-company-date-asset_turnover", data = train_subsample)

vif(fullmod_linear)
fullmod_linear = lm("merger~.-firm_id-company-date-asset_turnover-return_on_assets", data = train_subsample)
vif(fullmod_linear)
fullmod_linear = lm("merger~.-firm_id-company-date-asset_turnover-return_on_assets-debt_to_equity", data = train_subsample)
vif(fullmod_linear)

stepwise(fullmod_linear, direction = "backward", criterion = "AIC")

#### Performance ####
fitmodel1 = glm(merger ~ fixed_asset_turnover + current_ratio + quick_ratio + debt_to_assets + sub_region_Central_Asia + sub_region_Eastern_Asia + sub_region_Melanesia + sub_region_Micronesia + sub_region_South_eastern_Asia + sub_region_Southern_Asia + sub_region_Western_Asia + sector_cons_disc + sector_cons_staples + sector_energy + sector_healh_care + sector_industrials + sector_inf_tech + sector_materials + sector_Other + sector_real_estate, family = "binomial", data = train_subsample)

predmod_test1 = predict(fitmodel1, family = "binomial", newdata = test_subsample, type = "response")

thresh = mean(train_subsample$merger)
yes_or_no_pred = ifelse(predmod_test1 > thresh, 1, 0)
yes_or_no_actual= test_subsample$merger
confusionMatrix(as.factor(yes_or_no_pred), as.factor(yes_or_no_actual))

#### Model 2 ####
