setwd("/Users/miwi/Documents/r/data")

library(caret)
library(Rcmdr)
library(olsrr)
library(dummy)
library(fastDummies)
library(glmnet)

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

#### Model 2 (remove variables with too many NAs) ####
apply(is.na(data), 2, mean)
data2 = subset(data, select = -c(20,5,21)) 

for(i in 1:nrow(data2)){
  if(data2$sector[i] == "#N/A"){
    data2$sector[i] = "Other"
  }
}

data2 = na.omit(data2)

data2 = data2[data2$sales > 0 & data2$assets > 0 & data2$cost_goods_sold > 0 & data2$equity > 0 & data2$revenue > 0 & data2$inventory > 0 & data2$working_capital > 0 & data2$fixed_assets > 0 & data2$debt_short_term > 0 & data2$debt_long_term > 0 & data2$interest_expense > 0,]

ratios2 = subset(data2, select = c(1:4,20,19))

ratios2$inventory_turnover = data2$cost_goods_sold/data2$inventory
ratios2$asset_turnover = data2$revenue/data2$sales
ratios2$fixed_asset_turnover = data2$revenue/data2$fixed_assets
ratios2$working_capital_turnover = data2$revenue/data2$working_capital
ratios2$current_ratio = data2$current_assets/data2$current_liabilities
ratios2$quick_ratio = (data2$current_assets - data2$inventory)/data2$current_liabilities
ratios2$debt_to_equity = (data2$debt_short_term+data2$debt_long_term)/data2$equity
ratios2$debt_to_assets = (data2$debt_short_term+data2$debt_long_term)/data2$assets
ratios2$financial_leverage = data2$assets/data2$equity
ratios2$interest_coverage = data2$ebit/data2$interest_expense
ratios2$gross_profit_margin = (data2$sales - data2$cost_goods_sold)/data2$revenue

ratios2 = dummy_cols(ratios2, select_columns = c("sub_region", "sector"), remove_first_dummy = TRUE, remove_selected_columns = TRUE)

train_split2 = 0.8
train2 = sample(c(TRUE,FALSE), nrow(ratios2), rep = TRUE, prob = c(train_split2, 1-train_split2))
train_subsample2 = ratios2[train2,]
test_subsample2 = ratios2[!train2,]

fullmod2 = glm("merger~.-firm_id-company-date", family = "binomial", data = train_subsample2) 
summary(fullmod2)

fullmod_linear2 = lm("merger~.-firm_id-company-date", data = train_subsample2)
summary(fullmod_linear2)

fullmod_linear2 = lm("merger~.-firm_id-company-date-asset_turnover", data = train_subsample2)

vif(fullmod_linear2)
fullmod_linear2 = lm("merger~.-firm_id-company-date-asset_turnover-debt_to_equity", data = train_subsample2)
vif(fullmod_linear2)

stepwise(fullmod_linear2, direction = "backward", criterion = "AIC")

fitmodel2 = glm(merger ~ fixed_asset_turnover + current_ratio + 
                  quick_ratio + debt_to_assets + sub_region_Central_Asia + 
                  sub_region_Eastern_Asia + sub_region_Melanesia + sub_region_Micronesia + 
                  sub_region_South_eastern_Asia + sub_region_Southern_Asia + 
                  sub_region_Western_Asia + sector_cons_disc + sector_cons_staples + 
                  sector_energy + sector_healh_care + sector_industrials + 
                  sector_inf_tech + sector_materials + sector_Other + sector_real_estate, 
                data = train_subsample2)

predmod_test2 = predict(fitmodel2, family = "binomial", newdata = test_subsample2, type = "response")

thresh = mean(train_subsample2$merger)
yes_or_no_pred = ifelse(predmod_test2 > thresh, 1, 0)
yes_or_no_actual= test_subsample2$merger
confusionMatrix(as.factor(yes_or_no_pred), as.factor(yes_or_no_actual))

#### Model 3 (Lasso using data1) ####
# removed firm_id, company, date, merger, and all variables with high VIF from fullmod_linear
X = as.matrix(train_subsample[, -c(1:4,6,17,11)]) 

lasso_model = cv.glmnet(x = X, y = train_subsample$merger, family= "binomial", alpha = 1)

new_data = as.matrix(test_subsample[,-c(1:4,6,17,11)])

predmod_test3 = predict(lasso_model, family= "binomial", newx = new_data, type = "response")

thresh = mean(train_subsample$merger)
yes_or_no_pred = ifelse(predmod_test3 > thresh, 1, 0)
yes_or_no_actual= test_subsample$merger
confusionMatrix(as.factor(yes_or_no_pred), as.factor(yes_or_no_actual))

#### Model 4 (Lasso using data2) ####
X2 = as.matrix(train_subsample2[, -c(1:4,6,11)]) 
Y2 = train_subsample2$merger

lasso_model2 = cv.glmnet(x = X2, y = Y2, family= "binomial", alpha = 1)

new_data2 = as.matrix(test_subsample2[,-c(1:4,6,11)])

predmod_test4 = predict(lasso_model2, family= "binomial", newx = new_data2, type = "response")

thresh = mean(train_subsample2$merger)
yes_or_no_pred = ifelse(predmod_test4 > thresh, 1, 0)
yes_or_no_actual= test_subsample2$merger
confusionMatrix(as.factor(yes_or_no_pred), as.factor(yes_or_no_actual))
