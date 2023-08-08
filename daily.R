setwd('/Users/jkim/Library/Mobile Documents/com~apple~CloudDocs/퀀트응용경제학/과제/quant/Crypto/daily')


library(dplyr)
library(readxl)
library(car)
library(sandwich)
library(lmtest)
library("orcutt")
library(AER)
library(jsonlite)
library(rvest)
library(tidyverse)
library(httr)
library(jsonlite)
library(plyr)
library(data.table)
library(openssl)
library(tidyquant)
library(stringr)
library(dplyr)
library(readr)
library(dplyr)
library(zoo)
library(corrplot)
library(tseries)


getwd()
list.files()

pre_process = function(df, close_price, index){
  df$up_down = NA
  for (i in 1:nrow(df)) {
    try({
      if (close_price[i+1 + index] > close_price[i+index]) {
        df$up_down[i+index] = 1
      }
      else {
        df$up_down[i+index] = -1
      }
    }, silent=TRUE)
  }

  df$diff = NA
  for (i in 1:nrow(df)) {
    try({
      df$diff[i+index] = close_price[i+index+1] - close_price[i+index]
    }, silent=TRUE)
  }

  df$change_rate = NA
  for (i in 1:nrow(df)) {
    try({
      df$change_rate[i+index] = ((close_price[i+index+1] / close_price[i+index]) - 1)
    }, silent=TRUE)
  }
  return (df)


}
granger_process = function (y, x, lag, convert=0){
  flag = 0
  if (convert == 0){
    for (i in 1:lag){
      a = grangertest(y ~ x , order = i)
      if (a$Pr[2] < 0.01){
        print(paste(i, "1%유의 수준 있음"))
        flag = 1
      }
      else if (a$Pr[2] < 0.05){
        print(paste(i, "5%유의 수준 있음"))
        flag = 1
      }

    }
  }
  else{
    for (i in 1:lag){
      a = grangertest(x ~ y , order = i)
      if (a$Pr[2] < 0.01){
        print(paste(i, "1%유의 수준 있음"))
        flag = 1
      }
      else if (a$Pr[2] < 0.05){
        print(paste(i, "5%유의 수준 있음"))
        flag = 1
      }
    }
  }




  if (flag == 0){
    print("유의수준 없음")
  }
}
fgls_process = function(model, y, contents, data){
  res_m = resid(model)
  formula_string <- paste("I(log(res_m^2))", contents)
  formula_string2 <- paste(y, contents)
  ols_uhat = lm(as.formula(formula_string), data=data)
  h = exp(fitted(ols_uhat))
  h_result = lm(as.formula(formula_string2), weights=1/h, data=data)
  return (summary(h_result))
}

### 코인
btc_data_day=read.csv("BTC-USD_daily.csv")
btc_data_day=as.data.frame(btc_data_day)
names(btc_data_day)
btc_df = pre_process(btc_data_day, btc_data_day$Close, 1)
names(btc_df) <- tolower(names(btc_df))
head(btc_df)


### 유가
DCOILWTICO_data_day=read.csv("DCOILWTICO.csv")
DCOILWTICO_data_day=as.data.frame(DCOILWTICO_data_day)
names(DCOILWTICO_data_day)
DCOILWTICO_df = pre_process(DCOILWTICO_data_day, DCOILWTICO_data_day$DCOILWTICO, 0)
names(DCOILWTICO_df) <- tolower(names(DCOILWTICO_df))
head(DCOILWTICO_df)


### 필라델피아
philadelpia_data_day=read.csv("Philadelpia_daily.csv")
philadelpia_data_day=as.data.frame(philadelpia_data_day)
names(philadelpia_data_day)
philadelpia_df = pre_process(philadelpia_data_day, philadelpia_data_day$Price_CLOSE, 0)
names(philadelpia_df) <- tolower(names(philadelpia_df))
head(philadelpia_df)

### 금
gold_data_day =read.csv("금달러SpotPrice.csv")
gold_data_day=as.data.frame(gold_data_day)
names(gold_data_day)
gold_data_day$Price = as.numeric(gold_data_day$Price)
gold_df = pre_process(gold_data_day, gold_data_day$Price, 0)
names(gold_df) <- tolower(names(gold_df))
head(gold_df)

### 국채 수익률
yeild_10d_data_day =read.csv("미국 국채 수익률 10년_d.csv")
yeild_10d_data_day=as.data.frame(yeild_10d_data_day)
yeild_10d_data_day$DGS10 = as.numeric(yeild_10d_data_day$DGS10)
names(yeild_10d_data_day)
yeild_10d_df = pre_process(yeild_10d_data_day, yeild_10d_data_day$DGS10, 0)
names(yeild_10d_df) <- tolower(names(yeild_10d_df))
head(yeild_10d_df)

yeild_2d_data_day =read.csv("미국 국채 수익률 2년_d.csv")
yeild_2d_data_day=as.data.frame(yeild_2d_data_day)
yeild_2d_data_day$DGS2 = as.numeric(yeild_2d_data_day$DGS2)
names(yeild_2d_data_day)
yeild_2d_df = pre_process(yeild_2d_data_day, yeild_2d_data_day$DGS2, 0)
names(yeild_2d_df) <- tolower(names(yeild_2d_df))
head(yeild_2d_df)

stock_data_day =read.csv("SP_NASDAQ_AMD_NVDA22.csv")
stock_data_day=as.data.frame(stock_data_day)
names(stock_data_day) <- tolower(names(stock_data_day))
names(stock_data_day)


nasdaq_df = pre_process(stock_data_day, stock_data_day$nasdaq, 0)
sp_df = pre_process(stock_data_day, stock_data_day$s.p.500, 0)
energy_df = pre_process(stock_data_day, stock_data_day$energy, 0)
materials_df = pre_process(stock_data_day, stock_data_day$materials, 0)
industrials_df = pre_process(stock_data_day, stock_data_day$industrials, 0)
consumer.discretionary_df = pre_process(stock_data_day, stock_data_day$consumer.discretionary, 0)
consumer.staples_df = pre_process(stock_data_day, stock_data_day$consumer.staples, 0)
health.care_df = pre_process(stock_data_day, stock_data_day$health.care, 0)
financials_df = pre_process(stock_data_day, stock_data_day$financials, 0)
information.technology_df = pre_process(stock_data_day, stock_data_day$information.technology, 0)
communication.services_df = pre_process(stock_data_day, stock_data_day$communication.services, 0)
utilities_df = pre_process(stock_data_day, stock_data_day$utilities, 0)
real.estate_df = pre_process(stock_data_day, stock_data_day$real.estate, 0)
amd_df = pre_process(stock_data_day, stock_data_day$amd, 0)


#btc_df, DCOILWTICO_df, philadelpia_df, gold_df, yeild_10d_df, yeild_2d_df
full_day = merge(btc_df, DCOILWTICO_df, by='date', suffixes = c("_btc", "_dco"))
full_day = merge(full_day, philadelpia_df, by='date')
full_day = merge(full_day, gold_df, by='date', suffixes = c("_phi", "_gold"))
full_day = merge(full_day, yeild_10d_df, by='date')
full_day = merge(full_day, yeild_2d_df, by='date', suffixes = c("_y10d", "_y2d"))
full_day = merge(full_day, nasdaq_df, by='date')
full_day = merge(full_day, sp_df, by='date', suffixes = c("_nasdaq", "_sp"))
full_day = merge(full_day, energy_df, by='date')
full_day = merge(full_day, industrials_df, by='date', suffixes = c("_energy", "_industrials"))
full_day = merge(full_day, consumer.discretionary_df, by='date')
full_day = merge(full_day, consumer.staples_df, by='date', suffixes = c("_discretionary", "_staples"))
full_day = merge(full_day, health.care_df, by='date')
full_day = merge(full_day, financials_df, by='date', suffixes = c("_health", "_financials"))
full_day = merge(full_day, information.technology_df, by='date')
full_day = merge(full_day, communication.services_df, by='date', suffixes = c("_it", "_cs"))
full_day = merge(full_day, utilities_df, by='date')
full_day = merge(full_day, materials_df, by='date', suffixes = c("_utilities", "_materials"))
full_day = merge(full_day, real.estate_df, by='date')
full_day = merge(full_day, amd_df, by='date', suffixes = c("_real.estate", "_amd"))

head(full_day)
names(full_day)
for (i in names(full_day)){
  if (grepl("change_rate", i)){
    print(i)
  }
}

#### 종가에 대한 전처리 전 그래프
plot(full_day$adj.close, type='l')
##########################
#### Data Pre Process ####
##########################
#### change_rate_btc -> (close_price[t+1] / close_price[t]) - 1
#### diff_btc -> close_price[t+1] - close_price[t]
#### up_down_btc -> close_price[t+1] > close_price[t] = 1   close_price[t+1] <= close_price[t] = -1
plot(full_day$change_rate_btc, type='b')
plot(full_day$diff_btc, type='b')
plot(full_day$up_down_btc, type='b')
#hist(full_day$change_rate_btc, freq = TRUE, breaks=seq(-0.5, 0.5, by=0.01))
#hist(full_day$diff_btc, freq = TRUE, breaks=seq(-10000, 10000, by=100))
#hist(full_day$up_down_btc, freq = TRUE, breaks=seq(-1, 1, by=1))


#########################
#### Stationary Test ####
#########################
library(tseries)
adf.test(full_day$adj.close)
full_day[is.na(full_day)] <- 0
adf.test(full_day$change_rate_btc)
adf.test(full_day$diff_btc)
adf.test(full_day$up_down_btc)


#####################################
#### Change rate에 대한 다중 선형 회귀 ####
#####################################
day_change_rate_str = ("~ change_rate_dco + change_rate_phi + change_rate_gold + change_rate_y10d + change_rate_y2d + change_rate_nasdaq + change_rate_sp +
+ change_rate_energy + change_rate_materials + change_rate_industrials + change_rate_discretionary + change_rate_staples + change_rate_health +
+ change_rate_financials + change_rate_it +  change_rate_cs + change_rate_utilities + change_rate_real.estate")
day_change_rate = lm(paste("change_rate_btc", day_change_rate_str), data=full_day)
#### 공선성 테스트 -> 변수간 공선성 있음
vif(day_change_rate)
#### 공선성 있는 변수 제거 후 다중 선형 회귀 (S&P500 NASDAQ)
day_change_rate_str2 = ("~ change_rate_dco + change_rate_phi + change_rate_gold + change_rate_y10d + change_rate_y2d +
+ change_rate_energy + change_rate_materials + change_rate_industrials + change_rate_discretionary + change_rate_staples + change_rate_health +
+ change_rate_financials + change_rate_it +  change_rate_cs + change_rate_utilities + change_rate_real.estate")
day_change_rate2 = lm(paste("change_rate_btc", day_change_rate_str2), data=full_day)
vif(day_change_rate2)
#### 이분산 테스트 -> 이분산 존재
bptest(day_change_rate2, as.formula(day_change_rate_str2), data=full_day)
#### 이분산 존재로 FGLS
fgls_process(day_change_rate2, "change_rate_btc", day_change_rate_str2, full_day)
#### 위의 모델에서 5% 미만의 유의수준으로 상관관계가 있는 변수만 사용
day_change_rate_str_final = ("~ change_rate_gold + change_rate_materials + change_rate_industrials + change_rate_financials + change_rate_cs")
#### 상관관계가 있는 변수만 사용하여 다중 선형 회귀 진행
day_change_rate_final = lm(paste("change_rate_btc", day_change_rate_str_final), data=full_day)
#### 이분산 테스트 -> 이분산 존재
bptest(day_change_rate_final, as.formula(day_change_rate_str_final), data=full_day)
#### 이분산 존재로 FGLS
fgls_process(day_change_rate_final,"change_rate_btc", day_change_rate_str_final, data=full_day)
### 5% 유의수준의 상관관계 있는 변수를 활용한 granger 인과 관계 테스트 -> 일봉 데이터이기 때문에 14까지 테스트 (2주)
### 귀무가설 : y의 원인으로 x가 인과관계가 없다 -> 비트코인의 변화 원인으로 독립변수가 원인이아니다.
granger_process(full_day$change_rate_btc, full_day$change_rate_gold , 14)
granger_process(full_day$change_rate_btc, full_day$change_rate_materials , 14)
granger_process(full_day$change_rate_btc, full_day$change_rate_industrials , 14)
granger_process(full_day$change_rate_btc, full_day$change_rate_financials , 14)
granger_process(full_day$change_rate_btc, full_day$change_rate_cs , 14)
### 귀무가설 : x의 원인으로 y가 인과관계가 없다 -> 독립변수의 변화 원인으로 비트코인의 변화가 원인이아니다.
granger_process(full_day$change_rate_btc, full_day$change_rate_gold , 14,1)
granger_process(full_day$change_rate_btc, full_day$change_rate_materials , 14,1) ### (6~14) 1%, (1) 5% 유의수준에서 귀무가설 기각
granger_process(full_day$change_rate_btc, full_day$change_rate_industrials , 14,1) ### (6~14) 1% 유의수준에서 귀무가설 기각
granger_process(full_day$change_rate_btc, full_day$change_rate_financials , 14,1) ### (6~8, 12~14) 1%, (1, 9~11) 5% 유의수준에서 귀무가설 기각
granger_process(full_day$change_rate_btc, full_day$change_rate_cs , 14,1)



###################################
#### diff rate에 대한 다중 선형 회귀 ####
###################################
day_diff_str = ("~ diff_dco + diff_phi + diff_gold + diff_y10d + diff_y2d + diff_nasdaq + diff_sp +
+ diff_energy + diff_materials + diff_industrials + diff_discretionary + diff_staples + diff_health +
+ diff_financials + diff_it +  diff_cs + diff_utilities + diff_real.estate")
day_diff = lm(paste("diff_btc", day_diff_str), data=full_day)
#### 공선성 테스트 -> 변수간 공선성 있음
vif(day_diff)
#### 공선성 있는 변수 제거 후 다중 선형 회귀 (S&P500 NASDAQ IT 제거)
day_diff_str2 = ("~ diff_dco + diff_phi + diff_gold + diff_y10d + diff_y2d +
+ diff_energy + diff_materials + diff_industrials + diff_discretionary + diff_staples + diff_health +
+ diff_financials +  diff_cs + diff_utilities + diff_real.estate")
day_diff2 = lm(paste("diff_btc", day_diff_str2), data=full_day)
vif(day_diff2)
#### 이분산 테스트 -> 이분산 존재
bptest(day_diff2, as.formula(day_diff_str2), data=full_day)
#### 이분산 존재로 FGLS
fgls_process(day_diff2, 'diff_btc', day_diff_str2, full_day)
#### 위의 모델에서 5% 미만의 유의수준으로 상관관계가 있는 변수만 사용
day_diff_str_final = ("~ diff_dco + diff_phi + diff_y2d + diff_materials + diff_industrials + diff_discretionary + diff_staples + diff_financials")
#### 상관관계가 있는 변수만 사용하여 다중 선형 회귀 진행
day_diff_final = lm(paste("diff_btc", day_diff_str_final), data=full_day)
#### 이분산 테스트 -> 이분산 존재
bptest(day_diff_final, as.formula(day_diff_str_final), data=full_day)
#### 이분산 존재로 FGLS
fgls_process(day_diff_final, 'diff_btc', day_diff_str_final, data=full_day)
### 5% 유의수준의 상관관계 있는 변수를 활용한 granger 인과 관계 테스트 -> 일봉 데이터이기 때문에 14까지 테스트 (2주)
### 귀무가설 : y의 원인으로 x가 인과관계가 없다 -> 비트코인의 변화 원인으로 독립변수가 원인이아니다.
granger_process(full_day$diff_btc, full_day$diff_dco , 14)
granger_process(full_day$diff_btc, full_day$diff_phi , 14)
granger_process(full_day$diff_btc, full_day$diff_y2d , 14) ### (4) 5% 유의수준에서 귀무가설 기각
granger_process(full_day$diff_btc, full_day$diff_materials , 14)  ### (10~12) 5% 유의수준에서 귀무가설 기각
granger_process(full_day$diff_btc, full_day$diff_industrials , 14) ### (11, 13~14) 1%, (6~10, 12) 5% 유의수준에서 귀무가설 기각
granger_process(full_day$diff_btc, full_day$diff_discretionary , 14) ### (11~14) 5% 유의수준에서 귀무가설 기각
granger_process(full_day$diff_btc, full_day$diff_staples , 14) ### (13~14) 5% 유의수준에서 귀무가설 기각
granger_process(full_day$diff_btc, full_day$diff_financials , 14) ### (6~14) 5% 유의수준에서 귀무가설 기각
### 귀무가설 : x의 원인으로 y가 인과관계가 없다 -> 독립변수의 변화 원인으로 비트코인의 변화가 원인이아니다.
granger_process(full_day$diff_btc, full_day$diff_dco , 14,1) ### (2) 5% 유의수준에서 귀무가설 기각
granger_process(full_day$diff_btc, full_day$diff_phi , 14,1) ### (11~14) 1%, (2~8, 10) 5% 유의수준에서 귀무가설 기각
granger_process(full_day$diff_btc, full_day$diff_y2d , 14,1)
granger_process(full_day$diff_btc, full_day$diff_materials , 14,1)  ### (1~14) 1% 유의수준에서 귀무가설 기각
granger_process(full_day$diff_btc, full_day$diff_industrials , 14) ### (11, 13~14) 1%, (6~10, 12) 5% 유의수준에서 귀무가설 기각
granger_process(full_day$diff_btc, full_day$diff_discretionary , 14,1) ### (2~7) 1%, (1, 8~14) 5% 유의수준에서 귀무가설 기각
granger_process(full_day$diff_btc, full_day$diff_staples , 14,1) ### (6~7) 1%, (2~3, 8~10) 5% 유의수준에서 귀무가설 기각
granger_process(full_day$diff_btc, full_day$diff_financials , 14,1) ### (1~14) 1% 유의수준에서 귀무가설 기각



######################################
#### up_down rate에 대한 다중 선형 회귀 ####
######################################
day_up_down_str = ("~ up_down_dco + up_down_phi + up_down_gold + up_down_y10d + up_down_y2d + up_down_nasdaq + up_down_sp +
+ up_down_energy + up_down_materials + up_down_industrials + up_down_discretionary + up_down_staples + up_down_health +
+ up_down_financials + up_down_it +  up_down_cs + up_down_utilities + up_down_real.estate")
day_up_down = lm(paste("up_down_btc", day_up_down_str), data=full_day)
#### 공선성 테스트 -> 변수간 공선성 없음
vif(day_up_down)
#### 이분산 테스트 -> 이분산 존재
bptest(day_up_down, as.formula(day_up_down_str), data=full_day)
#### 이분산 존재로 FGLS
fgls_process(day_up_down, 'up_down_btc', day_up_down_str, full_day)
#### 위의 모델에서 5% 미만의 유의수준으로 상관관계가 있는 변수만 사용 사용후 -> 유의하지 않은 변수 제거 (Energy)
day_up_down_str2 = ("~ up_down_y2d + up_down_health + up_down_cs")
#### 상관관계가 있는 변수만 사용하여 다중 선형 회귀 진행
day_up_down2 = lm(paste("up_down_btc", day_up_down_str2), data=full_day)
#### 이분산 테스트 -> 이분산 존재
bptest(day_up_down2, as.formula(day_up_down_str2), data=full_day)
#### 이분산 존재로 FGLS
fgls_process(day_up_down2, 'up_down_btc', day_up_down_str2, full_day)
### 5% 유의수준의 상관관계 있는 변수를 활용한 granger 인과 관계 테스트 -> 일봉 데이터이기 때문에 14까지 테스트 (2주)
### 귀무가설 : y의 원인으로 x가 인과관계가 없다 -> 비트코인의 변화 원인으로 독립변수가 원인이아니다.
granger_process(full_day$up_down_btc, full_day$up_down_y2d , 14)
granger_process(full_day$up_down_btc, full_day$up_down_energy , 14)
granger_process(full_day$up_down_btc, full_day$up_down_cs , 14)
### 귀무가설 : x의 원인으로 y가 인과관계가 없다 -> 독립변수의 변화 원인으로 비트코인의 변화가 원인이아니다.
granger_process(full_day$up_down_btc, full_day$up_down_y2d , 14,1)
granger_process(full_day$up_down_btc, full_day$up_down_energy , 14,1)
granger_process(full_day$up_down_btc, full_day$up_down_cs , 14,1) ### (3) 1%, (1, 4~8, 13~14) 5% 유의수준에서 귀무가설 기각







#### 테스트용
#### y와 x를 섞어서 번외 실험 이것 저것
day_change_with_diff = lm(change_rate_btc ~  diff_phi + diff_gold + diff_materials + diff_industrials + diff_discretionary + diff_financials +  diff_cs, data=full_day)
vif(day_change_with_diff)
summary(day_change_with_diff, vcov=vcovHC)
day_change_with_updown = lm(change_rate_btc ~ up_down_nasdaq + up_down_energy + up_down_materials + up_down_cs + up_down_utilities, data=full_day)
vif(day_change_with_updown)
summary(day_change_with_updown, vcov=vcovHC)

day_diff_with_rate = lm(diff_btc ~ change_rate_phi + change_rate_y2d + change_rate_materials + change_rate_industrials + change_rate_discretionary + change_rate_staples + change_rate_health + change_rate_it, data=full_day)
vif(day_diff_with_rate)
summary(day_diff_with_rate, vcov=vcovHC)
day_diff_whth_updown = lm(diff_btc ~ up_down_phi +  up_down_y2d + up_down_nasdaq + up_down_energy + up_down_materials + up_down_financials +  up_down_cs + up_down_utilities, data=full_day)
vif(day_diff_whth_updown)
summary(day_diff_whth_updown, vcov=vcovHC)

day_updown_with_rate = lm(up_down_btc ~ change_rate_gold + change_rate_materials + change_rate_industrials + change_rate_cs, data=full_day)
vif(day_updown_with_rate)
summary(day_updown_with_rate)
day_updown_with_diff = lm(up_down_btc ~ diff_gold + diff_materials + diff_industrials + diff_cs , data=full_day)
vif(day_updown_with_diff)
summary(day_updown_with_diff)







