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

setwd('/Users/jkim/Library/Mobile Documents/com~apple~CloudDocs/퀀트응용경제학/과제/quant/Crypto/daily')
getwd()
list.files()

pre_process = function(df, close_price, index, title){
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

  up_down = as.data.frame(df$up_down, col_names = FALSE)
  diff = as.data.frame(df$diff, col_names = FALSE)
  change_rate = as.data.frame(df$change_rate, col_names = FALSE)
  new_df <- cbind(up_down, diff, change_rate)
  #colnames(new_df) <- c('Date', 'up_down', 'diff', 'change_rate')
  #print(new_df)
  #new_df$up_down = as.numeric(new_df$up_down)
  #new_df$diff = as.numeric(new_df$diff)
  #new_df$change_rate = as.numeric(new_df$change_rate)
  colnames(new_df) <- c(paste0(title, '_up_down'),paste0(title,'_diff'),paste0(title,'_change_rate'))
  return (new_df)
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

btc_data_month=read.csv("all_monthly_data.csv")
btc_data_month=as.data.frame(btc_data_month)
names(btc_data_month)
head(btc_data_month)

for (i in names(btc_data_month)){
  if (i == 'Bitcoin'){
    full_month = pre_process(btc_data_month, btc_data_month[[i]], 1, i)
  }
  else if (i != 'Date'){
    tmp_df = pre_process(btc_data_month, btc_data_month[[i]], 0, i)
    full_month = cbind(full_month, tmp_df)
  }
}
full_month = as.data.frame(full_month)
head(full_month)
names(full_month)

names(full_month)
for (i in names(full_month)){
  if (grepl("change_rate", i)){
    print(i)
  }
}



#### 종가에 대한 전처리 전 그래프
plot(btc_data_month$Bitcoin, type='l')
##########################
#### Data Pre Process ####
##########################
#### change_rate_btc -> (close_price[t+1] / close_price[t]) - 1
#### diff_btc -> close_price[t+1] - close_price[t]
#### up_down_btc -> close_price[t+1] > close_price[t] = 1   close_price[t+1] <= close_price[t] = -1
plot(full_month$Bitcoin_change_rate, type='b')
plot(full_month$Bitcoin_diff, type='b')
plot(full_month$Bitcoin_up_down, type='b')

#########################
#### Stationary Test ####
#########################
library(tseries)
full_month[is.na(full_month)] <- 0
adf.test(full_month$Bitcoin_change_rate)
adf.test(full_month$Bitcoin_diff)
adf.test(full_month$Bitcoin_up_down)



#####################################
#### Change rate에 대한 다중 선형 회귀 ####
#####################################
month_change_rate_str = ("~ M1_change_rate + PHILA_change_rate + NASDAQ_change_rate + SP500_change_rate +
  + Energy_change_rate + Materials_change_rate + Industrials_change_rate + Consumer.Discretionary_change_rate +
  + Consumer.Staples_change_rate + Health.Care_change_rate + Financials_change_rate + IT_change_rate + Communication.Services_change_rate +
  + Utilities_change_rate + Real.Estate_change_rate + Gold_change_rate  +Bond10yr_change_rate   + Bond2yr_change_rate + Oil_change_rate+
  + dollar_index_change_rate + CPI_change_rate +PPI_change_rate + UNRATE_change_rate + FEDFUNDS_change_rate")
month_change_rate = lm(paste("Bitcoin_change_rate", month_change_rate_str), data=full_month)
#### 공선성 테스트 -> 변수간 공선성 있음
vif(month_change_rate)
#### 공선성 있는 변수 제거 후 다중 선형 회귀 (S&P500 NASDAQ Industrials)
month_change_rate_str2 = ("~ M1_change_rate + PHILA_change_rate +
  + Energy_change_rate + Materials_change_rate + Consumer.Discretionary_change_rate +
  + Consumer.Staples_change_rate + Health.Care_change_rate + Financials_change_rate + IT_change_rate + Communication.Services_change_rate +
  + Utilities_change_rate + Real.Estate_change_rate + Gold_change_rate  +Bond10yr_change_rate   + Bond2yr_change_rate + Oil_change_rate+
  + dollar_index_change_rate + CPI_change_rate +PPI_change_rate + UNRATE_change_rate + FEDFUNDS_change_rate")
month_change_rate2 = lm(paste("Bitcoin_change_rate", month_change_rate_str2), data=full_month)
vif(month_change_rate2)
#### 이분산 테스트 -> 이분산 없음
bptest(month_change_rate2, as.formula(month_change_rate_str2), data=full_month)
summary(month_change_rate2)
#### 위의 모델에서 5% 미만의 유의수준으로 상관관계가 있는 변수만 사용
month_change_rate_str_final = ("~ PHILA_change_rate")#### 상관관계가 있는 변수만 사용하여 다중 선형 회귀 진행
month_change_rate_final = lm(paste("Bitcoin_change_rate", month_change_rate_str_final), data=full_month)
#### 이분산 테스트 -> 이분산 없음
bptest(month_change_rate_final, as.formula(month_change_rate_str_final), data=full_month)
### 상관관계가 있는 변수만 사용했을때의 최종 결과 -> PHILA_change_rate만 유의하다고 나왔으나 최종적으로 나머지 변수 제거시에 유의하지않음
summary(month_change_rate_final)
### 상관관계 있는 변수를 활용한 granger 인과 관계 테스트 -> 월봉 데이터이기 때문에 3까지 테스트 (3달 : 분기)
### 귀무가설 : y의 원인으로 x가 인과관계가 없다 -> 비트코인의 변화 원인으로 독립변수가 원인이아니다.
granger_process(full_month$Bitcoin_change_rate, full_month$PHILA_change_rate , 3) ### (1,2) 5% 유의수준에서 귀무가설 기각
### 귀무가설 : x의 원인으로 y가 인과관계가 없다 -> 독립변수의 변화 원인으로 비트코인의 변화가 원인이아니다.
granger_process(full_month$Bitcoin_change_rate, full_month$PHILA_change_rate , 3,1)



##############################
#### Diff에 대한 다중 선형 회귀 ####
##############################
month_diff_str = ("~ M1_diff + PHILA_diff + NASDAQ_diff + SP500_diff +
  + Energy_diff + Materials_diff + Industrials_diff + Consumer.Discretionary_diff +
  + Consumer.Staples_diff + Health.Care_diff + Financials_diff + IT_diff + Communication.Services_diff +
  + Utilities_diff + Real.Estate_diff + Gold_diff  +Bond10yr_diff   + Bond2yr_diff + Oil_diff+
  + dollar_index_diff + CPI_diff +PPI_diff + UNRATE_diff + FEDFUNDS_diff")
month_diff = lm(paste("Bitcoin_diff", month_diff_str), data=full_month)
#### 공선성 테스트 -> 변수간 공선성 있음
vif(month_diff)
#### 공선성 있는 변수 제거 후 다중 선형 회귀 (S&P500 NASDAQ Industrials)
month_diff_str2 = ("~ M1_diff + PHILA_diff +
  + Energy_diff + Materials_diff + Consumer.Discretionary_diff +
  + Consumer.Staples_diff + Health.Care_diff + Financials_diff + IT_diff + Communication.Services_diff +
  + Utilities_diff + Real.Estate_diff + Gold_diff  +Bond10yr_diff   + Bond2yr_diff + Oil_diff+
  + dollar_index_diff + CPI_diff +PPI_diff + UNRATE_diff + FEDFUNDS_diff")
month_diff2 = lm(paste("Bitcoin_diff", month_diff_str2), data=full_month)
vif(month_diff2)
#### 이분산 테스트 -> 이분산 있음
bptest(month_diff2, as.formula(month_diff_str2), data=full_month)
#### 이분산 존재로 FGLS
fgls_process(month_diff2, 'Bitcoin_diff', month_diff_str2, full_month)
#### 위의 모델에서 5% 미만의 유의수준으로 상관관계가 있는 변수만 사용 사용후 -> 유의하지 않은 변수 제거 (Energy_diff, Health.Care_diff, UNRATE_diff, Oil_diff, Utilities_diff)
month_diff_str_final = ("~ PHILA_diff + Consumer.Discretionary_diff + Consumer.Staples_diff")#### 상관관계가 있는 변수만 사용하여 다중 선형 회귀 진행
month_diff_final = lm(paste("Bitcoin_diff", month_diff_str_final), data=full_month)
#### 이분산 테스트 -> 이분산 없음
bptest(month_diff_final, as.formula(month_diff_str_final), data=full_month)
### 상관관계가 있는 변수만 사용했을때의 최종 결과 -> PHILA_diff만 유의하다고 나왔으나 최종적으로 나머지 변수 제거시에 유의하지않음
summary(month_diff_final)
### 상관관계 있는 변수를 활용한 granger 인과 관계 테스트 -> 월봉 데이터이기 때문에 3까지 테스트 (3달 : 분기)
### 귀무가설 : y의 원인으로 x가 인과관계가 없다 -> 비트코인의 변화 원인으로 독립변수가 원인이아니다.
granger_process(full_month$Bitcoin_diff, full_month$PHILA_change_rate , 3) ### (1,3) 5% 유의수준에서 귀무가설 기각
granger_process(full_month$Bitcoin_diff, full_month$Consumer.Discretionary_diff  , 3) ### (1) 5% 유의수준에서 귀무가설 기각
granger_process(full_month$Bitcoin_diff, full_month$Consumer.Staples_diff , 3) ### (1,3) 5% 유의수준에서 귀무가설 기각
### 귀무가설 : x의 원인으로 y가 인과관계가 없다 -> 독립변수의 변화 원인으로 비트코인의 변화가 원인이아니다.
granger_process(full_month$Bitcoin_diff, full_month$PHILA_change_rate , 3,1)
granger_process(full_month$Bitcoin_diff, full_month$Consumer.Discretionary_diff  , 3,1)
granger_process(full_month$Bitcoin_diff, full_month$Consumer.Staples_diff , 3,1)


#################################
#### up_down에 대한 다중 선형 회귀 ####
#################################
month_up_down_str = ("~ M1_up_down + PHILA_up_down + NASDAQ_up_down + SP500_up_down +
  + Energy_up_down + Materials_up_down + Industrials_up_down + Consumer.Discretionary_up_down +
  + Consumer.Staples_up_down + Health.Care_up_down + Financials_up_down + IT_up_down + Communication.Services_up_down +
  + Utilities_up_down + Real.Estate_up_down + Gold_up_down  +Bond10yr_up_down   + Bond2yr_up_down + Oil_up_down+
  + dollar_index_up_down + CPI_up_down +PPI_up_down + UNRATE_up_down + FEDFUNDS_up_down")
month_up_down = lm(paste("Bitcoin_up_down", month_up_down_str), data=full_month)
#### 공선성 테스트 -> 변수간 공선성 없음
vif(month_up_down)
summary(month_up_down)
#### 이분산 테스트 -> 이분산 없음
bptest(month_up_down, as.formula(month_up_down_str), data=full_month)
#### 위의 모델에서 5% 미만의 유의수준으로 상관관계가 있는 변수만 사용
month_up_down_str_final = ("~ UNRATE_up_down")#### 상관관계가 있는 변수만 사용하여 다중 선형 회귀 진행
month_up_down_final = lm(paste("Bitcoin_up_down", month_up_down_str_final), data=full_month)
#### 이분산 테스트 -> 이분산 없음
bptest(month_up_down_final, as.formula(month_up_down_str_final), data=full_month)
### 상관관계가 있는 변수만 사용했을때의 최종 결과 -> PHILA_up_down만 유의하다고 나왔으나 최종적으로 나머지 변수 제거시에 유의하지않음
summary(month_up_down_final)
### 상관관계 있는 변수를 활용한 granger 인과 관계 테스트 -> 월봉 데이터이기 때문에 3까지 테스트 (3달 : 분기)
### 귀무가설 : y의 원인으로 x가 인과관계가 없다 -> 비트코인의 변화 원인으로 독립변수가 원인이아니다.
granger_process(full_month$Bitcoin_up_down, full_month$UNRATE_up_down , 3)
### 귀무가설 : x의 원인으로 y가 인과관계가 없다 -> 독립변수의 변화 원인으로 비트코인의 변화가 원인이아니다.
granger_process(full_month$Bitcoin_up_down, full_month$UNRATE_up_down , 3,1)







