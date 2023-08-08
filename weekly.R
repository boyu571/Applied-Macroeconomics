
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

btc_data_week=read.csv("all_weekly_data.csv")
btc_data_week=as.data.frame(btc_data_week)
names(btc_data_week)
head(btc_data_week)
for (i in names(btc_data_week)){
  if (i == 'Bitcoin'){
    full_week = pre_process(btc_data_week, btc_data_week[[i]], 1, i)
  }
  else if (i != 'Date'){
    tmp_df = pre_process(btc_data_week, btc_data_week[[i]], 0, i)
    full_week = cbind(full_week, tmp_df)
  }
}
full_week = as.data.frame(full_week)
head(full_week)
names(full_week)



#### 종가에 대한 전처리 전 그래프
plot(btc_data_week$Bitcoin, type='l')
##########################
#### Data Pre Process ####
##########################
#### change_rate_btc -> (close_price[t+1] / close_price[t]) - 1
#### diff_btc -> close_price[t+1] - close_price[t]
#### up_down_btc -> close_price[t+1] > close_price[t] = 1   close_price[t+1] <= close_price[t] = -1
plot(full_week$Bitcoin_change_rate, type='b')
plot(full_week$Bitcoin_diff, type='b')
plot(full_week$Bitcoin_up_down, type='b')

#########################
#### Stationary Test ####
#########################
library(tseries)
full_week[is.na(full_week)] <- 0
adf.test(full_week$Bitcoin_change_rate)
adf.test(full_week$Bitcoin_diff)
adf.test(full_week$Bitcoin_up_down)


#####################################
#### Change rate에 대한 다중 선형 회귀 ####
#####################################
week_change_rate_str = ("~ M1_change_rate + PHILA_change_rate + NASDAQ_change_rate + SP500_change_rate +
  + Energy_change_rate + Materials_change_rate + Industrials_change_rate + Consumer.Discretionary_change_rate +
  + Consumer.Staples_change_rate + Health.Care_change_rate + Financials_change_rate + IT_change_rate + Communication.Services_change_rate +
  + Utilities_change_rate + Real.Estate_change_rate + Gold_change_rate  +Bond10yr_change_rate   + Bond2yr_change_rate + Oil_change_rate")
week_change_rate = lm(paste("Bitcoin_change_rate", week_change_rate_str), data=full_week)
#### 공선성 테스트 -> 변수간 공선성 있음
vif(week_change_rate)
#### 공선성 있는 변수 제거 후 다중 선형 회귀 (S&P500 NASDAQ)
week_change_rate_str2 = ("~ M1_change_rate + PHILA_change_rate +
  + Energy_change_rate + Materials_change_rate + Industrials_change_rate + Consumer.Discretionary_change_rate +
  + Consumer.Staples_change_rate + Health.Care_change_rate + Financials_change_rate + IT_change_rate + Communication.Services_change_rate +
  + Utilities_change_rate + Real.Estate_change_rate + Gold_change_rate  +Bond10yr_change_rate   + Bond2yr_change_rate + Oil_change_rate")
week_change_rate2 = lm(paste("Bitcoin_change_rate", week_change_rate_str2), data=full_week)
vif(week_change_rate2)
#### 이분산 테스트 -> 이분산 없음
bptest(week_change_rate2, as.formula(week_change_rate_str2), data=full_week)
summary(week_change_rate2)
#### 위의 모델에서 5% 미만의 유의수준으로 상관관계가 있는 변수만 사용 사용후 -> 유의하지 않은 변수 제거 (Health.Care_change_rate)
week_change_rate_str_final = ("~ Health.Care_change_rate + IT_change_rate + Communication.Services_change_rate")#### 상관관계가 있는 변수만 사용하여 다중 선형 회귀 진행
week_change_rate_final = lm(paste("Bitcoin_change_rate", week_change_rate_str_final), data=full_week)
#### 이분산 테스트 -> 이분산 없음
bptest(week_change_rate_final, as.formula(week_change_rate_str_final), data=full_week)
summary(week_change_rate_final)
### 5% 유의수준의 상관관계 있는 변수를 활용한 granger 인과 관계 테스트 -> 주봉 데이터이기 때문에 4까지 테스트 (1달)
### 귀무가설 : y의 원인으로 x가 인과관계가 없다 -> 비트코인의 변화 원인으로 독립변수가 원인이아니다.
granger_process(full_week$Bitcoin_change_rate, full_week$PHILA_change_rate , 4)
granger_process(full_week$Bitcoin_change_rate, full_week$IT_change_rate , 4)
granger_process(full_week$Bitcoin_change_rate, full_week$Communication.Services_change_rate , 4)
### 귀무가설 : x의 원인으로 y가 인과관계가 없다 -> 독립변수의 변화 원인으로 비트코인의 변화가 원인이아니다.
granger_process(full_week$Bitcoin_change_rate, full_week$PHILA_change_rate , 4,1) ### (2,3) 1% 유의수준, (1,4) 5% 유의수준에서 귀무가설 기각
granger_process(full_week$Bitcoin_change_rate, full_week$IT_change_rate , 4,1) ### (2~4) 1% 유의수준, (1) 5% 유의수준에서 귀무가설 기각
granger_process(full_week$Bitcoin_change_rate, full_week$Communication.Services_change_rate , 4,1) ### (2, 3) 1% 유의수준, (1, 4) 5% 유의수준에서 귀무가설 기각



##############################
#### Diff에 대한 다중 선형 회귀 ####
##############################
week_diff_str = ("~ M1_diff + PHILA_diff + NASDAQ_diff + SP500_diff +
  + Energy_diff + Materials_diff + Industrials_diff + Consumer.Discretionary_diff +
  + Consumer.Staples_diff + Health.Care_diff + Financials_diff + IT_diff + Communication.Services_diff +
  + Utilities_diff + Real.Estate_diff + Gold_diff  +Bond10yr_diff   + Bond2yr_diff + Oil_diff")
week_diff = lm(paste("Bitcoin_diff", week_diff_str), data=full_week)
#### 공선성 테스트 -> 변수간 공선성 있음
vif(week_diff)
#### 공선성 있는 변수 제거 후 다중 선형 회귀 (S&P500 NASDAQ)
week_diff_str2 = ("~ M1_diff + PHILA_diff +
  + Energy_diff + Materials_diff + Industrials_diff + Consumer.Discretionary_diff +
  + Consumer.Staples_diff + Health.Care_diff + Financials_diff + IT_diff + Communication.Services_diff +
  + Utilities_diff + Real.Estate_diff + Gold_diff  +Bond10yr_diff + Bond2yr_diff + Oil_diff")
week_diff2 = lm(paste("Bitcoin_diff", week_diff_str2), data=full_week)
vif(week_diff2)
#### 이분산 테스트 -> 1% 유의성으로 보면 이분산 없음
bptest(week_diff2, as.formula(week_diff_str2), data=full_week)
summary(week_diff2)
#### 위의 모델에서 5% 미만의 유의수준으로 상관관계가 있는 변수만 사용
week_diff_str_final = ("~ Financials_diff + Communication.Services_diff + Bond2yr_diff")
#### 상관관계가 있는 변수만 사용하여 다중 선형 회귀 진행
week_diff_final = lm(paste("Bitcoin_diff", week_diff_str_final), data=full_week)
#### 이분산 테스트 -> 이분산 없음
bptest(week_diff_final, as.formula(week_diff_str_final), data=full_week)
summary(week_diff_final)
### 5% 유의수준의 상관관계 있는 변수를 활용한 granger 인과 관계 테스트 -> 일봉 데이터이기 때문에 14까지 테스트 (2주)
### 귀무가설 : y의 원인으로 x가 인과관계가 없다 -> 비트코인의 변화 원인으로 독립변수가 원인이아니다.
granger_process(full_week$Bitcoin_diff, full_week$Financials_diff , 4) ### (1) 5% 유의수준에서 귀무가설 기각
granger_process(full_week$Bitcoin_diff, full_week$Communication.Services_diff , 4)
granger_process(full_week$Bitcoin_diff, full_week$Bond2yr_diff , 4)
### 귀무가설 : x의 원인으로 y가 인과관계가 없다 -> 독립변수의 변화 원인으로 비트코인의 변화가 원인이아니다.
granger_process(full_week$Bitcoin_diff, full_week$Financials_diff , 4,1) ### (1~4) 1% 유의수준에서 귀무가설 기각
granger_process(full_week$Bitcoin_diff, full_week$Communication.Services_diff , 4,1) ### (1~4) 1% 유의수준에서 귀무가설 기각
granger_process(full_week$Bitcoin_diff, full_week$Bond2yr_diff , 4,1)


##################################
#### up_down 에 대한 다중 선형 회귀 ####
##################################
week_up_down_str = ("~ M1_up_down + PHILA_up_down + NASDAQ_up_down + SP500_up_down +
  + Energy_up_down + Materials_up_down + Industrials_up_down + Consumer.Discretionary_up_down +
  + Consumer.Staples_up_down + Health.Care_up_down + Financials_up_down + IT_up_down + Communication.Services_up_down +
  + Utilities_up_down + Real.Estate_up_down + Gold_up_down  +Bond10yr_up_down   + Bond2yr_up_down + Oil_up_down")
week_up_down = lm(paste("Bitcoin_up_down", week_up_down_str), data=full_week)
#### 공선성 테스트 -> 변수간 공선성 없음
vif(week_up_down)
#### 이분산 테스트 -> 이분산 존재
bptest(week_up_down, as.formula(week_up_down_str), data=full_week)
#### 이분산 존재로 FGLS
fgls_process(week_up_down, 'Bitcoin_up_down', week_up_down_str, full_week)
#### 위의 모델에서 10% 미만의 유의수준으로 상관관계가 있는 변수만 사용
week_up_down_str2 = ("~ M1_up_down + Health.Care_up_down  + Financials_up_down")
#### 상관관계가 있는 변수만 사용하여 다중 선형 회귀 진행
week_up_down2 = lm(paste("Bitcoin_up_down", week_up_down_str2), data=full_week)
#### 이분산 테스트 -> 이분산 존재
bptest(week_up_down2, as.formula(week_up_down_str2), data=full_week)
#### 이분산 존재로 FGLS
fgls_process(week_up_down2, 'Bitcoin_up_down', week_up_down_str2, full_week)


### 상관관계 있는 변수를 활용한 granger 인과 관계 테스트 -> 주봉 데이터이기 때문에 4까지 테스트 (1달)
### 귀무가설 : y의 원인으로 x가 인과관계가 없다 -> 비트코인의 변화 원인으로 독립변수가 원인이아니다.
granger_process(full_week$Bitcoin_up_down, full_week$M1_up_down , 4)
granger_process(full_week$Bitcoin_up_down, full_week$Health.Care_up_down , 4)
granger_process(full_week$Bitcoin_up_down, full_week$Financials_up_down , 4)
granger_process(full_week$Bitcoin_up_down, full_week$Utilities_up_down , 4)
### 귀무가설 : x의 원인으로 y가 인과관계가 없다 -> 독립변수의 변화 원인으로 비트코인의 변화가 원인이아니다.
granger_process(full_week$Bitcoin_up_down, full_week$M1_up_down , 4,1)
granger_process(full_week$Bitcoin_up_down, full_week$Health.Care_up_down , 4,1)  ### (2,3) 1% 유의수준, (4) 5% 유의수준에서 귀무가설 기각
granger_process(full_week$Bitcoin_up_down, full_week$Financials_up_down , 4,1) ### (2~4) 1% 유의수준
granger_process(full_week$Bitcoin_up_down, full_week$Utilities_up_down , 4,1)


