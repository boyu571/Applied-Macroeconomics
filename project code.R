library(dplyr)
library(readxl)
library(car)
library(sandwich)
library(lmtest)
library(orcutt)
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
gold_data_day =read.csv("금달러SpotPrice.csv")
gold_data_day=as.data.frame(gold_data_day)
names(gold_data_day)
gold_data_day$Price = as.numeric(gold_data_day$Price)
gold_df = pre_process(gold_data_day, gold_data_day$Price, 0)
names(gold_df) <- tolower(names(gold_df))
head(gold_df)

### 국채 수익률
yeild_10d_data_day =read.csv("미국 국채 수익률 10년_d.csv")
yeild_10d_data_day=as.data.frame(yeild_10d_data_day)
yeild_10d_data_day$DGS10 = as.numeric(yeild_10d_data_day$DGS10)
names(yeild_10d_data_day)
yeild_10d_df = pre_process(yeild_10d_data_day, yeild_10d_data_day$DGS10, 0)
names(yeild_10d_df) <- tolower(names(yeild_10d_df))
head(yeild_10d_df)

yeild_2d_data_day =read.csv("미국 국채 수익률 2년_d.csv")
yeild_2d_data_day=as.data.frame(yeild_2d_data_day)
yeild_2d_data_day$DGS2 = as.numeric(yeild_2d_data_day$DGS2)
names(yeild_2d_data_day)
yeild_2d_df = pre_process(yeild_2d_data_day, yeild_2d_data_day$DGS2, 0)
names(yeild_2d_df) <- tolower(names(yeild_2d_df))
head(yeild_2d_df)

#btc_df, DCOILWTICO_df, philadelpia_df, gold_df, yeild_10d_df, yeild_2d_df
full_df = merge(btc_df, DCOILWTICO_df, by='date', suffixes = c("_btc", "_dco"))
full_df = merge(full_df, philadelpia_df, by='date')
full_df = merge(full_df, gold_df, by='date', suffixes = c("_phi", "_gold"))
full_df = merge(full_df, yeild_10d_df, by='date')
full_df = merge(full_df, yeild_2d_df, by='date', suffixes = c("_y10d", "_y2d"))
head(full_df)
names(full_df)
#full_df <- na.omit(full_df)

ols_full = lm(change_rate_btc ~ change_rate_dco + change_rate_phi + change_rate_gold + change_rate_y10d + change_rate_y2d, data=full_df)
summary(ols_full)

ols_diff = lm(diff_btc ~ diff_dco + diff_phi + diff_gold + diff_y10d + diff_y2d, data=full_df)
summary(ols_diff)

ols_up_down = lm(up_down_btc ~ up_down_dco + up_down_phi + up_down_gold + up_down_y10d + up_down_y2d, data=full_df)
summary(ols_up_down)



