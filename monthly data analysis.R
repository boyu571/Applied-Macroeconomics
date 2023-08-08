library(readxl)

data <- read.csv("C:/Users/boyu571/OneDrive/바탕 화면/data/monthly_final.csv")
df <- data[,-1]

ols = lm(log(Bitcoin_close)~gold+dollar_index+CPI+PPI+UNRATE+FEDFUNDS, data = df)
summary(ols)

normalize <- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}

normalize()