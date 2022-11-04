# task N4

install.packages("tidyquant")
library(tidyquant)
library(ggplot2)
library(dplyr)
library(patchwork)
library(hrbrthemes)

# Downloading S&P500 prices from 2000 to 2020 years for every 2 years 
# and calculating value at risk

values_at_risk = vector(length = 10, mode = 'list')
for (i in seq_along(values_at_risk)){
  from = sprintf("20%02d-01-01", (i - 1) * 2)
  to = sprintf("20%02d-01-01", (i - 1) * 2 + 2)
  getSymbols("^GSPC", from = from, to = to, warnings = FALSE)
  values_at_risk[[i]] = var(diff(log(drop(coredata(Ad(GSPC)))))) 
}

table(values_at_risk)

# Downloading S&P500 prices from 2000 to 2020 years

getSymbols("^GSPC", from = '2000-01-01',
           to = "2020-01-01",warnings = FALSE,
           auto.assign = TRUE)

GSPC = drop(coredata(Ad(GSPC)))[1:5030]

# Building dataframe

values_at_risk = unlist(rep(values_at_risk, each=503))

data = data.frame(
  day = as.Date("2000-01-01") + 0:5029,
  values_at_risk = values_at_risk,
  GSPC_price = GSPC
)

# Value used to transform the data

coeff = 6000000

# Color constants

varsColor = "#66b2a3"
priceColor = rgb(0.19, 0.59, 0.89, 1)

# Plotting

ggplot(data, aes(x = day)) +
  geom_line(aes(y = GSPC_price), size = 0.3, color = priceColor) + 
  geom_line(aes(y = values_at_risk * coeff), size = 1.5, color = varsColor) +
  scale_y_continuous(
    name = "S&P500 price",
    sec.axis = sec_axis(~./coeff, name = "Value at risk")
  ) + 
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = priceColor, size = 13),
    axis.title.y.right = element_text(color = varsColor, size = 13)
  ) +
  ggtitle("S&P500 price and its value at risk from 2000 to 2020")
