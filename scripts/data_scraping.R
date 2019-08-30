require(rvest)
require(dplyr)
require(lubridate)
require(ggplot2)
require(readr)


# Собираем данные с сайта CoinMarketCap (за период с 2013-04-28 по 2019-08-24):
base_url <- "https://coinmarketcap.com/"
currency <- "currencies/bitcoin/historical-data/"
period <- "?start=20130428&end=20190825"
page <- paste0(base_url, currency, period)

dat <- read_html(page) %>% 
    html_node("table") %>% 
    html_table() %>% 
    rename(y = "Close**",
           ds = "Date") %>% 
    select(y, ds) %>% 
    mutate(ds = mdy(ds))


# Первое знакомство с данными:
dat %>% 
    ggplot(., aes(ds, y)) +
    geom_line() + 
    labs(x = "Дата", y = "Стоимость ($)") +
    theme_minimal()

# Тот же временной ряд, но начиная с 1 января 2016 г.:
dat %>% 
    filter(ds >= as.Date("2016-01-01")) %>% 
    ggplot(., aes(ds, y)) +
    geom_line() + 
    theme_minimal()

# То же, но после логарифмирования отклика:
dat %>% 
    filter(ds >= as.Date("2016-01-01")) %>% 
    ggplot(., aes(ds, log(y))) +
    geom_line() + 
    labs(x = "Дата", y = "log стоимости ($)") +
    theme_minimal()

# Сохряняем данные для дальнейшего анализа:
write_csv(dat, "data/bitcoin_closing_price.csv")
