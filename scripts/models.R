require(readr)
require(dplyr)
require(prophet)
require(ggplot2)


# Загружаем исходных данных:
dat <- read_csv(file = "data/bitcoin_closing_price.csv")

# Удаляем наблюдения, которые старше 2016-01-01, и логарифмируем отклик
# (логарифмирование поможет "бороться" с большой дисперсией в данных):
clean_data <- dat %>% 
    filter(ds >= as.Date("2016-01-01")) %>% 
    mutate(y = log(y))

# Разбиваем данные на обучающую выборку (все наблюдения, за исключением последних
# 90 дней) и проверочную выборку (последние 90 дней):
train_df <- clean_data %>% 
    arrange(ds) %>% 
    slice(1:(n() - 90))

test_df <- clean_data %>% 
    arrange(ds) %>% 
    tail(90)


# --------------------- Первая простая модель -------------------------

# Подгонка модели:
M0 <- prophet(train_df)

# Таблица со следующими 90 днями для расчета прогноза:
future_df <- make_future_dataframe(M0, periods = 90)

# Расчет прогноза:
forecast_M0 <- predict(M0, future_df)
head(forecast_M0[, c("yhat", "yhat_lower", "yhat_upper")])

# Графическое представление прогноза:
plot(M0, forecast_M0)

# Графическое представление оцененных компонентов модели:
prophet_plot_components(M0, forecast_M0)
