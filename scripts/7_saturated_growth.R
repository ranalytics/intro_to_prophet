library(prophet)
library(ggplot2)
library(dplyr)

# --------------------- Логистическая функция ----------------------------------

logistic_growth <- function(x) {
    C / (1 + exp(-k*(x - m)))
}

par(mfrow = c(1, 3))

# Пример 1:
C <- 10
k <- 0.1
m <- 0
curve(logistic_growth(x), from = -100, to = 100)
abline(v = 0, col = "red", lty = 2)
abline(h = 10, col = "blue", lty = 2)
title("C=10, k=0.1, m=0")

# Пример 2:
C <- 10
k <- 0.1
m <- 20
curve(logistic_growth(x), from = -100, to = 100)
abline(v = 20, col = "red", lty = 2)
abline(h = 10, col = "blue", lty = 2)
title("C=10, k=0.1, m=10")

# Пример 3:
C <- 8
k <- 0.05
m <- 0
curve(logistic_growth(x), from = -100, to = 100,
      ylim = c(0, 10))
abline(v = 0, col = "red", lty = 2)
abline(h = 8, col = "blue", lty = 2)
title("C=8, k=0.05, m=0")


# ---------------- Пример модели с ограниченным ростом -------------------------

# Загружаем созданные ранее объекты:
load("./workspaces/intro_to_prophet.RData")

# Ограничим обучающие данные периодом до середины декабря 2017 г.:
train_df_short <- train_df %>% filter(ds <= as.Date("2017-12-15")) 

# График роста стоимости биткоина:
ggplot(train_df_short, aes(ds, y)) + geom_point()

# Добавляем верхний порог стоимости биткоина в обучающие данные:
train_df_short$cap <- 11.5

# Подгонка модели:
M17 <- prophet(train_df_short, growth = "logistic",
               changepoint.range = 0.95,
               changepoint.prior.scale = 0.15)

# Таблица с датами прогнозного периода в 180 дней:
future_df_short <- make_future_dataframe(M17, periods = 180) %>% 
    mutate(cap = 11.5) # верхний порог должен быть добавлен и в эту таблицу

# Расчет прогноза:
forecast_M17 <- predict(M17, future_df_short)

# Визуализация подогнанной модели и полученного прогноза:
plot(M17, forecast_M17)


# ----------------- Пример модель с ограничением по нижнему порогу -------------

# Обучающие данные за 2018 г.:
train_df_both <- train_df %>% 
    filter(ds >= as.Date("2018-01-01") & ds <= as.Date("2018-12-31"))

# Визуализация данных за 2018 г.:
ggplot(train_df_floor, aes(ds, y)) + geom_point()

# Добавляем верхний и нижний пороги стоимости биткоина в обучающие данные:
train_df_both$floor <- 7
train_df_both$cap <- 10.0

# Подгонка модели:
M18 <- prophet(train_df_both, growth = "logistic",
               changepoint.range = 0.85,
               changepoint.prior.scale = 0.15)

# Таблица с датами прогнозного периода в 180 дней:
future_df_both <- make_future_dataframe(M18, periods = 180) %>% 
    mutate(floor = 7, cap = 10)

# Расчет прогноза:
forecast_M18 <- predict(M18, future_df_both)

# Визуализация подогнанной модели и полученного прогноза:
plot(M18, forecast_M18)
