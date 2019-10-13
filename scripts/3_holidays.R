require(readr)
require(dplyr)
require(prophet)
require(ggplot2)
require(gridExtra)


# ------------------------ Эффекты "праздников" --------------------------------

# См. важные даты в истории биткоина здесь: 
# https://en.bitcoinwiki.org/wiki/Bitcoin_history

key_dates <- dplyr::tibble(
    
    holiday = c("Bitcoin_Cash_hard_fork",                      # Создание Bitcoin Cash
                "China_ICO_ban",                               # Запрет ICO в Китае
                "1South_Korea_announces_regulations",          # Введение регуляций в Ю. Корее
                "CoinMarketCap_removes_South_Korean_prices",   # Удаление южнокорейского рынка 
                # из трекера цен CoinMarketCap
                "Bitcoin_Cash_SV_and_Bitcoing_Cash_ABC_fork"), # Разветвление Bitcoin Cash
    ds = as.Date(c("2017-08-01",
                   "2017-09-04",
                   "2017-12-28",
                   "2018-01-08",
                   "2018-11-15"))
)

M5 <- prophet(train_df,
              holidays = key_dates,
              changepoint.range = 0.9)
forecast_M5 <- predict(M5, future_df)

# Графическое изображение компонентов модели:
prophet_plot_components(M5, forecast_M5)

# Графическое изображение эффекта события
# "Bitcoin Cash SV and Bitcoing Cash ABC fork":
plot_forecast_component(M5, forecast_M5,
                        name = "Bitcoin_Cash_SV_and_Bitcoing_Cash_ABC_fork")

# Эффекты всех событий на отдельном графике:
plot_forecast_component(M5, forecast_M5, name = "holidays")


# -------------------- Моделирование "предыстории" события ---------------------

key_dates2 <- dplyr::bind_cols(key_dates, 
                               lower_window = c(0, 0, 0, 0, -14),
                               upper_window = c(0, 0, 0, 0, 0))

M6 <- prophet(train_df,
              holidays = key_dates2,
              changepoint.range = 0.9)
forecast_M6 <- predict(M6, future_df)

# Эффект события с "предысторией":
plot_forecast_component(M6, forecast_M6,
                        name = "Bitcoin_Cash_SV_and_Bitcoing_Cash_ABC_fork")


# --------------------- Моделирование официальных праздников -------------------

# Обратите внимание: здесь мы инициализируем объект M7,
# но пока подаем на него таблицу с обучающими данными
M7 <- prophet(holidays = key_dates2, changepoint.range = 0.9)

# Добавляем официальные праздничные дни США:
M7 <- add_country_holidays(M7, country_name = 'US')

# Подгонка модели (обратите внимание на использование функции fit.prophet()):
M7 <- fit.prophet(M7, train_df)
forecast_M7 <- predict(M7, future_df)

# Эффекты всех событий на отдельном графике:
plot_forecast_component(M7, forecast_M7, name = "holidays")

# Включенные в модель события:
M7$train.holiday.names


# ---------------- Глобальная регуляризация эффектов праздников ----------------

# Для глобальной регуляризации эффектов праздников служит аргумент
# holidays.prior.scale:
M8 <- prophet(holidays = key_dates2,
              changepoint.range = 0.9,
              holidays.prior.scale = 0.01)

M8 <- add_country_holidays(M8, country_name = 'US')
M8 <- fit.prophet(M8, train_df)
forecast_M8 <- predict(M8, future_df)

# Эффекты праздников до (модель M7) и после (M8) глобальной регуляризации:
m7_holidays <- plot_forecast_component(M7, forecast_M7, name = "holidays") +
    labs(title = "Модель M7") +
    ylim(c(-0.15, 0.25))
m8_holidays <- plot_forecast_component(M8, forecast_M8, name = "holidays") +
    labs(title = "Модель M8") +
    ylim(c(-0.15, 0.25))
gridExtra::grid.arrange(m7_holidays, m8_holidays, nrow = 1)


# --------------- Регуляризация эффектов отдельных событий ---------------------

key_dates3 <- dplyr::bind_cols(key_dates2, 
                               prior_scale = c(10, 10, 10, 10, 0.01))

M9 <- prophet(holidays = key_dates3,
              changepoint.range = 0.9)

M9 <- add_country_holidays(M9, country_name = 'US')
M9 <- fit.prophet(M9, train_df)
forecast_M9 <- predict(M9, future_df)

m9_holidays <- plot_forecast_component(M9, forecast_M9, name = "holidays") +
    labs(title = "Модель M9") +
    ylim(c(-0.15, 0.25))
gridExtra::grid.arrange(m7_holidays, m9_holidays, nrow = 1)

