require(readr)
require(dplyr)
require(prophet)
require(ggplot2)
require(gridExtra)


# Загружаем исходные данные:
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

# Графическое представление оцененных точек излома тренда:
plot(M0, forecast_M0) + add_changepoints_to_plot(M0)



# -------------------------- Модель M1 ---------------------------------
# В этой модели мы изменяем количество потенциальных точек излома тренда
# с 25 принятых по умолчанию до 15:

# Подгонка модели:
M1 <- prophet(train_df, n.changepoints = 15)

# Расчет прогноза:
forecast_M1 <- predict(M1, future_df)

# Графическое представление оцененных точек излома тренда:
plot(M1, forecast_M1) + add_changepoints_to_plot(M1)


# -------------------------- Модель M2 ---------------------------------
# В этой модели мы увеличиваем интервал, в пределах которого
# оцениваются точки излома тренда, а также количество таких потенциальных
# точек (по сравнению с M1):

# Подгонка модели:
M2 <- prophet(train_df, n.changepoints = 20, changepoint.range = 0.9)

# Расчет прогноза:
forecast_M2 <- predict(M2, future_df)

# Графическое представление оцененных точек излома тренда:
plot(M2, forecast_M2) + add_changepoints_to_plot(M2)


# -------------------------- Модель M3 ---------------------------------
# В этой модели мы увеличиваем интервал, в пределах которого
# оцениваются точки излома тренда (до 90%), одновременно увеличив
# уровень регуляризации с помощью параметра changepoint.prior.scale.
# Начальное количество потенциальных точек излома оставим равным значению,
# принятому по умолчанию (25):

# Подгонка модели:
M3 <- prophet(train_df, 
              changepoint.range = 0.9, 
              changepoint.prior.scale = 0.02)

# Расчет прогноза:
forecast_M3 <- predict(M3, future_df)

# Графическое представление оцененных точек излома тренда:
plot(M3, forecast_M3) + add_changepoints_to_plot(M3)


# ------------------------ Модель М4 ------------------------------
# В этой модели мы задаем точки излома тренда самостоятельно
# (выбор дат, подаваемых на аргумент changepoints, основан на
# визуальном анализе исходных данных):

# Подгонка модели:
M4 <- prophet(train_df, 
              changepoints = c("2016-04-01",
                               "2016-06-15",
                               "2016-10-01",
                               "2017-04-01",
                               "2017-07-01",
                               "2017-09-01",
                               "2017-12-26",
                               "2018-04-01",
                               "2018-11-13",
                               "2018-12-15",
                               "2019-04-01"))

# Расчет прогноза:
forecast_M4 <- predict(M4, future_df)

# Графическое представление оцененных точек излома тренда:
plot(M4, forecast_M4) + add_changepoints_to_plot(M4)

# Компонент годовой сезонности в М4, оцененный по умолчанию:
prophet:::plot_yearly(M4)

# Уменьшение гладкости кривой годовой сезонности с помощью
# аргумента yearly.seasonality:
M4B <- prophet(train_df, 
               yearly.seasonality = 20,
               changepoints = c("2016-04-01",
                                "2016-06-15",
                                "2016-10-01",
                                "2017-04-01",
                                "2017-07-01",
                                "2017-09-01",
                                "2017-12-26",
                                "2018-04-01",
                                "2018-11-13",
                                "2018-12-15",
                                "2019-04-01"))

prophet:::plot_yearly(M4B)


# ------------------------ Эффекты праздников --------------------------------

# См. важные даты в истории биткоина здесь: 
# https://en.bitcoinwiki.org/wiki/Bitcoin_history

key_dates <- dplyr::tibble(
    holiday = c("Bitcoin_Cash_hard_fork",                     # Создание Bitcoin Cash
               "China_ICO_ban",                               # Запрет ICO в Китае
               "1South_Korea_announces_regulations",           # Введение регуляций в Ю. Корее
               "CoinMarketCap_removes_South_Korean_prices",   # Удаление южнокорейского рынка 
                                                              # из трекера цен CoinMarketCap
               "Bitcoin_Cash_SV_and_Bitcoing_Cash_ABC_fork"), # Разветвление Bitcoin Cash
    ds = as.Date(c(
        "2017-08-01",
        "2017-09-04",
        "2017-12-28",
        "2018-01-08",
        "2018-11-15"
    ))
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


# ----------------- Пользовательские сезонные компоненты -----------------------

M10 <- prophet(weekly.seasonality = FALSE) # отключаем недельную сезонность
M10 <- add_seasonality(m = M10,            # добавляем месячную сезонность
                       name = "monthly",
                       period = 30.5,
                       fourier.order = 5)
M10 <- fit.prophet(M10, train_df)
forecast_M10 <- predict(M10, future_df)
prophet_plot_components(M10, forecast_M10)


M11 <- prophet(weekly.seasonality = FALSE) # отключаем недельную сезонность
M11 <- add_seasonality(m = M11,            # добавляем квартальную сезонность
                       name = "quarter",
                       period = 365.25/4,
                       fourier.order = 2)
M11 <- fit.prophet(M11, train_df)
forecast_M11 <- predict(M11, future_df)
prophet_plot_components(M11, forecast_M11)


# -------------------- Условные режимы сезонности ------------------------------

# Функция для удобного добавления переключателей режимов в данные:
is_summer <- function(ds) {
    month <- as.numeric(format(ds, '%m'))
    return(month > 5 & month < 9)
}

# Добавляем переключатели в обучающие данные и в таблицу с будущими датами:
train_df$summer <- is_summer(train_df$ds)
train_df$not_summer <- !train_df$summer
future_df$summer <- is_summer(future_df$ds)
future_df$not_summer <- !future_df$summer

# Подгоняем модель:
M12 <- prophet(weekly.seasonality = FALSE) # отключаем автоматическую подгонку
                                           # недельной сезонности
M12 <- add_seasonality(M12, name = 'weekly_summer', 
                       period = 7,
                       fourier.order = 3,
                       condition.name = 'summer') # добавляем летний режим
M12 <- add_seasonality(M12, name = "weekly_not_summer",
                       period = 7, 
                       fourier.order = 3,
                       condition.name = "not_summer") # добавляем нелетний режим
M12 <- fit.prophet(M12, train_df)

forecast_M12 <- predict(M12, future_df)
prophet_plot_components(M12, forecast_M12)


# -------------- Регуляризация вклада сезонных составляющих --------------------

M13 <- prophet(weekly.seasonality = FALSE)
M13 <- add_seasonality(M13, name = 'weekly_summer', 
                       period = 7,
                       fourier.order = 3,
                       condition.name = 'summer',
                       prior.scale = 0.01)
M13 <- add_seasonality(M13, name = "weekly_not_summer",
                       period = 7, 
                       fourier.order = 3,
                       condition.name = "not_summer")
M13 <- fit.prophet(M13, train_df)

forecast_M13 <- predict(M13, future_df)
prophet_plot_components(M13, forecast_M13)


# ----------------- Аддитивные и мультипликативные модели ----------------------

M14 <- prophet(train_df, seasonality.mode = "multiplicative")
forecast_M14 <- predict(M14, future_df)
plot(M14, forecast_M14)

M15 <- prophet(yearly.seasonality = FALSE)
M15 <- add_seasonality(M15, name = 'yearly', 
                       period = 365.25,
                       fourier.order = 10, 
                       mode = "multiplicative")
M15 <- fit.prophet(M15, train_df)

forecast_M15 <- predict(M15, future_df)
prophet_plot_components(M15, forecast_M15)


# -------------------- Сохранение модельных объектов ---------------------------

save(list = paste0("M", 0:15), file = "models/models.RData")
