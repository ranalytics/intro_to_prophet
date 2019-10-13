require(readr)
require(dplyr)
require(prophet)
require(ggplot2)


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

