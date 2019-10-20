require(dplyr)
require(prophet)
require(ggplot2)
require(gridExtra)


# ---------------- Перекрестная проверка по методу SHF -------------------------

# Загружаем ранее подогнанные и сохраненные модели:
load("./workspaces/intro_to_prophet.RData")

# Пример использования функции cross_validation():
M3_cv <- cross_validation(M3, 
                          initial = 730, # длина первого блока (2 года)
                          period = 90,   # расстояние между точками отсчета
                          horizon = 90,  # длина прогнозного горизонта (90 дней)
                          units = "days")

head(M3_cv)

# Пример расчета метрики качества прогноза:
performance_metrics(M3_cv,
                    metrics = "mse",
                    rolling_window = 0.1) %>% head()

# Пример расчета метрики качества модели для каждой даты прогнозного горизота:
performance_metrics(M3_cv,
                    metrics = "mse",
                    rolling_window = 0) %>% head()

# Пример расчета усредненной метрики качества модели для всего прогнозного горизота:
performance_metrics(M3_cv,
                    metrics = "mse",
                    rolling_window = 1) %>% head()

# Визуализация метрики качества предсказаний, полученной по результатам 
# перекрестной проверки:
plot_cross_validation_metric(M3_cv,
                             metric = "mse",
                             rolling_window = 0.1)

# Пример выбора оптимальной модели:
M4_cv <- cross_validation(M4, 
                          initial = 730,
                          period = 180,
                          horizon = 90,
                          units = "days")
M5_cv <- cross_validation(M5, 
                          initial = 730,
                          period = 180,
                          horizon = 90,
                          units = "days")
M12_cv <- cross_validation(M12, 
                           initial = 730,
                           period = 180,
                           horizon = 90,
                           units = "days")
M4_perf <- performance_metrics(M4_cv,
                               metrics = c("mape", "coverage"),
                               rolling_window = 1)
M5_perf <- performance_metrics(M5_cv,
                               metrics = c("mape", "coverage"),
                               rolling_window = 1)
M12_perf <- performance_metrics(M12_cv,
                                metrics = c("mape", "coverage"),
                                rolling_window = 1)

# Графическое сравнение результатов выполнения перекрестной проверки:
M4_cv_plot <- plot_cross_validation_metric(M4_cv,
                                           metric = "mape",
                                           rolling_window = 0.1) +
    ylim(c(0, 0.15)) + ggtitle("M4")
M5_cv_plot <- plot_cross_validation_metric(M5_cv,
                                           metric = "mape",
                                           rolling_window = 0.1) +
    ylim(c(0, 0.15)) + ggtitle("M5")
M12_cv_plot <- plot_cross_validation_metric(M12_cv,
                                            metric = "mape",
                                            rolling_window = 0.1) +
    ylim(c(0, 0.15)) + ggtitle("M12")

grid.arrange(M4_cv_plot, M5_cv_plot, M12_cv_plot, ncol = 3)


# -- Сравнение предсказаний модели M12 и истинных значений стоимости биткоина --

plot(M12, forecast_M12) + 
    coord_cartesian(xlim = c(as.POSIXct("2019-01-01"),
                             as.POSIXct("2019-08-24"))) +
    geom_point(data = test_df, aes(as.POSIXct(ds), y), col = "red")

