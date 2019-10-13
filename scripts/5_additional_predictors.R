require(dplyr)
require(prophet)
require(ggplot2)
require(gridExtra)
require(tidyquant)
require(imputeTS)


# ------------------- Добавление предикторов -----------------------------------

# Собираем данные по цене акций Amazon, Google и Facebook на момент
# закрытия торгов:
AMZN <- tq_get(x = "AMZN", get = "stock.prices", 
               from = "2016-01-01", to = "2019-05-26") %>% 
    rename(ds = date, amzn = close) %>% 
    select(ds, amzn) %>% 
    mutate(amzn = log(amzn))

GOOG <- tq_get(x = "GOOG", get = "stock.prices", 
               from = "2016-01-01", to = "2019-05-26") %>% 
    rename(ds = date, goog = close) %>% 
    select(ds, goog) %>% 
    mutate(goog = log(goog))

FB <- tq_get(x = "FB", get = "stock.prices", 
             from = "2016-01-01", to = "2019-05-26") %>% 
    rename(ds = date, fb = close) %>% 
    select(ds, fb) %>% 
    mutate(fb = log(fb))

# Добавляем данные по цене акций в таблицу с обучающими данными: 
train_df <- left_join(train_df, AMZN) %>% 
    left_join(., GOOG) %>% 
    left_join(FB)

# Восстанавливаем пропущенные значения цены акций по методу locf 
# ("last observation carried over"), реализованному в функции na_locf 
# из пакета imputeTS:
train_df <- train_df %>% 
    mutate(amzn = na_locf(amzn),
           goog = na_locf(goog),
           fb = na_locf(fb))

# Корреляция между стоимостью биткоина и предикторами:
cor_amzn <- train_df %>% 
    ggplot(., aes(amzn, y)) +
    geom_point(alpha = 0.4) +
    ggtitle("y vs. amzn")
cor_goog <- train_df %>% 
    ggplot(., aes(goog, y)) +
    geom_point(alpha = 0.4) +
    ggtitle("y vs. goog")
cor_fb <- train_df %>% 
    ggplot(., aes(fb, y)) +
    geom_point(alpha = 0.4) +
    ggtitle("y vs. fb")
grid.arrange(cor_amzn, cor_goog, cor_fb, ncol = 3)

# Строим отдельные модели для каждого предиктора (параметры моделей
# подобраны после предварительного экспериментирования):
m_amzn <- prophet(rename(AMZN, y = amzn),
                  n.changepoints = 15, 
                  changepoint.range = 0.9,
                  weekly.seasonality = FALSE)
m_goog <- prophet(rename(GOOG, y = goog),
                  n.changepoints = 15, 
                  changepoint.range = 0.9,
                  weekly.seasonality = FALSE)
m_fb <- prophet(rename(FB, y = fb),
                n.changepoints = 15, 
                changepoint.range = 0.9,
                weekly.seasonality = FALSE)

# Рассчитываем прогнозные значения для цен на акции:
future_df_shares <- future_df %>% select(ds)
amzn_forecast <- predict(m_amzn, future_df_shares)
goog_forecast <- predict(m_goog, future_df_shares)
fb_forecast <- predict(m_fb, future_df_shares)

future_df_shares <- future_df_shares %>% 
    mutate(amzn = amzn_forecast$yhat,
           goog = goog_forecast$yhat,
           fb = fb_forecast$yhat)

# Визуализация полученных моделей для цен на акции:
amzn_plot <- plot(m_amzn, amzn_forecast) + ggtitle("Amazon")
goog_plot <- plot(m_goog, goog_forecast) + ggtitle("Google")
fb_plot <- plot(m_fb, fb_forecast) + ggtitle("Facebook")
grid.arrange(amzn_plot, goog_plot, fb_plot, ncol = 3)

# Модель стоимости биткоина, включающая три предиктора:
M16 <- prophet(n.changepoints = 15, changepoint.range = 0.9)
M16 <- add_regressor(M16, 'amzn')
M16 <- add_regressor(M16, 'goog')
M16 <- add_regressor(M16, 'fb')
M16 <- fit.prophet(M16, train_df)

forecast_M16 <- predict(M16, future_df_shares)

# Визуализация полученной модели и прогнозных значений:
plot(M16, forecast_M16)

# Визуализация компонентов модели:
prophet_plot_components(M16, forecast_M16)

amzn_comp <- plot_forecast_component(M16, forecast_M16, name = "amzn") +
    ggtitle("Amazon")
goog_comp <- plot_forecast_component(M16, forecast_M16, name = "goog") +
    ggtitle("Google")
fb_comp <- plot_forecast_component(M16, forecast_M16, name = "fb") +
    ggtitle("Facebook")
grid.arrange(amzn_comp, goog_comp, fb_comp, ncol = 3)

