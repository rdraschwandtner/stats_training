

library(tidyverse)

df <- tribble(
    ~dist_points, ~sales,
    1, 10,
    2, 20,
    3, 30,
    4, 40,
    5, 50,
    6, 60,
    7, 70,
    8, 80,
    9, 90,
    10, 100
)

(plt1 <- df %>% 
        ggplot(aes(x = dist_points, y = sales)) +
        geom_point() +
        scale_x_continuous(breaks = seq(1,10)) +
        scale_y_continuous(breaks = seq(10,100,10))
)

mean <- mean(df$sales)

(plt2 <- plt1 +
        geom_hline(yintercept = mean_df) +
        geom_text(x = 1, y = 60, label = mean)
)

k <- 10
x <- 1:10
d <- 0
pred_sales <- k * x + d
pred_sales_df <- tibble(x = x, pred_sales = pred_sales)

(plt3 <- plt2 +
        geom_hline(yintercept = mean_df) +
        geom_text(x = 1, y = 60, label = mean_df) +
        geom_line(data = pred_sales_df, aes(x = x, y = pred_sales), col = 'blue')
)

var_mean <- var(mean - df$sales)
var_line <- var(pred_sales_df$pred_sales - df$sales)

(r_squared <- (var_mean - var_line) / var_mean)
# 1

(plt4 <- plt3 +
        geom_hline(yintercept = mean_df) +
        geom_text(x = 1, y = 60, label = mean_df) +
        geom_line(data = pred_sales_df, aes(x = x, y = pred_sales), col = 'blue') + 
        geom_text(x = 9, y = 70, label = paste0('R^2 = ', r_squared), col = 'blue')
)




# --------------------------------------------------------



df2 <- tribble(
    ~dist_points, ~sales,
    1, 0,
    2, 30,
    3, 20,
    4, 50,
    5, 40,
    6, 70,
    7, 60,
    8, 90,
    9, 80,
    10, 100
)

mean_new <- mean(df2$sales)

(plt_new1 <- df2 %>% 
        ggplot(aes(x = dist_points, y = sales)) +
        geom_point() +
        geom_hline(yintercept = mean_new) + 
        geom_text(x = 1, y = 60, label = mean_new) +
        scale_x_continuous(breaks = seq(1,10)) +
        scale_y_continuous(breaks = seq(10,100,10))
)

res_lm <- lm('sales ~ dist_points', df2)
summary(res_lm)$r.squared
summary(res_lm)
new <- data.frame(dist_points = seq(1, 10, 1))
pred_lm <- tibble(y = predict(res_lm, new)) %>% mutate(dist_points = 1:n())

(plt_new2 <- plt_new1 +
        geom_line(data = pred_lm, aes(x =dist_points, y = y ), col = 'red') +
        geom_text(x= 8,y = 40, label = paste0('R^2: ', summary(res_lm)$r.squared ), col = 'red')
)


