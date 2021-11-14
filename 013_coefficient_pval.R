# Setup -------------------------------------------------------------------------------------------------------


library(tidyverse)
library(plotly)



# input data -------------------------------------------------------------------------------------------------------

dist_points_k <- 10
dist_points_d <- 0
dist_points_sigma <- 4

shelf_space_k <- 1
shelf_space_d <- 0
shelf_space_sigma <- 4


# std error is dependent on
# - n data points
# - shared sigma similarily distributed
# - distribution on x axis closer or wider

## 1 Example: sigma -------------------------------------

# expected sigma
# p values are significant
df_normal <- 
    expand.grid(dist_points = rep(1:10, 1),
                shelf_space = rep(50:100, 1)
    ) %>% 
    mutate(sales = dist_points * dist_points_k + dist_points_d + rnorm(n(), mean = 0, sd = 4) +
               shelf_space * shelf_space_k + shelf_space_d + rnorm(n(), mean = 0, sd = 100)
    )

(df_normal %>% 
        ggplot(aes(x = shelf_space, y = sales)) +
        geom_point(shape = 1, alpha = 0.3) +
        scale_x_continuous(breaks = 50:100) +
        scale_y_continuous(breaks = seq(-700,700,10)) +
        geom_smooth(method = lm)
)


(df_normal %>% 
        ggplot(aes(x = shelf_space, y = sales)) +
        geom_point(shape = 1, alpha = 0.3) +
        geom_smooth(method = lm) + 
        scale_x_continuous(breaks = 50:100) +
        scale_y_continuous(breaks = seq(-700,700,10))+
        geom_smooth(method = lm)
)


df_normal_randomised <- lm(sales ~ dist_points + shelf_space, data = df_normal)
summary(df_normal_randomised)
# Coefficients:
#     Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -30.8404    24.6726  -1.250    0.212    
# dist_points  11.8000     1.5521   7.602 1.42e-13 ***
#     shelf_space   1.3189     0.3029   4.355 1.61e-05 ***

df_normal %>% nrow
# 510





# Instead of normal distribution, use a uniform distribution but with the same mean!
# p values are not significant


df_unifdist <- 
    expand.grid(dist_points = rep(1:10, 1),
                shelf_space = rep(50:100, 1)
    ) %>% 
    mutate(sales = dist_points * dist_points_k + dist_points_d + runif(n(), min = 0, max = 500) +
               shelf_space * shelf_space_k + shelf_space_d + runif(n(), min = 0, max = 500)
    )

(df_unifdist %>% 
        ggplot(aes(x = shelf_space, y = sales)) +
        geom_point(shape = 1, alpha = 0.3) +
        scale_x_continuous(breaks = 50:100) +
        scale_y_continuous(breaks = seq(-700,1500,10)) +
        geom_smooth(method = lm)
)


(df_unifdist %>% 
        ggplot(aes(x = shelf_space, y = sales)) +
        geom_point(shape = 1, alpha = 0.3) +
        geom_smooth(method = lm) + 
        scale_x_continuous(breaks = 50:100) +
        scale_y_continuous(breaks = seq(-700,1500,10))+
        geom_smooth(method = lm)
)


lm_unifdist <- lm(sales ~ dist_points + shelf_space, data = df_unifdist)
summary(lm_unifdist)
# Coefficients:
#     Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 513.6269    51.0026  10.071  < 2e-16 ***
#     dist_points   8.8988     3.2085   2.774  0.00575 ** 
#     shelf_space   0.7164     0.6261   1.144  0.25305    

# both parameters not significant!!!!!!!!!
# just because we went from a normal distribution to a uniform distribution!!!!





## 2 Example: X axis more wide -------------------------------------
# We set the x axis wider
# this should produce higher p values than the initial model!


df_wider_x <- 
    expand.grid(dist_points = rep(c(1,10), 5),
                shelf_space = rep(c(50,100), 5)
    ) %>% 
    mutate(sales = dist_points * dist_points_k + dist_points_d + rnorm(n(), mean = 0, sd = 4) +
               shelf_space * shelf_space_k + shelf_space_d + rnorm(n(), mean = 0, sd = 100)
    )

(df_wider_x %>% 
        ggplot(aes(x = shelf_space, y = sales)) +
        geom_point(shape = 1, alpha = 0.3) +
        scale_x_continuous(breaks = 50:100) +
        scale_y_continuous(breaks = seq(-700,700,10)) +
        geom_smooth(method = lm)
)
# its hard to see the effect of shelf space

(df_wider_x %>% 
        ggplot(aes(x = shelf_space, y = sales)) +
        geom_point(shape = 1, alpha = 0.3) +
        geom_smooth(method = lm) + 
        scale_x_continuous(breaks = 50:100) +
        scale_y_continuous(breaks = seq(-700,700,10))+
        geom_smooth(method = lm)
)

lm_wider_x <- lm(sales ~ dist_points + shelf_space, data = df_wider_x)
# df_wider_x %>% nrow
# # 400

summary(df_normal_randomised)
# Coefficients:
#     Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -30.8404    24.6726  -1.250    0.212    
# dist_points  11.8000     1.5521   7.602 1.42e-13 ***
#     shelf_space   1.3189     0.3029   4.355 1.61e-05 ***


summary(lm_wider_x)
# Coefficients:
#     Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  32.7824    32.9597   0.995    0.322    
# dist_points  14.1725     2.1604   6.560 2.63e-09 ***
#     shelf_space   0.2059     0.3889   0.529    0.598   


# p value lower for both coefficients


## 3 Example: less data -------------------------------------


df_wider_x_less_data <- 
    expand.grid(dist_points = rep(c(1,10), 3),
                shelf_space = rep(c(50,100), 3)
    ) %>% 
    mutate(sales = dist_points * dist_points_k + dist_points_d + rnorm(n(), mean = 0, sd = 4) +
               shelf_space * shelf_space_k + shelf_space_d + rnorm(n(), mean = 0, sd = 100)
    )

(df_wider_x_less_data %>% 
        ggplot(aes(x = shelf_space, y = sales)) +
        geom_point(shape = 1, alpha = 0.3) +
        scale_x_continuous(breaks = 50:100) +
        scale_y_continuous(breaks = seq(-700,700,10)) +
        geom_smooth(method = lm)
)
# its hard to see the effect of shelf space

(df_wider_x_less_data %>% 
        ggplot(aes(x = shelf_space, y = sales)) +
        geom_point(shape = 1, alpha = 0.3) +
        geom_smooth(method = lm) + 
        scale_x_continuous(breaks = 50:100) +
        scale_y_continuous(breaks = seq(-700,700,10))+
        geom_smooth(method = lm)
)

lm_wider_x_less_data <- lm(sales ~ dist_points + shelf_space, data = df_wider_x_less_data)
summary(lm_wider_x_less_data)
# Coefficients:
#     Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -16.5979    54.6248  -0.304 0.763148    
# dist_points  14.1408     3.5805   3.949 0.000388 ***
#     shelf_space   0.7148     0.6445   1.109 0.275437    


# p value even lower!!!!!!!!!!!!!!


