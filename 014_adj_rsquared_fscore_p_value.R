# Setup -------------------------------------------------------------------------------------------------------


library(tidyverse)



# Initial input data -------------------------------------------------------------------------------------------------------
dist_points_k <- 10
dist_points_d <- 0
dist_points_sigma <- 4

df_distpoints <-
    data.frame(dist_points = rep(1:10, 100) ) %>% 
    mutate(sales = dist_points * dist_points_k + dist_points_d + rnorm(n(), mean = 0, sd = dist_points_sigma))

df_distpoints %>% 
    ggplot(aes(x = dist_points, y = sales)) +
    geom_point(shape = 1, alpha = 0.3) +
    scale_x_continuous(breaks = seq(1,10)) +
    scale_y_continuous(breaks = seq(10,100,10))



shelf_space_k <- 1
shelf_space_d <- 0
shelf_space_sigma <- 4

df_shelfspace <-
    data.frame(shelf_space = rep(50:100, 100) ) %>% # 50% until 100% 
    mutate(sales = shelf_space * shelf_space_k + shelf_space_d + rnorm(n(), mean = 0, sd = shelf_space_sigma))

df_shelfspace %>% 
    ggplot(aes(x = shelf_space, y = sales)) +
    geom_point(shape = 1, alpha = 0.3) +
    scale_x_continuous(breaks = seq(1,100)) +
    scale_y_continuous(breaks = seq(10,100,10))


# R squared
# refer to 011_simple_linear_regression.R



# Example 1 -------------------------------------------------------------------------------------------------------
# R squared vs adjusted R squared 

df_combinded_norandom <- 
    expand.grid(dist_points = rep(1:10, 10),
                shelf_space = rep(50:100, 10)
    ) %>% 
    mutate(sales = dist_points * dist_points_k + dist_points_d +
               shelf_space * shelf_space_k + shelf_space_d
    )

df_combinded_norandom %>% nrow
# 51000



## 1 parameter
# Expecation: R squared and adjusted R squared should be equal
lm_1param_distpoints <- lm(sales ~ dist_points, data = df_combinded_norandom)
summary(lm_1param_distpoints)
# Coefficients:
#     Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 75.00000    0.14081   532.6   <2e-16 ***
#     dist_points 10.00000    0.02269   440.7   <2e-16 ***
#     ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 14.72 on 50998 degrees of freedom
# Multiple R-squared:  0.792,	Adjusted R-squared:  0.792 
# F-statistic: 1.942e+05 on 1 and 50998 DF,  p-value: < 2.2e-16


# Multiple R-squared:  0.792,	Adjusted R-squared:  0.792 -> equal !!!!!!!!!
# low p value !!!!



lm_1param_shelf_space <- lm(sales ~ shelf_space, data = df_combinded_norandom)
summary(lm_1param_shelf_space)
# Coefficients:
#     Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 55.000000   0.660424   83.28   <2e-16 ***
#     shelf_space  1.000000   0.008641  115.73   <2e-16 ***
#     ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 28.72 on 50998 degrees of freedom
# Multiple R-squared:  0.208,	Adjusted R-squared:  0.208 
# F-statistic: 1.339e+04 on 1 and 50998 DF,  p-value: < 2.2e-16


# Multiple R-squared:  0.208,	Adjusted R-squared:  0.208  -> equal !!!!!!!!!
# low p value  !!!!



## Perfect fit!
lm_2param_norandom <- lm(sales ~ dist_points + shelf_space, data = df_combinded_norandom)
summary(lm_2param_norandom)
# Coefficients:
#     Estimate Std. Error    t value Pr(>|t|)    
# (Intercept) -2.714e-11  5.967e-13 -4.549e+01   <2e-16 ***
#     dist_points  1.000e+01  3.754e-14  2.664e+14   <2e-16 ***
#     shelf_space  1.000e+00  7.325e-15  1.365e+14   <2e-16 ***
#     ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.435e-11 on 50997 degrees of freedom
# Multiple R-squared:      1,	Adjusted R-squared:      1 
# F-statistic: 4.481e+28 on 2 and 50997 DF,  p-value: < 2.2e-16





df_combinded_modif <- 
    expand.grid(dist_points = rep(1:10, 1),
                shelf_space = rep(50:100, 1)
    ) %>% 
    mutate(                rand_x1 = runif(n()),
                           rand_x2 = runif(n()),
                           rand_x3 = runif(n()),
                           rand_x4 = runif(n()),
                           rand_x5 = runif(n()),
                           rand_x6 = runif(n()),
                           rand_x7 = runif(n()),
                           rand_x8 = runif(n()),
                           rand_x9 = runif(n()),
                           rand_x10 = runif(n())
                           ) %>% 
    mutate(sales = dist_points * dist_points_k + dist_points_d + rnorm(n(), mean = 0, sd = dist_points_sigma) +
               shelf_space * shelf_space_k + shelf_space_d + rnorm(n(), mean = 0, sd = shelf_space_sigma)
    )

df_combinded_modif %>% nrow
# 510


lm_12param <- lm(sales ~ ., data = df_combinded_modif)
summary(lm_12param)
# Coefficients:
#     Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.666492   2.086430  -0.799    0.425    
# dist_points 10.020939   0.091482 109.540   <2e-16 ***
#     shelf_space  1.012533   0.017889  56.601   <2e-16 ***
#     rand_x1     -1.619556   0.913895  -1.772    0.077 .  
# rand_x2      0.081665   0.896940   0.091    0.927    
# rand_x3      0.008887   0.921198   0.010    0.992    
# rand_x4      0.104281   0.866666   0.120    0.904    
# rand_x5     -0.257795   0.917209  -0.281    0.779    
# rand_x6      1.400948   0.913762   1.533    0.126    
# rand_x7      0.820087   0.895443   0.916    0.360    
# rand_x8      0.564252   0.915456   0.616    0.538    
# rand_x9      0.003492   0.971189   0.004    0.997    
# rand_x10     0.297085   0.915243   0.325    0.746    
# ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 5.872 on 497 degrees of freedom
# Multiple R-squared:  0.969,	Adjusted R-squared:  0.9682 
# F-statistic:  1293 on 12 and 497 DF,  p-value: < 2.2e-16


# Multiple R-squared:  0.969,	Adjusted R-squared:  0.9682 !!!!!
# adjusted smaller than normal r squared!!!!!!!






df_combinded_modif %>% nrow
# 510




# Example 2 -------------------------------------------------------------------------------------------------------
# F score and p value



# only random parameters
# Expectation: low r squared, high p value

lm_rand_params <- lm(sales ~ rand_x1 + rand_x2 + rand_x3 + rand_x4 + rand_x5 +
                         rand_x6 + rand_x7 + rand_x8 + rand_x9 + rand_x10, data = df_combinded_modif)
summary(lm_rand_params)
# Multiple R-squared:  0.01804,	Adjusted R-squared:  -0.001635 
# F-statistic: 0.9169 on 10 and 499 DF,  p-value: 0.5171

# low r squared, high p value





# what hapens if one param is now contributing to the sales?
# Expecataion: higher r squared, significant p value 
lm_2contrib_rand_params <- lm(sales ~ dist_points + shelf_space + 
                         rand_x1 + rand_x2 + rand_x3 + rand_x4 + rand_x5 +
                         rand_x6 + rand_x7 + rand_x8 + rand_x9 + rand_x10, data = df_combinded_modif)
summary(lm_2contrib_rand_params)
# Multiple R-squared:  0.7689,	Adjusted R-squared:  0.7638 
# F-statistic: 150.7 on 11 and 498 DF,  p-value: < 2.2e-16
# higher r squared
# significant p value

lm_2contrib_rand_params <- lm(sales ~ dist_points + 
                                  rand_x1 + rand_x2 + rand_x3 + rand_x4 + rand_x5 +
                                  rand_x6 + rand_x7 + rand_x8 + rand_x9 + rand_x10, data = df_combinded_modif)
summary(lm_1contrib_rand_params)
# Multiple R-squared:  0.969,	Adjusted R-squared:  0.9682 
# F-statistic:  1293 on 12 and 497 DF,  p-value: < 2.2e-16

# higher r squared
# significant p value







