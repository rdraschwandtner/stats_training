---
title: "012_classical_multiple_linear_regression"
output: 
  html_document: 
    keep_md: true
  github_document:
    toc: true
---


# Setup -------------------------------------------------------------------------------------------------------


```r
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
```

![](012_classical_multiple_linear_regression_files/figure-html/unnamed-chunk-1-1.png)<!-- -->


```r
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
```

![](012_classical_multiple_linear_regression_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

# Example 1 -------------------------------------------------------------------------------------------------------
2 variables contributing to sales - no randomness

```r
df_combinded_norandom <- 
    expand.grid(dist_points = rep(1:10, 10),
                shelf_space = rep(50:100, 10)
    ) %>% 
    mutate(sales = dist_points * dist_points_k + dist_points_d +
               shelf_space * shelf_space_k + shelf_space_d
    )

# df_combinded_norandom %>% nrow
# 51000



# 3d plot
# plot_ly(data = df_combinded_norandom, x=~dist_points, y=~shelf_space, z=~sales, type="scatter3d", mode="markers", alpha = 0.8) %>% 
#     layout(scene = list(camera=list(eye = list(x = -1.25, y = -1.5, z = 1.25))))
```

The result is now a plane instead of a a line!!!!!!!!!!!



```r
# actual residual standard error
sd(df_combinded_norandom %>% mutate(error = dist_points * dist_points_k + shelf_space * shelf_space_k - sales) %>% pull(error))
```

```
## [1] 0
```

```r
# 0
```


```r
lm_res_no_random <- lm(sales ~ dist_points + shelf_space, data = df_combinded_norandom)
summary(lm_res_no_random)
```

```
## 
## Call:
## lm(formula = sales ~ dist_points + shelf_space, data = df_combinded_norandom)
## 
## Residuals:
##        Min         1Q     Median         3Q        Max 
## -5.445e-09 -1.000e-13  1.000e-13  3.000e-13  2.063e-10 
## 
## Coefficients:
##               Estimate Std. Error    t value Pr(>|t|)    
## (Intercept) -2.714e-11  5.967e-13 -4.549e+01   <2e-16 ***
## dist_points  1.000e+01  3.754e-14  2.664e+14   <2e-16 ***
## shelf_space  1.000e+00  7.325e-15  1.365e+14   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.435e-11 on 50997 degrees of freedom
## Multiple R-squared:      1,	Adjusted R-squared:      1 
## F-statistic: 4.481e+28 on 2 and 50997 DF,  p-value: < 2.2e-16
```

```r
# Coefficients:
#     Estimate Std. Error    t value Pr(>|t|)    
# (Intercept) -2.714e-11  5.967e-13 -4.549e+01   <2e-16 ***
#     dist_points  1.000e+01  3.754e-14  2.664e+14   <2e-16 ***
#     shelf_space  1.000e+00  7.325e-15  1.365e+14   <2e-16 ***
#     ---
#     Signif. codes:  0 ???***??? 0.001 ???**??? 0.01 ???*??? 0.05 ???.??? 0.1 ??? ??? 1
# 
# Residual standard error: 2.435e-11 on 50997 degrees of freedom
# Multiple R-squared:      1,	Adjusted R-squared:      1 
# F-statistic: 4.481e+28 on 2 and 50997 DF,  p-value: < 2.2e-16

# parameter coefficients - fit very well
# no residual standard error - OK
```


## What happens if the output is explained only by one variable?

### Only dist points


```r
lm_res_no_random_onlydistpoints <- lm(sales ~ dist_points, data = df_combinded_norandom)
summary(lm_res_no_random_onlydistpoints)
```

```
## 
## Call:
## lm(formula = sales ~ dist_points, data = df_combinded_norandom)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
##    -25    -13      0     13     25 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 75.00000    0.14081   532.6   <2e-16 ***
## dist_points 10.00000    0.02269   440.7   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 14.72 on 50998 degrees of freedom
## Multiple R-squared:  0.792,	Adjusted R-squared:  0.792 
## F-statistic: 1.942e+05 on 1 and 50998 DF,  p-value: < 2.2e-16
```

```r
# Coefficients:
#     Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 75.00000    0.14081   532.6   <2e-16 ***
#     dist_points 10.00000    0.02269   440.7   <2e-16 ***
#     ---
#     Signif. codes:  0 ???***??? 0.001 ???**??? 0.01 ???*??? 0.05 ???.??? 0.1 ??? ??? 1
# 
# Residual standard error: 14.72 on 50998 degrees of freedom
# Multiple R-squared:  0.792,	Adjusted R-squared:  0.792 
# F-statistic: 1.942e+05 on 1 and 50998 DF,  p-value: < 2.2e-16

# dist_points coefficient is found very well!!!!!
# intercept is really high!!!!!!!
# R squared is already pretty high, but not as before!!!!!
```


```r
pred_lm_res_no_random_onlydistpoints <- tibble(y = predict(lm_res_no_random_onlydistpoints, tibble(dist_points = df_combinded_norandom$dist_points)) , dist_points = df_combinded_norandom$dist_points)
(df_combinded_norandom %>% 
        ggplot(aes(x = dist_points, y = sales)) +
        geom_point(shape = 1, alpha = 0.3) +
        scale_x_continuous(breaks = seq(1,10)) +
        scale_y_continuous(breaks = seq(10,500,10)) +
        geom_line(data = pred_lm_res_no_random_onlydistpoints, aes(x = dist_points, y = y), col = 'red')
)
```

![](012_classical_multiple_linear_regression_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

looks as if only dist points would cause the sales!!!!!!!!!!

its a very good explanation already!!!!! shelf space wouldnt be needed !?


### Only shelf space

```r
lm_res_no_random_onlyshelfspace <- lm(sales ~ shelf_space, data = df_combinded_norandom)
summary(lm_res_no_random_onlyshelfspace)
```

```
## 
## Call:
## lm(formula = sales ~ shelf_space, data = df_combinded_norandom)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
##    -45    -25      0     25     45 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 55.000000   0.660424   83.28   <2e-16 ***
## shelf_space  1.000000   0.008641  115.73   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 28.72 on 50998 degrees of freedom
## Multiple R-squared:  0.208,	Adjusted R-squared:  0.208 
## F-statistic: 1.339e+04 on 1 and 50998 DF,  p-value: < 2.2e-16
```

```r
# Coefficients:
#     Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 55.000000   0.660424   83.28   <2e-16 ***
#     shelf_space  1.000000   0.008641  115.73   <2e-16 ***
#     ---
#     Signif. codes:  0 ???***??? 0.001 ???**??? 0.01 ???*??? 0.05 ???.??? 0.1 ??? ??? 1
# 
# Residual standard error: 28.72 on 50998 degrees of freedom
# Multiple R-squared:  0.208,	Adjusted R-squared:  0.208 
# F-statistic: 1.339e+04 on 1 and 50998 DF,  p-value: < 2.2e-16
```

shelf_space coefficient is found very well!!!!!
intercept is really high!!!!!!!
R squared is verrrrry low!!!!!!!!!!
residual standard error is high



```r
pred_lm_res_no_random_onlyshelfspace <- tibble(y = predict(lm_res_no_random_onlyshelfspace, tibble(shelf_space = df_combinded_norandom$shelf_space)) , shelf_space = df_combinded_norandom$shelf_space)
(df_combinded_norandom %>% 
        ggplot(aes(x = shelf_space, y = sales)) +
        geom_point(shape = 1, alpha = 0.3) +
        scale_x_continuous(breaks = 50:100) +
        scale_y_continuous(breaks = seq(10,500,10)) +
        geom_line(data = pred_lm_res_no_random_onlyshelfspace, aes(x = shelf_space, y = y), col = 'red')
)
```

![](012_classical_multiple_linear_regression_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

describes the relation very well, although the R squared is super low!!!!!!!
R squared isnt super important to detect the effect of a parameter!!!!!!






















# Example 2 -------------------------------------------------------------------------------------------------------
2 variablesbles contributing to sales with a bit of randomness


```r
df_combinded <- 
    expand.grid(dist_points = rep(1:10, 10),
               shelf_space = rep(50:100, 10)
               ) %>% 
    mutate(sales = dist_points * dist_points_k + dist_points_d + rnorm(n(), mean = 0, sd = dist_points_sigma) +
               shelf_space * shelf_space_k + shelf_space_d + rnorm(n(), mean = 0, sd = shelf_space_sigma)
               )

# df_combinded %>% nrow
# 51000



# 3d plot
# plot_ly(data = df_combinded, x=~dist_points, y=~shelf_space, z=~sales, type="scatter3d", mode="markers", alpha = 0.8) %>% 
#     layout(scene = list(camera=list(eye = list(x = -1.25, y = -1.5, z = 1.25))))
```

actual residual standard error

```r
sd(df_combinded %>% mutate(error = dist_points * dist_points_k + shelf_space * shelf_space_k - sales) %>% pull(error))
```

```
## [1] 5.646853
```

```r
# 5.68907
```


```r
lm_res <- lm(sales ~ dist_points + shelf_space, data = df_combinded)
summary(lm_res)
```

```
## 
## Call:
## lm(formula = sales ~ dist_points + shelf_space, data = df_combinded)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -25.1950  -3.7846  -0.0154   3.8028  24.0436 
## 
## Coefficients:
##             Estimate Std. Error  t value Pr(>|t|)    
## (Intercept) 0.083276   0.138385    0.602    0.547    
## dist_points 9.998077   0.008706 1148.462   <2e-16 ***
## shelf_space 0.998636   0.001699  587.863   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.647 on 50997 degrees of freedom
## Multiple R-squared:  0.9703,	Adjusted R-squared:  0.9703 
## F-statistic: 8.323e+05 on 2 and 50997 DF,  p-value: < 2.2e-16
```

```r
# Coefficients:
#     Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.154233   0.138979    1.11    0.267    
# dist_points 10.002490   0.008743 1144.05   <2e-16 ***
#     shelf_space  0.997832   0.001706  584.88   <2e-16 ***
#     ---
#     Signif. codes:  0 ???***??? 0.001 ???**??? 0.01 ???*??? 0.05 ???.??? 0.1 ??? ??? 1
# 
# Residual standard error: 5.671 on 50997 degrees of freedom
# Multiple R-squared:   0.97,	Adjusted R-squared:   0.97 
# F-statistic: 8.255e+05 on 2 and 50997 DF,  p-value: < 2.2e-16
```

coefficients fit very well
standard errors are low
residual standard error fits

you can see the 'hold fix' effect!! at every point, the coefficients of the parameters are constant. No matter what the other parameter is! 






# Example 3 -------------------------------------------------------------------------------------------------------
from Example 1
2 variables contributing to sales - no randomness
What happens if one parameter has a range where it works very well, but not on the other range?
Would this have a effect on the p value?


```r
df_combinded_part_extreme_randomised <- 
    expand.grid(dist_points = rep(1:10, 1),
                shelf_space = rep(50:100, 1)
    ) %>% 
    mutate(sales = dist_points * dist_points_k + dist_points_d + rnorm(n(), mean = 0, sd = dist_points_sigma) +
               shelf_space * shelf_space_k + shelf_space_d + rnorm(n(), mean = 0, sd = 100)
    )

(df_combinded_part_extreme_randomised %>% 
        ggplot(aes(x = shelf_space, y = sales)) +
        geom_point(shape = 1, alpha = 0.3) +
        scale_x_continuous(breaks = 50:100) +
        scale_y_continuous(breaks = seq(-700,700,10))
)
```

![](012_classical_multiple_linear_regression_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
its hard to see the effect of shelf space


```r
(df_combinded_part_extreme_randomised %>% 
        ggplot(aes(x = shelf_space, y = sales)) +
        geom_point(shape = 1, alpha = 0.3) +
        geom_smooth(method = lm) + 
        scale_x_continuous(breaks = 50:100) +
        scale_y_continuous(breaks = seq(-700,700,10))
)
```

```
## `geom_smooth()` using formula 'y ~ x'
```

![](012_classical_multiple_linear_regression_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
but if you plot the fitted line, we can still see it


```r
(df_combinded_part_extreme_randomised %>% 
        ggplot(aes(x = dist_points, y = sales)) +
        geom_point(shape = 1, alpha = 0.3) +
        scale_x_continuous(breaks = 50:100) +
        scale_y_continuous(breaks = seq(-700,700,10))
)
```

![](012_classical_multiple_linear_regression_files/figure-html/unnamed-chunk-15-1.png)<!-- -->
dist points is still contributing to the sales!!!!



```r
df_combinded_part_extreme_randomised <- lm(sales ~ dist_points + shelf_space, data = df_combinded_part_extreme_randomised)
summary(df_combinded_part_extreme_randomised)
```

```
## 
## Call:
## lm(formula = sales ~ dist_points + shelf_space, data = df_combinded_part_extreme_randomised)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -258.73  -65.38   -6.16   64.42  269.35 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  31.9904    23.0963   1.385    0.167    
## dist_points  11.9188     1.4530   8.203 1.94e-15 ***
## shelf_space   0.4370     0.2835   1.541    0.124    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 94.25 on 507 degrees of freedom
## Multiple R-squared:  0.1208,	Adjusted R-squared:  0.1173 
## F-statistic: 34.83 on 2 and 507 DF,  p-value: 6.686e-15
```

```r
# Coefficients:
#     Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -1.7164    24.0538  -0.071  0.94314    
# dist_points  10.7457     1.5132   7.101  4.2e-12 ***
#     shelf_space   0.9498     0.2953   3.217  0.00138 ** 
#     ---
#     Signif. codes:  0 ???***??? 0.001 ???**??? 0.01 ???*??? 0.05 ???.??? 0.1 ??? ??? 1
# 
# Residual standard error: 98.15 on 507 degrees of freedom
# Multiple R-squared:  0.107,	Adjusted R-squared:  0.1035 
# F-statistic: 30.39 on 2 and 507 DF,  p-value: 3.433e-13
```

shelf space has a very high p value -> still the coefficient is properly calculated!!!!!!
R squared is low, but the underlying effect, how sales has been constructed has been found by the model!!!!!!
p value has very little to say about the effect of a parameter itself
it can be interpreted in a way that it gives you an indication if there are more or less data points available for the combinations of the model









# Example 4 -------------------------------------------------------------------------------------------------------
We know that:
- every shelf space point increases sales by 1
- every additional distribution point increases sales by 10
- placing a cooler increases sales by 30


```r
df_combinded_norandom_analyse_preknowledge <- 
    expand.grid(dist_points = rep(1:10, 10),
                shelf_space = rep(50:100, 10),
                is_cooler = c(TRUE, FALSE)
    ) %>% 
    mutate(sales = dist_points * dist_points_k + dist_points_d +
               shelf_space * shelf_space_k + shelf_space_d +
               if_else(is_cooler, 30, 0)
    )
```


```r
(df_combinded_norandom_analyse_preknowledge %>% 
        ggplot(aes(x = shelf_space, y = sales)) +
        geom_point(shape = 1, alpha = 0.3) +
        geom_smooth(method = lm) +
        scale_x_continuous(breaks = 50:100) +
        scale_y_continuous(breaks = seq(0,500,10))
)
```

```
## `geom_smooth()` using formula 'y ~ x'
```

![](012_classical_multiple_linear_regression_files/figure-html/unnamed-chunk-18-1.png)<!-- -->


```r
(df_combinded_norandom_analyse_preknowledge %>% 
        ggplot(aes(x = dist_points, y = sales)) +
        geom_point(shape = 1, alpha = 0.3) +
        geom_smooth(method = lm) +
        scale_x_continuous(breaks = 1:10) +
        scale_y_continuous(breaks = seq(0,500,10))
)
```

```
## `geom_smooth()` using formula 'y ~ x'
```

![](012_classical_multiple_linear_regression_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

```r
df_combinded_norandom_analyse_preknowledge %>% 
    group_by(is_cooler) %>% summarise(sales = mean(sales)) %>% 
    ggplot(aes(x = is_cooler, y = sales)) +
    geom_bar(stat= 'identity') +
    geom_label(aes(label= sales) )
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

![](012_classical_multiple_linear_regression_files/figure-html/unnamed-chunk-20-1.png)<!-- -->
we see that a cooler increases sales by 30 !!!!!


```r
lm_res_analyse_effect <- lm(sales ~ dist_points + shelf_space + is_cooler, data = df_combinded_norandom_analyse_preknowledge)
summary(lm_res_analyse_effect)
```

```
## 
## Call:
## lm(formula = sales ~ dist_points + shelf_space + is_cooler, data = df_combinded_norandom_analyse_preknowledge)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -9.37e-09 -1.00e-13  1.00e-13  3.00e-13  1.87e-11 
## 
## Coefficients:
##                 Estimate Std. Error    t value Pr(>|t|)    
## (Intercept)   -3.210e-11  5.174e-13 -6.204e+01   <2e-16 ***
## dist_points    1.000e+01  3.203e-14  3.122e+14   <2e-16 ***
## shelf_space    1.000e+00  6.251e-15  1.600e+14   <2e-16 ***
## is_coolerTRUE  3.000e+01  1.840e-13  1.630e+14   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.938e-11 on 101996 degrees of freedom
## Multiple R-squared:      1,	Adjusted R-squared:      1 
## F-statistic: 4.988e+28 on 3 and 101996 DF,  p-value: < 2.2e-16
```

```r
# Coefficients:
#     Estimate Std. Error    t value Pr(>|t|)    
# (Intercept)   -3.210e-11  5.174e-13 -6.204e+01   <2e-16 ***
#     dist_points    1.000e+01  3.203e-14  3.122e+14   <2e-16 ***
#     shelf_space    1.000e+00  6.251e-15  1.600e+14   <2e-16 ***
#     is_coolerTRUE  3.000e+01  1.840e-13  1.630e+14   <2e-16 ***
#     ---
#     Signif. codes:  0 ???***??? 0.001 ???**??? 0.01 ???*??? 0.05 ???.??? 0.1 ??? ??? 1
# 
# Residual standard error: 2.938e-11 on 101996 degrees of freedom
# Multiple R-squared:      1,	Adjusted R-squared:      1 
# F-statistic: 4.988e+28 on 3 and 101996 DF,  p-value: < 2.2e-16
```

we see exactly the above effects!!!!!!!!!










