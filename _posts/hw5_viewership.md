---
layout: post
title:  "Viewership Model"
date:   11/8/2019
excerpt: "Viewership Time Series Model"
tag:
- markdown
- syntax
- sample
- test
- jekyll
---

``` r
library(nlme)
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------ tidyverse 1.2.1 --

    ## v ggplot2 3.1.0     v purrr   0.2.5
    ## v tibble  2.0.1     v dplyr   0.7.8
    ## v tidyr   0.8.2     v stringr 1.3.1
    ## v readr   1.3.1     v forcats 0.3.0

    ## -- Conflicts --------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::collapse() masks nlme::collapse()
    ## x dplyr::filter()   masks stats::filter()
    ## x dplyr::lag()      masks stats::lag()

``` r
library(knitr)
```

    ## Warning: package 'knitr' was built under R version 3.5.3

``` r
library(splines)
library(astsa)
library(sarima)
```

    ## Loading required package: FitAR

    ## Loading required package: lattice

    ## Loading required package: leaps

    ## Loading required package: ltsa

    ## Loading required package: bestglm

    ## Loading required package: stats4

    ## 
    ## Attaching package: 'sarima'

    ## The following object is masked from 'package:astsa':
    ## 
    ##     sarima

``` r
views <- read.csv('https://mheaton.byu.edu/Courses/Stat469/Topics/2%20-%20TemporalCorrelation/1%20-%20TimeSeries/HWCaseStudy/Data/Viewership.txt', header = TRUE, sep="")

glimpse(views)
```

    ## Observations: 70
    ## Variables: 4
    ## $ Season  <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, ...
    ## $ Episode <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8,...
    ## $ ShowNum <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,...
    ## $ Viewers <dbl> 2.22, 2.20, 2.44, 2.45, 2.58, 2.44, 2.40, 2.72, 2.66, ...

Because the change in viewership is highly important in determining whether to keep producing the show, use the log-transformed Viewers variable in all your analysis below. This way, the change from one show to the next corresponds to a percentage increase or decrease in viewership.

``` r
#The following variable refers to the percentage increase of decrease in viewership
views$logviewers <- log(views$Viewers)
```

1
=

Create exploratory plots and calculate summary statistics from the time series. Comment on any potential relationships you see between log(Viewers) and ShowNum (note, we are using ShowNum to denote “time” in this analysis).

``` r
scat_log <- ggplot(views, aes(ShowNum, logviewers)) + geom_point() + geom_path() + geom_smooth(se = FALSE)
scat_log + labs(title = "Scatter Plot of Log Viewership")
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](hw5_viewership_files/figure-markdown_github/unnamed-chunk-3-1.png) Above we can see the linear relationship between the show number and the percentage change in viewerships. There is a positive correlation between both.

2
=

Fit a linear regression model to log(Viewers) using ShowNum as the explanatory variable. Determine if there is temporal correlation in the residuals which should be accounted for in your model. Discuss what this temporal correlation means for viewership.

``` r
#Create new variables of 
fit <- lm(views$logviewers~ShowNum, data = views)
summary(fit)
```

    ## 
    ## Call:
    ## lm(formula = views$logviewers ~ ShowNum, data = views)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.29302 -0.10173  0.01179  0.08494  0.26075 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 0.959945   0.033127   28.98   <2e-16 ***
    ## ShowNum     0.021691   0.000811   26.75   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1371 on 68 degrees of freedom
    ## Multiple R-squared:  0.9132, Adjusted R-squared:  0.9119 
    ## F-statistic: 715.3 on 1 and 68 DF,  p-value: < 2.2e-16

``` r
#Determine if there is temporal correlation in the residuals
views_resids <- fit$residuals

#Look at residual plot
plot(fit)
```

![](hw5_viewership_files/figure-markdown_github/unnamed-chunk-4-1.png)![](hw5_viewership_files/figure-markdown_github/unnamed-chunk-4-2.png)![](hw5_viewership_files/figure-markdown_github/unnamed-chunk-4-3.png)![](hw5_viewership_files/figure-markdown_github/unnamed-chunk-4-4.png)

``` r
my.ACF <- acf(views_resids, lag.max=10000)
```

![](hw5_viewership_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
ACF.dframe <- data.frame(Lag=my.ACF$lag, ACF=my.ACF$acf)
acf_plot <- ggplot(data=ACF.dframe, aes(x=Lag, y=ACF)) + geom_col()
acf_plot + labs(title = "Viewership Autocorrelation Plot")
```

![](hw5_viewership_files/figure-markdown_github/unnamed-chunk-5-2.png) \#3

Fixing d=0 and D=1, determine appropriate values of p, q, P, Q in your time series model (note you should be able to figure out the seasonal cycle value S). Only consider p∈{0,1,2}, q∈{0,1,2}, P∈{0,1} and Q∈{0,1}. Discuss how you came to choose your specific values.

``` r
#3x3x2x2 = 36
p <- c(0,1,2)
q <- c(0,1,2)
P <- c(0,1)
Q <- c(0,1)
model.combos <- expand.grid(p, q, P, Q)

AIC.vals <- rep(NA, nrow(model.combos))
X <- matrix(views$ShowNum, ncol=1)
for(m in 1:nrow(model.combos)) {
  my.model <- astsa::sarima(views$logviewers, p=model.combos[m,"Var1"],
                     d=0,
                     q=model.combos[m,"Var2"],
                     P=model.combos[m,"Var3"],
                     D=1,
                     Q=model.combos[m,"Var4"],
                     S=10,
                     xreg=X,
                     details=FALSE)
  AIC.vals[m] <- my.model$AIC
}

model.combos[which.min(AIC.vals),]
```

    ##    Var1 Var2 Var3 Var4
    ## 21    2    0    0    1

``` r
my.model$ttable
```

    ##      Estimate     SE t.value p.value
    ## ar1    0.5941 0.7142  0.8318  0.4092
    ## ar2    0.3174 0.6785  0.4678  0.6418
    ## ma1    0.0791 0.7258  0.1090  0.9136
    ## ma2    0.0440 0.2358  0.1865  0.8528
    ## sar1  -0.1243 0.2710 -0.4586  0.6484
    ## sma1  -0.6391 0.3131 -2.0413  0.0462
    ## xreg   0.0251 0.0048  5.2529  0.0000

I calculated the minimum AIC value and chose the model that corresponded to such which ended up being 2,0,0,0,1,1.

4
=

Write down your selected time series regression model in terms of population parameters including your specification for the time series component of the residuals. Explain the meaning of any parameters in your model (including the time series components). Explain how statistical inference for your model can be used to predict the viewership moving forward.

This is how we will model our analysis using X, *β*, and *ϵ* with *β* being our matrix of coefficients that correspond to each observation. The dimensions of the *β* matrix is 2 x 1 and the dimensions of the X matrix is n x 2. *y* = *X**β* + *ϵ*

To model correlation we look at the error using a SARIMA model. *ϵ* = *S**A**R**I**M**A*(2, 0, 0, 0, 1, 1)<sub>10</sub>

Each epsilon refers to differencing while our *ϕ* in this case is the autoregressive coefficient and the *ω* is our moving average coefficient.

*d*<sub>*t*</sub> = *ϵ*<sub>*t*</sub> + *ϵ*<sub>*t* − 10</sub> *d*<sub>*t*</sub> = *ϕ*<sub>1</sub>*d*<sub>*t* − 1</sub> + *ϕ*<sub>2</sub>*d*<sub>*t* − 2</sub> + *θ**ω*<sub>*t* − 10</sub> + *ω*<sub>*t*</sub>

*ω*<sub>*t*</sub> ∼ *N*(0, *σ*<sup>2</sup>*β*)

5
=

Fit your chosen time series model and validate any model assumptions you used.

``` r
fitTM <- astsa::sarima(views$logviewers, p=2, d=0, q=0, P=0, D=1, Q=1, S=10, xreg=X, details=FALSE)

#views$logviewers
#Get Residuals
views_resid <- resid(fitTM$fit)
fit_vals <- views$logviewers - views_resid

ggplot(views, aes(fit_vals, views_resid)) + geom_point() + xlab("Fitted Values") + ylab("Residuals")
```

    ## Don't know how to automatically pick scale for object of type ts. Defaulting to continuous.
    ## Don't know how to automatically pick scale for object of type ts. Defaulting to continuous.

![](hw5_viewership_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
acf(views_resid, lag.max=36)
```

![](hw5_viewership_files/figure-markdown_github/unnamed-chunk-7-2.png) To check our LINE assumptions we look at the scatter plot to check linearity, the Fitted vs the Residual values to check normality. and the acf plot to check for autocorrelation.

6
=

Perform a cross-validation of predictions generated from your model for the most recent season of shows. Report the quality of your predictions in terms of RPMSE.

``` r
#Split train and test data
train.set <- subset(views, Season != 7)
test.set <- subset(views, Season == 7)
X.train <- as.matrix(X[views$Season != 7,])
X.test <- as.matrix(X[views$Season == 7,])

my.For <- sarima.for(train.set$logviewers, p=2, d=0, q=0, P=0, D=1, Q=1, S=10, xreg=X.train, n.ahead=10, newxreg=X.test)
```

![](hw5_viewership_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
#Root predicted mean square error
rpmse <- (my.For$pred - test.set$logviewers)^2 %>% mean() %>% sqrt()
rpmse
```

    ## [1] 0.09846837

Our RPMSE is .098 which is a relatively good fit and can use our model for testing.

7
=

Determine if viewership is increasing or decreasing. Support your conclusions with appropriate hypothesis tests and confidence intervals.

Below we run a hypothesis test to see if viewership is increasing or decreasing. Our null hypothesis being that *H*<sub>0</sub> : *β*<sub>1</sub> = 0 and our alternative is that *H*<sub>*a*</sub> : *β*<sub>1</sub>  ≠ 0. Our p-value is 0.000 and reject the null hypothesis concluding that *β*<sub>1</sub> &gt; 0 and thus viewership is increasing.

``` r
my.model$ttable
```

    ##      Estimate     SE t.value p.value
    ## ar1    0.5941 0.7142  0.8318  0.4092
    ## ar2    0.3174 0.6785  0.4678  0.6418
    ## ma1    0.0791 0.7258  0.1090  0.9136
    ## ma2    0.0440 0.2358  0.1865  0.8528
    ## sar1  -0.1243 0.2710 -0.4586  0.6484
    ## sma1  -0.6391 0.3131 -2.0413  0.0462
    ## xreg   0.0251 0.0048  5.2529  0.0000

``` r
my.model$ttable[7,1] + c(-1,1)*qt(1-0.025, df=nrow(views)-5)*my.model$ttable[7,2]
```

    ## [1] 0.01551374 0.03468626

Season 8 is already in production. Forecast the log(Viewers) forward for season 8. Comment on how executives would be able to use these forecasts to gauge if the show should continue into a ninth season.

``` r
newX <- matrix(71:80, ncol=1)
sarima.for(xdata = views$logviewers, p=2, d=0, q=0, P=0, D=1, Q=1, S=1, n.ahead=10, newxreg=newX)
```

![](hw5_viewership_files/figure-markdown_github/unnamed-chunk-10-1.png)

    ## $pred
    ## Time Series:
    ## Start = 71 
    ## End = 80 
    ## Frequency = 1 
    ##  [1] 2.566269 2.594671 2.623470 2.645951 2.672244 2.697728 2.722777
    ##  [8] 2.748294 2.773628 2.798961
    ## 
    ## $se
    ## Time Series:
    ## Start = 71 
    ## End = 80 
    ## Frequency = 1 
    ##  [1] 0.08626833 0.10535769 0.11898414 0.13549659 0.14826675 0.16021950
    ##  [7] 0.17162878 0.18211406 0.19208837 0.20158064

Executives could use these prediction to determine how well season 8 performs based on whether season 8's viewership is better than the predicted values.
