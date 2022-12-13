> library(readr)
> library(tidyverse)
── Attaching packages ──────────────── tidyverse 1.3.2 ──
✔ ggplot2 3.4.0      ✔ dplyr   1.0.10
✔ tibble  3.1.8      ✔ stringr 1.5.0 
✔ tidyr   1.2.1      ✔ forcats 0.5.2 
✔ purrr   0.3.5      
── Conflicts ─────────────────── tidyverse_conflicts() ──
✖ dplyr::filter() masks stats::filter()
✖ dplyr::lag()    masks stats::lag()
> library(MASS)

Attaching package: ‘MASS’

The following object is masked from ‘package:dplyr’:
  

> library(ggplot2)
> library(ggpubr)
> longdata <- read_delim("longdata.csv", delim = ";", 
                         +                        escape_double = FALSE, trim_ws = TRUE)
Rows: 150 Columns: 9                                   
── Column specification ─────────────────────────────────
Delimiter: ";"
dbl (9): ID, 0.5kHz-dB-R, 0.5kHz-upplevd-R, 0.5kHz-dB...

ℹ Use `spec()` to retrieve the full column specification for this data.
ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
> #Rename
  > dat5L <- longdata[c("0.5kHz-upplevd-L", "0.5kHz-dB-L")]
> names(dat5L) <- c("loudness", "dB")
> dat5R <- longdata[c("0.5kHz-upplevd-R", "0.5kHz-dB-R")]
> names(dat5R) <- c("loudness", "dB")
> dat4L <- longdata[c("4kHz-upplevd-L", "4kHz-dB-L")]
> names(dat4L) <- c("loudness", "dB")
> dat4R <- longdata[c("4kHz-upplevd-R", "4kHz-dB-R")]
> names(dat4R) <- c("loudness", "dB")
> #Create linear model
  > mod5L <- lm(longdata$`0.5kHz-upplevd-L` ~ longdata$`0.5kHz-dB-L`) 
> mod5R <- lm(loudness ~ dB, data = dat5R)
> 
  > mod5R <- lm(longdata$`0.5kHz-upplevd-R` ~ longdata$`0.5kHz-dB-R`) 
> mod4L <- lm(longdata$`4kHz-upplevd-L` ~ longdata$`4kHz-dB-L`) 
> mod4R <- lm(longdata$`4kHz-upplevd-R` ~ longdata$`4kHz-dB-R`) 
> summary(mod5L)

Call:
  lm(formula = longdata$`0.5kHz-upplevd-L` ~ longdata$`0.5kHz-dB-L`)

Residuals:
  Min       1Q   Median       3Q      Max 
-1.95638 -0.50680  0.08202  0.54137  1.37050 

Coefficients:
  Estimate Std. Error t value
(Intercept)            0.119648   0.121716   0.983
longdata$`0.5kHz-dB-L` 0.048080   0.002219  21.669
Pr(>|t|)    
(Intercept)               0.327    
longdata$`0.5kHz-dB-L`   <2e-16 ***
  ---
  Signif. codes:  
  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.6726 on 148 degrees of freedom
Multiple R-squared:  0.7603,	Adjusted R-squared:  0.7587 
F-statistic: 469.5 on 1 and 148 DF,  p-value: < 2.2e-16

> summary(mod5R)

Call:
  lm(formula = longdata$`0.5kHz-upplevd-R` ~ longdata$`0.5kHz-dB-R`)

Residuals:
  Min      1Q  Median      3Q     Max 
-1.3618 -0.4342  0.1142  0.4623  1.3018 

Coefficients:
  Estimate Std. Error t value
(Intercept)            0.141567   0.111414   1.271
longdata$`0.5kHz-dB-R` 0.048063   0.002125  22.620
Pr(>|t|)    
(Intercept)               0.206    
longdata$`0.5kHz-dB-R`   <2e-16 ***
  ---
  Signif. codes:  
  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.627 on 148 degrees of freedom
Multiple R-squared:  0.7757,	Adjusted R-squared:  0.7741 
F-statistic: 511.7 on 1 and 148 DF,  p-value: < 2.2e-16

> summary(mod4L)

Call:
  lm(formula = longdata$`4kHz-upplevd-L` ~ longdata$`4kHz-dB-L`)

Residuals:
  Min      1Q  Median      3Q     Max 
-1.8559 -0.5065  0.0551  0.6641  1.4934 

Coefficients:
  Estimate Std. Error t value
(Intercept)          0.426618   0.155793   2.738
longdata$`4kHz-dB-L` 0.043379   0.002991  14.505
Pr(>|t|)    
(Intercept)           0.00693 ** 
  longdata$`4kHz-dB-L`  < 2e-16 ***
  ---
  Signif. codes:  
  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.8663 on 148 degrees of freedom
Multiple R-squared:  0.587,	Adjusted R-squared:  0.5842 
F-statistic: 210.4 on 1 and 148 DF,  p-value: < 2.2e-16

> summary(mod4R)

Call:
  lm(formula = longdata$`4kHz-upplevd-R` ~ longdata$`4kHz-dB-R`)

Residuals:
  Min       1Q   Median       3Q      Max 
-1.69720 -0.41058  0.09305  0.50043  1.72229 

Coefficients:
  Estimate Std. Error t value
(Intercept)           0.37517    0.12155   3.087
longdata$`4kHz-dB-R`  0.04147    0.00248  16.719
Pr(>|t|)    
(Intercept)           0.00242 ** 
  longdata$`4kHz-dB-R`  < 2e-16 ***
  ---
  Signif. codes:  
  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.6842 on 148 degrees of freedom
Multiple R-squared:  0.6538,	Adjusted R-squared:  0.6515 
F-statistic: 279.5 on 1 and 148 DF,  p-value: < 2.2e-16