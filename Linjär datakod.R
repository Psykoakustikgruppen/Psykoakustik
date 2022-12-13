 library(readr)
 library(tidyverse)
 library(ggplot2)
 library(ggpubr)
 longdata <- read_delim("longdata.csv", delim = ";", 
                        escape_double = FALSE, trim_ws = TRUE)

#Create linear model
mod5L <- lm(longdata$`0.5kHz-upplevd-L` ~ longdata$`0.5kHz-dB-L`) 
mod5R <- lm(longdata$`0.5kHz-upplevd-R` ~ longdata$`0.5kHz-dB-R`) 
mod4L <- lm(longdata$`4kHz-upplevd-L` ~ longdata$`4kHz-dB-L`) 
mod4R <- lm(longdata$`4kHz-upplevd-R` ~ longdata$`4kHz-dB-R`) 

summary(mod5L)

summary(mod5R)

summary(mod4L)

summary(mod4R)