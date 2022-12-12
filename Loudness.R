#Ladda in paket
library(readr)
library(tidyverse)
library(MASS)

#500 Hz - VÄNSTER
#Read in data
longdata <- read_delim("longdata.csv", delim = ";", 
                       escape_double = FALSE, trim_ws = TRUE)

#Create linear model
mod5L <- lm(longdata$`0.5kHz-upplevd-L` ~ longdata$`0.5kHz-dB-L`) 
mod5R <- lm(longdata$`0.5kHz-upplevd-R` ~ longdata$`0.5kHz-dB-R`) 
mod4L <- lm(longdata$`4kHz-upplevd - L` ~ longdata$`4kHz-dB-L`) 
mod4R <- lm(longdata$`4kHz-upplevd - R` ~ longdata$`4kHz-dB-R`) 

#Print summary of linear model
summary(mod5L)
summary(mod5R)
summary(mod4L)
summary(mod4R)

#Plot data from linear model
plot(longdata$`0.5kHz-dB-L`, longdata$`0.5kHz-upplevd-L`,
     xlab="dB Level",
     ylab="Loudness")
plot(longdata$`0.5kHz-dB-R`, longdata$`0.5kHz-upplevd-R`,
     xlab="dB Level",
     ylab="Loudness")
plot(longdata$`4kHz-dB-L`, longdata$`4kHz-upplevd - L`,
     xlab="dB Level",
     ylab="Loudness")
plot(longdata$`4kHz-dB-L`, longdata$`4kHz-upplevd - L`,
     xlab="dB Level",
     ylab="Loudness")

#Inspect class of "upplevd" variable
class(longdata$`0.5kHz-upplevd-L`)

#Overwrite upplevd as factor variable
longdata$`0.5kHz-upplevd-L` <- factor(longdata$`0.5kHz-upplevd-L`)

#Create logistics model
mod5L_glm <- glm(longdata$`0.5kHz-upplevd-L` ~ longdata$`0.5kHz-dB-L`, family = "binomial")

#Inspect logistic model
summary(mod5L_glm)
summary(mod5R_glm)
summary(mod4L_glm)
summary(mod4R_glm)