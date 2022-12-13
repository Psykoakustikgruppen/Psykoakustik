install.packages("ggplot2")
install.packages("ggpubr")

#Load packages
library(readr)
library(tidyverse)
library(MASS)
library(ggplot2)
library(ggpubr)


#Read in data
longdata <- read_delim("longdata.csv", delim = ";", 
                       escape_double = FALSE, trim_ws = TRUE)

dat5L <- longdata[c("0.5kHz-upplevd-L", "0.5kHz-dB-L")]
names(dat5L) <- c("loudness", "dB")
dat5R <- longdata[c("0.5kHz-upplevd-R", "0.5kHz-dB-R")]
names(dat5R) <- c("loudness", "dB")
dat4L <- longdata[c("4kHz-upplevd-L", "4kHz-dB-L")]
names(dat4L) <- c("loudness", "dB")
dat4R <- longdata[c("4kHz-upplevd-R", "4kHz-dB-R")]
names(dat4R) <- c("loudness", "dB")

#Create linear model
mod5L <- lm(longdata$`0.5kHz-upplevd-L` ~ longdata$`0.5kHz-dB-L`) 
mod5R <- lm(loudness ~ dB, data = dat5R)

mod5R <- lm(longdata$`0.5kHz-upplevd-R` ~ longdata$`0.5kHz-dB-R`) 
mod4L <- lm(longdata$`4kHz-upplevd-L` ~ longdata$`4kHz-dB-L`) 
mod4R <- lm(longdata$`4kHz-upplevd-R` ~ longdata$`4kHz-dB-R`) 

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
plot(longdata$`4kHz-dB-L`, longdata$`4kHz-upplevd-L`,
     xlab="dB Level",
     ylab="Loudness")
plot(longdata$`4kHz-dB-L`, longdata$`4kHz-upplevd-L`,
     xlab="dB Level",
     ylab="Loudness")

#Inspect class of "upplevd" variable
class(longdata$`0.5kHz-upplevd-L`)
class(longdata$`0.5kHz-upplevd-R`)
class(longdata$`4kHz-upplevd-L`)
class(longdata$`4kHz-upplevd-R`)

#Overwrite upplevd as factor variable
longdata$`0.5kHz-upplevd-L` <- factor(longdata$`0.5kHz-upplevd-L`)
longdata$`0.5kHz-upplevd-R` <- factor(longdata$`0.5kHz-upplevd-R`)
longdata$`4kHz-upplevd-L` <- factor(longdata$`4kHz-upplevd-L`)
longdata$`4kHz-upplevd-R` <- factor(longdata$`4kHz-upplevd-R`)

#Create logistics model
mod5L_glm <- glm(longdata$`0.5kHz-upplevd-L` ~ longdata$`0.5kHz-dB-L`, family = "binomial")
mod5R_glm <- glm(longdata$`0.5kHz-upplevd-L` ~ longdata$`0.5kHz-dB-L`, family = "binomial")
mod4L_glm <- glm(longdata$`4kHz-upplevd-L` ~ longdata$`4kHz-dB-L`, family = "binomial")
mod4R_glm <- glm(longdata$`4kHz-upplevd-R` ~ longdata$`4kHz-dB-R`, family = "binomial")

#Inspect logistic model
summary(mod5L_glm)
summary(mod5R_glm)
summary(mod4L_glm)
summary(mod4R_glm)

#A nicer plot(?) option using ggplot-package
plot1 <- ggplot(dat5R, aes(x = dB, y = loudness)) + 
  geom_point(shape = 1, size = 3) +
  stat_smooth(method = "lm", col = "red", alpha = 0.35)+
  xlab("Level presented (dB HL)")+
  ylab("Loudness category")+
  theme_bw()


plot2 <- ggplot(dat5L, aes(x = dB, y = loudness)) + 
  geom_point(shape = 1, size = 3) +
  stat_smooth(method = "lm", col = "blue", alpha = 0.35)+
  xlab("Level presented (dB HL)")+
  ylab("Loudness category")+
  theme_bw()

ggarrange(plot1, plot2, ncol=2, nrow=1)

plot3 <- ggplot(dat4R, aes(x = dB, y = loudness)) + 
  geom_point(shape = 1, size = 3) +
  stat_smooth(method = "lm", col = "red", alpha = 0.35)+
  xlab("Level presented (dB HL)")+
  ylab("Loudness category")+
  theme_bw()

plot4 <- ggplot(dat4L, aes(x = dB, y = loudness)) + 
  geom_point(shape = 1, size = 3) +
  stat_smooth(method = "lm", col = "blue", alpha = 0.35)+
  xlab("Level presented (dB HL)")+
  ylab("Loudness category")+
  theme_bw()

ggarrange(plot3, plot4, ncol=2, nrow=1)