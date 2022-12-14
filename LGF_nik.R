
#Load libraries
library(readr)
library(tidyverse)
library(MASS)
library(ggplot2)
library(ggpubr)

#Read in data
longdata <- read_delim("longdata.csv", delim = ";", 
                       escape_double = FALSE, trim_ws = TRUE)

#Subset data for frequency and ear - easier to work with
dat5L <- longdata[c("0.5kHz-upplevd-L", "0.5kHz-dB-L")]
names(dat5L) <- c("loudness", "dB")
dat5R <- longdata[c("0.5kHz-upplevd-R", "0.5kHz-dB-R")]
names(dat5R) <- c("loudness", "dB")
dat4L <- longdata[c("4kHz-upplevd-L", "4kHz-dB-L")]
names(dat4L) <- c("loudness", "dB")
dat4R <- longdata[c("4kHz-upplevd-R", "4kHz-dB-R")]
names(dat4R) <- c("loudness", "dB")

#Create linear models
lm_mod5L <- lm(loudness ~ dB, data = dat5L)
lm_mod5R <- lm(loudness ~ dB, data = dat5R)

lm_mod4L <- lm(loudness ~ dB, data = dat4L)
lm_mod4R <- lm(loudness ~ dB, data = dat4R)

#Create plots of data + linear models
plot5L <- ggplot(dat5L, aes(x = dB, y = loudness)) + 
  geom_point(shape = 1, size = 3) +
  stat_smooth(method = "lm", col = "red", alpha = 0.35)+
  xlab("")+
  ylab("Loudness category")+
  ggtitle("500Hz Left") +
  theme_bw()

plot5R <- ggplot(dat5R, aes(x = dB, y = loudness)) + 
  geom_point(shape = 1, size = 3) +
  stat_smooth(method = "lm", col = "red", alpha = 0.35)+
  xlab("Level presented (dB HL)")+
  ylab("Loudness category")+
  ggtitle("500Hz Right") +
  theme_bw()

plot4L <- ggplot(dat4L, aes(x = dB, y = loudness)) + 
  geom_point(shape = 1, size = 3) +
  stat_smooth(method = "lm", col = "red", alpha = 0.35)+
  xlab("")+
  ylab("")+
  ggtitle("4000Hz Left") +
  theme_bw()

plot4R <- ggplot(dat4R, aes(x = dB, y = loudness)) + 
  geom_point(shape = 1, size = 3) +
  stat_smooth(method = "lm", col = "red", alpha = 0.35)+
  xlab("Level presented (dB HL)")+
  ylab("")+
  ggtitle("4000Hz Right") +
  theme_bw()

#Arrange and output plots on 2-by-2 grid
ggarrange(plot5L, plot4L, plot5R, plot4R, ncol=2, nrow=2)

#Summarize lm: linear models (to report R2 value in figure legend)
summary(lm_mod5L)
summary(lm_mod5R)
summary(lm_mod4L)
summary(lm_mod4R)

#Copy data to new data.frames with "loudness" as categorical variable
dat5L_cat <- dat5L
dat5L_cat$loudness <- as.factor(dat5L_cat$loudness)

dat5R_cat <- dat5R
dat5R_cat$loudness <- as.factor(dat5R_cat$loudness)

dat4L_cat <- dat4L
dat4L_cat$loudness <- as.factor(dat4L_cat$loudness)

dat4R_cat <- dat4R
dat4R_cat$loudness <- as.factor(dat4R_cat$loudness)

#Create logistic regression models from dataframes with loudness as categorical
glm_mod5L <- glm(loudness ~ dB, data = dat5L_cat, family = "binomial")
glm_mod5R <- glm(loudness ~ dB, data = dat5R_cat, family = "binomial")

glm_mod4L <- glm(loudness ~ dB, data = dat4L_cat, family = "binomial")
glm_mod4R <- glm(loudness ~ dB, data = dat4R_cat, family = "binomial")

#Print summary of logistic models
summary(glm_mod5L)
summary(glm_mod5R)
summary(glm_mod4L)
summary(glm_mod4R)
