
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
  stat_smooth(method = "lm", col = "orange", alpha = 0.35)+
  xlab("")+
  ylab("Loudness category")+
  ggtitle("500Hz Left") +
  theme_bw()

plot5R <- ggplot(dat5R, aes(x = dB, y = loudness)) + 
  geom_point(shape = 1, size = 3) +
  stat_smooth(method = "lm", col = "orange", alpha = 0.35)+
  xlab("Level presented (dB HL)")+
  ylab("Loudness category")+
  ggtitle("500Hz Right") +
  theme_bw()

plot4L <- ggplot(dat4L, aes(x = dB, y = loudness)) + 
  geom_point(shape = 1, size = 3) +
  stat_smooth(method = "lm", col = "green", alpha = 0.35)+
  xlab("")+
  ylab("")+
  ggtitle("4000Hz Left") +
  theme_bw()

plot4R <- ggplot(dat4R, aes(x = dB, y = loudness)) + 
  geom_point(shape = 1, size = 3) +
  stat_smooth(method = "lm", col = "green", alpha = 0.35)+
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
ord_mod5L <- polr(loudness ~ dB, data = dat5L_cat, Hess = TRUE)
ord_mod5R <- polr(loudness ~ dB, data = dat5R_cat, Hess = TRUE)

ord_mod4L <- polr(loudness ~ dB, data = dat4L_cat, Hess = TRUE)
ord_mod4R <- polr(loudness ~ dB, data = dat4R_cat, Hess = TRUE)

#Print summary of logistic models
summary(ord_mod5L)
summary(ord_mod5R)
summary(ord_mod4L)
summary(ord_mod4R)

#Calculate Odds Ratio and its 95% confidence interval and p
OR5L <- unname(exp(coef(ord_mod5L)["dB"]))
conf5L <- unname(exp(confint(ord_mod5L)))
p5L <- pnorm(abs(coef(summary(ord_mod5L))["dB","t value"]), lower.tail = FALSE) * 2

OR5R <- unname(exp(coef(ord_mod5R)["dB"]))
conf5R <- unname(exp(confint(ord_mod5R)))
p5R <- pnorm(abs(coef(summary(ord_mod5R))["dB","t value"]), lower.tail = FALSE) * 2

OR4L <- unname(exp(coef(ord_mod4L)["dB"]))
conf4L <- unname(exp(confint(ord_mod4L)))
p4L <- pnorm(abs(coef(summary(ord_mod4L))["dB","t value"]), lower.tail = FALSE) * 2

OR4R <- unname(exp(coef(ord_mod4R)["dB"]))
conf4R <- unname(exp(confint(ord_mod4R)))
p4R <- pnorm(abs(coef(summary(ord_mod4R))["dB","t value"]), lower.tail = FALSE) * 2

#Order OR, CI and p in matrix of results
results <- matrix(nrow = 4, ncol = 4)

results[1,1:4] <- round(c(OR5L, conf5L, p5L), 3)
results[2,1:4] <- round(c(OR5R, conf5R, p5R), 3)
results[3,1:4] <- round(c(OR4L, conf4L, p4L), 3)
results[4,1:4] <- round(c(OR4R, conf4R, p4R), 3)

#Name rows and columns in result table
dimnames(results) = list(c("5L", "5R", "4L", "4R"), c("OR", "CI 2.5", "CI 97.5", "p"))
