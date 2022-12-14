#Installera paket
install.packages("readr")
install.packages("tidyverse")
install.packages("MASS")
install.packages("ggplot2")
install.packages("ggpubr")

#Load package (install with install.package("packagename") if missing)
library(readr)
library(tidyverse)
library(MASS)
library(ggplot2)
library(ggpubr)

#Read in data
longdata <- read_delim("longdata.csv", delim = ";", 
                       escape_double = FALSE, trim_ws = TRUE)

#LINJÄRA MODELLER
#Create linear model
mod5L <- lm(longdata$`0.5kHz-upplevd-L` ~ longdata$`0.5kHz-dB-L`) 
mod5R <- lm(longdata$`0.5kHz-upplevd-R` ~ longdata$`0.5kHz-dB-R`) 
mod4L <- lm(longdata$`4kHz-upplevd-L` ~ longdata$`4kHz-dB-L`) 
mod4R <- lm(longdata$`4kHz-upplevd-R` ~ longdata$`4kHz-dB-R`) 

#Linjära grafer - 500 Hz
par(mfrow=c(1,2))
plot(longdata$`0.5kHz-dB-R`, longdata$`0.5kHz-upplevd-R`,
     xlab="Level presented (dB HL)",
     ylab="Loudness (numeric)")
abline(mod5R)

plot(longdata$`0.5kHz-dB-L`, longdata$`0.5kHz-upplevd-L`,
     xlab="Level presented (dB HL)",
     ylab="Loudness (numeric)")
abline(mod5L)

#Linjära grafer - 4000 Hz
par(mfrow=c(1,2))
plot(longdata$`4kHz-dB-R`, longdata$`4kHz-upplevd-R`,
     xlab="Level presented (dB HL)",
     ylab="Loudness (numeric)")
abline(mod4R)

plot(longdata$`4kHz-dB-L`, longdata$`4kHz-upplevd-L`,
     xlab="Level presented (dB HL)",
     ylab="Loudness (numeric)")
abline(mod4L)


#LOGISTISKA MODELLER

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

# Grafer för 500 Hz - höger & vänster öra
plot1 <- ggplot(longdata, aes(x = longdata$`0.5kHz-dB-R`, y = longdata$`0.5kHz-upplevd-R`)) + 
  geom_point(shape = 1, size = 3) +
  stat_smooth(method = "lm", col = "red", alpha = 0.35)+
  xlab("Level presented (dB HL)")+
  ylab("Loudness (category)")+
  theme_bw()

plot2 <- ggplot(longdata, aes(x = longdata$`0.5kHz-dB-L`, y = longdata$`0.5kHz-upplevd-L`)) + 
  geom_point(shape = 1, size = 3) +
  stat_smooth(method = "lm", col = "blue", alpha = 0.35)+
  xlab("Level presented (dB HL)")+
  ylab("Loudness (category)")+
  theme_bw()

# 500 HZ : Lägg graferna för höger (plot1) och vänster (plot2) öra bredvid varandra
ggarrange(plot1, plot2, ncol=2, nrow=1)

# Grafer för 4 kHz - höger & vänster öra
plot3 <- ggplot(longdata, aes(x = longdata$`4kHz-dB-R`, y = longdata$`4kHz-upplevd-R`)) + 
  geom_point(shape = 1, size = 3) +
  stat_smooth(method = "lm", col = "red", alpha = 0.35)+
  xlab("Level presented (dB HL)")+
  ylab("Loudness (category)")+
  theme_bw()

plot4 <- ggplot(longdata, aes(x = longdata$`4kHz-dB-L`, y = longdata$`4kHz-upplevd-L`)) + 
  geom_point(shape = 1, size = 3) +
  stat_smooth(method = "lm", col = "blue", alpha = 0.35)+
  xlab("Level presented (dB HL)")+
  ylab("Loudness (category)")
  theme_bw()

# 4 kHz: Lägg graferna för höger (plot3) och vänster (plot4) öra - 4kHz, bredvid varandra
ggarrange(plot3, plot4, ncol=2, nrow=1)



#(test) generera R-värden för glm-modeller
install.packages("pscl")
library(pscl)
pR2(mod4R_glm)

#få information om R-värdena i pR2:
install.packages("sos")
library(sos)
help(package="pscl")
