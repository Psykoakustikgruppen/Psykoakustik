#Installera paket
install.packages("ggplot2")
install.packages("ggpubr")

# setup ====
library(readr)
library(tidyverse)
library(ggplot2)
library(ggpubr)

# Read data ====
aud.dat <- read_delim("aud.csv", delim = ";", 
                  escape_double = FALSE, trim_ws = TRUE)

#separate UCL data
UCL.dat <- aud.dat[c("ID", "UCLL500", "UCLL1000", "UCLL2000", "UCLL4000", "UCLR500", "UCLR1000", "UCLR2000", "UCLR4000")]

#Remove variable "Group" and UCÖ-variables from aud dataframe
aud.dat <- within(aud.dat, rm(Group, UCLL500, UCLL1000, UCLL2000, UCLL4000, UCLR500, UCLR1000, UCLR2000, UCLR4000))   


# make long
aud.dat <- gather(aud.dat, key = "ear-freq", value = "dB", -ID)
UCL.dat <- gather(UCL.dat, key = "ear-freq", value = "dB", -ID)

# split and rename variables
aud.dat <- aud.dat %>% 
  separate(col = "ear-freq", into = c("ear","freq"), sep = (1)) %>%
  mutate(freq = (type.convert(freq))/1000) %>% 
  mutate(freqLabels = factor(freq)) %>% 
  mutate(ear = factor(ear, levels = c("R", "L"))) %>%
  mutate(ear = recode(ear, "R" = "Right", "L" = "Left"))

UCL.dat <- UCL.dat %>% 
  separate(col = "ear-freq", into = c("ear","freq"), sep = (4)) %>%
  mutate(freq = (type.convert(freq))/1000) %>% 
  mutate(freqLabels = factor(freq)) %>% 
  mutate(ear = factor(ear, levels = c("UCLR", "UCLL"))) %>%
  mutate(ear = recode(ear, "UCLR" = "Right", "UCLL" = "Left"))

# define function for SEM (standard error of mean)
std_error <- function(x) sd(x)/sqrt(length(x))

# Regular audiogram with SEM ====
ggplot(data = aud.dat, aes(y = dB, x = freqLabels, group = ear, color = ear, shape = ear))+
  
  # Aud layers
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - std_error(x), 
               fun.max = function(x) mean(x) + std_error(x), 
               geom = "pointrange", position=position_dodge(width=0.3), size = 1, alpha = 0.5) +
  stat_summary(fun = mean, geom = "line") +
  
  # UCL layers
  stat_summary(data = UCL.dat, fun = mean, geom = "line") +
  stat_summary(data = UCL.dat, fun = mean,
               fun.min = function(x) mean(x) - std_error(x),
               fun.max = function(x) mean(x) + std_error(x), 
               geom = "pointrange", size = 1, alpha = 0.5, position=position_dodge(width=0.3)) +
  
  
  scale_y_reverse(limits = c(90,-10), breaks = seq(-10, 90, by=10))+
  
  geom_hline(yintercept=25, linetype="dashed", color = "black", size=0.5)+
  labs(x = "Frequency (kHz)", y = "Threshold (dB HL)")+
  ggtitle("Audiogram") +
  theme_bw()+
  theme(plot.title = element_text(size = 16), axis.text = element_text(size = 12))



######################
## Left and Right ear split with different groups
######################

# Read data ====
aud.dat <- read_delim("aud.csv", delim = ";", 
                      escape_double = FALSE, trim_ws = TRUE)

#Specify group as factor
aud.dat$Group <- factor(aud.dat$Group, levels = c("1", "2"))

#Recode group
aud.dat$Group <- recode(aud.dat$Group, "1" = "M", "2" = "F")

# Do NOT remove variable "Group" (as for above non-grouped aud)
# aud.dat <- within(aud.dat, rm(Group))

# make long
aud.dat <- gather(aud.dat, key = "ear-freq", value = "dB", -ID, -Group)

# split and rename variables
aud.dat <- aud.dat %>% 
  separate(col = "ear-freq", into = c("ear","freq"), sep = (1)) %>%
  mutate(freq = (type.convert(freq))/1000) %>% 
  mutate(freqLabels = factor(freq)) %>% 
  mutate(ear = factor(ear, levels = c("R", "L"))) %>%
  mutate(ear = recode(ear, "R" = "Right", "L" = "Left"))

# define function for SEM (standard error of mean)
std_error <- function(x) sd(x)/sqrt(length(x))

##Create plot for LEFT EAR ====
aud_L <- ggplot(data = aud.dat[aud.dat$ear == "Left",], aes(y = dB, x = freqLabels, group = Group, color = Group, shape = Group))+
  
  #Aud layers
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - std_error(x), 
               fun.max = function(x) mean(x) + std_error(x), 
               geom = "pointrange", position=position_dodge(width=0.3), size = 1, alpha = 0.5) +
  stat_summary(fun = mean, geom = "line") +
  
  scale_y_reverse(limits = c(90,-10), breaks = seq(-10, 90, by=10))+
  
  geom_hline(yintercept=25, linetype="dashed", color = "black", size=0.5)+
  labs(x = "Frequency (kHz)", y = "Threshold (dB HL)")+
  ggtitle("Audiogram L") +
  theme_bw()+
  theme(plot.title = element_text(size = 16), axis.text = element_text(size = 12))

#Create plot for RIGHT EAR
aud_R <- ggplot(data = aud.dat[aud.dat$ear == "Right",], aes(y = dB, x = freqLabels, group = Group, color = Group, shape = Group))+
  
  #Aud layers
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - std_error(x), 
               fun.max = function(x) mean(x) + std_error(x), 
               geom = "pointrange", position=position_dodge(width=0.3), size = 1, alpha = 0.5) +
  stat_summary(fun = mean, geom = "line") +
  
  scale_y_reverse(limits = c(90,-10), breaks = seq(-10, 90, by=10))+
  
  geom_hline(yintercept=25, linetype="dashed", color = "black", size=0.5)+
  labs(x = "Frequency (kHz)", y = "Threshold (dB HL)")+
  ggtitle("Audiogram R") +
  theme_bw()+
  theme(plot.title = element_text(size = 16), axis.text = element_text(size = 12))

## Display Left and Right ears for groups ====

ggarrange(aud_L, aud_R, ncol = 2, nrow = 1)