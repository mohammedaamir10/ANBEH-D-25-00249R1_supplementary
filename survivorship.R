#Script that plots surivivorship cureves for males and females under the three predation treatments

library (tidyverse)
library (ggplot2)
library (survival)  
library(ggfortify)
library(ggsurvfit)
library (survminer)

#Survivorship for males using the kaplan_meier method
setwd('/Users/debarpitadas/Desktop/O.henryi/Data&Code/')
dat <- read.csv('Data_Surv_Analysis-1.csv',na.strings = c("NA",""))
dat <- dat[!is.na(dat$Exp.ID), ]
dat$Latency[dat$Exp.ID== 1 & dat$Latency=='NaN'] <- 30
dat$Latency[dat$Exp.ID== 2 & dat$Latency=='NaN'] <- 20
dat$Exp.ID  <- ifelse (dat$Exp.ID ==1, "Experiment 1", "Experiment 2")
dat$Exp.ID <- as.factor(dat$Exp.ID)


######Figure S2 A.

survfit2(Surv(Latency, call_binary) ~ Exp.ID , data = dat) %>% 
  ggsurvfit(linetype_aes = TRUE,size=1) +
  labs(
    x = "Latency (min)",
    y = "Silent probability"
  ) +
  scale_colour_manual(values =  c("black","black","black")) +
  scale_x_continuous( breaks = c(0,5,10,15,20,25,30,35), limits=c(0,35)) +
  add_confidence_interval() +
  add_legend_title('Treatment') +
  theme_classic(base_size = 25) 

#testing for statistical differences between the survival curves

survdiff(Surv(Latency, call_binary) ~ Exp.ID,
         data = dat)



