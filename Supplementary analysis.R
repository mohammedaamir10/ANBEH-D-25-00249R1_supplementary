library(dplyr)
library(tidyverse)
library(ggplot2)
library(EnvStats)
library(ggpubr)
library(rstatix)
library(lme4)
library(gtsummary)

setwd('/Users/debarpitadas/Desktop/O.henryi/O_henryi_Data_Analysis')

#Supplementary SPL Pilot#
dat<-read.csv('Piolt_SPL.csv')


dat1<- wilcox.test(dat$MALE_SPL_PRE_EXPO,dat$MALE_SPL_POST_EXPO, paired = TRUE)
t.test(dat$MALE_SPL_PRE_EXPO,dat$MALE_SPL_POST_EXPO, paired = TRUE)

#rearranging data
spl <-c (dat$MALE_SPL_PRE_EXPO,dat$MALE_SPL_POST_EXPO)
id <- c(dat$TRIAL_NO.,dat$TRIAL_NO.)
df <- data.frame(id=id,treatment = c(dat$TREATMENT..min.,dat$TREATMENT..min.), spl_time = c(rep('Before exposure',nrow(dat)),(rep('Post exposure',nrow(dat)))),spl= spl)
df<-na.omit(df)
df$treatment <- as.factor(df$treatment)

df <- df %>%
  mutate(treatment = recode(treatment, 
                            "2" = "2 min",
                            "5" = "5 min",
                            "10" = "10 min"))

# Box plot pilot SPL change
my_plot<- df%>%
  ggplot(aes(x=spl_time,y=spl,fill=factor(spl_time)))+
  geom_boxplot() +
  scale_fill_manual(breaks = waiver(),values = c("white","grey75")) +
  geom_point(position=position_jitterdodge(),alpha=1,col='black')+
  labs(fill='Treatment') +
  labs (fill='') +
  xlab('Time of measurement') +
  ylab('SPL (dB)') +
  guides (fill=FALSE) +
  theme_classic(25)+
  theme(axis.text=element_text(face='bold'),axis.title = element_text(face='bold')) 

ggsave("SPL_change_pilot.jpg", plot = my_plot, width = 12, height = 8, units = "in", dpi = 300)

table(df$treatment,df$spl_time)

res.aov <- anova_test(data = df, dv = spl, wid = id, within = spl_time, between =treatment)
get_anova_table(res.aov)
ANOVA<- aov(data = df, dv = spl, wid = id, within = spl_time, between =treatment)
get_anova_table(ANOVA)
#Individual bins for 2,5,10 min
my_plot <- df%>%
  ggplot(aes(x=treatment,y=spl, fill=factor(spl_time)))+
  geom_boxplot() +
  scale_fill_manual(breaks = waiver(),values = c("white","grey70")) +
  geom_point(position=position_jitterdodge(),alpha=1,col='black')+
  #scale_y_continuous(breaks=c(0,0.1,0.5,1,1.5,2)) +
  # scale_y_break(c(1.05, 1.9)) +
  labs(fill='') +
  xlab('Playback duration (min)') +
  ylab('SPL (dB)') +
  theme_classic(base_size = 25) +
  theme(axis.text=element_text(face='bold'),axis.title = element_text(face='bold')) 
ggsave("SPL_Pilot_2_5_10.jpg", plot = my_plot, width = 12, height = 8, units = "in", dpi = 300)
# GLM Model
model1 <- lmer(spl ~ treatment * spl_time + (1|id), data = df)
summary(model1)
confint(model1)
anova(model1)

# Control t-test for SPL Change

dat <- read.csv('O.henryi_SPL_Movementt_D_DAS_ ALL_DATA_T1 .csv')
dat1 <- filter (dat,MALE_SPL_PRE_EXPO!='NaN', MALE_SPL_POST_EXPO!='NaN')

SPL_control<- dat1%>%
  filter(dat1$TREATMENT..min. == "0")
t.test(SPL_control$MALE_SPL_PRE_EXPO,SPL_control$MALE_SPL_POST_EXPO, paired = TRUE)

# Create a data frame for ggplot
pre_expo <- SPL_control$MALE_SPL_PRE_EXPO
post_expo <- SPL_control$MALE_SPL_POST_EXPO

plot_data <- data.frame(
  Measurement = c(rep("Five_mins_into_calling", length(pre_expo)), rep("Twenty_mins_into_calling", length(post_expo))),
  Value = c(pre_expo, post_expo)
)

CONTROL_SPL<- plot_data%>%
  ggplot(aes(x=Measurement,y=Value, fill= Measurement ))+
  geom_boxplot(size=1) +
  scale_fill_manual(breaks = waiver(),values = c("white","grey90")) +
  geom_point(position=position_jitterdodge(),alpha=1,col='black',size=2.5)+
  stat_n_text(size=10)+
  labs(fill='') +
  guides (fill=FALSE) +
  xlab('Time of measurement (min)') +
  ylab('SPL(dB)') +
  theme_classic(25)+
  theme(axis.text=element_text(face='bold'),axis.title = element_text(face='bold')) 

ggsave("Control_Spl.jpg", plot = CONTROL_SPL, width = 12, height = 8, units = "in", dpi = 300)


# Load the package.
library(rstatix)
setwd('/Users/debarpitadas/Desktop/O.henryi/O_henryi_Data_Analysis/presentation_debarpita_code_data 2')
dat<- read.csv('O.henryi_SPL_Movementt_D_DAS_ ALL_DATA_T1.csv')
dat1 <- filter (dat,MALE_SPL_PRE_EXPO!='NaN', MALE_SPL_POST_EXPO!='NaN')

#1 Welch's t-test for wild Vs Captive male SPL

library(BSDA)
# Performing the Welch's t-test with the given values
welch_result <- tsum.test(mean.x = 63.8, s.x = 2.5, n.x = 30,
                          mean.y = 62.2, s.y = 2.19, n.y = 75,
                          alternative = "two.sided", var.equal = FALSE)

# Printing the result
print(welch_result)
