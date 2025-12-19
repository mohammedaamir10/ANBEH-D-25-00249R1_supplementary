library(tidyverse)
library (ggplot2)
library(EnvStats)
library(ggpubr)
library(rstatix)
library(lme4)
library(glmmTMB)
library(car)
library(MuMIn)
library(moments)
library(patchwork)

#################################################################################################################
                                        #Main Manuscript Analysis and plots#
#################################################################################################################

#latency of callers and caller proportion during playback
setwd('/Users/debarpitadas/Desktop/O.henryi/O_henryi_Data_Analysis/presentation_debarpita_code_data 2')
df <- read.csv('Latency + Binary call data.csv')

#caller porportion during playback
nrow(filter(df,treatment == 'playback at start')).
nrow(filter(df,treatment == 'playback at start', latency>0,latency<=10))
nrow(filter(df,treatment == 'no playback at start', latency>0,latency<=10))
nrow(filter(df,treatment == 'no playback at start'))
prop.test(c(4,50), c(50,63), p = NULL, alternative = "two.sided",correct = TRUE)

#caller proportion without playback
fig_df<-data.frame(treatment = c('playback at start','no playback at start'), proportion = c(4/60, 50/63), null_proprtion = c(4/60,75/146))
my_plot <- fig_df%>%
  ggplot(aes(x=treatment,y=proportion,fill = treatment ))+
  geom_bar(stat = "identity") +
  scale_fill_manual(breaks = waiver(),values = c("grey75","grey50")) +
  guides (fill=FALSE) +
  xlab('Treatment') +
  ylab('Call propensity') +
  theme_classic(25)+
  theme(axis.text=element_text(face='bold'),axis.title = element_text(face='bold')) 

ggsave("call_propensity_during playback.jpg", plot = my_plot, width = 12, height = 8, units = "in", dpi = 300)


#calling latency test
df1 <- filter(df, tactic==1)  #filtering out animals that did not call.
my_plot <- df1%>%
  ggplot(aes(x=treatment,y=latency, fill= treatment ))+
  geom_boxplot(size=1) +
  scale_fill_manual(breaks = waiver(),values = c("white","grey90")) +
  geom_point(position=position_jitterdodge(),alpha=1,col='black',size=2.5)+
  stat_n_text(size=10)+
  labs(fill='latency (in min)') +
  guides (fill=FALSE) +
  xlab('Treatment') +
  ylab('latency (min)') +
  theme_classic(25)+
  theme(axis.text=element_text(face='bold'),axis.title = element_text(face='bold')) 



ggsave("latency.jpg", plot = my_plot, width = 12, height = 8, units = "in", dpi = 300)
#test for difference in latency

wilcox.test(Minute..0...Didn.t.call.at.all.~treatment, data=df1)

####################################################################################################
#movement analysis
df1 <- read.csv('O.henryi_SPL_Movementt_D_DAS_ ALL_DATA_T1 .csv')
df1 <- filter(df1,TREATMENT..min.!=0)
df1$movement <- ifelse(df1$MOVEMENT_SIDE..0.L.R.!=0,1,0)
movers_without_playback <- sum(df1$movement)   #calculates 
total_trials_without_playback <- length(df1$movement)
movment_towards_speaker_without_playback <-  sum(ifelse(df1$SPEAKER_SIDE==df1$MOVEMENT_SIDE..0.L.R.,1,0))





df2 <- read.csv('O.henryi_SPL_Movementt_D_DAS_ ALL_DATA_T2.csv')

df2$movement <- ifelse(df2$MOVEMENT_SIDE..0.L.R.!=0,1,0)
movers_with_playback <- sum(df2$movement)   #calculates 
total_trials_with_playback <- length(df2$movement)
movment_towards_speaker_with_playback <-  sum(ifelse(df2$SPEAKER_SIDE==df2$MOVEMENT_SIDE..0.L.R.,1,0))

#test for difference in proportion of movers
prop.test(c(movers_without_playback,movers_with_playback), c(total_trials_without_playback ,total_trials_with_playback), p = NULL, alternative = "two.sided",
          correct = TRUE)
##test for difference in proportion of movers who moved towards the speaker
prop.test(c(movment_towards_speaker_without_playback,movment_towards_speaker_with_playback), c(total_trials_without_playback,total_trials_with_playback), p = NULL, alternative = "two.sided",
          correct = TRUE)


#dataframe for plots of movement
df1<-data.frame(treatment = c("playback at start","no playback at start"), mover_proportion = c(movers_with_playback/total_trials_with_playback,movers_without_playback/total_trials_without_playback),mover_proportion_towards_speaker = c(movment_towards_speaker_with_playback/total_trials_with_playback,movment_towards_speaker_without_playback/total_trials_without_playback))


#plot for proportion of movers
my_plot <- df1%>%
  ggplot(aes(x=treatment,y=mover_proportion,fill = treatment ))+
  geom_bar(stat = "identity") +
  scale_fill_manual(breaks = waiver(),values = c("grey75","grey50")) +
  guides (fill=FALSE) +
  xlab('Treatment') +
  ylab('proportion of movers') +
  theme_classic(25)+
  theme(axis.text=element_text(face='bold'),axis.title = element_text(face='bold')) 



ggsave("proportion_of_movers.jpg", plot = my_plot, width = 12, height = 8, units = "in", dpi = 300)

#plot for proportion of movers towards speaker
my_plot <- df1%>%
  ggplot(aes(x=treatment,y=mover_proportion_towards_speaker,fill = treatment ))+
  geom_bar(stat = "identity") +
  scale_fill_manual(breaks = waiver(),values = c("grey75","grey50")) +
  guides (fill=FALSE) +
  xlab('Treatment') +
  ylab('proportion of movers towards speaker') +
  theme_classic(25)+
  theme(axis.text=element_text(face='bold'),axis.title = element_text(face='bold')) 

ggsave("proportion_of_movers_towrds_speaker.jpg", plot = my_plot, width = 12, height = 8, units = "in", dpi = 300)


######################################################################################################







#######################change in spl plots and analysis###############

dat <- read.csv('O.henryi_SPL_Movementt_D_DAS_ ALL_DATA_T1 .csv')
dat1 <- filter (dat,MALE_SPL_PRE_EXPO!='NaN' , MALE_SPL_POST_EXPO!='NaN')


#rearranging data
spl <-c (dat1$MALE_SPL_PRE_EXPO,dat1$MALE_SPL_POST_EXPO)
id <- c(dat1$TRIAL_NO.,dat1$TRIAL_NO.)
df <- data.frame(id=id,treatment = c(dat1$TREATMENT..min.,dat1$TREATMENT..min.), spl_time = c(rep('pre_exposure',nrow(dat1)),(rep('post_exposure',nrow(dat1)))),spl=spl)
df<-na.omit(df)
df$treatment <- as.factor(df$treatment)

df <- df %>%
  mutate(treatment = recode(treatment, 
                            "0" = "control",
                            "2" = "2 min",
                            "5" = "5 min",
                            "10" = "10 min"))
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

ggsave("spl.jpg", plot = my_plot, width = 12, height = 8, units = "in", dpi = 300)

table(df$treatment,df$spl_time)


d <- filter (dat,MALE_SPL_POST_EXPO=='NaN'| MALE_SPL_PRE_EXPO=='NaN')


#overall model
res.aov <- anova_test(data = df, dv = spl, wid = id, within = spl_time, between =treatment)
get_anova_table(res.aov)

##GLM SPL Table###

model1 <- lmer(spl ~ treatment * spl_time + (1|id), data = df)
summary(model1)
confint(model1)
anova(model1)

dat3 <- data_frame("Level" = pspl$Predictor, "t-value" = pspl$t.value, "DF" = c(3,3,3,3,1,3,3,3))
dat3<- dat3 %>%
  mutate("p-value" = 2*pt(-abs(dat3$`t-value`), df = dat3$DF))

###########################################################################################################





#change in chirp rate


dat <- read.csv('O.henryi_SPL_Movementt_D_DAS_ ALL_DATA_T1 .csv')
dat$CHIRP_RATE_PRE_EXPO_3min[dat$TRIAL_NO.==95] <- dat$CHIRP_RATE_PRE_EXPO_8min[dat$TRIAL_NO.==95]   #using chirp rate measured at 3 min.
df <- filter (dat,CHIRP_RATE_PRE_EXPO_3min != 'NaN')

sd(df$CHIRP_RATE_PRE_EXPO_3min)/mean(df$CHIRP_RATE_PRE_EXPO_3min)   #coefficient of variation for chirp_rate measured at 3 min. This was comaprable to the cv at 5 and 8 mins. Also, chirp_rate at 3 mins has the most sample points.




dat <- filter (dat,CHIRP_RATE_PRE_EXPO_3min != 'NaN',CHIRP_RATE_POST_EXPO != 'NaN')
chirp_rate <-c (dat$CHIRP_RATE_PRE_EXPO_3min,dat$CHIRP_RATE_POST_EXPO)




df <- data.frame(treatment = c(dat$TREATMENT..min.,dat$TREATMENT..min.),id = c(dat$TRIAL_NO.,dat$TRIAL_NO.), time_of_measurement = c(rep('before_exposure',nrow(dat)),(rep('post_exposure',nrow(dat)))),chirp_rate=chirp_rate)
df$treatment <- as.factor(df$treatment)
df$id <-as.factor(df$id)
df$time_of_measurement <-as.factor(df$time_of_measurement)
df<-na.omit(df)

df <- df %>%
  mutate(treatment = recode(treatment, 
                            "0" = "control",
                            "2" = "2 min",
                            "5" = "5 min",
                            "10" = "10 min"))


my_plot <- df%>%
  ggplot(aes(x=treatment,y=chirp_rate, fill=factor(time_of_measurement)))+
  geom_boxplot() +
  scale_fill_manual(breaks = waiver(),values = c("white","grey70")) +
  geom_point(position=position_jitterdodge(),alpha=1,col='black') +
  #scale_y_continuous(breaks=c(0,0.1,0.5,1,1.5,2)) +
  # scale_y_break(c(1.05, 1.9)) +
  labs(fill='') +
  xlab('Playback duration (min)') +
  ylab('Chirp rate (chirps per second)') +
  theme_classic(base_size = 25)+
  theme(axis.text=element_text(face='bold'),axis.title = element_text(face='bold')) 

ggsave("chirp_rate.jpg", plot = my_plot, width = 12, height = 8, units = "in", dpi = 300)

table(df$treatment,df$time_of_measurement)

#test for normality
shapiro.test(df$chirp_rate)
hist(df$chirp_rate)


#test for difference in chirp rates

df %>%
  group_by(treatment, time_of_measurement) %>%
  get_summary_stats(chirp_rate, type = "mean_sd")

df %>%
  group_by(treatment, time_of_measurement) %>%identify_outliers(chirp_rate)

df %>%
  group_by(treatment, time_of_measurement) %>%shapiro_test(chirp_rate)


ggqqplot(df, "chirp_rate", ggtheme = theme_bw()) +
  facet_grid(time_of_measurement ~ treatment, labeller = "label_both")

#overall m0del
res.anova <- anova_test(
  data = df, dv = chirp_rate, wid = id,
  within = c( time_of_measurement),between = c(treatment))
get_anova_table(res.anova)

###GLM Table CR###
confint(res.anova)
anova(res.anova)
y<- glm(chirp_rate~time_of_measurement+treatment+time_of_measurement:treatment, family = gaussian, df)
summary(y)

model <- lmer(chirp_rate ~ treatment * time_of_measurement + (1|id), data = df)
summary(model)
anova(model)
confint(model)

#difference between treatments
one.way <- df %>%
  group_by(time_of_measurement) %>%
  anova_test(dv = chirp_rate, wid = id, between = treatment) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

# Pairwise comparisons between time points
one.way2 <- df %>%
  group_by(treatment) %>%
  anova_test(dv = chirp_rate, wid = id, within = time_of_measurement) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way2
summary(one.way2)

######################################################################################################

###############################tes for difference in chirp rate between time points
#supplementary analyses for change in chirp rate with time

dat <- read.csv('O.henryi_SPL_Movementt_D_DAS_ ALL_DATA_T1 .csv')

dat<-filter(dat,CHIRP_RATE_PRE_EXPO_3min!='NaN',CHIRP_RATE_PRE_EXPO_5min!='NaN',CHIRP_RATE_PRE_EXPO_8min!='NaN')

chirp_rate <- c(dat$CHIRP_RATE_PRE_EXPO_3min,dat$CHIRP_RATE_PRE_EXPO_5min,dat$CHIRP_RATE_PRE_EXPO_8min)
df <- data.frame(treatment = rep(dat$TREATMENT..min.,3),id = rep(dat$TRIAL_NO.,3), time_of_measurement = c(rep(3,nrow(dat)),rep(5,nrow(dat)),rep(8,nrow(dat))),chirp_rate=chirp_rate)

df$time_of_measurement<-as.factor(df$time_of_measurement)
my_plot <- df%>%
  ggplot(aes(x=time_of_measurement,y=chirp_rate, fill=factor(time_of_measurement)))+
  geom_boxplot() +
  scale_fill_manual(breaks = waiver(),values = c("white","grey70", 'grey90')) +
  geom_point(position=position_jitterdodge(),alpha=1,col='black') +
  #scale_y_continuous(breaks=c(0,0.1,0.5,1,1.5,2)) +
  # scale_y_break(c(1.05, 1.9)) +
  labs(fill='') +
  xlab('Time of measurement(min)') +
  ylab('Chirp rate(chirps per second)') +
  theme_classic(base_size = 25)

ggsave("chirp_rate_supplementary.jpg", plot = my_plot, width = 12, height = 8, units = "in", dpi = 300)


dat_n <- filter(df,time_of_measurement!=3)

#### wilcox.test(chirp_rate~time_of_measurement,data=dat_n,paired=TRUE)

res.anova <- anova_test(
  data = df, dv = chirp_rate, wid = id,
  within = c( time_of_measurement))
get_anova_table(res.anova)

pwc <- df %>%
  pairwise_t_test(
    chirp_rate ~ time_of_measurement, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc



dat_n <- filter(df,treatment==0)
wilcox.test(chirp_rate~time_of_measurement,data=dat_n,paired=TRUE)

dat_n <- filter(df,treatment==2)
wilcox.test(chirp_rate~time_of_measurement,data=dat_n,paired=TRUE)

dat_n <- filter(df,treatment==5)
wilcox.test(chirp_rate~time_of_measurement,data=dat_n,paired=TRUE)

dat_n <- filter(df,treatment==10)
wilcox.test(chirp_rate~time_of_measurement,data=dat_n,paired=TRUE)


######################################################################################################






#################test for call effort############################
df <-read.csv('Calling_effort.csv')
na.omit(df)
df_new <- data.frame (treatment = c(rep(df$Treatment,2)), id = c(rep(df$Audi_id,2)),exposure_time = c(rep('before exposure',nrow(df)),rep('post exposure',nrow(df))), call_effort = c(df$Calling_effort_pre_expo.proportion.,df$Calling_effort_post.proportion.))


skewness(df_new$call_effort) #skew of call effort
n <- length(df_new$call_effort)
df_new$transformed_call_effort <- (df_new$call_effort*(n-1)+0.5)/n

# Fit the model
model <- glmmTMB(
  transformed_call_effort ~ treatment * exposure_time + (1|id),
  data = df_new,
  family = beta_family()
)
summary(model)
confint(model)
Anova(model,type = "III") 

df_new <- df_new %>%
  mutate(treatment = recode(treatment, 
                            "CONTROL.WAV" = "control",
                            "2min.WAV" = "2 min",
                            "5min.WAV" = "5 min",
                            "10min.WAV" = "10 min"))

my_plot <- df_new%>%
  mutate(treatment = factor(treatment, levels = c("control", "2 min", "5 min", "10 min"))) %>%
  ggplot(aes(x=treatment,y=call_effort, fill= exposure_time ))+
  geom_boxplot() +
  scale_fill_manual(breaks = waiver(),values = c("white","grey75")) +
  geom_point(position=position_jitterdodge(),alpha=1,col='black')+
  #stat_n_text(size=10)+
  labs(fill='Call effort') +
  labs (fill='') +
  xlab('Playback duration (min)') +
  ylab('Call effort') +
  theme_classic(25)+
  theme(axis.text=element_text(face='bold'),axis.title = element_text(face='bold')) 

ggsave("call_effort.jpg", plot = my_plot, width = 12, height = 8, units = "in", dpi = 300)


##################################################################################################


#####################call energetics and decision making#############

#####################spl rate######################
df <- read.csv('Pre_Expo_SPL_CR3.csv')
df <- df %>%filter(if_any(everything(), ~ !is.na(.) & . != ""))
wilcox.test(df$MALE_SPL_PRE_EXPO~df$Treatment) #non-parametric
t.test(df$MALE_SPL_PRE_EXPO~df$Treatment) #parametric

#(non)parametric tests for chirp rate
wilcox.test(df$CHIRP_RATE_PRE_EXPO_3min~df$Treatment) #non-parametric
t.test(df$CHIRP_RATE_PRE_EXPO_3min~df$Treatment) #parametric

my_plot <- df%>%
  ggplot(aes(x=Treatment,y=MALE_SPL_PRE_EXPO, fill= Treatment))+
  geom_boxplot() +
  scale_fill_manual(breaks = waiver(),values = c("white","grey75")) +
  geom_point(position=position_jitterdodge(),alpha=1,col='black')+
  labs(fill='Treatment') +
  labs (fill='') +
  xlab('Treatment') +
  ylab('SPL (dB)') +
  guides (fill=FALSE) +
  theme_classic(25)+
  theme(axis.text=element_text(face='bold'),axis.title = element_text(face='bold')) 

ggsave("SPL_playback_vs_noplayback.jpg", plot = my_plot, width = 12, height = 8, units = "in", dpi = 300)


my_plot <- df%>%
  ggplot(aes(x=Treatment,y=CHIRP_RATE_PRE_EXPO_3min, fill= Treatment))+
  geom_boxplot() +
  scale_fill_manual(breaks = waiver(),values = c("white","grey75")) +
  geom_point(position=position_jitterdodge(),alpha=1,col='black')+
  labs(fill='Treatment') +
  labs (fill='') +
  xlab('Treatment') +
  ylab('Chirp rate(chirps per second)') +
  guides (fill=FALSE) +
  theme_classic(25)+
  theme(axis.text=element_text(face='bold'),axis.title = element_text(face='bold')) 

ggsave("chirp_rate_playback_vs_noplayback.jpg", plot = my_plot, width = 12, height = 8, units = "in", dpi = 300)

#################################################################################################################
                                          #Supplementary Ananlysis#
#################################################################################################################

#Supplementary SPL Pilot#
dat<-read.csv('Piolt_SPL.csv')


dat1<- wilcox.test(dat$MALE_SPL_PRE_EXPO,dat$MALE_SPL_POST_EXPO, paired = TRUE)
t.test(dat$MALE_SPL_PRE_EXPO,dat$MALE_SPL_POST_EXPO, paired = TRUE)

#rearranging data
spl <-c (dat$MALE_SPL_PRE_EXPO,dat$MALE_SPL_POST_EXPO)c
id <- c(dat$TRIAL_NO.,dat$TRIAL_NO.)
df <- data.frame(id=id,treatment = c(dat$TREATMENT..min.,dat$TREATMENT..min.), spl_time = c(rep('Before exposure',nrow(dat)),(rep('Post exposure',nrow(dat)))),spl= spl)
df<-na.omit(df)
df$treatment <- as.factor(df$treatment)

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

###############correlation between body size and postive phonotaxis###########

df <-read.csv('Bodysize+movement.csv')
df<-na.omit(df)
nrow(df)
model <- glm(Moved.Towards~Body.size, family = binomial, data=df)
summary(model)

my_plot <- df %>%
  ggplot(aes(x = Body.size, y = Moved.Towards)) +
  geom_point(position = position_jitter(width = 0.1, height = 0), alpha = 1, col = 'black') +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE,linetype = "dashed", color = 'black') +
  xlab('Body size (mm)') +
  ylab('Movement towards the speaker') +
  theme_classic(25) +
  theme(axis.text = element_text(face = 'bold'), axis.title = element_text(face = 'bold'))

ggsave("body_size_postive _phonotaxis.jpg", plot = my_plot, width = 12, height = 8, units = "in", dpi = 300)

#test for movement males after male release
df1 <- read.csv('Movement_proportion_null.csv')
my_plot <- df1%>%
  ggplot(aes(x=Treatement,y=Proportion.of.movers,fill = Treatement ))+
  geom_bar(stat = "identity") +
  scale_fill_manual(breaks = waiver(),values = c("grey75","grey50")) +
  guides (fill=FALSE) +
  xlab('Treatment') +
  ylab('proportion of movers') +
  theme_classic(25)+
  theme(axis.text=element_text(face='bold'),axis.title = element_text(face='bold')) 

ggsave("proportion of movers null.jpg", plot = my_plot, width = 12, height = 8, units = "in", dpi = 300)

#Statistical test for the above plot
prop.test(c(33,31), c(120,60), p = NULL, alternative = "two.sided",
          correct = TRUE)

#Null call propensity 

df <- read.csv('Latency + Binary call data.csv')
fig_df<-data.frame(treatment = c('playback at start','no playback at start'), null_proprtion = c(4/60,75/146))

my_plot <- fig_df%>%
  ggplot(aes(x=treatment,y=null_proprtion,fill = treatment ))+
  geom_bar(stat = "identity") +
  scale_fill_manual(breaks = waiver(),values = c("grey75","grey50")) +
  guides (fill=FALSE) +
  xlab('Treatment') +
  ylab('Call propensity') +
  theme_classic(25)+
  theme(axis.text=element_text(face='bold'),axis.title = element_text(face='bold')) 

ggsave("call_propensity_null.jpg", plot = my_plot, width = 12, height = 8, units = "in", dpi = 300)

