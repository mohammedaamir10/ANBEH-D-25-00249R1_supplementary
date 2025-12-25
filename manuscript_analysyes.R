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
library(rstudioapi)

#set to current work directory where the data and the code is stored
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#################################################################################################################
                                        #Main Manuscript Analysis and plots#
#################################################################################################################

#################################################################################################################
                                  #Calling latency and caller proportion analysis and plots


#latency of callers and caller proportion during playback
df <- read.csv('Latency + Binary call data.csv')

#Figure 3a with analyses
#Figure 3a.Analysis of calling latency
df1 <- dplyr::filter(df, call_no_call==1)  #filtering out animals that did not call.
#Wilcox test for difference in latency
wilcox.test(latency ~treatment, data=df1)
sample_size_playback_at_start <- nrow(dplyr::filter (df1,treatment=='playback_at_start'))
sample_size_no_playback_at_start <- nrow(dplyr::filter (df1,treatment=='no_playback_at_start'))


sample_sizes <- data.frame(
  treatment = c("playback at start", "no playback at start"),
  y = -0.05,
  label = I(list(
    bquote(italic(N) == .(sample_size_playback_at_start)),
    bquote(italic(N) == .(sample_size_no_playback_at_start))
  ))
)

#plot for calling latency
df1 <- df1 %>%
  mutate(treatment = as.character(treatment)) %>%
  mutate(treatment = dplyr::recode(treatment,
    "playback_at_start"    = "playback at start",
    "no_playback_at_start" = "no playback at start"
  ))

my_plot <- df1%>%
  ggplot(aes(x=treatment,y=latency, fill= treatment))+
  geom_boxplot(size=1,coef = Inf, outlier.shape = NA) +
  stat_boxplot(
    geom = "errorbar",
     coef = Inf,
    linewidth = 1,
    width = 0.5
  ) +
  scale_fill_manual(breaks = waiver(),values = c("white","grey90")) +
  geom_point(position=position_jitterdodge(),alpha=1,col='black',size=2.5)+
  #stat_n_text(size=10)+
  labs(fill='latency (in min)') +
  guides (fill=FALSE) +
  xlab('Treatment') +
  ylab('latency (min)') +
  theme_classic(25)+
  theme(axis.text=element_text(face='bold'),axis.title = element_text(face='bold')) 
my_plot <- my_plot +
  geom_text(
    data = sample_sizes,
    aes(x = treatment, y = y, label = label),
    inherit.aes = FALSE,
    size = 7,
    parse = TRUE 
  )
my_plot <- my_plot +
  annotate(
    "text",
    x = -Inf, y = Inf,
    label = "a)",
    hjust = -0.2, vjust = 1.2,
    fontface = "bold",
    size = 10
  )

ggsave("fig_3_a.jpg", plot = my_plot, width = 12, height = 8, units = "in", dpi = 300)




#Figure 3b.
#Experiment T1 (playback at start).
#caller porportion during playback
callers_t1 <- nrow(dplyr::filter(df,treatment == 'playback_at_start', latency>0,latency<=10))
#number of of total trials in Experiment T1.
trials_t1 <-nrow(dplyr::filter(df,treatment == 'playback_at_start'))

#Experiment T2 (no playback at start).
df_2 <- read.csv('O.henryi_SPL_MovementtALL_DATA_T2.csv')
#caller porportion during playback for trials with 2,5,10 mins of playback
callers_t2 <- nrow(dplyr::filter(df_2,TREATMENT..min.> 0, Call_NOCALL_DURING.EXPO == 1))
#number of of total trials in Experiment T2 with 2,5,10 mins of playback
trials_t2 <- nrow(dplyr::filter(df_2,TREATMENT..min.> 0))
prop.test(c(callers_t1,callers_t2), c(trials_t1,trials_t2), p = NULL, alternative = "two.sided",correct = TRUE)

#caller proportion without playback
fig_df<-data.frame(treatment = c('playback at start','no playback at start'), proportion = c(callers_t1/trials_t1, callers_t2/trials_t2) , label = c(paste0(callers_t1, '/', trials_t1), paste0(callers_t2, '/', trials_t2)))
my_plot <- fig_df%>%
  ggplot(aes(x=treatment,y=proportion,fill = treatment ))+
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = label),
    vjust = 1.3,          # moves text inside bar
    fontface = "bold",
    color = "black",
    size = 10
    ) +
  scale_x_discrete(labels = c(
    playback_at_start     = "Playback at start",
    no_playback_at_start  = "No playback at start"
  )) +
  scale_fill_manual(breaks = waiver(),values = c("grey75","grey50")) +
  scale_y_continuous(
    labels = function(x) ifelse(x == 0, "0", x)
  ) +
  guides (fill=FALSE) +
  xlab('Treatment') +
  ylab('Call propensity') +
  theme_classic(25)+
  theme(axis.text=element_text(face='bold'),axis.title = element_text(face='bold')) 
my_plot <- my_plot +
  annotate(
    "text",
    x = -Inf, y = Inf,
    label = "b)",
    hjust = -0.2, vjust = 1.2,
    fontface = "bold",
    size = 10
  )
ggsave("fig_3_b.jpg", plot = my_plot, width = 12, height = 8, units = "in", dpi = 300)




#################################################################################################################
                                  #Movment analysis and plots

#Experiment T2 (no playback at start).
#dataset for trials with no playback at start
df1 <- read.csv('O.henryi_SPL_MovementtALL_DATA_T2.csv')
#filter data to include trials with playback of 2,5,10 mins
df1 <- filter(df1,TREATMENT..min.!=0)
#Classification of movers and non-movers in Experiment T2 for 2,5,10 mins of playback
df1$movement <- ifelse(df1$MOVEMENT_SIDE..0.L.R.!=0,1,0)
#number of movers in Experiment T2 for 2,5,10 mins of playback
movers_without_playback <- sum(df1$movement)
#total number of trials in Experiment T2 for 2,5,10 mins of playback
total_trials_without_playback <- length(df1$movement)
#number of movers who moved towards the speaker in Experiment T2 for 2,5,10 mins of playback
movment_towards_speaker_without_playback <-  sum(ifelse(df1$SPEAKER_SIDE==df1$MOVEMENT_SIDE..0.L.R.,1,0))




#Experiment T1 (with playback at start).
df2 <- read.csv('O.henryi_SPL_MovementtALL_DATA_T1.csv')

df2$movement <- ifelse(df2$MOVEMENT_SIDE..0.L.R.!=0,1,0)
movers_with_playback <- sum(df2$movement)   #calculates 
total_trials_with_playback <- length(df2$movement)
movment_towards_speaker_with_playback <-  sum(ifelse(df2$SPEAKER_SIDE==df2$MOVEMENT_SIDE..0.L.R.,1,0))

#test for difference in proportion of movers between Experiment T1 and T2
prop.test(c(movers_without_playback,movers_with_playback), c(total_trials_without_playback ,total_trials_with_playback), p = NULL, alternative = "two.sided",
          correct = TRUE)
##test for difference in proportion of movers who moved towards the speaker between Experiment T1 and T2
prop.test(c(movment_towards_speaker_without_playback,movment_towards_speaker_with_playback), c(total_trials_without_playback,total_trials_with_playback), p = NULL, alternative = "two.sided",
          correct = TRUE)


#dataframe for plots of movement
df_plot<-data.frame(treatment = c("playback at start","no playback at start"), mover_proportion = c(movers_with_playback/total_trials_with_playback,movers_without_playback/total_trials_without_playback), label = c(paste0(movers_with_playback, '/', total_trials_with_playback), paste0( movers_without_playback, '/', total_trials_without_playback)))


#Fig_4a. plot for proportion of movers
my_plot <- df_plot%>%
  ggplot(aes(x=treatment,y=mover_proportion,fill = treatment ))+
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = label),
    vjust = 1.3,          # moves text inside bar
    fontface = "bold",
    color = "black",
    size = 10
    ) +
  scale_x_discrete(labels = c(
    playback_at_start     = "Playback at start",
    no_playback_at_start  = "No playback at start"
  )) +
  scale_fill_manual(breaks = waiver(),values = c("grey75","grey50")) +
    scale_y_continuous(
    labels = function(x) ifelse(x == 0, "0", x)
  ) +
  guides (fill=FALSE) +
  xlab('Treatment') +
  ylab('proportion of movers') +
  theme_classic(25)+
  theme(axis.text=element_text(face='bold'),axis.title = element_text(face='bold')) 
my_plot <- my_plot +
  annotate(
    "text",
    x = -Inf, y = Inf,
    label = "a)",
    hjust = -0.2, vjust = 1.2,
    fontface = "bold",
    size = 10
  )
ggsave("fig_4_a.jpg", plot = my_plot, width = 12, height = 8, units = "in", dpi = 300)

#Fig_4b.plot for proportion of movers towards speaker
df_plot<-data.frame(treatment = c("playback at start","no playback at start"), mover_proportion_towards_speaker = c(movment_towards_speaker_with_playback/total_trials_with_playback,movment_towards_speaker_without_playback/total_trials_without_playback), label = c(paste0(movment_towards_speaker_with_playback, '/', total_trials_with_playback), paste0( movment_towards_speaker_without_playback, '/', total_trials_without_playback)))
my_plot <- df_plot%>%
  ggplot(aes(x=treatment,y=mover_proportion_towards_speaker,fill = treatment ))+
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = label),
    vjust = 1.3,          # moves text inside bar
    fontface = "bold",
    color = "black",
    size = 10
    ) +
  scale_x_discrete(labels = c(
    playback_at_start     = "Playback at start",
    no_playback_at_start  = "No playback at start"
  )) +
  scale_fill_manual(breaks = waiver(),values = c("grey75","grey50")) +
    scale_y_continuous(
    labels = function(x) ifelse(x == 0, "0", x)
  ) +
  scale_fill_manual(breaks = waiver(),values = c("grey75","grey50")) +
  guides (fill=FALSE) +
  xlab('Treatment') +
  ylab('proportion of movers towards speaker') +
  theme_classic(25)+
  theme(axis.text=element_text(face='bold'),axis.title = element_text(face='bold'))
my_plot <- my_plot +
  annotate(
    "text",
    x = -Inf, y = Inf,
    label = "b)",
    hjust = -0.2, vjust = 1.2,
    fontface = "bold",
    size = 10
  )

ggsave("fig_4_b.jpg", plot = my_plot, width = 12, height = 8, units = "in", dpi = 300)





#################################################################################################################
                                  #SPL and chirp rate analysis with plots
#Analysis and plots for Figure 5a 
#Non-parametric tests for SPL difference between Experiment T1 and T2
df <- read.csv('Pre_Expo_SPL_CR3.csv')
df_spl <- df %>%
  filter(!is.na(MALE_SPL.dB.)) %>%
  select(MALE_SPL.dB., Treatment)
wilcox.test(df$MALE_SPL.dB.~df$Treatment) #non-parametric

#Figure 5a. 

sample_size_playback_at_start <- nrow(dplyr::filter (df_spl,Treatment=='playback at start'))
sample_size_no_playback_at_start <- nrow(dplyr::filter (df_spl,Treatment=='no playback at start'))

sample_sizes <- data.frame(
  treatment = c("playback at start", "no playback at start"),
  y = 54,
  label = I(list(
    bquote(italic(N) == .(sample_size_playback_at_start)),
    bquote(italic(N) == .(sample_size_no_playback_at_start))
  ))
)

my_plot <- df_spl%>%
  ggplot(aes(x=Treatment,y=MALE_SPL.dB., fill= Treatment))+
  geom_boxplot(size=1,coef = Inf, outlier.shape = NA) +
  stat_boxplot(
    geom = "errorbar",
     coef = Inf,
    linewidth = 1,
    width = 0.5
  ) +
  scale_fill_manual(breaks = waiver(),values = c("white","grey90")) +
  geom_point(position=position_jitterdodge(),alpha=1,col='black',size=2.5)+
  #stat_n_text(size=10)+
  labs(fill='SPL (dB)') +
  guides (fill=FALSE) +
  xlab('Treatment') +
  ylab('SPL (dB)') +
  theme_classic(25)+
  theme(axis.text=element_text(face='bold'),axis.title = element_text(face='bold')) 
my_plot <- my_plot +
  geom_text(
    data = sample_sizes,
    aes(x = treatment, y = y, label = label),
    inherit.aes = FALSE,
    size = 7,
    parse = TRUE 
  )
my_plot <- my_plot +
  annotate(
    "text",
    x = -Inf, y = Inf,
    label = "a)",
    hjust = -0.2, vjust = 1.2,
    fontface = "bold",
    size = 10
  )

ggsave("fig_5_a.jpg", plot = my_plot, width = 12, height = 8, units = "in", dpi = 300)


#Analysis and plot for Figure 5b 
#Non-parametric tests for chirp rate difference between Experiment T1 and T2
df_chirp_rate <- df %>%
  filter(!is.na(CHIRP_RATE)) %>%
  select(CHIRP_RATE, Treatment)
wilcox.test(df$CHIRP_RATE~df$Treatment) #non-parametric


#Figure 5b.
sample_size_playback_at_start <- nrow(dplyr::filter (df_chirp_rate,Treatment=='playback at start'))
sample_size_no_playback_at_start <- nrow(dplyr::filter (df_chirp_rate,Treatment=='no playback at start'))

sample_sizes <- data.frame(
  treatment = c("playback at start", "no playback at start"),
  y = 1,
  label = I(list(
    bquote(italic(N) == .(sample_size_playback_at_start)),
    bquote(italic(N) == .(sample_size_no_playback_at_start))
  ))
)
my_plot <- df_chirp_rate%>%
  ggplot(aes(x=Treatment,y=CHIRP_RATE, fill= Treatment))+
  geom_boxplot(size=1,coef = Inf, outlier.shape = NA) +
  stat_boxplot(
    geom = "errorbar",
     coef = Inf,
    linewidth = 1,
    width = 0.5
  ) +
  scale_fill_manual(breaks = waiver(),values = c("white","grey90")) +
  scale_y_continuous(
  labels = function(x) ifelse(x == 2, "2", x)
  ) +
  geom_point(position=position_jitterdodge(),alpha=1,col='black',size=2.5)+
  #stat_n_text(size=10)+
  labs(fill='Chirps / second') +
  guides (fill=FALSE) +
  xlab('Treatment') +
  ylab('Chirps / second') +
  theme_classic(25)+
  theme(axis.text=element_text(face='bold'),axis.title = element_text(face='bold')) 
my_plot <- my_plot +
  geom_text(
    data = sample_sizes,
    aes(x = treatment, y = y, label = label),
    inherit.aes = FALSE,
    size = 7,
    parse = TRUE 
  )
my_plot <- my_plot +
  annotate(
    "text",
    x = -Inf, y = Inf,
    label = "b)",
    hjust = -0.2, vjust = 1.2,
    fontface = "bold",
    size = 10
  )
ggsave("fig_5_b.jpg", plot = my_plot, width = 12, height = 8, units = "in", dpi = 300)





####################################################################################################################
# Change in SPL:analyses with plots
dat <- read.csv('O.henryi_SPL_MovementtALL_DATA_T2.csv')
dat1 <- dplyr::filter (dat,MALE_SPL_PRE_EXPO.dB. !='NaN' , MALE_SPL_POST_EXPO.dB. !='NaN')


#rearranging data
spl <-c (dat1$MALE_SPL_PRE_EXPO.dB. ,dat1$MALE_SPL_POST_EXPO.dB. )
id <- c(dat1$TRIAL_NO.,dat1$TRIAL_NO.)
df <- data.frame(id=id,treatment = c(dat1$TREATMENT..min.,dat1$TREATMENT..min.), spl_time = c(rep('before exposure',nrow(dat1)),(rep('post exposure',nrow(dat1)))),spl=spl)
df<-na.omit(df)
df$treatment <- as.factor(df$treatment)


#Figure 6a. plot for change in SPL with exposure
df <- df %>%
  mutate(treatment = dplyr::recode(treatment, 
                            "0" = "control",
                            "2" = "2 min",
                            "5" = "5 min",
                            "10" = "10 min"))

sample_sizes <- df %>%
dplyr::count(treatment) %>%
dplyr::mutate(
  n = n / 2,
  y = 54.5,  # just below your lower y-limit (54)
  label = paste0("italic(N)==", n)
)
pd <- position_dodge(width = 0.75)


my_plot <- df%>%
  ggplot(aes(x=treatment,y=spl, fill=factor(spl_time)))+
  geom_boxplot(size=1,coef = Inf, outlier.shape = NA,linewidth = 2) +
  stat_boxplot(
    geom = "errorbar",
     coef = Inf,
    linewidth = 2,
    width = 0.5,
    position = pd 
  ) +
  scale_fill_manual(breaks = waiver(),values = c("white","grey70")) +
  geom_point(position=position_jitterdodge(),alpha=1,col='black',size=4)+
  labs(fill='') +
  xlab('') +
  ylab('SPL (dB)') +
  guides(fill = guide_legend(override.aes = list(linewidth = 0.5,size=0.5))) +
  scale_y_continuous(breaks = seq(54, 66, 4), limits = c(54,68),expand = expansion(mult = c(0, 0.02))) +
  theme_classic(base_size = 30) +
  theme(axis.text=element_text(face='bold'),axis.title = element_text(face='bold'))
my_plot <- my_plot +
  geom_text(
    data = sample_sizes,
    aes(x = treatment, y = y, label = label),
    inherit.aes = FALSE,
    size = 7,
    parse = TRUE
  )
my_plot <- my_plot +
  annotate(
    "text",
    x = -Inf, y = Inf,
    label = "a)",
    hjust = -0.2, vjust = 1.2,
    fontface = "bold",
    size = 10
  )

#saving Figure 6a
p1 <- my_plot

##GLM SPL Table 1

model1 <- lmer(spl ~ treatment * spl_time + (1|id), data = df)
summary(model1)
confint(model1)
anova(model1)
Anova(model1,type = "III") 


###########################################################################################################

## Change in chirp_rate:analyses with plots

dat <- read.csv('O.henryi_SPL_MovementtALL_DATA_T2.csv')
dat <- dplyr::filter (dat,CHIRP_RATE_PRE_EXPO_3min.s. != 'NaN',CHIRP_RATE_POST_EXPO.s. != 'NaN')

chirp_rate <-c (dat$CHIRP_RATE_PRE_EXPO_3min.s.,dat$CHIRP_RATE_POST_EXPO.s.)


df <- data.frame(treatment = c(dat$TREATMENT..min.,dat$TREATMENT..min.),id = c(dat$TRIAL_NO.,dat$TRIAL_NO.), time_of_measurement = c(rep('before exposure',nrow(dat)),(rep('post exposure',nrow(dat)))),chirp_rate=chirp_rate)
df$treatment <- as.factor(df$treatment)
df$id <-as.factor(df$id)
df$time_of_measurement <-as.factor(df$time_of_measurement)
df<-na.omit(df)


#Figure 6b. plot for change in chirp_rate with exposure
df <- df %>%
  mutate(treatment = dplyr::recode(treatment,
                            "0" = "control",
                            "2" = "2 min",
                            "5" = "5 min",
                            "10" = "10 min"))

sample_sizes <- df %>%
dplyr::count(treatment) %>%
dplyr::mutate(
  n = n / 2,
  y = 1.2,  # just below your lower y-limit (54)
  label = paste0("italic(N)==", n)
)
pd <- position_dodge(width = 0.75)


my_plot <- df%>%
  ggplot(aes(x=treatment,y=chirp_rate, fill=factor(time_of_measurement)))+
  geom_boxplot(size=1,coef = Inf, outlier.shape = NA,linewidth = 2) +
  stat_boxplot(
    geom = "errorbar",
     coef = Inf,
    linewidth = 2,
    width = 0.5,
    position = pd 
  ) +
  scale_fill_manual(breaks = waiver(),values = c("white","grey70")) +
  geom_point(position=position_jitterdodge(),alpha=1,col='black',size=4)+
  labs(fill='') +
  scale_y_continuous(
    labels = function(x) ifelse(x == 2, "2", x)
  ) +
  guides(fill = guide_legend(override.aes = list(linewidth = 0.5,size=0.5))) +
  xlab('') +
  ylab('Chirps / second') +
  theme_classic(base_size = 30) +
  theme(axis.text=element_text(face='bold'),axis.title = element_text(face='bold'))
my_plot <- my_plot +
  geom_text(
    data = sample_sizes,
    aes(x = treatment, y = y, label = label),
    inherit.aes = FALSE,
    size = 7,
    parse = TRUE
  )
my_plot <- my_plot +
  annotate(
    "text",
    x = -Inf, y = Inf,
    label = "b)",
    hjust = -0.2, vjust = 1.2,
    fontface = "bold",
    size = 10
  )

#Saving Figure 6b
p2 <- my_plot



#test for difference in chirp rates

model <- lmer(chirp_rate ~ treatment * time_of_measurement + (1|id), data = df)
summary(model)
confint(model)
anova(model)
Anova(model,type = "III") 


######################################################################################################

#################test for call effort############################
df <-read.csv('Calling_effort.csv')
na.omit(df)
df_new <- data.frame (treatment = c(rep(df$Treatment,2)), id = c(rep(df$Audi_id,2)),exposure_time = c(rep('before exposure',nrow(df)),rep('post exposure',nrow(df))), call_effort = c(df$Calling_effort_pre_expo.proportion.,df$Calling_effort_post.proportion.))


skewness(df_new$call_effort) #skew of call effort
n <- length(df_new$call_effort)
df_new$transformed_call_effort <- (df_new$call_effort*(n-1)+0.5)/n
df_new <- df_new %>%
  mutate(
    treatment = factor(
      dplyr::recode(
        treatment,
        "CONTROL.WAV" = "control",
        "2min.WAV"    = "2 min",
        "5min.WAV"    = "5 min",
        "10min.WAV"   = "10 min"
      ),
      levels = c("control", "2 min", "5 min", "10 min")
    )
  )
# Fit the model
model <- glmmTMB(
  transformed_call_effort ~ treatment * exposure_time + (1|id),
  data = df_new,
  family = beta_family()
)
summary(model)
confint(model)
Anova(model,type = "III") 


sample_sizes <- df_new %>%
dplyr::count(treatment) %>%
dplyr::mutate(
  n = n / 2,
  y = -0.05,  # just below your lower y-limit (54)
  label = paste0("italic(N)==", n)
)
pd <- position_dodge(width = 0.75)


my_plot <- df_new%>%
  ggplot(aes(x=treatment,y=call_effort, fill=factor(exposure_time)))+
  geom_boxplot(size=1,coef = Inf, outlier.shape = NA,linewidth = 2) +
  stat_boxplot(
    geom = "errorbar",
     coef = Inf,
    linewidth = 2,
    width = 0.5,
    position = pd 
  ) +
  scale_fill_manual(breaks = waiver(),values = c("white","grey70")) +
  geom_point(position=position_jitterdodge(),alpha=1,col='black',size=4)+
  labs(fill='') +
  xlab('Exposure duration (min)') +
  ylab('Call effort') +
  scale_y_continuous(
  labels = function(x) ifelse(x %in% c(0, 1), as.character(x), x)
) +
guides(fill = guide_legend(override.aes = list(linewidth = 0.5,size=0.5))) +
  theme_classic(base_size = 30) +
  theme(axis.text=element_text(face='bold'),axis.title = element_text(face='bold'))
my_plot <- my_plot +
  geom_text(
    data = sample_sizes,
    aes(x = treatment, y = y, label = label),
    inherit.aes = FALSE,
    size = 7,
    parse = TRUE
  )
my_plot <- my_plot +
  annotate(
    "text",
    x = -Inf, y = Inf,
    label = "c)",
    hjust = -0.2, vjust = 1.2,
    fontface = "bold",
    size = 10
  )
#Saving Figure 6c
p3 <-my_plot
#ggsave("fig_6_c.jpg", plot = my_plot, width = 12, height = 8, units = "in", dpi = 300)
final_plot <- p1 / p2 / p3 + plot_layout(heights = c(1,1,1))

ggsave("fig_6.jpg", plot = final_plot, width = 24, height = 16, units = "in", dpi = 300)

##################################################################################################






######################################################################################################
                                  #Supplementary Ananlysis#
##################################################################################################################                                       

#Figure out the relevant dataset and then plot
#Supplementary Figure 1
dat <- read.csv('O.henryi_SPL_MovementtALL_DATA_T2.csv')
#Subseeting control data
SPL_control <- dat%>% filter (TREATMENT..min. == "0",MALE_SPL_PRE_EXPO.dB. !='NaN', MALE_SPL_POST_EXPO.dB. !='NaN')

#shapiro test for the normality
shapiro.test(SPL_control$MALE_SPL_PRE_EXPO.dB.)
shapiro.test(SPL_control$MALE_SPL_POST_EXPO.dB.)
#paired t-test
t.test(SPL_control$MALE_SPL_PRE_EXPO.dB.,SPL_control$MALE_SPL_POST_EXPO.dB., paired = TRUE)

#rearranging data
sample_zize_5mins <- length (SPL_control$MALE_SPL_PRE_EXPO.dB.)
sample_size_20mins <- length(SPL_control$MALE_SPL_POST_EXPO.dB.)
sample_sizes <- data.frame(
  treatment = c("5 mins after call initiation", "20 mins after call initiation"),
  y = 55,
  label = I(list(
    bquote(italic(N) == .(length (SPL_control$MALE_SPL_PRE_EXPO.dB.))),
    bquote(italic(N) == .(length(SPL_control$MALE_SPL_POST_EXPO.dB.)))
  ))
)
#dataframe for plots of movement
df_plot<-data.frame(treatment = c(rep("5 mins after call initiation", sample_zize_5mins), rep("20 mins after call initiation", sample_size_20mins)), spl  = c(SPL_control$MALE_SPL_PRE_EXPO.dB.,SPL_control$MALE_SPL_POST_EXPO.dB.))
df_plot$treatment <- factor(
  df_plot$treatment,
  levels = c("5 mins after call initiation", "20 mins after call initiation")
)

my_plot <- df_plot %>%
  ggplot(aes(x=treatment,y=spl, fill= treatment))+
  geom_boxplot(size=1,coef = Inf, outlier.shape = NA) +
  stat_boxplot(
    geom = "errorbar",
     coef = Inf,
    linewidth = 1,
    width = 0.5
  ) +
  scale_fill_manual(breaks = waiver(),values = c("white","grey90")) +
  scale_y_continuous(labels = function(x) ifelse(x %% 1==0, as.character(x), x)) +
  geom_point(position=position_jitterdodge(),alpha=1,col='black',size=2.5)+
  #stat_n_text(size=10)+
  labs(fill='SPL (dB)') +
  guides (fill=FALSE) +
  xlab('Treatment') +
  ylab('SPL (dB)') +
  theme_classic(25)+
  theme(axis.text=element_text(face='bold'),axis.title = element_text(face='bold')) 
my_plot <- my_plot +
  geom_text(
    data = sample_sizes,
    aes(x = treatment, y = y, label = label),
    inherit.aes = FALSE,
    size = 7,
    parse = TRUE 
  )
ggsave("supplementary_fig_1.jpg", plot = my_plot, width = 12, height = 8, units = "in", dpi = 300)
ß
############################################################################################ 
#Supplementary Figure 3 with analysis
#

df <-read.csv('Bodysize+movement.csv')
df<-na.omit(df)
nrow(df)
model <- glm(Moved.Towards~Body.size.mm., family = binomial, data=df)
summary(model)

my_plot <- df %>%
  ggplot(aes(x = Body.size.mm., y = Moved.Towards)) +
  geom_point(position = position_jitter(width = 0.1, height = 0), alpha = 1, col = 'black',size = 4) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE,linetype = "dashed", color = 'black', linewidth=2) +
  xlab('Body size (mm)') +
  ylab('Movement towards the speaker') +
  scale_y_continuous(
  labels = function(x) ifelse(x %in% c(0, 1), as.character(x), x)
) +
  theme_classic(25) +
  theme(axis.text = element_text(face = 'bold'), axis.title = element_text(face = 'bold'))

ggsave("supplementary_fig_3.jpg", plot = my_plot, width = 12, height = 8, units = "in", dpi = 300)
############################################################################################
#TODO:Figure out the relevant dataset and then plot
#Supplementary Figure 4

#test for movement males after male release
df1 <- read.csv('Movement_proportion_null.csv')
movers_without_playback <- sum(df1$Movement)
#total number of trials in Experiment T2 for 2,5,10 mins of playback
total_trials_without_playback <- length(df1$movement)
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

############################################################################################
#Supplementary Figure 5

df <- read.csv('Latency + Binary call data.csv')
#Experiment T1 (playback at start).
#caller porportion during playback
callers_t1 <- nrow(dplyr::filter(df,treatment == 'playback_at_start', latency>0,latency<=10))
#number of of total trials in Experiment T1.
trials_t1 <-nrow(dplyr::filter(df,treatment == 'playback_at_start'))

#Experiment T2 (playback at start).
#caller porportion during playback
callers_t2 <- nrow(dplyr::filter(df,treatment == 'no_playback_at_start', latency>0,latency<=10))
#number of of total trials in Experiment T1.
trials_t2 <-nrow(dplyr::filter(df,treatment == 'no_playback_at_start'))

prop.test(c(callers_t1,callers_t2), c(trials_t1,trials_t2), p = NULL, alternative = "two.sided",correct = TRUE)

#caller proportion without playback
fig_df<-data.frame(treatment = c('playback at start','no playback at start'), proportion = c(callers_t1/trials_t1, callers_t2/trials_t2) , label = c(paste0(callers_t1, '/', trials_t1), paste0(callers_t2, '/', trials_t2)))
my_plot <- fig_df%>%
  ggplot(aes(x=treatment,y=proportion,fill = treatment ))+
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = label),
    vjust = 1.3,          # moves text inside bar
    fontface = "bold",
    color = "black",
    size = 10
    ) +
  scale_x_discrete(labels = c(
    playback_at_start     = "Playback at start",
    no_playback_at_start  = "No playback at start"
  )) +
  scale_fill_manual(breaks = waiver(),values = c("grey75","grey50")) +
  scale_y_continuous(
    labels = function(x) ifelse(x == 0, "0", x)
  ) +
  guides (fill=FALSE) +
  xlab('Treatment') +
  ylab('Call propensity') +
  theme_classic(25)+
  theme(axis.text=element_text(face='bold'),axis.title = element_text(face='bold')) 

ggsave("supplementary_fig_5.jpg", plot = my_plot, width = 12, height = 8, units = "in", dpi = 300)





# ###############################tes for difference in chirp rate between time points
# #supplementary analyses for change in chirp rate with time

# dat <- read.csv('O.henryi_SPL_MovementtALL_DATA_T2.csv')

# dat<-filter(dat,CHIRP_RATE_PRE_EXPO_3min!='NaN',CHIRP_RATE_PRE_EXPO_5min!='NaN',CHIRP_RATE_PRE_EXPO_8min!='NaN')

# chirp_rate <- c(dat$CHIRP_RATE_PRE_EXPO_3min,dat$CHIRP_RATE_PRE_EXPO_5min,dat$CHIRP_RATE_PRE_EXPO_8min)
# df <- data.frame(treatment = rep(dat$TREATMENT..min.,3),id = rep(dat$TRIAL_NO.,3), time_of_measurement = c(rep(3,nrow(dat)),rep(5,nrow(dat)),rep(8,nrow(dat))),chirp_rate=chirp_rate)

# df$time_of_measurement<-as.factor(df$time_of_measurement)
# my_plot <- df%>%
#   ggplot(aes(x=time_of_measurement,y=chirp_rate, fill=factor(time_of_measurement)))+
#   geom_boxplot() +
#   scale_fill_manual(breaks = waiver(),values = c("white","grey70", 'grey90')) +
#   geom_point(position=position_jitterdodge(),alpha=1,col='black') +
#   #scale_y_continuous(breaks=c(0,0.1,0.5,1,1.5,2)) +
#   # scale_y_break(c(1.05, 1.9)) +
#   labs(fill='') +
#   xlab('Time of measurement(min)') +
#   ylab('Chirp rate(chirps per second)') +
#   theme_classic(base_size = 25)

# ggsave("chirp_rate_supplementary.jpg", plot = my_plot, width = 12, height = 8, units = "in", dpi = 300)


# dat_n <- filter(df,time_of_measurement!=3)

# #### wilcox.test(chirp_rate~time_of_measurement,data=dat_n,paired=TRUE)

# res.anova <- anova_test(
#   data = df, dv = chirp_rate, wid = id,
#   within = c( time_of_measurement))
# get_anova_table(res.anova)

# pwc <- df %>%
#   pairwise_t_test(
#     chirp_rate ~ time_of_measurement, paired = TRUE,
#     p.adjust.method = "bonferroni"
#   )
# pwc



# dat_n <- filter(df,treatment==0)
# wilcox.test(chirp_rate~time_of_measurement,data=dat_n,paired=TRUE)

# dat_n <- filter(df,treatment==2)
# wilcox.test(chirp_rate~time_of_measurement,data=dat_n,paired=TRUE)

# dat_n <- filter(df,treatment==5)
# wilcox.test(chirp_rate~time_of_measurement,data=dat_n,paired=TRUE)

# dat_n <- filter(df,treatment==10)
# wilcox.test(chirp_rate~time_of_measurement,data=dat_n,paired=TRUE)

