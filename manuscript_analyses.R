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
library (survival) 
library(ggfortify)
library(ggsurvfit)
library (survminer)

#set to current work directory where the data and the code is stored
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#################################################################################################################
                                        #Main Manuscript Analysis and plots#
#################################################################################################################

#################################################################################################################
                                  #Calling latency and caller proportion analysis and plots


#latency of callers and caller proportion during playback
df <- read.csv('Latency + Binary call data.csv')

#Figure 4a with analyses
#Figure 4a.Analysis of calling latency
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

########### Changed 3a -> 4a ###############

ggsave("fig_4_a.tiff", plot = my_plot, width = 12, height = 8, units = "in", dpi = 300)

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
fig_df<-data.frame(treatment = c('playback at start','no playback at start'), proportion = c(callers_t1/trials_t1, callers_t2/trials_t2) )
my_plot <- fig_df%>%
  ggplot(aes(x=treatment,y=proportion,fill = treatment ))+
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = sprintf("%.2f", proportion)),
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
ggsave("fig_4_b.tiff", plot = my_plot, width = 12, height = 8, units = "in", dpi = 300)

#Boxing the plots and Combining them
shared_theme <- theme_classic(25) + 
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.5),
    axis.text = element_text(face = 'bold'),
    axis.title = element_text(face = 'bold'),
    plot.margin = margin(15, 15, 15, 15),
    
    # 1. Move ticks inside (negative length pulls them into the plot)
    axis.ticks.length = unit(-0.25, "cm"), 
    
    # 2. Add margin to text so labels don't hit the inward ticks
    axis.text.x = element_text(margin = margin(t = 15), face = 'bold'),
    axis.text.y = element_text(margin = margin(r = 15), face = 'bold'),
    
    # 3. Ensure tick lines match your border thickness
    axis.ticks = element_line(colour = "black", linewidth = 1.5)
  )
# --- FIGURE 4A ---
p1 <- df1 %>%
  ggplot(aes(x=treatment, y=latency, fill=treatment)) +
  geom_boxplot(size=1, coef = Inf, outlier.shape = NA) +
  stat_boxplot(geom = "errorbar", coef = Inf, linewidth = 1, width = 0.5) +
  scale_fill_manual(values = c("white", "grey90")) +
  geom_point(position=position_jitterdodge(), alpha=1, col='black', size=2.5) +
  labs(x = 'Treatment', y = 'Latency (min)') +
  guides(fill = "none") +
  shared_theme + # Apply shared theme here
  geom_text(data = sample_sizes, aes(x = treatment, y = y, label = label),
            inherit.aes = FALSE, size = 7, parse = TRUE) +
  scale_x_discrete(labels = c("playback at start" = "Playback at start", 
                              "no playback at start" = "No playback at start")) +
  annotate("text", x = -Inf, y = Inf, label = "a)", 
           hjust = -0.3, vjust = 1.2, fontface = "bold", size = 10)

# --- FIGURE 4B ---
p2 <- fig_df %>%
  ggplot(aes(x=treatment, y=proportion, fill=treatment)) +
  geom_bar(stat = "identity") + 
  geom_text(aes(label = sprintf("%.2f", proportion)),
            vjust = 1.3, fontface = "bold", color = "black", size = 10) +
  scale_x_discrete(labels = c("playback at start" = "Playback at start", 
                              "no playback at start" = "No playback at start")) +
  scale_fill_manual(values = c("grey75", "grey50")) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = 'Treatment', y = 'Call propensity') +
  guides(fill = "none") +
  shared_theme + # Apply shared theme here
  annotate("text", x = -Inf, y = Inf, label = "b)", 
           hjust = -0.3, vjust = 1.2, fontface = "bold", size = 10)

# --- COMBINE WITH ALIGNMENT ---
# The '&' operator applies the alignment to all plots in the patchwork
final_plot <- (p1 | p2) + 
  plot_layout(widths = c(1, 1)) & 
  theme(plot.margin = margin(20, 20, 20, 20))
 
ggsave("fig_4_combined.tiff", plot = final_plot, width = 24, height = 8, units = "in", dpi = 300)

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
df_plot<-data.frame(treatment = c("playback at start","no playback at start"), mover_proportion = c(movers_with_playback/total_trials_with_playback,movers_without_playback/total_trials_without_playback))


#Fig_5a. plot for proportion of movers
my_plot <- df_plot%>%
  ggplot(aes(x=treatment,y=mover_proportion,fill = treatment ))+
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = sprintf("%.2f", mover_proportion)),
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

ggsave("fig_5_a.tiff", plot = my_plot, width = 12, height = 8, units = "in", dpi = 300)

#Fig_5b.plot for proportion of movers towards speaker
df_plot<-data.frame(treatment = c("playback at start","no playback at start"), mover_proportion_towards_speaker = c(movment_towards_speaker_with_playback/total_trials_with_playback,movment_towards_speaker_without_playback/total_trials_without_playback))
my_plot <- df_plot%>%
  ggplot(aes(x=treatment,y=mover_proportion_towards_speaker,fill = treatment ))+
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = sprintf("%.2f", mover_proportion_towards_speaker)),
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

#######Combining the plot 5a and 5b with boxes #############

shared_theme <- theme_classic(25) + 
  theme(
    # Create the full box
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.5),
    axis.line = element_blank(), # Remove classic lines so they don't overlap border
    
    axis.text = element_text(face = 'bold'),
    axis.title = element_text(face = 'bold'),
    plot.margin = margin(1, 1, 1, 1),
    
    # Move ticks inside
    axis.ticks.length = unit(-0.25, "cm"), 
    
    # Add margin to labels to avoid the inward ticks
    axis.text.x = element_text(margin = margin(t = 15), face = 'bold'),
    axis.text.y = element_text(margin = margin(r = 15), face = 'bold'),
    
    # Tick style
    axis.ticks = element_line(colour = "black", linewidth = 1.5)
  )

# --- 2. Fig_5a: Total Proportion of Movers ---
df_plot_a <- data.frame(
  treatment = c("playback at start", "no playback at start"), 
  mover_proportion = c(movers_with_playback/total_trials_with_playback, 
                       movers_without_playback/total_trials_without_playback)
)

p1 <- df_plot_a %>%
  ggplot(aes(x = treatment, y = mover_proportion, fill = treatment)) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = sprintf("%.2f", mover_proportion)),
    vjust = 1.3, fontface = "bold", color = "black", size = 10
  ) +
  scale_x_discrete(labels = c("playback at start" = "Playback at start", 
                              "no playback at start" = "No playback at start")) +
  scale_fill_manual(values = c("grey75", "grey50")) +
  scale_y_continuous(limits = c(0, 1), labels = function(x) ifelse(x == 0, "0", x)) +
  guides(fill = "none") +
  labs(x = 'Treatment', y = 'Proportion of movers') +
  shared_theme +
  annotate("text", x = -Inf, y = Inf, label = "a)", 
           hjust = -0.3, vjust = 1.2, fontface = "bold", size = 10)

# --- 3. Fig_5b: Proportion of Movers Towards Speaker ---
df_plot_b <- data.frame(
  treatment = c("playback at start", "no playback at start"), 
  mover_proportion_towards_speaker = c(movment_towards_speaker_with_playback/total_trials_with_playback, 
                                       movment_towards_speaker_without_playback/total_trials_without_playback)
)

p2 <- df_plot_b %>%
  ggplot(aes(x = treatment, y = mover_proportion_towards_speaker, fill = treatment)) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = sprintf("%g", round(mover_proportion_towards_speaker, 2))),
    vjust = 1.3, fontface = "bold", color = "black", size = 10
  ) +
  scale_x_discrete(labels = c("playback at start" = "Playback at start", 
                              "no playback at start" = "No playback at start")) +
  scale_fill_manual(values = c("grey75", "grey50")) +
  scale_y_continuous(limits = c(0, 1), labels = function(x) ifelse(x == 0, "0", x)) +
  guides(fill = "none") +
  labs(x = 'Treatment', y = 'Proportion of movers towards speaker') +
  shared_theme +
  annotate("text", x = -Inf, y = Inf, label = "b)", 
           hjust = -0.3, vjust = 1.2, fontface = "bold", size = 10)

# --- 4. Combine and Save ---
final_plot_5 <- (p1 | p2) + 
  plot_layout(widths = c(1, 1)) & 
  theme(plot.margin = margin(20, 20, 20, 20))

ggsave("fig_5_combined.tiff", plot = final_plot_5, width = 24, height = 8, units = "in", dpi = 300)

#################################################################################################################
                                  #SPL and chirp rate analysis with plots
#Analysis and plots for Figure 5a 
#Non-parametric tests for SPL difference between Experiment T1 and T2
df <- read.csv('Pre_Expo_SPL_CR3.csv')
df_spl <- df %>%
  filter(!is.na(MALE_SPL.dB.)) %>%
  select(MALE_SPL.dB., Treatment)
wilcox.test(df$MALE_SPL.dB.~df$Treatment) #non-parametric

#Figure 6a. 

sample_size_playback_at_start <- nrow(dplyr::filter (df_spl,Treatment=='playback at start'))
sample_size_no_playback_at_start <- nrow(dplyr::filter (df_spl,Treatment=='no playback at start'))

sample_sizes_a <- data.frame(
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
    data = sample_sizes_a,
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

ggsave("fig_6_a.jpg", plot = my_plot, width = 12, height = 8, units = "in", dpi = 300)


#Analysis and plot for Figure 6b 
#Non-parametric tests for chirp rate difference between Experiment T1 and T2
df_chirp_rate <- df %>%
  filter(!is.na(CHIRP_RATE)) %>%
  select(CHIRP_RATE, Treatment)
wilcox.test(df$CHIRP_RATE~df$Treatment) #non-parametric


#Figure 6b.
sample_size_playback_at_start <- nrow(dplyr::filter (df_chirp_rate,Treatment=='playback at start'))
sample_size_no_playback_at_start <- nrow(dplyr::filter (df_chirp_rate,Treatment=='no playback at start'))

sample_sizes_b <- data.frame(
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
    data = sample_sizes_b,
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
ggsave("fig_6_b.tiff", plot = my_plot, width = 12, height = 8, units = "in", dpi = 300)

#Combining Figure6a and b and framing them
shared_theme <- theme_classic(25) + 
  theme(
    # Create the full box
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.5),
    axis.line = element_blank(), 
    
    axis.text = element_text(face = 'bold'),
    axis.title = element_text(face = 'bold'),
    plot.margin = margin(15, 15, 15, 15),
    
    # Move ticks inside
    axis.ticks.length = unit(-0.25, "cm"), 
    
    # Add margin to text so labels don't hit the inward ticks
    axis.text.x = element_text(margin = margin(t = 15), face = 'bold'),
    axis.text.y = element_text(margin = margin(r = 15), face = 'bold'),
    
    # Ensure tick lines match your border thickness
    axis.ticks = element_line(colour = "black", linewidth = 1.5)
  )

# --- 2. FIGURE 6A: SPL Plot ---
p1 <- df_spl %>%
  ggplot(aes(x = Treatment, y = MALE_SPL.dB., fill = Treatment)) +
  geom_boxplot(size = 1, coef = Inf, outlier.shape = NA) +
  stat_boxplot(geom = "errorbar", coef = Inf, linewidth = 1, width = 0.5) +
  scale_x_discrete(labels = c("playback at start" = "Playback at start", 
                              "no playback at start" = "No playback at start")) +
  scale_fill_manual(values = c("white", "grey90")) +
  geom_point(position = position_jitterdodge(), alpha = 1, col = 'black', size = 2.5) +
  labs(x = 'Treatment', y = 'SPL (dB)') +
  guides(fill = "none") +
  shared_theme + 
  geom_text(data = sample_sizes_a, aes(x = treatment, y = y, label = label),
            inherit.aes = FALSE, size = 7, parse = TRUE) +
  annotate("text", x = -Inf, y = Inf, label = "a)", 
           hjust = -0.3, vjust = 1.2, fontface = "bold", size = 10)

# --- 3. FIGURE 6B: Chirp Rate Plot ---
p2 <- df_chirp_rate %>%
  ggplot(aes(x = Treatment, y = CHIRP_RATE, fill = Treatment)) +
  geom_boxplot(size = 1, coef = Inf, outlier.shape = NA) +
  stat_boxplot(geom = "errorbar", coef = Inf, linewidth = 1, width = 0.5) +
  scale_x_discrete(labels = c("playback at start" = "Playback at start", 
                              "no playback at start" = "No playback at start")) +
  scale_fill_manual(values = c("white", "grey90")) +
  scale_y_continuous(labels = function(x) ifelse(x == 2, "2", x)) +
  geom_point(position = position_jitterdodge(), alpha = 1, col = 'black', size = 2.5) +
  labs(x = 'Treatment', y = 'Chirps / second') +
  guides(fill = "none") +
  shared_theme +
  geom_text(data = sample_sizes_b, aes(x = treatment, y = y, label = label),
            inherit.aes = FALSE, size = 7, parse = TRUE) +
  annotate("text", x = -Inf, y = Inf, label = "b)", 
           hjust = -0.3, vjust = 1.2, fontface = "bold", size = 10)

# --- 4. Combine with patchwork ---
final_plot_6 <- (p1 | p2) + 
  plot_layout(widths = c(1, 1)) & 
  theme(plot.margin = margin(20, 20, 20, 20))

ggsave("fig_6_combined.tiff", plot = final_plot_6, width = 24, height = 8, units = "in", dpi = 300)


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


#Figure 7a. plot for change in SPL with exposure
df <- df %>%
  mutate(treatment = dplyr::recode(treatment, 
                            "0" = "control",
                            "2" = "2 min",
                            "5" = "5 min",
                            "10" = "10 min"))

sample_sizes_a <- df %>%
dplyr::count(treatment) %>%
dplyr::mutate(
  n = n / 2,
  y = 54.5,  # just below your lower y-limit (54)
  label = paste0("italic(N)==", n)
)
pd <- position_dodge(width = 0.75)

############################################## NO BOXES AROUND PLOT ##########################################################################################################################################################################

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
    data = sample_sizes_a,
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

############################################ SPL PLOT WITH BOX AND TICK INSIDE THE PLOT #######################################################################################################################################################
# --- 1. SHARED THEME (Frame + Inward Ticks) ---
shared_theme <- theme_classic(base_size = 30) + 
  theme(
    # Create the full box
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 2),
    axis.line = element_blank(), 
    
    axis.text = element_text(face = 'bold'),
    axis.title = element_text(face = 'bold'),
    plot.margin = margin(15, 15, 15, 15),
    
    # Move ticks inside
    axis.ticks.length = unit(-0.3, "cm"), 
    
    # Add margin to text so labels don't hit the inward ticks
    axis.text.x = element_text(margin = margin(t = 20), face = 'bold'),
    axis.text.y = element_text(margin = margin(r = 20), face = 'bold'),
    
    # Tick lines match border thickness
    axis.ticks = element_line(colour = "black", linewidth = 2),
    
    # Legend at the top for multi-panel consistency
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(face = "bold")
  )

# --- 2. FIGURE 7A PLOT ---
# Ensure your factor levels for the legend are capitalized
df$spl_time <- factor(df$spl_time, levels = c("before exposure", "post exposure"), 
                      labels = c("Before exposure", "Post exposure"))

pd <- position_dodge(width = 0.75)

p1 <- df %>%
  ggplot(aes(x = treatment, y = spl, fill = spl_time)) +
  geom_boxplot(size = 1, coef = Inf, outlier.shape = NA, linewidth = 1) +
  stat_boxplot(
    geom = "errorbar",
    coef = Inf,
    linewidth = 2,
    width = 0.5,
    position = pd 
  ) +
  # Harmonized Legend
  scale_fill_manual(
    values = c("white", "grey70"),
    name = "Time", 
    labels = c("Before exposure", "Post exposure")
  ) +
  # Use position_jitterdodge to match the boxplot dodge width
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75), 
             alpha = 1, col = 'black', size = 2.5) +
  
  labs(x = '', y = 'SPL (dB)') +
  scale_y_continuous(breaks = seq(54, 66, 4), limits = c(54, 68), 
                     expand = expansion(mult = c(0, 0.02))) +
  
  shared_theme + # APPLYING THE SHARED THEME
  
  # Sample sizes at the bottom
  geom_text(
    data = sample_sizes_a,
    aes(x = treatment, y = y, label = label),
    inherit.aes = FALSE,
    size = 7,
    parse = TRUE
  ) +
  
  # Panel label (a)
  annotate(
    "text",
    x = -Inf, y = Inf,
    label = "a)",
    hjust = -0.3, vjust = 1.3,
    fontface = "bold",
    size = 10
  )

##GLM SPL Table 1

model1 <- lmer(spl ~ treatment * spl_time + (1|id), data = df)
summary(model1)
confint(model1)
anova(model1)
Anova(model1,type = "III") 


##############################################################################################################################################################################################################################

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

sample_sizes_b <- df %>%
dplyr::count(treatment) %>%
dplyr::mutate(
  n = n / 2,
  y = 1.2,  # just below your lower y-limit (54)
  label = paste0("italic(N)==", n)
)
pd <- position_dodge(width = 0.75)

############################################## NO BOXES AROUND PLOT ##########################################################################################################################################################################

my_plot <- df%>%
  ggplot(aes(x=treatment,y=chirp_rate, fill=factor(time_of_measurement)))+
  geom_boxplot(size=1,coef = Inf, outlier.shape = NA,linewidth = 1) +
  stat_boxplot(
    geom = "errorbar",
     coef = Inf,
    linewidth = 2,
    width = 0.5,
    position = pd 
  ) +
  scale_fill_manual(breaks = waiver(),values = c("white","grey70")) +
  geom_point(position=position_jitterdodge(),alpha=1,col='black',size=2.5)+
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
    data = sample_sizes_b,
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

p2<-my_plot
############################################### Chirp Rate  Plotting With Box and Tick Mark Inside ###############################################
# --- 1. SHARED THEME (Frame + Inward Ticks) ---
shared_theme <- theme_classic(base_size = 30) + 
  theme(
    # Create the full box
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 2),
    axis.line = element_blank(), 
    
    axis.text = element_text(face = 'bold'),
    axis.title = element_text(face = 'bold'),
    plot.margin = margin(15, 15, 15, 15),
    
    # Move ticks inside
    axis.ticks.length = unit(-0.3, "cm"), 
    
    # Add margin to text so labels don't hit the inward ticks
    axis.text.x = element_text(margin = margin(t = 20), face = 'bold'),
    axis.text.y = element_text(margin = margin(r = 20), face = 'bold'),
    
    # Tick lines match border thickness
    axis.ticks = element_line(colour = "black", linewidth = 2),
    
    # Legend at the top for multi-panel consistency
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(face = "bold")
  )

p2 <- df %>%
  ggplot(aes(x = treatment, y = chirp_rate, fill = time_of_measurement)) +
  geom_boxplot(size = 1, coef = Inf, outlier.shape = NA, linewidth = 1) +
  stat_boxplot(
    geom = "errorbar",
    coef = Inf,
    linewidth = 2,
    width = 0.5,
    position = pd 
  ) +
  # Harmonized Legend
  scale_fill_manual(
    values = c("white", "grey70"),
    name = "Time", 
    labels = c("Before exposure", "Post exposure")
  ) +
  # Use jitterdodge to match boxplot centers
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75), 
             alpha = 1, col = 'black', size = 2.5) +
  
  scale_y_continuous(labels = function(x) ifelse(x == 2, "2", x)) +
  labs(x = '', y = 'Chirps / second') +
  
  shared_theme + # APPLYING THE SHARED THEME
  
  # Sample sizes text
  geom_text(
    data = sample_sizes_b,
    aes(x = treatment, y = y, label = label),
    inherit.aes = FALSE,
    size = 7,
    parse = TRUE
  ) +
  
  # Panel label (b)
  annotate(
    "text",
    x = -Inf, y = Inf,
    label = "b)",
    hjust = -0.3, vjust = 1.3,
    fontface = "bold",
    size = 10
  )


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


sample_sizes_c <- df_new %>%
dplyr::count(treatment) %>%
dplyr::mutate(
  n = n / 2,
  y = -0.05,  # just below your lower y-limit (54)
  label = paste0("italic(N)==", n)
)
pd <- position_dodge(width = 0.75)

############################################## NO BOXES AROUND PLOT ##########################################################################################################################################################################

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
    data = sample_sizes_c,
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

#########################################  Call effort Plots With Boxing ans Tick Inside ############################################################################

# --- 1. SHARED THEME (Frame + Inward Ticks) ---
# Ensure this is defined so all plots look identical
shared_theme <- theme_classic(base_size = 30) + 
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 2),
    axis.line = element_blank(), 
    axis.text = element_text(face = 'bold'),
    axis.title = element_text(face = 'bold'),
    plot.margin = margin(15, 15, 15, 15),
    
    # Inward ticks logic
    axis.ticks.length = unit(-0.3, "cm"), 
    axis.text.x = element_text(margin = margin(t = 20), face = 'bold'),
    axis.text.y = element_text(margin = margin(r = 20), face = 'bold'),
    axis.ticks = element_line(colour = "black", linewidth = 2),
    
    # Legend at the top
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(face = "bold")
  )

# --- 2. DATA PREPARATION ---
# Capitalizing legend labels for consistency across all panels
pd <- position_dodge(width = 0.75)

# --- 3. FIGURE 7C PLOT ---
p3 <- df_new %>%
  ggplot(aes(x = treatment, y = call_effort, fill = exposure_time)) +
  geom_boxplot(size = 1, coef = Inf, outlier.shape = NA, linewidth = 1) +
  stat_boxplot(
    geom = "errorbar",
    coef = Inf,
    linewidth = 2,
    width = 0.5,
    position = pd 
  ) +
  # Harmonized Legend
  scale_fill_manual(
    values = c("white", "grey70"),
    name = "Time", 
    labels = c("Before exposure", "Post exposure")
  ) +
  # Points sized 4 and dodged to match box centers
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75), 
             alpha = 1, col = 'black', size = 2.5) +
  
  # Clean y-axis labels for 0 and 1
  scale_y_continuous(
    labels = function(x) ifelse(x %in% c(0, 1), as.character(x), x)
  ) +
  
  labs(x = 'Exposure duration (min)', y = 'Call effort') +
  
  shared_theme + # APPLYING THE SHARED THEME
  
  # Sample sizes text
  geom_text(
    data = sample_sizes_c,
    aes(x = treatment, y = y, label = label),
    inherit.aes = FALSE,
    size = 7,
    parse = TRUE
  ) +
  
  # Panel label (c)
  annotate(
    "text",
    x = -Inf, y = Inf,
    label = "c)",
    hjust = -0.3, vjust = 1.3,
    fontface = "bold",
    size = 10
  )

final_fig_7 <- (p1 / p2 / p3) + 
  plot_layout(guides = 'collect') & 
  theme(legend.position = 'top')

ggsave("fig_7 combined.tiff", plot = final_fig_7, width = 24, height = 24, units = "in", dpi = 300)

######################################################################################################
                                  #Supplementary Ananlysis#
##################################################################################################################                                       

#Figure out the relevant dataset and then plot
#Supplementary Figure 1
dat <- read.csv('O.henryi_SPL_MovementtALL_DATA_T2.csv')
#Subsetting control data
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
########################## Plot With Box and Tick mark inside#########################
# --- 1. SHARED THEME (Frame + Inward Ticks) ---
shared_theme <- theme_classic(base_size = 25) + 
  theme(
    # Create the full box
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 2),
    axis.line = element_blank(), 
    
    axis.text = element_text(face = 'bold'),
    axis.title = element_text(face = 'bold'),
    plot.margin = margin(15, 15, 15, 15),
    
    # Move ticks inside
    axis.ticks.length = unit(-0.3, "cm"), 
    
    # Add margin to text so labels don't hit the inward ticks
    axis.text.x = element_text(margin = margin(t = 20), face = 'bold'),
    axis.text.y = element_text(margin = margin(r = 20), face = 'bold'),
    
    # Tick lines match border thickness
    axis.ticks = element_line(colour = "black", linewidth = 2)
  )

# --- 2. THE PLOT ---
my_plot <- df_plot %>%
  ggplot(aes(x = treatment, y = spl, fill = treatment)) +
  geom_boxplot(size = 1, coef = Inf, outlier.shape = NA) +
  stat_boxplot(
    geom = "errorbar",
    coef = Inf,
    linewidth = 1,
    width = 0.5
  ) +
  # Capitalizing and wrapping X-axis labels
  scale_x_discrete(labels = c("playback at start" = "Playback at start", 
                              "no playback at start" = "No playback at start")) +
  scale_fill_manual(values = c("white", "grey90")) +
  scale_y_continuous(labels = function(x) ifelse(x %% 1 == 0, as.character(x), x)) +
  geom_point(position = position_jitterdodge(), alpha = 1, col = 'black', size = 2.5) +
  
  # Set xlab to Read Time of Measurement (min) 
  labs(x = "Time of Measurement (min)", y = "SPL (dB)") + 
  
  guides(fill = "none") +
  shared_theme + # APPLYING THE SHARED THEME
  
  # Sample sizes text
  geom_text(
    data = sample_sizes,
    aes(x = treatment, y = y, label = label),
    inherit.aes = FALSE,
    size = 7,
    parse = TRUE 
  )


ggsave("supplementary_fig_1.tiff", plot = my_plot, width = 12, height = 8, units = "in", dpi = 300)

############################################################################################ 
#Supplementary Figure 4 with analysis

df <-read.csv('Bodysize+movement.csv')
df<-na.omit(df)
#sample size
nrow(df)
#GLM for body size and movement towards speaker
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
############################## body size and movement towards speaker + Box ################################
# --- 1. SHARED THEME (Frame + Inward Ticks) ---
shared_theme <- theme_classic(base_size = 25) + 
  theme(
    # Create the full box
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 2),
    axis.line = element_blank(), 
    
    axis.text = element_text(face = 'bold'),
    axis.title = element_text(face = 'bold'),
    plot.margin = margin(15, 15, 15, 15),
    
    # Move ticks inside
    axis.ticks.length = unit(-0.3, "cm"), 
    
    # Add margin to text so labels don't hit the inward ticks
    axis.text.x = element_text(margin = margin(t = 20), face = 'bold'),
    axis.text.y = element_text(margin = margin(r = 20), face = 'bold'),
    
    # Tick lines match border thickness
    axis.ticks = element_line(colour = "black", linewidth = 2)
  )

# --- 2. THE PLOT ---
my_plot <- df %>%
  ggplot(aes(x = Body.size.mm., y = Moved.Towards)) +
  # Jittered points for binary data
  geom_point(position = position_jitter(width = 0.1, height = 0.02), 
             alpha = 1, col = 'black', size = 2.5) +
  # Logistic regression curve
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE, 
              linetype = "dashed", 
              color = 'black', 
              linewidth = 2) +
  # Labels
  labs(x = 'Body size (mm)', y = 'Movement towards the speaker') +
  # Ensure y-axis only highlights 0 and 1
  scale_y_continuous(breaks = c(0, 0.5, 1),
                     labels = function(x) ifelse(x %in% c(0, 1), as.character(x), x)) +
  
  shared_theme # APPLYING THE SHARED THEME
ggsave("supplementary_fig_4.tiff", plot = my_plot, width = 12, height = 8, units = "in", dpi = 300)
############################################################################################

#Supplementary Figure 5
#Experiment T2 (no playback at start).
#dataset for trials with no playback at start
df1 <- read.csv('Exp2_null_movement.csv')
na.omit(df1)

#number of movers in Experiment T2 within 10 mins of release for all trails (i.e., 0,2,5,10 mins of playback)
movers_without_playback <- sum(df1$Movement)
#total number of trials in Experiment T2 
total_trials_without_playback <- length(df1$Movement)
#number of movers who moved towards the speaker in Experiment T2 for 2,5,10 mins of playback



#Experiment T1 (with playback at start).
df2 <- read.csv('O.henryi_SPL_MovementtALL_DATA_T1.csv')
#number of movers in Experiment T1 within 10 mins
df2$movement <- ifelse(df2$MOVEMENT_SIDE..0.L.R.!=0,1,0)
movers_with_playback <- sum(df2$movement)
#number of movers in Experiment T1 within 10 mins of 
total_trials_with_playback <- length(df2$movement)


#test for difference in proportion of movers between Experiment T1 and T2 within 10 mins of release
prop.test(c(movers_without_playback,movers_with_playback), c(total_trials_without_playback ,total_trials_with_playback), p = NULL, alternative = "two.sided",
          correct = TRUE)



#dataframe for plots of movement
df_plot<-data.frame(treatment = c("playback at start","no playback at start"), mover_proportion = c(movers_with_playback/total_trials_with_playback,movers_without_playback/total_trials_without_playback))



my_plot <- df_plot%>%
  ggplot(aes(x=treatment,y=mover_proportion,fill = treatment ))+
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = sprintf("%.2f", mover_proportion)),
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
################################ BOX AND TICK ADDED ####################
# --- 1. THE SHARED THEME (Box + Inward Ticks) ---
shared_theme <- theme_classic(base_size = 25) + 
  theme(
    # Create the full box
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 2),
    axis.line = element_blank(), 
    
    axis.text = element_text(face = 'bold'),
    axis.title = element_text(face = 'bold'),
    plot.margin = margin(15, 15, 15, 15),
    
    # Move ticks inside
    axis.ticks.length = unit(-0.3, "cm"), 
    
    # Add margin to text so labels don't hit the inward ticks
    axis.text.x = element_text(margin = margin(t = 20), face = 'bold'),
    axis.text.y = element_text(margin = margin(r = 20), face = 'bold'),
    
    # Ensure tick lines match border thickness
    axis.ticks = element_line(colour = "black", linewidth = 2)
  )

# --- 2. THE PLOT ---
my_plot <- df_plot %>%
  ggplot(aes(x = treatment, y = mover_proportion, fill = treatment)) +
  geom_bar(stat = "identity") + # No color = "black" for unboxed bars
  geom_text(
    aes(label = sprintf("%.2f", mover_proportion)),
    vjust = 1.3,           # moves text inside bar
    fontface = "bold",
    color = "black",
    size = 10
  ) +
  # Capitalizing and wrapping labels for a professional boxed look
  scale_x_discrete(labels = c(
    "playback at start"    = "Playback at start",
    "no playback at start" = "No playback at start"
  )) +
  scale_fill_manual(values = c("grey75", "grey50")) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = function(x) ifelse(x == 0, "0", x)
  ) +
  labs(x = 'Treatment', y = 'Proportion of movers') +
  guides(fill = "none") +
  shared_theme # APPLYING THE SHARED THEME

ggsave("supplementary_fig_5.tiff", plot = my_plot, width = 12, height = 8, units = "in", dpi = 300)

############################################################################################
#Supplementary Figure 6

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
fig_df<-data.frame(treatment = c('playback at start','no playback at start'), proportion = c(callers_t1/trials_t1, callers_t2/trials_t2) )
my_plot <- fig_df%>%
  ggplot(aes(x=treatment,y=proportion,fill = treatment ))+
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = sprintf("%.2f", proportion)),
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
######################################### BOX and Ticks ####################################

# --- 1. THE SHARED THEME (Box + Inward Ticks) ---
shared_theme <- theme_classic(base_size = 25) + 
  theme(
    # Create the full box enclosure
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 2),
    axis.line = element_blank(), 
    
    axis.text = element_text(face = 'bold'),
    axis.title = element_text(face = 'bold'),
    plot.margin = margin(15, 15, 15, 15),
    
    # Move ticks inside (negative length pulls them into the plot)
    axis.ticks.length = unit(-0.3, "cm"), 
    
    # Add margin to text so labels don't hit the inward ticks
    axis.text.x = element_text(margin = margin(t = 20), face = 'bold'),
    axis.text.y = element_text(margin = margin(r = 20), face = 'bold'),
    
    # Ensure tick lines match your border thickness
    axis.ticks = element_line(colour = "black", linewidth = 2)
  )

# --- 2. THE PLOT ---
my_plot <- fig_df %>%
  ggplot(aes(x = treatment, y = proportion, fill = treatment)) +
  geom_bar(stat = "identity") + # Keeps bars "unboxed" (no black outline)
  geom_text(
    aes(label = sprintf("%.2f", proportion)),
    vjust = 1.3,           # moves text inside bar
    fontface = "bold",
    color = "black",
    size = 10
  ) +
  # Capitalizing and wrapping labels for a professional boxed look
  scale_x_discrete(labels = c(
    "playback at start"    = "Playback at start",
    "no playback at start" = "No playback at start"
  )) +
  scale_fill_manual(values = c("grey75", "grey50")) +
  scale_y_continuous(
    limits = c(0, 0.7),
    labels = function(x) ifelse(x == 0, "0", x)
  ) +
  labs(x = 'Treatment', y = 'Call propensity') +
  guides(fill = "none") +
  shared_theme # APPLYING THE SHARED THEME

ggsave("supplementary_fig_6.tiff", plot = my_plot, width = 12, height = 8, units = "in", dpi = 300)

############################################################################################
#Supplementary Figure 7
#Survivorship for males using the kaplan_meier method
dat <- read.csv('Data_Surv_Analysis-1.csv',na.strings = c("NA",""))
dat <- dat[!is.na(dat$Exp.ID), ]
dat$Latency[dat$Exp.ID== 1 & dat$Latency=='NaN'] <- 30
dat$Latency[dat$Exp.ID== 2 & dat$Latency=='NaN'] <- 20
dat$Exp.ID  <- ifelse (dat$Exp.ID ==1, "Experiment 1", "Experiment 2")
dat$Exp.ID <- as.factor(dat$Exp.ID)


######################################## Boxed Plot ############################################
# --- 1. THE SHARED THEME (Box + Inward Ticks) ---
shared_theme <- theme_classic(base_size = 25) + 
  theme(
    # Create the full box enclosure
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 2),
    axis.line = element_blank(), 
    
    axis.text = element_text(face = 'bold'),
    axis.title = element_text(face = 'bold'),
    plot.margin = margin(15, 15, 15, 15),
    
    # Move ticks inside (negative length pulls them into the plot)
    axis.ticks.length = unit(-0.3, "cm"), 
    
    # Add margin to text so labels don't hit the inward ticks
    axis.text.x = element_text(margin = margin(t = 20), face = 'bold'),
    axis.text.y = element_text(margin = margin(r = 20), face = 'bold'),
    
    # Ensure tick lines match your border thickness
    axis.ticks = element_line(colour = "black", linewidth = 2),
    
    # Legend formatting
    legend.position = "top",
    legend.text = element_text(face = "bold")
  )

my_plot <- survfit2(Surv(Latency, call_binary) ~ Exp.ID , data = dat) %>% 
  ggsurvfit(linetype_aes = TRUE,size=1) +
  labs(
    x = "Latency (min)",
    y = "Silent probability"
  ) +
  scale_colour_manual(values =  c("black","black","black")) +
  scale_x_continuous( breaks = c(0,5,10,15,20,25,30,35), limits=c(0,35)) +
  scale_y_continuous(
    labels = function(x) ifelse(x %% 1 == 0, as.character(x), x)
  ) +
  add_confidence_interval() +
  add_legend_title('') +
  theme_classic(25) +
  theme(axis.text=element_text(face='bold'),axis.title = element_text(face='bold'))+
  shared_theme

ggsave("supplementary_fig_7.tiff", plot = my_plot, width = 12, height = 8, units = "in", dpi = 300)

#testing for statistical differences between the survival curves
survdiff(Surv(Latency, call_binary) ~ Exp.ID,
         data = dat)



