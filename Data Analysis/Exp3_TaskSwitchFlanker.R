#### initial ####
#packages
library(tidyverse)
library(reshape2)
library(psych)
library(knitr)
library(plotrix)
library(patchwork)

#load data
path <-  "C:/..."
file <- "combinedDataF.csv"
df <- read_csv(paste(path,file,sep=""))

#filter out unimportant trials
df <- df %>% 
  filter(sectionType == "mainTask") %>% 
  mutate(stimCongruency = ifelse(grepl("Inc",stimCongruency),"i","c")) 

#exclude participants
acc_cutoff = .75
distTrialsAccCutoff = 0.6
accuracies <- df %>% 
  group_by(subject) %>% 
  summarise(mean_acc = mean(acc, na.rm = TRUE)) 
rel_accuracies <- df %>% 
  group_by(subject, taskRelevancy) %>% 
  summarise(mean_acc = mean(acc, na.rm = TRUE)) 

excluded_subs <- accuracies %>% 
  filter(mean_acc < acc_cutoff) %>% 
  select(subject, mean_acc)
excluded_subs_distractors <- rel_accuracies %>% 
  filter(mean_acc < distTrialsAccCutoff) %>% 
  select(subject, mean_acc)

#final df
df <- df %>% 
  filter(!subject %in% excluded_subs$subject) %>% 
  filter(!subject %in% excluded_subs_distractors$subject) %>% 
  filter(switchType != "n") %>% 
  mutate(switch_perc = ifelse(blockType == "B" | blockType == "D", "75% Switch", "25% Switch")) %>% 
  mutate(congruency_perc = ifelse(blockType == "A" | blockType == "B", "75% Incongruent", "25% Incongruent"))

# Trial Exclusions
df_trimmed <- df %>% 
  group_by(subject) %>% 
  filter(taskRelevancy != 'd') %>% 
  filter(acc == 1) %>% 
  filter((abs(RT - mean(RT,na.rm = TRUE)) <= 3*sd(RT, na.rm = TRUE))) %>% 
  filter(RT >= 300) %>% 
  filter(RT <= 2000)

# for the group means
data_summary <- function(x){
  m <- mean(x)
  ymin <- m - 1.96*std.error(x, na.rm = TRUE)
  ymax <- m + 1.96*std.error(x, na.rm = TRUE)
  return(c(y = m, ymin = ymin, ymax = ymax))
}

#params
dot_size = 1.5
bin_width = 20
sum_size = 0.8
rt_l1 = 600
rt_l2 = 1600
rt_l3 = -5
rt_l4 = 250
acc_l1 = 0.4
acc_l2 = 1
acc_l3 = -0.1
acc_l4 = 0.01

#switch and repeat rts
switch_RT <- df_trimmed %>% 
  group_by(subject, switch_perc, congruency_perc, switchType) %>% 
  summarise(mean_RT = mean(RT,na.rm = TRUE))

# group RTs
group_RT <- switch_RT %>%
  group_by(switchType,switch_perc,congruency_perc) %>%
  summarise(group_means = mean(mean_RT, na.rm = TRUE),
            se_RT = std.error(mean_RT, na.rm = TRUE),
            sd_RT = sd(mean_RT, na.rm = TRUE),
            n = n()) %>%
  mutate(upper.ci = group_means + qt(1-(0.05/2), n-1)*se_RT,
         lower.ci = group_means - qt(1-(0.05/2), n-1)*se_RT)
kable(group_RT)

#switch cost RTs
switch_cost_RT <- switch_RT %>%
  group_by(subject,switch_perc,congruency_perc) %>%
  mutate(sum_RT = sum(mean_RT, na.rm = TRUE)) %>%
  filter(switchType == 's') %>%
  mutate(reverse = sum_RT - mean_RT) %>%
  mutate(switch_cost = mean_RT - reverse)


# switch cost by switch proportion
switch_cost_sp_RT_mean <- switch_cost_RT %>%
  group_by(switchType,switch_perc) %>%
  summarise(mean_switch_cost = mean(switch_cost, na.rm = TRUE),
            se_sc = std.error(switch_cost, na.rm = TRUE)) %>%
  mutate (ci = 1.96*se_sc)

#plot version 1
dot_plot <- ggplot(switch_RT, aes(x = switch_perc, y = mean_RT, fill = switchType)) +
  scale_fill_manual(values = c('dodgerblue3', 'gold')) +
  coord_cartesian(ylim = c(rt_l1, rt_l2)) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', position = position_dodge(0.8), dotsize = dot_size, colour = 'white', binwidth = bin_width) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_blank(), #element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"),
        legend.key = element_rect(fill = c("dodgerblue3","gold")),
        legend.position = "none",
        strip.text.x = element_text(size = 16)
  ) +
  labs(x = "Switch Proportion", 
       y = "RT (ms)") + 
  scale_x_discrete(labels = c('25% Switch' = "25%",'75% Switch' = "75%")) 

# putting group means on dot_rt
sw_sp_rt <- dot_plot + 
  stat_summary(
    fun.data = data_summary,
    position = position_dodge(0.8), 
    aes(shape = factor(switchType)), size = sum_size) +
  scale_shape_manual(name = "switchType", values = c(15, 17))

sc_sp_rt <- ggplot(switch_cost_sp_RT_mean, aes(x = switch_perc, y = mean_switch_cost)) + 
  geom_bar(position = "dodge", stat = "identity", fill = "gray", color = "gray") + 
  labs(x = "Switch Proportion", 
       y = "Switch Cost") + 
  geom_errorbar(aes(ymin = mean_switch_cost - ci, ymax = mean_switch_cost + ci), 
                width = .2, 
                position = position_dodge(.9)) + 
  coord_cartesian(ylim = c(rt_l3, rt_l4)) +
  scale_x_discrete(labels = c('25% Switch' = "25%",'75% Switch' = "75%")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  scale_y_continuous(breaks = c(0, 100, 200))

#sw_sp_rt + sc_sp_rt + plot_layout(nrow = 2, heights = c(3,1))

# switch cost by congruency proportion
switch_cost_cp_RT_mean <- switch_cost_RT %>%
  group_by(switchType,congruency_perc) %>%
  summarise(mean_switch_cost = mean(switch_cost, na.rm = TRUE),
            se_sc = std.error(switch_cost, na.rm = TRUE)) %>%
  mutate (ci = 1.96*se_sc)

#plot version
dot_plot <- ggplot(switch_RT, aes(x = congruency_perc, y = mean_RT, fill = switchType)) +
  scale_fill_manual(values = c('dodgerblue3', 'gold')) +
  coord_cartesian(ylim = c(rt_l1, rt_l2)) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', position = position_dodge(0.8), dotsize = dot_size, colour = 'white', binwidth = bin_width) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_blank(), #element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"),
        legend.key = element_rect(fill = c("dodgerblue3","gold")),
        legend.position = "none",
        strip.text.x = element_text(size = 16)
  ) +
  labs(x = "Congruency Proportion", 
       y = "RT (ms)") + 
  scale_x_discrete(labels = c('25% Incongruent' = "25%",'75% Incongruent' = "75%")) 

# putting group means on dot_rt
sw_cp_rt <- dot_plot + 
  stat_summary(
    fun.data = data_summary,
    position = position_dodge(0.8), 
    aes(shape = factor(switchType)), size = sum_size) +
  scale_shape_manual(name = "switchType", values = c(15, 17))

sc_cp_rt <- ggplot(switch_cost_cp_RT_mean, aes(x = congruency_perc, y = mean_switch_cost)) + 
  geom_bar(position = "dodge", stat = "identity", fill = "gray", color = "gray") + 
  labs(x = "Congruency Proportion", 
       y = "Switch Cost") + 
  geom_errorbar(aes(ymin = mean_switch_cost - ci, ymax = mean_switch_cost + ci), 
                width = .2, 
                position = position_dodge(.9)) + 
  coord_cartesian(ylim = c(rt_l3, rt_l4)) +
  scale_x_discrete(labels = c('25% Incongruent' = "25%",'75% Incongruent' = "75%")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  scale_y_continuous(breaks = c(0, 100, 200))

#sw_cp_rt + sc_cp_rt + plot_layout(nrow = 2, heights = c(3,1))

layout <- "
AC
AC
AC
BD
"

#sw_sp_rt + sc_sp_rt + sw_cp_rt + sc_cp_rt + plot_layout(design = layout)

##### Congruency #####

#same thing but for congruency effects
congruency_RT <- df_trimmed %>% 
  group_by(subject, switch_perc, congruency_perc, stimCongruency) %>% 
  summarise(mean_RT = mean(RT,na.rm = TRUE))

# group RTs
group_RT <- congruency_RT %>%
  group_by(stimCongruency,switch_perc,congruency_perc) %>%
  summarise(group_means = mean(mean_RT, na.rm = TRUE),
            se_RT = std.error(mean_RT, na.rm = TRUE),
            sd_RT = sd(mean_RT, na.rm = TRUE),
            n = n()) %>%
  mutate(upper.ci = group_means + qt(1-(0.05/2), n-1)*se_RT,
         lower.ci = group_means - qt(1-(0.05/2), n-1)*se_RT)
kable(group_RT)

#congruency effect RTs
congruency_effect_RT <- congruency_RT %>%
  group_by(subject,switch_perc,congruency_perc) %>%
  mutate(sum_RT = sum(mean_RT, na.rm = TRUE)) %>%
  filter(stimCongruency == 'i') %>%
  mutate(reverse = sum_RT - mean_RT) %>%
  mutate(congruency_effect = mean_RT - reverse)

# conruency by congruency RT
congruency_effect_RT_cp_mean <- congruency_effect_RT %>%
  group_by(stimCongruency,congruency_perc) %>%
  summarise(mean_congruency_effect = mean(congruency_effect, na.rm = TRUE),
            se_ce = std.error(congruency_effect, na.rm = TRUE)) %>%
  mutate (ci = 1.96*se_ce)

#plot version 1
dot_plot <- ggplot(congruency_RT, aes(x = congruency_perc, y = mean_RT, fill = stimCongruency)) +
  scale_fill_manual(values = c('dodgerblue3', 'gold')) +
  coord_cartesian(ylim = c(rt_l1, rt_l2)) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', position = position_dodge(0.8), dotsize = dot_size, colour = 'white', binwidth = bin_width) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_blank(), #element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"),
        legend.key = element_rect(fill = c("dodgerblue3","gold")),
        legend.position = "none",
        strip.text.x = element_text(size = 16)
  ) +
  labs(x = "Congruency Proportion", 
       y = "RT (ms)") + 
  scale_x_discrete(labels = c('25% Incongruent' = "25%",'75% Incongruent' = "75%")) 

# putting group means on dot_rt
cn_cp_rt <- dot_plot + 
  stat_summary(
    fun.data = data_summary,
    position = position_dodge(0.8), 
    aes(shape = factor(stimCongruency)), size = sum_size) +
  scale_shape_manual(name = "switchType", values = c(16, 18))

ce_cp_rt <- ggplot(congruency_effect_RT_cp_mean, aes(x = congruency_perc, y = mean_congruency_effect)) + 
  geom_bar(position = "dodge", stat = "identity", fill = "gray", color = "gray") + 
  labs(x = "Congruency Proportion", 
       y = "Congruency Effect") + 
  geom_errorbar(aes(ymin = mean_congruency_effect - ci, ymax = mean_congruency_effect + ci), 
                width = .2, 
                position = position_dodge(.9)) + 
  coord_cartesian(ylim = c(rt_l3, rt_l4)) +
  scale_x_discrete(labels =  c('25% Incongruent' = "25%",'75% Incongruent' = "75%"))  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  scale_y_continuous(breaks = c(0, 100, 200))

#cn_cp_rt + ce_cp_rt + plot_layout(nrow = 2, heights = c(3,1))

# congruency by switch proportion RT
congruency_effect_RT_sp_mean <- congruency_effect_RT %>%
  group_by(stimCongruency,switch_perc) %>%
  summarise(mean_congruency_effect = mean(congruency_effect, na.rm = TRUE),
            se_ce = std.error(congruency_effect, na.rm = TRUE)) %>%
  mutate (ci = 1.96*se_ce)

#plot version 1
dot_plot <- ggplot(congruency_RT, aes(x = switch_perc, y = mean_RT, fill = stimCongruency)) +
  scale_fill_manual(values = c('dodgerblue3', 'gold')) +
  coord_cartesian(ylim = c(rt_l1, rt_l2)) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', position = position_dodge(0.8), dotsize = dot_size, colour = 'white', binwidth = bin_width) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_blank(), #element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"),
        legend.key = element_rect(fill = c("dodgerblue3","gold")),
        legend.position = "none",
        strip.text.x = element_text(size = 16)
  ) +
  labs(x = "Swich Proportion", 
       y = "RT (ms)") + 
  scale_x_discrete(labels = c('25% Switch' = "25%",'75% Switch' = "75%")) 

# putting group means on dot_rt
cn_sp_rt <- dot_plot + 
  stat_summary(
    fun.data = data_summary,
    position = position_dodge(0.8), 
    aes(shape = factor(stimCongruency)), size = sum_size) +
  scale_shape_manual(name = "switchType", values = c(16, 18))

ce_sp_rt <- ggplot(congruency_effect_RT_sp_mean, aes(x = switch_perc, y = mean_congruency_effect)) + 
  geom_bar(position = "dodge", stat = "identity", fill = "gray", color = "gray") + 
  labs(x = "Switch Proportion", 
       y = "Congruency\nEffect") + 
  geom_errorbar(aes(ymin = mean_congruency_effect - ci, ymax = mean_congruency_effect + ci), 
                width = .2, 
                position = position_dodge(.9)) + 
  coord_cartesian(ylim = c(rt_l3, rt_l4)) +
  scale_x_discrete(labels =  c('25% Switch' = "25%",'75% Switch' = "75%"))  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  scale_y_continuous(breaks = c(0, 100, 200))

#cn_sp_rt + ce_sp_rt + plot_layout(nrow = 2, heights = c(3,1))


layout <- "
AC
AC
AC
BD
"

#cn_cp_rt + ce_cp_rt + cn_sp_rt + ce_sp_rt + plot_layout(design = layout)


#### Final #####

layout <- "
AC
AC
AC
BD
EG
EG
EG
FH
"

#minor edits
sc_sp_rt = sc_sp_rt + theme(axis.title.x = element_blank())
sw_cp_rt = sw_cp_rt + theme(axis.title.y = element_blank())
sc_cp_rt = sc_cp_rt + theme(axis.title.x = element_blank(),
                            axis.title.y = element_blank())
cn_cp_rt = cn_cp_rt + theme(axis.title.y = element_blank())
ce_cp_rt = ce_cp_rt + theme(axis.title.y = element_blank())

#final accuracy plot
sw_sp_rt + sc_sp_rt + sw_cp_rt + sc_cp_rt + cn_sp_rt + ce_sp_rt + cn_cp_rt + ce_cp_rt  + plot_layout(design = layout)

#create plot for accuracy analysis
switch_acc <- df %>% 
  group_by(subject, switch_perc, congruency_perc, switchType) %>% 
  summarise(mean_acc = mean(acc,na.rm = TRUE))

# group acc
group_acc <- switch_acc %>%
  group_by(switchType,switch_perc,congruency_perc) %>%
  summarise(group_means = mean(mean_acc, na.rm = TRUE),
            se_acc = std.error(mean_acc, na.rm = TRUE),
            sd_acc = sd(mean_acc, na.rm = TRUE),
            n = n()) %>%
  mutate(upper.ci = group_means + qt(1-(0.05/2), n-1)*se_acc,
         lower.ci = group_means - qt(1-(0.05/2), n-1)*se_acc)
kable(group_acc)

#switch cost acc
switch_cost_acc <- switch_acc %>%
  group_by(subject,switch_perc,congruency_perc) %>%
  mutate(sum_acc = sum(mean_acc, na.rm = TRUE)) %>%
  filter(switchType == 's') %>%
  mutate(reverse = sum_acc - mean_acc) %>%
  mutate(switch_cost = mean_acc - reverse)

# switch cost by switch proportion
switch_cost_sp_acc_mean <- switch_cost_acc %>%
  group_by(switchType,switch_perc) %>%
  summarise(mean_switch_cost = mean(switch_cost, na.rm = TRUE),
            se_sc = std.error(switch_cost, na.rm = TRUE)) %>%
  mutate (ci = 1.96*se_sc)

#accuracy
dot_size = 20
bin_width = 0.001

dot_plot <- ggplot(switch_acc, aes(x = switch_perc, y = mean_acc, fill = switchType)) +
  scale_fill_manual(values = c('dodgerblue3', 'gold')) +
  coord_cartesian(ylim = c(acc_l1, acc_l2)) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', position = position_dodge(0.8), dotsize = dot_size, colour = 'white', binwidth = bin_width, width = 0.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_blank(), #element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"),
        legend.key = element_rect(fill = c("dodgerblue3","gold")),
        legend.position = "none",
        strip.text.x = element_text(size = 16)
  ) +
  labs(x = "Switch Proportion", 
       y = "Accuracy") + 
  scale_x_discrete(labels = c('25% Switch' = "25%",'75% Switch' = "75%")) 

# putting group means on dot_rt
sw_sp_acc <- dot_plot + 
  stat_summary(
    fun.data = data_summary,
    position = position_dodge(0.8), 
    aes(shape = factor(switchType)), size = sum_size) +
  scale_shape_manual(name = "switchType", values = c(15, 17))

sc_sp_acc <- ggplot(switch_cost_sp_acc_mean, aes(x = switch_perc, y = mean_switch_cost)) + 
  geom_bar(position = "dodge", stat = "identity", fill = "gray", color = "gray") + 
  labs(x = "Switch Proportion", 
       y = "Switch Cost") + 
  geom_errorbar(aes(ymin = mean_switch_cost - ci, ymax = mean_switch_cost + ci), 
                width = .2, 
                position = position_dodge(.9)) + 
  coord_cartesian(ylim = c(acc_l3, acc_l4)) +
  scale_x_discrete(labels = c('25% Switch' = "25%",'75% Switch' = "75%")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  scale_y_continuous(breaks = c(-.2,-.1, 0))

#sw_sp_acc + sc_sp_acc + plot_layout(nrow = 2, heights = c(3,1))


# switch cost by congruency proportion acc
switch_cost_cp_acc_mean <- switch_cost_acc %>%
  group_by(switchType,congruency_perc) %>%
  summarise(mean_switch_cost = mean(switch_cost, na.rm = TRUE),
            se_sc = std.error(switch_cost, na.rm = TRUE)) %>%
  mutate (ci = 1.96*se_sc)

#plot version
dot_plot <- ggplot(switch_acc, aes(x = congruency_perc, y = mean_acc, fill = switchType)) +
  scale_fill_manual(values = c('dodgerblue3', 'gold')) +
  coord_cartesian(ylim = c(acc_l1, acc_l2)) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', position = position_dodge(0.8), dotsize = dot_size, colour = 'white', binwidth = bin_width) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_blank(), #element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"),
        legend.key = element_rect(fill = c("dodgerblue3","gold")),
        legend.position = "none",
        strip.text.x = element_text(size = 16)
  ) +
  labs(x = "Congruency Proportion", 
       y = "Accuracy") + 
  scale_x_discrete(labels = c('25% Incongruent' = "25%",'75% Incongruent' = "75%")) 

# putting group means on dot_rt
sw_cp_acc <- dot_plot + 
  stat_summary(
    fun.data = data_summary,
    position = position_dodge(0.8), 
    aes(shape = factor(switchType)), size = sum_size) +
  scale_shape_manual(name = "switchType", values = c(15, 17))

sc_cp_acc <- ggplot(switch_cost_cp_acc_mean, aes(x = congruency_perc, y = mean_switch_cost)) + 
  geom_bar(position = "dodge", stat = "identity", fill = "gray", color = "gray") + 
  labs(x = "Congruency Proportion", 
       y = "Switch Cost") + 
  geom_errorbar(aes(ymin = mean_switch_cost - ci, ymax = mean_switch_cost + ci), 
                width = .2, 
                position = position_dodge(.9)) + 
  coord_cartesian(ylim = c(acc_l3, acc_l4)) +
  scale_x_discrete(labels = c('25% Incongruent' = "25%",'75% Incongruent' = "75%")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"),
        strip.background = element_blank(),
        strip.text.x = element_blank())  +
  scale_y_continuous(breaks = c(-.2,-.1, 0))

#sw_cp_acc + sc_cp_acc + plot_layout(nrow = 2, heights = c(3,1))

layout <- "
AC
AC
AC
BD
"

#sw_sp_acc + sc_sp_acc + sw_cp_acc + sc_cp_acc + plot_layout(design = layout)


#same thing but for congruency effects
congruency_acc <- df %>% 
  group_by(subject, switch_perc, congruency_perc, stimCongruency) %>% 
  summarise(mean_acc = mean(acc,na.rm = TRUE))

# group accs
group_acc <- congruency_acc %>%
  group_by(stimCongruency,switch_perc,congruency_perc) %>%
  summarise(group_means = mean(mean_acc, na.rm = TRUE),
            se_acc = std.error(mean_acc, na.rm = TRUE),
            sd_acc = sd(mean_acc, na.rm = TRUE),
            n = n()) %>%
  mutate(upper.ci = group_means + qt(1-(0.05/2), n-1)*se_acc,
         lower.ci = group_means - qt(1-(0.05/2), n-1)*se_acc)
kable(group_acc)

#congruency effect accs
congruency_effect_acc <- congruency_acc %>%
  group_by(subject,switch_perc,congruency_perc) %>%
  mutate(sum_acc = sum(mean_acc, na.rm = TRUE)) %>%
  filter(stimCongruency == 'i') %>%
  mutate(reverse = sum_acc - mean_acc) %>%
  mutate(congruency_effect = mean_acc - reverse)

# conruency by congruency acc
congruency_effect_acc_cp_mean <- congruency_effect_acc %>%
  group_by(stimCongruency,congruency_perc) %>%
  summarise(mean_congruency_effect = mean(congruency_effect, na.rm = TRUE),
            se_ce = std.error(congruency_effect, na.rm = TRUE)) %>%
  mutate (ci = 1.96*se_ce)

#plot version 1
dot_plot <- ggplot(congruency_acc, aes(x = congruency_perc, y = mean_acc, fill = stimCongruency)) +
  scale_fill_manual(values = c('dodgerblue3', 'gold')) +
  coord_cartesian(ylim = c(acc_l1, acc_l2)) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', position = position_dodge(0.8), dotsize = dot_size, colour = 'white', binwidth = bin_width) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_blank(), #element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"),
        legend.key = element_rect(fill = c("dodgerblue3","gold")),
        legend.position = "none",
        strip.text.x = element_text(size = 16)
  ) +
  labs(x = "Congruency Proportion", 
       y = "Accuracy") + 
  scale_x_discrete(labels = c('25% Incongruent' = "25%",'75% Incongruent' = "75%")) 

# putting group means on dot_acc
cn_cp_acc <- dot_plot + 
  stat_summary(
    fun.data = data_summary,
    position = position_dodge(0.8), 
    aes(shape = factor(stimCongruency)), size = sum_size) +
  scale_shape_manual(name = "switchType", values = c(16, 18))

ce_cp_acc <- ggplot(congruency_effect_acc_cp_mean, aes(x = congruency_perc, y = mean_congruency_effect)) + 
  geom_bar(position = "dodge", stat = "identity", fill = "gray", color = "gray") + 
  labs(x = "Congruency Proportion", 
       y = "Congruency Effect") + 
  geom_errorbar(aes(ymin = mean_congruency_effect - ci, ymax = mean_congruency_effect + ci), 
                width = .2, 
                position = position_dodge(.9)) + 
  coord_cartesian(ylim = c(acc_l3, acc_l4)) +
  scale_x_discrete(labels =  c('25% Incongruent' = "25%",'75% Incongruent' = "75%"))  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  scale_y_continuous(breaks = c(-.2,-.1, 0))

#cn_cp_acc + ce_cp_acc + plot_layout(nrow = 2, heights = c(3,1))

# congruency by switch propoaccion acc
congruency_effect_acc_sp_mean <- congruency_effect_acc %>%
  group_by(stimCongruency,switch_perc) %>%
  summarise(mean_congruency_effect = mean(congruency_effect, na.rm = TRUE),
            se_ce = std.error(congruency_effect, na.rm = TRUE)) %>%
  mutate (ci = 1.96*se_ce)

#plot version 1
dot_plot <- ggplot(congruency_acc, aes(x = switch_perc, y = mean_acc, fill = stimCongruency)) +
  scale_fill_manual(values = c('dodgerblue3', 'gold')) +
  coord_cartesian(ylim = c(acc_l1, acc_l2)) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', position = position_dodge(0.8), dotsize = dot_size, colour = 'white', binwidth = bin_width) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_blank(), #element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"),
        legend.key = element_rect(fill = c("dodgerblue3","gold")),
        legend.position = "none",
        strip.text.x = element_text(size = 16)
  ) +
  labs(x = "Swich Proportion", 
       y = "Accuracy") + 
  scale_x_discrete(labels = c('25% Switch' = "25%",'75% Switch' = "75%")) 

# putting group means on dot_acc
cn_sp_acc <- dot_plot + 
  stat_summary(
    fun.data = data_summary,
    position = position_dodge(0.8), 
    aes(shape = factor(stimCongruency)), size = sum_size) +
  scale_shape_manual(name = "switchType", values = c(16, 18))

ce_sp_acc <- ggplot(congruency_effect_acc_sp_mean, aes(x = switch_perc, y = mean_congruency_effect)) + 
  geom_bar(position = "dodge", stat = "identity", fill = "gray", color = "gray") + 
  labs(x = "Switch Proportion", 
       y = "Congruency\nEffect") + 
  geom_errorbar(aes(ymin = mean_congruency_effect - ci, ymax = mean_congruency_effect + ci), 
                width = .2, 
                position = position_dodge(.9)) + 
  coord_cartesian(ylim = c(acc_l3, acc_l4)) +
  scale_x_discrete(labels =  c('25% Switch' = "25%",'75% Switch' = "75%"))  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  scale_y_continuous(breaks = c(-.2,-.1, 0))

#cn_sp_acc + ce_sp_acc + plot_layout(nrow = 2, heights = c(3,1))


layout <- "
AC
AC
AC
BD
"

#cn_cp_acc + ce_cp_acc + cn_sp_acc + ce_sp_acc + plot_layout(design = layout)


#### Final #####

layout <- "
AC
AC
AC
BD
EG
EG
EG
FH
"

#minor edits
sc_sp_acc = sc_sp_acc + theme(axis.title.x = element_blank())
sw_cp_acc = sw_cp_acc + theme(axis.title.y = element_blank())
sc_cp_acc = sc_cp_acc + theme(axis.title.x = element_blank(),
                              axis.title.y = element_blank())
cn_cp_acc = cn_cp_acc + theme(axis.title.y = element_blank())
ce_cp_acc = ce_cp_acc + theme(axis.title.y = element_blank())

#final accuracy plot
sw_sp_acc + sc_sp_acc + sw_cp_acc + sc_cp_acc + cn_sp_acc + ce_sp_acc + cn_cp_acc + ce_cp_acc  + plot_layout(design = layout)

###Reaction Time (RT) Means 

omni_df <- df_trimmed %>% 
  group_by(subject, switchType, stimCongruency, switch_perc, congruency_perc) %>% 
  summarise(mean_RT = mean(RT,na.rm = TRUE))

#mean switch type RT
switchType_mean <- omni_df %>% 
  group_by(switchType) %>% 
  summarise(mean = mean(mean_RT,na.rm = TRUE), se = std.error(mean_RT)) %>% 
  mutate(upper_95 = mean + 1.96*se,
         lower_95 = mean - 1.96*se)

#mean congruency effect RT
congruency_mean <- omni_df %>% 
  group_by(stimCongruency) %>% 
  summarise(mean = mean(mean_RT,na.rm = TRUE), se = std.error(mean_RT)) %>% 
  mutate(upper_95 = mean + 1.96*se,
         lower_95 = mean - 1.96*se)

#mean switch costs RT by switch percentage
switchCost_mean1 <- omni_df %>% 
  group_by(subject, switch_perc, switchType) %>% 
  summarise(mean = mean(mean_RT))

switchCost_mean2 <- switchCost_mean1 %>% 
  group_by(subject, switch_perc) %>% 
  summarise(switch_cost = mean[2] - mean[1])

switch_perc_mean <- switchCost_mean2 %>% 
  group_by(switch_perc) %>% 
  summarise(mean_sc = mean(switch_cost), se = std.error(switch_cost)) %>% 
  mutate(upper_95 = mean_sc + 1.96*se,
         lower_95 = mean_sc - 1.96*se)

#mean congruency effect RT by congruency percantage
congruencyEffect_mean1 <- omni_df %>% 
  group_by(subject, congruency_perc, stimCongruency) %>% 
  summarise(mean = mean(mean_RT))

congruencyEffect_mean2 <- congruencyEffect_mean1 %>% 
  group_by(subject, congruency_perc) %>% 
  summarise(congruency_effect = mean[2] - mean[1])

congruency_perc_mean <- congruencyEffect_mean2 %>% 
  group_by(congruency_perc) %>% 
  summarise(mean_ce = mean(congruency_effect), se = std.error(congruency_effect)) %>% 
  mutate(upper_95 = mean_ce + 1.96*se,
         lower_95 = mean_ce - 1.96*se)


###Accuracy (Acc) Means
omni_df_acc <- df %>% 
  group_by(subject, switchType, stimCongruency, switch_perc, congruency_perc) %>% 
  summarise(mean_acc = mean(acc,na.rm = TRUE))

#mean switch type acc
switchType_mean <- omni_df_acc %>% 
  group_by(switchType) %>% 
  summarise(mean = mean(mean_acc,na.rm = TRUE), se = std.error(mean_acc)) %>% 
  mutate(upper_95 = mean + 1.96*se,
         lower_95 = mean - 1.96*se)

#mean congruency effect acc
congruency_mean <- omni_df_acc %>% 
  group_by(stimCongruency) %>% 
  summarise(mean = mean(mean_acc,na.rm = TRUE), se = std.error(mean_acc)) %>% 
  mutate(upper_95 = mean + 1.96*se,
         lower_95 = mean - 1.96*se)

#mean switch costs acc by switch percentage
switchCost_mean1 <- omni_df_acc %>% 
  group_by(subject, stimCongruency, switchType) %>% 
  summarise(mean = mean(mean_acc))

switchCost_mean2 <- switchCost_mean1 %>% 
  group_by(subject, stimCongruency) %>% 
  summarise(switch_cost = mean[1] - mean[2])

switch_perc_mean <- switchCost_mean2 %>% 
  group_by(stimCongruency) %>% 
  summarise(mean_sc = mean(switch_cost), se = std.error(switch_cost)) %>% 
  mutate(upper_95 = mean_sc + 1.96*se,
         lower_95 = mean_sc - 1.96*se)

#mean congruency effect acc by congruency percantage
congruencyEffect_mean1 <- omni_df_acc %>% 
  group_by(subject, congruency_perc, stimCongruency) %>% 
  summarise(mean = mean(mean_acc))

congruencyEffect_mean2 <- congruencyEffect_mean1 %>% 
  group_by(subject, congruency_perc) %>% 
  summarise(congruency_effect = mean[1] - mean[2])

congruency_perc_mean <- congruencyEffect_mean2 %>% 
  group_by(congruency_perc) %>% 
  summarise(mean_ce = mean(congruency_effect), se = std.error(congruency_effect)) %>% 
  mutate(upper_95 = mean_ce + 1.96*se,
         lower_95 = mean_ce - 1.96*se)UE))
describe(block_effects)