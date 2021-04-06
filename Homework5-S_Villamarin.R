#################################
### Homework5_Data_Vis_ggplot ###
### Santiago Villamarin       ###
#################################

#Load libraries
library(tidyverse)
library(ggforce) 
library(ggsci)
library(patchwork)
library(Hmisc)

#set working directory
setwd("\Users\evillamarin\Documents\DataScienceII\1-ggplot")

#Load dataset
bloom_df <- read.csv("bloom_df.csv")

#Make a dataframe from the dataset
bloom_df[1:5,]
str(bloom_df)

# Task 1: Continuous vs. continuous plot
# Create two scatterplots of logbodysize vs. trophic_position grouped by reg
# First plot : a 2 trend lines (method = 'lm'), one for each reg variable

ggplot(data=bloom_df,aes(logbodysize, trophic_position, color=reg)) +
  geom_point() +
  geom_smooth ()

# Second plot : a single trend line for the whole model

ggplot(data=bloom_df,aes(logbodysize, trophic_position)) +
  geom_point(aes(color=reg, shape=reg), size=3) +
  stat_smooth (method="lm", linetype="twodash", color="mediumblue", size=1.5)

# Task 2: Continuous vs. categorical
# 2 panels: trophic_position and logbodysize
# x axis: reg
# y axis: value of trophic_position and logbodysize

#Plot1
# bar and error bars (mean and 95% conf. int,)
# First summarize statistical data, mean, sd, N, 95% CI) for trophic_position.

trophic_position_df <- bloom_df %>%
  group_by(reg) %>%
  summarise(mean = mean(trophic_position, na.rm = TRUE), #trophic_position mean
            sd = sd(trophic_position, na.rm = TRUE), #trophic_position standard deviation
            n = n()) %>% #trophic_position count
  mutate(se = sd / sqrt(n), #trophic_position standard error
         ci = 1.96 * se) #trophic_position 95% confidence interval

trophic_position_df$reg2 <- 'TP'

# Second, summarize statistical data, mean, sd, N, 95% CI) for logbodysize

logbodysize_df <- bloom_df %>%
  group_by(reg) %>%
  summarise(mean = mean(logbodysize, na.rm = TRUE), #logbodysize mean
            sd = sd(logbodysize, na.rm = TRUE), #logbodysize standard deviation
            n = n()) %>% #logbodysize count
  mutate(se = sd / sqrt(n), #logbodysize standard error
         ci = 1.96 * se) #logbodysize 95% confidence interval

logbodysize_df$reg2 <- 'BZ'

# Bind both data frames together
ecol_sum_df <- rbind(trophic_position_df,logbodysize_df)
ecol_sum_df

# Make trophic_position and logbodysize long formats
ecol_long_df <- bloom_df %>%
  gather(key=reg2,value=mean,c(trophic_position,logbodysize))

# Barchart and error bars, mean and 95% CI
ggplot(data=ecol_long_df,aes(x=reg, y=mean, fill=reg2)) +
  facet_wrap(~reg2, nrow=1, scales="free") +
  xlab("Anadromous and Catadromous Fish") + ylab("Mean Body Size and Trophic Position") +
  ggtitle("Fish Tolerance")+
  stat_summary(fun = mean, geom = "bar") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar",color="black") +
  theme_classic()+
  theme(legend.position = "None")

# Scatter plot and error bars, mean and 95% CI
ggplot(data=ecol_long_df,aes(x=reg, y=mean, colour=reg2)) +
  facet_wrap(~reg2, nrow=1, scales="free") +
  geom_point(size=3) +
  xlab("Anadromous and Catadromous Fish") + ylab("Mean Body Size and Trophic Position") +
  ggtitle("Fish Tolerance")+
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar",color="black")+
  stat_summary(fun = mean, geom = "point", size=5, colour="black")+
  theme_classic()+
  theme(legend.position = "None")

# Boxplot and error bars, mean and 95% CI
ggplot(data=ecol_long_df,aes(x=reg, y=mean, fill=reg2)) +
  facet_wrap(~reg2, nrow=1, scales="free") +
  geom_boxplot()+
  xlab("Anadromous and Catadromous Fish") + ylab("Mean Body Size and Trophic Position") +
  ggtitle("Fish Tolerance")+
  theme_classic()+
  theme(legend.position = "None")

# Raw data 
ggplot(data=ecol_long_df,aes(x=reg, y=mean, colour=reg2)) +
  facet_wrap(~reg2, nrow=1, scales="free") +
  geom_point(size=4) +
  geom_sina(size=3) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar",color="black")+
  stat_summary(fun = mean, geom = "point", size=5, colour="black")+
  xlab("Anadromous and Catadromous Fish") + ylab("Mean Body Size and Trophic Position") +
  ggtitle("Fish Tolerance")+
  theme_classic()+
  theme(legend.position = "None")

#Patchwork
ggplot(data=ecol_long_df,aes(x=reg, y=mean, fill=reg2)) +
  facet_wrap(~reg2, nrow=1, scales="free") +
  xlab("Anadromous and Catadromous Fish") + ylab("Mean Body Size and Trophic Position") +
  ggtitle("Fish Tolerance")+
  geom_sina(size=4, pch=21)+
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar",color="black", width=0.3, size=1.4) +
  stat_summary(fun = mean, geom = "point",size=7,colour='black',pch=22,fill='white') +
  scale_fill_npg() +
  theme_classic()+
  theme(legend.position = 'None', 
        plot.title = element_text(size = 26, colour="black",face = "bold"),
        axis.text = element_text(size=18),
        axis.title = element_text(size = 22, colour="black",face = "bold"),
        panel.border = element_rect(size = 1.5, colour = "black"),
        legend.title = element_text(size = 22, colour="black",face = "bold",vjust = 1),
        legend.text = element_text(size=18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(size=22, face="bold"),
        strip.background = element_rect(size=1.5,colour="#333333",fill="#CCCCCC"))
