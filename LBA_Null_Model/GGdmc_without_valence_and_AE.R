library(tidyverse)
library(dplyr)
library(MASS)
library(readr)
library(readxl)
library(ggdmc)
library(rtdists)



set1 <- read_excel("D:/OneDrive/EladsB/CognitiveNeuroscience/nachshon/Experiment_2/LBA_25_02_2022/sub_set1.xlsx")
set2 <- read_excel("D:/OneDrive/EladsB/CognitiveNeuroscience/nachshon/Experiment_2/LBA_25_02_2022/sub_set2.xlsx")
set3 <- read_excel("D:/OneDrive/EladsB/CognitiveNeuroscience/nachshon/Experiment_2/LBA_25_02_2022/sub_set3.xlsx")
set4 <- read_excel("D:/OneDrive/EladsB/CognitiveNeuroscience/nachshon/Experiment_2/LBA_25_02_2022/sub_set4.xlsx")
set5 <- read_excel("D:/OneDrive/EladsB/CognitiveNeuroscience/nachshon/Experiment_2/LBA_25_02_2022/sub_set5.xlsx")
set6 <- read_excel("D:/OneDrive/EladsB/CognitiveNeuroscience/nachshon/Experiment_2/LBA_25_02_2022/sub_set6.xlsx")
set7 <- read_excel("D:/OneDrive/EladsB/CognitiveNeuroscience/nachshon/Experiment_2/LBA_25_02_2022/sub_set7.xlsx")
set8 <- read_excel("D:/OneDrive/EladsB/CognitiveNeuroscience/nachshon/Experiment_2/LBA_25_02_2022/sub_set8.xlsx")


all_data <- rbind(set1,set2,set3,set4,set5,set6,set7,set8)




all_data$participant <- rep(1:70,each = 240)

all_data <- all_data[c('response_emo_resp_keys','correct_emo_resp_keys','visualhint','response_time_emo_resp_keys',
                       'response_time_vis_resp_keys','correct_vis_resp_keys','emotion','participant','emo_stim_name')]

all_data <- na.omit(all_data)

for (i in 1:nrow(all_data)) {
  if (grepl('p',all_data$emo_stim_name[i]) & all_data$response_emo_resp_keys[i] == 'p'){ #If positive stim - pressed 'P'
    all_data$correct_emo_resp_keys[i] <- 1
  } else if (grepl('p',all_data$emo_stim_name[i]) & all_data$response_emo_resp_keys[i] == 'q') {#if positive stim - pressed Q
    all_data$correct_emo_resp_keys[i] <- 0
  } else if (grepl('n',all_data$emo_stim_name[i]) & all_data$response_emo_resp_keys[i] == 'q') {#if negative stim - pressed 'Q'
    all_data$correct_emo_resp_keys[i] <- 1
  } else if (grepl('n',all_data$emo_stim_name[i]) & all_data$response_emo_resp_keys[i] == 'p') { #if negative stim - pressed 'P'
    all_data$correct_emo_resp_keys[i] <- 0
  }
}


all_data <- na.omit(all_data)


#### Data Pre-processing ###############
# 1) Finding participants that responded in the opposite (supposed) evoked emotion:
all_data <- subset(all_data, participant != 29 & participant != 42 & participant != 68 & participant != 52 & participant != 35 
                   & participant != 59)
# participant != 35)# & participant !=59) #& participant != 28 & participant != 58)

#all_data <- all_data %>% group_by(participant,emotion) %>% filter(mean(correct_emo_resp_keys) > 0.5)
#View(all_data %>% group_by(participant,emotion) %>% summarise(CR = mean(correct_emo_resp_keys))) 



#2) Finding pictures whos average error responses is higher then 50%:

all_data <- subset(all_data, emo_stim_name != 'p_94' & emo_stim_name != 'p_42' & emo_stim_name != 'p_108' & emo_stim_name != 'p_12' & emo_stim_name != 'p_21' & emo_stim_name != 'p_79' & emo_stim_name != 'p_39' & emo_stim_name != 'p_22' & emo_stim_name != 'p_80' & emo_stim_name != 'p_6')


#all_data <- all_data %>% group_by(emo_stim_name) %>% filter(mean(correct_emo_resp_keys) > 0.5)
#View(all_data %>% group_by(emo_stim_name) %>% summarize(CR = mean(correct_emo_resp_keys)))
sort(unique(all_data$emo_stim_name))



# 3) Examining the RT density function:
ggplot(all_data,aes(x=response_time_emo_resp_keys)) + geom_density() + ggtitle('Before Triming') #need to trim 0.005 of trials with slow RT


all_data_new <- subset(all_data,response_time_emo_resp_keys <= 6150 & response_time_emo_resp_keys >= 200)
1 - nrow(all_data_new)/nrow(all_data)
all_data <- all_data_new



all_data <- all_data  %>% group_by(participant,emotion,visualhint) %>% 
  mutate(rt_z_score = scale(response_time_emo_resp_keys))
all_data <- subset(all_data,rt_z_score < 3.5 & rt_z_score > -3.5)


#ggplot(all_data,aes(x=response_time_emo_resp_keys)) + geom_density() + ggtitle('After Triming')


all_data$visualhint <-ifelse(all_data$visualhint == 'fs_shape_h' | all_data$visualhint == 'fs_color_h', 'easy','hard')




# LBA Data Preperation ------------------------------------------------------------------

# preparing DF for ggdmc - S = stimuli, s - subject
d_dmc<-all_data %>%
  mutate(RT=response_time_emo_resp_keys/1000)  %>% #must be in scales of seconds.
  mutate(s=as.factor(participant)) %>% #this is the subject, define as factor.
  mutate(S = if_else(emotion == 'positive',"s1","s2")) %>% #this is the REAL correct responses (Not to insert it? Ella took only the negative stim..).
  #                   Should I pet the actuall correct keys??? = no, just the  0 and 1
  mutate(R=if_else(response_emo_resp_keys == "p","r1","r2")) %>% #this is the reported by subject responses
  # (r1 =correct response, r2=incorrect response)
  mutate(MP=if_else(visualhint=="easy","easy","hard")) #easy/hard manipulation.
d_dmc<-d_dmc[,c("s","S","R","RT","MP")]



# Un-comment these if it gives you error while modeling:
d_dmc_z <- d_dmc %>% group_by(s,S,MP) %>% mutate(Zrt = scale(RT))
d_dmc_z <- subset(d_dmc_z,Zrt < 3.5 & Zrt > -3.5)




# Mean_V parameter estimation (copied with changes from Ella's code) ------------------------------------------------------------------

par(mar=c(2,2,2,2))
dat1<-d_dmc_z
#modeling
t0_up<-mean(dat1$RT)*0.6
#specify the model - Remember that I use the data with only negative trials.
model <- BuildModel(
  p.map     = list(A = "1", B = "1", t0 = "1", mean_v = "M", #what's the ''M'' does?? => true/false accumulator seperator...
                   sd_v = "1", st0 = "1"),
  match.map = list(M = list(s1 = "r1", s2 = "r2")),
  factors   = list(S = c("s1", "s2")),
  constants = c(sd_v = 1, st0 = 0),
  responses = c("r1", "r2"),
  type      = "norm")

  

#number of parameters
npar <- length(GetPNames(model))

#data model integration
dat1$s<-as.factor(dat1$s)
dmi <- BuildDMI(dat1, model)

#parameters for prior dist population mean and sd (scale) and beta
pop.mean<-c(2,2,.3,2,.5)
names(pop.mean)<-GetPNames(model)
pop.scale<-c(.6,.6,.1,.8,.8)
names(pop.scale)<-GetPNames(model)
prior.beta <- c(rep(1,npar))
names(prior.beta)<-GetPNames(model)



#prior
p.prior<-BuildPrior(
  dists = rep("tnorm", npar),
  p1    = pop.mean,
  p2    = pop.scale,
  lower = c(rep(0,npar)),
  upper = pop.mean+pop.scale*8)

mu.prior <- BuildPrior(
  dists = rep("tnorm", npar),
  p1    = pop.mean,
  p2    = pop.scale,
  lower = c(rep(0,npar)),
  upper = pop.mean+pop.scale*8)

sig<-c(rep(1,npar))

names(sig)<-GetPNames(model)



sigma.prior<- BuildPrior(
  
  dists = rep("beta", npar),
  
  p1    = sig,
  
  p2    = sig,
  
  lower = c(rep(0,npar)),
  
  upper = c(rep(5,npar)))


#incpect prior
plot(p.prior)
plot(mu.prior)
plot(sigma.prior)

#creat one set of priors for hierarchical fit
priors <- list(pprior=p.prior, location=mu.prior, scale=sigma.prior)




#burning in period
fit0 <- StartNewsamples(dmi, priors, ncore = 4, nmc=2e3)  # 2e3


##fit the model = the actuall running ####
fit <- run(fit0, 6e3, ncore = 4, thin = 12) # 6e3

#evaluate model fit
GR_dr<-hgelman(fit,verbose = T)
plot(fit)
dr_sum<-summary(fit)


write.csv(GR_dr,file="GelmanRubin_without_val_AE_DR_all.csv")
write.csv(dr_sum,file="summary_without_val_AE_DR_all.csv")



