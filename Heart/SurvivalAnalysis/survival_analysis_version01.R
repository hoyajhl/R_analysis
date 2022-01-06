getwd()
setwd("C:/Kaggle/heart_failure/archive")
heartdata<-read.csv('heart_failure_clinical_records_dataset.csv',encoding="UTF-8")
str(heartdata)
table(heartdata$DEATH_EVENT) #0->203,1->96
with(heartdata,hist(age))
library('survival') ##package for survival analysis
library('ggplot2')  ##package for survival visualization
library('survminer')##package for survival visualization

##Kaplan Meier
##Create categorical variable from continuous variable
heartdata$sodiumc <- ifelse(heartdata$serum_sodium >135 & heartdata$serum_sodium<145,
                            "Serum Sodium Normal", "Serum Sodium Abnormal")
heartdata$efraction <-ifelse(heartdata$ejection_fraction<=75 & heartdata$ejection_fraction>=41, 
                             "Ejection Normal", "Ejection Abnormal")
heartdata$sodiumc<- as.factor(heartdata$sodiumc)
heartdata$efraction<- as.factor(heartdata$efraction)
fit_sd<-survfit(Surv(time,DEATH_EVENT)~sodiumc, data=heartdata)
summary(fit_sd,times = c(0,50,100,150,200)) ##Number at risk depending on time
ggsurvplot(fit_sd,data=heartdata,xlab="Days", ggtheme=theme_minimal())
str(heartdata)
##Final model for risk factors of Kaplan Meier
heartdata$riskgp <-ifelse(heartdata$age<65
                          & heartdata$efraction=="Ejection Normal"
                          & heartdata$sodiumc=="Serum Sodium Normal", "Risk Low", "Risk High")
table(heartdata$riskgp)##High: 272, Low: 27
heartdata$riskgp<- as.factor(heartdata$riskgp)
fit<-survfit(Surv(time,DEATH_EVENT)~riskgp, data=heartdata)
ggsurvplot(fit,data=heartdata,xlab="Days", ggtheme=theme_minimal())
##Log rank of P-value for risk group
survdiff(Surv(time, DEATH_EVENT) ~ riskgp, data=heartdata)
## Difference between group seems significant engough, p= 0.009


###Cox proportional Hazard model 
cox.risk<-coxph(Surv(time,DEATH_EVENT) ~ riskgp, data=heartdata)
summary(cox.risk)
Individual_data=with(heartdata,data.frame(riskgp=c("Risk Low", "Risk High")))
Individual_data
fit0<- survfit(cox.risk,newdata=Individual_data)
riskfactor_plot=ggsurvplot(fit0,
                           Individual_data,
                 legend.labs=c("Risk Low","Risk High"),
                 ggtheme = theme_minimal())
riskfactor_plot


##Cox proportional Hazard model
heart.cox<-coxph(Surv(time,DEATH_EVENT)~age+sex+
                   anaemia+serum_sodium+creatinine_phosphokinase+
                   ejection_fraction+diabetes+high_blood_pressure+
                   smoking+serum_creatinine,data=heartdata)
summary(heart.cox)
##To check the assumption
## H0: follow proportional assumption vs H1: not
test.ph<- cox.zph(heart.cox)
test.ph ## p-vale>0.05, pass the assumption
ggcoxzph(test.ph) ##visualization of cox zph

##Result for hazard ratio visualization of Cox proportional Hazard model
ggforest(heart.cox,data=heartdata)
hf_plot=ggsurvplot(survfit(heart.cox,data=heartdata),
                   palette = 'skyblue',
                   ggtheme=theme_minimal())
hf_plot

##Create Individual data
Individual_data=with(heartdata,data.frame(anaemia=c(0,1,1),age=rep(mean(age),3), sex=c(0,0,1),
                                          serum_sodium=rep(mean(serum_sodium),3),ejection_fraction=rep(mean(ejection_fraction),3),
                                          creatinine_phosphokinase=rep(mean(creatinine_phosphokinase),3),
                                          diabetes=c(0,0,1),high_blood_pressure=c(0,0,1),smoking=c(0,0,1),
                                          serum_creatinine=rep(mean(serum_creatinine),3)))
##Fitting from our cox-model
fit=survfit(heart.cox,newdata=Individual_data)
Individual_sp_plot=ggsurvplot(fit, anaemia_data, 
                              legend.labs=c("Patient1","patient2","patient3"),
                              ggtheme = theme_minimal())
Individual_sp_plot

high_blood_pressure_data=with(heartdata,data.frame(high_blood_pressure=c(0,1), anaemia=c(0,0), age=rep(mean(age),2), sex=c(0,0),
                                                   serum_sodium=rep(mean(serum_sodium),2), ejection_fraction=rep(mean(ejection_fraction),2),
                                                   creatinine_phosphokinase=rep(mean(creatinine_phosphokinase),2),
                                                   diabetes=c(0,0),smoking=c(0,0),
                                                   serum_creatinine=rep(mean(serum_creatinine),2)))

##Strata depending on high_blood_pressure (no:0, yes:1)
fit1=survfit(heart.cox,newdata=high_blood_pressure_data)  
Highbloodpressure_plot=ggsurvplot(fit1, high_blood_pressure_data, legend.labs=c("High-Bloodpressure1","High_Bloodpressure2"),ggtheme = theme_minimal())
Highbloodpressure_plot
