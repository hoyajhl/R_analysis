# AIM: How to use pipeline properly 
## EDA for censored data

# Sample 3 + 3 points from death/censored groups, and plot them for comparison
library(dplyr)
library(tidyr)
plot_censoring <- heartdata %>% 
  group_by(DEATH_EVENT) %>% 
  sample_n(3) %>% 
  ungroup() %>% ##ungroup() to realease the cluster
  select(time, DEATH_EVENT)
plot_censoring## Censoring Plot (Visualization)
plot_censoring %>% ##Using mutate function to add variable
  mutate(time_start = 0,
         case_id = factor(c(1:nrow(plot_censoring))),
         death_event = factor(ifelse(DEATH_EVENT == 1, "death", "censored"))) %>% 
  pivot_longer(cols = c(time, time_start), ##time_start~time
               names_to = "source",
               values_to = "time") %>%
  ggplot(aes(x = time, y = case_id, group = factor(case_id))) + 
  geom_bar(stat = "Identity", aes(fill = death_event), colour = "blue", width = 0.2) +
  ggtitle("Time till Death/Censoring (6 sampled cases)") + 
  theme(plot.title = element_text(size = 10), 
        legend.title = element_text(size = 10), 
        legend.text = element_text(size = 10), 
        axis.title = element_text(size = 10))
##The above plot shows 6 patients, 3 of whom died and 3 of whom were censored 
## (dropped out of the study before death could be observed).
##The fact that some patients were censored does not mean that
##their data is completely missing. We still know that they did not die, 
##at least till the time they were censored!

# Censoring vs Death
options(repr.plot.width = 10, repr.plot.height = 4)

heartdata%>%
  mutate(event_type = factor(ifelse(DEATH_EVENT == 1, "death", "censored"))) %>%
  group_by(event_type) %>%
  tally(name = "count") %>%
  ggplot(aes(x = event_type, y = count)) + 
  geom_bar(stat = "Identity", fill = "black", width = 0.4, colour = "blue") +
  ggtitle("Censored vs Deaths") + 
  theme(plot.title = element_text(size = 10),  
        axis.title = element_text(size = 10))

### Distribution of time-to-event by event type
options(repr.plot.width = 12, repr.plot.height = 5)

heartdata %>%
  mutate(event_type = factor(ifelse(DEATH_EVENT == 1, "death", "censored"))) %>%
  select(event_type, time) %>%
  ggplot(aes(x = time, colour = event_type)) + 
  geom_density() + 
  ggtitle("Distribution of time-to-event by type of event (Censored vs Deaths)") + 
  theme(plot.title = element_text(size = 10),  
        axis.title = element_text(size = 10), 
        legend.title = element_text(size = 10), 
        legend.text = element_text(size = 10))

###Cox Proportional Hazard Model
## Change columns into factors and scale columns to enable better model fit
all_df <- heartdata %>% 
  mutate(anaemia = factor(ifelse(anaemia == 1, "anaemic", "non-anaemic"),levels = c("non-anaemic", "anaemic")),
         diabetes = factor(ifelse(diabetes == 1, "diabetic", "non-diabetic"),levels = c("non-diabetic", "diabetic")),
         high_blood_pressure = factor(ifelse(high_blood_pressure == 1, "high-bp", "non-high-bp"), levels = c("non-high-bp", "high-bp")),
         sex = factor(ifelse(sex == 0, "female", "male"), levels = c("female", "male")),
         smoking = factor(ifelse(smoking == 0, "non-smoker", "smoker"), levels = c("non-smoker", "smoker")),
         platelets = platelets/1e4, 
         creatinine_phosphokinase = creatinine_phosphokinase/1e3)
all_df %>% head
# Cox proportional hazard model
cox_model <- coxph(Surv(time, DEATH_EVENT) ~ age + anaemia + creatinine_phosphokinase 
                   + diabetes + ejection_fraction +high_blood_pressure + platelets 
                   + smoking + sex, data = all_df)
summary(cox_model) # Concordance= 0.706  (se = 0.029 )
# Plot the survival for a population with mean value of covariates
ggsurvplot(survfit(cox_model), data = all_df,xlab="Days", ggtheme=theme_minimal())
summary(cox_model,times=c(0,100,200,300)) ##Number at risk depending on time

## **Comparing the survival of 2 groups of populations 
## A data-set with 2 rows. 1 row per factor level
## Numerical covariates are set to median value.
compare_groups <- tibble(age = rep(median(all_df$age), 2), 
                         anaemia = factor(c("anaemic", "non-anaemic"), levels = levels(all_df$anaemia)),
                         creatinine_phosphokinase = rep(median(all_df$creatinine_phosphokinase), 2),
                         diabetes = factor(c("diabetic", "non-diabetic"), levels = levels(all_df$diabetes)),
                         ejection_fraction = rep(median(all_df$ejection_fraction), 2),
                         high_blood_pressure = factor(c("high-bp", "non-high-bp"), levels = levels(all_df$high_blood_pressure)),
                         platelets = rep(median(all_df$platelets), 2), 
                         smoking = factor(c("smoker", "non-smoker"), levels = levels(all_df$smoking)), 
                         sex = factor(c("male", "female"), levels = levels(all_df$sex)))
compare_groups
##Prediction using compare_groups
options(repr.plot.width = 18, repr.plot.height = 8)
ggsurvplot(survfit(cox_model, data = compare_groups, newdata=compare_groups),
           legend.labs=c("High-risk","Low_risk"),
           palette="simpsons",
           xlab="Days", ggtheme=theme_minimal())

##Test assumptions of Hazard Model
### Using the scaled schoenfeld residual test
cox.zph(cox_model)
## Then, plot the Schoenfeld residuals 
options(repr.plot.width = 18, repr.plot.height = 12)
ggcoxzph(cox.zph(cox_model))
##It doesn't look like any of the covariates have time-varying residuals from the plots


# Predict the new survival function for censored patients

# Given the coxph model, calculate the survival probability of each censored patient at the time they got censored
censored_patients <- all_df %>%
  filter(DEATH_EVENT == 0) %>%
  mutate(last_survival = exp(-1 * predict(cox_model, newdata = ., type = "expected")), 
         join_col = 1, patient_id = seq(1, nrow(.), 1))

censored_patients
# Since we know that the patients were alive at the time they got censored,
# use that information to update
# the survival probability for each patient (calculated in the previous code chunk)
censored_patients %<>%
  inner_join(tibble(time_pred = seq(1, 300, 1), join_col = 1),by = "join_col") %>%
  rename(censored_time = time,time = time_pred) %>%
  mutate(original_survival = exp(-1 * predict(cox_model, newdata = ., type = "expected")), 
         updated_survival = case_when(time <= censored_time ~ 1,
                                      TRUE ~ original_survival/last_survival)) 
## time:1-30, censored_time:30 -> survival prob,1 

censored_patients %>%
  select(patient_id, censored_time, time, original_survival, updated_survival) %>%
  head(15)

censored_patients %>%
  select(patient_id, time, original_survival, updated_survival) %>%
  filter(patient_id < 3) %>%
  pivot_longer(cols = original_survival:updated_survival, 
               names_to = "source", 
               values_to = "survival")%>%
  ggplot(aes(x = time, y = survival, colour = source)) + 
  geom_line() + 
  facet_wrap(vars(patient_id)) + 
  ggtitle("Original survival curves vs Updated survival curves - 4 patients") + 
  theme_minimal()+
  theme(plot.title = element_text(size = 10),  
        axis.title = element_text(size = 10), 
        legend.title = element_text(size = 10), 
        legend.text = element_text(size = 10), 
        legend.position = "bottom")