if(!require(BiocManager)) install.packages("BiocManager")
if(!require(TCGAbiolinks)) BiocManager::install("TCGAbiolinks")
library(TCGAbiolinks)
getwd()
setwd("./Desktop/qbio_data_analysis_christopherk/anaylsis/")
getwd()
getwd()

clin_query <- GDCquery(project = "TCGA-COAD", data.category = "Clinical", file.type = "xml")
#GDCdownload(clin_query)
clinic <- GDCprepare_clinic(clin_query, clinical.info = "patient")
names(clinic)[names(clinic)=="days_to_last_followup"] <- "days_to_last_follow_up"

str(clinic)
head(clinic)

clinic$race_list = as.character(clinic$race_list)
#number_of_first_degree_relatives_with_cancer_diagnosis
#radiation_therapy
#days_to_death

colnames(clinic)
#age: $age_at_initial_pathologic_diagnosis
#weight: $weight

plot(clinic$age_at_initial_pathologic_diagnosis, clinic$weight, xlab = "age", ylab = "weight")

colnames(clinic)
unique(clinic$race_list)
boxplot(clinic$age_at_initial_pathologic_diagnosis~clinic$race_list, data=clinic, xlab = "race", 
        ylab = "age", las = 2, cex.axis = 0.5)

table(clinic$race_list, useNA = "ifany")
#unable to change " " to "no data"

min(clinic$age_at_initial_pathologic_diagnosis)
max(clinic$age_at_initial_pathologic_diagnosis)
mean(clinic$age_at_initial_pathologic_diagnosis)
median(clinic$age_at_initial_pathologic_diagnosis)
summary(clinic$age_at_initial_pathologic_diagnosis)

exercise_2.5 = c(ifelse(clinic$age_at_initial_pathologic_diagnosis < 50, 
       is.na(clinic$age_at_initial_pathologic_diagnosis), clinic$age_at_initial_pathologic_diagnosis))
table(excercise_2.5, useNA = "ifany")

young_patient_ids = ifelse(clinic$age_at_initial_pathologic_diagnosis < 50, clinic$patient_id, 
                           is.na(clinic$age_at_initial_pathologic_diagnosis))
young_patient_ids = young_patient_ids[!is.na(young_patient_ids)]
unique(young_patient_ids)
young_patient_ids = young_patient_ids[!isFALSE(young_patient_ids)]
str(young_patient_ids)

clinic$age_category = ifelse(clinic$age_at_initial_pathologic_diagnosis < 50, "Young", "Old")
clinic[1,1] #top left entry of dataframe
clinic[1,] #first row
clinic[2:5,] #rows 2-5
clinic[,3] #third column

clinic_young = data.frame(clinic$age_category, clinic$age_at_initial_pathologic_diagnosis < 50)
clinic_old = data.frame(clinic$age_category, clinic$age_at_initial_pathologic_diagnosis >= 50)

install.packages("survival")
install.packages("survminer")
library("survival")
library("survminer")

clinic$days_to_death = ifelse(is.na(clinic$days_to_death), clinic$days_to_last_follow_up, 
                              clinic$days_to_death)
str(clinic$vital_status)
clinic$death_event = ifelse(clinic$vital_status == "Dead", 1, 0)

surv_object <- Surv(time = clinic$days_to_death, 
                    event = clinic$death_event)

race_fit <- surv_fit( surv_object ~ clinic$race_list, data = clinic )

survplot = ggsurvplot(race_fit, 
                      pval=TRUE, 
                      ggtheme = theme(plot.margin = unit(c(1,1,1,1), "cm")), 
                      legend = "right")

p = survplot$plot + 
  theme_bw() +  # changes the appearance to be a bit prettier
  theme(axis.title = element_text(size=20), # increase font sizes
        axis.text = element_text(size=16),
        legend.title = element_text(size=14),
        legend.text = element_text(size=12))
p

ggsave("../week4_clinical/kmplot_by_race.png", plot = p, width = 12, height = 9)
getwd()
write.csv(clinic, "/Users/christopherkim/Desktop/qbio_data_analysis_christopherk/anaylsis_data", row.names = F)
