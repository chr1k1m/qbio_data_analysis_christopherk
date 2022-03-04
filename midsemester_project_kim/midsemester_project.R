library(TCGAbiolinks)
library(SummarizedExperiment)
setwd("./Desktop/qbio_data_analysis_christopherk/anaylsis_data/")

query <- GDCquery(project = "TCGA-COAD", 
                  data.category = "Transcriptome Profiling",
                  data.type = "Gene Expression Quantification",
                  workflow.type = "HTSeq - Counts")
sum_exp <- GDCprepare(query)

# Locating EGFL6 in rowData(sum_exp)$external_gene_name
colnames(rowData(sum_exp))
head(rowData(sum_exp))
"EGFL6" %in% rowData(sum_exp)$external_gene_name

EGFL6_id_mask = (rowData(sum_exp))$external_gene_name == "EGFL6"
ensembl_EGFL6 = rowData(sum_exp)$ensembl_gene_id[EGFL6_id_mask]
summary(assays(sum_exp)$"HTSeq - Counts"[ensembl_EGFL6, ])

age_category = ifelse(colData(sum_exp)$age_at_index < 50, "Young", "Old")
bool_age_NA = is.na(age_category)
age_cat_no_NAs = age_category[!bool_age_NA]

gene_counts = assays(sum_exp)$"HTSeq - Counts"[ensembl_EGFL6, !bool_age_NA]

#Creating box plot of gene count vs age category
boxplot(
  gene_counts ~ age_cat_no_NAs, 
  xlab = "Age category", 
  ylab = "Gene count"
)

library(survival)
library(survminer)

colnames(colData(sum_exp))
colData(sum_exp)$days_to_death
colData(sum_exp)$days_to_last_follow_up

#Cleaning survival time
survival_time_all = ifelse(is.na(colData(sum_exp)$days_to_death), 
                                        colData(sum_exp)$days_to_last_follow_up, 
                                        colData(sum_exp)$days_to_death)
#Obtaining only the survival time of relevant subjects
survival_time = survival_time_all[!is.na(age_category)]

#Confirming indexing of line 47
sum(is.na(survival_time_all)) #survival_time_all: NA = 3
sum(is.na(survival_time)) #survival_time: NA = 1
table(survival_time_all) #survival_time_all: 0 = 22
table(survival_time) #survival_time: 0 = 22

#Converting "Dead" and "Alive" into 1 and 0 respectively
death_event_all = ifelse(colData(sum_exp)$vital_status == "Dead", 1, 0)
#Obtaining only the survival time of relevant subjects
death_event = death_event_all[!is.na(age_category)]

#Creating KM plot
surv_object <- Surv(time = survival_time, 
                    event = death_event)

age_fit <- surv_fit( surv_object ~ age_cat_no_NAs, data = colData(sum_exp))

survplot = ggsurvplot(age_fit, 
                      pval=TRUE, 
                      ggtheme = theme(plot.margin = unit(c(1,1,1,1), "cm")), 
                      legend = "right")

p = survplot$plot + 
  theme_bw() +  
  theme(axis.title = element_text(size=20),
        axis.text = element_text(size=16),
        legend.title = element_text(size=14),
        legend.text = element_text(size=12))
p

