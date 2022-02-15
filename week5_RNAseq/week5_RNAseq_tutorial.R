BiocManager::install("SummarizedExperiment") #no
library(TCGAbiolinks)
library(SummarizedExperiment)

getwd()
setwd("./Desktop/qbio_data_analysis_christopherk/anaylsis_data/")
getwd()

query <- GDCquery(project = "TCGA-COAD", 
                  data.category = "Transcriptome Profiling",
                  data.type = "Gene Expression Quantification",
                  workflow.type = "HTSeq - Counts")
GDCdownload(query) # only need to download the data once! Comment this out once you have completed it once
sum_exp <- GDCprepare(query)

assays(sum_exp)$"HTSeq - Counts"[1:5, 1:5] #genes:patient IDs
head(rowData(sum_exp)) #rowData is the genes #ensembl ID:alternative ID
#colData gives information on individual patients #patient ID:clinical info

colData(sum_exp)[1:5, 25:29]

dim(assays(sum_exp)$"HTSeq - Counts")
dim(rowData(sum_exp))
dim(colData(sum_exp))

str(colData(sum_exp))
head(colData(sum_exp))
colnames(colData(sum_exp))

colData(sum_exp)$age_at_diagnosis[1:20]

colData(sum_exp)$age_at_diagnosis = colData(sum_exp)$age_at_diagnosis/365

colData(sum_exp)$age_category = ifelse(colData(sum_exp)$age_at_diagnosis<50, colData(sum_exp)$age_at_diagnosis<-"Young", colData(sum_exp)$age_at_diagnosis<-"Old")

head(rowData(sum_exp))
dim(rowData(sum_exp))

"KRAS" %in% rowData(sum_exp)$external_gene_name
"TP53" %in% rowData(sum_exp)$external_gene_name

KRAS_id_mask = (rowData(sum_exp))$external_gene_name == "KRAS"
sum(KRAS_id_mask)

ensembl_KRAS = rowData(sum_exp)$ensembl_gene_id[KRAS_id_mask]
print(ensembl_KRAS)

TP53_id_mask = (rowData(sum_exp))$external_gene_name == "TP53"
sum(TP53_id_mask)

ensembl_TP53 = rowData(sum_exp)$ensembl_gene_id[TP53_id_mask]
print(ensembl_TP53)

#ensembl are the rows in assays

min(assays(sum_exp)$"HTSeq - Counts"[ensembl_KRAS, ])
max(assays(sum_exp)$"HTSeq - Counts"[ensembl_KRAS, ])
summary(assays(sum_exp)$"HTSeq - Counts"[ensembl_TP53, ])

data_for_KRAS = assays(sum_exp)$"HTSeq - Counts"[ensembl_KRAS, ]
data_for_TP53 = assays(sum_exp)$"HTSeq - Counts"[ensembl_TP53, ]
plot(data_for_KRAS, data_for_TP53, xlab = "KRAS", ylab = "TP53")

bool_age_na = is.na(colData(sum_exp)$age_category)
num_na = sum(bool_age_na) 
num_na #4 NAs

age_cat_no_NAs = colData(sum_exp)$age_category[!bool_age_na]

length(age_cat_no_NAs)

dim(colData(sum_exp))[1]

x = length(age_cat_no_NAs)
y = num_na
z = dim(colData(sum_exp))[1]

x + y == z

dim(assays(sum_exp)$"HTSeq - Counts") #[56602, 521] <- 521 patients, 4 more than age_cat_no_NAs

identical(rownames(colData(sum_exp)), colnames(assays(sum_exp)$"HTSeq - Counts"))
gene_counts = assays(sum_exp)$"HTSeq - Counts"[ensembl_KRAS, !bool_age_na]
length(gene_counts)
identical(length(age_cat_no_NAs), length(gene_counts))

boxplot(
  gene_counts ~ age_cat_no_NAs, 
  xlab = "Age category", 
  ylab = "Gene count"
)

#Conclusions
#1. access through: assays(sum_exp)$"HTSeq - Counts" . Rows represent ensembl IDs.
#Columns represent patient data.
#2. access through: rowData(sum_exp). Rows represent ensembl IDs. Columns represent alternative IDs.
#assays(sum_exp)$"HTSeq - Counts" row = rowData(sum_exp) row
#3. access through: colData(sum_exp). Rows represent patient IDs. Columns represent clinical information.
#assays(sum_exp)$"HTSeq - Counts" column = colData(sum_exp) row
