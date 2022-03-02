#Exercise 1.1
BiocManager::install("maftools")
library(maftools)
library(TCGAbiolinks)
library(SummarizedExperiment)
setwd("./Desktop/qbio_data_analysis_christopherk/anaylsis_data/")

query <- GDCquery(project = "TCGA-COAD", 
                  data.category = "Transcriptome Profiling",
                  data.type = "Gene Expression Quantification",
                  workflow.type = "HTSeq - Counts")
sum_exp <- GDCprepare(query)

#Exercise 1.2
clinic <- data.table::fread("/Users/christopherkim/Desktop/qbio_data_analysis_christopherk/week4_clinical/clinic.csv",
                            data.table = F)
colnames(clinic)[ colnames(clinic) == "bcr_patient_barcode" ] <- "Tumor_Sample_Barcode"

#Exercise 1.3
#1
dim(clinic)
#length(colnames(clinic)) = 76

#2
length(colnames(clinic) == "bcr_patient_barcode")
colnames(clinic) == "bcr_patient_barcode"
#length(colnames(clinic) == "bcr_patient_barcode") = 76
#boolean data?

#3
sum(colnames(clinic) == "bcr_patient_barcode")
# 0 TRUEs

#Exercise 1.4
mutation_query <- GDCquery_Maf(tumor = "COAD", 
                               pipeline = "mutect2",
                               save.csv = TRUE)

maf_object <- read.maf(maf = mutation_query, 
                       clinicalData = clinic, 
                       isTCGA = TRUE)

getwd()
list.files()
setwd("./GDCdata/")
list.files()
maf_dataframe = data.table::fread("TCGA.COAD.mutect.03652df4-6090-4f5a-a2ff-ee28a37f9301.DR-10.0.somatic.maf.csv")

clinic <- data.table::fread("/Users/christopherkim/Desktop/qbio_data_analysis_christopherk/week4_clinical/clinic.csv",
                            data.table = F)
colnames(clinic)[ colnames(clinic) == "bcr_patient_barcode" ] <- "Tumor_Sample_Barcode"

maf_object <- read.maf(maf = maf_dataframe, 
                       clinicalData = clinic, 
                       isTCGA = TRUE)


colnames(clinic)[ colnames(clinic) == "bcr_patient_barcode" ] <- "Tumor_Sample_Barcode"
mutation_query <- GDCquery_Maf(tumor = "COAD", 
                               pipeline = "mutect2",
                               save.csv = TRUE)

maf_object <- read.maf(maf = mutation_query, 
                       clinicalData = clinic, 
                       isTCGA = TRUE)

#Exercise 2.1
maf_object
str(maf_object)
maf_object@data
maf_object@clinical.data
# both have a column for the Tumor Sample Barcode. Makes sense as
# the information of which patient the tumor sample came from and
# all the information about the tumor's mutation in its gene sequence are needed. 

#Exercise 3.1 
oncoplot(maf = maf_object,
         top = 20) 

library("ggplot2")

ggsave("/Users/christopherkim/Desktop/qbio_data_analysis_christopherk/week7_MAF/oncoplot.png")

#Exercise 3.2
# Most mutated gene = APC. Function of the APC gene is to
# provide a template to produce the APC protein, which is involved in
# suppressing tumors. Hence, mutations in the APC gene can contribute to proliferation. 

#Exercise 3.3
# 1. Write to clinic again
clinic = maf_object@clinical.data

# 2. Create the young_patients_ids vector
clinic$age_category = ifelse(clinic$age_at_initial_pathologic_diagnosis < 50, "young", "old")
young_patients_ids = c(clinic$Tumor_Sample_Barcode[clinic$age_category == "young"])

# 3. Create another 
young_maf = subsetMaf(maf = maf_object,
                      tsb = young_patients_ids)

# 4. Repeat steps 2-3 to create an old_maf! Can you do it in one line?
old_patients_ids = c(clinic$Tumor_Sample_Barcode[clinic$age_category == "old"])
old_maf = subsetMaf(maf = maf_object,
                    tsb = old_patients_ids)

#Exercise 3.4
#install.packages("ggplot2")     # Install ggplot2 package
library("ggplot2") 
coOncoplot(m1 = young_maf, 
           m2 = old_maf, 
           m1Name = "Young Patients", 
           m2Name = "Old Patients")
ggsave("/Users/christopherkim/Desktop/qbio_data_analysis_christopherk/week7_MAF/young_vs_old_oncoplot.png")

#Exercise 3.5
# Percentages for the old patients tend to be higher. Older patients possess higher numbers of
#mutated APC, TP53, and TTN genes. To expected since cellular mechanisms may not be efficient
#in older individuals. 

#Exercise 3.6
dev.off()
lollipopPlot(maf_object, gene = "MSH2")

ggsave("/Users/christopherkim/Desktop/qbio_data_analysis_christopherk/week7_MAF/lollipopPlot.png")

#Exercise 3.7
lollipopPlot2(m1 = young_maf, 
              m2 = old_maf, 
              m1_name = "Young Patients", 
              m2_name = "Old Patients",
              gene = "MSH2")

ggsave("/Users/christopherkim/Desktop/qbio_data_analysis_christopherk/week7_MAF/lollipopPlot2.png")

#Exercise 3.8
# 10 patients without mutations in both genes A and B.

#Exercise 3.9
b = 7
c = 2
d = 35
e = 37
f = 42

#Exercise 3.10
# geneA = TP53
geneA_maf <- subsetMaf(maf = maf_object,
                       genes = "TP53")

# geneB = KRAS
geneB_maf <- subsetMaf(maf = maf_object, 
                       genes = "KRAS")

#Exercise 3.11
# subsetMaf got the data from each specific gene. Provides the number of that gene 
# went through each type of mutation. 
# No, there are multiple types of mutations that geneA can go through. Looking at
# geneA_maf, it shows each  type of mutation and how many of geneA has that mutation. 
# It is not the same since not every patient has a mutation in geneA.

#Exercise 3.12
# 1. Access the barcodes of the patients with mutations in genes A and B
# bc stands for barcode
mut_bc_geneA = c(geneA_maf@clinical.data$Tumor_Sample_Barcode)
mut_bc_geneB = c(geneB_maf@clinical.data$Tumor_Sample_Barcode)

# 2. Get the lengths of these two vectors
num_mut_geneA = length(mut_bc_geneA)
num_mut_geneB = length(mut_bc_geneB)

# 3. Fill in the intersect here! Then get the nubmer of patients
mut_bc_geneAB = intersect(mut_bc_geneA, mut_bc_geneB)
num_mut_geneAB = length(mut_bc_geneAB)

#Exercise 3.13
num_mut_geneA_only = num_mut_geneA - num_mut_geneAB
num_mut_geneB_only = num_mut_geneB - num_mut_geneAB

#Exercise 3.14
num_neither_mutation = length(maf_object@clinical.data$Tumor_Sample_Barcode) - num_mut_geneAB - num_mut_geneA_only - num_mut_geneB_only

contig_table <- matrix(c(num_mut_geneAB, 
                         num_mut_geneB_only,
                         num_mut_geneA_only,
                         num_neither_mutation), 
                       nrow=2)
contig_table

#Exercise 3.15
fe_results <- fisher.test(contig_table)
fe_results
# p-value = 0.06543. As the p-value is small, this corresponds to a statistical
#significance between the mutation of gene A and B.

