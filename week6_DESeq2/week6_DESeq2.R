BiocManager::install("DESeq2")
library(TCGAbiolinks)
library(SummarizedExperiment)
library(DESeq2)
getwd()
setwd("./Desktop/qbio_data_analysis_christopherk/anaylsis_data/")

query <- GDCquery(project = "TCGA-COAD", 
                  data.category = "Transcriptome Profiling",
                  data.type = "Gene Expression Quantification",
                  workflow.type = "HTSeq - Counts")
sum_exp <- GDCprepare(query)

dim(assays(sum_exp)$"HTSeq - Counts")
dim(rowData(sum_exp))
dim(colData(sum_exp))
#Exercise 1.2
# 1. Identify which patients have NA in their age as before
colnames(colData(sum_exp))
colData(sum_exp)$age_at_index
sum(is.na(colData(sum_exp)$age_at_index))

# 2. Make a copy of the clinical and counts data; fill in here
patients_data = colData(sum_exp)
counts = assays(sum_exp)$"HTSeq - Counts"

# 4. Remove the NA patients from both patients_data and counts
patients_data = colData(sum_exp)[!is.na(colData(sum_exp)$age_at_index), ]
counts = assays(sum_exp)$"HTSeq - Counts"[ , !is.na(colData(sum_exp)$age_at_index)]

# 5. Create the age_category column
patients_data$age_category = ifelse(patients_data$age_at_index<50, "Young", "Old")

# 6. Turn the age_category column into a factor
patients_data$age_category = factor(patients_data$age_category, levels = c("Young", "Old"))

#Exercise 1.3
# 2. Compute the sum across the row of the counts data frame
counts_row_sums = rowSums(counts)

# 3. Identify the genes that have fewer than 10 reads
low_counts_mask = counts_row_sums>=10
sum(low_counts_mask)
sum(!low_counts_mask)
length(low_counts_mask)
low_counts_mask

# 4. Remove these lowly expressed genes from counts
counts = counts[low_counts_mask, ]

#Exercise 2.1
dds = DESeqDataSetFromMatrix(countData = counts,
                             colData = patients_data,
                             design = ~age_category)

dds_obj = DESeq(dds)
resultsNames(dds_obj)

results = results(dds_obj, format = "DataFrame", contrast = c("age_category", "Young", "Old"))

#Exercise 2.2
str(results)
head(results)
dim(results)

my_df = data.frame(x = c('b', 'd', 'c', 'e', 'a'),
                   y = c(2,4,3,5,1))
order_indices = order(my_df$y)
my_df = my_df[order_indices, ]
my_df

#Exercise 2.3
row_order = order(results$padj)
results = results[row_order, ]
results[1:20, ]

#4 ENSG00000138823
#a) Highly expressed in old patients
#b) MTTP (Microsomal Triglyceride Transfer Protein) gene, encodes for the 
# large subunit of the heterodimeric microsomal triglyceride transfer protein, 
# which plays a role in lipoprotein assembly 

#Exercise 2.4
log2FoldChange_threshold = 1
padj_threshold = 0.05

log2FoldChange_threshold_pass = results[results$log2FoldChange > log2FoldChange_threshold | 
                                   results$log2FoldChange < -log2FoldChange_threshold, ]

#Exercise 2.5
fc_threshold = 2
p_threshold = 0.05

plot(x = results$log2FoldChange,
     y = -log10(results$padj),
     xlab = "log2 Fold Change (Young/Old)",
     ylab = "-log10(p adjusted)",
     pch = 20)

abline(v=c(-log2(fc_threshold), log2(fc_threshold)), h= c(-log10(p_threshold)), col="green")

fc_threshold = 2  # set a threshold of at least a 2 fold increase (double)
p_threshold = 0.05  # set a threshold of adjusted p-value being <= 0.05


library(ggplot2)

volcano_plot = ggplot(data = data.frame(results), 
                      aes(x = log2FoldChange, y = -log10(padj))) + 
  geom_point(aes(color = ifelse(log2FoldChange < -1 & padj < 0.05, "lower in young",
                                ifelse(log2FoldChange > 1 & padj < 0.05, "higher in young", "NS"))),
             size = 0.5) + 
  theme_minimal() + # make things pretty +
  theme(legend.title = element_blank()) + 
  # next 2 lines draw lines at the thresholds
  geom_vline(xintercept=c(-log2(fc_threshold), log2(fc_threshold)), color="green") + 
  geom_hline(yintercept=-log10(p_threshold), color="green") + 
  scale_color_discrete(type=c("red", "blue", "black")) +
  labs(x = "log2 Fold Change (Young/Old)",
       y = "-log10 Adjusted p-value")


volcano_plot

getwd()
setwd("../week6_DESeq2")

write.csv(x = results,
          file = "./results.csv",
          row.names = FALSE)

