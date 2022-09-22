# Preproccess TCGA
## Steps
+ filter_cols (Julia). ```Pick samples' ID owning 3 omics including meth, rnaseqv2 and mirnaseq. Select columns by target features. Sort columns by ID (length = 16).```
+ filter_rows (Julia). ```Set thresholds for var & NA and pick probes.```
+ Download subtypes (R lang)
+ Search labels by shared IDs. (R lang)
