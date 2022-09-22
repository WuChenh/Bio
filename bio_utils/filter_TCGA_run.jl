include("filter_main.jl")
tags = ["normalized_count", "Beta_value", "reads_per_million_miRNA_mapped", "RPKM", "scaled_estimate"]

## Filter columns
filter_cols(["_prj/dataset/KIPAN/KIPAN_meth.csv",
             "_prj/dataset/KIPAN/KIPAN_mirnaseq.csv",
             "_prj/dataset/KIPAN/KIPAN_rnaseqv2.csv"], tags)
#
filter_cols(["_prj/dataset/gdac.broadinstitute.org_LGG.Merge_methylation__humanmethylation450__jhu_usc_edu__Level_3__within_bioassay_data_set_function__data.Level_3.2016012800.0.0/LGG.methylation__humanmethylation450__jhu_usc_edu__Level_3__within_bioassay_data_set_function__data.data.txt",
             "_prj/dataset/gdac.broadinstitute.org_LGG.Merge_mirnaseq__illuminahiseq_mirnaseq__bcgsc_ca__Level_3__miR_gene_expression__data.Level_3.2016012800.0.0/LGG.mirnaseq__illuminahiseq_mirnaseq__bcgsc_ca__Level_3__miR_gene_expression__data.data.txt",
             "_prj/dataset/gdac.broadinstitute.org_LGG.Merge_rnaseqv2__illuminahiseq_rnaseqv2__unc_edu__Level_3__RSEM_genes__data.Level_3.2016012800.0.0/LGG.rnaseqv2__illuminahiseq_rnaseqv2__unc_edu__Level_3__RSEM_genes__data.data.txt"],
             tags)
#
## Filter rows
filter_rows("_prj/dataset/KIPAN/.fltCol_KIPAN_mirnaseq.csv", 0)
filter_rows("_prj/dataset/KIPAN/.fltCol_KIPAN_rnaseqv2.csv", 0.058)
filter_rows("_prj/dataset/KIPAN/.fltCol_KIPAN_meth.csv", 0.095)
#
filter_rows("_prj/dataset/LGG/.fltCol_LGG.mirnaseq.txt", 0)
filter_rows("_prj/dataset/LGG/.fltCol_LGG.rnaseqv2.txt", 0.027)
filter_rows("_prj/dataset/LGG/.fltCol_LGG.methylation.txt", 0.108)
#
