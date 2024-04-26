**A Genome Wide Association Study (GWAS) R-based data processing code for studying mitochondrial diseases in Drosophila**

**Introduction**

The following code is for [bang assay](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1526683/) recovery data collected from a number of ND2 mutant Drosophila flies that were crossed to the DGRP [Drosophila Genetics Reference Panel](https://www.nature.com/articles/nature10811). The [ND2](https://journals.biologists.com/dmm/article/7/10/1165/3533/A-Drosophila-model-of-mitochondrial-disease-caused) mutant flies carry a mutation in their mitochondrial DNA rendering them less capable of coupling electrons to proton pumping in the electron transport chain. In this dataset, we also include w118 wildtype flies as a control and uncrossed ND2 flies. The Bang assay functions by vortexing flies at a set high velocity for a set amount of time and then recording how much time it takes to recover (i.e. start moving again and flying) for each individual fly in a vial. 

**Motivation**

The ultimate goal of this code is to take the original file and be able to transform the data to be able to upload it for genome wide association studies to identify SNPs associated with ameliorating or exacerbating the effects of ND2 alone upon "paralysis".

**Raw Data Organization**

While the specific organization of the data in the dataset we used is not the only way to organize values, for the purposes of running this R code, it may save you some trouble and time to organize your data before starting your experiments in a similar manner. In the dataset we used to write this R code, there are values such as:
1. original.order (the order in which the flies were vortexed),
2. the block (multiple blocks of lines were included to reduce the likelihood of experimental confounding factors),
3. tag (different tag numbers are used for different lines - 23 had a total of 53 lines we investigated),
4. vial (a number of vials for each line was used),
5. N (number of flies),
6. and fly number (fly 1-12).

Thus, this allows us to look at changes in between groups versus within different groups during this analysis. 

**Additional Specifications**

Before you start Genome Wide Association Studies with your dataset, you must make several assumptions that will lead you towards either performing parametric or non-parametric statistics. This can be determined by whether your dataset can be normalized using a variety of transformations that still retains the initial representation of data as much as possible. In our case, we had a gross inflation of zeroes in our dataset, making it difficult to normalize our data despite various efforts. Therefore, we continued with non-parametric statistical analyses. 

**Actual GWAS**

Once your data is processed and prepared, you can upload it to [DGRP2](http://dgrp2.gnets.ncsu.edu/) which will be able to complete Genome Wide Association Studies for your processed dataset and produce a table with the lines associated with the single nucleotide polymorphisms (SNPs) most likely associated with your observed phenotype. These SNP hits will then need to be further assessed for genomic relevance. This will provide you with the actual cutoff for statistically significant SNP hits, which you can also view using a manhattan plot. 

**Running**

Given that you are able to organize your collected data in the same format as specified above, running this code should only include the installation of R on your computer and uploaded your data into an open R environmenet along with this code. 

**Our Results**

The results of this study can be found and read as a [poster](https://tagc2020.figshare.com/articles/poster/Understanding_how_nuclear_genetic_variation_in_a_population_can_affect_mutant_ND2_gene_phenotypes_associated_with_complex-I_mitochondrial_diseases_using_Drosophila_melanogaster/12149838/1) presented at The Allied Genetics Conference of America of the Genetics Society of America 2020 if you are interested. 
