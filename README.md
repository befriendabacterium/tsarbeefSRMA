# beefantibiotics-SRMA
Code for 'Effects of antibiotics on the abundance of antibiotic resistance determinants during and after antibiotic administration to beef cattle: A systematic review and meta-analysis of longitudinal studies'

There are two main scripts:

1. **prep_for_meta:** This script installs/loads the necessary packages, downloads the data from OSF (if not already present in the local repository), processes it for meta-analysis (e.g. to calculate effect sizes and standardise other variables), and produces the initial forest plots to summarise the results of individual studies.
   
2. **run_metanalaysis:** This script runs the meta-analysis - tests of hypothesis 1 and 2 and their sensitivity (for the during and after subsets of the data), subgroup and other alternative analyses, and publication bias tests. It produces the models and figures associated with these.

The input data needed to run the scripts is stored at https://doi.org/10.17605/OSF.IO/2FYC8, and will be downloaded automatically by 'prep_for_meta.R' if not already present in the local repository. Whilst the paper is in peer review, only peer reviewers can download the data and will need an API key provided to them to do so. The OSF repository will be made public upon publication of the paper, and thus an API key will no longer be required (and the script edited accordingly).
   
