# AIM
Examine whether pre-diagnostic inflammatory-related exposures impact the immune cells in the tumor microenvironment in the overall study populations and by race

# Data
This repository use the data prepared from the IF_AACES_NCOCS repositories. This data includes the tertiles and clusters categories created based on the immune cell type abundance.

# Cleaning
Running the "011.OCWAA cleaning.R" script read this clinical_data.rds file and recode/clean then the “02.create var.R “ script which create new variables. This saves the "OCWAA_markers_category.rds" used by the Rmd to do the analyses.

# Statistical analyses
The Rmd files run the statistical analyses.
