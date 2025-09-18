This repository analyses confidence data from Open Science Framework (OSF) datasets, comparing equal-variance and unequal-variance signal detection theory (SDT) models. 

The analysis builds on published confidence datasets, fitting and evaluating SDT frameworks to better understand how people report certainty in binary decision tasks.

## Repository Structure
- `knit1.Rmd` is the main file producing an overview of the reanalysis with plots and tables. In addition to the data, this file requires `mod_data.RData` produced by `confidence mod1.Rmd`.
- `knit1.html` is the output file showing both plots and tables. Note, to see the rendered version of this file use the following link: [https://htmlpreview.github.io/?https://github.com/kiarachung/Confidence-Data-Analysis/blob/main/knit1.html](https://htmlpreview.github.io/?https://github.com/kiarachung/Confidence-Data-Analysis/blob/main/knit1.html)
- `confidence mod1.Rmd` is the file fitting both equal variance and unequal variance SDT model to the confidence rating data using `MPTinR` which produces `mod_data.RData`. This file requires:
  - The prepared data in wide format, `confidence_data_wide.RData`, produced by file: `confidence-preparation.R`.
  - The fitting functions provided in `model-fun-rocs.R`
- `confidence-preparation.R`: This files prepares the data for fitting in the wide format and saves them in file `confidence_data_wide.RData`. Note that the content of this file (i.e., the data preparation steps) are identical to the steps also contained in `knit1.Rmd`.

## Data

The original datasets are available via the OSF respository: https://osf.io/s46pr/


