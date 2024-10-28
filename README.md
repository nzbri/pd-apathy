# Apathy in Parkinson's

This repository contains the key code attached with the CMRF-funded
[*Predictors of apathy in Parkinson's disease*](https://cmrf.org.nz/research/predictors-of-apathy-in-parkinsons-disease/)
project.

Anonymised data is available on request for those wishing to reproduce the
various analyses, and the general properties of the data itself are detailed in
[the data overview document](DataOverview.md).

More detailed notes and some of the core hypotheses are detailed in
[the analysis plan](AnalysisPlan.md), and the imaging analyses are maintained
in a separate repository:
<https://github.com/nzbri/pd-apathy_imaging>.

### Running the Code

To reproduce the analyses from the manuscript, use:
```shell
cd pd-apathy/Code/
NPI_APATHY_THRESHOLD=4 Rscript exploratory_visualisations.R
NPI_APATHY_THRESHOLD=4 Rscript summary_tables.R
NPI_APATHY_THRESHOLD=4 Rscript cross_sectional_analyses.R
NPI_APATHY_THRESHOLD=4 Rscript individual_test_predictions.R
NPI_APATHY_THRESHOLD=4 Rscript predictive_analyses.R
```

The files can also be run interactively using Rstudio, e.g.
```R
setwd("pd-apathy/Code/")
source("predictive_analyses.R", echo = TRUE)
```

### Contributors

 + [Sam Harrison](https://www.nzbri.org/People/harrison/)
 + [Campbell Le Heron](https://www.nzbri.org/People/le-heron/)
 + [Kyla Horne](https://www.nzbri.org/People/horne/)
