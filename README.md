# Reconstructing the last 30,000 years of environmental history in the savannas of Northern Australia

This repo contains source files for my PhD thesis titled, "Multi-proxy evidence for long environmental change in northern Australia's tropical savannas", at James Cook University. The PDF and further information can be accessed at [JCU Research Repository](https://doi.org/10.25903/nhws-h016).

The thesis followed the FAIR data principles and metadata was created according to the National Centers for Environmental Information: https://www.ncei.noaa.gov/access/paleo-search/cvterms?termId=0. Refer to the Metada.xslm file in the main branch.

The datasets are from biological and chemical indicators measured in a sediment core. I integrated the information from all different indicators using several statistical techniques, including hierarchical cluster, principal component analysis, regression, and time series analysis. I performed the data cleaning, wrangling, manipulation, analysis, and visualisation of a dataset with over 3,000 observations from more than 35 variables.

The Rmd files in the main directory are the first four chapters of the thesis and show the code used to write them.


Directories

    experiments/: raw data obtained from the different experiments carried out for the thesis.
    preprocessing/: contains the cleaning and wrangling of each dataset.
    processed_data/: final processed data and corresponding dictionaries.
    analysis/: R code used for the analyses and graphs. Some of the files in this directory contain code to produce graphs.
    bib/: Bibliography files.
    img/: Images made with QGIS and other tools to illustrate ideas.
    short_overview: short presentation with a summary of the thesis outcomes
    Figs/: Figures included in the final thesis
    _book/: examples of html files generated from the thesis.
    

