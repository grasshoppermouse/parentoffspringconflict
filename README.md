[![DOI](https://zenodo.org/badge/336434305.svg)](https://zenodo.org/badge/latestdoi/336434305)

# parentoffspringconflict

This repository contains all code necessary to reproduce:

[*Parent-Offspring Conflict and Bargaining Behaviors in Adolescence and Young Adulthood among Chon Chuuk (Chuukese) Micronesians*](https://grasshoppermouse.github.io/parentoffspringconflict/). Kristen L. Syme and Edward H. Hagen

Instructions:

1. Clone this repository
2. Open the project in RStudio or `cd` into the directory and launch `R`. This will automatically bootstrap [`renv`](https://rstudio.github.io/renv/index.html).
3. After the bootstrapping process, enter the following in the console: `renv::restore()`. This should install all the necessary packages, including the separate data package [`chuuk2019public`](https://github.com/grasshoppermouse/chuuk2019public), in an isolated project-specific library.
4. knit the `paper.Rmd` file using the RStudio GUI or with `rmarkdown::render('paper.Rmd')`. This will generate the preprint file [`paper.html`](https://grasshoppermouse.github.io/parentoffspringconflict/), which will display in the RStudio Viewer or can be viewed in any web browser. (Note: if not using RStudio, you will need a recent version of [pandoc](https://pandoc.org) installed.)

Note: Analyses used "R version 3.6.3 (2020-02-29)". You might need to install this version of R to reproduce them.
