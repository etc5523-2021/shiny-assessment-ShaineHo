
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Shinyapp of Access to Clean Fuels based on Population

## The shinyapp is created by Yin Shan Ho 31201474

This is a template that contains materials to create a shiny gadget
application for assessment 2. You will need the following packages
installed to generate the gadget from this template:

``` r
install.packages(c("shiny", "crosstalk", "plotly", "DT", "tidyverse", "here", "shinyWidgets"))
```

## How to run the app

This app is the proportion access to clean fuels and technologies for
cooking based on population to GDP. The **color of the chart was chosen
based on the Olympics Rings** which colors represent the five
continents.  
The function of the chart are as follow:

> Chart Tab

  - **The chart can be shown in logged or linearized** form by switching
    the button titled’“Linear/Log Selector” at the top left-hand corner.

  - **Small countries which population below 1 million can be filtered**
    by the checkbox titled “Hide countries \< 1 million population” at
    top right-hand corner.

  - **The countries in the plot can be selected** by directly clicking
    the the points in the chart or inputting the country in the box
    titled “Select Countries to show in the Plot”.  
    **NOTE**: The selected data can be reset by double clicking the
    plot.

  - **The time of the Data can be shown accordingly** to the setting of
    the sliderbar titled “Year” at the bottom.  
    **NOTE**: The data can be shown by individual year or the range of
    the year by moving the slider bar.

> Table Tab

  - **The countries in the table can be selected** by inputting the
    country in the box titled “Select Countries to show in the Table”.

  - **The time of the Data can be shown accordingly** to the setting of
    the sliderbar titled “Year” in the chart tab.  
    **NOTE**: The data of the table can be shown by the selected
    beginning and the ending year or individual year specified.

The data of the app is downloaded from the [World
Bank](https://databank.worldbank.org/reports.aspx?dsid=2&series=EG.CFT.ACCS.ZS).
Whereas the shinyapp was made based on the chart from [Our World in
Data](https://ourworldindata.org/grapher/access-to-clean-fuels-for-cooking-vs-gdp-per-capita?time=earliest..2016).

## Session Info

``` r
utils::sessionInfo()
#> R version 4.0.5 (2021-03-31)
#> Platform: x86_64-apple-darwin17.0 (64-bit)
#> Running under: macOS Big Sur 10.16
#> 
#> Matrix products: default
#> BLAS:   /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRblas.dylib
#> LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib
#> 
#> locale:
#> [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> loaded via a namespace (and not attached):
#>  [1] compiler_4.0.5    magrittr_2.0.1    tools_4.0.5       htmltools_0.5.1.1
#>  [5] yaml_2.2.1        stringi_1.7.3     rmarkdown_2.10    knitr_1.33       
#>  [9] stringr_1.4.0     xfun_0.22         digest_0.6.27     rlang_0.4.11     
#> [13] evaluate_0.14
```
