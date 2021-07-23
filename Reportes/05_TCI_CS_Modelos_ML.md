---
title: "TCI - Cerro Seco / Suelos"
subtitle: "Covariables"
author: "Carlos Guio"
date: "2021-07-23"
knit: (function(inputFile, encoding) { 
      out_dir <- 'Reportes';
      rmarkdown::render(input = inputFile,
                        encoding = encoding, 
                        output_file = file.path(
                                        here::here(), 
                                        out_dir, 
                                        '05_TCI_CS_Modelos_ML.html'))
                                        })
output:
  html_document:
    theme: journal
    highlight: tango
    keep_md: true
---


```r
library(tidyverse)
library(rgdal) #leer polígono
library(sf) #manipular objetos espaciales tipo sf
library(raster) #manipular objetos raster
library(showtext)

knitr::opts_chunk$set(include = FALSE, echo = FALSE, warning = FALSE, message = FALSE, fig.align="center", fig.showtext = TRUE, fig.retina = 1, dpi = 300, out.width = "70%")

showtext_auto()
```





```r
plot(topoind)
```

<img src="C:\Users\cguio\DOCUME~1\Terrae\TCI_CE~1\_Git\Reportes\05_TCI~1/figure-html/plot-1.png" width="70%" style="display: block; margin: auto;" />

```r
plot(s2)
```

<img src="C:\Users\cguio\DOCUME~1\Terrae\TCI_CE~1\_Git\Reportes\05_TCI~1/figure-html/plot-2.png" width="70%" style="display: block; margin: auto;" />



