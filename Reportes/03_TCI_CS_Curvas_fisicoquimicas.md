---
title: "TCI - Cerro Seco / Suelos"
subtitle: "Perfiles de propiedades fisicoqiímicas"
author: "Carlos Guio"
date: "14.07.2021"
knit: (function(inputFile, encoding) { 
      out_dir <- 'Reportes';
      rmarkdown::render(input = inputFile,
                        encoding = encoding, 
                        output_file = file.path(
                                        here::here(), 
                                        out_dir, 
                                        '03_TCI_CS_Curvas_fisicoquimicas.html'))
                                        })
output:
  html_document:
    theme: journal
    highlight: tango
    keep_md: true
editor_options:
  chunk_output_type: console
---



```r
library(tidyverse)
library(aqp)
library(sp)
library(lattice)
library(colorspace)
library(showtext) #google fonts
library(soilDB)
library(latticeExtra)


knitr::opts_chunk$set(include = FALSE, echo = FALSE, warning = FALSE, message = FALSE, fig.align="center", fig.showtext = TRUE, fig.retina = 1, dpi = 300, out.width = "75%")

showtext_auto()
```







## Curvas de retención idealizadas









```r
#plot_curvas <- xyplot(
#  phi ~ theta | ID, data = res, groups = HZ,
#    type = c('l'),
#  panel = function(...) {panel.xyplot(...)
#                         panel.grid(lty = 3, col = "#DAD7D6")},
#  scales = list(alternating=1, 
#                x=list(tick.number=3), 
#                y=list(log=10, tick.number=5), 
#                cex = 0.8), 
#  yscale.components = yscale.components.logpower, 
#  ylab = list(label = 'Potencial matricial (-kPa)', fontsize = 9.5), 
#  xlab = list(label = expression(Contenido~volumétrico~agua~~(cm^3/cm^3)), fontsize = #9.5), 
#  par.settings = sty_c, 
#  strip=strip.custom(bg= NA, 
#                          par.strip.text=list(font=2, 
#                                              cex=1, 
#                                              col= darken("#F5F2F1", 
#                                                          0.5, 
#                                                          space = "HCL"))), 
#  as.table = TRUE,
#    layout = c(4,1))

#plot_curvas
```


## Las propiedades 




### Código: xyplot 
#### Propiedades relevantes para infiltración


```r
# Nombres para paneles
strip_names <-c( "Arena %", "Limo %","Arcilla %","Fe-di","Densidad", "Ks cm/día")

plot_hidro <- xyplot(top ~ p.q50 | variable, 
       groups = which,
       data=CShg,
       ylab=list(label ='Profundidad (cm)', fontsize = 9.5),
       xlab=list(label = ""),
       lower=CShg$p.q25, upper=CShg$p.q75, ylim=c(400,-2),
       panel=panel.depth_function2,
       sync.colors=TRUE,
       par.settings= sty_p,
       prepanel=prepanel.depth_function,
       layout=c(6,1), 
       strip=strip.custom(bg= NA, 
                          par.strip.text=list(font=2, 
                                              cex=0.7, 
                                              col= darken("#F5F2F1", 
                                                          0.5, 
                                                          space = "HCL")),
                          factor.levels=strip_names),
       scales=list(x=list(tick.number=3, alternating=1, relation='free', cex = 0.6), 
                   y = list(tick.number=6)),
       auto.key=list(columns=4, 
                     lines=TRUE, 
                     points=FALSE, 
                     col = darken("#F5F2F1", 0.3, space = "HCL"),
                     size = 2.5,
                     font = 2)) #legend

plot_hidro
```

<img src="C:\Users\cguio\DOCUME~1\Terrae\TCI_CE~1\_Git\Reportes\03_TCI~1/figure-html/plot_hidro-1.png" width="75%" style="display: block; margin: auto;" />

### Código: xyplot
#### Propiedades relevantes para vegetación


```r
# Nombres para paneles
strip_names <-c( "K ppm", "CO %", "Ca+Mg/K","P ppm","Si -ox % ", "CIC")

plot_bio <- xyplot(top ~ p.q50 | variable, 
       groups = which,
       data=CSbg,
       ylab=list(label ='Profundidad (cm)', fontsize = 9.5),
       xlab=list(label = ""),
       lower=CSbg$p.q25, upper=CSbg$p.q75, ylim=c(200,-2),
       panel=panel.depth_function2,
       sync.colors=TRUE,
       par.settings= sty_p,
       prepanel=prepanel.depth_function,
       layout=c(6,1), 
       strip=strip.custom(bg= NA, 
                          par.strip.text=list(font=2, 
                                              cex=0.7, 
                                              col= darken("#F5F2F1", 
                                                          0.5, 
                                                          space = "HCL")),
                          factor.levels=strip_names),
       scales=list(x=list(tick.number=3, alternating=1, relation='free', cex = 0.6), 
                   y = list(tick.number=6)),
       auto.key=list(columns=4, 
                     lines=TRUE, 
                     points=FALSE, 
                     col = darken("#F5F2F1", 0.3, space = "HCL"),
                     size = 2.5,
                     font = 2)) #legend
plot_bio
```

<img src="C:\Users\cguio\DOCUME~1\Terrae\TCI_CE~1\_Git\Reportes\03_TCI~1/figure-html/plot_bio-1.png" width="75%" style="display: block; margin: auto;" />

### Código: xyplot
#### Propiedades relevantes para erosión


```r
strip_names <-c("BT cmol/kg" , "SB %","Na cmol/kg", "Ca:Mg","CE dS/m","pH")

plot_ero <- xyplot(top ~ p.q50 | variable, 
       groups = which,
       data=CSeg,
       ylab=list(label ='Profundidad (cm)', fontsize = 9.5),
       xlab=list(label = ""),
       lower=CSeg$p.q25, upper=CSeg$p.q75, ylim=c(400,-2),
       panel=panel.depth_function2,
       sync.colors=TRUE,
       par.settings= sty_p,
       prepanel=prepanel.depth_function,
       layout=c(6,1), 
       strip=strip.custom(bg= NA, 
                          par.strip.text=list(font=2, 
                                              cex=0.7, 
                                              col= darken("#F5F2F1", 
                                                          0.5, 
                                                          space = "HCL")),
                          factor.levels=strip_names),
       scales=list(x=list(tick.number=3, alternating=1, relation='free', cex = 0.6), 
                   y = list(tick.number=6)),
       auto.key=list(columns=4, 
                     lines=TRUE, 
                     points=FALSE, 
                     col = darken("#F5F2F1", 0.3, space = "HCL"),
                     size = 2.5,
                     font = 2)) #legend

plot_ero
```

<img src="C:\Users\cguio\DOCUME~1\Terrae\TCI_CE~1\_Git\Reportes\03_TCI~1/figure-html/plot_ero-1.png" width="75%" style="display: block; margin: auto;" />

Toda las propiedades


```r
strip_names <-c( "K ppm", "CO %", "Ca+Mg/K","P ppm","Si -ox % ", "CIC", "BT cmol/kg" , "SB %","Na cmol/kg", "Ca:Mg","CE dS/m","pH", "Arena %", "Limo %","Arcilla %","Fe-di","Densidad", "Ks cm/día")

plot_props <- xyplot(top ~ p.q50 | variable, 
       groups = which,
       data=CSpg,
       ylab=list(label ='Profundidad (cm)', fontsize = 11),
       xlab=list(label = ""),
       lower=CSpg$p.q25, upper=CSpg$p.q75, ylim=c(400,-2),
       panel=panel.depth_function2,
       sync.colors=TRUE,
       par.settings= sty_p,
       prepanel=prepanel.depth_function,
       layout=c(6,3), 
       strip=strip.custom(bg= NA, 
                          par.strip.text=list(font=2, 
                                              cex=0.9, 
                                              col= darken("#F5F2F1", 
                                                          0.5, 
                                                          space = "HCL")),
                          factor.levels=strip_names),
       scales=list(x=list(tick.number=3, alternating=1, relation='free', cex = 0.7), 
                   y = list(tick.number=6)),
       auto.key=list(columns=4, 
                     lines=TRUE, 
                     points=FALSE, 
                     col = darken("#F5F2F1", 0.3, space = "HCL"),
                     size = 3.5,
                     font = 2)) #legend

plot_props
```

<img src="C:\Users\cguio\DOCUME~1\Terrae\TCI_CE~1\_Git\Reportes\03_TCI~1/figure-html/plot_props-1.png" width="75%" style="display: block; margin: auto;" />





