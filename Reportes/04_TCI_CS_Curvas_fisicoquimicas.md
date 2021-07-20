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
                                        '04_TCI_CS_Curvas_fisicoquimicas.html'))
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
knitr::opts_chunk$set(include = FALSE, echo = FALSE, warning = FALSE, message = FALSE, fig.align="center", fig.showtext = TRUE, fig.retina = 1, dpi = 300, out.width = "75%", dev = "ragg_png")

library(tidyverse)
library(aqp)
library(sp)
library(lattice)
library(colorspace)
library(showtext) #google fonts
library(soilDB)
library(latticeExtra)
```












## Curvas de retención idealizadas


```r
res <- lapply(1:nrow(res_rosetta), function(i) {
  
  # model bounds are given in kPA of suction
  vg <- KSSL_VG_model(VG_params = res_rosetta[i, ], phi_min = 10^-3, phi_max=10^6)
  
  # extract curve and add texture ID
  m <- vg$VG_curve
  m$HZ <- res_rosetta$HZ[i]
  m$ID <- res_rosetta$ID[i]
  
  return(m)
})

res <- do.call('rbind', res)


xyplot(
  phi ~ theta | ID, data = res, groups = HZ,
    type = c('l'),
  panel = function(...) {panel.xyplot(...)
                         panel.grid(lty = 3, col = "#DAD7D6")},
  scales = list(alternating=1, 
                x=list(tick.number=3), 
                y=list(log=10, tick.number=5), 
                cex = 0.8), 
  yscale.components = yscale.components.logpower, 
  ylab = expression(Potencial~~matricial~~(-kPa)), 
  xlab = expression(Contenido~volumetrico~agua~~(cm^3/cm^3)), 
  par.settings = sty_c, 
  strip=strip.custom(bg= NA, 
                          par.strip.text=list(font=2, 
                                              cex=0.7, 
                                              col= darken("#F5F2F1", 
                                                          0.5, 
                                                          space = "HCL"))), 
  as.table = TRUE,
    layout = c(4,1)
)
```

<img src="C:\Users\cguio\Documents\Terrae\TCI_Cerro Seco\TERRAE_CerroSeco_DSM_git\Reportes\04_TCI_CS_Curvas_fisicoquimicas_files/figure-html/plot_retencion-1.png" width="75%" style="display: block; margin: auto;" />


## Las propiedades 




### Código: xyplot 
#### Propiedades relevantes para infiltración


```r
# Nombres para paneles
strip_names <-c( "Arena %", "Limo %","Arcilla %","Densidad", "Ks cm/día", "Ks ROSETTA")


xyplot(top ~ p.q50 | variable, 
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
```

<img src="C:\Users\cguio\Documents\Terrae\TCI_Cerro Seco\TERRAE_CerroSeco_DSM_git\Reportes\04_TCI_CS_Curvas_fisicoquimicas_files/figure-html/plot_hidro-1.png" width="75%" style="display: block; margin: auto;" />

### Código: xyplot
#### Propiedades relevantes para vegetación


```r
# Nombres para paneles
strip_names <-c( "pH", "CO %","Bases cmol/kg","P ppm","Si -ox % ", "CIC")

xyplot(top ~ p.q50 | variable, 
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
```

<img src="C:\Users\cguio\Documents\Terrae\TCI_Cerro Seco\TERRAE_CerroSeco_DSM_git\Reportes\04_TCI_CS_Curvas_fisicoquimicas_files/figure-html/plot_bio-1.png" width="75%" style="display: block; margin: auto;" />

### Código: xyplot
#### Propiedades relevantes para erosión


```r
strip_names <-c( "Limo %","CO %" ,"Na cmol/Kg", "Ca:Mg","CE dS/m","pH")

xyplot(top ~ p.q50 | variable, 
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
```

<img src="C:\Users\cguio\Documents\Terrae\TCI_Cerro Seco\TERRAE_CerroSeco_DSM_git\Reportes\04_TCI_CS_Curvas_fisicoquimicas_files/figure-html/plot_ero-1.png" width="75%" style="display: block; margin: auto;" />




