---
title: "TCI - Cerro Seco / Suelos"
subtitle: "Localizaciones y perfiles"
author: "Carlos Guio"
date: "5.7.2021"
knit: (function(inputFile, encoding) { 
      out_dir <- 'Reportes';
      rmarkdown::render(input = inputFile,
                        encoding = encoding, 
                        output_file = file.path(
                                        here::here(), 
                                        out_dir, 
                                        '02_TCI_CS_Perfiles.html'))
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
library(osmdata) #obtener datos de osm
library(ggplot2)
library(aqp) #Munsell to HEX colors
library(showtext) #fuentes de goolge
library(colorspace) #lighten or darken colors
library(ggrepel) #etiquetas 
library(ggsn) #escala gráfica
library(gggibbous) #moons with grain size %
library(patchwork) #plot + inset

knitr::opts_chunk$set(include = FALSE, echo = FALSE, warning = FALSE, message = FALSE, fig.align="center", fig.showtext = TRUE, fig.retina = 1, dpi = 300, out.width = "70%")

showtext_auto()
```








## Los perfiles

Los cuatro perfiles estudiados en detalle se presentan como modelos con propiedades de campo. En ellos se observan dos secuencias de suelos contrastantes en el paisaje.


 
\



 

```r
p_perfiles <- ggplot(hz_bdf, aes(x = reorder(ID, desc(ID)), y = ESP, fill = forcats::fct_rev(ID_HZ2))) + 
  geom_bar(position="stack", stat="identity", width = 0.35) +
  scale_fill_manual(values = rev(hz_bdf$RGBmx),
                    guide = FALSE) +
  geom_text_repel( data = hz_bdf,   
                   aes(y = BASE - (ESP/3), label = HZ),
                   color = darken(hz_bdf$RGBmx, .2, space = "HCL"),
                   size = 3,
                   face = "bold",
                   family = "robotoc",
                   hjust = 0,
                   direction = "y",
                   nudge_x = 0.3,
                   nudge_y = -3,
                   segment.size = .5,
                   segment.square = TRUE,
                   segment.curvature = 0.1,
                   segment.angle = 40,
                   segment.alpha = 0.5,
                   box.padding = 0.3)+
  #y: location from where jitter spreads out vertically, i,e. from the base minus half the tickness
  geom_jitter(data = hz_jdf, aes(x = ID, y = BASE - (ESP/2)),  
              width = 0.15, 
              # height: how far jitter spreads out to each side, i.e. half the tickness
              height = hz_jdf$ESP*0.5,
              size = 0.3,
              col = hz_jdf$RGBco,
              shape = 16)+
  geom_moon(data = hz_bdf %>% dplyr::filter(!is.na(ARCILLA)), aes(x = ID, 
                               y = BASE - (ESP/2), 
                               ratio = ARCILLA/100), 
             size = 4,
             right = TRUE,
             fill = darken("#c3beb8", 0.3, space = "HCL"),
             color = darken("#c3beb8", 0.3, space = "HCL"),
             position = position_nudge(x = -0.3))+
  geom_moon(data = hz_bdf %>% dplyr::filter(!is.na(ARENA)), aes(x = ID, 
                               y = BASE - (ESP/2), 
                               ratio = GRUESOS/100), 
             size = 4,
             right = FALSE,
             fill = lighten("#c3beb8", 0.1, space = "HCL"),
             color = lighten("#c3beb8", 0.1, space = "HCL"),
             position = position_nudge(x = -0.3))+
  geom_hline(yintercept = 0, col = '#f2d29b')+
  scale_y_reverse(breaks = c(0,100,200,300,400,500), 
                  labels=c("0", "100", "200", "300", "400", "500\ncm"))+
  scale_x_discrete(position = "top") +
  theme(axis.text.x = element_text(family = "robotoc",
                           colour = c('#DA7543','#DA7543','#4B6E8E', '#6AB6AA'),
                           face = "bold"),
               axis.ticks.x =  element_blank(),
        panel.grid.major.y = element_line(color = "#c3beb8", size = .4, linetype = c("13"))) +
  coord_cartesian(clip = "off")
```


```r
p_layout <- p_perfiles + inset_element(p_moon, 0.62, -0.17, 1.12, 0.53) # l, b, r, t

p_layout
```

<img src="C:\Users\cguio\Documents\Terrae\TCI_Cerro Seco\_Git\Reportes\02_TCI_CS_Perfiles_files/figure-html/layout_perfiles-1.png" width="70%" style="display: block; margin: auto;" />



