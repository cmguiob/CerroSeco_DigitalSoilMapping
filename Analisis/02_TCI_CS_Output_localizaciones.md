---
title: "TCI/Cerro Seco - Localizaciones"
author: "Carlos Guio"
date: "5th of July 2021"
always_allow_html: true
output:
  html_document:
    theme: journal
    highlight: tango
    keep_md: true
editor_options:
  chunk_output_type: console
---

<style type="text/css">
  body{
  font-size: 14pt;}
}
</style>




\

## Los datos

El levantamiento de suelos se realizó con colaboración de  miembros de la comunidad local: en la medición de perfiles, registro del color y la estructura. La base de datos está compuesta por 62 perfiles con información en diferente detalle: 

* 4 perfiles con datos completos de campo y laboratorio
* 27 perfiles con datos de campo parciales, e.g. profundidad, nomenclatura de horizontes y color.
* 31 perfiles con registro fotográfico.

Los perfiles de suelo estudiados en detalle y en campo se utilizaron para interpretar los perfiles que solo contaban con registro fotográfico. De esta interpretación se generalizaron dos tipos de secuencias de horizontes de suelos y paleosuelos, las cuales tienen relevancia para la infiltración de agua, el soporte de la vegetación y los procesos erosivos que modelan el paisaje.








## La distribución de los perfiles

Debido a la accesibilidad a diferentes zonas, los perfiles de suelo estudiados corresponden a exposiciones naturales, en cárcavas, a lo largo de una franja orientada SO-NE. En la figura se descatan cuatro perfiles, los cuales se estudiaron mediante técnicas de campo y laboratorio.

\ 


```r
library(ggplot2)
library(ggrepel) #etiquetas 
library(ggsn) #escala gráfica

ggplot() +
  geom_sf(data = cutout,
          color = "#e2ddd6",
          size = .4)+
  geom_raster(data = DEM_df_clip, 
              aes(fill = altitude, 
                  x=long, 
                  y=lat))+
  scale_fill_gradient2(high= '#f2d29b', 
                       mid='#faf7ef', 
                       low= 'white', 
                       midpoint = 2820, 
                       na.value=NA,
                       guide = F) +
  geom_sf(data = CSsf84, 
          fill = NA,
          color = "#c3beb8",
          size = 0.7) +
  geom_sf(data = min2019sf, 
          fill = "grey30",
          col = "grey30") +
  geom_sf(data=sitio_sf84, 
          aes(col = SECUENCIA), 
          size = 1.5) +
  scale_color_manual(values= col_scp, 
                     name = "Secuencia tipo")+
  # Etiquetas de puntos
  geom_text_repel( data = sitio_sf84[1:4,],   
                   aes(label = ID, geometry = geometry, col = SECUENCIA),
                   size = 3.5,
                   family = "robotoc",
                   fontface = "bold",
                   force_pull  = -0.2,
                   nudge_x = -0.1,
                   direction = "y",
                   box.padding = 0.5,
                   stat = "sf_coordinates",
                   segment.square = FALSE,
                   segment.curvature = -0.3,
                   segment.angle = 30, 
                   segment.ncp = 10,
                   show.legend = FALSE) +
  # Escala gráfica
  ggsn::scalebar(data = CSsf84, 
           dist = 0.5, 
           dist_unit = "km",
           transform = TRUE,
           st.size = 3,
           height=0.015,
           border.size = 0.05,
           box.color = "#e2ddd6",
           box.fill = c("grey20","#e2ddd6"),
           family = "robotoc")+
  # Notas de texto
  annotate(geom = "text", 
           x = -74.168, y = 4.57, 
           label = "Bogotá \n(Ciudad Bolivar)", 
           hjust = "left", 
           size = 4.5,
           family = "roboto",
           fontface = "bold",
           col = "#c3beb8") +
  annotate(geom = "text", 
           x = -74.163, y = 4.548, 
           label = "Minería", 
           size = 3.5,
           family = "roboto",
           fontface = "bold",
           col = "grey30") +
  annotate(geom = "curve", 
           x = -74.163, 
           y = 4.549,
           xend = -74.156, 
           yend = 4.552, 
           curvature = -.3,
           col = "grey30",
           size = 0.5) +
  # Modificación ejes
  scale_x_continuous(breaks=c(-74.18, -74.17, -74.16))+
  scale_y_continuous(breaks=c(4.55,4.56,4.57))+
  # Eje de coordenadas y a la derecha
  coord_sf(label_axes = list(bottom = "E", right = "N", left = NA, top = NA),
           clip = "off") + 
  # Tamaño de ícono color
  guides(color = guide_legend(override.aes = list(size = 3.5))) 
```

<img src="02_TCI_CS_Output_localizaciones_files/figure-html/map-1.png" width="85%" style="display: block; margin: auto;" />

\

