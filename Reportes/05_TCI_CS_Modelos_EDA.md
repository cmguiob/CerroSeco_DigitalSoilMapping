---
title: "TCI - Cerro Seco / Suelos"
subtitle: "Modelado I: Analisis exploratorio de datos"
author: "Carlos Guio"
date: "2021-07-26"
knit: (function(inputFile, encoding) { 
      out_dir <- 'Reportes';
      rmarkdown::render(input = inputFile,
                        encoding = encoding, 
                        output_file = file.path(
                                        here::here(), 
                                        out_dir, 
                                        '05_TCI_CS_Modelos_EDA.html'))
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
library(ggcorrplot)
library(GGally)
library(caret)
library(patchwork)


knitr::opts_chunk$set(include = FALSE, echo = FALSE, warning = FALSE, message = FALSE, fig.align="center", fig.showtext = TRUE, fig.retina = 1, dpi = 300, out.width = "80%")

showtext_auto()
```






```r
# Renombrar
names(topoind) <- c("ASE", "ASN", "DEM","DSC", "FLA", "FLD", "LSF", "MPI",
                    "PLC", "PRC", "RSP", "SLP", "TPI", "TRI1", "TRI5", "TWI", "USC", 
                    "VDN", "WSB") #Rename stack layers

names(s2) <- c("B2", "B3", "B4", "B5", "B6", "B7", "B8", "B8A", "B11", "B12") 


# Crear NDVI
NDVI <- (s2[[7]] - s2[[3]]) / (s2[[7]] + s2[[3]])
names(NDVI) <- "NDVI"

#Poner rasters en mismo origen para alinear
topoind <- resample(topoind, NDVI) # This also puts them in same extent

#Chequeo
t(sapply(c(topoind, NDVI), origin))
```

```
##      [,1] [,2]
## [1,]    0    0
## [2,]    0    0
```

```r
#Clip raster a poligono externo
topoind_clip <- mask(topoind, CSsf_18N)
NDVI_clip <- mask(NDVI, CSsf_18N)

#Check extent 
t(sapply(c(topoind_clip, NDVI_clip), function(i) as.vector(extent(i))))
```

```
##        [,1]   [,2]   [,3]   [,4]
## [1,] 589080 595020 501960 505800
## [2,] 589080 595020 501960 505800
```

```r
covars <- stack(topoind_clip, NDVI_clip)

#Clip interno de poligonos mineros
covars_clip <- mask(covars, min2019sf_18N, inverse = TRUE)

#Check plot
plot(covars_clip[[c("DEM","TWI","SLP", "NDVI")]])
```

<img src="C:\Users\cguio\DOCUME~1\Terrae\TCI_CE~1\_Git\Reportes\05_TCI~1/figure-html/prep_covar-1.png" width="80%" style="display: block; margin: auto;" />

```r
# Crear set de puntos para entrenamiento y test. Usar raster::extract por interferencia con tidyr
sitio_modelo <- sitio[,c("ID","SECUENCIA","long","lat")]
coordinates(sitio_modelo) <- ~  long + lat
proj4string(sitio_modelo) <- "+proj=longlat +datum=WGS84 +no_defs"
sitio_modelo_18N <- st_as_sf(sitio_modelo) %>% st_transform(crs = 32618)

covars_puntos <- raster::extract(covars_clip, sitio_modelo_18N)

full_set <- cbind(data.frame(secuencia = sitio_modelo_18N[["SECUENCIA"]],
                             long = st_coordinates(sitio_modelo_18N)[, 1], 
                             lat = st_coordinates(sitio_modelo_18N)[, 2]), 
                     covars_puntos) %>%
          #drop_na() %>%
          dplyr::mutate(secuencia = as.factor(secuencia))
  
#Clip raster a poligono externo: 
# se hace al final, porque algunos puntos para entrenamiento caen fuera del poligono
covars_clip <- mask(covars_clip, CSsf_18N)

# Crear df de variables
covars_df <- as.data.frame(covars_clip, xy= TRUE, na.rm = TRUE) %>% 
  rename(long = x, lat = y)
```








Preguntas...


Resume


¿Que covariables están altamente correlacionadas?:_ corrplot







¿Estan las clases balanceadas?: barplot

¿Cuantas clases podrian reconocerse sin supervisión?: kmeans, pca?

¿Como son las curvas de densidad de las cuatro secuencias para cada predictor?: caret feature plot

¿Que covariavles tienen varianza cero o muy baja?



¿Que tan representativos son los puntos de entrenamiento y test en relación a las covariables del raster?: dot plot de la distribución de cada covariable, identificando los puntos de las secuencias


```r
# Distribución de identificación de puntos de entrenamiento y test
covars_norm_l %>% 
  filter(covariables %in% c("DEM", "NDVI", "TWI", "SLP")) %>%
  filter(seq_len(nrow(.)) %in% sample(1:nrow(.), 800) |(set == "train" | set == "test"))%>%
ggplot() +
  ggdist::geom_dots(aes(x = valores, y = covariables, fill = set, color = set), point_size = 0.3) +
  theme(axis.title = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_line(colour = "grey") )
```

<img src="C:\Users\cguio\DOCUME~1\Terrae\TCI_CE~1\_Git\Reportes\05_TCI~1/figure-html/plot_distr-1.png" width="80%" style="display: block; margin: auto;" />

