---
title: "TCI - Cerro Seco / Suelos"
subtitle: "Modelado I: Analisis exploratorio de datos"
author: "Carlos Guio"
date: "2021-08-10"
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
library(glue)


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
topoind2 <- resample(topoind, NDVI) # This also puts them in same extent

#Chequeo
t(sapply(c(topoind2, NDVI), origin))
```

```
##      [,1] [,2]
## [1,]    0    0
## [2,]    0    0
```

```r
#Stack
covars <- stack(topoind2, NDVI)

#Check plot
plot(covars[[c("DEM","TWI","SLP", "NDVI")]])
```

<img src="C:\Users\cguio\DOCUME~1\Terrae\TCI_CE~1\_Git\Reportes\05_TCI~1/figure-html/prep_covar-1.png" width="80%" style="display: block; margin: auto;" />

```r
#Clip interno de poligonos mineros
covars_clip <- mask(covars, min2019sf_18N, inverse = TRUE)

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

# Crear df de variables. NAs removed corespond to pixels outside the polygon
covars_df <- as.data.frame(covars_clip, xy= TRUE, na.rm = TRUE) %>%
             rename(long = x, lat = y)


#Check plot
plot(covars_clip[[c("DEM","TWI","SLP", "NDVI")]])
```

<img src="C:\Users\cguio\DOCUME~1\Terrae\TCI_CE~1\_Git\Reportes\05_TCI~1/figure-html/prep_covar-2.png" width="80%" style="display: block; margin: auto;" />










```r
covars_norm_l %>%
  filter(covariables == "long" & (set == "train" | set == "test")) %>%
  ggplot()+
  geom_bar(aes(x = secuencia, fill = secuencia)) +
  facet_wrap(~ set)
```

<img src="C:\Users\cguio\DOCUME~1\Terrae\TCI_CE~1\_Git\Reportes\05_TCI~1/figure-html/EDA_particion-1.png" width="80%" style="display: block; margin: auto;" />

```r
#change colors
#add glue to display total number of observations in each set as facet label
```

### ¿Que covariables tienen varianza cero o muy baja?

### ¿Que covariables están altamente correlacionadas?



```r
#Correlación
cor_mx <- cor(covars_norm %>% 
                dplyr::filter(set == "train") %>%
                dplyr::select(-set, -secuencia))

ggcorrplot(cor_mx, type = "upper", hc.order = TRUE) +
  theme(axis.text = element_text(size= 10),
        axis.text.x = element_text(angle = 90),
        legend.key.size = unit(0.3, "in"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
```

<img src="C:\Users\cguio\DOCUME~1\Terrae\TCI_CE~1\_Git\Reportes\05_TCI~1/figure-html/EDA_corr-1.png" width="80%" style="display: block; margin: auto;" />

Después de observar las variables corelacionadas, hay que seleccionar las que tienen mayor contenido de información. Comparar

*  MPI - LSF - TRI5 - TRI1 - SLP
* VDN - RSP
* WSB - long


### ¿Cómo se comparan la media de las variables predictoras para cada secuencia?


```r
ggplot(data = covars_norm_l %>% filter(set == "train")) +
  geom_boxplot(mapping = aes(secuencia, valores, color = secuencia)) +
  facet_wrap(~ covariables) +
  theme(panel.border = element_rect(fill = NA, colour = "#c3beb8"))
```

<img src="C:\Users\cguio\DOCUME~1\Terrae\TCI_CE~1\_Git\Reportes\05_TCI~1/figure-html/EDA_medias-1.png" width="80%" style="display: block; margin: auto;" />

Al comparar la media de las variables predictoras para las cuatro clases observa que algunas tienen muy poca información, como FLA, FLD, PRC y NDVI.

Respecto a las variables corelacionadas:
* LSF < MPI < SLP = TRI1 = TRI5
* VDN = RSP

El siguiente aspect oa comparar para seleccionar variables co nmayor información es la distribución de valores.

### ¿Cómo se compara el patron de densidad de los predictores para cada secuencia?


```r
ggplot(data = covars_norm_l %>% filter(set == "train"), 
       aes(x = valores, y = ..scaled.., color = secuencia)) +
  geom_density() +
  facet_wrap(~ covariables)+
  scale_y_continuous(limits = c(0, 1)) +
  theme(panel.border = element_rect(fill = NA, colour = "#c3beb8"))
```

<img src="C:\Users\cguio\DOCUME~1\Terrae\TCI_CE~1\_Git\Reportes\05_TCI~1/figure-html/EDA_densidades-1.png" width="80%" style="display: block; margin: auto;" />


Al comparar las curvas de densidad se observa que las curvas de cada secuencia tienen NDVI diferenciados. FLA, FLD, LSF y PRC muestran poca información también en este análsis, por lo que serán removidas. Al comparar variables similares se observa que ASN tine más información que ASE. Respecto a las varaibles correlacionadas:

*  TRI5 < TRI1 < SLP
* RSP < VDN

En conclusión, remover TRI1, TRI5, RSP, FLA, FLD, LSF, MPI, PRC, ASE, WSB.


### ¿Que tan representativos son los puntos de entrenamiento y test en relación a las variables predictoras en todo el raster?



```r
# Distribución de identificación de puntos de entrenamiento y test
covars_norm_l %>% 
  filter(covariables %in% c("ASN","DSC","DEM", "NDVI","PLC","TPI", "USC", "TWI", "SLP", "VDN")) %>%
  filter(seq_len(nrow(.)) %in% sample(1:nrow(.), 800) |(set == "train" | set == "test"))%>%
ggplot() +
  ggdist::geom_dots(aes(x = valores, y = covariables, fill = set, color = set), point_size = 0.3) +
  theme(axis.title = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_line(colour = "grey") )
```

<img src="C:\Users\cguio\DOCUME~1\Terrae\TCI_CE~1\_Git\Reportes\05_TCI~1/figure-html/plot_distr-1.png" width="80%" style="display: block; margin: auto;" />

### Que falta?

¿Estan las clases balanceadas?: barplot

¿Cuantas clases podrian reconocerse sin supervisión?: kmeans, pca?
