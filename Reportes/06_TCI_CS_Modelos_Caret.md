---
title: "TCI - Cerro Seco / Suelos"
subtitle: "Modelado II: caret"
author: "Carlos Guio"
date: "2021-08-17"
knit: (function(inputFile, encoding) { 
      out_dir <- 'Reportes';
      rmarkdown::render(input = inputFile,
                        encoding = encoding, 
                        output_file = file.path(
                                        here::here(), 
                                        out_dir, 
                                        '06_TCI_CS_Modelos_Caret.html'))
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
library(caret)
library(ggsn)
library(colorspace)
library(patchwork)

knitr::opts_chunk$set(
	echo = FALSE,
	fig.align = "center",
	fig.retina = 1,
	fig.showtext = TRUE,
	message = FALSE,
	warning = FALSE,
	dpi = 300,
	include = FALSE,
	out.width = "80%"
)
showtext_auto()
```





```r
# Crear set de puntos para entrenamiento y test.
sitio_modelo_18N <- sitio[,c("ID","SECUENCIA","long","lat")] %>%
  #transformar a sp
  `coordinates<-`(~  long + lat) %>%  
  #definir crs de sp
  `proj4string<-`("+proj=longlat +datum=WGS84 +no_defs") %>%
  # transformar a sf
  st_as_sf() %>% 
  st_transform(crs = 32618)


# Crear NDVI a partir de Sentinel 2
NDVI <- (s2[[7]] - s2[[3]]) / (s2[[7]] + s2[[3]])
names(NDVI) <- "NDVI"

#Calculate category raster for watershed basin
# later do one hot encoding
WSB_mx <- getValues(topoind$WSB)
WSB_mx[is.na(WSB_mx)] <- 0

set.seed(123)
km.WSB <- kmeans(WSB_mx, 9, nstart = 19, iter.max = 500)

km.WSB$cluster[km.WSB$cluster == 1] <-  NA
km.WSB$cluster <- as.factor(km.WSB$cluster)
  
topoind$WSB <- setValues(topoind$WSB, km.WSB$cluster)

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
# Calculate distance buffer
empty_raster <- NDVI %>% `values<-`(NA)
dist_names <- c("A1", "A2", "B1", "B2")

secuencias_dist <- stack()
for(i in seq_along(dist_names)){
  raster_i <- raster::distanceFromPoints(object = empty_raster,
                                         xy = sitio_modelo_18N %>%
                                              filter(SECUENCIA == dist_names[i]) %>%
                                              st_coordinates())
  names(raster_i) <- paste("dist",dist_names[i], sep = "_")
secuencias_dist <- stack(secuencias_dist, raster_i)
}


#Stack
covars <- stack(topoind2, NDVI, secuencias_dist)

#Check plot
plot(covars[[c("DEM","SLP", "NDVI", "dist_B2")]])
```

<img src="C:\Users\cguio\DOCUME~1\Terrae\TCI_CE~1\_Git\Reportes\06_TCI~1/figure-html/prep_covar-1.png" width="80%" style="display: block; margin: auto;" />

```r
#Extraer covariables de puntos de muestreo
covars_puntos <- raster::extract(covars, sitio_modelo_18N)

# Agregar coordenadas y secuencias a covariables de puntos de muestreo
full_set <- cbind(data.frame(secuencia = sitio_modelo_18N[["SECUENCIA"]],
                             long = st_coordinates(sitio_modelo_18N)[, 1], 
                             lat = st_coordinates(sitio_modelo_18N)[, 2]), 
                     covars_puntos) %>%
          #drop_na() %>%
          dplyr::mutate(secuencia = as.factor(secuencia)) %>%
          dplyr::mutate(WSB = as.factor(WSB))


#Clip interno de poligonos mineros
covars_clip <- mask(covars, min2019sf_18N, inverse = TRUE)

#Clip raster a poligono externo: 
# se hace al final, porque algunos puntos para entrenamiento caen fuera del poligono
covars_clip <- mask(covars_clip, CSsf_18N)

# Crear df de covariables. NAs removed corespond to pixels outside the polygon
covars_df <- as.data.frame(covars_clip, xy= TRUE, na.rm = TRUE) %>%
             rename(long = x, lat = y)  %>%
             dplyr::mutate(WSB = as.factor(round(WSB,digits = 0)))

#Check plot
plot(covars_clip[[c("DEM","WSB", "NDVI", "dist_B2")]])
```

<img src="C:\Users\cguio\DOCUME~1\Terrae\TCI_CE~1\_Git\Reportes\06_TCI~1/figure-html/prep_covar-2.png" width="80%" style="display: block; margin: auto;" />

Se utilizaron cuatro tipos de variables para la predicción: raster numérico de índices derivados del DEM, raster categórico derivado del DEM, NDVI derivado de Sentinel-2 y raster de distancias a cada secuencia.

La predicción sin ajustar devulve un kapp de 0.2 - 0.3. Factores que afectan son el porcentaje de la partición, el tipo de remuestreo (cv vs loocv), el desbalance de clases, la falta de representatividad de las covariables raster en el set de entrenamiento, la selección de variables. A continuación se mencionan los efectos:



Ensayé partición 0.7, 0.75 y 0.8. Al aumentar el porcentaje de observaciones en el set de entrenamiento ...



Los efectos del preprocesamiento fueron ...



El modelo con remuestreo loocv tardó mas en correr, pero mostró una mejora en el Kappa de 0.1 a 0.2 puntos, el mejor en random forest. Al predecir sobre el raster ambos modelos mostraron una mejora en la granularidad, es decir, la distribución de las secuencias adoptaba patrones relacionados a las covariables geográficas. 







```r
model_vars <- caret_train %>%
              dplyr:: select(-secuencia, -lat, -long, -TRI1, -TRI5,-RSP,-FLA, -FLD, -PRC) %>%
              colnames() %>%
              paste(collapse = "+")

formula_train <- as.formula(paste("secuencia ~", model_vars)) 

# train random forest model
set.seed(1)
caret_rf <- train(formula_train,
                  data = caret_train,
                  method = "rf",
                  metric = "Kappa",
                  # importance type can be selected in ranger, e.g. permutation
                  # rf importance = TRUE returns importance for each predicted class
                  importance = TRUE, 
                  trControl = caret_cv,
                  #no need to tune trees(Hengl et al. 2018).rf:ntree, ranger:num.trees
                  ntree = 5000,   
                  tuneGrid = tuneGrid_rf)

caret_rf
```

```
## Random Forest 
## 
## 48 samples
## 25 predictors
##  4 classes: 'A1', 'A2', 'B1', 'B2' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold, repeated 10 times) 
## Summary of sample sizes: 43, 43, 43, 42, 42, 43, ... 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy   Kappa    
##    1    0.7453333  0.5121490
##    2    0.9036667  0.8382541
##    3    0.9305000  0.8866752
##    4    0.9491667  0.9215141
##    5    0.9511667  0.9244850
##    6    0.9573333  0.9322862
##    7    0.9606667  0.9373367
##    8    0.9606667  0.9373367
##    9    0.9606667  0.9373367
##   10    0.9606667  0.9373367
##   11    0.9606667  0.9373367
##   12    0.9606667  0.9373367
##   13    0.9606667  0.9373367
##   14    0.9606667  0.9373367
##   15    0.9606667  0.9373367
## 
## Kappa was used to select the optimal model using the largest value.
## The final value used for the model was mtry = 7.
```

```r
#train knn model
set.seed(1)
caret_knn <- train(formula_train,
                  data = caret_train,
                  method = "kknn",
                  metric = "Kappa",
                  trControl = caret_cv,
                  tuneGrid = tuneGrid_kknn)

caret_knn
```

```
## k-Nearest Neighbors 
## 
## 48 samples
## 25 predictors
##  4 classes: 'A1', 'A2', 'B1', 'B2' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold, repeated 10 times) 
## Summary of sample sizes: 43, 43, 43, 42, 42, 43, ... 
## Resampling results across tuning parameters:
## 
##   kmax  distance  kernel        Accuracy   Kappa    
##    2    1         gaussian      0.6988333  0.5384580
##    2    1         triangular    0.6988333  0.5384580
##    2    1         rectangular   0.6988333  0.5384580
##    2    1         epanechnikov  0.6988333  0.5384580
##    2    1         biweight      0.6988333  0.5384580
##    2    1         triweight     0.6988333  0.5384580
##    2    1         cos           0.6988333  0.5384580
##    2    1         inv           0.6988333  0.5384580
##    2    1         rank          0.6988333  0.5384580
##    2    1         optimal       0.6988333  0.5384580
##    2    2         gaussian      0.6645000  0.4891655
##    2    2         triangular    0.6645000  0.4891655
##    2    2         rectangular   0.6645000  0.4891655
##    2    2         epanechnikov  0.6645000  0.4891655
##    2    2         biweight      0.6645000  0.4891655
##    2    2         triweight     0.6645000  0.4891655
##    2    2         cos           0.6645000  0.4891655
##    2    2         inv           0.6645000  0.4891655
##    2    2         rank          0.6645000  0.4891655
##    2    2         optimal       0.6645000  0.4891655
##    3    1         gaussian      0.7055000  0.5425128
##    3    1         triangular    0.7228333  0.5700820
##    3    1         rectangular   0.6883333  0.5218586
##    3    1         epanechnikov  0.7253333  0.5758547
##    3    1         biweight      0.7025000  0.5437419
##    3    1         triweight     0.7005000  0.5407657
##    3    1         cos           0.7253333  0.5758547
##    3    1         inv           0.7533333  0.6198543
##    3    1         rank          0.6988333  0.5384580
##    3    1         optimal       0.6988333  0.5384580
##    3    2         gaussian      0.6618333  0.4887230
##    3    2         triangular    0.6625000  0.4848621
##    3    2         rectangular   0.6528333  0.4717062
##    3    2         epanechnikov  0.6568333  0.4769527
##    3    2         biweight      0.6661667  0.4914732
##    3    2         triweight     0.6645000  0.4891655
##    3    2         cos           0.6568333  0.4769527
##    3    2         inv           0.6693333  0.5011309
##    3    2         rank          0.6645000  0.4891655
##    3    2         optimal       0.6645000  0.4891655
##    4    1         gaussian      0.6955000  0.5252058
##    4    1         triangular    0.7388333  0.5957438
##    4    1         rectangular   0.6816667  0.5104599
##    4    1         epanechnikov  0.7196667  0.5651516
##    4    1         biweight      0.7065000  0.5493669
##    4    1         triweight     0.7025000  0.5432657
##    4    1         cos           0.7196667  0.5656062
##    4    1         inv           0.7566667  0.6228734
##    4    1         rank          0.6870000  0.5188515
##    4    1         optimal       0.6988333  0.5384580
##    4    2         gaussian      0.6366667  0.4474663
##    4    2         triangular    0.6780000  0.5059928
##    4    2         rectangular   0.6463333  0.4614909
##    4    2         epanechnikov  0.6760000  0.5049499
##    4    2         biweight      0.6698333  0.4958962
##    4    2         triweight     0.6645000  0.4891655
##    4    2         cos           0.6760000  0.5053911
##    4    2         inv           0.7018333  0.5453832
##    4    2         rank          0.6553333  0.4799421
##    4    2         optimal       0.6645000  0.4891655
##    5    1         gaussian      0.6983333  0.5290557
##    5    1         triangular    0.7413333  0.5993802
##    5    1         rectangular   0.6816667  0.5104599
##    5    1         epanechnikov  0.7196667  0.5651516
##    5    1         biweight      0.7130000  0.5597038
##    5    1         triweight     0.7045000  0.5463907
##    5    1         cos           0.7196667  0.5656062
##    5    1         inv           0.7545000  0.6149203
##    5    1         rank          0.6878333  0.5192746
##    5    1         optimal       0.6988333  0.5384580
##    5    2         gaussian      0.6321667  0.4376135
##    5    2         triangular    0.6890000  0.5294797
##    5    2         rectangular   0.6368333  0.4438416
##    5    2         epanechnikov  0.6765000  0.5026932
##    5    2         biweight      0.6738333  0.5015212
##    5    2         triweight     0.6661667  0.4914732
##    5    2         cos           0.6866667  0.5204374
##    5    2         inv           0.6915000  0.5274418
##    5    2         rank          0.6378333  0.4508805
##    5    2         optimal       0.6645000  0.4891655
##    6    1         gaussian      0.7135000  0.5512507
##    6    1         triangular    0.7331667  0.5849217
##    6    1         rectangular   0.6851667  0.5098752
##    6    1         epanechnikov  0.7216667  0.5681511
##    6    1         biweight      0.7341667  0.5950348
##    6    1         triweight     0.7045000  0.5463907
##    6    1         cos           0.7196667  0.5657112
##    6    1         inv           0.7390000  0.5862858
##    6    1         rank          0.6873333  0.5158415
##    6    1         optimal       0.7150000  0.5573632
##    6    2         gaussian      0.6300000  0.4330561
##    6    2         triangular    0.6865000  0.5252481
##    6    2         rectangular   0.6368333  0.4430335
##    6    2         epanechnikov  0.6740000  0.4990569
##    6    2         biweight      0.6755000  0.5061366
##    6    2         triweight     0.6701667  0.4970982
##    6    2         cos           0.6841667  0.5168011
##    6    2         inv           0.6915000  0.5274418
##    6    2         rank          0.6353333  0.4465168
##    6    2         optimal       0.6830000  0.5112294
##    7    1         gaussian      0.7266667  0.5697283
##    7    1         triangular    0.7333333  0.5848732
##    7    1         rectangular   0.6831667  0.5050948
##    7    1         epanechnikov  0.7193333  0.5647723
##    7    1         biweight      0.7425000  0.6072951
##    7    1         triweight     0.7168333  0.5640172
##    7    1         cos           0.7263333  0.5755395
##    7    1         inv           0.7370000  0.5831608
##    7    1         rank          0.6873333  0.5146658
##    7    1         optimal       0.7081667  0.5432074
##    7    2         gaussian      0.6300000  0.4330561
##    7    2         triangular    0.6873333  0.5263845
##    7    2         rectangular   0.6343333  0.4367352
##    7    2         epanechnikov  0.6733333  0.4974808
##    7    2         biweight      0.6930000  0.5308223
##    7    2         triweight     0.6805000  0.5111017
##    7    2         cos           0.6810000  0.5115518
##    7    2         inv           0.6915000  0.5274418
##    7    2         rank          0.6320000  0.4393481
##    7    2         optimal       0.6840000  0.5094441
##    8    1         gaussian      0.7266667  0.5695649
##    8    1         triangular    0.7328333  0.5877855
##    8    1         rectangular   0.6831667  0.5050948
##    8    1         epanechnikov  0.7233333  0.5694830
##    8    1         biweight      0.7335000  0.5904740
##    8    1         triweight     0.7356667  0.5961613
##    8    1         cos           0.7303333  0.5850582
##    8    1         inv           0.7370000  0.5829974
##    8    1         rank          0.6946667  0.5241235
##    8    1         optimal       0.7081667  0.5432074
##    8    2         gaussian      0.6333333  0.4380561
##    8    2         triangular    0.6770000  0.5079989
##    8    2         rectangular   0.6343333  0.4367352
##    8    2         epanechnikov  0.6733333  0.4995901
##    8    2         biweight      0.6955000  0.5340950
##    8    2         triweight     0.6830000  0.5147380
##    8    2         cos           0.6770000  0.5071835
##    8    2         inv           0.6915000  0.5274418
##    8    2         rank          0.6295000  0.4353481
##    8    2         optimal       0.6816667  0.5022463
##    9    1         gaussian      0.7266667  0.5695649
##    9    1         triangular    0.7228333  0.5706585
##    9    1         rectangular   0.6831667  0.5050948
##    9    1         epanechnikov  0.7351667  0.5904216
##    9    1         biweight      0.7183333  0.5665362
##    9    1         triweight     0.7535000  0.6229553
##    9    1         cos           0.7248333  0.5755027
##    9    1         inv           0.7378333  0.5817254
##    9    1         rank          0.6926667  0.5211824
##    9    1         optimal       0.7036667  0.5346771
##    9    2         gaussian      0.6353333  0.4405071
##    9    2         triangular    0.6770000  0.5079989
##    9    2         rectangular   0.6343333  0.4367352
##    9    2         epanechnikov  0.6733333  0.4995901
##    9    2         biweight      0.6935000  0.5309700
##    9    2         triweight     0.6830000  0.5141428
##    9    2         cos           0.6745000  0.5014692
##    9    2         inv           0.6935000  0.5298928
##    9    2         rank          0.6295000  0.4353481
##    9    2         optimal       0.6766667  0.4946100
##   10    1         gaussian      0.7266667  0.5695649
##   10    1         triangular    0.7270000  0.5767949
##   10    1         rectangular   0.6831667  0.5050948
##   10    1         epanechnikov  0.7446667  0.6031177
##   10    1         biweight      0.7158333  0.5626083
##   10    1         triweight     0.7535000  0.6229553
##   10    1         cos           0.7335000  0.5877442
##   10    1         inv           0.7378333  0.5817254
##   10    1         rank          0.6926667  0.5211824
##   10    1         optimal       0.7036667  0.5346771
##   10    2         gaussian      0.6368333  0.4338187
##   10    2         triangular    0.6745000  0.5022846
##   10    2         rectangular   0.6343333  0.4367352
##   10    2         epanechnikov  0.6713333  0.4960186
##   10    2         biweight      0.6960000  0.5354145
##   10    2         triweight     0.6920000  0.5271983
##   10    2         cos           0.6725000  0.4978977
##   10    2         inv           0.6935000  0.5298928
##   10    2         rank          0.6295000  0.4353481
##   10    2         optimal       0.6721667  0.4870385
##   11    1         gaussian      0.7216667  0.5606760
##   11    1         triangular    0.7270000  0.5767949
##   11    1         rectangular   0.6831667  0.5050948
##   11    1         epanechnikov  0.7446667  0.6031177
##   11    1         biweight      0.7195000  0.5659922
##   11    1         triweight     0.7515000  0.6201776
##   11    1         cos           0.7368333  0.5927442
##   11    1         inv           0.7378333  0.5817254
##   11    1         rank          0.6906667  0.5182412
##   11    1         optimal       0.7036667  0.5346771
##   11    2         gaussian      0.6368333  0.4338187
##   11    2         triangular    0.6685000  0.4863799
##   11    2         rectangular   0.6343333  0.4367352
##   11    2         epanechnikov  0.6713333  0.4960186
##   11    2         biweight      0.6960000  0.5354145
##   11    2         triweight     0.6986667  0.5340815
##   11    2         cos           0.6685000  0.4895644
##   11    2         inv           0.6935000  0.5298928
##   11    2         rank          0.6295000  0.4353481
##   11    2         optimal       0.6721667  0.4870385
##   12    1         gaussian      0.7216667  0.5606760
##   12    1         triangular    0.7290000  0.5798893
##   12    1         rectangular   0.6831667  0.5050948
##   12    1         epanechnikov  0.7446667  0.6031177
##   12    1         biweight      0.7228333  0.5719922
##   12    1         triweight     0.7485000  0.6131953
##   12    1         cos           0.7368333  0.5927442
##   12    1         inv           0.7378333  0.5817254
##   12    1         rank          0.6906667  0.5182412
##   12    1         optimal       0.7016667  0.5318993
##   12    2         gaussian      0.6338333  0.4248306
##   12    2         triangular    0.6660000  0.4823799
##   12    2         rectangular   0.6343333  0.4367352
##   12    2         epanechnikov  0.6646667  0.4852665
##   12    2         biweight      0.6960000  0.5354145
##   12    2         triweight     0.7073333  0.5470226
##   12    2         cos           0.6660000  0.4855644
##   12    2         inv           0.6935000  0.5298928
##   12    2         rank          0.6295000  0.4353481
##   12    2         optimal       0.6721667  0.4870385
##   13    1         gaussian      0.7216667  0.5606760
##   13    1         triangular    0.7290000  0.5798893
##   13    1         rectangular   0.6831667  0.5050948
##   13    1         epanechnikov  0.7446667  0.6031177
##   13    1         biweight      0.7228333  0.5719922
##   13    1         triweight     0.7485000  0.6131953
##   13    1         cos           0.7368333  0.5927442
##   13    1         inv           0.7378333  0.5817254
##   13    1         rank          0.6906667  0.5182412
##   13    1         optimal       0.6980000  0.5253033
##   13    2         gaussian      0.6338333  0.4248306
##   13    2         triangular    0.6660000  0.4823799
##   13    2         rectangular   0.6343333  0.4367352
##   13    2         epanechnikov  0.6646667  0.4852665
##   13    2         biweight      0.6960000  0.5354145
##   13    2         triweight     0.7110000  0.5516825
##   13    2         cos           0.6660000  0.4855644
##   13    2         inv           0.6935000  0.5298928
##   13    2         rank          0.6295000  0.4353481
##   13    2         optimal       0.6721667  0.4870385
##   14    1         gaussian      0.7216667  0.5606760
##   14    1         triangular    0.7290000  0.5798893
##   14    1         rectangular   0.6831667  0.5050948
##   14    1         epanechnikov  0.7446667  0.6031177
##   14    1         biweight      0.7278333  0.5784207
##   14    1         triweight     0.7465000  0.6106158
##   14    1         cos           0.7368333  0.5927442
##   14    1         inv           0.7378333  0.5817254
##   14    1         rank          0.6906667  0.5182412
##   14    1         optimal       0.6960000  0.5221987
##   14    2         gaussian      0.6278333  0.4117261
##   14    2         triangular    0.6660000  0.4823799
##   14    2         rectangular   0.6343333  0.4367352
##   14    2         epanechnikov  0.6646667  0.4852665
##   14    2         biweight      0.6960000  0.5354145
##   14    2         triweight     0.7135000  0.5561269
##   14    2         cos           0.6660000  0.4855644
##   14    2         inv           0.6935000  0.5298928
##   14    2         rank          0.6295000  0.4353481
##   14    2         optimal       0.6721667  0.4870385
##   15    1         gaussian      0.7216667  0.5606760
##   15    1         triangular    0.7290000  0.5798893
##   15    1         rectangular   0.6831667  0.5050948
##   15    1         epanechnikov  0.7446667  0.6031177
##   15    1         biweight      0.7278333  0.5784207
##   15    1         triweight     0.7448333  0.6043441
##   15    1         cos           0.7368333  0.5927442
##   15    1         inv           0.7378333  0.5817254
##   15    1         rank          0.6906667  0.5182412
##   15    1         optimal       0.6976667  0.5246987
##   15    2         gaussian      0.6278333  0.4117261
##   15    2         triangular    0.6660000  0.4823799
##   15    2         rectangular   0.6343333  0.4367352
##   15    2         epanechnikov  0.6646667  0.4852665
##   15    2         biweight      0.6940000  0.5312478
##   15    2         triweight     0.7131667  0.5551754
##   15    2         cos           0.6660000  0.4855644
##   15    2         inv           0.6935000  0.5298928
##   15    2         rank          0.6295000  0.4353481
##   15    2         optimal       0.6721667  0.4870385
## 
## Kappa was used to select the optimal model using the largest value.
## The final values used for the model were kmax = 10, distance = 1 and kernel
##  = triweight.
```

Sensibilidad ...  


```r
# Validar con el test set
caret_rf_result <- predict(caret_rf, newdata = caret_test)
confusionMatrix(caret_rf_result, caret_test$secuencia)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction A1 A2 B1 B2
##         A1  2  0  0  0
##         A2  0  1  0  0
##         B1  0  0  1  0
##         B2  0  1  1  8
## 
## Overall Statistics
##                                           
##                Accuracy : 0.8571          
##                  95% CI : (0.5719, 0.9822)
##     No Information Rate : 0.5714          
##     P-Value [Acc > NIR] : 0.02481         
##                                           
##                   Kappa : 0.7407          
##                                           
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A1 Class: A2 Class: B1 Class: B2
## Sensitivity             1.0000   0.50000   0.50000    1.0000
## Specificity             1.0000   1.00000   1.00000    0.6667
## Pos Pred Value          1.0000   1.00000   1.00000    0.8000
## Neg Pred Value          1.0000   0.92308   0.92308    1.0000
## Prevalence              0.1429   0.14286   0.14286    0.5714
## Detection Rate          0.1429   0.07143   0.07143    0.5714
## Detection Prevalence    0.1429   0.07143   0.07143    0.7143
## Balanced Accuracy       1.0000   0.75000   0.75000    0.8333
```

```r
caret_knn_result <- predict(caret_knn, newdata = caret_test )
confusionMatrix(caret_knn_result, caret_test$secuencia)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction A1 A2 B1 B2
##         A1  2  0  0  0
##         A2  0  1  0  0
##         B1  0  0  1  0
##         B2  0  1  1  8
## 
## Overall Statistics
##                                           
##                Accuracy : 0.8571          
##                  95% CI : (0.5719, 0.9822)
##     No Information Rate : 0.5714          
##     P-Value [Acc > NIR] : 0.02481         
##                                           
##                   Kappa : 0.7407          
##                                           
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A1 Class: A2 Class: B1 Class: B2
## Sensitivity             1.0000   0.50000   0.50000    1.0000
## Specificity             1.0000   1.00000   1.00000    0.6667
## Pos Pred Value          1.0000   1.00000   1.00000    0.8000
## Neg Pred Value          1.0000   0.92308   0.92308    1.0000
## Prevalence              0.1429   0.14286   0.14286    0.5714
## Detection Rate          0.1429   0.07143   0.07143    0.5714
## Detection Prevalence    0.1429   0.07143   0.07143    0.7143
## Balanced Accuracy       1.0000   0.75000   0.75000    0.8333
```

La matriz de confusión ...



knn tiene distribución discreta de valores de probabilidades para las diferentes secuencias, mientras que la distribución de valores para rf es masomenos continua. En el primer caso, las probabilidades están limitadas por la relación entre las secuencias y el número de vecinos k. En el segundo caso ... 






```r
p_modelos <- ggplot() + 
  geom_raster(data = predicted_raster, 
              aes(x = long, y = lat, 
                  fill = secuencia ,
                  alpha = prob))+
  geom_sf(data = CSsf_18N, fill = NA) +
  ggsn::scalebar(data = CSsf_18N, 
           dist = 0.5, 
           dist_unit = "km",
           transform = FALSE,
           st.size = 3,
           height=0.015,
           border.size = 0.05,
           box.color = "#e2ddd6",
           box.fill = c("grey20","#e2ddd6"),
           family = "robotoc" )+
  scale_fill_manual(values = col_scp)+
  scale_alpha_continuous(guide = "none")+
  facet_wrap(vars(modelo))+
  labs(x = "", y = "")+
  scale_x_continuous(breaks=c(-74.18, -74.17))+
  scale_y_continuous(breaks=c(4.55,4.56)) +
  theme(strip.text = element_text(family = "roboto", 
                                  face = "bold",
                                  size = 11,
                                  color = "grey20"))

p_modelos
```

<img src="C:\Users\cguio\DOCUME~1\Terrae\TCI_CE~1\_Git\Reportes\06_TCI~1/figure-html/plot_models-1.png" width="80%" style="display: block; margin: auto;" />

knnn predijo secuencias distribuídas sobre grandes áreas, mientras que rf predijo una distribución mas fina.

Iportancia...



```r
imp_rf <- varImp(caret_rf)$importance %>%
          mutate(variable = factor(row.names(.))) %>%
          pivot_longer(cols = !variable, 
                       names_to = "secuencia", 
                       values_to = "importancia") 

p_imp_rf <- imp_rf %>%
  # Las distanciaS son las mas importantes, pero no útiles para interpretar procesos
  filter(!variable %in% c("dist_A1", "dist_A2", "dist_B1", "dist_B2")) %>%
  group_by(secuencia) %>%
  arrange(desc(importancia)) %>%
  slice(1:5) %>%
ggplot()+
  geom_bar(aes(x = importancia, y = factor(importancia)), 
           stat = "identity",
           fill = darken("#c3beb8", 0.5, space = "HCL")) +
    geom_text(aes(x = importancia, y = factor(importancia), label = variable), 
            hjust=1.15,
            colour = lighten("#c3beb8", 0.5, space = "HCL"),
            family = "robotoc",
            fontface = "bold",
            size = 3.5) +
  facet_wrap(~secuencia, scales = "free_y", ncol = 1) +
  xlab("Importancia") +
  theme(plot.margin = margin(0, 10, 0, 20),
        plot.background = element_rect(fill = "white", color = "white"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(family = "roboto", 
                                  face = "bold",
                                  size = 11,
                                  color = "grey20"),
        axis.text.x = element_text(vjust = -2),
        axis.ticks.x = element_blank(),
        legend.position = "none",
        axis.title.x.bottom = element_text(margin = margin(10, 0, 0, 0))
    )

p_imp_rf
```

<img src="C:\Users\cguio\DOCUME~1\Terrae\TCI_CE~1\_Git\Reportes\06_TCI~1/figure-html/plot_imp_rf-1.png" width="80%" style="display: block; margin: auto;" />




Las variables importantes tienen la siguiente distribución en el área


```r
imp_vars <- imp_rf %>%
  # Las distanciaS son las mas importantes, pero no útiles para interpretar procesos
  filter(!variable %in% c("dist_A1", "dist_A2", "dist_B1", "dist_B2")) %>%
  group_by(secuencia) %>%
  arrange(desc(importancia)) %>%
  slice(1:5) %>%
  ungroup()%>%
  dplyr::select(variable) %>%
  distinct() %>%
  unlist() %>%
  as.character() 

p_imp_ras <- predict_set_rf %>%
  dplyr::select(-modelo, -secuencia) %>%
  pivot_longer(cols = -c(long,lat), 
               names_to = "variable", 
               values_to = "valor") %>%
  filter(variable %in% imp_vars) %>%
  ggplot() +
  geom_raster(aes(x = long, y = lat, 
                  fill = valor)) +
  facet_wrap(~variable, nrow = 4) +
  viridis::scale_fill_viridis() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(family = "roboto", 
                                  face = "bold",
                                  size = 10,
                                  colour = "grey20"),
        plot.background = element_rect(color =  "grey96",
                                            fill =  "grey96", 
                                            size = 2),
        legend.position = "right",
        legend.title = element_blank())+
        guides(colour = "none",
         fill = guide_colourbar(barheight = unit(12,"lines"),
                                barwidth = unit(0.4,"lines")))+
  coord_fixed()
```



```r
p_imp <- p_imp_rf + p_imp_ras + plot_layout(widths = c(1, 2))

p_imp
```

<img src="C:\Users\cguio\DOCUME~1\Terrae\TCI_CE~1\_Git\Reportes\06_TCI~1/figure-html/plot_imp-1.png" width="80%" style="display: block; margin: auto;" />


