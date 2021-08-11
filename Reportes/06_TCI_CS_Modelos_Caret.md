---
title: "TCI - Cerro Seco / Suelos"
subtitle: "Modelado II: caret"
author: "Carlos Guio"
date: "2021-08-11"
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


#Calculate category raster for watershed basin
# later do one hot encoding

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
          dplyr::mutate(secuencia = as.factor(secuencia))


#Clip interno de poligonos mineros
covars_clip <- mask(covars, min2019sf_18N, inverse = TRUE)

#Clip raster a poligono externo: 
# se hace al final, porque algunos puntos para entrenamiento caen fuera del poligono
covars_clip <- mask(covars_clip, CSsf_18N)

# Crear df de covariables. NAs removed corespond to pixels outside the polygon
covars_df <- as.data.frame(covars_clip, xy= TRUE, na.rm = TRUE) %>%
             rename(long = x, lat = y) 

#Check plot
plot(covars_clip[[c("DEM","SLP", "NDVI", "dist_B2")]])
```

<img src="C:\Users\cguio\DOCUME~1\Terrae\TCI_CE~1\_Git\Reportes\06_TCI~1/figure-html/prep_covar-2.png" width="80%" style="display: block; margin: auto;" />

La predicción sin ajustar devulve un kapp de 0.2 - 0.3. Factores que afectan son el porcentaje de la partición, el tipo de remuestreo (cv vs loocv), el desbalance de clases, la falta de representatividad de las covariables raster en el set de entrenamiento, la selección de variables. A continuación se mencionan los efectos:



Ensayé partición 0.7, 0.75 y 0.8. Al aumentar el porcentaje de observaciones en el set de entrenamiento ...



Los efectos del preprocesamiento fueron ...



El modelo con remuestreo loocv tardó mas en correr, pero mostró una mejora en el Kappa de 0.1 a 0.2 puntos, el mejor en random forest. Al predecir sobre el raster ambos modelos mostraron una mejora en la granularidad, es decir, la distribución de las secuencias adoptaba patrones relacionados a las covariables geográficas. 







```r
model_vars <- caret_train %>%
              dplyr:: select(-secuencia, -lat, -long) %>%
              colnames() %>%
              paste(collapse = "+")

formula_train <- as.formula(paste("secuencia ~", model_vars)) 

# train random forest model
set.seed(1)
caret_rf <- train(formula_train,
                  data = caret_train,
                  method = "rf",
                  metric = "Kappa",
                  importance = TRUE,
                  trControl = caret_cv,
                  ntree = 5000,   #no need to tune this (Hengl et al. 2018)
                  tuneGrid = tuneGrid_rf)

caret_rf
```

```
## Random Forest 
## 
## 47 samples
## 24 predictors
##  4 classes: 'A1', 'A2', 'B1', 'B2' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold, repeated 10 times) 
## Summary of sample sizes: 43, 41, 41, 43, 42, 42, ... 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy   Kappa    
##    1    0.7381667  0.5479278
##    2    0.8515000  0.7364905
##    3    0.9101667  0.8525562
##    4    0.9470000  0.9130615
##    5    0.9593333  0.9327436
##    6    0.9593333  0.9327436
##    7    0.9593333  0.9327436
##    8    0.9593333  0.9327436
##    9    0.9593333  0.9327436
##   10    0.9593333  0.9327436
## 
## Kappa was used to select the optimal model using the largest value.
## The final value used for the model was mtry = 5.
```

```r
#train knn model
set.seed(1)
caret_knn <- train(formula_train,
                  data = caret_train,
                  method = "kknn",
                  metric = "Kappa",
                  trControl = caret_cv,
                  tuneGrid = tuneGrid_knn)

caret_knn
```

```
## k-Nearest Neighbors 
## 
## 47 samples
## 24 predictors
##  4 classes: 'A1', 'A2', 'B1', 'B2' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold, repeated 10 times) 
## Summary of sample sizes: 43, 41, 41, 43, 42, 42, ... 
## Resampling results across tuning parameters:
## 
##   kmax  distance  kernel        Accuracy   Kappa     
##    2    1         gaussian      0.5793333  0.31862479
##    2    1         triangular    0.5793333  0.31862479
##    2    1         rectangular   0.5793333  0.31862479
##    2    1         epanechnikov  0.5793333  0.31862479
##    2    1         biweight      0.5793333  0.31862479
##    2    1         triweight     0.5793333  0.31862479
##    2    1         cos           0.5793333  0.31862479
##    2    1         inv           0.5793333  0.31862479
##    2    1         rank          0.5793333  0.31862479
##    2    1         optimal       0.5793333  0.31862479
##    2    2         gaussian      0.5345000  0.28044913
##    2    2         triangular    0.5345000  0.28044913
##    2    2         rectangular   0.5345000  0.28044913
##    2    2         epanechnikov  0.5345000  0.28044913
##    2    2         biweight      0.5345000  0.28044913
##    2    2         triweight     0.5345000  0.28044913
##    2    2         cos           0.5345000  0.28044913
##    2    2         inv           0.5345000  0.28044913
##    2    2         rank          0.5345000  0.28044913
##    2    2         optimal       0.5345000  0.28044913
##    3    1         gaussian      0.5418333  0.25318359
##    3    1         triangular    0.5473333  0.26548387
##    3    1         rectangular   0.5525000  0.26970150
##    3    1         epanechnikov  0.5478333  0.26442504
##    3    1         biweight      0.5715000  0.30155409
##    3    1         triweight     0.5715000  0.30155409
##    3    1         cos           0.5478333  0.26442504
##    3    1         inv           0.5455000  0.25509376
##    3    1         rank          0.5773333  0.31601041
##    3    1         optimal       0.5793333  0.31862479
##    3    2         gaussian      0.5063333  0.21492544
##    3    2         triangular    0.5295000  0.26819764
##    3    2         rectangular   0.5096667  0.23129022
##    3    2         epanechnikov  0.5355000  0.26512922
##    3    2         biweight      0.5310000  0.27048623
##    3    2         triweight     0.5345000  0.28044913
##    3    2         cos           0.5385000  0.27383986
##    3    2         inv           0.5076667  0.21822060
##    3    2         rank          0.5285000  0.27222698
##    3    2         optimal       0.5345000  0.28044913
##    4    1         gaussian      0.5385000  0.24120012
##    4    1         triangular    0.5456667  0.26113033
##    4    1         rectangular   0.5525000  0.26780251
##    4    1         epanechnikov  0.5531667  0.27051819
##    4    1         biweight      0.5611667  0.27945910
##    4    1         triweight     0.5715000  0.30155409
##    4    1         cos           0.5478333  0.26361696
##    4    1         inv           0.5455000  0.25509376
##    4    1         rank          0.5670000  0.29774526
##    4    1         optimal       0.5793333  0.31862479
##    4    2         gaussian      0.4983333  0.19756000
##    4    2         triangular    0.5295000  0.26819764
##    4    2         rectangular   0.5060000  0.22127509
##    4    2         epanechnikov  0.5338333  0.26172013
##    4    2         biweight      0.5293333  0.26325180
##    4    2         triweight     0.5193333  0.25290610
##    4    2         cos           0.5385000  0.27383986
##    4    2         inv           0.5060000  0.19576620
##    4    2         rank          0.5130000  0.24206583
##    4    2         optimal       0.5345000  0.28044913
##    5    1         gaussian      0.5380000  0.23554404
##    5    1         triangular    0.5433333  0.25596545
##    5    1         rectangular   0.5450000  0.23593232
##    5    1         epanechnikov  0.5450000  0.25505280
##    5    1         biweight      0.5546667  0.26735991
##    5    1         triweight     0.5658333  0.28960412
##    5    1         cos           0.5413333  0.25087885
##    5    1         inv           0.5408333  0.24003969
##    5    1         rank          0.5428333  0.24716682
##    5    1         optimal       0.5793333  0.31862479
##    5    2         gaussian      0.4958333  0.18527725
##    5    2         triangular    0.5173333  0.23999310
##    5    2         rectangular   0.4851667  0.17803100
##    5    2         epanechnikov  0.5315000  0.25346478
##    5    2         biweight      0.5231667  0.25171392
##    5    2         triweight     0.5193333  0.25140610
##    5    2         cos           0.5325000  0.25725147
##    5    2         inv           0.5060000  0.19576620
##    5    2         rank          0.5031667  0.21737367
##    5    2         optimal       0.5345000  0.28044913
##    6    1         gaussian      0.5486667  0.24983440
##    6    1         triangular    0.5433333  0.25565585
##    6    1         rectangular   0.5390000  0.22477391
##    6    1         epanechnikov  0.5430000  0.25180202
##    6    1         biweight      0.5486667  0.25729047
##    6    1         triweight     0.5638333  0.28666294
##    6    1         cos           0.5450000  0.25474320
##    6    1         inv           0.5371667  0.22992554
##    6    1         rank          0.5415000  0.24314301
##    6    1         optimal       0.5695000  0.30106998
##    6    2         gaussian      0.5065000  0.19917597
##    6    2         triangular    0.5155000  0.23345911
##    6    2         rectangular   0.4905000  0.18209846
##    6    2         epanechnikov  0.5436667  0.26154803
##    6    2         biweight      0.5166667  0.23516668
##    6    2         triweight     0.5156667  0.24244089
##    6    2         cos           0.5348333  0.25236508
##    6    2         inv           0.5060000  0.19576620
##    6    2         rank          0.4953333  0.19275341
##    6    2         optimal       0.5143333  0.24328593
##    7    1         gaussian      0.5600000  0.25856594
##    7    1         triangular    0.5433333  0.25565585
##    7    1         rectangular   0.5295000  0.20778978
##    7    1         epanechnikov  0.5371667  0.24124647
##    7    1         biweight      0.5486667  0.25729047
##    7    1         triweight     0.5638333  0.28666294
##    7    1         cos           0.5450000  0.25474320
##    7    1         inv           0.5506667  0.24179188
##    7    1         rank          0.5500000  0.25464627
##    7    1         optimal       0.5641667  0.28814923
##    7    2         gaussian      0.5101667  0.19869034
##    7    2         triangular    0.5171667  0.23442221
##    7    2         rectangular   0.4871667  0.17380412
##    7    2         epanechnikov  0.5465000  0.25780017
##    7    2         biweight      0.5191667  0.23850001
##    7    2         triweight     0.5120000  0.23496470
##    7    2         cos           0.5401667  0.25412428
##    7    2         inv           0.5040000  0.19071982
##    7    2         rank          0.4953333  0.19149467
##    7    2         optimal       0.5086667  0.23120060
##    8    1         gaussian      0.5713333  0.26937885
##    8    1         triangular    0.5383333  0.24070480
##    8    1         rectangular   0.5253333  0.18018046
##    8    1         epanechnikov  0.5355000  0.23806465
##    8    1         biweight      0.5486667  0.25729047
##    8    1         triweight     0.5613333  0.28229931
##    8    1         cos           0.5400000  0.24456138
##    8    1         inv           0.5670000  0.25337672
##    8    1         rank          0.5480000  0.24815363
##    8    1         optimal       0.5625000  0.28460378
##    8    2         gaussian      0.5110000  0.19730226
##    8    2         triangular    0.5155000  0.23042221
##    8    2         rectangular   0.4753333  0.14325678
##    8    2         epanechnikov  0.5425000  0.24981405
##    8    2         biweight      0.5155000  0.23092244
##    8    2         triweight     0.5095000  0.22969198
##    8    2         cos           0.5405000  0.25187498
##    8    2         inv           0.5008333  0.18116426
##    8    2         rank          0.4920000  0.18558558
##    8    2         optimal       0.5036667  0.21891302
##    9    1         gaussian      0.5693333  0.26585321
##    9    1         triangular    0.5353333  0.23582980
##    9    1         rectangular   0.5228333  0.17555219
##    9    1         epanechnikov  0.5378333  0.23727821
##    9    1         biweight      0.5486667  0.25729047
##    9    1         triweight     0.5613333  0.28229931
##    9    1         cos           0.5358333  0.23518764
##    9    1         inv           0.5670000  0.25337672
##    9    1         rank          0.5521667  0.24910134
##    9    1         optimal       0.5625000  0.28460378
##    9    2         gaussian      0.5098333  0.18891536
##    9    2         triangular    0.5155000  0.23042221
##    9    2         rectangular   0.4753333  0.14325678
##    9    2         epanechnikov  0.5425000  0.24981405
##    9    2         biweight      0.5130000  0.22400325
##    9    2         triweight     0.5095000  0.22969198
##    9    2         cos           0.5425000  0.25382351
##    9    2         inv           0.5008333  0.17876054
##    9    2         rank          0.4876667  0.17317552
##    9    2         optimal       0.5050000  0.20881054
##   10    1         gaussian      0.5773333  0.27322626
##   10    1         triangular    0.5353333  0.23586481
##   10    1         rectangular   0.5228333  0.17555219
##   10    1         epanechnikov  0.5435000  0.24356542
##   10    1         biweight      0.5486667  0.25729047
##   10    1         triweight     0.5613333  0.28229931
##   10    1         cos           0.5358333  0.23522266
##   10    1         inv           0.5750000  0.26123997
##   10    1         rank          0.5578333  0.24552836
##   10    1         optimal       0.5543333  0.26919243
##   10    2         gaussian      0.5026667  0.17399491
##   10    2         triangular    0.5171667  0.22639074
##   10    2         rectangular   0.4730000  0.12271597
##   10    2         epanechnikov  0.5408333  0.24455531
##   10    2         biweight      0.5118333  0.21967222
##   10    2         triweight     0.5131667  0.23243291
##   10    2         cos           0.5458333  0.25456477
##   10    2         inv           0.4903333  0.15477844
##   10    2         rank          0.4851667  0.16791898
##   10    2         optimal       0.5050000  0.20881054
##   11    1         gaussian      0.5803333  0.26830201
##   11    1         triangular    0.5373333  0.23713606
##   11    1         rectangular   0.5228333  0.17263112
##   11    1         epanechnikov  0.5468333  0.24801997
##   11    1         biweight      0.5420000  0.24279905
##   11    1         triweight     0.5613333  0.28229931
##   11    1         cos           0.5395000  0.23767572
##   11    1         inv           0.5750000  0.26123997
##   11    1         rank          0.5658333  0.25490336
##   11    1         optimal       0.5498333  0.26125534
##   11    2         gaussian      0.5063333  0.17454276
##   11    2         triangular    0.5285000  0.23343627
##   11    2         rectangular   0.4618333  0.08836057
##   11    2         epanechnikov  0.5481667  0.25321953
##   11    2         biweight      0.5118333  0.21967222
##   11    2         triweight     0.5070000  0.21743544
##   11    2         cos           0.5551667  0.26453699
##   11    2         inv           0.4905000  0.15094375
##   11    2         rank          0.4758333  0.15080884
##   11    2         optimal       0.5030000  0.20086937
##   12    1         gaussian      0.5766667  0.25975813
##   12    1         triangular    0.5356667  0.23408199
##   12    1         rectangular   0.5228333  0.17263112
##   12    1         epanechnikov  0.5508333  0.24864005
##   12    1         biweight      0.5415000  0.24033080
##   12    1         triweight     0.5613333  0.28229931
##   12    1         cos           0.5451667  0.24440596
##   12    1         inv           0.5693333  0.25207887
##   12    1         rank          0.5638333  0.25124732
##   12    1         optimal       0.5473333  0.24969978
##   12    2         gaussian      0.5093333  0.16042773
##   12    2         triangular    0.5296667  0.23005290
##   12    2         rectangular   0.4623333  0.07912224
##   12    2         epanechnikov  0.5465000  0.24816546
##   12    2         biweight      0.5118333  0.21967222
##   12    2         triweight     0.5045000  0.20936849
##   12    2         cos           0.5571667  0.26105185
##   12    2         inv           0.4888333  0.14694375
##   12    2         rank          0.4775000  0.15124903
##   12    2         optimal       0.5013333  0.19686937
##   13    1         gaussian      0.5780000  0.25312626
##   13    1         triangular    0.5426667  0.23932590
##   13    1         rectangular   0.5228333  0.17263112
##   13    1         epanechnikov  0.5538333  0.24288648
##   13    1         biweight      0.5411667  0.23889140
##   13    1         triweight     0.5613333  0.28129931
##   13    1         cos           0.5451667  0.23422273
##   13    1         inv           0.5673333  0.24913770
##   13    1         rank          0.5601667  0.24435338
##   13    1         optimal       0.5473333  0.24969978
##   13    2         gaussian      0.5188333  0.15579644
##   13    2         triangular    0.5305000  0.21863342
##   13    2         rectangular   0.4626667  0.06886570
##   13    2         epanechnikov  0.5520000  0.24703754
##   13    2         biweight      0.5078333  0.21348027
##   13    2         triweight     0.5011667  0.20181702
##   13    2         cos           0.5531667  0.24506791
##   13    2         inv           0.4888333  0.14694375
##   13    2         rank          0.4675000  0.12755000
##   13    2         optimal       0.5033333  0.19505941
##   14    1         gaussian      0.5780000  0.25312626
##   14    1         triangular    0.5475000  0.23851743
##   14    1         rectangular   0.5228333  0.17263112
##   14    1         epanechnikov  0.5608333  0.24984829
##   14    1         biweight      0.5431667  0.23992527
##   14    1         triweight     0.5613333  0.28129931
##   14    1         cos           0.5460000  0.22964905
##   14    1         inv           0.5673333  0.24913770
##   14    1         rank          0.5601667  0.24402658
##   14    1         optimal       0.5473333  0.24858867
##   14    2         gaussian      0.5163333  0.14850327
##   14    2         triangular    0.5268333  0.20746183
##   14    2         rectangular   0.4623333  0.06133035
##   14    2         epanechnikov  0.5495000  0.23934057
##   14    2         biweight      0.5033333  0.20410527
##   14    2         triweight     0.5036667  0.20333722
##   14    2         cos           0.5506667  0.23737094
##   14    2         inv           0.4888333  0.14631875
##   14    2         rank          0.4633333  0.11744899
##   14    2         optimal       0.4965000  0.18053801
##   15    1         gaussian      0.5780000  0.25312626
##   15    1         triangular    0.5495000  0.24076416
##   15    1         rectangular   0.5228333  0.17263112
##   15    1         epanechnikov  0.5666667  0.25840384
##   15    1         biweight      0.5395000  0.23227101
##   15    1         triweight     0.5573333  0.27169604
##   15    1         cos           0.5551667  0.24096795
##   15    1         inv           0.5673333  0.24859224
##   15    1         rank          0.5601667  0.24402658
##   15    1         optimal       0.5493333  0.24373148
##   15    2         gaussian      0.5163333  0.14900327
##   15    2         triangular    0.5401667  0.22260902
##   15    2         rectangular   0.4688333  0.06732383
##   15    2         epanechnikov  0.5511667  0.23987446
##   15    2         biweight      0.4995000  0.19175436
##   15    2         triweight     0.5056667  0.20396222
##   15    2         cos           0.5506667  0.23700329
##   15    2         inv           0.4908333  0.14880413
##   15    2         rank          0.4633333  0.11817627
##   15    2         optimal       0.4895000  0.16560010
## 
## Kappa was used to select the optimal model using the largest value.
## The final values used for the model were kmax = 5, distance = 1 and kernel
##  = optimal.
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
##         B1  0  0  3  0
##         B2  0  0  0  7
## 
## Overall Statistics
##                                      
##                Accuracy : 1          
##                  95% CI : (0.7529, 1)
##     No Information Rate : 0.5385     
##     P-Value [Acc > NIR] : 0.0003199  
##                                      
##                   Kappa : 1          
##                                      
##  Mcnemar's Test P-Value : NA         
## 
## Statistics by Class:
## 
##                      Class: A1 Class: A2 Class: B1 Class: B2
## Sensitivity             1.0000   1.00000    1.0000    1.0000
## Specificity             1.0000   1.00000    1.0000    1.0000
## Pos Pred Value          1.0000   1.00000    1.0000    1.0000
## Neg Pred Value          1.0000   1.00000    1.0000    1.0000
## Prevalence              0.1538   0.07692    0.2308    0.5385
## Detection Rate          0.1538   0.07692    0.2308    0.5385
## Detection Prevalence    0.1538   0.07692    0.2308    0.5385
## Balanced Accuracy       1.0000   1.00000    1.0000    1.0000
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
##         B1  0  0  2  1
##         B2  0  0  1  6
## 
## Overall Statistics
##                                           
##                Accuracy : 0.8462          
##                  95% CI : (0.5455, 0.9808)
##     No Information Rate : 0.5385          
##     P-Value [Acc > NIR] : 0.02222         
##                                           
##                   Kappa : 0.7547          
##                                           
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A1 Class: A2 Class: B1 Class: B2
## Sensitivity             1.0000   1.00000    0.6667    0.8571
## Specificity             1.0000   1.00000    0.9000    0.8333
## Pos Pred Value          1.0000   1.00000    0.6667    0.8571
## Neg Pred Value          1.0000   1.00000    0.9000    0.8333
## Prevalence              0.1538   0.07692    0.2308    0.5385
## Detection Rate          0.1538   0.07692    0.1538    0.4615
## Detection Prevalence    0.1538   0.07692    0.2308    0.5385
## Balanced Accuracy       1.0000   1.00000    0.7833    0.8452
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
  labs(x = "", y = "",
       title = "Kappa 0.5 al quitar m y o y expandir area")+
  scale_x_continuous(breaks=c(-74.18, -74.17, -74.16))+
  scale_y_continuous(breaks=c(4.55,4.56,4.57))

p_modelos
```

<img src="C:\Users\cguio\DOCUME~1\Terrae\TCI_CE~1\_Git\Reportes\06_TCI~1/figure-html/plot_map-1.png" width="80%" style="display: block; margin: auto;" />

knnn predijo secuencias distribuídas sobre grandes áreas, mientras que rf predijo una distribución mas fina.

Iportancia...




