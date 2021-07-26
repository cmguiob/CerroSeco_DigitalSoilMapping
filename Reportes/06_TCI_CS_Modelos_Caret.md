---
title: "TCI - Cerro Seco / Suelos"
subtitle: "Modelado II: caret"
author: "Carlos Guio"
date: "2021-07-25"
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
# Renombrar
names(topoind) <- c("ASE", "ASN", "DEM","DSC", "FLA", "FLD", "LSF", "MPI",
                    "PLC", "PRC", "RSP", "SLP", "TPI", "TRI1", "TRI5", "TWI",
                    "USC", "VDN", "WSB") 
names(s2) <- c("B2", "B3", "B4", "B5", "B6", "B7", "B8", "B8A", "B11", "B12") 

# Crear NDVI
NDVI <- (s2[[7]] - s2[[3]]) / (s2[[7]] + s2[[3]])
names(NDVI) <- "NDVI"

#Poner rasters en mismo origen para alinear
topoind <- resample(topoind, NDVI) # This also puts them in same extent

covars <- stack(topoind, NDVI)

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

# Crear df de variables
covars_df <- as.data.frame(covars_clip, xy= TRUE, na.rm = TRUE) 
```

La predicción sin ajustar devulve un kapp de 0.2 - 0.3. Factores que afectan son el porcentaje de la partición, el tipo de remuestreo (cv vs loocv), el desbalance de clases, la falta de representatividad de las covariables raster en el set de entrenamiento, la selección de variables. A continuación se mencionan los efectos:



Ensayé partición 0.7, 0.75 y 0.8. Al aumentar el porcentaje de observaciones en el set de entrenamiento ...



Los efectos del preprocesamiento fueron ...



El modelo con remuestreo loocv tardó mas en correr, pero mostró una mejora en el Kappa de 0.1 a 0.2 puntos, el mejor en random forest. Al predecir sobre el raster ambos modelos mostraron una mejora en la granularidad, es decir, la distribución de las secuencias adoptaba patrones relacionados a las covariables geográficas. 







```r
# train random forest model
set.seed(1)
caret_rf <- train(secuencia ~ NDVI + DEM + TWI,
                  data = caret_train,
                  method = "rf",
                  metric = "Kappa",
                  #transformations are applied just on the variables in the formula
                  #preProcess = c("center"), 
                  trControl = caret_cv,
                  ntree = 5000,   #num.trees -ranger, ntree - rf
                  tuneGrid = tuneGrid_rf)

caret_rf
```

```
## Random Forest 
## 
## 47 samples
##  3 predictor
##  4 classes: 'A1', 'A2', 'B1', 'B2' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold, repeated 10 times) 
## Summary of sample sizes: 43, 41, 41, 43, 42, 42, ... 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy   Kappa    
##    1    0.6625000  0.4445089
##    2    0.6046667  0.3617523
##    3    0.5861667  0.3363725
##    4    0.5920000  0.3446755
##    5    0.5878333  0.3393725
##    6    0.5878333  0.3386452
##    7    0.5861667  0.3363725
##    8    0.5903333  0.3424028
##    9    0.5945000  0.3502311
##   10    0.5903333  0.3416755
## 
## Kappa was used to select the optimal model using the largest value.
## The final value used for the model was mtry = 1.
```

```r
#train knn model
set.seed(1)
caret_knn <- train(secuencia ~ NDVI + DEM + TWI,
                  data = caret_train,
                  method = "kknn",
                  metric = "Kappa",
                  #preProcess = c("center"), 
                  trControl = caret_cv,
                  tuneGrid = tuneGrid_knn)

caret_knn
```

```
## k-Nearest Neighbors 
## 
## 47 samples
##  3 predictor
##  4 classes: 'A1', 'A2', 'B1', 'B2' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold, repeated 10 times) 
## Summary of sample sizes: 43, 41, 41, 43, 42, 42, ... 
## Resampling results across tuning parameters:
## 
##   kmax  distance  kernel        Accuracy   Kappa    
##    2    1         gaussian      0.6966667  0.5280272
##    2    1         triangular    0.6966667  0.5280272
##    2    1         rectangular   0.6966667  0.5280272
##    2    1         epanechnikov  0.6966667  0.5280272
##    2    1         biweight      0.6966667  0.5280272
##    2    1         triweight     0.6966667  0.5280272
##    2    1         cos           0.6966667  0.5280272
##    2    1         inv           0.6966667  0.5280272
##    2    1         rank          0.6966667  0.5280272
##    2    1         optimal       0.6966667  0.5280272
##    2    2         gaussian      0.6998333  0.5351568
##    2    2         triangular    0.6998333  0.5351568
##    2    2         rectangular   0.6998333  0.5351568
##    2    2         epanechnikov  0.6998333  0.5351568
##    2    2         biweight      0.6998333  0.5351568
##    2    2         triweight     0.6998333  0.5351568
##    2    2         cos           0.6998333  0.5351568
##    2    2         inv           0.6998333  0.5351568
##    2    2         rank          0.6998333  0.5351568
##    2    2         optimal       0.6998333  0.5351568
##    3    1         gaussian      0.6760000  0.4867671
##    3    1         triangular    0.6773333  0.4996784
##    3    1         rectangular   0.6966667  0.5280272
##    3    1         epanechnikov  0.6806667  0.5046127
##    3    1         biweight      0.6946667  0.5252494
##    3    1         triweight     0.6966667  0.5280272
##    3    1         cos           0.6781667  0.5007239
##    3    1         inv           0.6736667  0.4932312
##    3    1         rank          0.6966667  0.5280272
##    3    1         optimal       0.6966667  0.5280272
##    3    2         gaussian      0.6795000  0.4888842
##    3    2         triangular    0.7043333  0.5415709
##    3    2         rectangular   0.6998333  0.5351568
##    3    2         epanechnikov  0.6941667  0.5243204
##    3    2         biweight      0.7043333  0.5415709
##    3    2         triweight     0.6998333  0.5351568
##    3    2         cos           0.7006667  0.5345196
##    3    2         inv           0.6935000  0.5212588
##    3    2         rank          0.6998333  0.5351568
##    3    2         optimal       0.6998333  0.5351568
##    4    1         gaussian      0.6710000  0.4781657
##    4    1         triangular    0.6773333  0.4996784
##    4    1         rectangular   0.6966667  0.5280272
##    4    1         epanechnikov  0.6786667  0.5011201
##    4    1         biweight      0.6946667  0.5252494
##    4    1         triweight     0.6966667  0.5280272
##    4    1         cos           0.6761667  0.4972312
##    4    1         inv           0.6640000  0.4740905
##    4    1         rank          0.6906667  0.5179373
##    4    1         optimal       0.6740000  0.4810459
##    4    2         gaussian      0.6688333  0.4717720
##    4    2         triangular    0.7023333  0.5384663
##    4    2         rectangular   0.6998333  0.5351568
##    4    2         epanechnikov  0.6848333  0.5106892
##    4    2         biweight      0.7043333  0.5415709
##    4    2         triweight     0.6998333  0.5351568
##    4    2         cos           0.6893333  0.5178182
##    4    2         inv           0.6865000  0.5047049
##    4    2         rank          0.6795000  0.4888842
##    4    2         optimal       0.6653333  0.4607433
##    5    1         gaussian      0.6710000  0.4781657
##    5    1         triangular    0.6761667  0.4960922
##    5    1         rectangular   0.6893333  0.5152891
##    5    1         epanechnikov  0.6736667  0.4925187
##    5    1         biweight      0.6946667  0.5252494
##    5    1         triweight     0.6986667  0.5311522
##    5    1         cos           0.6703333  0.4820389
##    5    1         inv           0.6471667  0.4280245
##    5    1         rank          0.6783333  0.4976083
##    5    1         optimal       0.6740000  0.4810459
##    5    2         gaussian      0.6608333  0.4601879
##    5    2         triangular    0.7093333  0.5424559
##    5    2         rectangular   0.6998333  0.5351568
##    5    2         epanechnikov  0.6768333  0.4969670
##    5    2         biweight      0.7003333  0.5343341
##    5    2         triweight     0.7003333  0.5356886
##    5    2         cos           0.6821667  0.5046263
##    5    2         inv           0.6885000  0.5042813
##    5    2         rank          0.6755000  0.4828384
##    5    2         optimal       0.6653333  0.4607433
##    6    1         gaussian      0.6573333  0.4554419
##    6    1         triangular    0.6728333  0.4904103
##    6    1         rectangular   0.6763333  0.4944815
##    6    1         epanechnikov  0.6736667  0.4925187
##    6    1         biweight      0.6946667  0.5251032
##    6    1         triweight     0.6966667  0.5283744
##    6    1         cos           0.6703333  0.4820389
##    6    1         inv           0.6698333  0.4499625
##    6    1         rank          0.6763333  0.4943576
##    6    1         optimal       0.6666667  0.4693183
##    6    2         gaussian      0.6568333  0.4534445
##    6    2         triangular    0.7093333  0.5424559
##    6    2         rectangular   0.6998333  0.5351568
##    6    2         epanechnikov  0.6768333  0.4969670
##    6    2         biweight      0.6970000  0.5295529
##    6    2         triweight     0.7013333  0.5357726
##    6    2         cos           0.6821667  0.5046263
##    6    2         inv           0.6868333  0.5010870
##    6    2         rank          0.6755000  0.4828384
##    6    2         optimal       0.6620000  0.4548342
##    7    1         gaussian      0.6573333  0.4554419
##    7    1         triangular    0.6728333  0.4904103
##    7    1         rectangular   0.6763333  0.4944815
##    7    1         epanechnikov  0.6716667  0.4892679
##    7    1         biweight      0.6930000  0.5221568
##    7    1         triweight     0.6951667  0.5264552
##    7    1         cos           0.6683333  0.4787881
##    7    1         inv           0.6740000  0.4531078
##    7    1         rank          0.6723333  0.4884752
##    7    1         optimal       0.6606667  0.4601852
##    7    2         gaussian      0.6568333  0.4534445
##    7    2         triangular    0.7093333  0.5424559
##    7    2         rectangular   0.6998333  0.5351568
##    7    2         epanechnikov  0.6768333  0.4969670
##    7    2         biweight      0.6893333  0.5176359
##    7    2         triweight     0.6998333  0.5329550
##    7    2         cos           0.6821667  0.5046263
##    7    2         inv           0.6828333  0.4939964
##    7    2         rank          0.6615000  0.4617331
##    7    2         optimal       0.6620000  0.4548342
##    8    1         gaussian      0.6573333  0.4550782
##    8    1         triangular    0.6728333  0.4904103
##    8    1         rectangular   0.6643333  0.4714881
##    8    1         epanechnikov  0.6663333  0.4810995
##    8    1         biweight      0.6930000  0.5221568
##    8    1         triweight     0.6951667  0.5263090
##    8    1         cos           0.6646667  0.4731197
##    8    1         inv           0.6706667  0.4407536
##    8    1         rank          0.6613333  0.4709262
##    8    1         optimal       0.6606667  0.4601852
##    8    2         gaussian      0.6568333  0.4534445
##    8    2         triangular    0.7093333  0.5424559
##    8    2         rectangular   0.6998333  0.5351568
##    8    2         epanechnikov  0.6768333  0.4969670
##    8    2         biweight      0.6860000  0.5117268
##    8    2         triweight     0.6961667  0.5270178
##    8    2         cos           0.6821667  0.5046263
##    8    2         inv           0.6788333  0.4869376
##    8    2         rank          0.6615000  0.4617331
##    8    2         optimal       0.6480000  0.4330753
##    9    1         gaussian      0.6500000  0.4395516
##    9    1         triangular    0.6651667  0.4775868
##    9    1         rectangular   0.6643333  0.4714881
##    9    1         epanechnikov  0.6546667  0.4601264
##    9    1         biweight      0.6930000  0.5221568
##    9    1         triweight     0.6935000  0.5238090
##    9    1         cos           0.6530000  0.4521466
##    9    1         inv           0.6723333  0.4406627
##    9    1         rank          0.6613333  0.4709262
##    9    1         optimal       0.6536667  0.4483551
##    9    2         gaussian      0.6568333  0.4534445
##    9    2         triangular    0.7093333  0.5424559
##    9    2         rectangular   0.6998333  0.5351568
##    9    2         epanechnikov  0.6728333  0.4902235
##    9    2         biweight      0.6860000  0.5117268
##    9    2         triweight     0.6905000  0.5169236
##    9    2         cos           0.6821667  0.5046263
##    9    2         inv           0.6711667  0.4699364
##    9    2         rank          0.6615000  0.4617331
##    9    2         optimal       0.6480000  0.4330753
##   10    1         gaussian      0.6500000  0.4395516
##   10    1         triangular    0.6555000  0.4592453
##   10    1         rectangular   0.6643333  0.4714881
##   10    1         epanechnikov  0.6563333  0.4616719
##   10    1         biweight      0.6806667  0.5019912
##   10    1         triweight     0.6901667  0.5181272
##   10    1         cos           0.6513333  0.4481466
##   10    1         inv           0.6723333  0.4406627
##   10    1         rank          0.6613333  0.4709262
##   10    1         optimal       0.6500000  0.4391468
##   10    2         gaussian      0.6568333  0.4534445
##   10    2         triangular    0.7053333  0.5357125
##   10    2         rectangular   0.6998333  0.5351568
##   10    2         epanechnikov  0.6728333  0.4902235
##   10    2         biweight      0.6860000  0.5117268
##   10    2         triweight     0.6868333  0.5117920
##   10    2         cos           0.6781667  0.4978828
##   10    2         inv           0.6695000  0.4659364
##   10    2         rank          0.6615000  0.4617331
##   10    2         optimal       0.6480000  0.4330753
##   11    1         gaussian      0.6500000  0.4395516
##   11    1         triangular    0.6518333  0.4534946
##   11    1         rectangular   0.6643333  0.4714881
##   11    1         epanechnikov  0.6543333  0.4584211
##   11    1         biweight      0.6730000  0.4895117
##   11    1         triweight     0.6811667  0.5038679
##   11    1         cos           0.6473333  0.4407292
##   11    1         inv           0.6723333  0.4406627
##   11    1         rank          0.6613333  0.4709262
##   11    1         optimal       0.6500000  0.4391468
##   11    2         gaussian      0.6568333  0.4534445
##   11    2         triangular    0.7053333  0.5357125
##   11    2         rectangular   0.6998333  0.5351568
##   11    2         epanechnikov  0.6728333  0.4902235
##   11    2         biweight      0.6860000  0.5117268
##   11    2         triweight     0.6868333  0.5117920
##   11    2         cos           0.6781667  0.4978828
##   11    2         inv           0.6645000  0.4550476
##   11    2         rank          0.6615000  0.4617331
##   11    2         optimal       0.6480000  0.4330753
##   12    1         gaussian      0.6500000  0.4395516
##   12    1         triangular    0.6518333  0.4534946
##   12    1         rectangular   0.6643333  0.4714881
##   12    1         epanechnikov  0.6523333  0.4515461
##   12    1         biweight      0.6710000  0.4847498
##   12    1         triweight     0.6751667  0.4951906
##   12    1         cos           0.6453333  0.4372366
##   12    1         inv           0.6723333  0.4406627
##   12    1         rank          0.6613333  0.4709262
##   12    1         optimal       0.6500000  0.4391468
##   12    2         gaussian      0.6568333  0.4534445
##   12    2         triangular    0.7053333  0.5357125
##   12    2         rectangular   0.6998333  0.5351568
##   12    2         epanechnikov  0.6728333  0.4902235
##   12    2         biweight      0.6820000  0.5058445
##   12    2         triweight     0.6868333  0.5117920
##   12    2         cos           0.6781667  0.4978828
##   12    2         inv           0.6645000  0.4550476
##   12    2         rank          0.6615000  0.4617331
##   12    2         optimal       0.6480000  0.4330753
##   13    1         gaussian      0.6500000  0.4395516
##   13    1         triangular    0.6501667  0.4503477
##   13    1         rectangular   0.6643333  0.4714881
##   13    1         epanechnikov  0.6523333  0.4515461
##   13    1         biweight      0.6710000  0.4847498
##   13    1         triweight     0.6731667  0.4924128
##   13    1         cos           0.6420000  0.4295026
##   13    1         inv           0.6723333  0.4406627
##   13    1         rank          0.6613333  0.4709262
##   13    1         optimal       0.6500000  0.4391468
##   13    2         gaussian      0.6568333  0.4534445
##   13    2         triangular    0.7053333  0.5357125
##   13    2         rectangular   0.6998333  0.5351568
##   13    2         epanechnikov  0.6728333  0.4902235
##   13    2         biweight      0.6820000  0.5058445
##   13    2         triweight     0.6868333  0.5117920
##   13    2         cos           0.6761667  0.4937710
##   13    2         inv           0.6645000  0.4550476
##   13    2         rank          0.6615000  0.4617331
##   13    2         optimal       0.6480000  0.4330753
##   14    1         gaussian      0.6500000  0.4395516
##   14    1         triangular    0.6481667  0.4468551
##   14    1         rectangular   0.6643333  0.4714881
##   14    1         epanechnikov  0.6523333  0.4515461
##   14    1         biweight      0.6670000  0.4787040
##   14    1         triweight     0.6731667  0.4924128
##   14    1         cos           0.6420000  0.4295026
##   14    1         inv           0.6723333  0.4406627
##   14    1         rank          0.6613333  0.4709262
##   14    1         optimal       0.6500000  0.4391468
##   14    2         gaussian      0.6568333  0.4534445
##   14    2         triangular    0.7053333  0.5357125
##   14    2         rectangular   0.6998333  0.5351568
##   14    2         epanechnikov  0.6728333  0.4902235
##   14    2         biweight      0.6800000  0.5025937
##   14    2         triweight     0.6868333  0.5117920
##   14    2         cos           0.6761667  0.4937710
##   14    2         inv           0.6645000  0.4550476
##   14    2         rank          0.6615000  0.4617331
##   14    2         optimal       0.6480000  0.4330753
##   15    1         gaussian      0.6500000  0.4395516
##   15    1         triangular    0.6481667  0.4458134
##   15    1         rectangular   0.6643333  0.4714881
##   15    1         epanechnikov  0.6523333  0.4515461
##   15    1         biweight      0.6670000  0.4787040
##   15    1         triweight     0.6731667  0.4924128
##   15    1         cos           0.6420000  0.4295026
##   15    1         inv           0.6723333  0.4406627
##   15    1         rank          0.6613333  0.4709262
##   15    1         optimal       0.6500000  0.4391468
##   15    2         gaussian      0.6568333  0.4534445
##   15    2         triangular    0.7020000  0.5290202
##   15    2         rectangular   0.6998333  0.5351568
##   15    2         epanechnikov  0.6728333  0.4902235
##   15    2         biweight      0.6800000  0.5025937
##   15    2         triweight     0.6868333  0.5117920
##   15    2         cos           0.6761667  0.4937710
##   15    2         inv           0.6645000  0.4550476
##   15    2         rank          0.6615000  0.4617331
##   15    2         optimal       0.6480000  0.4330753
## 
## Kappa was used to select the optimal model using the largest value.
## The final values used for the model were kmax = 9, distance = 2 and kernel
##  = triangular.
```

Sensibilidad ...  


```r
caret_rf_result <- predict(caret_rf, newdata = caret_test)
confusionMatrix(caret_rf_result, caret_test$secuencia)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction A1 A2 B1 B2
##         A1  1  0  0  1
##         A2  0  0  0  1
##         B1  0  1  2  0
##         B2  1  0  1  5
## 
## Overall Statistics
##                                           
##                Accuracy : 0.6154          
##                  95% CI : (0.3158, 0.8614)
##     No Information Rate : 0.5385          
##     P-Value [Acc > NIR] : 0.3938          
##                                           
##                   Kappa : 0.3868          
##                                           
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A1 Class: A2 Class: B1 Class: B2
## Sensitivity            0.50000   0.00000    0.6667    0.7143
## Specificity            0.90909   0.91667    0.9000    0.6667
## Pos Pred Value         0.50000   0.00000    0.6667    0.7143
## Neg Pred Value         0.90909   0.91667    0.9000    0.6667
## Prevalence             0.15385   0.07692    0.2308    0.5385
## Detection Rate         0.07692   0.00000    0.1538    0.3846
## Detection Prevalence   0.15385   0.07692    0.2308    0.5385
## Balanced Accuracy      0.70455   0.45833    0.7833    0.6905
```

```r
caret_knn_result <- predict(caret_knn, newdata = caret_test)
confusionMatrix(caret_knn_result, caret_test$secuencia)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction A1 A2 B1 B2
##         A1  0  0  0  0
##         A2  1  0  0  1
##         B1  0  0  2  2
##         B2  1  1  1  4
## 
## Overall Statistics
##                                           
##                Accuracy : 0.4615          
##                  95% CI : (0.1922, 0.7487)
##     No Information Rate : 0.5385          
##     P-Value [Acc > NIR] : 0.7981          
##                                           
##                   Kappa : 0.1415          
##                                           
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A1 Class: A2 Class: B1 Class: B2
## Sensitivity             0.0000   0.00000    0.6667    0.5714
## Specificity             1.0000   0.83333    0.8000    0.5000
## Pos Pred Value             NaN   0.00000    0.5000    0.5714
## Neg Pred Value          0.8462   0.90909    0.8889    0.5000
## Prevalence              0.1538   0.07692    0.2308    0.5385
## Detection Rate          0.0000   0.00000    0.1538    0.3077
## Detection Prevalence    0.0000   0.15385    0.3077    0.5385
## Balanced Accuracy       0.5000   0.41667    0.7333    0.5357
```

La matriz de confusión ...



knn tiene distribución discreta de valores de probabilidades para las diferentes secuencias, mientras que la distribución de valores para rf es masomenos continua. En el primer caso, las probabilidades están limitadas por la relación entre las secuencias y el número de vecinos k. En el segundo caso ... 






```r
p_modelos <- ggplot() + 
  geom_raster(data = predicted_raster, 
              aes(x=long, y=lat, 
                  fill = secuencia ,
                  alpha = prob))+
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
  geom_sf(data = CSsf_18N, fill = NA)+
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
