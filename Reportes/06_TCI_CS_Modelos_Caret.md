---
title: "TCI - Cerro Seco / Suelos"
subtitle: "Modelado II: caret"
author: "Carlos Guio"
date: "2021-07-24"
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

#Clip raster a poligono externo
topoind_clip <- mask(topoind, CSsf_18N)
NDVI_clip <- mask(NDVI, CSsf_18N)

covars <- stack(topoind_clip, NDVI_clip)

#Clip interno de poligonos mineros
covars_clip <- mask(covars, min2019sf_18N, inverse = TRUE)

# Crear df de variables
covars_df <- as.data.frame(covars_clip, xy= TRUE, na.rm = TRUE) 

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
          drop_na() %>%
          dplyr::mutate(secuencia = as.factor(secuencia))
```

La predicción sin ajustar devulve un kapp de 0.2 - 0.3. Factores que afectan son el porcentaje de la partición, el tipo de remuestreo (cv vs loocv), el desbalance de clases, la falta de representatividad de las covariables raster en el set de entrenamiento, la selección de variables. A continuación se mencionan los efectos:



Ensayé partición 0.7, 0.75 y 0.8. Al aumentar el porcentaje de observaciones en el set de entrenamiento ...



Los efectos del preprocesamiento fueron ...



El modelo con remuestreo loocv tardó mas en correr, pero mostró una mejora en el Kappa de 0.1 a 0.2 puntos, el mejor en random forest. Al predecir sobre el raster ambos modelos mostraron una mejora en la granularidad, es decir, la distribución de las secuencias adoptaba patrones relacionados a las covariables geográficas. 






```r
# train random forest model
set.seed(2)
caret_rf <- train(secuencia ~ NDVI + DEM + TWI,
                  data = caret_train,
                  method = "rf",
                  metric = "kappa",
                  trControl = caret_loocv,
                  ntree = 500,   #num.trees -ranger, ntree - rf
                  tuneGrid = tuneGrid_rf)

caret_rf
```

```
## Random Forest 
## 
## 43 samples
##  3 predictor
##  4 classes: 'A1', 'A2', 'B1', 'B2' 
## 
## No pre-processing
## Resampling: Leave-One-Out Cross-Validation 
## Summary of sample sizes: 42, 42, 42, 42, 42, 42, ... 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy   Kappa    
##   2     0.4883721  0.1200000
##   3     0.4883721  0.1477477
##   4     0.4651163  0.1240035
##   5     0.4651163  0.1090090
## 
## Accuracy was used to select the optimal model using the largest value.
## The final value used for the model was mtry = 2.
```

```r
#train knn model
set.seed(2)
caret_knn <- train(secuencia ~ NDVI + DEM + TWI,
                  data = caret_train,
                  method = "kknn",
                  metric = "kappa",
                  trControl = caret_loocv,
                  tuneGrid = tuneGrid_knn)

caret_knn
```

```
## k-Nearest Neighbors 
## 
## 43 samples
##  3 predictor
##  4 classes: 'A1', 'A2', 'B1', 'B2' 
## 
## No pre-processing
## Resampling: Leave-One-Out Cross-Validation 
## Summary of sample sizes: 42, 42, 42, 42, 42, 42, ... 
## Resampling results across tuning parameters:
## 
##   kmax  kernel        Accuracy   Kappa     
##    1    gaussian      0.5348837  0.26116838
##    1    triangular    0.5348837  0.26116838
##    1    epanechnikov  0.5348837  0.26116838
##    1    biweight      0.5348837  0.26116838
##    1    rank          0.5348837  0.26116838
##    1    optimal       0.5348837  0.26116838
##    2    gaussian      0.5348837  0.26116838
##    2    triangular    0.5348837  0.26116838
##    2    epanechnikov  0.5348837  0.26116838
##    2    biweight      0.5348837  0.26116838
##    2    rank          0.5348837  0.26116838
##    2    optimal       0.5348837  0.26116838
##    3    gaussian      0.4883721  0.16431095
##    3    triangular    0.5348837  0.26116838
##    3    epanechnikov  0.5348837  0.26116838
##    3    biweight      0.5348837  0.26116838
##    3    rank          0.5348837  0.26116838
##    3    optimal       0.5348837  0.26116838
##    4    gaussian      0.4883721  0.16431095
##    4    triangular    0.5348837  0.26116838
##    4    epanechnikov  0.5116279  0.23474576
##    4    biweight      0.5348837  0.26116838
##    4    rank          0.4883721  0.16431095
##    4    optimal       0.4883721  0.16431095
##    5    gaussian      0.4651163  0.09099265
##    5    triangular    0.5348837  0.26116838
##    5    epanechnikov  0.4883721  0.18588640
##    5    biweight      0.5348837  0.26116838
##    5    rank          0.4651163  0.11459266
##    5    optimal       0.4883721  0.16431095
##    6    gaussian      0.5581395  0.20833333
##    6    triangular    0.4883721  0.18728522
##    6    epanechnikov  0.4883721  0.14697926
##    6    biweight      0.5348837  0.25989673
##    6    rank          0.4418605  0.08672566
##    6    optimal       0.4651163  0.11220826
##    7    gaussian      0.5581395  0.20833333
##    7    triangular    0.4651163  0.10335449
##    7    epanechnikov  0.4883721  0.12811060
##    7    biweight      0.5116279  0.21204188
##    7    rank          0.4651163  0.06255924
##    7    optimal       0.4651163  0.10820559
##    8    gaussian      0.5581395  0.19348470
##    8    triangular    0.5581395  0.21892925
##    8    epanechnikov  0.5348837  0.19172932
##    8    biweight      0.4883721  0.18588640
##    8    rank          0.5348837  0.14171657
##    8    optimal       0.4651163  0.09266055
##    9    gaussian      0.6046512  0.25331971
##    9    triangular    0.5348837  0.16504854
##    9    epanechnikov  0.5348837  0.19172932
##    9    biweight      0.4883721  0.17235346
##    9    rank          0.6046512  0.24091381
##    9    optimal       0.4651163  0.09266055
##   10    gaussian      0.6046512  0.25331971
##   10    triangular    0.5348837  0.16504854
##   10    epanechnikov  0.5581395  0.22041985
##   10    biweight      0.4651163  0.12244898
##   10    rank          0.6046512  0.21313240
##   10    optimal       0.5348837  0.18173168
## 
## Tuning parameter 'distance' was held constant at a value of 1
## Accuracy was used to select the optimal model using the largest value.
## The final values used for the model were kmax = 10, distance = 1 and kernel
##  = gaussian.
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
##         A1  1  0  0  0
##         A2  1  1  0  1
##         B1  0  0  2  0
##         B2  0  0  0  6
## 
## Overall Statistics
##                                           
##                Accuracy : 0.8333          
##                  95% CI : (0.5159, 0.9791)
##     No Information Rate : 0.5833          
##     P-Value [Acc > NIR] : 0.06713         
##                                           
##                   Kappa : 0.7419          
##                                           
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A1 Class: A2 Class: B1 Class: B2
## Sensitivity            0.50000   1.00000    1.0000    0.8571
## Specificity            1.00000   0.81818    1.0000    1.0000
## Pos Pred Value         1.00000   0.33333    1.0000    1.0000
## Neg Pred Value         0.90909   1.00000    1.0000    0.8333
## Prevalence             0.16667   0.08333    0.1667    0.5833
## Detection Rate         0.08333   0.08333    0.1667    0.5000
## Detection Prevalence   0.08333   0.25000    0.1667    0.5000
## Balanced Accuracy      0.75000   0.90909    1.0000    0.9286
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
##         A1  1  0  0  0
##         A2  0  0  0  0
##         B1  0  1  2  0
##         B2  1  0  0  7
## 
## Overall Statistics
##                                           
##                Accuracy : 0.8333          
##                  95% CI : (0.5159, 0.9791)
##     No Information Rate : 0.5833          
##     P-Value [Acc > NIR] : 0.06713         
##                                           
##                   Kappa : 0.7             
##                                           
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A1 Class: A2 Class: B1 Class: B2
## Sensitivity            0.50000   0.00000    1.0000    1.0000
## Specificity            1.00000   1.00000    0.9000    0.8000
## Pos Pred Value         1.00000       NaN    0.6667    0.8750
## Neg Pred Value         0.90909   0.91667    1.0000    1.0000
## Prevalence             0.16667   0.08333    0.1667    0.5833
## Detection Rate         0.08333   0.00000    0.1667    0.5833
## Detection Prevalence   0.08333   0.00000    0.2500    0.6667
## Balanced Accuracy      0.75000   0.50000    0.9500    0.9000
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

<img src="C:\Users\cguio\Documents\Terrae\TCI_Cerro Seco\_Git\Reportes\06_TCI_CS_Modelos_Caret_files/figure-html/plot_map-1.png" width="80%" style="display: block; margin: auto;" />

knnn predijo secuencias distribuídas sobre grandes áreas, mientras que rf predijo una distribución mas fina.
