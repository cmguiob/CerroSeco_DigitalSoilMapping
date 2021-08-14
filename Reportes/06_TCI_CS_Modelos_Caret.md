---
title: "TCI - Cerro Seco / Suelos"
subtitle: "Modelado II: caret"
author: "Carlos Guio"
date: "2021-08-12"
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
                  #importance can be selected in ranger. In rf is default impurity
                  #importance = "permutation", 
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
## 24 predictors
##  4 classes: 'A1', 'A2', 'B1', 'B2' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold, repeated 10 times) 
## Summary of sample sizes: 43, 43, 43, 42, 42, 43, ... 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy   Kappa    
##    1    0.8328333  0.7143123
##    2    0.8978333  0.8268423
##    3    0.9321667  0.8888119
##    4    0.9548333  0.9295926
##    5    0.9606667  0.9373367
##    6    0.9606667  0.9373367
##    7    0.9606667  0.9373367
##    8    0.9606667  0.9373367
##    9    0.9606667  0.9373367
##   10    0.9606667  0.9373367
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
                  tuneGrid = tuneGrid_kknn)

caret_knn
```

```
## k-Nearest Neighbors 
## 
## 48 samples
## 24 predictors
##  4 classes: 'A1', 'A2', 'B1', 'B2' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold, repeated 10 times) 
## Summary of sample sizes: 43, 43, 43, 42, 42, 43, ... 
## Resampling results across tuning parameters:
## 
##   kmax  distance  kernel        Accuracy   Kappa    
##    2    1         gaussian      0.6765000  0.5140682
##    2    1         triangular    0.6765000  0.5140682
##    2    1         rectangular   0.6765000  0.5140682
##    2    1         epanechnikov  0.6765000  0.5140682
##    2    1         biweight      0.6765000  0.5140682
##    2    1         triweight     0.6765000  0.5140682
##    2    1         cos           0.6765000  0.5140682
##    2    1         inv           0.6765000  0.5140682
##    2    1         rank          0.6765000  0.5140682
##    2    1         optimal       0.6765000  0.5140682
##    2    2         gaussian      0.6806667  0.5030052
##    2    2         triangular    0.6806667  0.5030052
##    2    2         rectangular   0.6806667  0.5030052
##    2    2         epanechnikov  0.6806667  0.5030052
##    2    2         biweight      0.6806667  0.5030052
##    2    2         triweight     0.6806667  0.5030052
##    2    2         cos           0.6806667  0.5030052
##    2    2         inv           0.6806667  0.5030052
##    2    2         rank          0.6806667  0.5030052
##    2    2         optimal       0.6806667  0.5030052
##    3    1         gaussian      0.7451667  0.5954908
##    3    1         triangular    0.7110000  0.5601087
##    3    1         rectangular   0.7030000  0.5451385
##    3    1         epanechnikov  0.7236667  0.5776258
##    3    1         biweight      0.6798333  0.5176396
##    3    1         triweight     0.6765000  0.5140682
##    3    1         cos           0.7236667  0.5776258
##    3    1         inv           0.7213333  0.5640674
##    3    1         rank          0.6700000  0.5048763
##    3    1         optimal       0.6765000  0.5140682
##    3    2         gaussian      0.7163333  0.5459735
##    3    2         triangular    0.7023333  0.5390426
##    3    2         rectangular   0.6846667  0.5066449
##    3    2         epanechnikov  0.7020000  0.5378305
##    3    2         biweight      0.6873333  0.5111221
##    3    2         triweight     0.6823333  0.5055052
##    3    2         cos           0.7023333  0.5390426
##    3    2         inv           0.7200000  0.5516078
##    3    2         rank          0.6846667  0.5094186
##    3    2         optimal       0.6806667  0.5030052
##    4    1         gaussian      0.7451667  0.5954908
##    4    1         triangular    0.7126667  0.5572031
##    4    1         rectangular   0.6950000  0.5298512
##    4    1         epanechnikov  0.7140000  0.5582730
##    4    1         biweight      0.6818333  0.5201396
##    4    1         triweight     0.6765000  0.5140682
##    4    1         cos           0.7106667  0.5543606
##    4    1         inv           0.7228333  0.5636970
##    4    1         rank          0.7193333  0.5603007
##    4    1         optimal       0.6765000  0.5140682
##    4    2         gaussian      0.7163333  0.5459735
##    4    2         triangular    0.7093333  0.5485328
##    4    2         rectangular   0.6846667  0.5066449
##    4    2         epanechnikov  0.7023333  0.5376797
##    4    2         biweight      0.6983333  0.5253100
##    4    2         triweight     0.6966667  0.5241224
##    4    2         cos           0.7043333  0.5411995
##    4    2         inv           0.7183333  0.5488805
##    4    2         rank          0.6663333  0.4774253
##    4    2         optimal       0.6806667  0.5030052
##    5    1         gaussian      0.7365000  0.5786259
##    5    1         triangular    0.7163333  0.5614192
##    5    1         rectangular   0.6941667  0.5274537
##    5    1         epanechnikov  0.7233333  0.5702183
##    5    1         biweight      0.6803333  0.5180636
##    5    1         triweight     0.6765000  0.5140682
##    5    1         cos           0.7180000  0.5635621
##    5    1         inv           0.7575000  0.6106815
##    5    1         rank          0.7216667  0.5615216
##    5    1         optimal       0.6765000  0.5140682
##    5    2         gaussian      0.7063333  0.5303689
##    5    2         triangular    0.7051667  0.5414003
##    5    2         rectangular   0.6741667  0.4894927
##    5    2         epanechnikov  0.6826667  0.5054769
##    5    2         biweight      0.6983333  0.5253100
##    5    2         triweight     0.6966667  0.5241224
##    5    2         cos           0.6886667  0.5152672
##    5    2         inv           0.7063333  0.5277124
##    5    2         rank          0.6491667  0.4478476
##    5    2         optimal       0.6806667  0.5030052
##    6    1         gaussian      0.7430000  0.5864565
##    6    1         triangular    0.7370000  0.5865174
##    6    1         rectangular   0.6995000  0.5346126
##    6    1         epanechnikov  0.7423333  0.5939736
##    6    1         biweight      0.6860000  0.5242862
##    6    1         triweight     0.6801667  0.5193171
##    6    1         cos           0.7473333  0.6016877
##    6    1         inv           0.7565000  0.6065835
##    6    1         rank          0.7213333  0.5604746
##    6    1         optimal       0.7355000  0.5866972
##    6    2         gaussian      0.6943333  0.5106015
##    6    2         triangular    0.7051667  0.5417475
##    6    2         rectangular   0.6716667  0.4845998
##    6    2         epanechnikov  0.6765000  0.4941322
##    6    2         biweight      0.6983333  0.5256368
##    6    2         triweight     0.6966667  0.5241224
##    6    2         cos           0.6870000  0.5131144
##    6    2         inv           0.6978333  0.5120611
##    6    2         rank          0.6471667  0.4447226
##    6    2         optimal       0.6615000  0.4671871
##    7    1         gaussian      0.7493333  0.5930216
##    7    1         triangular    0.7651667  0.6229963
##    7    1         rectangular   0.6975000  0.5302586
##    7    1         epanechnikov  0.7590000  0.6126939
##    7    1         biweight      0.6873333  0.5221172
##    7    1         triweight     0.6908333  0.5346454
##    7    1         cos           0.7618333  0.6202441
##    7    1         inv           0.7598333  0.6102848
##    7    1         rank          0.7193333  0.5571412
##    7    1         optimal       0.7355000  0.5866972
##    7    2         gaussian      0.6878333  0.4969507
##    7    2         triangular    0.7051667  0.5417475
##    7    2         rectangular   0.6651667  0.4722665
##    7    2         epanechnikov  0.6790000  0.4981322
##    7    2         biweight      0.7003333  0.5281368
##    7    2         triweight     0.6966667  0.5241224
##    7    2         cos           0.6870000  0.5131144
##    7    2         inv           0.6961667  0.5091186
##    7    2         rank          0.6471667  0.4447226
##    7    2         optimal       0.6653333  0.4706668
##    8    1         gaussian      0.7493333  0.5930216
##    8    1         triangular    0.7668333  0.6250417
##    8    1         rectangular   0.6975000  0.5255919
##    8    1         epanechnikov  0.7590000  0.6126939
##    8    1         biweight      0.7041667  0.5432722
##    8    1         triweight     0.6941667  0.5401908
##    8    1         cos           0.7598333  0.6176125
##    8    1         inv           0.7598333  0.6102848
##    8    1         rank          0.7218333  0.5588403
##    8    1         optimal       0.7383333  0.5899174
##    8    2         gaussian      0.6915000  0.5013591
##    8    2         triangular    0.7051667  0.5417475
##    8    2         rectangular   0.6606667  0.4605046
##    8    2         epanechnikov  0.6806667  0.5006322
##    8    2         biweight      0.7028333  0.5351209
##    8    2         triweight     0.6966667  0.5241224
##    8    2         cos           0.6920000  0.5207508
##    8    2         inv           0.6936667  0.5046742
##    8    2         rank          0.6471667  0.4447226
##    8    2         optimal       0.6518333  0.4497924
##    9    1         gaussian      0.7493333  0.5930216
##    9    1         triangular    0.7668333  0.6248955
##    9    1         rectangular   0.6975000  0.5255919
##    9    1         epanechnikov  0.7590000  0.6111990
##    9    1         biweight      0.7011667  0.5390289
##    9    1         triweight     0.7085000  0.5588794
##    9    1         cos           0.7598333  0.6176125
##    9    1         inv           0.7598333  0.6102848
##    9    1         rank          0.7218333  0.5582451
##    9    1         optimal       0.7383333  0.5899174
##    9    2         gaussian      0.6951667  0.5027819
##    9    2         triangular    0.7106667  0.5482583
##    9    2         rectangular   0.6533333  0.4483559
##    9    2         epanechnikov  0.6786667  0.4975277
##    9    2         biweight      0.7068333  0.5418815
##    9    2         triweight     0.7006667  0.5299312
##    9    2         cos           0.6855000  0.5079730
##    9    2         inv           0.6916667  0.4988408
##    9    2         rank          0.6471667  0.4442782
##    9    2         optimal       0.6688333  0.4774501
##   10    1         gaussian      0.7493333  0.5930216
##   10    1         triangular    0.7626667  0.6180420
##   10    1         rectangular   0.6975000  0.5255919
##   10    1         epanechnikov  0.7575000  0.6044926
##   10    1         biweight      0.7036667  0.5420350
##   10    1         triweight     0.7110000  0.5597711
##   10    1         cos           0.7553333  0.6076681
##   10    1         inv           0.7598333  0.6102848
##   10    1         rank          0.7238333  0.5607305
##   10    1         optimal       0.7383333  0.5893114
##   10    2         gaussian      0.6920000  0.4982917
##   10    2         triangular    0.7106667  0.5482583
##   10    2         rectangular   0.6533333  0.4479114
##   10    2         epanechnikov  0.6786667  0.4975277
##   10    2         biweight      0.7088333  0.5456315
##   10    2         triweight     0.7046667  0.5363037
##   10    2         cos           0.6855000  0.5079730
##   10    2         inv           0.6916667  0.4988408
##   10    2         rank          0.6471667  0.4438834
##   10    2         optimal       0.6628333  0.4657438
##   11    1         gaussian      0.7493333  0.5930216
##   11    1         triangular    0.7581667  0.6098753
##   11    1         rectangular   0.6975000  0.5255919
##   11    1         epanechnikov  0.7575000  0.6044926
##   11    1         biweight      0.7016667  0.5384636
##   11    1         triweight     0.7171667  0.5664873
##   11    1         cos           0.7595000  0.6106205
##   11    1         inv           0.7573333  0.6039212
##   11    1         rank          0.7255000  0.5630381
##   11    1         optimal       0.7408333  0.5913543
##   11    2         gaussian      0.6895000  0.4942917
##   11    2         triangular    0.7106667  0.5482583
##   11    2         rectangular   0.6500000  0.4422296
##   11    2         epanechnikov  0.6765000  0.4905424
##   11    2         biweight      0.7138333  0.5536315
##   11    2         triweight     0.7066667  0.5397964
##   11    2         cos           0.6835000  0.5048684
##   11    2         inv           0.6916667  0.4980328
##   11    2         rank          0.6471667  0.4438834
##   11    2         optimal       0.6628333  0.4657438
##   12    1         gaussian      0.7493333  0.5930216
##   12    1         triangular    0.7655000  0.6165322
##   12    1         rectangular   0.6975000  0.5255919
##   12    1         epanechnikov  0.7595000  0.6069780
##   12    1         biweight      0.7053333  0.5424141
##   12    1         triweight     0.7208333  0.5712171
##   12    1         cos           0.7595000  0.6106205
##   12    1         inv           0.7573333  0.6039212
##   12    1         rank          0.7255000  0.5630381
##   12    1         optimal       0.7441667  0.5957774
##   12    2         gaussian      0.6870000  0.4842917
##   12    2         triangular    0.7106667  0.5482583
##   12    2         rectangular   0.6500000  0.4422296
##   12    2         epanechnikov  0.6765000  0.4897343
##   12    2         biweight      0.7138333  0.5536315
##   12    2         triweight     0.7066667  0.5397964
##   12    2         cos           0.6815000  0.5017638
##   12    2         inv           0.6916667  0.4980328
##   12    2         rank          0.6426667  0.4313478
##   12    2         optimal       0.6608333  0.4615771
##   13    1         gaussian      0.7493333  0.5930216
##   13    1         triangular    0.7680000  0.6215322
##   13    1         rectangular   0.6975000  0.5255919
##   13    1         epanechnikov  0.7595000  0.6069780
##   13    1         biweight      0.7155000  0.5542293
##   13    1         triweight     0.7228333  0.5738314
##   13    1         cos           0.7595000  0.6106205
##   13    1         inv           0.7573333  0.6039212
##   13    1         rank          0.7255000  0.5630381
##   13    1         optimal       0.7441667  0.5957774
##   13    2         gaussian      0.6870000  0.4842917
##   13    2         triangular    0.7021667  0.5323998
##   13    2         rectangular   0.6500000  0.4422296
##   13    2         epanechnikov  0.6745000  0.4849724
##   13    2         biweight      0.7138333  0.5536315
##   13    2         triweight     0.7071667  0.5426138
##   13    2         cos           0.6790000  0.4968750
##   13    2         inv           0.6916667  0.4980328
##   13    2         rank          0.6426667  0.4313478
##   13    2         optimal       0.6608333  0.4615771
##   14    1         gaussian      0.7493333  0.5930216
##   14    1         triangular    0.7680000  0.6215322
##   14    1         rectangular   0.6975000  0.5255919
##   14    1         epanechnikov  0.7628333  0.6113661
##   14    1         biweight      0.7195000  0.5589515
##   14    1         triweight     0.7211667  0.5715237
##   14    1         cos           0.7611667  0.6128932
##   14    1         inv           0.7573333  0.6039212
##   14    1         rank          0.7255000  0.5630381
##   14    1         optimal       0.7466667  0.5990501
##   14    2         gaussian      0.6870000  0.4842917
##   14    2         triangular    0.6946667  0.5195109
##   14    2         rectangular   0.6500000  0.4422296
##   14    2         epanechnikov  0.6680000  0.4708772
##   14    2         biweight      0.7188333  0.5616315
##   14    2         triweight     0.7071667  0.5426138
##   14    2         cos           0.6770000  0.4932148
##   14    2         inv           0.6916667  0.4980328
##   14    2         rank          0.6426667  0.4313478
##   14    2         optimal       0.6571667  0.4557683
##   15    1         gaussian      0.7493333  0.5930216
##   15    1         triangular    0.7696667  0.6238049
##   15    1         rectangular   0.6975000  0.5255919
##   15    1         epanechnikov  0.7628333  0.6113661
##   15    1         biweight      0.7265000  0.5675370
##   15    1         triweight     0.7098333  0.5541682
##   15    1         cos           0.7611667  0.6128932
##   15    1         inv           0.7573333  0.6039212
##   15    1         rank          0.7255000  0.5630381
##   15    1         optimal       0.7466667  0.5990501
##   15    2         gaussian      0.6870000  0.4842917
##   15    2         triangular    0.6946667  0.5187028
##   15    2         rectangular   0.6500000  0.4422296
##   15    2         epanechnikov  0.6680000  0.4708772
##   15    2         biweight      0.7188333  0.5616315
##   15    2         triweight     0.7071667  0.5426138
##   15    2         cos           0.6730000  0.4836910
##   15    2         inv           0.6916667  0.4980328
##   15    2         rank          0.6426667  0.4313478
##   15    2         optimal       0.6571667  0.4557683
## 
## Kappa was used to select the optimal model using the largest value.
## The final values used for the model were kmax = 8, distance = 1 and kernel
##  = triangular.
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
##         A1  2  1  0  0
##         A2  0  0  0  0
##         B1  0  0  1  0
##         B2  0  1  1  8
## 
## Overall Statistics
##                                          
##                Accuracy : 0.7857         
##                  95% CI : (0.492, 0.9534)
##     No Information Rate : 0.5714         
##     P-Value [Acc > NIR] : 0.08559        
##                                          
##                   Kappa : 0.6111         
##                                          
##  Mcnemar's Test P-Value : NA             
## 
## Statistics by Class:
## 
##                      Class: A1 Class: A2 Class: B1 Class: B2
## Sensitivity             1.0000    0.0000   0.50000    1.0000
## Specificity             0.9167    1.0000   1.00000    0.6667
## Pos Pred Value          0.6667       NaN   1.00000    0.8000
## Neg Pred Value          1.0000    0.8571   0.92308    1.0000
## Prevalence              0.1429    0.1429   0.14286    0.5714
## Detection Rate          0.1429    0.0000   0.07143    0.5714
## Detection Prevalence    0.2143    0.0000   0.07143    0.7143
## Balanced Accuracy       0.9583    0.5000   0.75000    0.8333
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




