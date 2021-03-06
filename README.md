# Cerro Seco - Digital Soil Mapping

![*Residual and buried paleosol in Cerro Seco (tepetate, cangahua)*](/IMG_20210307_110010.jpg)

![estado](https://img.shields.io/badge/status-finished-lightgrey&?style=for-the-badge&logo=appveyor) ![made-with-R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)

 ## In this repository you will find:
 * ### [Field and laboratory data](/Datos)
 * ### [Spatial data](/Datos_GIS)
 * ### [R code](/R)
 * ### [Simplified reports](/Reportes)
 * ### [Visualizations](/Graficas)


The data comes from a citizen-based soil survey in the area known as [Cerro Seco](https://goo.gl/maps/C2dALBSiKC8B5a4k9) (southern Bogotá). It was collected between 06.2020 and 07.2021. **Funds** for this project were given by the [*True Cost Initiative*](https://truecostsinitiative.org/) to [*Corporación Geoambiental Terrae (NGO)*](https://www.terraegeoambiental.org/), with whom I collaborate.
 
This study aimed to create a preliminary map of ancient soil (paleosol) sequences distribution in the area, using machine learning classification. Two models were selected due to time and data availability: K-nearest neighbors and Random Forest. The map was integrated with hydrological models of typical soil profiles and of the underlying rock, to make a first estimate of water recharge and overflow/interflow in the area. Information was submitted to the *Secretaría Distrital de Planeación* and the *Secretaría Distrital de Ambiente* to support the creation and management of a new conservation area in Cerro Seco.
 
## Study locations · *Ubicaciones de estudio*
  ![Location Cerro Seco](Graficas/localizaciones.png)
 
## Model profiles of paleosol/hydropedologic sequences *Modelos de perfiles de suelo*
![Perfiles](Graficas/perfiles.png)
 
 ## Random Forest Importance of remote sensing-derived indices *Importancia de índices*
 ![Importancia](Graficas/importancia_rf_variables.png)
 
 ## Preliminary sequence distribution models *Modelos preliminares de distribución de secuencias*
 ![Modelos](Graficas/mapas_modelos.png)
 
