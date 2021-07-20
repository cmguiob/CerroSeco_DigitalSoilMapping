
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

knitr::opts_chunk$set(include = FALSE, echo = FALSE, warning = FALSE, message = FALSE, fig.align="center", fig.showtext = TRUE, fig.retina = 1, dpi = 300, out.width = "70%", dev = "ragg_png")



# Cargar datos de perfiles
hz <- readr::read_csv('https://raw.githubusercontent.com/cmguiob/TCI_CerroSeco_git/main/Datos/Suelos_CS_Horiz.csv')

sitio <- readr::read_csv('https://raw.githubusercontent.com/cmguiob/TCI_CerroSeco_git/main/Datos/Suelos_CS_Sitio.csv')

#Select four profiles and relevant properties for plot
hz4 <- hz %>%
  dplyr::filter(ID %in% c("CS01", "CS02","CS03","CS04")) %>%
  dplyr::select(ID, BASE, TOPE, ESP, HZ, CON_POR, MX_H, MX_V, MX_C, CON_H, CON_V, CON_C, ARENA, LIMO, ARCILLA )

#Cargar poligono CS como sf
url_limite <- "https://raw.githubusercontent.com/cmguiob/TCI_CerroSeco_git/main/Datos_GIS/Poligonos/limite_CS_WGS84.geojson"
CSsf84 <- st_read(url_limite)

#Poligons were loaded first from local pc as .shp and then transformed to .geojson
#writeOGR(CS_limite, "limite_CS_WGS84.geojson", layer = "limite_CS_WGS84" driver = )

# Cargar poligonos mineria 2019 como sf
url_mineria <- ("https://raw.githubusercontent.com/cmguiob/TCI_CerroSeco_git/main/Datos_GIS/Poligonos/mineria_2019_CS_WGS84.geojson")
min2019sf <- st_read(url_mineria)

# Cargar DEM y transformar a data frame para manipular con ggplot
url_DEM <- "https://github.com/cmguiob/TCI_CerroSeco_git/blob/main/Datos_GIS/DEM_derivados/DEM_CS_Clip_4326.tif?raw=true"
DEM_ras84 <- raster(url_DEM)
DEM_clip <- mask(DEM_ras84, min2019sf, inverse = TRUE)
DEM_df_clip <- as.data.frame(DEM_clip, xy = TRUE)
names(DEM_df_clip) <- c("long", "lat", "altitude")

#Crear objeto espacial sf de sitio 
sitio_sp84 <- sitio
if(is.data.frame(sitio_sp84) == TRUE)coordinates(sitio_sp84) <- ~  long + lat
proj4string(sitio_sp84) <- "+proj=longlat +datum=WGS84 +no_defs"
sitio_sf84 <- st_as_sf(sitio_sp84, coords = c("long", "lat"),crs = 4326)

#Extraer datos de elevacion y pegarlos a sf
#alt <- raster::extract(DEM_clip, sitio_sp84)
#sitio = cbind(sitio, alt)
#sitio_sf84 = cbind(sitio_sf84, alt)

#Get osm data
calles <- getbb("Bogotá")%>% 
  opq()%>% 
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary",
                            "residential", "living_street",
                            "footway")) %>%  osmdata_sf()



# Calcular centroide
centro_sf <- st_centroid(CSsf84)

# Función para círculo
circleFun <- function(center=c(0,0), diameter=1, npoints=100, start=0, end=2, filled=TRUE){
  tt <- seq(start*pi, end*pi, length.out=npoints)
  dfc <- data.frame(
    x = center[1] + diameter / 2 * cos(tt),
    y = center[2] + diameter / 2 * sin(tt)
  )
  if(filled==TRUE) { 
    dfc <- rbind(df, center)
  }
  return(dfc)
}

# Aplicar función
circle <- circleFun(center = c(as.vector(centro_sf$geometry[[1]])), npoints = 1000,diameter = 0.039, filled = FALSE)
circle_sf <- SpatialPolygons(list(Polygons(list(Polygon(circle[,])), ID=1))) %>% st_as_sfc()
st_crs(circle_sf) = 4326

# Clip calles OSM 
cutout <- st_intersection(calles$osm_lines, circle_sf)





# Posibles escalas de color
col_scp <- c('#6AB6AA', '#4B6E8E', '#F9C93C', '#DA7543')
col_ito <- c('#56B4E9', '#009E73',"#E69F00", "#D55E00")

# Obtener fuentes
font_add_google(name = "Roboto Condensed", family= "robotoc")
font_add_google(name = "Roboto", family= "roboto")


# Definir theme
theme_set(theme_minimal(base_family = "roboto"))

theme_update(panel.grid = element_blank(),
             axis.text = element_text(family = "robotoc",
                                        color = "#c3beb8"),
             axis.title = element_blank(),
             axis.ticks.x =  element_line(color = "#c3beb8", size = .7),
             axis.ticks.y.right =  element_line(color = "#c3beb8", size = .7),
             legend.position = c(0,0.85),
             legend.direction = "vertical", 
             legend.box = "horizontal",
             legend.title = element_text(size = 13, 
                                         face = "bold", 
                                         color = "grey20", 
                                         family = "roboto"),
             legend.text = element_text(size = 10, 
                                        color = "#c3beb8", 
                                        family = "robotoc",
                                        face = "bold"),
             legend.key.size = unit(0.8, "cm"))



p_localizaciones <- ggplot() +
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

p_localizaciones




ggsave(file = "localizaciones.png", plot = p_localizaciones, device = ragg::agg_png, path = here::here("graficas"), dpi = 300)

