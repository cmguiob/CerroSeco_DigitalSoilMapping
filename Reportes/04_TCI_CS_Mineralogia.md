---
title: "TCI - Cerro Seco / Suelos"
subtitle: "Resultados XRD"
author: "Carlos Guio"
date: "10.7.2021"
knit: (function(inputFile, encoding) { 
      out_dir <- 'Reportes';
      rmarkdown::render(input = inputFile,
                        encoding = encoding, 
                        output_file = file.path(
                                        here::here(), 
                                        out_dir, 
                                        '04_TCI_CS_Mineralogia.html'))
                                        })
output:
  html_document:
    theme: journal
    highlight: tango
    keep_md: true
---


```r
library(hrbrthemes)
library(waffle) #gg_waffle
library(ggplot2)
library(ggforce) #trans_reverser
library(ggrepel)
library(colorspace) #manipulate colors
library(tidyverse)
library(patchwork) #plot layout
library(showtext) #google fonts

knitr::opts_chunk$set(include = FALSE, echo = FALSE, warning = FALSE, message = FALSE, fig.align="center", fig.showtext = TRUE, fig.retina = 1, dpi = 300, out.width = "70%")

showtext_auto()
```







### Código: waffle legend


```r
df_legend <- data.frame(x = seq(-0.45,0.45, by = 0.1 ), 
                 y = 0, 
                 cls = c(rep(lighten("#c3beb8", 0.2, space = "HCL"), 9),
                         darken("#c3beb8", 0.4, space = "HCL")))
p_legend <- 
  
  ggplot() +
  geom_point(data= df_legend, aes(x = x, y = y), 
             col = df_legend$cls,
             size = 2,
             pch = 16) +
  geom_text(aes(x = -0.03, y = -0.25, label = "9%"), 
            size = 3.5,
            col = lighten("#c3beb8", 0.2, space = "HCL"),
            fontface = "bold")+
  annotate(geom = "curve", 
           x = -0.45, 
           y = -0.1,
           xend = -0.18, 
           yend = -0.25, 
           curvature = 0.35,
           col = lighten("#c3beb8", 0.2, space = "HCL"),
           size = 0.5) +
  annotate(geom = "curve", 
           x = 0.35, 
           y = -0.1,
           xend = 0.09, 
           yend = -0.25, 
           curvature = -0.35,
           col = lighten("#c3beb8", 0.2, space = "HCL"),
           size = 0.5) +
  geom_text(aes(x = 0.5, y = 0.25, label = "1%"), 
            size = 3.5,
            col = darken("#c3beb8", 0.4, space = "HCL"),
            fontface = "bold")+
  xlim(-0.7, 0.7) +
  ylim(-0.4, 0.4)+
  theme_void() 
```


### Código: waffle chart de proporciones minerales


```r
p_riet_03 <- riet_03 %>%
  ggplot() +
  geom_waffle(aes(fill = mine_corto, values = porcentaje),
              show.legend = FALSE,
              size = 0.1, #lining spacing
              color = "white",
              flip = TRUE,
              radius = unit(2, "pt"))+
  coord_equal() +
  labs(fill = NULL, colour = NULL) +
  scale_fill_manual(values = unique(riet_03$colores))+
  facet_wrap(~horizonte, ncol = 1, strip.position = "left")+
  theme(plot.margin = margin(20, 10, 20, 10),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        strip.placement = "outside",
        strip.text.y.left = element_text(colour = darken("#F5F2F1", 0.3, 
                                                       space = "HCL"), 
                                        face = "bold",
                                        angle = 0),
        strip.switch.pad.wrap = unit(0.5, "lines"))

p_riet_01 <- riet_01 %>%
  ggplot() +
  geom_waffle(aes(fill = mine_corto, values = porcentaje),
              show.legend = FALSE,
              size = 0.1, #lining spacing
              color = "white",
              flip = TRUE,
              radius = unit(2, "pt"))+
  coord_equal() +
  labs(fill = NULL, colour = NULL) +
  scale_fill_manual(values = unique(riet_01$colores))+
  facet_wrap(~horizonte, ncol = 1, strip.position = "left")+
  theme(plot.margin = margin(20, 10, 20, 10),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        strip.placement = "outside",
        strip.text.y.left = element_text(colour = darken("#F5F2F1", 0.3, 
                                                       space = "HCL"), 
                                        face = "bold",
                                        angle = 0),
        strip.switch.pad.wrap = unit(0.5, "lines"))
```




### Código: line plot de difractograma CS03


```r
mine_d_03 <- c(15, 10, 7.3, 4.5, 4.25, 4.175, 4.05, 3.75, 3.55, 3.35, 
            3.2, 2.98,2.71, 2.58, 2.52, 2.45)
mine_l_03 <- c("S", "I", "K/H", "","","G","C","F","","Q", "","Hb" ,"He","","", "")
mine_cols_03 <- c("#806dac","#c581a3","#9f742f","#9f742f","#2165aa","#eddc5e",
               "#38a6e3","#fc9681","#9f742f","#2165aa","#fc9681","#5ca22f",
               "#d26428", "#5ca22f", "#9f742f", "#2165aa")


DRX_CS03 <- DRX_df %>% dplyr::filter(ID == "CS03" & d < 17 & d > 2.295 )


p_xrd_03 <- ggplot() +
            geom_line(data = DRX_CS03 %>% dplyr::filter(TRATAMIENTO == "P"), 
                aes(x= d, y= I_plot), 
                size = 0.5,
                color = darken("#F5F2F1", 0.5, space = "HCL")) +
            geom_line(data = DRX_CS03 %>% dplyr::filter(TRATAMIENTO == "N"), 
                aes(x= d, y= I_plot), 
                size = 0.1,
                color = darken("#F5F2F1", 0.3, space = "HCL")) +
            geom_line(data = DRX_CS03 %>% dplyr::filter(TRATAMIENTO == "EG"), 
                aes(x= d, y= I_plot), 
                size = 0.1,
                color = darken("#F5F2F1", 0.3, space = "HCL")) +
            geom_line(data = DRX_CS03 %>% dplyr::filter(TRATAMIENTO == "C100"), 
                 aes(x= d, y= I_plot), 
                size = 0.1,
                color = darken("#F5F2F1", 0.3, space = "HCL")) +
            geom_text_repel(data = DRX_CS03,
                  aes(x = d, y = I_plot, color = TRATAMIENTO, label = ETIQUETA),
                  size = 2.5, #font size
                  colour = darken("#F5F2F1", 0.5, space = "HCL"),
                  family = "roboto",
                  hjust = 0,
                  direction = "y",
                  xlim = c(2.25, NA), #para que salgan de la gráfica
                  ylim = c(-0.8, NA), #para que salgan de la gráfica
                  nudge_x = 0.1,
                  segment.size = 0.4, #segment thickness
                  segment.curvature = -0.2, #curvature direction - for left
                  segment.alpha = .5,
                  segment.linetype = "dotted",
                  segment.ncp = 3,
                  segment.angle = 10,
                  point.padding = 0.05,
                  box.padding = 0.18) + #If too high, labels clump
            scale_y_continuous(limits = c(0,7), expand = c(0,0)) +
  #limits in scale_x_continuous would cut the line, not plot area: avoided
            scale_x_continuous(trans = trans_reverser('log10'), 
                     breaks= c(3,4,5, 7,10,15), 
                     labels = c(3,4,5,7,10,15),
                     sec.axis = sec_axis(~., breaks = mine_d_03,
                                         labels = mine_l_03))+
            facet_wrap(~ HZ, ncol = 1 ) + 
            labs(x = "Distancia interplanar (Angstrom)")+
            theme(plot.margin = margin(20, 70, 20, 10), #top, right, bottom, left
                  panel.grid = element_line(colour = "white"),
                  axis.ticks.x.top =element_line(
                                  size = 0.1,
                                  arrow = arrow(length = unit(0.4, "lines"), 
                                                ends = "first", 
                                                type = "closed"),
                                                color = mine_cols_03),
                  axis.text.x = element_text(
                                  color = darken("#F5F2F1", 
                                                 0.3, 
                                                 space = "HCL")),
                  axis.text.x.top = element_text(margin = margin(0,0,16,0),
                                                 color = mine_cols_03,
                                                 size = 8,
                                                 face = "bold"),
                  axis.title.x.bottom = element_text(
                                  margin = margin(16, 0, 0, 0)),
                  strip.text = element_blank(),
                  legend.position = "none") +
                  # with coord_cartesian the limits cut the plot area
                  coord_cartesian(xlim = c(15.1, 2.54), ylim = c(0, 7), clip = "off") 
```

### Código: line plot de difractograma CS01


```r
mine_d_01 <- c(7.3, 4.5, 4.25, 4.175, 4.05, 3.75, 3.55, 3.35, 
            3.2, 2.98,2.71, 2.58, 2.52, 2.45)
mine_l_01 <- c("K/H", "","","G","C","F","","Q", "", "Hb" ,"He","","", "")
mine_cols_01 <- c("#9f742f","#9f742f","#2165aa","#eddc5e",
               "#38a6e3","#fc9681","#9f742f", "#2165aa","#fc9681", "#5ca22f",
               "#d26428", "#5ca22f", "#9f742f", "#2165aa")

DRX_CS01 <- DRX_df %>% dplyr::filter(ID == "CS01" & d < 15 & d > 2.295 )


p_xrd_01 <- ggplot() +
            geom_line(data = DRX_CS01 %>% dplyr::filter(TRATAMIENTO == "P"), 
                aes(x= d, y= I_plot), 
                size = 0.5,
                color = darken("#F5F2F1", 0.5, space = "HCL")) +
            geom_line(data = DRX_CS01 %>% dplyr::filter(TRATAMIENTO == "N"), 
                aes(x= d, y= I_plot), 
                size = 0.1,
                color = darken("#F5F2F1", 0.3, space = "HCL")) +
            geom_line(data = DRX_CS01 %>% dplyr::filter(TRATAMIENTO == "EG"), 
                aes(x= d, y= I_plot), 
                size = 0.1,
                color = darken("#F5F2F1", 0.3, space = "HCL")) +
            geom_line(data = DRX_CS01 %>% dplyr::filter(TRATAMIENTO == "C100"), 
                 aes(x= d, y= I_plot), 
                size = 0.1,
                color = darken("#F5F2F1", 0.3, space = "HCL"))+
            geom_text_repel(data = DRX_CS01,
                  aes(x = d, y = I_plot, color = TRATAMIENTO, label = ETIQUETA),
                  size = 2.5, #font size
                  colour = darken("#F5F2F1", 0.5, space = "HCL"),
                  family = "roboto",
                  hjust = 0,
                  direction = "y",
                  xlim = c(2.25, NA), #para que salgan de la gráfica
                  ylim = c(-0.8, NA), #para que salgan de la gráfica
                  nudge_x = 0.1,
                  segment.size = 0.4, #segment thickness
                  segment.curvature = -0.2, #curvature direction - for left
                  segment.alpha = .5,
                  segment.linetype = "dotted",
                  segment.ncp = 3,
                  segment.angle = 10,
                  point.padding = 0.05,
                  box.padding = 0.18) + #If too high, labels clump
            scale_y_continuous(limits = c(0,8), expand = c(0,0)) +
  #limits in scale_x_continuous would cut the line, not plot area: avoided
            scale_x_continuous(trans = trans_reverser('log10'), 
                     breaks= c(3,4,5, 7,10), 
                     labels = c(3,4,5,7,10),
                     sec.axis = sec_axis(~., breaks = mine_d_01,
                                         labels = mine_l_01))+
            facet_wrap(~ HZ, ncol = 1 ) + 
            labs(x = "Distancia interplanar (Angstrom)")+
            theme(plot.margin = margin(20, 70, 20, 10), #top, right, bottom, left
                  panel.grid = element_line(colour = "white"),
                  axis.ticks.x.top =element_line(
                                  size = 0.1,
                                  arrow = arrow(length = unit(0.4, "lines"), 
                                                ends = "first", 
                                                type = "closed"),
                                                color = mine_cols_01),
                  axis.text.x = element_text(
                                  color = darken("#F5F2F1", 
                                                 0.3, 
                                                 space = "HCL")),
                  axis.text.x.top = element_text(margin = margin(0,0,16,0),
                                                 color = mine_cols_01,
                                                 size = 8,
                                                 face = "bold"),
                  axis.title.x.bottom = element_text(
                                  margin = margin(16, 0, 0, 0)),
                  strip.text = element_blank(),
                  legend.position = "none") +
                  # with coord_cartesian the limits cut the plot area
                  coord_cartesian(xlim = c(14, 2.5), ylim = c(0, 8), clip = "off") 
```

### Layout


```r
p_layout_01 <- p_riet_01 + p_xrd_01 + plot_layout(widths = c(1, 3.5)) + inset_element(p_legend, -0.53, -0.29, -0.13, -0.01) # l, b, r, t

p_layout_01
```

<img src="C:\Users\cguio\Documents\Terrae\TCI_Cerro Seco\_Git\Reportes\04_TCI_CS_Mineralogia_files/figure-html/layout_01-1.png" width="70%" style="display: block; margin: auto;" />


```r
p_layout_03 <-p_riet_03 + p_xrd_03 + plot_layout(widths = c(1, 3.5)) + inset_element(p_legend, -0.53, -0.17, -0.13, -0.01) # l, b, r, t

p_layout_03
```

<img src="C:\Users\cguio\Documents\Terrae\TCI_Cerro Seco\_Git\Reportes\04_TCI_CS_Mineralogia_files/figure-html/layout_03-1.png" width="70%" style="display: block; margin: auto;" />



