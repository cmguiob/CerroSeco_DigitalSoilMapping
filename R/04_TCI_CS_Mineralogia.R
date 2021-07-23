
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



# Obtener fuentes
font_add_google(name = "Roboto Condensed", family= "robotoc")
font_add_google(name = "Roboto", family= "roboto")

theme_set(theme_minimal(base_family = "roboto"))

theme_update(strip.background = element_blank(),
             axis.text = element_text(family = "robotoc"),
             axis.title.y = element_blank(),
             axis.text.y = element_blank(),
             axis.ticks.y = element_blank(),
             #spacing between figures/blocks and outer rim
             plot.margin = margin(c(1,1,1,1)),
             plot.background = element_rect(color =  "grey96",
                                            fill =  "grey96", 
                                            size = 2),
             # spacing between facets in a single block
             panel.spacing = unit(0.02, "lines"),
             panel.grid.major.y = element_blank(),
             panel.grid.minor.y = element_blank(),
             axis.title = element_text(colour = darken("#F5F2F1", 0.3, 
                                                       space = "HCL"), 
                                       face = "bold",
                                       family = "roboto")) 





riet <- readr::read_csv("https://raw.githubusercontent.com/cmguiob/TCI_CerroSeco_git/main/Datos/Difraccion/CS_Rietvelt.csv")

riet <- riet %>%
  mutate(colores = case_when(
      mine_corto == "s" ~  "#806dac",
      mine_corto == "i" ~  "#c581a3",
      mine_corto == "h" ~  "#a47832", 
      mine_corto == "hk" ~ "#9f742f",
      mine_corto == "g" ~  "#eddc5e",
      mine_corto == "c" ~  "#38a6e3",
      mine_corto == "pl" ~ "#FFA08E"  ,
      mine_corto == "pf" ~ "#fc9681",
      mine_corto == "q" ~  "#2165aa",
      mine_corto == "m" ~  "#d26428",
      mine_corto == "b" ~  "#5ca22f",
      mine_corto == "a" ~  "grey98",
      mine_corto == "x" ~  "grey94"))%>%
    mutate(mine_corto = as_factor(mine_corto))
      

riet_03  <- riet %>%
  filter(perfil == "CS03") %>%
  mutate(horizonte = factor(horizonte, 
                            levels = c("A/E", "Bth", "B", "Bdc2", "B/C2", 
                                       "Bv2", "B2", "R"))) %>%
  filter(porcentaje != 0)

riet_01  <- riet %>%
  filter(perfil == "CS01") %>%
  mutate(horizonte = factor(horizonte, 
                            levels = c("Bt", "Bn1", "Bt1"))) %>%
  filter(porcentaje != 0)



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




ruta <- "C:/Users/cguio/Documents/Terrae/TCI_Cerro Seco/"

files_list <- list.files(path = (paste(ruta,"Datos/Difraccion/", sep = "")), pattern = ".*xy$")

file_name <- paste(ruta,"Datos/Difraccion/",files_list, sep = "")

sample_names <- c("CS02_A_P","CS01_Bt_P", "CS01_Bn1_P", "CS01_Bt1_P",   
                  "CS03_A/E_P","CS03_A/E_C",  "CS03_A/E_C100", "CS03_A/E_EG", 
                  "CS03_A/E_N","CS03_Bth_P", "CS03_Bth_C", "CS03_Bth_C100",
                  "CS03_Bth_EG","CS03_Bth_N","CS03_Bdc2_P","CS03_Bdc2_C",
                  "CS03_Bdc2_C100","CS03_Bdc2_EG","CS03_Bdc2_N","CS03_B/C2_P",
                  "CS03_B/C2_C","CS03_B/C2_C100","CS03_B/C2_EG","CS03_B/C2_N",
                  "CS03_Bv2_P", "CS03_Bv2_C", "CS03_Bv2_C100", "CS03_Bv2_EG",
                  "CS03_Bv2_N","CS03_B2_P","CS03_B2_C","CS03_B2_C100",
                  "CS03_B2_EG","CS03_B2_N")

#Calculate D for optional plotting
braggs <- function(ttheta, l, d2r){l/(2*sin(ttheta*0.5*d2r))}

DRX_df <- file_name[] %>%
          map(~ read.table(., sep = " "))%>%
          map(~ `names<-`(., c("ttheta","I")))%>%
         `names<-`(sample_names)             %>%
          bind_rows(.id = "ID_HZ_T") %>%
          separate(col = ID_HZ_T, 
                   into = c("ID", "HZ", "TRATAMIENTO"), sep = "_") %>%
          mutate(I_scaled = scale(I, center = FALSE)) %>% 
          mutate(I_plot = case_when(
            (TRATAMIENTO == "P") ~ I_scaled + 0.5,
            (TRATAMIENTO == "N") ~ I_scaled + 2,
            (TRATAMIENTO == "EG") ~ I_scaled + 3.5,
            (TRATAMIENTO == "C100") ~ I_scaled + 5)) %>% 
          mutate(d = braggs(ttheta = ttheta, 
                            l = 1.541838, 
                            d2r = 0.0174532925199433)) %>%
          mutate(ETIQUETA = case_when(
            ((HZ == "A/E" | HZ == "Bt") & TRATAMIENTO == "P"  & 
               d > 2.2999 & d < 2.3002 ) ~ "Polvo",
            ((HZ == "A/E" | HZ == "Bt") & TRATAMIENTO == "N" &
               d > 2.2999 & d < 2.3002) ~ "Orientada",
            ((HZ == "A/E" | HZ == "Bt") & TRATAMIENTO == "EG" &
               d > 2.2999 & d < 2.3002) ~ "Etilenglicol",
            ((HZ == "A/E" | HZ == "Bt") & TRATAMIENTO == "C100" &
               d > 2.2999 & d < 2.3002) ~ "Caliente"))%>%
          mutate_at(c("ID", "HZ","TRATAMIENTO", "ETIQUETA"), as_factor)






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



p_layout_01 <- p_riet_01 + p_xrd_01 + plot_layout(widths = c(1, 3.5)) + inset_element(p_legend, -0.53, -0.29, -0.13, -0.01) # l, b, r, t

p_layout_01


p_layout_03 <-p_riet_03 + p_xrd_03 + plot_layout(widths = c(1, 3.5)) + inset_element(p_legend, -0.53, -0.17, -0.13, -0.01) # l, b, r, t

p_layout_03


ggsave(file = "mineralogia_01.png", plot = p_layout_01, device = "png", type ="cairo", path = here::here("graficas"), dpi = 300, width = 5.8, height = 3.8)

ggsave(file = "mineralogia_03.png", plot = p_layout_03, device = "png", type ="cairo", path = here::here("graficas"), dpi = 300, width = 5.8, height = 5.75)

