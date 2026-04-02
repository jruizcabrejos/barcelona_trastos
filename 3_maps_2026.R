# ================================
# ==========  Libraries ==========
# ================================

library(httr)
library(jsonlite)
library(tidyverse)
library(gtsummary)
library(gghighlight)
library(png)
library(grid)
library("viridis")
library(ggplot2)
library(showtext)
library(ggrepel)
library(cowplot)
library(ggtext)
library(extrafont)
library(scales)
library(ggridges)
library(ggpubr)
library(magick)

# ================================
# ============  SETUP ============
# ================================

## Export settings

scale_factor = 5
# variable name      |n      or | or \ Symbol on Keyboard
# "forge-"    
# e901 github
# e900 discord
font_add(family = "emojis", "./indata/emojis.ttf")
font_import()
n
showtext_auto(TRUE)

## Read shapefiles
sf_AEB <- sf::st_read("./indata/Barcelona_shp/0301040100_AEB_UNITATS_ADM.shp")
sf_barris <- sf::st_read("./indata/Barcelona_shp/0301040100_Barris_UNITATS_ADM.shp")
sf_districts <- sf::st_read("./indata/Barcelona_shp/0301040100_Districtes_UNITATS_ADM.shp")
sf_roads <- sf::st_read("./indata/BCN_GrafVial_SHP/BCN_GrafVial_Trams_ETRS89_SHP.shp")

## Read data
df <- read_csv("./geocoded_scrapped_data20260401.csv") %>% 
  
  ## Clean variables
  janitor::clean_names() %>%
  dplyr::select(-c(x1,x2)) %>%
  
  ## Filter out "muebles y trastos viejos en el teléfono 010"
  filter(nchar(time) < 50) %>%
  
  ## Encode variable of interst
  mutate(Time_custom = case_when(time=="Lunes, de 20.00 a 22.00 h."~ "Lunes",
                                 time=="Martes, de 20.00 a 22.00 h."~ "Martes",
                                 time=="Miércoles, de 20.00 a 22.00 h."~ "Miércoles",
                                 time=="Jueves, de 20.00 a 22.00 h."~ "Jueves",
                                 time=="Viernes, de 20.00 a 22.00 h."~ "Viernes",
                                 TRUE ~ time)) %>%
  mutate(Time_custom = factor(Time_custom,
                              c("Lunes","Martes",
                                "Miércoles","Jueves","Viernes"),
                              c("Lunes","Martes",
                                "Miércoles","Jueves","Viernes"))) %>%
  
  ## For good measure from previous version
  distinct(df_name,k,df_name_specific,.keep_all = T)


# ==================================
# ============  PROCESS ============
# ==================================

sf_city <- st_as_sf(df %>% 
                      filter(!is.na(lon)), 
                    
                    coords=c("lon","lat"),
                    crs=4326, remove=F) %>% 
  
  st_transform(st_crs(sf_AEB)) %>%
  mutate(area_class_1 = lengths(st_within(.,sf_AEB)))

sf_final <- sf_city %>% 
  
  
  group_by(lat,lon) %>%
  slice(1) %>%
  ungroup() %>% 
  
  
  st_join(sf_AEB) %>%
  as.data.frame(.) %>%
  
  ## Count by AEB
  group_by(Time_custom, AEB) %>%
  tally() %>%
  
  ## Filter by AEB
  group_by(AEB) %>%
  filter(n==max(n))
  
  
sf_AEB <- sf_AEB %>%
  left_join(sf_final, by=c("AEB"))

# ===================================
# ============  PLOTTING ============
# ===================================

## Others
colors <- c("#D40000","#FFCC00","#D97D4B","#006747","#005F7F")


# sf_city  %>%
#   ggplot()+
#   geom_sf(data=sf_city%>%
#             filter(nchar(time) < 50 &
#                      area_class_1==1), aes(color=time))+
#   geom_sf(fill=NA)

##==##==##==## 
## Dot plot ##
##==##==##==## 
plot_dots <- ggplot()+
  
  ## Data layer
  geom_sf(data=sf_city%>%
            filter(area_class_1==1) %>% # Inside Barcelona
            
            group_by(lat,lon) %>%
            slice(1) %>%
            ungroup(), 
          
          aes(color=Time_custom),alpha=0.35,stroke=NA,size=1)+
  scale_color_manual(values = colors,
                     name="De 20.00h a 22.00h.")  +
  
  ## Roads
  geom_sf(data=sf_roads,fill=NA,color=alpha("grey50"))+
  ## AEB
  geom_sf(data=sf_AEB, fill = alpha("white",.25),color=alpha("black", .2), linetype = "dotted") +
  ## Barris
  geom_sf(data=sf_barris, fill=NA, color = alpha("black",.6), linetype = "dotted") +
  ## Districts
  geom_sf(data=sf_districts, fill=NA, color = alpha("black"))+
  
  labs(title="Días de Recojo de Muebles y Trastos", subtitle="Siempre hay algo que encontrar.",
       caption=c("<p><span style='font-family:emojis'>&#xe901;</span> /jruizcabrejos/barcelona_trastos   &#8212; Última Actualización:  &emsp; 29/01/2025 &#40;dd/mm/yyyy&#41;","<br>Fuente: https:&#47;&#47;www&#46;ajuntament.barcelona.cat/cercador-de-residus</p>"))+
  
  guides(color = guide_legend(override.aes = list(size=5,alpha=1))) +
  
  theme_bw()+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.caption = element_markdown(hjust=0,vjust=1,
                                        face="italic", size = scale_factor*9, 
                                        lineheight = 0.4),
        
        plot.title = element_markdown(face = "bold", size = scale_factor * 17,
                                      hjust = 0,
                                      lineheight=0.3),
        
        plot.subtitle = element_markdown(face="italic",
                                         size = scale_factor * 14,
                                         lineheight=0.3),
        legend.position = "inside",
        legend.position.inside = c(.2, .28),   
        # legend.spacing.x = unit(1, "pt"),
        # legend.spacing.y = unit(0.5, "pt"),
        # legend.direction="horizontal",
        # legend.box.just = "left",
        
        legend.title = element_text(size=scale_factor*12),
        legend.text = element_text(size = scale_factor * 11,
                                   lineheight=0.5),
        strip.text.x = element_text(size = scale_factor * 12),
        legend.background = element_rect(fill = alpha('white', 0.4))#,
        # plot.margin = margin(0, 0, 0, 0, "pt")
  )


ggsave("./mapsV2/Barcelona_Basura_Muebles_Mapa_Dias_puntos_raw6_filter.png",plot_dots,
       width=8,height=10,units="in",device="png",dpi=300)




##==##==##==##==## 
##  Filled plot ##
##==##==##==##==## 

AEB_plot <- ggplot() +
  
  ## Roads
  geom_sf(data=sf_roads,fill=NA, color=alpha("grey50"))+
  
  ## Data layer
  geom_sf(data=sf_AEB, # Inside Barcelona
          
          aes(fill=Time_custom),alpha=0.7, size=0.5)+
  scale_fill_manual(values = colors,
                     name="De 20.00h a 22.00h.")   +
  ## AEB
  geom_sf(data=sf_AEB, fill=NA, color=alpha("black", .2), linetype = "dotted") +
  ## Barris
  geom_sf(data=sf_barris, fill=NA, color = alpha("black",.6), size=0.5) +
  ## Districts
  geom_sf(data=sf_districts, fill=NA, color = alpha("black"), size=1)+
  
  labs(title="Días de Recojo de Muebles y Trastos", subtitle="Siempre hay algo que encontrar.",
       caption=c("<p><span style='font-family:emojis'>&#xe901;</span> /jruizcabrejos/barcelona_trastos   &#8212; Última Actualización:  &emsp; 29/01/2025 &#40;dd/mm/yyyy&#41;","<br>Fuente: https:&#47;&#47;www&#46;ajuntament.barcelona.cat/cercador-de-residus</p>"))+
  
  guides(color = guide_legend(override.aes = list(size=5,alpha=1))) +
  
  theme_bw()+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.caption = element_markdown(hjust=0,vjust=1,
                                        face="italic", size = scale_factor*9, 
                                        lineheight = 0.4),
        
        plot.title = element_markdown(face = "bold", size = scale_factor * 17,
                                      hjust = 0,
                                      lineheight=0.3),
        
        plot.subtitle = element_markdown(face="italic",
                                         size = scale_factor * 14,
                                         lineheight=0.3),
        legend.position = "inside",
        legend.position.inside = c(.2, .28),   
        # legend.spacing.x = unit(1, "pt"),
        # legend.spacing.y = unit(0.5, "pt"),
        # legend.direction="horizontal",
        # legend.box.just = "left",
        
        legend.title = element_text(size=scale_factor*12),
        legend.text = element_text(size = scale_factor * 11,
                                   lineheight=0.5),
        strip.text.x = element_text(size = scale_factor * 12),
        legend.background = element_rect(fill = alpha('white', 0))#,
        # plot.margin = margin(0, 0, 0, 0, "pt")
  )

AEB_plot
ggsave("./mapsV2/Barcelona_Basura_Muebles_Mapa_Dias_AEB_raw6_filter.png",AEB_plot,
       width=8,height=10,units="in",device="png",dpi=300)



