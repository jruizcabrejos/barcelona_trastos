##==##==##==##
## ENGLISH  ##
##==##==##==##
plot_dots <- ggplot()+

  ## Data layer
  geom_sf(data=sf_AEB_dots%>%
            filter(area_class_1==1) %>% # Inside Barcelona
            group_by(lat,lon) %>%
            slice(1) %>% # Keep unique LAT/LON
            ungroup(),

          aes(color=Time_custom_eng,geometry=geometry),
          alpha=0.35,stroke=NA,size=1) +

  scale_color_manual(values = colors,
                     name="From 20.00h to 22.00h.")  +

  ## Roads
  geom_sf(data=sf_roads,fill=NA,color=alpha("grey50"))+
  ## AEB
  geom_sf(data=sf_AEB,aes(geometry=geometry), fill = alpha("white",.25),color=alpha("black", .2), linetype = "dotted") +
  ## Barris
  geom_sf(data=sf_barris, fill=NA, color = alpha("black",.6), linetype = "dotted") +
  ## Districts
  geom_sf(data=sf_districts, fill=NA, color = alpha("black"))  +

  ## Compass and Ruler
  # ggspatial::annotation_scale(
  #   location = "br",
  #   width_hint = 0.2,
  #   text_cex = 1.2 # Adjust scale bar text size here
  # ) +
  ggspatial::annotation_north_arrow(
    location = "tr",
    which_north = "true",
    pad_x = unit(0.1, "in"),
    pad_y = unit(0.3, "in"),

    height = unit(2.25, "cm"),
    width = unit(2.25, "cm"),
    style = ggspatial::north_arrow_fancy_orienteering(
      text_size = scale_factor_n * 11,line_width = 1,  # Adjust compass 'N' size here
    )
  ) +

  labs(title="Furniture and Junk Collection Days", subtitle="There's always a find.",
       caption=c("<p><span style='font-family:emojis'>&#xe901;</span> /jruizcabrejos/barcelona_trastos",
                 "<br>Last Update:  &emsp; 05/04/2026 &#40;dd/mm/yyyy&#41;",
                 "<br><br>Source: https:&#47;&#47;www&#46;ajuntament.barcelona.cat/cercador-de-residus</p>"))+

  guides(color = guide_legend(override.aes = list(size=5,alpha=1))) +

  theme_barcelona_trashmap()

ggsave("./image/eng_dots_Barcelona_Furniture_Junk_Map_days.png",plot_dots,
       width=10,height=10,units="in",device="png",dpi=300)



##==##==##==##
## CATALAN  ##
##==##==##==##
plot_dots <- ggplot()+

  ## Data layer
  geom_sf(data=sf_AEB_dots%>%
            filter(area_class_1==1) %>% # Inside Barcelona
            group_by(lat,lon) %>%
            slice(1) %>% # Keep unique LAT/LON
            ungroup(),

          aes(color=Time_custom_cat,geometry=geometry),
          alpha=0.35,stroke=NA,size=1) +

  scale_color_manual(values = colors,
                     name="De 20.00h a 22.00h.")  +

  ## Roads
  geom_sf(data=sf_roads,fill=NA,color=alpha("grey50"))+
  ## AEB
  geom_sf(data=sf_AEB,aes(geometry=geometry), fill = alpha("white",.25),color=alpha("black", .2), linetype = "dotted") +
  ## Barris
  geom_sf(data=sf_barris, fill=NA, color = alpha("black",.6), linetype = "dotted") +
  ## Districts
  geom_sf(data=sf_districts, fill=NA, color = alpha("black"))  +

  ## Compass and Ruler
  # ggspatial::annotation_scale(
  #   location = "br",
  #   width_hint = 0.2,
  #   text_cex = 1.2 # Adjust scale bar text size here
  # ) +
  ggspatial::annotation_north_arrow(
    location = "tr",
    which_north = "true",
    pad_x = unit(0.1, "in"),
    pad_y = unit(0.3, "in"),

    height = unit(2.25, "cm"),
    width = unit(2.25, "cm"),
    style = ggspatial::north_arrow_fancy_orienteering(
      text_size = scale_factor_n * 11,line_width = 1,  # Adjust compass 'N' size here
    )
  ) +

  labs(title="Dies de recollida de mobles i trastos", subtitle="Sempre es troba algun tresor.",
       caption=c("<p><span style='font-family:emojis'>&#xe901;</span> /jruizcabrejos/barcelona_trastos",
                 "<br>Última Actualització:  &emsp; 05/04/2026 &#40;dd/mm/yyyy&#41;",
                 "<br><br>Font: https:&#47;&#47;www&#46;ajuntament.barcelona.cat/cercador-de-residus</p>"))+

  guides(color = guide_legend(override.aes = list(size=5,alpha=1))) +

  theme_barcelona_trashmap()

ggsave("./image/cat_dots_Barcelona_Mobles_Trastos_Dies.png",plot_dots,
       width=10,height=10,units="in",device="png",dpi=300)


##==##==##==##
## ENGLISH  ##
##==##==##==##

AEB_plot <- ggplot() +

  ## Roads
  geom_sf(data=sf_roads,fill=NA, color=alpha("grey50"))+

  ## Data layer
  geom_sf(data=sf_AEB%>% # Inside Barcelona
            group_by(AEB) %>%
            slice(1) %>% # Keep unique LAT/LON
            ungroup(), # Inside Barcelona

          aes(fill=Time_custom_eng, geometry = geometry),alpha=0.7, size=0.5)+
  scale_fill_manual(values = colors,
                    name="From 20.00h to 22.00h.",na.translate = F)   +
  ## AEB
  geom_sf(data=sf_AEB,aes(geometry = geometry), fill=NA, color=alpha("black", .2), linetype = "dotted") +
  ## Barris
  geom_sf(data=sf_barris, fill=NA, color = alpha("black",.6), linewidth =0.2) +
  ## Districts
  geom_sf(data=sf_districts, fill=NA, color = alpha("black"),linewidth=0.8)  +

  ## Compass and Ruler
  # ggspatial::annotation_scale(
  #   location = "br",
  #   width_hint = 0.2,
  #   text_cex = 1.2 # Adjust scale bar text size here
  # ) +
  ggspatial::annotation_north_arrow(
    location = "tr",
    which_north = "true",
    pad_x = unit(0.1, "in"),
    pad_y = unit(0.3, "in"),

    height = unit(2.25, "cm"),
    width = unit(2.25, "cm"),
    style = ggspatial::north_arrow_fancy_orienteering(
      text_size = scale_factor_n * 11,line_width = 1,  # Adjust compass 'N' size here
    )
  ) +

  labs(title="Furniture and Junk Collection Days", subtitle="There’s always a find.",
       caption=c("<p><span style='font-family:emojis'>&#xe901;</span> /jruizcabrejos/barcelona_trastos",
                 "<br>Last Update:  &emsp; 05/04/2026 &#40;dd/mm/yyyy&#41;",
                 "<br><br>Source: https:&#47;&#47;www&#46;ajuntament.barcelona.cat/cercador-de-residus</p>"))+

  guides(color = guide_legend(override.aes = list(size=5,alpha=1))) +

  theme_barcelona_trashmap()


ggsave("./image/eng_Map_Barcelona_Furniture_Junk_Map_days.png",AEB_plot,
       width=10,height=10,units="in",device="png",dpi=300)



##==##==##==##
## CATALAN  ##
##==##==##==##

AEB_plot <- ggplot() +

  ## Roads
  geom_sf(data=sf_roads,fill=NA, color=alpha("grey50"))+

  ## Data layer
  geom_sf(data=sf_AEB %>% # Inside Barcelona
            group_by(AEB) %>%
            slice(1) %>% # Keep unique LAT/LON
            ungroup(), # Inside Barcelona

          aes(fill=Time_custom_cat, geometry = geometry),alpha=0.7, size=0.5)+
  scale_fill_manual(values = colors,
                    name="De 20.00h a 22.00h.",na.translate = F)   +
  ## AEB
  geom_sf(data=sf_AEB,aes(geometry = geometry), fill=NA, color=alpha("black", .2), linetype = "dotted") +
  ## Barris
  geom_sf(data=sf_barris, fill=NA, color = alpha("black",.6), linewidth =0.2) +
  ## Districts
  geom_sf(data=sf_districts, fill=NA, color = alpha("black"),linewidth=0.8)  +

  ## Compass and Ruler
  # ggspatial::annotation_scale(
  #   location = "br",
  #   width_hint = 0.2,
  #   text_cex = 1.2 # Adjust scale bar text size here
  # ) +
  ggspatial::annotation_north_arrow(
    location = "tr",
    which_north = "true",
    pad_x = unit(0.1, "in"),
    pad_y = unit(0.3, "in"),

    height = unit(2.25, "cm"),
    width = unit(2.25, "cm"),
    style = ggspatial::north_arrow_fancy_orienteering(
      text_size = scale_factor_n * 11,line_width = 1,  # Adjust compass 'N' size here
    )
  ) +

  labs(title="Dies de recollida de mobles i trastos", subtitle="Sempre es troba algun tresor.",
       caption=c("<p><span style='font-family:emojis'>&#xe901;</span> /jruizcabrejos/barcelona_trastos",
                 "<br>Última Actualització:  &emsp; 05/04/2026 &#40;dd/mm/yyyy&#41;",
                 "<br><br>Font: https:&#47;&#47;www&#46;ajuntament.barcelona.cat/cercador-de-residus</p>"))+

  guides(color = guide_legend(override.aes = list(size=5,alpha=1))) +

  theme_barcelona_trashmap()

ggsave("./image/cat_Map_Barcelona_Mobles_Trastos_Dies.png",AEB_plot,
       width=10,height=10,units="in",device="png",dpi=300)

