#-----------------------------------------------------------------------------------------------------------------------
#                                                 TASA DE ANEMIA - DEPARTAMENTOS
# AUTOR: César Anderson Huamaní Ninahuanca
# Correo: pe.cesar.huamani.n@uni.pe
#-----------------------------------------------------------------------------------------------------------------------
library(dplyr)
library(data.table)
library(srvyr)
library(datametria)
library(ggplot2)
library(openxlsx)
library(gt)
library(gtExtras)
library(ggridges)

# ENDES. 
r0<-fread("datos/RECH0_2024.csv") %>% as_tibble() 
r1<-fread("datos/RECH1_2024.csv") %>% as_tibble() 
r6<-fread("datos/RECH6_2024.csv") %>% as_tibble() 

r6 %>% inner_join(r1 %>% rename(HC0=HVIDX)) %>% 
  inner_join(r0) %>% filter(HV007==2024)->tb_1

tb_1 %>% mutate(peso=HV005/1000000,
                alt=(HV040/1000)*3.3) %>% 
  mutate(haj=case_when(HV040<1000~HC53/10,
                       HV040>=1000~HC53/10 -(-0.032*alt+0.022*alt*alt),
                       TRUE~NA_real_))->tb_1

# Anemia. 
tb_1 %>% mutate(años3=ifelse(HV103==1 & (HC1>=6 & HC1<=35),"SI","NO"),
                años5=ifelse(HV103==1 & (HC1>=6 & HC1<=59),"SI","NO")) %>% 
  mutate(anemia_3años=case_when(años3=="SI" & haj>1 & haj<11~"Tiene anemia",
                                años3=="SI" & haj>=11 & haj<30~"No tiene anemia",
                                TRUE~NA_character_),
         anemia_niveles_3años=case_when(años3=="SI" & haj>=10 & haj<11~"Anemia leve",
                                        años3=="SI" & haj>=7 & haj<10~"Anemia moderada",
                                        años3=="SI" & haj>1 & haj<7~"Anemia grave",
                                        años3=="SI" & haj>=11 & haj<30~"No tiene anemia",
                                        TRUE~NA_character_),
         anemia_5años=case_when(años5=="SI" & haj>1 & haj<11~"Tiene anemia",
                                años5=="SI" & haj>=11 & haj<30~"No tiene anemia",
                                TRUE~NA_character_),
         anemia_niveles_5años=case_when(años5=="SI" & haj>=10 & haj<11~"Anemia leve",
                                        años5=="SI" & haj>=7 & haj<10~"Anemia moderada",
                                        años5=="SI" & haj>1 & haj<7~"Anemia grave",
                                        años5=="SI" & haj>=11 & haj<30~"No tiene anemia",
                                        TRUE~NA_character_))->tb_1

# Generando los departamentos
tb_1$departamentos<-dpto_peru(ifelse(nchar(tb_1$UBIGEO)==5, paste0("0",tb_1$UBIGEO),tb_1$UBIGEO))

# A nivel distrital
ubigeos<-read.xlsx("datos/geodir-ubigeo-inei.xlsx")

ubigeos %>% distinct(Ubigeo,Departamento,Provincia,Distrito) %>% mutate_all(limpiecito) %>% as_tibble()->ubigeos

tb_1 %>% left_join(ubigeos %>% mutate(Ubigeo=as.numeric(Ubigeo)), by=c("UBIGEO"="Ubigeo"))->tb_1

#-----
options(survey.lonely.psu = "adjust")

# Definiendo el diseño muestral.
diseño2<-tb_1 %>% as_survey_design(ids=HV001, strata=HV022, weights=peso, nest = T)

# diseño2 %>% filter(!is.na(anemia_3años) & HV015==1 & HC55==0) %>% 
#   group_by(departamentos,anemia_3años) %>% 
#   summarize(survey_prop(vartype = c("ci","cv")))
# 
# diseño2 %>% filter(!is.na(anemia_niveles_3años) & HV015==1 & HC55==0) %>% 
#   group_by(departamentos,anemia_niveles_3años) %>% 
#   summarize(survey_prop(vartype = c("ci","cv")))
# 
# diseño2 %>% filter(!is.na(anemia_5años) & HV015==1 & HC55==0) %>% 
#   group_by(departamentos,anemia_5años) %>% 
#   summarize(survey_prop(vartype = c("ci","cv")))
# 
# diseño2 %>% filter(!is.na(anemia_niveles_5años) & HV015==1 & HC55==0) %>% 
#   group_by(departamentos,anemia_niveles_5años) %>% 
#   summarize(survey_prop(vartype = c("ci","cv")))

#----
# Distrital
diseño2 %>% filter(!is.na(anemia_3años) & HV015==1 & HC55==0 & Departamento=="JUNIN") %>% 
  group_by(Provincia,anemia_3años) %>% 
  summarize(survey_prop(vartype = c("ci","cv")))

# General
diseño2 %>% filter(!is.na(anemia_3años) & HV015==1 & HC55==0) %>% 
  group_by(anemia_3años) %>% 
  summarize(survey_prop(vartype = c("ci","cv")))

diseño2 %>% filter(!is.na(anemia_5años) & HV015==1 & HC55==0) %>% 
  group_by(anemia_5años) %>% 
  summarize(survey_prop(vartype = c("ci","cv")))
#-----------------------------------------------------------------------------------------------------------------------
# El Mapa.
Peru1<-sf::st_read("MAPA/peru/Departamental INEI 2023 geogpsperu SuyoPomalia.shp")
Peru2<-sf::st_read("MAPA/provincias/Provincial INEI 2023 geogpsperu SuyoPomalia.shp")
Sur_America<-sf::st_read("MAPA/sudamerica/Sudamérica.shp") 

Peru1 %>% left_join(
  diseño2 %>% filter(!is.na(anemia_3años) & HV015==1 & HC55==0) %>% 
    group_by(departamentos,anemia_3años) %>% 
    summarize(survey_prop(vartype = c("ci","cv"))) %>% filter(anemia_3años=="Tiene anemia") %>% 
    mutate(departamentos=limpiecito(departamentos)),
  by=c("DEPARTAMEN"="departamentos")
)->anemia3

anemia3 %>% mutate(etiq=paste0(DEPARTAMEN,"\n",round(coef*100,1),"%"))->anemia3

c_anemia3<-sf::st_centroid(anemia3) 
c_anemia3<-cbind(anemia3, sf::st_coordinates(sf::st_centroid(anemia3$geometry)))

ggplot()+
  geom_sf(data= Sur_America, fill="gray80", linewidth=1)+
  geom_sf(data= anemia3, color="black", aes(fill=coef), show.legend = F)+
  scale_fill_distiller(palette = "YlOrRd", trans = "reverse")+
  annotate(geom = "text", x = -80, y = -10, label = "Océano \nPacífico", fontface = "italic", color = "Blue", size = 3)+
  annotate(geom = "text", x = -78, y = -2, label = "Ecuador", fontface = "italic", color = "Black", size = 3)+
  annotate(geom = "text", x = -72, y = -1, label = "Colombia", fontface = "italic", color = "Black", size = 3)+
  annotate(geom = "text", x = -70, y = -7, label = "Brasil", fontface = "italic", color = "Black", size = 3)+
  coord_sf(xlim = c(-81.3307,-68.65311), ylim = c(-18.3518,-0.03747),expand = FALSE)+
  ggrepel::geom_label_repel(data = c_anemia3, aes(x=X, y=Y, label = etiq), size = 3,
                            color="black", fontface = "bold", alpha=.8)+
  theme_minimal() +
  theme(panel.grid.major = element_line(color = gray(.5),
                                        linetype = "dashed", size = 0.5),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "lightblue1",colour= "black", size = 1))+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.4) +
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true",
                                    pad_x = unit(0.9, "in"), pad_y = unit(0.3, "in"),
                                    style = ggspatial::north_arrow_fancy_orienteering)+
  labs(title = "Tasa de anemia en menores de 3 años")+
  theme(plot.title = element_text(hjust = .5, face = "bold"))
ggsave("graficos/mapa_anemia_3años.png", width = 9, height = 9)
#-----
Peru1 %>% left_join(
  diseño2 %>% filter(!is.na(anemia_5años) & HV015==1 & HC55==0) %>% 
    group_by(departamentos,anemia_5años) %>% 
    summarize(survey_prop(vartype = c("ci","cv"))) %>% filter(anemia_5años=="Tiene anemia") %>% 
    mutate(departamentos=limpiecito(departamentos)),
  by=c("DEPARTAMEN"="departamentos")
)->anemia5

anemia5 %>% mutate(etiq=paste0(DEPARTAMEN,"\n",round(coef*100,1),"%"))->anemia5

c_anemia5<-sf::st_centroid(anemia5) 
c_anemia5<-cbind(anemia5, sf::st_coordinates(sf::st_centroid(anemia5$geometry)))

ggplot()+
  geom_sf(data= Sur_America, fill="gray80", linewidth=1)+
  geom_sf(data= anemia5, color="black", aes(fill=coef), show.legend = F)+
  scale_fill_distiller(palette = "YlOrRd", trans = "reverse")+
  annotate(geom = "text", x = -80, y = -10, label = "Océano \nPacífico", fontface = "italic", color = "Blue", size = 3)+
  annotate(geom = "text", x = -78, y = -2, label = "Ecuador", fontface = "italic", color = "Black", size = 3)+
  annotate(geom = "text", x = -72, y = -1, label = "Colombia", fontface = "italic", color = "Black", size = 3)+
  annotate(geom = "text", x = -70, y = -7, label = "Brasil", fontface = "italic", color = "Black", size = 3)+
  coord_sf(xlim = c(-81.3307,-68.65311), ylim = c(-18.3518,-0.03747),expand = FALSE)+
  ggrepel::geom_label_repel(data = c_anemia5, aes(x=X, y=Y, label = etiq), size = 3,
                            color="black", fontface = "bold", alpha=.8)+
  theme_minimal() +
  theme(panel.grid.major = element_line(color = gray(.5),
                                        linetype = "dashed", size = 0.5),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "lightblue1",colour= "black", size = 1))+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.4) +
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true",
                                    pad_x = unit(0.9, "in"), pad_y = unit(0.3, "in"),
                                    style = ggspatial::north_arrow_fancy_orienteering)+
  labs(title = "Tasa de anemia en menores de 5 años")+
  theme(plot.title = element_text(hjust = .5, face = "bold"))
ggsave("graficos/mapa_anemia_5años.png", width = 9, height = 9)

#-----------------------------------------------------------------------------------------------------------------------

Peru2 %>% filter(DEPARTAMEN=="JUNIN") %>% left_join(
  diseño2 %>% filter(!is.na(anemia_3años) & HV015==1 & HC55==0 & Departamento=="JUNIN") %>% 
    group_by(Provincia,anemia_3años) %>% 
    summarize(survey_prop(vartype = c("ci","cv"))) %>% filter(anemia_3años=="Tiene anemia") %>% 
    mutate(Provincia=limpiecito(Provincia)),
  by=c("PROVINCIA"="Provincia")
)->anemia3_

anemia3_ %>% mutate(etiq=paste0(PROVINCIA,"\n",round(coef*100,1),"%"))->anemia3_

c_anemia3_<-sf::st_centroid(anemia3_) 
c_anemia3_<-cbind(anemia3_, sf::st_coordinates(sf::st_centroid(anemia3_$geometry)))

ggplot()+
  geom_sf(data= Peru1, fill="gray80", linewidth=1)+
  geom_sf(data= anemia3_, color="black", aes(fill=coef), show.legend = F)+
  scale_fill_distiller(palette = "YlOrRd", trans = "reverse")+
  coord_sf(xlim = c(-77,-73.2), ylim = c(-10.2,-13.2),expand = FALSE)+
  ggrepel::geom_label_repel(data = c_anemia3_, aes(x=X, y=Y, label = etiq), size = 3,
                            color="black", fontface = "bold", alpha=.8)+
  theme_minimal() +
  theme(panel.grid.major = element_line(color = gray(.5),
                                        linetype = "dashed", size = 0.5),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "lightblue1",colour= "black", size = 1))+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.4) +
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true",
                                    pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                                    style = ggspatial::north_arrow_fancy_orienteering)+
  labs(title = "Tasa de anemia en menores de 3 años\nProvincias de Junín")+
  theme(plot.title = element_text(hjust = .5, face = "bold"))
ggsave("graficos/mapa_anemia_3años_.png", width = 9, height = 6)

#------------------
diseño2 %>% filter(!is.na(anemia_niveles_3años) & HV015==1 & HC55==0) %>% 
   group_by(departamentos,anemia_niveles_3años) %>% 
   summarize(survey_prop(vartype = c("ci","cv"))) %>%
  rename(departamento=anemia_niveles_3años,
         `Incidencia (%)`=coef,
         `Lim. Inferior`=`_low`,
         `Lim. Superior`=`_upp`,
         `Coef de Variación`=`_cv`) %>% 
  gt() %>% 
  fmt_percent(column = c(`Incidencia (%)`,`Lim. Inferior`,`Lim. Superior`,`Coef de Variación`)) %>% 
  tab_header(title = "Niveles de Anemia para niños menores de 3 años",
             subtitle = "Por Departamentos") %>% 
  gt_theme_538(quiet = TRUE) %>% 
  tab_source_note("ELABORACIÓN: https://github.com/CesarAHN") %>% 
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_column_labels(columns = c(`Incidencia (%)`,`Lim. Inferior`,`Lim. Superior`,`Coef de Variación`)))) %>%
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_body(columns = c(`Incidencia (%)`,`Lim. Inferior`,`Lim. Superior`,`Coef de Variación`)))) %>% 
  gt_color_rows(`Incidencia (%)`:`Coef de Variación`, palette = "RColorBrewer::RdBu", reverse = T) %>% 
  gtsave("graficos/tabla_3años.png", vwidth = 600, vheight = 1500)
