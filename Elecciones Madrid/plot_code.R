load("data/df_bloque.Rdata")
load("data/df_partido.Rdata")

library(tidyverse)

# Gráfico 1. Participación y Voto Bloques

col_bloque <- c("Izquierda" = "darkred", "Derecha" = "darkblue")

df_bloque %>%
  ggplot(aes( participacion, p_voto_bloque, col = bloque)) +
  theme_minimal()+
  geom_point(alpha = 0.03)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_x_continuous(
    breaks = seq(0,1,.2),
    labels = scales::percent_format(accuracy = 1))+
  geom_smooth()+
    facet_wrap(~eleccion, ncol = 3)+
  coord_cartesian(xlim = c(0.4,0.9))+
  
  scale_color_manual(values = col_bloque) +
  
  labs( title = "Participación y Voto en las Elecciones Autonómicas de Madrid",
        subtitle = "Resultados a nivel de sección para todas las elecciones autonómicas desde 1995.\nEn rojo el voto a la Izquierda, en azul a la derecha.\n\n% de voto: ", 
        x = "% participación", y = "",
        col = "", 
        caption = "Fuenta: Elaboración propia a partir de datos de la CAM.")+
  
  
  theme( panel.grid.major.x = element_blank(),
         panel.grid.minor.x = element_blank(),
         panel.grid.minor.y = element_blank(),
         plot.title = element_text(face = "bold"),
         legend.position = "none", # c(0.5,0.5),
         strip.text = element_text(face = "bold"),
         plot.subtitle = element_text(size = 11),
         plot.caption = element_text(hjust = 0, size = 9)
         )
         
         
# Gráfico 2. Participación y Renta
load("data/renta.rdata")

df_bloque %>% 
  filter(eleccion %in% c("2019","2021") ) %>% 
  left_join(renta) %>% 
  ggplot(aes(renta, participacion))+
  theme_minimal()+
  geom_point(alpha = 0.1, col = "darkorange")+
  scale_y_continuous( 
    breaks = seq(0,1,.1),
    labels = scales::percent_format(accuracy = 1) )+
  scale_x_continuous( breaks = seq(0,30000,5000) )+
  coord_cartesian(ylim = c(0.2,.9))+
  labs(title = "Madrid: Renta y Participacion",
       subtitle = "Renta media por persona en cada sección y participación en las elecciones de 2019 y 2021.\n\n % participación:", y = "",
       x = "Renta neta media por persona y sección",
       caption = "Fuenta: Elaboración propia a partir de datos de la CAM e INE (2018).")+
   theme( panel.grid.major.x = element_blank(),
         panel.grid.minor.x = element_blank(),
         panel.grid.minor.y = element_blank(),
         plot.title = element_text(face = "bold"),
         legend.position = c(0.5,1.1),
         strip.text = element_text(face = "bold"),
         plot.subtitle = element_text(size = 11),
         plot.caption = element_text(hjust = 0, size = 9)
         )+
  facet_wrap(~eleccion)



# Gráfico 3. Curva de Lorenz: Bloques

df_lorenz <- df_bloque %>% 
  filter(eleccion %in% c("2021","2019") ) %>% 
  left_join(renta) %>% 
  select(eleccion, CUSEC, bloque, voto_bloque, renta) %>% 
  arrange(renta) %>%
  group_by(eleccion) %>% 
  mutate( 
    renta_cut = cut( renta,10, labels = c("Bottom\n10%",
                   "20\n%","30\n%",
                   "40\n%","50\n%","60\n%",
                   "70\n%","80\n%","90\n%",
                   "Top\n10%")) ) %>% 
  select(CUSEC, bloque, voto_bloque, renta_cut) %>% 
  drop_na(voto_bloque) %>% 
  drop_na(renta_cut) %>% 
  group_by(eleccion, bloque) %>% 
  mutate( total_voto = sum(voto_bloque, na.rm = TRUE) ) %>% 
  ungroup() %>% 
  group_by(eleccion, renta_cut, bloque) %>%
  summarise( voto = sum(voto_bloque/total_voto,na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(eleccion, bloque) %>% 
  mutate(  voto_cum = cumsum(voto) ) %>% 
  ungroup()  
  
ggplot()+
  theme_minimal()+
  geom_point( data = df_lorenz ,mapping = aes(renta_cut, voto_cum, col = bloque),
              size = 2)+
  geom_line( data = df_lorenz ,mapping = aes(renta_cut, voto_cum, col = bloque, group = bloque),
             size = 1.2)+
  scale_color_manual(values = col_bloque)+
 
  geom_line(data = tibble(x = seq(0,10,1), 
                          y = seq(0,10,1)/10  ),  
          mapping = aes(x,y,group = 1), linetype = 3 )+
  geom_vline(xintercept = 5, linetype = 2)+
  geom_hline(yintercept = 0.7, linetype = 3)+
  
  facet_wrap(~eleccion)+

  scale_y_continuous( 
    breaks = seq(0,1,.2),
    labels = scales::percent_format(accuracy = 1) )+
  labs(title = "Curva de Lorenz: Renta y Voto a cada Bloque",
       subtitle = "Deciles de Renta y voto acumulado de cada bloque en las elecciones de 2019 y 2021.\n\n % voto acumulado:",
       x = "Deciles de renta", y = "",
       col = "",
       caption = "Fuenta: Elaboración propia a partir de datos de la CAM e INE (2018).")+
    theme( panel.grid.major.x = element_blank(),
         panel.grid.minor.x = element_blank(),
         panel.grid.minor.y = element_blank(),
         plot.title = element_text(face = "bold"),
         legend.position = c(0.5,0.73),
         strip.text = element_text(face = "bold"),
         plot.subtitle = element_text(size = 11),
         plot.caption = element_text(hjust = 0, size = 9) )
         
         
# Gráfico 4. Curva de Lorenz: Partidos

df_lorenz_partido <- df_partido %>% 
  filter(eleccion %in% c("2021","2019") ) %>% 
  left_join(renta) %>% 
  select(eleccion, CUSEC, partido, voto_partido = voto, renta) %>% 
  arrange(renta) %>%
  group_by(eleccion) %>% 
  mutate( 
    renta_cut = cut( renta,10, labels = c("Bottom\n10%",
                   "20\n%","30\n%",
                   "40\n%","50\n%","60\n%",
                   "70\n%","80\n%","90\n%",
                   "Top\n10%")) ) %>% 
  select(CUSEC, partido, voto_partido, renta_cut) %>% 
  drop_na(voto_partido) %>% 
  drop_na(renta_cut) %>% 
  group_by(eleccion, partido) %>% 
  mutate( total_voto = sum(voto_partido, na.rm = TRUE) ) %>% 
  ungroup() %>% 
  group_by(eleccion, renta_cut, partido) %>%
  summarise( voto = sum(voto_partido/total_voto,na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(eleccion, partido) %>% 
  mutate(  voto_cum = cumsum(voto) ) %>% 
  ungroup()  %>% 
  mutate( partido = fct_relevel( partido, "Podemos","PSOE","MM","VOX","Cs","PP"  ) )
  

colores <- c("#fa4e00", "#0fddc4","#007cc9", "#dc0612", "#732a66", "#59c232" )
names(colores) <- c("Cs", "MM", "PP", "PSOE", "Podemos", "VOX")

ggplot()+
  theme_minimal()+
  geom_point( data = df_lorenz_partido ,mapping = aes(renta_cut,
                                                      voto_cum,
                                                      col = partido),
              size = 2.4)+
  geom_line( data = df_lorenz_partido ,mapping = aes(renta_cut,
                                                     voto_cum, 
                                                     col = partido, 
                                                     group = partido),
             size = 1.2)+
  scale_color_manual(values = colores)+
  geom_line(data = tibble(x = seq(0,10,1), 
                          y = seq(0,10,1)/10  ),  
          mapping = aes(x,y,group = 1), linetype = 3 )+
  geom_vline(xintercept = 5, linetype = 2)+
  scale_y_continuous( 
    breaks = seq(0,1,.1),
    labels = scales::percent_format(accuracy = 1) )+

  geom_vline(xintercept = 5, linetype = 2)+
  geom_hline(yintercept = 0.7, linetype = 3)+

  facet_wrap(~eleccion)+
  
  scale_y_continuous( 
    breaks = seq(0,1,.2),
    labels = scales::percent_format(accuracy = 1) )+
  labs(title = "Curva de Lorenz: Renta y Voto a cada Partido",
       subtitle = "Deciles de Renta y voto acumulado de cada partido en las elecciones de 2019 y 2021.\n\n % voto acumulado:",
       x = "Deciles de renta", y = "",
       col = "",
       caption = "Fuenta: Elaboración propia a partir de datos de la CAM e INE (2018).")+
    theme( panel.grid.major.x = element_blank(),
         panel.grid.minor.x = element_blank(),
         panel.grid.minor.y = element_blank(),
         plot.title = element_text(face = "bold"),
         legend.position = c(0.5,0.53),
         strip.text = element_text(face = "bold"),
         plot.subtitle = element_text(size = 11),
         plot.caption = element_text(hjust = 0, size = 9) )
         
   
# Gráfico 5. Participación y voto laggeado

df_bloque %>% 
  filter( eleccion != "1995") %>% 
  ggplot(aes( participacion_lag, p_voto_bloque_lag, col = bloque))+
  theme_minimal()+
  geom_point( alpha = 0.09)+
  geom_smooth(method = "lm")+
  scale_color_manual(values = col_bloque)+
  coord_cartesian(xlim = c(-0.4,0.4))+
  geom_vline(xintercept = 0, linetype = 2)+
  geom_hline(yintercept = 0, linetype = 2)+
  
  facet_wrap(~eleccion, ncol = 4)+
  
  scale_y_continuous( breaks = seq(-0.4,0.4,0.4),
                      labels = c("-40\npp","0","+40\npp") )+
  
  scale_x_continuous( breaks = seq(-0.4,0.4,0.2),
                      labels = c("-40\npp","-20","0","+20","+40\npp") )+
  
  labs(title = "Participación y Voto laggeado",
       subtitle = "Incrementos de participación y voto a cada bloque a nivel de sección en cada elección\nrespecto a los anteriores comicios.\nEn rojo el voto a la Izquierda, en azul a la derecha.\n\n% de voto: ",
       
       x = "% incremento de participación", y = "% incremento del voto:",
       col = "",
       caption = "Fuenta: Elaboración propia a partir de datos de la CAM.\nNota: los incrementos son puntos porcenutales, no porcentajes.")+
  
  theme( panel.grid.major.x = element_blank(),
         panel.grid.minor.x = element_blank(),
         panel.grid.minor.y = element_blank(),
         plot.title = element_text(face = "bold"),
         legend.position = "none", #c(0.85,0.3),
         strip.text = element_text(face = "bold"),
         plot.subtitle = element_text(size = 11),
         plot.caption = element_text(hjust = 0, size = 9) )


# Gráfico 6 Zoom en 2021

df_bloque %>% 
  filter( eleccion == "2021") %>% 
  ggplot(aes( participacion_lag, p_voto_bloque_lag, col = bloque))+
  theme_minimal()+
  geom_point( alpha = 0.09)+
  geom_smooth(method = "lm")+
  scale_color_manual(values = col_bloque)+
  
  coord_cartesian(xlim = c(-0.2,0.2), ylim = c(-0.5,0.5))+
  
  geom_vline(xintercept = 0, linetype = 2)+
  geom_hline(yintercept = 0, linetype = 2)+
  
  scale_y_continuous( breaks = seq(-0.5,0.5,0.25),
                      labels = c("-50\npuntos\nporcentuales", "-25", "0","+25","+50\npuntos\nporcentuales") )+
  
  scale_x_continuous( breaks = seq(-0.2,0.2,0.05),
                     labels = c("-20\npuntos\nporcentuales","-15","-10","-5","0",
                                "+5","+10","+15","+20\npuntos\nporcentuales") )+
  
  labs(title = "ZOOM 2021: Participación y Voto laggeado",
       subtitle = "Incrementos de participación y voto a cada bloque a nivel de sección en cada elección\nrespecto a los anteriores comicios.\n\n% de voto: ",
       
       x = "% incremento de participación", y = "% incremento del voto:",
       col = "",
       caption = "Fuenta: Elaboración propia a partir de datos de la CAM.\nNota: los incrementos son puntos porcenutales, no porcentajes.")+
  
  theme( panel.grid.major.x = element_blank(),
         panel.grid.minor.x = element_blank(),
         panel.grid.minor.y = element_blank(),
         plot.title = element_text(face = "bold"),
         legend.position = c(0.3,0.85),
         strip.text = element_text(face = "bold"),
         plot.subtitle = element_text(size = 11),
         plot.caption = element_text(hjust = 0, size = 9) )






























