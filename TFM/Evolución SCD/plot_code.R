# Lo primero es descargar los datos. Para ello, poner el email de su cuenta en el parámetro especificado.
# Para cada país se descargan todas las encuestas en las cuales aparece. El resultado que devuelve la función
# es una lista donde cada elemento es un dataframe

spain_evolucion <- essurvey::import_country("Spain", seq(1,9,1), 
                                            ess_email = "xxxx@gmail.com" )
portugal_evolucion <- essurvey::import_country("Portugal", seq(1,9,1), 
                                            ess_email = "xxxx@gmail.com" )
grecia_evolucion <- essurvey::import_country("Greece", c(1,2,4,5), 
                                            ess_email = "xxxx@gmail.com" )
italia_evolucion <- essurvey::import_country("Italy", c(1,2,6,8,9), 
                                            ess_email = "xxxx@gmail.com" )
                                            
# para unir estas cuatro listas en un único dataset realizamos lo siguiente:                                            

sur_europa <-   map(list( spain_evolucion, portugal_evolucion,  # las unimos en una única lista
        #grecia_evolucion, 
        italia_evolucion ),
      
      .f = function(x){ map(  x, function(z){
        z %>% 
         select( fecha = essround, pais = cntry, SCD = stfdem, peso = pspwght  ) # seleccionamos las variables de interés
       
     }  ) }
        
   ) %>% 
  bind_rows() %>% # unir listas y ajustar variables:
  mutate( pais = factor(pais, levels = c("ES", #"GR",
                                         "IT","PT"),
                        labels = c("España", "Grecia",
                                   "Italia","Portugal")),
          pais = fct_relevel(pais, "España","Portugal","Italia", "Grecia" ),
          across( SCD:peso, as.numeric ),
          fecha = factor(fecha, levels = seq(1,9,1),
                         labels = c("2002","2004","2006","2008","2010",
                                    "2012","2014","2016","2018"))
          ) %>% 
  drop_na(SCD)

# A continuación creamos el gráfico,
# pero previamente asignamos los respectivos colores
col_pais <- c("España"="brown4", 
              "Italia" ="skyblue4", 
              "Portugal" = "palegreen4")
              "Grecia"="gray60" )

#se calcula la media para cada ola y país y se realiza el plot

sur_europa %>% 
  group_by( fecha, pais ) %>% 
  summarise( mean = weighted.mean( SCD, peso )  ) %>% 
  
  ggplot(aes(fecha, y = mean, col = pais ))+
  theme_minimal()+
  
  geom_point( size = 4 )+
  geom_line(aes(group = pais), size = 1.2, alpha = .8)+
  geom_hline(yintercept = 5, size = 0.05, alpha = 0.2)+

  scale_y_continuous(breaks = seq(2,7,1))+
  scale_color_manual(values = col_pais)+
  
  labs( title = "Evolución de la satisfacción media con la democracia en el Sur de Europa",
        x = "", y = "", col = "",
        caption = "Fuente: para España y Portugal todas las rondas de la ESS. Italia: 1, 2, 6, 8, 9. Grecia: 1,2,4 y 5. 
Nota: media ponderada con peso de post-estratificación. La fecha es genérica ya que el traba-
jo de campo varía según país. Las fechas correctas para España son las del gráfico anterior." )+

  theme( panel.grid.major.x = element_blank(),
         panel.grid.minor = element_blank(),
         plot.title = element_text(face = "bold"),
         plot.caption = element_text(hjust = 0, size = 10),
         text = element_text(family = "serif"),
         axis.text = element_text(size = 11),
         legend.position = c(0.7,0.78))+
         
  coord_cartesian(ylim = c(2,7))
