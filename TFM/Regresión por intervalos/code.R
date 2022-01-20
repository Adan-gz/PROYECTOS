load("data/spain_ATE.rData") #pronto subiré la limpieza y preparación de los datos

intervalo <- 1:75 #hay entrevistas hasta 75 días después del tratamiento

list_ATE <- vector("list",75) # lista para guardar el output de las regresiones

names(list_ATE) <- unite(   # se nombra cada lista con el número de días 
  tibble( x = as.character(intervalo), 
          dia = c("día",rep("días",74))),
  intervalo,
  c(x,dia) , sep = " ") %>% pull()

 # se efectúa la regresión para cada intervalo de días:
for (i in intervalo) {
     # se guarda el resultado en la lista, pero de forma tidy 
  list_ATE[[i]] <- lm(SCD ~ treatment, 
         data = spain_ATE %>% 
           filter( end_fecha >=  ymd(20110515)-i &
                   end_fecha <=  ymd(20110515)+i ) ) %>% 
    tidy() %>%  #y se incluye una variable que identifique el intervalo correspondiente
    mutate( "Significativo" = ifelse(p.value <= 0.05,
                                     "Sí","No"),
            intervalo = names(list_ATE)[i],
            dia = str_remove(intervalo, pattern = " día"),
            dia = str_extract(intervalo, pattern = "\\d+") %>% 
              as.numeric() ) %>%
    select( -statistic )
}
 
# se realiza el segundo gráfico:
plot_ATE_bin <- bind_rows(list_ATE) %>% 
  filter(term == "treatment1") %>% 
  ggplot(aes( dia, estimate, col = Significativo ))+
  theme_minimal()+
  geom_pointrange( aes( ymin = estimate - 1.96*std.error,
                        ymax = estimate + 1.96*std.error)  )+
  scale_x_continuous("Intervalo de  \u00b1 días pre y post 15-M",
                     breaks = seq(0,75,5))+
  geom_hline(yintercept = 0)+
  scale_y_continuous(breaks = seq(-0.6,2,0.4), 
                     labels = scales::number_format(accuracy = 0.1))+
  coord_cartesian(ylim = c(-0.6, 1.8))+
  scale_color_manual(values = c("gray10","gray40"))+
  
  labs(
       title = "Efecto del 15M sobre la SCD: regresión por MCO para diferentes bandwidths",
       subtitle = "Coeficiente del tratamiento e intervalo de confianza al 95%:",
       color = "Efecto significativo",
       y = "")+
  
  theme(
    plot.title = element_text(size = 11, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = c(0.75,0.75)
    
  )

# a continuación se obtiene el
# histograma de entrevistas

 # se crea un tibble donde guardar los resultados
N_entrevistados <- 
  tibble( N_total = vector("numeric",length = 75),
          "N Control" = vector("numeric",length = 75),
          "N Tratamiento" = vector("numeric",length = 75),
          intervalo = vector("numeric",length = 75))

for (i in intervalo) {
  
  data <- spain_ATE %>% 
           filter( end_fecha >=  ymd(20110515)-i &
                   end_fecha <=  ymd(20110515)+i ) %>%   
    drop_na(SCD) # se filtra par acada intervalo
  
  N_entrevistados[ i, 1 ] <- data %>% count() %>% pull()
  N_entrevistados[ i, 2 ] <- data %>% filter(treatment == 0) %>% count() %>% pull()
  N_entrevistados[ i, 3 ] <- data %>% filter(treatment == 1) %>% count() %>% pull()
  N_entrevistados[ i, 4 ] <- intervalo[i]
}

#se realiza el plot
plot_ATE_hist <- N_entrevistados %>% 
  select(-N_total) %>%
  pivot_longer("N Control":"N Tratamiento") %>% 
  mutate( name = fct_relevel(name, "N Tratamiento") ) %>% 
  
  ggplot(aes( intervalo, value, fill = name ))+
  theme_minimal()+
  geom_col()+
  scale_fill_manual(values = c("gray40","gray10"))+
  scale_x_continuous("Intervalo de  \u00b1 días pre y post 15-M",
                     breaks = seq(0,75,5))+
  labs(
     title = "Número de entrevistados para cada intervalo de días",
     fill = "",
     y = "")+
  
  theme(
    plot.title = element_text(size = 11, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = c(0.15,0.75)
    
  )

  
# se combinan ambos gráficos

gridExtra::grid.arrange( plot_ATE_hist,
              plot_ATE_bin, ncol = 1)

