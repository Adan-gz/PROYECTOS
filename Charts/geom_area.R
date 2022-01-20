# Originally, labels were not related to countries but to different type of company products, so
# I changed the labels to share the plot code. 

# The dataframe contains a date variable with months, a numerical value 'p' which is the monthly 
# transactions percentage of each product, and a factor with the product labels.

### named vector with product colors:
col_product <- c('Spain'='#1E86CD', 'PO'='#136AA7','ARG'='#0B5F9A','SWE'='#0D4D7A',
                 'IRE'='#ECEDEF','IT'="#7C8092", 'FR'="#C087B2",
                 'GER'='#845E7B','USA'='#64465D' )
                 
### reorder the factor variable
df_git$product <- fct_relevel(df_git$product, names(col_product))



### the plot with ggplto2
 # windowsfonts (maybe you need to install them)
windowsFonts(Norwester = windowsFont("Norwester"))
windowsFonts(Norwester = windowsFont("Roboto"))

df %>% 
  ggplot(aes(month, p, fill=product))+
  geom_area(alpha=.85)+
  scale_x_date('',breaks = scales::breaks_width('2 month'), labels = scales::label_date_short(),
                  expand = expansion(.01))+
  scale_y_continuous('',labels = scales::percent_format(), expand = expansion(.015) )+
  scale_fill_manual(values = col_product)+
  
  labs(fill='',caption = paste('Data as of',format(Sys.Date(),'%B %d, %Y')),
       title = '\nThe evolution of relative products importance (\u20AC)',
       subtitle = '% of monthly transactions by product:')+
  
  # comments: 
  ## 2020
  annotate(geom="text", x=as_date('2020-04-15'), y=.9, 
           label="2020 was dominated by\nFR, GER and USA",
           color="white",size=4.5)+
  annotate(geom="text", x=as_date('2020-03-15'), y=.42, 
           label="More than 50% of\npurchases were made\nby GER in this period",
           color="white",size=3.5)+
  geom_rect(aes(xmin=as_date('2020-01-01'),xmax=as_date('2020-06-01'),
                ymin=.1,ymax=.64),alpha=0,col='gray',linetype=2,show.legend = F)+
  
     ## transition period
  geom_vline(xintercept = as_date('2021-03-01'), linetype=2,size=.4, col='white')+
  geom_vline(xintercept = as_date('2021-05-01'), linetype=2,size=.4, col='white')+
  annotate(geom="text", x=as_date('2020-12-01'), y=.1, 
           label="Transition period",
           color="white",size=3.5)+
  geom_curve(
    aes(x = as_date('2021-01-01'), xend = as_date('2021-04-01'),
        y = .08,  yend = .15),col='white',
    arrow = arrow(length = unit(0.03, "npc")))+
  
    ## 2021
  annotate(geom="text", x=as_date('2021-09-01'), y=.9, 
           label="In 2021 took the lead\nSpain, PO, ARG and SWE",
           color="white",size=4.5)+
  
  annotate(geom="text", x=as_date('2021-09-01'), y=.17, 
           label="SWE lost importance\nat the end of 2021",
           color="white",size=3.4)+
  
    # plot theme
  theme(
    text = element_text(family = "Roboto"), 
    plot.title = element_text(colour = "#BBBBBB", size = 17.5, family = "Norwester"), 
    plot.subtitle = element_text(colour = "#BBBBBB", size = 13, family = "Norwester"),
    axis.text = element_text(colour = "#BBBBBB"), 
    plot.caption = element_text(colour = "#BBBBBB",vjust = 6), 
    legend.key = element_blank(),
    legend.background = element_blank(),
    legend.text = element_text(colour = "#BBBBBB"),  
    plot.background = element_rect(fill = "#252827", colour = NA),
    panel.background = element_rect(fill = "#252827", colour = NA), 
    axis.ticks.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(), 
    panel.grid.minor = element_blank()  )

