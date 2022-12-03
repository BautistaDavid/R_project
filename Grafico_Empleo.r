library(tidyverse)
library(ggplot2)

df <- read_delim('empleo.csv',delim=';')
df <- df %>% mutate(año = substr(periodo, start = 1, stop = 4),
              mes = substr(periodo, start = 6, stop = 7))

total_ano <- df %>% 
  group_by(año) %>% 
  summarise(sumatoria = sum(empleos))

df2 <- df %>%
  merge(total_ano,by='año',all=TRUE)

df2 <- df2 %>% 
  mutate(porcentaje = (empleos/sumatoria)*100)


datos <- df2 %>% 
  group_by(mes) %>% 
  summarise(value = mean(porcentaje))


meses_names <- c('ENE','FEB','MAR','ABR','MAY','JUN','JUL','AGO','SEP','OCT','NOV','DIC')
datos <- datos %>% 
  mutate(individual = meses_names,
         id = seq(1,12))
data <- datos


label_data <- data

# calculate the ANGLE of the labels
number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
label_data$hjust<-ifelse( angle < -90, 1, 0)

# flip angle BY to make them readable
label_data$angle<-ifelse(angle < -90, angle+180, angle)



label_data <- label_data %>% 
  mutate(value_round = round(value,2))


data <- data %>% 
  mutate(value_round = round(value,2))







# Start the plot
p <- ggplot(data, aes(x=as.factor(id), y=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  # This add the bars with a blue color
  geom_bar(stat="identity", fill=alpha("skyblue", 0.7)) +
  
  # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
  ylim(-8,18) +
  
  # Custom the theme: no axis title and no cartesian grid
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(1,1,1,1,)      # Adjust the margin to make in sort labels are not truncated!
  ) +
  

  
  # This makes the coordinate polar instead of cartesian.
  coord_polar(start = 0) +
  
  # Add the labels, using the label_data dataframe that we have created before
  geom_text(data=label_data, aes(x=id, y=value+1, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE )+
  geom_text(
    x = 0, y = -8,
    label = "********\nPorcentaje Promedio\nde Ofertas Laborales\n en El Tiempo 1976 - 2022",
    family = "Roboto Mono Medium",
    size = 4,
    lineheight = 0.87,
    color = "grey60")

p



fig1 <- ggplot(data, aes(x=as.factor(id), y=value, fill = value)) +
  geom_bar(stat="identity") +
  # scale_fill_gradientn("(%)",colours = c( "#6C5B7B","#C06C84","#F67280","#F8B195"),limits=c(5,12)) + 
  scale_fill_gradient(high='#F3A4A4', low='#EA2800',limits=c(5,12)) +
  ylim(-1,13) +
  theme_minimal()+
  theme(
    legend.key.height  = unit(1.5, 'cm'),
    legend.key.width = unit(0.7,'cm'),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#F2F0F0"),
    legend.text = element_text(size=15),
    legend.title = element_text(size=16),
    legend.position = 'none'
  ) +
  guides(colour = guide_legend(title.position = "bottom")) +
  geom_text(x=2,y=20,label='@PipeBau_',family='Roboto Mono Medium',
            size = 5,color='black',fontface='bold') +
  geom_text(data=label_data, aes(x=id, y=value+1, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=6, angle= label_data$angle, inherit.aes = FALSE ) +
  #geom_text(data=label_data, aes(x=id, y=value-2, label=value_round, hjust=hjust), color="white", fontface="bold",alpha=0.6, size=2, angle= label_data$angle, inherit.aes = FALSE ) +
  geom_label(data=label_data,aes(x=id-0.05, y=value-2.1, label=value_round, hjust=hjust), color="black", fontface="bold", size=5,inherit.aes = FALSE,fill='#F2F0F0') + # aca el fill
  geom_text(
    x = 0.5, y = -16,
    label = "Porcentaje Promedio del Anual Total de \nOfertas Laborales Publicadas en El Tiempo\n 1976 - 2022",
    family = "Roboto Mono Medium",
    size = 8,
    lineheight = 0.9,
    color = "black",
    #color = "grey60",
    fontface="bold") + 
  coord_polar(start=0) 
  

  
fig1


ggsave("circle_test.png", width = 10, height = 10, units = "in", dpi = 300)
  
