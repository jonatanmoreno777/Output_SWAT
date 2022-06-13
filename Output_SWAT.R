library (data.table)
library(ggplot2)
library(reshape2)
library(dplyr)


setwd("D:/Qswat_cachi/Jonatan_tesis/Scenarios/Simulación_R")
rm(list=ls())
system("SWAT_64rel.exe")

#crear tabla para datos de subcuenca:TxtInOut
sboptab <- read.table("output (2075-2099).sub", skip = 9, header = FALSE) #copiar output.sub de scenarios
sboptab = as.data.table(sboptab)

#dividir 11 datos de subcuencas de esa tabla :TablesOut (formato access) EN QSWAT
sbdttab <- tail(sboptab,11)

#dividir los datos de la columna de la tabla y crear una nueva tabla #TablesOut
sbdttabnew <- sbdttab[, list(V2, V5, V8, V9, V11, V12, V13, V20)]

#sumar 
#sbdttabnew$bwf <- rowSums(sbdttabnew[, 3:6])

#sumar colmumnas especificas
sbdttabnew$bwf <- rowSums(sbdttabnew[, c (5,6,8)],na.rm = F)
sbdttabnew$gwf <- rowSums(sbdttabnew[, c (3,4)],na.rm = F)

#Agregar columna
sbdttabnew1 <- cbind(sbdttabnew,Surname=c("San Pedro de Cachi","Vinchos","Pongora","Yucaes","Paccha", "Huatatas", "chillico","Chicllarazo (aguas arriba)",
                                          "Apacheta","Huanta","Chicllarazo (aguas abajo)"))

#establecer el nombre de la columna
setnames(sbdttabnew1, old = c("Surname", "V5","V8","V9","V11", "V12","V13","V20","bwf","gwf"), new = c("Subcuencas", "PRECIP", "ET", "SW", "SURQ","GW","WYLD", "LAT","BWF","GWF"))

#Eliminar columna
tab <- select(sbdttabnew1, -V2)

#cambiar la tabla a una forma más larga desde una forma más ancha
sbpartab <- melt(tab, id.vars = "Subcuencas", variable.name = "Parámetros", value.name = "Q_mmaño")

write.csv(sbpartab ,"D:/Qswat_cachi/Jonatan_tesis/Scenarios/Simulacion_Python/GWF_BWFpy.csv", quote = F)

#redondear
library(dplyr)
sbpartab1 <- sbpartab %>% 
  mutate_if(is.numeric, round,0)
#as.numeric(sbpartab$Q_mmaño)

#gráfico de barras replanteadas


ggplot(data = sbpartab1, mapping = aes(x = Subcuencas, y = Q_mmaño, fill = Parámetros)) + geom_bar(stat = "identity") + ggtitle("Comparación de diferentes parámetros a nivel de subcuencas")
#windows()
ggplot(data = sbpartab1, mapping = aes(x = Subcuencas, y = Q_mmaño, fill = Parámetros)) +
  geom_bar(stat = "identity") + ggtitle("Comparación de diferentes parámetros a mivel de subcuencas")+
  labs(title = "Observed genera through time",
       x = "Subcuencas",
       y = "mm/año")

ggplot(data = sbpartab1, mapping = aes(x = Subcuencas, y = Q_mmaño, fill = Parámetros))+
  geom_bar(stat = "identity")+coord_flip()+
  labs(y='(mm/año)')+ theme(axis.text.y=element_text(size = 8)) +
  geom_text(aes(label = paste(Q_mmaño, "")),
            position = position_stack(vjust = 0.5), size=2.6) #https://github.com/tidyverse/ggplot2/issues/3612
#scale_fill_manual(values=c("red", "blue", "green", "yellow", "gray70"))

ggplot(data = sbpartab, mapping = aes(x = Subcuencas, y = Q_mmaño, colour = Parámetros, shape = Parámetros)) + geom_point() + geom_line() + geom_smooth(method = lm, se = FALSE, fullrange = TRUE) 
#scatterplot
ggplot(data = sbpartab, mapping = aes(x = Subcuencas, y = Q_mmaño, colour = Parámetros, shape = Parámetros)) + geom_point() + geom_line() + geom_smooth(method = lm, se = FALSE, fullrange = TRUE) 

#boxplot
ggplot(data = sbpartab, mapping = aes(x = Parámetros, y = Q_mmaño)) + geom_boxplot()

ggplot(data = sbpartab, mapping = aes(x = Parámetros, y = Q_mmaño)) +
  geom_boxplot(alpha = 0) +
  geom_jitter(alpha = 0.4, color = "blue")+
  labs(title = "Comparación de diferentes parámetros anualmente (2075 - 2099)") 


ggplot(data = sbpartab,
       mapping = aes(x = Subcuencas, y = Q_mmaño, color = Parámetros)) +
  geom_line() +
  facet_grid(cols = vars(Parámetros))

ggplot(data = sbpartab,
       mapping = aes(x = Subcuencas, y = Q_mmaño, color = Parámetros)) +
  geom_line() +
  facet_wrap(vars(Parámetros)) +
  theme_bw()

########## swat grafico

ggplot(data = sbpartab, mapping = aes(x = Subcuencas, y = Q_mmaño, color = Parámetros)) +
  geom_line() +
  facet_wrap(vars(Parámetros)) +
  labs(title = "Observed genera through time",
       x = "Year of observation",
       y = "mm/año") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 12, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 12),
        strip.text = element_text(face = "italic"),
        text = element_text(size = 16))

ggplot(data = sbpartab, aes(x = Subcuencas, y = Q_mmaño, fill = Parametros)) +
  facet_wrap(~Parametros) +
  geom_boxplot() +
  scale_y_continuous(name = "Average Life Expectancy") +
  scale_x_discrete(labels = abbreviate, name = "Continent")



####################################################################################

library(gapminder)

# Charge libraries:
library(ggplot2)
library(gganimate)

# Make a ggplot, but add frame=year: one image per year
ggplot(gapminder, aes(Subcuencas, lifeExp, size = pop, color = continent)) +
  geom_point() +
  scale_x_log10() +
  theme_bw() +
  # gganimate specific bits:
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')
