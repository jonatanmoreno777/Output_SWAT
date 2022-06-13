######## 

setwd("D:/Qswat_cachi/Jonatan_tesis/Scenarios/Simulación_R")
rm(list=ls())
system("SWAT_64rel.exe")

library(pacman)#cargar paquetes
p_load(data.table, ggplot2, reshape2, dplyr)

#crear tabla para datos de subcuenca:TxtInOut
datos <- read.table("output (2075-2099).sub", skip = 9, header = FALSE) #copiar output.sub de scenarios
datos = as.data.table(datos)

#dividir 11 datos(# nro de subcuencas) de la tabla :TablesOut (formato access) EN QSWAT
datos1 <- tail(datos,11)

#dividir los datos de la columna de la tabla y crear una nueva tabla #TablesOut
datos2 <- datos1[, list(V2, V5, V8, V9, V11, V12, V13, V20)]

#sumar colmumnas especificas
datos2$bwf <- rowSums(datos2[, c (5,6,8)],na.rm = F)
datos2$gwf <- rowSums(datos2[, c (3,4)],na.rm = F)

#Agregar columna
datos3<- cbind(datos2,Surname=c("San Pedro de Cachi","Vinchos","Pongora","Yucaes","Paccha", "Huatatas", "chillico","Chicllarazo (aguas arriba)",
                                          "Apacheta","Huanta","Chicllarazo (aguas abajo)"))

#establecer el nombre de la columna
setnames(datos3, old = c("Surname", "V5","V8","V9","V11", "V12","V13","V20","bwf","gwf"), new = c("Subcuencas", "PRECIP", "ET", "SW", "SURQ","GW","WYLD", "LAT","BWF","GWF"))

#Eliminar columna
tab <- select(datos3, -V2)

#cambiar la tabla a una forma más larga desde una forma más ancha
tab1 <- melt(tab, id.vars = "Subcuencas", variable.name = "Parámetros", value.name = "Q_mmaño")

#Guardar
#write.csv(tab1,"D:/Qswat_cachi/Jonatan_tesis/Scenarios/Simulacion_Python/GWF_BWFpy.csv", quote = F)

#redondear
#library(dplyr)
tab2 <- tab1 %>% 
  mutate_if(is.numeric, round,0)

#gráfico de barras replanteadas

#fig1
ggplot(data = tab2, mapping = aes(x = Subcuencas, y = Q_mmaño, fill = Parámetros)) + geom_bar(stat = "identity") + ggtitle("Comparación de diferentes parámetros a nivel de subcuencas")
#windows()
ggplot(data = tab2, mapping = aes(x = Subcuencas, y = Q_mmaño, fill = Parámetros)) +
  geom_bar(stat = "identity") + ggtitle("Comparación de diferentes parámetros a mivel de subcuencas")+
  labs(title = "",
       x = "Subcuencas",
       y = "mm/año")
#fig2
ggplot(data = tab2, mapping = aes(x = Subcuencas, y = Q_mmaño, fill = Parámetros))+
  geom_bar(stat = "identity")+coord_flip()+
  labs(y='(mm/año)')+ theme(axis.text.y=element_text(size = 8)) +
  geom_text(aes(label = paste(Q_mmaño, "")),
            position = position_stack(vjust = 0.5), size=2.6) 
#scale_fill_manual(values=c("red", "blue", "green", "yellow", "gray70"))

ggplot(data = tab2, mapping = aes(x = Subcuencas, y = Q_mmaño, colour = Parámetros, shape = Parámetros)) + geom_point() + geom_line() + geom_smooth(method = lm, se = FALSE, fullrange = TRUE) 
#scatterplot
ggplot(data = tab2, mapping = aes(x = Subcuencas, y = Q_mmaño, colour = Parámetros, shape = Parámetros)) + geom_point() + geom_line() + geom_smooth(method = lm, se = FALSE, fullrange = TRUE) 

#boxplot
ggplot(data = tab2, mapping = aes(x = Parámetros, y = Q_mmaño)) + geom_boxplot()

ggplot(data = tab2, mapping = aes(x = Parámetros, y = Q_mmaño)) +
  geom_boxplot(alpha = 0) +
  geom_jitter(alpha = 0.4, color = "blue")+
  labs(title = "Comparación de diferentes parámetros anualmente (2075 - 2099)") 


####################################################################################


