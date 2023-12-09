#Juan Kamilo Narvaez - 202113808
#"R version 4.3.1 (2023-06-16 ucrt)"

#Limpiar el entorno
rm(list=ls()) 

## instalar/llamar pacman
require(pacman)
p_load(rio,
       skimr,
       janitor,
       data.table,
       tidyverse,
       ggplot2,
       dplyr)

#Verificar el directorio
getwd()
list.files()


### PUNTO 1 ###
## Punto 1.1
rutas <- list.files("input/" , recursive=T , full.names=T)
## Punto 1.2 

## Extraer las rutas
rutas_resto <- str_subset(string = rutas, pattern ="Resto - C")

## Cargar en lista
lista_resto <- import_list(file = rutas_resto)

## Textear la cadena de caracteres 
rutas_resto[1]
str_sub(rutas_resto[20],start = 7 , 10)

## Agregar ruta
View(lista_resto[[1]])
lista_resto[[1]]$path <- rutas_resto[1]
  
## Aplicar loop  
for (i in 1:length(lista_resto)){
     lista_resto[[i]]$path <- rutas_resto[i]  
     lista_resto[[i]]$year <- str_sub(lista_resto[[i]]$path, start = 7 , 10)
}
View(lista_resto[[35]])

## Punto 1.3
lista_resto[[36]] <- NULL #elimina el data frame 36 no cumple con los requerimientos
df_resto <- rbindlist(l=lista_resto , use.names=T , fill=T)
## export
export(df_resto,"output/cg.rds")

### PUNTO 2##
#base de datos que se usara
cg_geih = import("output/cg.rds")

## Grafico raza
#cambiar las variables para el violin
#remplazar nulos como ninguna raza
cg_geih$P6080 <- ifelse(is.na(cg_geih$P6080), 6, cg_geih$P6080)
#cambiar los valores para tenerlaos como categoricos
cg_geih$P6080 <- factor(cg_geih$P6080, levels = 1:6, labels = c(
  "Indigena",
  "Gitano",
  "Raizal",
  "Palenquero",
  "Afro",
  "Ninguno"
))
# conjunto de datos para los graficos cg_geih
cg_geih_g1 <- subset(cg_geih, !(P6080 %in% c("Gitano","Raizal", "Palenquero")))
## Grafico de violin
g1 <- ggplot(cg_geih_g1, aes(x = P6080, y = P6040, fill = P6080)) +
  geom_violin(color = "white", trim = FALSE) +
  scale_fill_brewer(palette = "Set3") +  # Cambia la paleta de colores
  labs(title = "Diagrama de Violín de Edad por Raza", x = "Raza", y = "Edad") +
  theme_minimal() +  # Cambia el tema del gráfico
  theme(legend.position = "none")
ggsave("output/plot1.jpeg", plot = g1, device = "jpeg") # guarda el grafico


##Gráfico 2: sabe leer por raza
cg_geih_g2 <- subset(cg_geih_g1, P6040 > 15)
cg_geih_g2 <- cg_geih_g2%>%
  mutate(P6160 = factor(P6160, levels = c(1, 2), labels = c("Sabe Leer", "No Sabe Leer")))

g2 <- ggplot(cg_geih_g2, aes(fill = P6160, x = P6080)) +
  geom_bar(position = "fill", color = "white", width = 0.7) +
  labs(title = "Proporción de Personas que Saben Leer según la Raza (Mayores de 15)",
       x = "Raza",
       y = "Proporcion",
       fill = "Sabe leer") +
  theme_minimal()
ggsave("output/plot2t.jpeg", plot = g2, device = "jpeg") # guarda el grafico