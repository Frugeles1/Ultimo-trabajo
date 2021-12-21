#######GUIA YAPO.CL######

#setwd("D:/Desktop/Big Data")

#librerias necesarias

library(xml2)
library(rvest)

#leer una pagina web

Yapo_araucania <- read_html("https://www.yapo.cl/araucania")
print(html_text(Yapo_araucania))

#obteniendo listing_thumbs.

listing_thumbs <- html_nodes(Yapo_araucania, css=".listing_thumbs")
print(html_text(listing_thumbs))

# obteniendo la fecha de los productos
fechaProductosYapo <- html_nodes(listing_thumbs, css=".date")
print(html_text(fechaProductosYapo))

# obteniendo la hora de los productos
horaProductosYapo <- html_nodes(listing_thumbs, css=".hour")
print(html_text(horaProductosYapo))

# Obteniendo los titulos de los productos
nombresProductosYapo <- html_nodes(listing_thumbs, css=".title")
print(html_text(nombresProductosYapo))

# Obteniendo los precios
preciosProductosYapo <- html_nodes(listing_thumbs, css=".price")
print(html_text(preciosProductosYapo))

# Obteniendo las categoria de los productos
categoriasProductosYapo <- html_nodes(listing_thumbs, css=".category")
print(html_text(categoriasProductosYapo))

# Obteniendo las regiones de los productos
regionesProductosYapo <- html_nodes(listing_thumbs, css=".region")
print(html_text(regionesProductosYapo))

# Obteniendo las comunas de los productos
comunasProductosYapo <- html_nodes(listing_thumbs, css=".commune")
print(html_text(comunasProductosYapo))

# Obteniendo el link
seccionImagenProductoYapo <- html_nodes(listing_thumbs,css=".listing_thumbs_image")
linkProductoYapo <- html_nodes(seccionImagenProductoYapo,css="a")
hrefProductoYapo <- html_attr(linkProductoYapo,"href")

# creacion de variables, liempieza y su rellenado acorde a la paguina.

Valor_UF <- 30887.21
precio_normal <- c()
precio_calculado <- c()
vectorvalor_uf <- c()
tipo_moneda <- c()
for (elemento in 2:length(listing_thumbs)) {
  precio <- html_nodes(listing_thumbs[elemento],css=".price")
  if(length(precio)>0){
    precio <- html_text(precio)
    precio <- gsub("\t","",precio)
    precio <- gsub("\n","",precio)
    precio <- gsub("[$]","",precio)
    precio <- gsub("[.]","",precio)
    precio <- gsub(",",".",precio)
    precio <- gsub(" ","",precio)
    
    if(substr(precio,1,2) == 'UF'){
      precio <- gsub("UF","",precio)
      precio <- as.numeric(precio)
      #guardando precio normal
      precio_normal <- c(precio_normal,precio)
      
      precio <- precio*Valor_UF
      precio_calculado <- c(precio_calculado,precio)
      vectorvalor_uf <- c(vectorvalor_uf,Valor_UF)
      tipo_moneda <- c(tipo_moneda,"UF")
    }else{
      precio <- as.numeric(precio)
      precio_normal <- c(precio_normal,precio)
      precio_calculado <- c(precio_calculado,NA)
      vectorvalor_uf <- c(vectorvalor_uf,NA)
      tipo_moneda <- c(tipo_moneda,"peso")
    }
    
  }else{
    precio <- NA
    precio_normal <- c(precio_normal,precio)
    precio_calculado <- c(precio_calculado,NA)
    vectorvalor_uf <- c(vectorvalor_uf,NA)
    tipo_moneda <- c(tipo_moneda,NA)
  }
  print("====================================" )
  print(precio)
}

#creacion de un data con todos los datos

araucania_1 <- data.frame(Fecha=html_text(fechaProductosYapo),Hora=html_text(horaProductosYapo),Nombre=html_text(nombresProductosYapo),
                          Categoria=html_text(categoriasProductosYapo),Region=html_text(regionesProductosYapo),
                          Comuna=html_text(comunasProductosYapo),PrecioNormal=precio_normal,
                          PrecioCalculado=precio_calculado,TipoMoneda=tipo_moneda,ValorUf=vectorvalor_uf)

#guardando data creado como un archivo csv

write.csv2(araucania_1,"araucania_1.csv")

#se cargan los datas

Datos1 <- read.csv("datosYapo_1.csv") ### Esta no debido a la separacion de los datos ###
Datos1 <- read.csv2("datosYapo_1.csv")

Datos2 <- read.csv2("datosYapo_2.csv")

Datos3 <- read.csv2("datosYapo_3.csv") ###Cambio debido al error ###

Datos4 <- read.csv2("araucania_1.csv") 

#se verifica las etiquetas

names(Datos1) 

#se combinan las 3 datas en una sola

alldatos = rbind(Datos1,Datos2,Datos3)
alldatos = rbind(Datos1,Datos2,Datos4)

#se limpia la data

library(dplyr)

alldatosC <- select(alldatos,-X,-Fecha,-Hora)

### dejo de funcionar ###

unique(alldatosC)

alldatoslimpia <- unique(alldatosC)

duplicated(alldatosC)
duplicated(alldatoslimpia)
alldatosC[duplicated(alldatosC),]

### esta si funciono ###

distinct(data.frame(alldatosC))

alldatoslimpia <- distinct(data.frame(alldatosC)) #data sin duplicados ###

### informacion mediante graficos ###

library(ggplot2)
?ggplot()

#====================================#

Grafico_1 <- ggplot(alldatoslimpia, aes(x = PrecioNormal , y = Categoria, colour = TipoMoneda)) + geom_point()


#====================================#

Grafico_2 <- ggplot(alldatoslimpia, aes(x=Categoria)) + geom_bar() + coord_flip() + ggtitle("Conteo por Categoria")

#====================================#

Grafico_3 <- ggplot(alldatoslimpia, aes(x=Categoria, y=PrecioNormal)) + geom_boxplot() + coord_flip()

#====================================#

Grafico_4 <- ggplot(alldatoslimpia, aes(x = PrecioNormal , y = Categoria, colour = Comuna)) + geom_point()
Grafico_4 <- Grafico_4 + geom_point(aes(shape = Comuna))
Grafico_4 <- Grafico_4 + geom_smooth(method = "lm")
Grafico_4 <- Grafico_4 + facet_grid(. ~ Comuna)

#====================================#

Grafico_5 <- hist(vector_Comunas) 

count(alldatoslimpia,Comuna)#otra forma#
vector_ComunasN <- pull(porcentaje,n)


#====================================#

Grafico_6 <- plot(vector_Comunas)

#====================================#

Grafico_7 <- plot(x = vector_ComunasN, main ="Gráfica de Comunas",
                  xlab="Comuna", ylab="n", col = rainbow(10))   #no funciona#
Grafico_7 <- barplot(vector_ComunasN, main ="Gráfica de Comunas", 
                     xlab="Comuna", ylab="n", col = rainbow(10)) #funciona#

vector_ComunasL <- pull(alldatoslimpia,Comuna) #factorcualitativo#

#====================================#

porcentaje <- alldatoslimpia %>%
  group_by(Comuna) %>%
  count() %>%
  ungroup() %>%
  mutate(percentage=`n`/sum(`n`) * 100)

Grafico_8 <- ggplot(porcentaje, aes(x=1, y=percentage, fill=Comuna)) +
  geom_bar(stat="identity") +
  geom_text(aes(label = paste0(round(percentage,1),"%")), 
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") + 
  theme_void() + scale_fill_brewer(palette="RdGy")

#====================================#

Grafico_9 <- plot(porcentaje) #matriz de correlacion#

#====================================#

Grafico_10 <- ggplot(porcentaje, aes(x=Comuna, y=n, fill=percentage)) + 
  geom_bar(stat="identity", position="dodge")

