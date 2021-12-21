# Obteniendo el link
seccionImagenProductoYapo <- html_nodes(listing_thumbs,css=".listing_thumbs_image")
linkProductoYapo <- html_nodes(seccionImagenProductoYapo,css="a")
hrefProductoYapo <- html_attr(linkProductoYapo,"href")
length(hrefProductoYapo)
print(hrefProductoYapo[1])

# Obteniendo la sub-paguina
subPaginaYapoCL <- read_html(print(hrefProductoYapo[1]))
print(html_text(subPaginaYapoCL))

boxProductoYapo <- html_nodes(subPaginaYapoCL, xpath="")
print(html_text(boxProductoYapo))

tabladelproducto <-
  
# Sacando tipo de transaccion
  
datailProductoYapo <- html_nodes(Datosub_pagina,css=".details")
trDetailsProductosYapo <- html_nodes(datailProductoYapo,css="tr")
  
for(detail in trDetailsProductoYapo){
  thDetail <- html_nodes(detail,css="th")
  print(html_text(detail))
  if(html_text(thDetail)=="Tipo"){
    tdDetail <- html_nodes(detail,css="td")
    print(html_text(tdDetail))
  }
   
} 

#===========================================#

# Se quita print detail

datailProductoYapo <- html_nodes(Datosub_pagina,css=".details")
trDetailsProductosYapo <- html_nodes(datailProductoYapo,css="tr")

for(detail in trDetailsProductoYapo){
  thDetail <- html_nodes(detail,css="th")

  if(html_text(thDetail)=="Tipo"){
    tdDetail <- html_nodes(detail,css="td")
    print(html_text(tdDetail))
  }
  
} 

#===========================================#

# ObteniendoTipo Generalizando
# se pone antes de todo

tipo <- obteniendotipo(hrefProductoYapo[1])

obteniendoTipo <- function(linkSubPagina) {
  resultadoTipo <- NA
 
   # Obteniendo la sub-paguina

  subPaginaYapoCL <- read_html(linkSubPagina)
  boxProductoYapo <- html_nodes(subPaginaYapoCL, xpath="")
    
    # Sacando tipo de transaccion
    
    datailProductoYapo <- html_nodes(boxProductoYapo,css=".details")
  trDetailsProductosYapo <- html_nodes(datailProductoYapo,css="tr")
  
  for(detail in trDetailsProductoYapo){
    thDetail <- html_nodes(detail,css="th")
    if(html_text(thDetail)=="Tipo"){
      tdDetail <- html_nodes(detail,css="td")
      print(html_text(tdDetail))
      resultadoTipo <- html_text(tdDetail)
    }
    
  } 
  return(resultadoTipo)
}

#===========================================#

Valor_UF <- 30887.21
precio_normal <- c()
precio_calculado <- c()
vectorvalor_uf <- c()
tipo_moneda <- c()
tipo_transaccion <- c()
for (elemento in 2:length(listing_thumbs)) {
  #obteniendo el tipo
  tipo <- obteniendotipo(hrefProductoYapo[elemento-1])
  tipo_transaccion <- c(tipo_transaccion,tipo)
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

#===========================================#

#Agregar a la base de datos el tipo y el link

#===========================================#

#cambia xpath de boxProductosYapo x subPaginaYapoCL,css".info")
#cambia  subPaginaYapoCL <- read_html(hrefProudctosYapo[6]) para luego cambiarla x linkSubPagina

#===========================================#

# Buscar en todas las paginas del catalogo

Yapo_Paginas <- read_html("https://www.yapo.cl/region_metropolitana?ca=15_s&o=",1,sep = "")
Yapo_Paginas <- read_html("https://www.yapo.cl/region_metropolitana?ca=15_s&o=",pagina,sep = "")
# Automatizar lo anterior


Valor_UF <- 30887.21
precio_normal <- c()
precio_calculado <- c()
vectorvalor_uf <- c()
tipo_moneda <- c()
tipo_transaccion <- c()

for (pagina in 1:5) {
  for (elemento in 2:length(listing_thumbs)) {
    #obteniendo el tipo
    tipo <- obteniendotipo(hrefProductoYapo[elemento-1])
    tipo_transaccion <- c(tipo_transaccion,tipo)
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
}



  