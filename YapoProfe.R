
library('rvest')
obteniendoTipo <- function(linkSubPagina){
  resultadoTipo <- NA
  
  # Obteniendo la sub-pagina
  
  subPaginaYapoCL <- read_html(linkSubPagina)
  
  boxProductoYapo <- html_nodes(subPaginaYapoCL, css=".info")
  
  # Sacando el tipo de transacccion
  datailProductoYapo <- html_nodes(boxProductoYapo,css=".details")
  trDetailsProductoYapo <- html_nodes(datailProductoYapo,css="tr")
  
  for (detail in trDetailsProductoYapo) {
    thDetail <- html_nodes(detail,css="th")
    if(html_text(thDetail)=="Tipo"){
      tdDetail <- html_nodes(detail,css="td")
      print(html_text(tdDetail))
      resultadoTipo <- html_text(tdDetail)
    }
  }
  
  return(resultadoTipo)
}



valorUF <- 30052.36
precioNormal <- c()
precioCalculado <- c()
vectorValorUF <- c()
tipoMoneda <- c()
tipoTransaccion <- c()

for (pagina in 1:5) {
  # Leyendo pagina
  paginaYapoCl <- read_html(paste("https://www.yapo.cl/region_metropolitana?ca=15_s&o=",pagina,sep = ""))
  
  # Obteniendo la clase listing_thumbs
  listingThumbs <- html_nodes(paginaYapoCl, css=".listing_thumbs")
  
  # Obteniendo los titulos de los productos
  nombresProductosYapo <- html_nodes(listingThumbs, css=".title")
  
  # Obteniendo las categoria de los productos
  categoriasProductosYapo <- html_nodes(listingThumbs, css=".category")
  
  # Obteniendo las regiones de los productos
  regionesProductosYapo <- html_nodes(listingThumbs, css=".region")
  
  # Obteniendo las comunas de los productos
  comunasProductosYapo <- html_nodes(listingThumbs, css=".commune")
  
  # Obteniendo los precios
  preciosProductosYapo <- html_nodes(listingThumbs, css=".price")
  
  # Obteniendo el link
  seccionImagenProductoYapo <- html_nodes(listingThumbs,css=".listing_thumbs_image")
  linkProductoYapo <- html_nodes(seccionImagenProductoYapo,css="a")
  hrefProductoYapo <- html_attr(linkProductoYapo,"href")
  
  for (elemnto in 2:length(listingThumbs)) {
    print(paste("##################################### ELEMENTO", elemnto - 1 , " - PAGINA", pagina, "############################################"))
    print(html_text(nombresProductosYapo[elemnto - 1]))
    # Obteniendo el tipo
    tipo <- obteniendoTipo(hrefProductoYapo[elemnto-1])
    tipoTransaccion <- c(tipoTransaccion,tipo)
    
    precio <- html_nodes(listingThumbs[elemnto],css=".price")
    if(length(precio)>0){
      precio <- html_text(precio)
      
      print(paste("ANTES:",precio))
      
      precio <- gsub("\t","",precio)
      precio <- gsub("\n","",precio)
      precio <- gsub("[$]","",precio)
      precio <- gsub("[.]","",precio)
      precio <- gsub(",",".",precio)
      precio <- gsub(" ","",precio)
      
      print(paste("DESPUES:",precio))
      
      if(substr(precio, 1, 2) == 'UF'){
        precio <- gsub("UF","",precio)
        precio <- as.numeric(precio)
        # guardando precio normal
        precioNormal <- c(precioNormal,precio)
        
        precio <- precio*valorUF
        precioCalculado <- c(precioCalculado,precio)
        
        vectorValorUF <- c(vectorValorUF,valorUF)
        tipoMoneda <- c(tipoMoneda,'UF')
      }else{
        precio <- as.numeric(precio)
        precioNormal <- c(precioNormal,precio)
        precioCalculado <- c(precioCalculado,NA)
        vectorValorUF <- c(vectorValorUF,NA)
        tipoMoneda <- c(tipoMoneda,'peso')
      }
    }else{
      precio <- NA
      precioNormal <- c(precioNormal,precio)
      precioCalculado <- c(precioCalculado,NA)
      vectorValorUF <- c(vectorValorUF,NA)
      tipoMoneda <- c(tipoMoneda,NA)
    }
    print(precio)
  }
}


nuestrosDatosPROFE <- data.frame(nombre=html_text(nombresProductosYapo),
                            categoria=html_text(categoriasProductosYapo),
                            region=html_text(regionesProductosYapo),
                            comuna=html_text(comunasProductosYapo),
                            tipoTransaccion=tipoTransaccion,
                            precioNormal=precioNormal,
                            precioCalculado=precioCalculado,
                            tipoMoneda=tipoMoneda,
                            valorUf=vectorValorUF,
                            linkProducto=hrefProductoYapo
)



write.csv2(nuestrosDatos,"datosYapo2.csv")