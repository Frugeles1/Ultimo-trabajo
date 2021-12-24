library(xml2)
library(rvest)

# REPARTO DE VOCES #

obteniendoRepartos <- function(linkSubPagina1){
  
  resultadoRepartos <- NA
  
  subPaginaOtakusTV <- read_html(linkSubPagina1)
  
  boxrepartoAnimes <- html_nodes(subPaginaOtakusTV, css=".carusel_voces")
  repartoAnimes <- html_nodes(boxrepartoAnimes, css= ".text-white")
  resultadoRepartos <- html_text(repartoAnimes)
  print(resultadoRepartos)
  
  return(resultadoRepartos)
  
}

# TEMPORADAS #

obteniendoNtemporadas <- function(linkSubPagina2){
  
  resultadoNtemporadas <- NA
  
  subPaginaOtakusTV <- read_html(linkSubPagina2)
  
  box1 <- html_nodes(subPaginaOtakusTV, xpath="//*[@id=\"back_data_perfil\"]/div")
  Ntemporadas <- html_nodes(box1,css="h2")
  resultadoNtemporadas <- html_text(Ntemporadas)
  
  return(resultadoNtemporadas)
  
}

# ESTADO DEL ANIME# 

obteniendoEanimes <- function(linkSubPagina3){
  
  resultadoEanimes <- NA
  
  subPaginaOtakusTV <- read_html(linkSubPagina3)
  estadoAnimes <- html_nodes(subPaginaOtakusTV,xpath="//*[@id=\"back_data_perfil\"]/div/div/div[3]/div/span")

    resultadoEanimes <- html_text(estadoAnimes)
  
  return(resultadoEanimes)
  
}

# FECHA ESTRENO #

obteniendoFestrenos <- function(linkSubPagina4){
  
  resultadoFestrenos <- NA
  
  subPaginaOtakusTV <- read_html(linkSubPagina4)
  
  fechaAnimes <- html_nodes(subPaginaOtakusTV,xpath="//*[@id=\"back_data_perfil\"]/div/div/div[3]/div/div/span")
  resultadoFestrenos <- html_text(fechaAnimes)
  
  return(resultadoFestrenos)
  
}

# DIRECTOR # 

obteniendoDirectores <- function(linkSubPagina5){
  
  resultadoDirectores <- NA
  
  subPaginaOtakusTV <- read_html(linkSubPagina5)
  
  Directores <- html_nodes(subPaginaOtakusTV,xpath="//*[@id=\"nav-informacion\"]/div[1]/div/div/p[1]/span[2]")
  resultadoDirectores <- html_text(Directores)
  return(resultadoDirectores)
  
}

# CREADOR #

obteniendoCreadores <- function(linkSubPagina6){
  
  resultadoCreadores <- NA
  
  subPaginaOtakusTV <- read_html(linkSubPagina6)
  Creadores <- html_nodes(subPaginaOtakusTV,xpath="//*[@id=\"nav-informacion\"]/div[1]/div/div/p[2]/span[2]")
  resultadoCreadores <- html_text(Creadores)
  return(resultadoCreadores)
  
}

# TITULO ALTERNATIVO #

obteniendoTalternativos <- function(linkSubPagina7){
  
  resultadoTalternativos<- NA
  
  subPaginaOtakusTV <- read_html(linkSubPagina7)
  
  TituloAlternativos <- html_nodes(subPaginaOtakusTV,xpath="//*[@id=\"nav-informacion\"]/div[1]/div/div/p[3]/span[2]")
  resultadoTalternativos <- html_text(TituloAlternativos)
  return(resultadoTalternativos)
  
}

# PALABRAS CLAVES #

obteniendoPclaves<- function(linkSubPagina8){
  
  resultadoPclaves<- NA
  
  subPaginaOtakusTV <- read_html(linkSubPagina8)
  box5 <- html_nodes(subPaginaOtakusTV,xpath="/html/body/div[3]/div")
  palabrasClaves <- html_nodes(box5,css=".col-6")
  resultadoPclaves <- html_text(palabrasClaves)
  
  return(resultadoPclaves)
  
}

# DESCRIPCION ANIME

obteniendoDescripciones <- function(linkSubPagina9){
  
  resultadoDescripciones <- NA
  
  subPaginaOtakusTV <- read_html(linkSubPagina9)
  
  descripcionAnimes <- html_nodes(subPaginaOtakusTV,xpath="//*[@id=\"back_data_perfil\"]/div/div/div[3]/div/p/text()")
  descripcionAnimes <- html_text(descripcionAnimes)
  
  return(resultadoDescripciones)
  
}

# Titulo #

obteniendoTitulos <- function(linkSubPagina10){
  
  resultadoTitulos <- NA
  
  subPaginaOtakusTV <- read_html(linkSubPagina10)
  
  
  box1 <- html_nodes(subPaginaOtakusTV, xpath="//*[@id=\"back_data_perfil\"]/div")
  TitulosAnimes <- html_nodes(box1,css="h1")
  resultadoTitulo <- html_text(TitulosAnimes)
  
  return(resultadoTitulos)
  
}

# N CAPITULOS #

obteniendoNcapitulos<- function(linkSubPagina11){
  
  resultadoNcapitulos<- NA
  
  subPaginaOtakusTV <- read_html(linkSubPagina11)
  
  Ncapitulos <- html_nodes(subPaginaOtakusTV,xpath="//*[@id=\"nav-episodios\"]/div[1]/div/span")
  Ncapitulos <- html_text(Ncapitulos)
  
  return(resultadoNcapitulos)
  
}

pegandoYLimpiando <- function(texto){
  
  
  
  textoTotal <- paste(texto,collapse=" ")
  
  textoTotal <- gsub("[.]","",textoTotal)
  
  return(textoTotal)
}

reparto_Animes <-c()
N_temporadas <-c()
estado_Animes <-c()
fecha_Animes <-c()
Directores_ <-c()
Creadores_ <-c()
Titulo_Alternativos <-c()
palabras_Claves <-c()
Descripciones_ <- c()
Titulos_ <- c()
Ncapitulos <- c() 

for (pagina in 1:2){
  
  OtakusTV <- read_html(paste("https://www.otakustv.com/animes?page=",pagina,sep = ""))
  
  # Obteniendo la clase lista de animes
  
  Animes <- html_nodes(OtakusTV, css=".animes_lista")
  
  # Obteniendo el Link
  seccionImagenAnimes <- html_nodes(Animes,css=".col-6")
  linkAnimes <- html_nodes(seccionImagenAnimes,css="a")
  hrefAnimes <- html_attr(linkAnimes,"href")
  
  
  for  ( hrefAnime in hrefAnimes) {
    
    # obteniendo Repartos
    
    repartos <- obteniendoRepartos(hrefAnime)
    reparto_Animes <-c (reparto_Animes,repartos)
    
    
    # obteniendo Ntemporada
    
    ntemporadas <- obteniendoNtemporadas(hrefAnime)
    N_temporadas <- c(N_temporadas,ntemporadas)
    
    # obteniendo EstadoAnime
    
    Eanimes <- obteniendoEanimes(hrefAnime)
    estado_Animes <-c(estado_Animes,Eanimes)
    estado_Animes <- html_text(estado_Animes)
    estado_Animes <- gsub("\n","",estado_Animes)
    
  }
    
    # obteniendo Fecha
    
    fechasE <- obteniendoFestrenos(hrefAnime)
    fecha_Animes <-c(fecha_Animes,fechasE)
    
    # obteniendo Director
    
    Danimes <- obteniendoDirectores(hrefAnime)
    Directores_ <-c(Directores_,Danimes)
    
    # obteniendo Creador
    
    Canimes <- obteniendoCreadores(hrefAnime)
    Creadores_ <-c(Creadores_,Canimes)
    
    # obteniendo Titulo Alternativo
    
    Tituloalt <- obteniendoTalternativos(hrefAnime)
    Titulo_Alternativos <-c(Titulo_Alternativos,Tituloalt)
    
    # obteniendo Palabras Claves
    
    Pclaves <- obteniendoPclaves(hrefAnime)
    palabras_Claves <-c(palabras_Claves,Pclaves)
    
    
    # Obteniendo la Descripcion
    
    descripciones <- obteniendoDescripciones(hrefAnime)
    Descripciones_ <- c(Descripciones_,descripciones)
    
    # Obteniendo Titulos
    
    
    TitulosAnimes <- obteniendoTitulos(hrefAnime)
    Titulos_ <- c(Titulos_,TitulosAnimes)
    
    # Obteniendo Capitulos
    
    CapitulosAnimes <- obteniendoNcapitulos(hrefAnime)
    Ncapitulos <- c(Ncapitulos,CapitulosAnimes)
    
    for (actor in repartos) {
      print(paste(Titulos_,actor,N_temporadas,estado_Animes,fecha_Animes,Directores_,Creadores_,Titulo_Alternativos,palabras_Claves,Descripciones_,Ncapitulos))
    }

  }
  






#para probar

print(Ncapitulos)
print(Titulos_)
print(Descripciones_)
print(palabras_Claves)
print(Titulo_Alternativos)
print(Creadores_)
print(Directores_)
print(fecha_Animes)
print(estado_Animes)
print(N_temporadas)
print(reparto_Animes)
