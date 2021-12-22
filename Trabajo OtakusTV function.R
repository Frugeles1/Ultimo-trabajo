
# REPARTO DE VOCES #

obteniendoReparto <- function(linkSubPagina1){
  
resultadoReparto <- NA

subPaginaOtakusTV <- read_html(linkSubPagina1)

boxrepartoAnime <- html_nodes(subPaginaOtakusTV, css=".carusel_voces")
repartoAnime <- html_nodes(boxrepartoAnime, css= ".text-white")
resultadoReparto <- html_text(repartoAnime)
print(resultadoReparto)

return(resultadoReparto)

}

# TEMPORADAS #

obteniendoNtemporadas <- function(linkSubPagina2){
  
  resultadoNtemporadas <- NA
  
  subPaginaOtakusTV <- read_html(linkSubPagina2)

inn_text <- html_nodes(box1,css=".inn-text")
Ntemporadas <- html_nodes(inn_text,css="h2")
if(length(Ntemporadas)){
  Ntemporadas <- html_text(Ntemporadas)
  Ntemporadas <- gsub("TEMPORADA(S) DISPONIBLE(S)","",Ntemporadas)
  resultadoNtemporadas <- html_text(Ntemporadas)
}



return(resultadoNtemporadas)

}

# ESTADO DEL ANIME# 

obteniendoEanime <- function(linkSubPagina3){
  
  resultadoEanime <- NA
  
  subPaginaOtakusTV <- read_html(linkSubPagina3)

estadoAnime <- html_nodes(inn_text,xpath="//*[@id=\"back_data_perfil\"]/div/div/div[3]/div/span")
if(length(estado_Anime)){
  estadoAnime <- html_text(estadoAnime)
  estadoAnime <- gsub("\n","",estadoAnime)
  resultadoEanime <- html_text(estadoAnime)
}


return(resultadoEanime)

}

# FECHA ESTRENO #

obteniendoFestreno <- function(linkSubPagina4){
  
  resultadoFestreno<- NA
  
  subPaginaOtakusTV <- read_html(linkSubPagina4)

fechaAnime <- html_nodes(inn_text,xpath="//*[@id=\"back_data_perfil\"]/div/div/div[3]/div/div/span")
fechaAnime <- html_text(fechaAnime)
if (length(fechaAnime)){
  fechaAnime  <- gsub("Estreno","",fechaAnime)
  fechaAnime  <- gsub(":","",fechaAnime)
  resultadoFestreno <- html_text(fechaAnime)
}


return(resultadoEanime)

}

# DIRECTOR # 

obteniendoDirector <- function(linkSubPagina5){
  
  resultadoDirector<- NA
  
  subPaginaOtakusTV <- read_html(linkSubPagina5)

box3 <- html_nodes(subPaginaOtakusTV,xpath="//*[@id=\"nav-informacion\"]/div[1]")

Director <- html_nodes(inn_text,xpath="//*[@id=\"nav-informacion\"]/div[1]/div/div/p[1]/span[2]")
resultadoDirector <- html_text(Director)
return(resultadoDirector)

}

# CREADOR #

obteniendoCreador <- function(linkSubPagina6){
  
  resultadoCreador<- NA
  
  subPaginaOtakusTV <- read_html(linkSubPagina6)
  
  box3 <- html_nodes(subPaginaOtakusTV,xpath="//*[@id=\"nav-informacion\"]/div[1]")

Creador <- html_nodes(inn_text,xpath="//*[@id=\"nav-informacion\"]/div[1]/div/div/p[2]/span[2]")
resultadoCreador <- html_text(Creador)
return(resultadoCreador)

}

# TITULO ALTERNATIVO #

obteniendoTalternativo <- function(linkSubPagina7){
  
  resultadoTalternativo<- NA
  
  subPaginaOtakusTV <- read_html(linkSubPagina7)

TituloAlternativo <- html_nodes(inn_text,xpath="//*[@id=\"nav-informacion\"]/div[1]/div/div/p[3]/span[2]")
resultadoTalternativo <- html_text(TituloAlternativo)
return(resultadoTalternativo)

}


# PALABRAS CLAVES #

obteniendoPclaves<- function(linkSubPagina8){
  
  resultadoPclaves<- NA
  
  subPaginaOtakusTV <- read_html(linkSubPagina8)

palabrasClaves <- html_nodes(box5,css=".col-6")
palabrasClaves <- html_text(palabrasClaves)
if(length(palabrasClaves)){
  palabrasClaves <- gsub("\n","",palabrasClaves)
  resultadoPclaves <- html_text(palabrasClaves)
}


return(resultadoPclaves)

}


reparto_Anime <-c()
N_temporada <-c()
estado_Anime <-c()
fecha_Anime <-c()
Director_ <-c()
Creador_ <-c()
Titulo_Alternativo <-c()
palabras_Claves <-c()

for (pagina in 1:5){
  
  OtakusTV <- read_html("https://www.otakustv.com/animes=",pagina,sep = "")
  
  #PAGINA CON EL MISMO LINK
  
  # Obteniendo la clase lista de animes
  
  Animes <- html_nodes(OtakusTV, css=".animes_lista")
  
  # Obteniendo el Link
  seccionImagenAnime<- html_nodes(Animes,css=".col-6")
  linkAnime <- html_nodes(seccionImagenAnime,css="a") 
  hrefAnime <- html_attr(linkAnime,"href")
  
  
  # Obteniendo la Descripcion
  
  descripcionAnime <- html_attr(linkAnime,"data-original-title")
  
  
  
  # Obteniendo Titulos
  
  seccionImagenAnime<- html_nodes(Animes,css=".col-6")
  linkAnime <- html_nodes(seccionImagenAnime,css="a") 
  titulosAnime <- html_nodes(linkAnime,css="p") 
  
  # Obteniendo Capitulos
  
  seccionImagenAnime<- html_nodes(Animes,css=".col-6")
  capitulosAnime <- html_nodes(seccionImagenAnime, css=".bog")
  }


  # obteniendo Repartos
  
  reparto <- obteniendoReparto(hrefAnime)
  reparto_Anime <-c(reparto_Anime,reparto)
  
  # obteniendo Ntemporada
  
  ntemporadas <- obteniendoNtemporadas(hrefAnime)
  N_temporada <-c(ntemporadas,ntemporadas)
  
  # obteniendo EstadoAnime
  
  eanime <- obteniendoEanime(hrefAnime)
  estado_Anime <-c(estado_Anime,eanime)
  
  # obteniendo Fecha
  
  fechasE <- obteniendoFestreno(hrefAnime)
  fecha_Anime <-c(fecha_Anime,fechasE)
  
  # obteniendo Director
  
  Danime <- obteniendoDirector(hrefAnime)
  Director_ <-c(Director_,Danime)
  
  # obteniendo Creador
  
  Canime <- obteniendoCreador(hrefAnime)
  Creador_ <-c(Creador_,Canime)
  
  # obteniendo Titulo Alternativo
  
  Tituloalt <- obteniendoTalternativo(hrefAnime)
  Titulo_Alternativo <-c(Titulo_Alternativo,Tituloalt)
  
  # obteniendo Palabras Claves
  
  Pclaves <- obteniendoPclaves(hrefAnime)
  palabras_Claves <-c( palabras_Claves,Pclaves)
  
  for (elemnto in 1:length(Animes)){
    capitulosAnime <- html_text(capitulosAnime)
    if(length(capitulosAnime)){
      capitulosAnime <- gsub("Película","",capitulosAnime)
      capitulosAnime <- gsub(".","",capitulosAnime)
      capitulosAnime <- gsub(" ","",capitulosAnime)
  }
    
}


#Error in read_xml.raw(raw, encoding = encoding, base_url = base_url, as_html = as_html,  : 
                       # STRING_ELT() can only be applied to a 'character vector', not a 'integer'

    