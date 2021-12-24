# DESCRIPCION ANIME

obteniendoDescripciones <- function(linkSubPagina9){
  
  resultadoDescripciones <- NA
  
  subPaginaOtakusTV <- read_html(linkSubPagina9)
  Boxdescripcion <- html_nodes(subPaginaOtakusTV, xpath ="//*[@id=\"myModal\"]/div")
  descripcionAnimes <- html_nodes(Boxdescripcion, xpath ="//*[@id=\"myModal\"]/div/div/div[2]")
  descripcionAnimes <- html_text(descripcionAnimes)
  
  return(resultadoDescripciones)
  
}

Descripciones_ <- c()

for (pagina in 1:5){
  
  OtakusTV <- read_html(paste("https://www.otakustv.com/animes?page=",pagina,sep = ""))
  
  # Obteniendo la clase lista de animes
  
  Animes <- html_nodes(OtakusTV, css=".animes_lista")
  
  # Obteniendo el Link
  seccionImagenAnimes <- html_nodes(Animes,css=".col-6")
  linkAnimes <- html_nodes(seccionImagenAnimes,css="a")
  hrefAnimes <- html_attr(linkAnimes,"href")
  
  # Obteniendo la Descripcion
  
  descripcionAnime <- html_attr(linkAnimes,"data-original-title")
  
  # Obteniendo Titulos
  
  titulosAnime <- html_nodes(seccionImagenAnimes, css =".mb-1")
  titulosAnime <- html_text(titulosAnime)

  # Obteniendo Capitulos
  
  capitulosAnime <- html_nodes(seccionImagenAnimes, css=".bog")
  capitulosAnime <- html_text(capitulosAnime)
  }

print(descripcionAnime)
print(titulosAnime)
print(capitulosAnime)
