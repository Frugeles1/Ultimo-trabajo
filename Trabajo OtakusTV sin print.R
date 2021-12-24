

library(xml2)
library(rvest)


for (pagina in 1:5){
  
  OtakusTV <- read_html("https://www.otakustv.com/animes",pagina, sep = "")

  
  # Obteniendo la clase lista de animes
  
  Animes <- html_nodes(OtakusTV, css=".animes_lista")
  
  # Obteniendo el link
  seccionImagenAnime<- html_nodes(Animes,css=".col-6")
  linkAnime <- html_nodes(seccionImagenAnime,css="a") 
  hrefAnime <- html_attr(linkAnime,"href")

  
  # Obteniendo de la descripcion
  
  descripcionAnime <- html_attr(linkAnime,"data-original-title")
  

  
  # Obteniendo titulos
  
  seccionImagenAnime<- html_nodes(Animes,css=".col-6")
  linkAnime <- html_nodes(seccionImagenAnime,css="a") 
  titulosAnime <- html_nodes(linkAnime,css="p") 

  
  # Obteniendo capitulos
  
  seccionImagenAnime<- html_nodes(Animes,css=".col-6")
  capitulosAnime <- html_nodes(seccionImagenAnime, css=".bog")
  capitulosAnime <- html_text(capitulosAnime)
  capitulosAnime <- gsub("Película","",capitulosAnime)
  capitulosAnime <- gsub(".","",capitulosAnime)
  capitulosAnime <- gsub(" ","",capitulosAnime)

  
  #========================#
  
  subPaginaOtakusTV <- read_html(hrefAnime[1])

  
  
  box <- html_nodes(subPaginaOtakusTV, css=".container-fluid")

  
  box1 <- html_nodes(subPaginaOtakusTV, xpath="//*[@id=\"back_data_perfil\"]/div")

  
  box3 <- html_nodes(subPaginaOtakusTV,xpath="//*[@id=\"nav-informacion\"]/div[1]")

  
  box5 <- html_nodes(subPaginaOtakusTV,xpath="/html/body/div[3]/div")

  
  # Reparto de voces
  
  boxrepartoAnime <- html_nodes(subPaginaOtakusTV, css=".carusel_voces")
  repartoAnime <- html_nodes(boxrepartoAnime, css= ".text-white")

  
  # Sacando datos box1
  
  inn_text <- html_nodes(box1,css=".inn-text")
  Ntemporadas <- html_nodes(inn_text,css="h2")
  Ntemporadas <- html_text(Ntemporadas)
  Ntemporadas <- gsub("TEMPORADA(S) DISPONIBLE(S)","",Ntemporadas)

  
  estadoAnime <- html_nodes(inn_text,xpath="//*[@id=\"back_data_perfil\"]/div/div/div[3]/div/span")
  estadoAnime <- html_text(estadoAnime)
  estadoAnime <- gsub("\n","",estadoAnime)

  
  fechaAnime <- html_nodes(inn_text,xpath="//*[@id=\"back_data_perfil\"]/div/div/div[3]/div/div/span")
  fechaAnime <- html_text(fechaAnime)
  fechaAnime  <- gsub("Estreno","",fechaAnime)
  fechaAnime  <- gsub(":","",fechaAnime)

  
  # Sacando datos box3 
  
  
  Director <- html_nodes(inn_text,xpath="//*[@id=\"nav-informacion\"]/div[1]/div/div/p[1]/span[2]")

  Creador <- html_nodes(inn_text,xpath="//*[@id=\"nav-informacion\"]/div[1]/div/div/p[2]/span[2]")

  TituloAlternativo <- html_nodes(inn_text,xpath="//*[@id=\"nav-informacion\"]/div[1]/div/div/p[3]/span[2]")

  
  # Sacando datos box5
  
  
  palabrasClaves <- html_nodes(box5,css=".col-6")
  palabrasClaves <- html_text(palabrasClaves)
  palabrasClaves <- gsub("\n","",palabrasClaves)

  
}
